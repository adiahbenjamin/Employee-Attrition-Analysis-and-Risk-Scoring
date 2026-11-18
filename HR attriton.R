# Packages and data 
library(readxl)
library(dplyr)
library(glmnet)
library(pROC)
library(xgboost)
library(PRROC)
library(ranger)
library(writexl)
library(ggplot2)

Data <- read_excel("C://Users//Adiah Benjamin//Downloads//employee_attrition.xlsx")
y <- as.integer(Data$Attrition == "Yes")
n <- length(y)
names(Data)

# Prepare features
x <- Data %>%
  select(-Employee_ID, -Employee_Name, -Date, -Hire_Date,
         -Reason_For_Leaving, -Attrition)

# Fill missing values with the median and keep a flag for what was missing.
# Run this block once per session.
for (col in c("Job_Satisfaction", "Career_Growth_Opportunity",
              "Training_Hours", "Distance_From_Home_KM")) {
  stopifnot(col %in% names(x))
  x[[paste0(col, "_missing")]] <- as.integer(is.na(x[[col]]))
  x[[col]][is.na(x[[col]])] <- median(x[[col]], na.rm = TRUE)
}

x$Overtime <- as.integer(x$Overtime == "Yes")

x <- x %>%
  group_by(Job_Level) %>%
  mutate(Salary_vs_Level = Monthly_Salary_GHS / median(Monthly_Salary_GHS)) %>%
  ungroup()

x$Tenure_lt1    <- as.integer(x$Years_At_Company == 0)
x$Promo_gap     <- x$Years_Since_Promotion / (x$Years_At_Company + 1)
x$Low_sat_count <- (x$Job_Satisfaction <= 2) + (x$Work_Life_Balance <= 2) +
  (x$Career_Growth_Opportunity <= 2) + (x$Manager_Relationship <= 2) +
  (x$Environment_Satisfaction <= 2)

# One-hot encode text columns (first level dropped), keep numbers as they are.
x <- x %>% mutate(across(where(is.character), as.factor))
X <- model.matrix(~ ., data = as.data.frame(x))[, -1]
stopifnot(nrow(X) == n)            # stops if rows were dropped because of missing values
colnames(X) <- make.names(colnames(X), unique = TRUE)
storage.mode(X) <- "double"
p <- ncol(X)

# Models 
fit_predict <- function(model, Xtr, ytr, Xte) {
  if (model == "lr") {
    # Ridge logistic regression (strength roughly equal to C = 0.05 in scikit-learn)
    fit <- glmnet(Xtr, ytr, family = "binomial", alpha = 0,
                  lambda = 0.002, standardize = TRUE)
    return(as.numeric(predict(fit, Xte, type = "response")))
  }
  if (model == "rf") {
    dtr <- data.frame(Xtr, y = factor(ytr))
    fit <- ranger(y ~ ., data = dtr, num.trees = 500, mtry = floor(0.3 * ncol(Xtr)),
                  min.node.size = 25, probability = TRUE, seed = 1)
    return(predict(fit, data.frame(Xte))$predictions[, "1"])
  }
  if (model == "gb") {
    dtr <- xgb.DMatrix(Xtr, label = ytr)
    fit <- xgb.train(
      params = list(objective = "binary:logistic", max_depth = 3, eta = 0.03,
                    min_child_weight = 9, lambda = 5, nthread = 2),
      data = dtr, nrounds = 250, verbose = 0)
    return(predict(fit, xgb.DMatrix(Xte)))
  }
}

stratified_folds <- function(y, k) {
  f <- integer(length(y))
  for (cls in unique(y)) {
    idx <- which(y == cls)
    f[idx] <- sample(rep_len(seq_len(k), length(idx)))
  }
  f
}

# Out-of-fold scores (5 folds, repeated 3 times, averaged)
# Every employee is scored by a model that never saw that employee.
models <- c("lr", "rf", "gb")
oof    <- sapply(models, function(m) numeric(n), simplify = FALSE)
reps   <- 3

for (r in seq_len(reps)) {
  set.seed(100 + r)
  fold <- stratified_folds(y, 5)
  for (k in 1:5) {
    tr <- fold != k; te <- fold == k
    for (m in models) {
      oof[[m]][te] <- oof[[m]][te] +
        fit_predict(m, X[tr, ], y[tr], X[te, , drop = FALSE]) / reps
    }
  }
  cat("Finished repeat", r, "of", reps, "\n")
}

prob <- (oof$lr + oof$rf + oof$gb) / 3     # simple average of the three models

# Performance
auc_of <- function(s) as.numeric(auc(roc(y, s, quiet = TRUE)))
perf <- data.frame(
  Model = c("Logistic regression", "Random forest", "Gradient boosting", "Blend of all three (used)"),
  ROC_AUC = round(c(auc_of(oof$lr), auc_of(oof$rf), auc_of(oof$gb), auc_of(prob)), 3))
print(perf)

base  <- mean(y)
ord   <- order(prob, decreasing = TRUE)
k10   <- ceiling(0.10 * n)
top10 <- mean(y[ord[1:k10]])
bot10 <- mean(y[ord[(n - k10 + 1):n]])
prauc <- pr.curve(scores.class0 = prob[y == 1], scores.class1 = prob[y == 0])$auc.integral
extra <- data.frame(
  Metric = c("Baseline attrition rate", "PR-AUC (blend)", "Attrition in top 10% by score",
             "Attrition in bottom 10% by score", "Lift in top 10%"),
  Value  = round(c(base, prauc, top10, bot10, top10 / base), 3))
print(extra)

# Risk levels (percentile bands) 
q   <- rank(prob, ties.method = "first") / n
lvl <- ifelse(q > 0.95, "Very High", ifelse(q > 0.80, "High", ifelse(q > 0.50, "Moderate", "Low")))

chk <- data.frame(Risk_Level = c("Low", "Moderate", "High", "Very High"),
                  Definition = c("Bottom 50% of scores", "Next 30%", "Next 15%", "Top 5%"))
chk$Employees <- sapply(chk$Risk_Level, function(l) sum(lvl == l))
chk$Actual_Attrition_Rate <- round(sapply(chk$Risk_Level, function(l) mean(y[lvl == l])), 3)
print(chk)

# Drivers (from the logistic model fitted on all data)
lr_full <- glmnet(X, y, family = "binomial", alpha = 0, lambda = 0.002, standardize = TRUE)
coefs   <- as.numeric(coef(lr_full))[-1]
names(coefs) <- colnames(X)

centered <- sweep(X, 2, colMeans(X))
contrib  <- sweep(centered, 2, coefs, "*")      # push of each feature on this person's log-odds

prefixes <- c(Education_Field = "Field", Job_Role = "Role", Marital_Status = "Marital status",
              Employment_Type = "Employment", Travel_Frequency = "Travel",
              Department = "Department", Location = "Location",
              Education = "Education", Gender = "Gender")

nice <- function(nm, pos) {
  for (pre in names(prefixes)) {
    if (startsWith(nm, pre)) {
      return(paste0(prefixes[[pre]], ": ", gsub("\\.", " ", substring(nm, nchar(pre) + 1))))
    }
  }
  if (nm == "Overtime")        return("Works overtime")
  if (nm == "Tenure_lt1")      return("First-year employee")
  if (nm == "Low_sat_count")   return("Several low satisfaction scores")
  if (nm == "Promo_gap")       return(if (pos) "Long time since promotion" else "Recent promotion")
  if (nm == "Salary_vs_Level") return(if (pos) "Pay above level median" else "Pay below level median")
  paste0(if (pos) "High " else "Low ", tolower(gsub("_missing", " (missing)", gsub("_", " ", nm))))
}

drivers <- t(apply(contrib, 1, function(row) {
  idx <- order(row, decreasing = TRUE)[1:3]
  sapply(idx, function(j) if (row[j] > 0.02) nice(colnames(X)[j], coefs[j] > 0) else "")
}))
colnames(drivers) <- c("Top_Driver_1", "Top_Driver_2", "Top_Driver_3")

# Feature importance
rf_full <- ranger(y ~ ., data = data.frame(X, y = factor(y)), num.trees = 500,
                  mtry = floor(0.3 * p), min.node.size = 25, importance = "impurity", seed = 1)
imp <- data.frame(Feature = colnames(X),
                  Logistic_Std_Coef = round(coefs * apply(X, 2, sd), 3),
                  RF_Importance = round(as.numeric(rf_full$variable.importance[colnames(X)]), 2))
imp <- imp[order(-abs(imp$Logistic_Std_Coef)), ][1:20, ]


# ROC curve 
roc_df <- function(s, label) {
  r <- roc(y, s, quiet = TRUE)
  data.frame(FPR = 1 - r$specificities, TPR = r$sensitivities, Model = label)
}

roc_all <- rbind(
  roc_df(oof$lr, "Logistic regression"),
  roc_df(oof$rf, "Random forest"),
  roc_df(oof$gb, "Gradient boosting"),
  roc_df(prob,   "Blend (used)")
)
roc_all$Model <- factor(roc_all$Model,
                        levels = c("Logistic regression", "Random forest", "Gradient boosting", "Blend (used)"))

auc_blend <- round(as.numeric(auc(roc(y, prob, quiet = TRUE))), 3)

p_roc <- ggplot(roc_all, aes(FPR, TPR, colour = Model, linewidth = Model)) +
  geom_abline(linetype = "dashed", colour = "red") +
  geom_line() +
  scale_colour_manual(values = c("#8A93A0", "#F2A93B", "#1F9D78", "#07243A")) +
  scale_linewidth_manual(values = c(0.6, 0.6, 0.6, 1.1)) +
  coord_equal() +
  labs(title = "ROC curve (out-of-fold scores)",
       subtitle = paste0("Blend AUC = ", auc_blend, "  |  dashed line = random guessing"),
       x = "False positive rate", y = "True positive rate") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor = element_blank())

print(p_roc)
ggsave("roc_curve.png", p_roc, width = 6, height = 6, dpi = 300)

# Risk-level bars 
bars <- chk
bars$Risk_Level <- factor(bars$Risk_Level, levels = c("Low", "Moderate", "High", "Very High"))

p_bars <- ggplot(bars, aes(Risk_Level, Actual_Attrition_Rate, fill = Risk_Level)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = mean(y), linetype = "dashed", colour = "grey50") +
  annotate("text", x = 0.55, y = mean(y), hjust = 0, vjust = -0.6, size = 3.3,
           colour = "grey40", label = paste0("company ", round(mean(y) * 100, 1), "%")) +
  scale_fill_manual(values = c("Low" = "#1F9D78", "Moderate" = "#F2A93B",
                               "High" = "#E5604F", "Very High" = "#A82D20"),
                    guide = "none") +
  scale_y_continuous(labels = function(v) paste0(round(v * 100), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Actual attrition by risk level",
       subtitle = "Out-of-fold check on all records",
       x = NULL, y = "Attrition rate") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())

print(p_bars)
ggsave("risk_level_bars.png", p_bars, width = 6, height = 4.5, dpi = 300)

# Employee scores sheet
scores <- data.frame(
  Employee_ID         = Data$Employee_ID,
  Employee_Name       = Data$Employee_Name,
  Department          = Data$Department,
  Job_Role            = Data$Job_Role,
  Location            = Data$Location,
  Attrition           = Data$Attrition,
  Is_Current_Employee = ifelse(Data$Attrition == "No", "Yes", "No"),
  Risk_Probability    = round(prob, 4),
  Risk_Percent        = round(prob * 100, 1),
  Risk_Level          = lvl)
scores$Risk_Rank <- rank(-scores$Risk_Probability, ties.method = "first")
scores <- cbind(scores, drivers)
scores <- scores[order(scores$Risk_Rank), ]

notes <- data.frame(Notes = c(
  "Scores are out-of-fold: each employee was scored by models that never saw that employee (5 folds, repeated 3 times, averaged).",
  "Excluded to avoid leakage: Reason_For_Leaving, Date, Hire_Date, names and IDs.",
  "Signal is moderate (ROC AUC about 0.65). Use scores to prioritise conversations, not to decide individual outcomes.",
  "Risk levels are percentile bands: Very High = top 5%, High = next 15%, Moderate = next 30%, Low = bottom 50%.",
  "Top_Driver columns come from the logistic model and are associations, not proven causes.",
  "Treat the Employee_Scores sheet as sensitive HR data."))


write_xlsx(
  list(Model_Performance  = rbind(perf, data.frame(Model = "", ROC_AUC = NA)),
       Model_Extra        = extra,
       Risk_Level_Check   = chk,
       Feature_Importance = imp,
       Employee_Scores    = scores,
       Notes              = notes),
  "attrition_model_scores_R.xlsx")

cat("Done. Saved attrition_model_scores_R.xlsx\n")