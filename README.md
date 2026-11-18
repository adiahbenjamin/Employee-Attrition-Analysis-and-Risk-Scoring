## Project Overview

Employee attrition is a major workforce management challenge. Understanding why employees leave, identifying patterns associated with turnover, and recognising employees who may require additional support can help organisations make better workforce decisions.

This project develops an end-to-end **Employee Attrition Analysis and Risk Scoring System** using a sample HR dataset containing 10,000 employee records.

The project combines **Exploratory Data Analysis (EDA), statistical analysis, machine learning and interactive Power BI reporting** to examine workforce attrition and identify patterns associated with employee departures.

The final solution includes an employee attrition dashboard, predictive risk scoring models and employee-level risk categorisation for analytical demonstration.

> **Dataset Notice:** This project uses sample HR data. All figures, findings and model results represent the supplied dataset and should not be interpreted as actual organisational workforce statistics.
>
> **Interested in the dataset? DM me.**

---

## Project Objectives

- Measure overall employee attrition and examine its distribution over time.
- Identify departments and employee groups with relatively higher attrition.
- Investigate relationships between employee satisfaction, engagement, overtime and attrition.
- Develop machine learning models to estimate attrition probabilities.
- Rank current employees into risk categories.
- Build an interactive Power BI dashboard to communicate findings.
- Provide evidence-based insights that can support workforce retention strategies.

---

## Predictive Modelling

Three machine learning algorithms were evaluated to estimate employee attrition probabilities.

### Models Evaluated

1. **Logistic Regression (Ridge)**
2. **Random Forest**
3. **Gradient Boosting**

### Model Evaluation

The models were evaluated using **stratified five-fold cross-validation, repeated three times**.

The final ensemble combines predictions from the three algorithms.

| Model | ROC AUC |
|---|---:|
| Logistic Regression | 0.648 |
| Random Forest | 0.647 |
| Gradient Boosting | 0.653 |
| **Ensemble Model** | **0.656** |

---

## Limitations and Responsible Use

This project has several limitations:

- The dataset does not contain actual employee exit dates.
- Monthly and yearly attrition views represent record-based proportions rather than true workforce turnover.
- The model achieves a moderate ROC AUC of approximately **0.656**.
- Predictive relationships do not establish causation.
- Some employee groups have relatively small sample sizes.
- Potential demographic bias requires further investigation.

**Responsible Use:** Employee risk scores should only support analytical exploration and supportive retention conversations. They should never independently determine recruitment, promotion, compensation or termination decisions.

Individual employee records and risk scores must not be published in a public repository.

---

## Future Improvements

- Incorporate actual employee exit dates and monthly headcount snapshots.
- Introduce additional workload and management variables.
- Perform fairness and bias assessments.
- Improve predictive performance through additional validated features.
- Develop an automated employee retention monitoring system.
- Introduce explainability methods such as SHAP.
- Monitor model performance as new workforce data becomes available.
