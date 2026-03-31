# Biostatistical Methods Reference

A practical guide for selecting, executing, and validating statistical 
methods across biomedical and clinical research.

---

## How to Use This Guide

1. Identify outcome (dependent variable) type
2. Identify predictor (independent variable) type
3. Use the decision table to select a method
4. Follow the **Execution Steps** before interpreting results

---

## Quick Method Selection

| Outcome Type               | Predictor Type         | Recommended Method  |
| -------------------------- | ---------------------- | ------------------- |
| Continuous                 | Continuous             | Linear regression   |
| Continuous                 | Categorical            | t-test / ANOVA      |
| Binary                     | Continuous/Categorical | Logistic regression |
| Time-to-event              | Any                    | Cox PH model        |
| Categorical vs Categorical | Categorical            | Chi-square test     |

---

# Descriptive Statistics

## Continuous Variables

### Mean / Median / SD

**Execution Steps**

1. Check missing data
2. Visualize distribution (histogram, boxplot)
3. Assess normality
4. Choose:

   * Mean + SD → normal data
   * Median + IQR → skewed data

| Step             | Data Type  | Purpose          | Method        | Output         |
| ---------------- | ---------- | ---------------- | ------------- | -------------- |
| Summarize center | Continuous | Central tendency | Mean / Median | Mean or Median |
| Measure spread   | Continuous | Variability      | SD / IQR      | SD or IQR      |

**R Code**

```r
summary(df$variable)
mean(df$variable, na.rm = TRUE)
median(df$variable, na.rm = TRUE)
sd(df$variable, na.rm = TRUE)
IQR(df$variable, na.rm = TRUE)
```

---

## Categorical Variables

### Frequency Table

**Execution Steps**

1. Check missing values
2. Count observations per category
3. Convert to proportions (%)

| Step         | Data Type   | Purpose             | Method               | Output         |
| ------------ | ----------- | ------------------- | -------------------- | -------------- |
| Distribution | Categorical | Describe categories | Counts / Proportions | % per category |

**R Code**

```r
table(df$category)
prop.table(table(df$category))
```

---

# Inferential Statistics

## Group Comparisons

### t-test (2 Groups)

**Execution Steps**

1. Check outcome is continuous
2. Check grouping variable has 2 levels
3. Assess normality (histogram or Shapiro test)
4. Check variance equality
5. Run t-test

| Purpose       | Method | Output                   |
| ------------- | ------ | ------------------------ |
| Compare means | t-test | Mean difference, p-value |

**R Code**

```r
t.test(outcome ~ group, data = df)
```

---

### ANOVA (3+ Groups)

**Execution Steps**

1. Confirm ≥3 groups
2. Check normality
3. Check homogeneity of variance
4. Run ANOVA
5. If significant → post-hoc test

| Purpose                | Method | Output               |
| ---------------------- | ------ | -------------------- |
| Compare multiple means | ANOVA  | F-statistic, p-value |

**R Code**

```r
model <- aov(outcome ~ group, data = df)
summary(model)
```

---

### Chi-Square Test

**Execution Steps**

1. Confirm both variables are categorical
2. Create contingency table
3. Check expected cell counts (>5 recommended)
4. Run chi-square test

| Purpose          | Method     | Output  |
| ---------------- | ---------- | ------- |
| Test association | Chi-square | p-value |

**R Code**

```r
tbl <- table(df$var1, df$var2)
chisq.test(tbl)
```

---

## Continuous Outcome

### Linear Regression

**Execution Steps**

1. Confirm outcome is continuous
2. Explore relationship (scatterplot)
3. Check linearity
4. Fit model
5. Check residuals
6. Interpret coefficients

| Purpose            | Method            | Output            |
| ------------------ | ----------------- | ----------------- |
| Model relationship | Linear regression | Beta, CI, p-value |

**R Code**

```r
model <- lm(outcome ~ predictor, data = df)
summary(model)

# Diagnostic plots
plot(model)
```

**Clinical Relevance:** Continuous endpoints (e.g., lab values, vitals)

---

## Binary Outcome

### Logistic Regression

**Execution Steps**

1. Confirm outcome is binary
2. Check predictor distributions
3. Assess multicollinearity
4. Fit model
5. Interpret odds ratios

| Purpose              | Method              | Output          |
| -------------------- | ------------------- | --------------- |
| Model binary outcome | Logistic regression | OR, CI, p-value |

**R Code**

```r
model <- glm(outcome ~ predictor, data = df, family = binomial)
summary(model)

# Odds ratios
exp(coef(model))
```

**Clinical Relevance:** Risk modeling, treatment response

---

## Time-to-Event Data

### Kaplan-Meier

**Execution Steps**

1. Confirm time-to-event data
2. Define time variable and event indicator
3. Fit survival curve
4. Plot survival function

| Purpose           | Method       | Output         |
| ----------------- | ------------ | -------------- |
| Estimate survival | Kaplan-Meier | Survival curve |

**R Code**

```r
library(survival)

fit <- survfit(Surv(time, event) ~ 1, data = df)
plot(fit)
```

---

### Cox Proportional Hazards

**Execution Steps**

1. Confirm time-to-event outcome
2. Fit Cox model
3. Check proportional hazards assumption
4. Interpret hazard ratios

| Purpose         | Method       | Output |
| --------------- | ------------ | ------ |
| Compare hazards | Cox PH model | HR, CI |

**R Code**

```r
library(survival)

model <- coxph(Surv(time, event) ~ predictor, data = df)
summary(model)

# PH assumption
cox.zph(model)
```

**Clinical Relevance:** Survival analysis in clinical trials

---

# Key Validation Checks

| Check                | Application       |
| -------------------- | ----------------- |
| Normality            | t-test, ANOVA     |
| Linearity            | Linear regression |
| Multicollinearity    | Regression models |
| Proportional Hazards | Cox model         |
| Expected Counts      | Chi-square        |

---

# Cross-References

See:

* biostat_glossary.md
