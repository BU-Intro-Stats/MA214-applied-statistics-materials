# MA 214 Learning Objectives

<!--
Instructor notes:
- Calendar/weekly schedule: objective text is not shown directly, but Lab and Project tags drive prerequisite checks and instructor flags.
- Lecture links: lecture_summary.md should use objective codes in M#.L## format so the generator can match coverage timing.
- Quiz-to-objective alignment is intentionally left undecided; add a quiz table and Quiz tags after the alignment is finalized.
- All objectives currently have the same status; there is no Core/Auxiliary distinction.
-->

**Global:** 4
**Specific:** 22

---

## Module 1: Modeling and Linear Regression
*(IMS Chapters 7–10; 6 objectives)*

1. **Statistical Models and Maximum Likelihood:** I can describe what a statistical model is and determine its likelihood and, for simple models, the maximum likelihood estimator. I can explain the least squares method used to estimate regression coefficients as a maximum likelihood estimator and interpret its significance in fitting a linear regression model to data. [Lab2, Project1]
2. **Understanding and Applying Linear Regression Models:** I can explain the meaning and assumptions of a linear regression model in the context of a specific data set, interpret the model coefficients, and assess the model fit. [Lab2, Project1]
3. **Using Regression Models:** I can describe the problem of multicollinearity in regression models and explain its impact on model interpretation. I can differentiate between interpolation and extrapolation, apply each in context, and recognize potential risks or limitations when predicting beyond the observed data range. [Project1]
4. **Model Selection and R² and Adjusted R²:** I can interpret R² and adjusted R² as measures of model fit, explain the difference between them, and understand their appropriate use in different modeling contexts. I can explain the rationale for using adjusted R² or AIC for model selection and apply it to select models that balance goodness of fit with complexity. [Lab2, Project1]
5. **Categorical Variables and Indicator Variables:** I can describe how to incorporate categorical variables in a regression model using indicator (dummy) variables and interpret the corresponding coefficients in relation to the reference category. [Project1]
6. **Logistic Regression Models:** I can explain the logistic regression model, justify its use for predicting binary outcomes, describe how it is fit using maximum likelihood estimation, and interpret the coefficients in terms of odds ratios. [Project1]

---

## Module 2: Statistical Inference
*(IMS Chapters 11–23 [partial]; 4 objectives)*

1. **[code] Approaches to Inference:** I can describe the important mathematical and computational approaches to approximating sampling distributions, including when to use them and how to use the results to compute p-values and construct confidence intervals. [Project2]
2. **[code] Simulation-based Inference:** Given an inference problem of interest, I can determine how to use randomization or bootstrapping to solve that problem. [Project2]
3. **Analysis of Variance (ANOVA):** I can use mathematical and computational approaches to compare multiple group means using ANOVA, including checking assumptions like normality and homogeneity of variances.
4. **Testing Independence:** I can use mathematical and computational approaches to assess independence in a two-way table.

---

## Module 3: Regression Inference
*(IMS Chapters 24–27 + supplements; 6 objectives)*

1. **[code] Randomization Tests and Bootstrap Confidence Intervals in Regression:** I can use randomization and bootstrap methods in R to carry out tests and construct confidence intervals for the slope of a simple linear regression model, compare these with model-based approaches, and interpret the results within the context of the problem. [Project2]
2. **[code] Model Checking and Remedial Measures in Linear Regression:** I can assess whether the assumptions (e.g., linearity, independence, homoscedasticity) for hypothesis tests and confidence intervals in linear regression are satisfied, and apply appropriate remedial techniques (e.g., transformations) when necessary. [Project1, Project2]
3. **[code] Interpreting Software Output for Multiple Regression:** I can interpret the output of statistical software for multiple regression models and use it for performing hypothesis tests and constructing confidence intervals. [Project1]
4. **Building and Interpreting Non-linear and Interaction Models:** I can build regression models in R that include non-linear relationships and interaction terms, and accurately interpret the meaning of the coefficients in these more complex models. [Project1]
5. **Prediction and Cross-Validation for Model Comparison:** I can describe the challenges prediction with multiple regression models and how to implement cross-validation to evaluate and compare the predictive accuracy of regression models and explain why cross-validation is a statistically sound approach.
6. **Logistic Regression Inference:** I can carry out tests and construct confidence intervals for the coefficients of a logistic regression model, including interpreting output from software. [Project1]

---

## Module 4: Bayesian Statistics
*(*Bayes Rules!* Chapters 1–9; 6 objectives)*

1. **[code] Bayesian Inference:** I can construct probability models for events and discrete distributions, and compute marginal and conditional probabilities within these models. I can explain how Bayesian analyses can be updated sequentially as new data arrives and how the results remain invariant to the order in which the data is observed. [Lab3]
2. **Comparing Bayesian and Frequentist Approaches:** I can describe the key differences between Bayesian and frequentist approaches to statistical inference and reasoning, including providing examples of when each method is appropriate.
3. **Conjugate Models:** I can explain the features, advantages, and limitations of conjugate models in Bayesian analysis. I can interpret conjugate models, explaining the role of the prior and posterior distributions and assessing the relative influence of prior information versus the data likelihood. [Lab4]
4. **[code] Inference and Prediction:** I can use posterior distributions to estimate parameters, compare models, construct credible intervals, and make predictions, interpreting the results in the context of the data and problem. [Lab5]
5. **Using Markov Chain Monte Carlo:** I can use trace plots, acceptance probabilities, and R to diagnose Markov chain Monte Carlo convergence, including estimating the burn-in period and determining whether the step size is too large or too small. [Lab5]
6. **Bayesian Linear Regression:** I can construct and interpret prior distributions for linear regression models, interpret posterior distributions, and build posterior predictive distributions. [Lab5]

---

## Module 5: Global Module
*(Shared with MA 213; 4 objectives)*

1. I can carry out a complete, reproducible statistical workflow in R (load data, run exploratory analyses, transform variables, run statistical analyses, display and interpret results) using the frequentist methods from the course. [Lab1, Lab2, Project1, Project2, Lab3, Lab4, Lab5]
2. Given R code for a statistical analysis, I can explain what it does and why (in terms of the methods from the course), and identify both programming and statistical errors. [Lab1, Lab2, Project1, Project2, Lab3, Lab4, Lab5]
3. When solving probability and statistics problems, I can support my answers by writing out the steps using the notation and conventions of statistical exposition. [Project1, Project2]
4. I can recognize whether a statistical workflow is appropriate for the given data and data analysis goals, and explain the results of a statistical analysis to stakeholders
   1. as a presentation, and [Project1]
   2. in a written report. [Project2]
