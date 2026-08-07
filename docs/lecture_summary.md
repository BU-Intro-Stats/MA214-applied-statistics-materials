# MA 214 Lecture Summary

## Module 1: Modeling and Linear Regression

### Lecture 1: Introduction
- **Topic:** Overview of course content, Review of key concepts (probability theory, data-generating / sampling distributions, goals of inference) 
- **Reading:** 
- **Learning Objectives:**

### Lecture 2: Statistical Modeling 
- **Topic:** Probability models and likelihoods, Maximum likelihood, Applications to estimating a proportion, mean, variance
- **Reading:** 
- **Learning Objectives:**

### Lecture 3: Linear regression model
- **Topic:** Linear regression model and parameter interpretation, Visualization and interpretation, MLE and least squares 
- **Reading:** IMS Ch. 7.1–7.2
- **Learning Objectives:**
  - M1.L01: I can carry out a complete, reproducible statistical workflow in R (load data, run exploratory analyses, transform variables, run statistical analyses, display and interpret results) using the frequentist methods from the course
  - M1.L02: Given R code for a statistical analysis, I can explain what it does and why (in terms of the methods from the course), and identify both programming and statistical errors 

### Lecture 4: Using linear regression
- **Topic:** R² and correlation, prediction and extrapolation, categorical predictors
- **Reading:** IMS Ch. 7.3–7.4
- **Learning Objectives:**
  - M1.L02: I can explain the meaning and assumptions of a linear regression model in the context of a specific data set, interpret the model coefficients, and assess the model fit.
  - M1.L03: I can describe the problem of multicollinearity in regression models and explain its impact on model interpretation. I can differentiate between interpolation and extrapolation, apply each in context, and recognize potential risks or limitations when predicting beyond the observed data range.
  - M1.L04: I can interpret R² and adjusted R² as measures of model fit, explain the difference between them, and understand their appropriate use in different modeling contexts. I can explain the rationale for using adjusted R² or AIC for model selection and apply it to select models that balance goodness of fit with complexity.

### Lecture 5: Multiple regression
- **Topic:** Multiple regression model and parameter interpretation, categorical and indicator variables, R² and adjusted R²
- **Reading:** IMS Ch. 8.1–8.3
- **Learning Objectives:**
  - M1.L03: I can describe the problem of multicollinearity in regression models and explain its impact on model interpretation. I can differentiate between interpolation and extrapolation, apply each in context, and recognize potential risks or limitations when predicting beyond the observed data range.
  - M1.L04: I can interpret R² and adjusted R² as measures of model fit, explain the difference between them, and understand their appropriate use in different modeling contexts. I can explain the rationale for using adjusted R² or AIC for model selection and apply it to select models that balance goodness of fit with complexity.
  - M1.L05: I can describe how to incorporate categorical variables in a regression model using indicator (dummy) variables and interpret the corresponding coefficients in relation to the reference category.

### Lecture 6: Logistic regression
- **Topic:** Logistic regression model and parameter interpretation, maximum likelihood estimation, unbalanced data
- **Reading:** IMS Ch. 9.1–9.2 and 9.4
- **Learning Objectives:**
  - M1.L06: I can explain the logistic regression model, justify its use for predicting binary outcomes, describe how it is fit using maximum likelihood estimation, and interpret the coefficients in terms of odds ratios.

### Lecture 7: Model selection
- **Topic:** Multiple logistic regression, multicollinearity, model selection with adjusted R² and AIC
- **Reading:** IMS Chs. 8.4 and 9.3
- **Learning Objectives:**
  - M1.L03: I can describe the problem of multicollinearity in regression models and explain its impact on model interpretation. I can differentiate between interpolation and extrapolation, apply each in context, and recognize potential risks or limitations when predicting beyond the observed data range.
  - M1.L04: I can interpret R² and adjusted R² as measures of model fit, explain the difference between them, and understand their appropriate use in different modeling contexts. I can explain the rationale for using adjusted R² or AIC for model selection and apply it to select models that balance goodness of fit with complexity.

---

## Module 2: Statistical Inference

### Lecture 8: Hypothesis testing with randomization
- **Topic:** Hypothesis testing review and decision errors, statistics and sampling distributions, hypothesis testing with randomization
- **Reading:** IMS Chs. 11 and 14
- **Learning Objectives:**
  - M2.L01: I can describe the important mathematical and computational approaches to approximating sampling distributions, including when to use them and how to use the results to compute p-values and construct confidence intervals.
  - M2.L02: Given an inference problem of interest, I can determine how to use randomization or bootstrapping to solve that problem.

### Lecture 9: Confidence intervals with bootstrapping
- **Topic:** Confidence interval review, bootstrap distributions, percentile and standard-error intervals
- **Reading:** IMS Ch. 12
- **Learning Objectives:**
  - M2.L01: I can describe the important mathematical and computational approaches to approximating sampling distributions, including when to use them and how to use the results to compute p-values and construct confidence intervals.
  - M2.L02: Given an inference problem of interest, I can determine how to use randomization or bootstrapping to solve that problem.

### Lecture 10: Inference with mathematical models
- **Topic:** Exact calculations with normal and binomial models, central limit theorem, two-sample and paired data
- **Reading:** IMS Ch. 13
- **Learning Objectives:**
  - M2.L01: I can describe the important mathematical and computational approaches to approximating sampling distributions, including when to use them and how to use the results to compute p-values and construct confidence intervals.
  - M2.L02: Given an inference problem of interest, I can determine how to use randomization or bootstrapping to solve that problem.

### Lecture 11: Testing for independence
- **Topic:** Two-way tables, expected counts, randomization testing, chi-square testing
- **Reading:** IMS Ch. 18
- **Learning Objectives:**
  - M2.L04: I can use mathematical and computational approaches to assess independence in a two-way table.

### Lecture 12: Analysis of variance
- **Topic:** Comparing multiple means, randomization testing, mathematical testing, post-hoc comparisons
- **Reading:** IMS Ch. 22
- **Learning Objectives:**
  - M2.L03: I can use mathematical and computational approaches to compare multiple group means using ANOVA, including checking assumptions like normality and homogeneity of variances.

---

## Module 3: Regression Inference

### Lecture 13: Inference about the slope
- **Topic:** Randomization tests, bootstrap confidence intervals, mathematical approaches, checking model conditions
- **Reading:** IMS Ch. 24
- **Learning Objectives:**
  - M3.L01: I can use randomization and bootstrap methods in R to carry out tests and construct confidence intervals for the slope of a simple linear regression model, compare these with model-based approaches, and interpret the results within the context of the problem.
  - M3.L02: I can assess whether the assumptions (e.g., linearity, independence, homoscedasticity) for hypothesis tests and confidence intervals in linear regression are satisfied, and apply appropriate remedial techniques (e.g., transformations) when necessary.

### Lecture 14: Inference for multiple regression
- **Topic:** Inference using software output, nonlinear and interaction models, transformations, multicollinearity
- **Reading:** IMS Ch. 25.1–25.2 and the nonlinear and interaction supplements
- **Learning Objectives:**
  - M3.L03: I can interpret the output of statistical software for multiple regression models and use it for performing hypothesis tests and constructing confidence intervals.
  - M3.L04: I can build regression models in R that include nonlinear relationships and interaction terms, and accurately interpret the meaning of the coefficients in these more complex models.

### Lecture 15: Prediction and cross-validation
- **Topic:** Confidence and prediction intervals, transformed and polynomial models, extrapolation, cross-validation
- **Reading:** IMS Ch. 25 and the prediction and cross-validation supplement
- **Learning Objectives:**
  - M3.L04: I can build regression models in R that include nonlinear relationships and interaction terms, and accurately interpret the meaning of the coefficients in these more complex models.
  - M3.L05: I can describe the challenges of prediction with multiple regression models and how to implement cross-validation to evaluate and compare the predictive accuracy of regression models, and explain why cross-validation is a statistically sound approach.

### Lecture 16: Logistic regression inference
- **Topic:** Software output, coefficient and prediction intervals, diagnostics, prediction and cross-validation
- **Reading:** IMS Ch. 26
- **Learning Objectives:**
  - M3.L06: I can carry out tests and construct confidence intervals for the coefficients of a logistic regression model, including interpreting output from software.

---

## Module 4: Bayesian Statistics

### Lecture 17: Review of Bayesian statistics
- **Topic:** Conditional probability and Bayes' rule, prior distributions, sequential updating, Bayesian and frequentist comparisons
- **Reading:** Bayes Rules! Chs. 1, 2, and 4
- **Learning Objectives:**
  - M4.L01: I can construct probability models for events and discrete distributions, and compute marginal and conditional probabilities within these models. I can explain how Bayesian analyses can be updated sequentially as new data arrives and how the results remain invariant to the order in which the data is observed.
  - M4.L02: I can describe the key differences between Bayesian and frequentist approaches to statistical inference and reasoning, including providing examples of when each method is appropriate.

### Lecture 18: Conjugate models
- **Topic:** Beta-binomial, normal-normal, and gamma-Poisson models; conjugate derivations; prior and likelihood weights
- **Reading:** Bayes Rules! Chs. 3 and 5
- **Learning Objectives:**
  - M4.L03: I can explain the features, advantages, and limitations of conjugate models in Bayesian analysis. I can interpret conjugate models, explaining the role of the prior and posterior distributions and assessing the relative influence of prior information versus the data likelihood.

### Lecture 19: Posterior approximation
- **Topic:** Grid approximation, Markov chain Monte Carlo, trace plots, burn-in, step size, R-hat, ESS, and MCSE
- **Reading:** Bayes Rules! Chs. 6 and 7
- **Learning Objectives:**
  - M4.L05: I can use trace plots, acceptance probabilities, and R to diagnose Markov chain Monte Carlo convergence, including estimating the burn-in period and determining whether the step size is too large or too small.

### Lecture 20: Posterior inference and prediction
- **Topic:** Point estimates, credible intervals, hypothesis testing, model comparison, posterior prediction, predictive checking
- **Reading:** Bayes Rules! Ch. 8
- **Learning Objectives:**
  - M4.L04: I can use posterior distributions to estimate parameters, compare models, construct credible intervals, and make predictions, interpreting the results in the context of the data and problem.

### Lecture 21: Bayesian linear regression
- **Topic:** Priors for regression parameters, prior predictive checks, posterior inference, credible intervals, posterior prediction
- **Reading:** Bayes Rules! Ch. 9
- **Learning Objectives:**
  - M4.L06: I can construct and interpret prior distributions for linear regression models, interpret posterior distributions, and build posterior predictive distributions.
