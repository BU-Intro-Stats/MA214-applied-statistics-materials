# MA 214 Labs and Projects

## Lab and project details

### Lab 1: Working with R and Data

- **Week:** 1
- **Lecture Anchor:** Lecture 1
- **Purpose:** Establish the R workflow used throughout the course and review basic data exploration with flights data.
- **Primary Objectives:**
  - Carry out a complete, reproducible statistical workflow in R
  - Import, inspect, summarize, visualize, and interpret a data set
- **Pre-Lab Activity:**
  - Introduce yourself to a partner and identify what one row represents in a familiar data set.
- **In-Lab Activity:**
  - Open `lab-starter.R` and import `flights.csv` without using `setwd()`.
  - Inspect dimensions, names, structure, and the meaning of `origin`, `dest`, and `air_time`.
  - Predict which NYC airport has the most flights, then verify with grouped counts.
  - Summarize destinations and mean air time by month.
  - Interpret the strongest comparison in context and identify a limitation.
  - Optionally plot air time across months or origin airports and save it with `ggsave()`.
- **Post-Lab Activity:**
  - Write a final claim about the flights data, cite evidence, and explain the context.
- **Deliverables:**
  - Completed `lab-starter.R` with the requested objects and plots.
  - Completed in-lab activity response sheet.

---

### Lab 2: Regression Models

- **Week:** 2
- **Lecture Anchor:** Lecture 3
- **Purpose:** Connect regression computation in R to the Module 1 modeling lectures.
- **Primary Objectives:**
  - Understanding and Applying Linear Regression Models
  - Use R for a reproducible statistical workflow
  - Compare regression models using summaries, diagnostics, and R-squared
- **Pre-Lab Activity:**
  - Introduce yourself to a partner and identify a variable that might predict a car's miles per gallon.
- **In-Lab Activity:**
  - Import `mtcars.csv` and inspect the response and candidate predictors.
  - Plot `mpg` against `wt` and compute their correlation.
  - Predict the sign of the slope, then fit `mpg ~ wt`.
  - Fit `mpg ~ hp` and `mpg ~ qsec` and compare the three models using summaries and R-squared.
  - Interpret the strongest simple model and identify a limitation or caution.
  - Optionally fit `mpg ~ wt + hp` and compare it with the best simple model.
- **Post-Lab Activity:**
  - Write a final response naming the chosen model, citing evidence, and interpreting it in context.
- **Deliverables:**
  - Completed `lab-starter.R` with the fitted models and requested autograder objects.
  - Completed in-lab activity response sheet.

---

### Project1-1: Project 1 Launch and Outline

- **Week:** 3
- **Lecture Anchor:** Lecture 4
- **Purpose:** Launch the statistical analysis and model-application project.
- **Primary Objectives:**
  - Understanding and Applying Linear Regression Models
  - Explain statistical results in a presentation
- **Pre-Lab Activity:**
  - Review the Project 1 overview, rubric, required sections, and final submission format.
- **In-Lab Activity:**
  - Create and share one group Google Doc named `P1_GroupNumber_Outline`.
  - Add names and BUIDs and assign project, data, analysis, and editor/scheduler roles.
  - Select a data set and record its source and access information.
  - Draft and prioritize research questions, objectives, or hypotheses.
  - Identify the response, candidate predictors, initial summaries/plots, and limitations.
  - Peer-edit the complete draft and ask the instructor/TA to check feasibility.
- **Post-Lab Activity:**
  - Confirm that the document is readable, comments are resolved, all members contributed, and the link is accessible.
- **Deliverables:**
  - Completed Project 1 outline submitted through the course platform.
  - Shared Google Doc containing the title, motivation, questions, data description, limitations, roles, and timeline.

---

### Project1-2: Project 1 Data Cleaning and EDA

- **Week:** 4
- **Lecture Anchor:** Lecture 5
- **Purpose:** Build a clean and interpretable foundation for the project analysis.
- **Primary Objectives:**
  - Use R for a reproducible statistical workflow
  - Categorical Variables and Indicator Variables
- **Pre-Lab Activity:**
  - Open the Project 1 outline and identify the data, response, predictors, and unresolved data questions.
- **In-Lab Activity:**
  - Import and inspect dimensions, variable types, missing values, and unusual entries.
  - Rename variables, recode categories, create useful derived variables, and document decisions.
  - Summarize the response and candidate predictors.
  - Create exploratory plots connected to the research question and model assumptions.
  - Record the most important cleaning decision and revise the research questions if needed.
- **Post-Lab Activity:**
  - Update the shared outline with cleaned-data notes, initial EDA, and revised research questions.
- **Deliverables:**
  - Project 1 progress materials submitted through the course platform.
  - Cleaned-data notes, initial EDA, revised questions, and accessible shared Google Doc link.

---

### Project1-3: Project 1 Modeling and Diagnostics

- **Week:** 5
- **Lecture Anchor:** Lecture 7
- **Purpose:** Select and diagnose a final modeling strategy.
- **Primary Objectives:**
  - Model Selection, R-squared, and Adjusted R-squared
  - Logistic Regression Models
- **Pre-Lab Activity:**
  - Review the EDA and identify candidate models and assumptions that require checking.
- **In-Lab Activity:**
  - Fit linear, multiple regression, or logistic regression models as appropriate.
  - Compare fit, prediction, and interpretability using adjusted R-squared, AIC, residual behavior, and other relevant evidence.
  - Check residuals, influential observations, transformations, and classification summaries as appropriate.
  - Translate key coefficients or predictions into language connected to the research question.
  - Select a justified modeling direction and document remaining concerns.
- **Post-Lab Activity:**
  - Update the shared report with candidate models, diagnostics, interpretation, and the modeling decision.
- **Deliverables:**
  - Project 1 progress report submitted through the course platform.
  - Candidate models, diagnostics, justified modeling direction, and accessible shared Google Doc link.

---

### Project1-4: Project 1 Presentation Work Session

- **Week:** 6
- **Lecture Anchor:** Lecture 8
- **Purpose:** Prepare a clear, reproducible presentation of the Project 1 analysis.
- **Primary Objectives:**
  - Explain statistical results in a presentation
- **Pre-Lab Activity:**
  - Review the final model, key plots, statistical interpretation, and known limitations.
- **In-Lab Activity:**
  - Confirm the final model, plots, and interpretation.
  - Build a concise presentation covering motivation, data, method, results, limitations, and conclusion.
  - Rehearse, revise wording and visuals, and check timing.
  - Confirm that code runs and figures/tables are reproducible.
- **Post-Lab Activity:**
  - Rehearse the final presentation and verify that all four members contributed.
- **Deliverables:**
  - Project 1 final presentation materials or video submitted through the course platform.
  - Shared script/document link and reproducible supporting files.

---

### Project2-1: Bootstrapping and Study Selection

- **Week:** 7
- **Lecture Anchor:** Lecture 9
- **Purpose:** Launch the project on the reliability of linear regression inference.
- **Primary Objectives:**
  - Simulation-based Inference
  - Randomization Tests and Bootstrap Confidence Intervals in Regression
- **Pre-Lab Activity:**
  - Review the Project 2 overview and rubric and identify the required simulation and real-data components.
- **In-Lab Activity:**
  - Create and share one group Google Doc named `P2_GroupNumber_Outline` and assign group roles.
  - Implement or review a bootstrap confidence interval for a regression quantity.
  - Select two studies (A-D), explain their assumption violations, and justify the comparison.
  - Select a real data set, response Y, predictor X, source, and expected violations.
  - Add sample sizes, repetitions, data-generating processes, outputs, roles, and timeline.
  - Ask the instructor/TA to review the study choices and simulation/data connection.
- **Post-Lab Activity:**
  - Confirm the outline is complete, readable, accessible, and reviewed by every member.
- **Deliverables:**
  - Completed Project 2 outline submitted through the course platform.
  - Shared document with study plans, research questions, real-data plan, timeline, roles, and planned outputs.

---

### Project2-2: Simulation Design

- **Week:** 8
- **Lecture Anchor:** Lecture 10
- **Purpose:** Design simulation studies that compare classical and bootstrap inference.
- **Primary Objectives:**
  - Approaches to Inference
  - Simulation-based Inference
- **Pre-Lab Activity:**
  - Review the two selected studies, violations, data-generating processes, and research questions.
- **In-Lab Activity:**
  - Specify scenario assumptions and parameter values.
  - Code one simulated data set, then wrap it in repeated simulation.
  - Store classical intervals, bootstrap intervals, coverage indicators, and interval widths.
  - Decide which tables and plots will summarize the simulation results.
  - Define how the group will judge whether one inference method performs better.
- **Post-Lab Activity:**
  - Review the algorithm and outputs for reproducibility and update the shared plan.
- **Deliverables:**
  - Project 2 simulation-design materials submitted through the course platform.
  - Simulation algorithm, planned outputs, shared document link, and reproducible code files.

---

### Project2-3: Real Data Analysis

- **Week:** 9
- **Lecture Anchor:** Lecture 13
- **Purpose:** Connect simulation results to a real regression analysis.
- **Primary Objectives:**
  - Model Checking and Remedial Measures in Linear Regression
  - Interpret Software Output for Multiple Regression
- **Pre-Lab Activity:**
  - Review the planned real-data model and identify the simulation scenarios most relevant to its assumptions.
- **In-Lab Activity:**
  - Import and inspect the response, predictors, missing values, and variable types.
  - Fit the planned regression model and interpret the target coefficient or prediction.
  - Compare classical and bootstrap intervals for the target quantity.
  - Check residual patterns and connect diagnostics to the simulation assumptions.
  - Decide whether the real data resemble one of the simulated scenarios.
- **Post-Lab Activity:**
  - Update the report with the real-data analysis, diagnostics, and connection to the simulations.
- **Deliverables:**
  - Project 2 progress report submitted through the course platform.
  - Real-data analysis, diagnostics, shared document link, and reproducible code files.

---

### Project2-4: Final Work Session

- **Week:** 10
- **Lecture Anchor:** Lecture 15
- **Purpose:** Complete the simulation results, real-data analysis, interpretation, and written report.
- **Primary Objectives:**
  - Prediction and Cross-Validation for Model Comparison
  - Explain statistical results in a written report
- **Pre-Lab Activity:**
  - Review the complete simulation and real-data results and identify unresolved interpretation or reproducibility issues.
- **In-Lab Activity:**
  - Verify that all simulation scenarios run reproducibly and summarize key results.
  - Finalize the real-data model, intervals, diagnostics, and interpretation.
  - Explain when classical inference worked well, when bootstrap inference helped, and remaining limitations.
  - Assemble the report, code, data, output files, and shared link.
- **Post-Lab Activity:**
  - Have every member review the complete submission before upload.
- **Deliverables:**
  - Project 2 writeup, code, and required supporting files submitted through the course platform.
  - Shared Google Doc link and confirmation of group review.

---

### Lab 3: Bayes' Rule

- **Week:** 11
- **Lecture Anchor:** Lecture 17
- **Purpose:** Practice Bayesian updating in a discrete coin-flip setting.
- **Primary Objectives:**
  - Bayesian Inference
  - Comparing Bayesian and Frequentist Approaches
- **Pre-Lab Activity:**
  - Consider what an observed sequence of heads suggests about five possible coin types before formal calculation.
- **In-Lab Activity:**
  - Load the assigned coin file and compute flips, heads, and the sample proportion.
  - Use equal prior probabilities and `dbinom()` to compute a batch posterior.
  - Update the posterior one flip at a time and compare it with the batch result.
  - Interpret the most likely coin type, posterior probability, and limitations.
  - Optionally plot posterior probabilities after every flip.
- **Post-Lab Activity:**
  - State a coin-type claim, cite one posterior probability, and explain the context.
- **Deliverables:**
  - Completed `lab-starter.R` with the required posterior objects.
  - Completed posterior-probability practice or Tutorial 3 hash, as assigned.

---

### Lab 4: Beta-Binomial Conjugate Models

- **Week:** 12
- **Lecture Anchor:** Lecture 18
- **Purpose:** Interpret prior, likelihood, and posterior relationships in a conjugate model.
- **Primary Objectives:**
  - Conjugate Models
- **Pre-Lab Activity:**
  - Choose and justify a Beta prior for the probability of heads before seeing coin data.
- **In-Lab Activity:**
  - Load the assigned coin file and compute flips, heads, tails, and the sample proportion.
  - Choose prior parameters and explain how much the prior should influence the answer.
  - Compute the Beta-Binomial posterior mean and credible interval.
  - Update the Beta parameters one flip at a time and compare with the batch posterior.
  - Interpret the posterior estimate and credible interval in context.
  - Optionally plot the posterior density with its mean and interval.
- **Post-Lab Activity:**
  - State an estimate of pi, cite a credible interval, and explain the context.
- **Deliverables:**
  - Completed `lab-starter.R` with the requested posterior summaries.
  - Completed conjugate-model practice or Tutorial 4 hash, as assigned.

---

### Lab 5: Bayesian Linear Regression

- **Week:** 14
- **Lecture Anchor:** Lecture 21
- **Purpose:** Connect priors, posterior inference, and prediction in a regression model.
- **Primary Objectives:**
  - Inference and Prediction
  - Bayesian Linear Regression
- **Pre-Lab Activity:**
  - Use the scatter plot and prior for the slope to predict the direction of the customer-revenue relationship.
- **In-Lab Activity:**
  - Load the coffee data and plot revenue against customers.
  - Identify the response, predictor, and model parameters.
  - Review the log-posterior using normal priors and a log-normal variance prior.
  - Run the provided Metropolis-Hastings code and summarize post-burn-in draws.
  - Compute the posterior credible interval for the slope.
  - Use posterior samples to predict revenue for a new day with three customers.
  - Interpret the posterior slope, predictive result, and limitations.
- **Post-Lab Activity:**
  - State a posterior predictive claim, cite one posterior summary, and explain the context.
- **Deliverables:**
  - Completed `lab-starter.R` with the data, log-posterior, credible interval, and posterior predictive mean objects.
  - Completed Bayesian-regression practice or Tutorial 5 hash, as assigned.
