

Bayesian Modeling of Diabetes in Pima Indians

A comprehensive analysis using Bayesian methods to model and predict the prevalence of Type 2 Diabetes among the Pima Indian population. This project leverages logistic regression and Naive Bayes classifiers to explore key risk factors and build a robust predictive model.

Project Motivation-----------------------------------------------------------------------------------------

The Pima Indian population of Arizona has one of the highest reported incidences of Type 2 Diabetes in the world, making it a critical area of study. The primary goal of this project is to apply Bayesian statistical modeling to the Pima Indians Diabetes Dataset to:

Identify the key physiological and genetic factors that contribute to the onset of diabetes.

Build and evaluate predictive models to accurately classify patients.

Compare the performance of different modeling approaches, namely Bayesian Logistic Regression and the Naive Bayes classifier.

Test specific hypotheses regarding risk factors using Bayesian inference.

This project serves as a practical application of advanced statistical techniques to a real-world public health problem.

Dataset Description---------------------------------------------------------------------------------------

The analysis is performed on the PimaIndiansDiabetes2 dataset, sourced from the mlbench package in R. This dataset was originally collected by the US National Institute of Diabetes and Digestive and Kidney Diseases. It contains data from 768 female patients of Pima Indian heritage.

Predictor Variables:

pregnant: Number of times pregnant.

glucose: Plasma glucose concentration after a 2-hour oral glucose tolerance test.

pressure: Diastolic blood pressure (mm Hg).

triceps: Triceps skinfold thickness (mm), an indicator of body fat.

insulin: 2-Hour serum insulin (mu U/ml).

mass: Body Mass Index (BMI).

pedigree: Diabetes pedigree function, a measure of genetic influence.

age: Age in years.

Target Variable:

diabetes: pos (has diabetes) or neg (does not have diabetes).

Data pre-processing involved imputing missing values for key variables using the median of each respective column.

Methodology-----------------------------------------------------------------------------------------------

Two primary Bayesian modeling approaches were implemented and compared:

Bayesian Logistic Regression:

Implemented using the rstanarm package, which leverages Stan for MCMC simulations.

A primary model (model_diabetes) was constructed using all predictors.

Several variations were tested, including a model with all pairwise interactions (model_diabetes_pairwise), to explore complex relationships between variables.

Model convergence was thoroughly checked using MCMC diagnostics (trace plots, density plots, autocorrelation).

Naive Bayes Classifier:

Implemented using the bayesrules package.

This model was used as a comparative baseline, assuming conditional independence between predictors.

Multiple versions were tested with different subsets of predictors to evaluate performance.

Model Comparison:
Models were rigorously compared using 5-fold cross-validation and the Expected Log Predictive Density (ELPD) via the loo package to assess out-of-sample predictive accuracy.

Key Analyses Performed--------------------------------------------------------------------------------------

Exploratory Data Analysis (EDA): Initial analysis of summary statistics, variable distributions, and correlations.

Multicollinearity Check: Variance Inflation Factor (VIF) was calculated to ensure the stability of the logistic regression model.

MCMC Diagnostics: Ensured the reliability of the Bayesian models by confirming the convergence of the MCMC chains.

Performance Evaluation: Assessed models based on overall accuracy, sensitivity (recall), specificity, and the Area Under the ROC Curve (AUC).

Hypothesis Testing: Used Bayes Factors to formally test hypotheses about the influence of pregnancy and genetic predisposition on diabetes risk.

Technical Stack--------------------------------------------------------------------------------------

Language: R

Core Packages:

rstanarm: For Bayesian regression modeling via Stan.

tidyverse: For data manipulation and visualization.

bayesplot & tidybayes: For visualizing MCMC output and posterior distributions.

pROC: For calculating and plotting ROC curves.

corrplot: For visualizing the correlation matrix.

car: For calculating VIF.

bayesrules: For implementing the Naive Bayes classifier.

Installation & Usage ----------------------------------------------------------------------------------------

Clone the repository:

git clone [https://github.com/your-username/your-repo-name.git](https://github.com/your-username/your-repo-name.git)
cd your-repo-name

Open the R Project:
Open the .Rproj file in RStudio.

Run the Script:
The file BAAN_Zaverecny_projekt_Diabetes_Pima_indiani_skript.R contains the complete analysis pipeline. You can run the script from top to bottom to replicate all results, including data pre-processing, model fitting, diagnostics, and evaluation. Ensure you have installed all the packages listed at the top of the script.

Model Performance & Results ---------------------------------------------------------------------------------

The primary logistic regression model (model_diabetes) demonstrated strong predictive power and was selected as the optimal model based on a balance of performance (via ELPD) and interpretability.

model_diabetes Performance:

Metric

Value

Overall Accuracy

77.5%

Sensitivity

56.7%

Specificity

88.6%

AUC

0.84

The model shows excellent performance in correctly identifying patients without diabetes (high specificity), and good overall classification ability (AUC of 0.84). The Naive Bayes model showed slightly higher sensitivity but lower overall accuracy, likely due to violations of its conditional independence assumption.

Hypothesis Testing ------------------------------------------------------------------------------------------

The analysis formally tested two key hypotheses using Bayes Factors:

Hₐ: The probability of diabetes is higher in women who have been pregnant.

Result: The data provided strong evidence in favor of this hypothesis (Bayes Factor ≈ 1.98). Pregnancy is a significant risk factor.

Hₐ: A higher genetic predisposition increases the probability of diabetes.

Result: The data showed overwhelming evidence in favor of this hypothesis (Bayes Factor ≈ 266). The pedigree variable is a very strong predictor.

Conclusion & Key Findings -----------------------------------------------------------------------------------

This project successfully applied Bayesian methods to develop a predictive model for diabetes in the Pima Indian population.

The Bayesian logistic regression model proved to be a robust and accurate tool, achieving an AUC of 0.84.

Key predictors, particularly glucose, mass (BMI), pedigree, and pregnant, were identified as significant risk factors.

Formal hypothesis testing confirmed that both pregnancy history and genetic predisposition substantially increase the risk of developing diabetes.

The findings from this analysis align with established medical research and demonstrate the power of Bayesian inference in extracting meaningful insights from complex health data.
