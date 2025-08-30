library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(broom.mixed)
library(mlbench)
install.packages("pROC")
library(pROC)
install.packages("corrplot")
library(corrplot)
library(car)

data("PimaIndiansDiabetes2", package = "mlbench")

diabetes_USA <- PimaIndiansDiabetes2

par(mfrow = c(2, 4))
hist(diabetes_USA$glucose, main = "glucose", xlab = "Values")
hist(diabetes_USA$pressure, main = "pressure", xlab = "Values")
hist(diabetes_USA$triceps, main = "triceps", xlab = "Values")
hist(diabetes_USA$insulin, main = "insulin", xlab = "Values")
hist(diabetes_USA$mass, main = "mass", xlab = "Values")
# Data, except for pressure, are skewed, so we will use the median to fill missing values.
# (For pressure, the difference between median and mean is about 0.4, so we'll stick with the median).

diabetes_USA_imputed <- PimaIndiansDiabetes2

variables_to_impute <- c("glucose", "pressure", "triceps", "insulin", "mass")

# For each variable, replace NA with the median
for (var in variables_to_impute) {
  diabetes_USA_imputed[[var]][is.na(diabetes_USA_imputed[[var]])] <- median(diabetes_USA_imputed[[var]], na.rm = TRUE)
}

diabetes_USA_imputed

# Transform 'pos' and 'neg' to 1 and 0
diabetes_USA_imputed$diabetes <- ifelse(diabetes_USA$diabetes == "pos", 1, 0)

summary(diabetes_USA_imputed)

############# Multicollinearity and Correlation ########################################

# correlations
str(diabetes_USA_imputed)
correlations <- cor(diabetes_USA_imputed[, c("pregnant", "glucose", "pressure", 
                                             "triceps", "insulin", "mass", 
                                             "pedigree", "age")])

# correlation matrix
cor_matrix <- cor(diabetes_USA_imputed[, c("pregnant", "glucose", "pressure", 
                                           "triceps", "insulin", "mass", 
                                           "pedigree", "age")])

# correlogram
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.cex = 0.8, # text label size
         tl.col = "black", # text label color
         diag = FALSE) # Hide the diagonal

# Multicollinearity ######################################################################
# VIF
# define a linear model
lm_model <- lm(glucose ~ pregnant + pressure + glucose + triceps + insulin + mass + pedigree + age, 
               data = diabetes_USA_imputed)

# calculate VIF for the model
vif_values <- vif(lm_model)
print(vif_values)

# calculate eigenvalues
numeric_data <- diabetes_USA_imputed[, c("pregnant", "glucose", "pressure", 
                                         "triceps", "insulin", "mass", 
                                         "pedigree", "age")]

eigenvalues <- eigen(cor(numeric_data))$values
print(eigenvalues)

################ First Model ################################################################

# Posterior model
# We believe that 22.9% of Pima Indian women have diabetes -> log(0.229/(1-0.229)) = -1.21
model_diabetes <- stan_glm(diabetes ~ pregnant + glucose
                           + pressure + triceps + insulin + mass
                           + pedigree + age,
                           data = diabetes_USA_imputed, family = binomial,
                           prior_intercept = normal(-1.21, 1),
                           prior = normal(0, 1, autoscale = TRUE),
                           chains = 4, iter = 5000*2, seed = 84735)


mcmc_trace(model_diabetes)
mcmc_dens_overlay(model_diabetes)
mcmc_acf(model_diabetes)

# numerical diagnostics

# extraction
rhat_values <- rhat(model_diabetes)

# print
print(rhat_values)

pp_check(model_diabetes)

posterior_interval(model_diabetes, prob = 0.80)

exp(posterior_interval(model_diabetes, prob = 0.80))

# some variables have confidence intervals containing 0. Should they be removed or ignored?
tidy(model_diabetes, effects = "fixed", conf.int = TRUE, conf.level = 0.95)

classification_summary(model = model_diabetes, data = diabetes_USA_imputed, cutoff = 0.5)

# ROC curve
predicted_probs <- predict(model_diabetes, newdata = diabetes_USA_imputed, type = "response")

# calculate ROC curve
roc_curve <- roc(diabetes_USA_imputed$diabetes, predicted_probs)

plot(roc_curve, main = "ROC Curve for Model 1A", col = "blue", lwd = 2)

# add AUC to the plot
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lwd = 2)

# Calculate AUC
auc_value <- auc(roc_curve)
print(auc_value)

# cross-validation test
cv1 <- prediction_summary_cv(model = model_diabetes, data = diabetes_USA_imputed, k = 5)

print(cv1)

###################### Variations of the First Model - Not in Final Version ###########################################################


# test of the model with insulin removed
#model_diabetes_reduced <- stan_glm(diabetes ~ pregnant + glucose + pressure + triceps
#+ mass + pedigree + age,
#data = diabetes_USA_imputed, family = binomial,
#prior_intercept = normal(-1.21, 1),
#prior = normal(0, 1, autoscale = TRUE),
#chains = 4, iter = 5000*2, seed = 84735)

#tidy(model_diabetes_reduced, effects = "fixed", conf.int = TRUE, conf.level = 0.95)

# lower accuracy than the model with all variables
#classification_summary(model = model_diabetes_reduced, data = diabetes_USA_imputed, cutoff = 0.5)

# without pressure and triceps
#model_diabetes_interaction <- stan_glm(diabetes ~ pregnant + glucose
#+ mass + pedigree + age,
#data = diabetes_USA_imputed, family = binomial,
#prior_intercept = normal(-1.21, 1),
#prior = normal(0, 1, autoscale = TRUE),
#chains = 4, iter = 5000*2, seed = 84735)


########################## Model with All Pairwise Interactions #######################################################

model_diabetes_pairwise <- stan_glm(
  diabetes ~ (pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age)^2,  
  data = diabetes_USA_imputed, 
  family = binomial,
  prior_intercept = normal(-1.21, 1),
  prior = normal(0, 1, autoscale = TRUE),
  chains = 4, 
  iter = 5000*2, 
  seed = 84735
)

posterior_interval(model_diabetes_pairwise, prob = 0.95)

summary(model_diabetes_pairwise)

results <- tidy(model_diabetes_pairwise, effects = "fixed", conf.int = TRUE, conf.level = 0.80)
# more rows
print(results, n = 37)
# coefficient above 0.1 without 0 in the credible interval only for pregnant:pedigree
print(classification_summary(model = model_diabetes_pairwise, data = diabetes_USA_imputed, cutoff = 0.5))
# overall accuracy 0.7890625
predicted_probs <- predict(model_diabetes_pairwise, newdata = diabetes_USA_imputed, type = "response")

roc_curve <- roc(diabetes_USA_imputed$diabetes, predicted_probs)
plot(roc_curve, main = "ROC Curve for Diabetes Prediction", col = "blue", lwd = 2)
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lwd = 2)

# calculate AUC
auc_value <- auc(roc_curve)
print(auc_value)

# model_diabetes2 with only the pregnant:pedigree interaction, other variables included ###########################

model_diabetes2 <- stan_glm(diabetes ~ pregnant + glucose
                            + pressure + triceps + insulin + mass
                            + pedigree + age + pregnant:pedigree,
                            data = diabetes_USA_imputed, family = binomial,
                            prior_intercept = normal(-1.21, 1),
                            prior = normal(0, 1, autoscale = TRUE),
                            chains = 4, iter = 5000*2, seed = 84735)

# Posterior interval with 95% probability
posterior_interval(model_diabetes2, prob = 0.95)


summary(model_diabetes2)

# Tidy output
tidy(model_diabetes2, effects = "fixed", conf.int = TRUE, conf.level = 0.95)

# Print classification summary 
print(classification_summary(model = model_diabetes2, data = diabetes_USA_imputed, cutoff = 0.5))


predicted_probs <- predict(model_diabetes2, newdata = diabetes_USA_imputed, type = "response")

# Compute ROC 
roc_curve <- roc(diabetes_USA_imputed$diabetes, predicted_probs)

plot(roc_curve, main = "ROC Curve for Diabetes Prediction", col = "blue", lwd = 2)

# add AUC to the plot
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lwd = 2)

# calculate AUC
auc_value <- auc(roc_curve)
print(auc_value)

#term              estimate std.error  conf.low conf.high
#<chr>                <dbl>     <dbl>     <dbl>     <dbl>
#  1 (Intercept)       -9.03      0.834   -10.7      -7.47   
#2 pregnant           0.0755    0.0504   -0.0243    0.175  
#3 glucose            0.0380    0.00387   0.0306    0.0458 
#4 pressure          -0.00925   0.00855  -0.0262    0.00760
#5 triceps            0.00478   0.0131   -0.0207    0.0303 
#6 insulin           -0.00116   0.00112  -0.00339   0.00110
#7 mass               0.0946    0.0177    0.0611    0.130  
#8 pedigree           0.497     0.418    -0.321     1.34   
#9 age                0.0133    0.00944  -0.00517   0.0318 
#10 pregnant:pedigree  0.105     0.0832   -0.0589    0.274


# model 3 without predictors that have a coefficient below the threshold of 0.01, i.e., pressure, triceps, insulin
model_diabetes3 <- stan_glm(diabetes ~ pregnant + glucose + mass
                            + pedigree + age + pregnant:pedigree,
                            data = diabetes_USA_imputed, family = binomial,
                            prior_intercept = normal(-1.21, 1),
                            prior = normal(0, 1, autoscale = TRUE),
                            chains = 4, iter = 5000*2, seed = 84735)

# Posterior interval with 95% probability
posterior_interval(model_diabetes3, prob = 0.95)

# Summary of the model
summary(model_diabetes3)

# Tidy output with fixed effects and 95% confidence interval
tidy(model_diabetes3, effects = "fixed", conf.int = TRUE, conf.level = 0.95)

# Print classification summary for the new model
print(classification_summary(model = model_diabetes3, data = diabetes_USA_imputed, cutoff = 0.5))

predicted_probs <- predict(model_diabetes3, newdata = diabetes_USA_imputed, type = "response")

# Compute the ROC curve
roc_curve <- roc(diabetes_USA_imputed$diabetes, predicted_probs)

plot(roc_curve, main = "ROC Curve for Diabetes Prediction", col = "blue", lwd = 2)

# Add AUC to the plot
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lwd = 2)

# calculate AUC
auc_value <- auc(roc_curve)
print(auc_value)


# model 4 with only glucose and mass ######################################################

model_diabetes4 <- stan_glm(diabetes ~ glucose + mass,
                            data = diabetes_USA_imputed, family = binomial,
                            prior_intercept = normal(-1.21, 1),
                            prior = normal(0, 1, autoscale = TRUE),
                            chains = 4, iter = 5000*2, seed = 84735)

# Posterior interval with 95% probability
posterior_interval(model_diabetes4, prob = 0.95)

summary(model_diabetes4)

# output
tidy(model_diabetes4, effects = "fixed", conf.int = TRUE, conf.level = 0.95)

# Print classification summary 
print(classification_summary(model = model_diabetes4, data = diabetes_USA_imputed, cutoff = 0.5))

predicted_probs <- predict(model_diabetes_pairwise, newdata = diabetes_USA_imputed, type = "response")

# calculate ROC curve
roc_curve <- roc(diabetes_USA_imputed$diabetes, predicted_probs)

plot(roc_curve, main = "ROC Curve for Diabetes Prediction", col = "blue", lwd = 2)

# add AUC to the plot
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lwd = 2)

# calculate AUC
auc_value <- auc(roc_curve)
print(auc_value)

################# CROSS-VALIDATION #######################################################################

cv1 <- prediction_summary_cv(model = model_diabetes, data = diabetes_USA_imputed, k = 5)
cv2 <- prediction_summary_cv(model = model_diabetes_pairwise, data = diabetes_USA_imputed, k = 5)
cv3 <- prediction_summary_cv(model = model_diabetes2, data = diabetes_USA_imputed, k = 5)
cv4 <- prediction_summary_cv(model = model_diabetes3, data = diabetes_USA_imputed, k = 5)
cv5 <- prediction_summary_cv(model = model_diabetes4, data = diabetes_USA_imputed, k = 5)

print(cv1)
print(cv2)
print(cv3)
print(cv4)
print(cv5)

### ELPD #########################################################################################

# Compute LOO-CV for each model
loo_cv1 <- loo(model_diabetes)
loo_cv2 <- loo(model_diabetes_pairwise, k_threshold = 0.7)
loo_cv3 <- loo(model_diabetes2)
loo_cv4 <- loo(model_diabetes3)
loo_cv5 <- loo(model_diabetes4)

# Extract ELPD estimates for each model
elpd_values <- c(
  loo_cv1$estimates[1, "Estimate"], 
  loo_cv2$estimates[1, "Estimate"], 
  loo_cv3$estimates[1, "Estimate"], 
  loo_cv4$estimates[1, "Estimate"],
  loo_cv5$estimates[1, "Estimate"]
)

# Print ELPD values for the models
print(elpd_values)

# Compare models based on ELPD
loo_comparison <- loo_compare(loo_cv1, loo_cv2, loo_cv3, loo_cv4, loo_cv5)
print(loo_comparison)
################### Hypothesis Testing ######################################################
## Ho: The probability of diabetes in women who have been/are pregnant is the same or lower than in women who have never been pregnant.
## Ha: The probability of diabetes is higher in pregnant women.
# categorical variable (1-pregnant, 0-not pregnant)
diabetes_USA_imputed$pregnant_binary <- ifelse(diabetes_USA_imputed$pregnant > 0, 1, 0)
model_diabetes_binary <- stan_glm(diabetes ~ pregnant_binary + glucose + pressure + triceps + insulin + mass + pedigree + age,
                                  data = diabetes_USA_imputed,
                                  family = binomial(link = "logit"),
                                  prior_intercept = normal(-0.619, 1),  # Intercept corresponding to 34.9%
                                  prior = normal(c(0.385, rep(0, 7)), c(1, rep(1, 7)), autoscale = FALSE),  # Specific prior for pregnant_binary
                                  chains = 4, iter = 10000, seed = 84735)

# Display model results
summary(model_diabetes_binary)
# Extract posterior samples for the 'pregnant' coefficient
posterior_samples <- as.data.frame(as.matrix(model_diabetes_binary))

# Display statistics for the 'pregnant' coefficient
summary(posterior_samples$pregnant_binary)

# Calculate the posterior distribution density for 'pregnant_binary'
density_pregnant <- density(posterior_samples$pregnant_binary)
# Create a data frame for the posterior density
density_df <- data.frame(
  x = density_pregnant$x,
  y = density_pregnant$y
)
# Testing Ha (posterior): Probability that βpregnant > 0
posterior_Ha <- mean(posterior_samples$pregnant_binary > 0)
posterior_Ha # Probability that βpregnant > 0

# Testing Ho (posterior): Probability that βpregnant <= 0 
posterior_H0 <- 1 - posterior_Ha
posterior_H0

# Posterior odds
posterior_odds <- posterior_Ha / posterior_H0
cat("Posterior odds:", posterior_odds, "\n") # The alternative hypothesis (Ha) is more likely than the null hypothesis (Ho) based on the posterior samples.

# Prior
prior_pregnant_binary <- rnorm(10000, mean = 0.385, sd = 1)

# Assume prior odds
prior_Ha <- mean(prior_pregnant_binary > 0)
prior_Ha

prior_H0 <- 1 - prior_Ha
prior_H0


# Calculate prior odds
prior_odds <- prior_Ha / prior_H0
cat("Prior odds:", prior_odds, "\n")

# Bayes factor 
bayes_factor <- posterior_odds / prior_odds
cat("Bayes factor:", bayes_factor, "\n") 
################################
## Ho: The probability of diabetes does not depend on genetic predisposition.
## Ha: Higher genetic predisposition increases the probability of diabetes.
# Extract posterior samples
model_diabetes<- stan_glm(diabetes ~ glucose + pressure + triceps + insulin + mass + pedigree + age,
                          data = diabetes_USA_imputed,
                          family = binomial(link = "logit"),
                          prior_intercept = normal(-0.619, 1),  # Intercept corresponding to 34.9%
                          prior = normal(c(0, 0, 0, 0, 0, 0.3, 0), c(1, 1, 1, 1, 1, 2, 1), autoscale = FALSE),  # Prior for pedigree (genetic predisposition)
                          chains = 4, iter = 10000, seed = 84735)

posterior_samples2 <- as.data.frame(as.matrix(model_diabetes))

# Probability that higher genetic predisposition increases the probability of diabetes (Ha)
posterior_Ha2 <- mean(posterior_samples2$pedigree > 0)
posterior_Ha2

# Probability that genetic predisposition has no effect or reduces the probability of diabetes (H0)
posterior_H02 <- 1 - posterior_Ha2
posterior_H02

# Posterior odds
posterior_odds2 <- posterior_Ha2 / posterior_H02
cat("Posterior odds:", posterior_odds2, "\n")

# Prior
prior_pedigree <- rnorm(10000, mean = 0.3, sd = 2)

# Assume prior odds
prior_Ha2 <- mean(prior_pedigree > 0)
prior_Ha2

prior_H02 <- 1 - prior_Ha2
prior_H02


# Calculate prior odds
prior_odds2 <- prior_Ha2 / prior_H02
cat("Prior odds:", prior_odds2, "\n")

# Bayes factor 
bayes_factor2 <- posterior_odds2 / prior_odds2
cat("Bayes factor:", bayes_factor2, "\n") 

#################_______    NAIVE BAYES    __________########################
# conditional correlation matrices (2 for raw data)
library(dplyr)
library(corrplot)

# Ensure only numeric columns are used for the correlation matrix
cor_matrix_pos <- cor(subset(diabetes_USA_imputed, diabetes == 1) %>% select(where(is.numeric)))
cor_matrix2_pos <- cor(subset(diabetes_USA, diabetes == 1) %>% select(where(is.numeric)), use = "complete.obs")
cor_matrix_neg <- cor(subset(diabetes_USA_imputed, diabetes == 0) %>% select(where(is.numeric)))
cor_matrix2_neg <- cor(subset(diabetes_USA, diabetes == 0) %>% select(where(is.numeric)), use = "complete.obs")

# Graphical visualization of correlation matrices
corrplot(cor_matrix_pos, method = "circle", type = "upper", 
         tl.cex = 1, # Text label size
         tl.col = "black", # Text label color
         diag = FALSE) # Hide the diagonal
corrplot(cor_matrix2_pos, method = "circle", type = "upper", 
         tl.cex = 1, # Text label size
         tl.col = "black", # Text label color
         diag = FALSE) # Hide the diagonal
corrplot(cor_matrix_neg, method = "circle", type = "upper", 
         tl.cex = 1, # Text label size
         tl.col = "black", # Text label color
         diag = FALSE) # Hide the diagonal
corrplot(cor_matrix2_neg, method = "circle", type = "upper", 
         tl.cex = 1, # Text label size
         tl.col = "black", # Text label color
         diag = FALSE) # Hide the diagonal

### highest correlation in the data between 
# pregnant and age 
# triceps and mass 
# glucose and insulin 

diabetes_USA$diabetes_old <- ifelse(diabetes_USA$diabetes == 1, "pos", "neg")
diabetes_USA_imputed$diabetes_old <- ifelse(diabetes_USA_imputed$diabetes == 1,"pos","neg")

### graphical representation of predictors
#   graph for each predictor (i.e., target_var sequentially set to values: 
#               pregnant, glucose, pressure, triceps, insulin, mass, pedigree, age)
#     with an estimate of the Normal distribution using the sample mean and sd 
target_var <- sym("age")
pom <- diabetes_USA_imputed %>% 
  group_by(diabetes) %>% 
  summarize(mean = mean(!!target_var), 
            sd = sd(!!target_var))
ggplot(diabetes_USA_imputed, aes(x = !!target_var, fill = diabetes_old, colour = diabetes_old)) + 
  geom_density(alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = pom$mean[1], sd = pom$sd[1]), aes(color = "neg")) +
  stat_function(fun = dnorm, args = list(mean = pom$mean[2], sd = pom$sd[2]), aes(color = "pos"))

### graphical representation of predictors - original data
pom2 <- diabetes_USA %>% 
  group_by(diabetes) %>% 
  summarize(mean = mean(!!target_var, na.rm =TRUE), 
            sd = sd(!!target_var, na.rm=TRUE))
ggplot(diabetes_USA, aes(x = !!target_var, fill = diabetes_old, colour = diabetes_old)) + 
  geom_density(alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = pom2$mean[1], sd = pom2$sd[1]), 
                aes(color = "neg")) +
  stat_function(fun = dnorm, args = list(mean = pom2$mean[2], sd = pom2$sd[2]),
                aes(color = "pos"))
# The distributions of predictors Glucose, Pressure, Triceps, and Mass are close to Normal distribution
#   while Pregnant, Insulin, Pedigree, and Age are skewed, so their distribution is different from normal

####____ Model all ______ ####
naive_model_all <- naiveBayes(diabetes ~ pregnant + glucose + pressure + triceps +
                                insulin + mass + pedigree + age 
                              , data = diabetes_USA_imputed)
# confusion matrix
diabetes_USA_imputed <- diabetes_USA_imputed %>% 
  mutate(model_all = predict(naive_model_all, newdata = .))
diabetes_USA_imputed %>% 
  tabyl(diabetes, model_all) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
# 10-fold cross validation
set.seed(12345)
cv_model_all <- naive_classification_summary_cv(
  model = naive_model_all, data = diabetes_USA_imputed, y = "diabetes", k = 10)
cv_model_all$cv # avg across folds

# Model all - without NA data imputation
naive_model_all2 <- naiveBayes(diabetes ~ pregnant + glucose + pressure + triceps +
                                 insulin + mass + pedigree + age 
                               , data = diabetes_USA)
# confusion matrix
diabetes_USA <- diabetes_USA %>% 
  mutate(model_all2 = predict(naive_model_all2, newdata = .))
diabetes_USA %>% 
  tabyl(diabetes, model_all2) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
# 10-fold cross validation
set.seed(12345)
cv_model_all <- naive_classification_summary_cv(
  model = naive_model_all2, data = diabetes_USA, y = "diabetes", k = 10)
cv_model_all$cv # avg across folds  
### The model without data imputation is slightly more accurate

####_____ Model with only glucose and mass ______ ####
# we see that these two predictors have the highest correlation with the classified variable Diabetes
# 2 at the end of the variable name indicates the use of raw data
naive_model_gm <- naiveBayes(diabetes ~ glucose + mass 
                             , data = diabetes_USA_imputed)
naive_model_gm2 <- naiveBayes(diabetes ~ glucose + mass 
                              , data = diabetes_USA)
# confusion matrix
diabetes_USA_imputed <- diabetes_USA_imputed %>% 
  mutate(model_gm = predict(naive_model_gm, newdata = .))
diabetes_USA <- diabetes_USA %>% 
  mutate(model_gm2 = predict(naive_model_gm2, newdata = .))
diabetes_USA_imputed %>% 
  tabyl(diabetes, model_gm) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
diabetes_USA %>% 
  tabyl(diabetes, model_gm2) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# 10-fold cross validation
set.seed(12345)
cv_model_gm <- naive_classification_summary_cv(
  model = naive_model_gm, data = diabetes_USA_imputed, y = "diabetes", k = 10)
cv_model_gm$cv # avg across folds
cv_model_gm <- naive_classification_summary_cv(
  model = naive_model_gm2, data = diabetes_USA, y = "diabetes", k = 10)
cv_model_gm$cv # avg across folds

####_____ Model with only glucose, mass, pregnant, age ______ ####
# highest correlation with Diabetes, but the 2 added variables violate the normality assumption
naive_model_gmpa <- naiveBayes(diabetes ~ glucose + mass + pregnant + age
                               , data = diabetes_USA_imputed)
naive_model_gmpa2 <- naiveBayes(diabetes ~ glucose + mass + pregnant + age
                                , data = diabetes_USA)
# confusion matrix
diabetes_USA_imputed <- diabetes_USA_imputed %>% 
  mutate(model_gmpa = predict(naive_model_gmpa, newdata = .))
diabetes_USA <- diabetes_USA %>% 
  mutate(model_gmpa2 = predict(naive_model_gmpa2, newdata = .))
diabetes_USA_imputed %>% 
  tabyl(diabetes, model_gmpa) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
diabetes_USA %>% 
  tabyl(diabetes, model_gmpa2) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# 10-fold cross validation
set.seed(12345)
cv_model_gmpa <- naive_classification_summary_cv(
  model = naive_model_gmpa, data = diabetes_USA_imputed, y = "diabetes", k = 10)
cv_model_gmpa$cv # avg across folds
cv_model_gmpa <- naive_classification_summary_cv(
  model = naive_model_gm2, data = diabetes_USA, y = "diabetes", k = 10)
cv_model_gmpa$cv # avg across folds