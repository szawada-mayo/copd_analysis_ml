#install.packages("rpart.plot")
library(rpart.plot)
#remove.packages("mice")
#install.packages("mice", dependencies = TRUE)
library(mice)
# Install if not already installed
#install.packages("caret")
#install.packages("pROC")
#install.packages("tidyverse")  # Optional, for dplyr/mutate
library(sandwich)
library(dplyr)
library(lubridate)
library(tidyverse)
library(caret)
library(pROC)
df_imputed <- read.csv("imputeddf.csv")
#data_adh <- read.csv("adherence_metrics1.csv")

#pca_data <- df_imputed[, c("mean_waso", "sd_waso", "mean_time", "sd_time", "mean_eff", "mean_noa", "mean_loaim", "mean_ac", "mean_mi", "mean_fi", "mean_sfi")]
#pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)
#summary(pca_result)
#pca_result$rotation
#head(pca_result$x)
#pc1_scores <- pca_result$x[, 1]
#df_imputed$PC1 <- pc1_scores

library(caret)
ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

pls_model <- train(
  adh_yes_factor ~ mean_waso + sd_waso + mean_time + sd_time +mean_eff +mean_noa +mean_loaim+ mean_ac +mean_mi + mean_fi+ mean_sfi,
  data = df_imputed,
  method = "pls",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary),
  tuneLength = 5,
  metric = "ROC"
)

# Check AUC
print(pls_model)
pls_model$bestTune
pls_scores <- pls_model$finalModel$scores
PLS1 <- pls_scores[, 1]
df_imputed$PLS1 <- PLS1

######pca done
train_control <- trainControl(
  method = "cv",     # Cross-validation
  number = 10,       # 10 folds
  classProbs = TRUE, # Needed for ROC/AUC
  summaryFunction = twoClassSummary  # For binary classification metrics
)

tree_model_without<- train(
  adh_yes_factor ~age2_a + sex_numeric_a  + charlson_a + fev1_updated_a + edu_level + current_smoker_a,
  data = df_imputed,
  method = "rpart",       # decision tree method
  trControl = train_control,
  metric = "ROC"          # optimize for AUC
)

print(tree_model_without)
plot(tree_model_without)   # Visualize tuning parameter (cp) performance
rpart.plot(tree_model_without$finalModel, 
           type = 2,
           extra = 106,
           box.palette = "GnBu",
           shadow.col = "gray",
           nn = TRUE)

var_imp <- varImp(tree_model_without)
print(var_imp)
plot(var_imp)

print(tree_model_without$results)     # All tuning results including ROC scores
print(tree_model_without$bestTune)    # Best tuning parameter (e.g., cp)
print(max(tree_model_without$results$ROC))

###### Max AUC across tuning

tree_model_with <- train(
  adh_yes_factor ~ PLS1  + age2_a + sex_numeric_a  + charlson_a + fev1_updated_a + edu_level + current_smoker_a,
  data = df_imputed,
  method = "rpart",       # decision tree method
  trControl = train_control,
  metric = "ROC"          # optimize for AUC
)

print(tree_model_with)
plot(tree_model_with)   # Visualize tuning parameter (cp) performance

rpart.plot(tree_model_with$finalModel, 
           type = 2,
           extra = 106,
           box.palette = "GnBu",
           shadow.col = "gray",
           nn = TRUE)
var_imp <- varImp(tree_model_with)
print(var_imp)
plot(var_imp)

print(tree_model_with$results)     # All tuning results including ROC scores
print(tree_model_with$bestTune)    # Best tuning parameter (e.g., cp)
print(max(tree_model_with$results$ROC))


# Compare performance using caret
results <- resamples(list(Tree1 = tree_model_with, Tree2 = tree_model_without))
summary(results)
bwplot(results)
diffs <- diff(results)
str(results$values)
head(results$values)
t_test_res <- t.test(
  results$values$`Tree1~ROC`,  # Note backticks because of special chars
  results$values$`Tree2~ROC`,
  paired = TRUE
)
print(t_test_res)

probs1 <- predict(tree_model_with, newdata = df_imputed, type = "prob")[, "Yes"]
probs2 <- predict(tree_model_without, newdata = df_imputed, type = "prob")[, "Yes"]
roc1 <- roc(df_imputed$adh_yes_factor, probs1)
roc2 <- roc(df_imputed$adh_yes_factor, probs2)
roc.test(roc1, roc2, method = "delong")

#########
# Set up cross-validation
ctrl <- trainControl(
  method = "cv",         # 10-fold cross-validation
  number = 10,
  classProbs = TRUE,     # Needed for AUC
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# -------------------------
# Model 1: with all variables
# -------------------------
model_all <- train(
  adh_yes_factor ~ PLS1  + age2_a + sex_numeric_a  + charlson_a + fev1_updated_a + edu_level + current_smoker_a,
  data = df_imputed,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

final_model_all <- model_all$finalModel
tidy_model_all <- broom::tidy(final_model_all, exponentiate = TRUE, conf.int = TRUE)
print(tidy_model_all)
# -------------------------
# Model 2: without sd_exercise_time
# -------------------------
model_reduced <- train(
  adh_yes_factor ~ age2_a +current_smoker_a  +edu_level+sex_numeric_a   + fev1_updated_a +charlson_a ,
  data = df_imputed,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)
final_model <- model_reduced$finalModel
tidy_model <- broom::tidy(final_model, exponentiate = TRUE, conf.int = TRUE)
print(tidy_model)
# Compare AUCs
cat("AUC with sd_exercise_time:", model_all$results$ROC, "\n")
cat("AUC without sd_exercise_time:", model_reduced$results$ROC, "\n")

# Access resampled AUCs
resamples_all <- model_all$resample$ROC
resamples_reduced <- model_reduced$resample$ROC
t.test(resamples_all, resamples_reduced, paired = TRUE)

probs_model1 <- predict(model_all, newdata = df_imputed, type = "prob")[, "Yes"]
probs_model2 <- predict(model_reduced, newdata = df_imputed, type = "prob")[, "Yes"]

roc1 <- roc(df_imputed$adh_yes_factor, probs_model1)
roc2 <- roc(df_imputed$adh_yes_factor, probs_model2)

roc.test(roc1, roc2, method = "delong")

model_all$results$Accuracy
model_reduced$results$Accuracy

