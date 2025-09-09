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
data <- read.csv("df_foradherence.csv")
data_adh <- read.csv("9.4.25_adherence_metrics.csv")

names(data_adh)[names(data_adh) == "ID"] <- "Subject.Name_a"
data_matched <- inner_join(data_adh, data, by = "Subject.Name_a")

data_matched$weeks_adherent <- as.numeric(as.character(data_matched$weeks_adherent))
data_matched$adh_yes <- ifelse(data_matched$weeks_adherent == 12, 1, 0)

data_matched$bmi_level <- factor(data_matched$bmi_level,
                                       levels = c(1, 2, 3),
                                       labels = c("healthy", "overweight", "obese"))


#########
data_matched$adh_yes <- as.numeric(as.character(data_matched$adh_yes))
data_matched$adh_yes_factor <- factor(ifelse(data_matched$adh_yes == 1, "Yes", "No"), levels = c("No", "Yes"))
data_matched$bmi_factor<- factor(data_matched$bmi_level, 
                                 levels = c("healthy", "overweight", "obese"), 
                                 labels = c("healthy", "overweight", "obese"))

# Convert variables if needed
data_ml <- data_matched %>%
  mutate(
    sex_numeric_a = factor(sex_numeric_a),
  )


##############
train_control <- trainControl(
  method = "cv",     # Cross-validation
  number = 10,       # 10 folds
  classProbs = TRUE, # Needed for ROC/AUC
  summaryFunction = twoClassSummary  # For binary classification metrics
)

tree_model_without<- train(
  adh_yes_factor ~ age2_a  + sex_numeric_a + mmrc_a + charlson_a + fev1_updated_a,
  data = data_matched,
  method = "rpart",       # decision tree method
  trControl = train_control,
  metric = "ROC"          # optimize for AUC
)

print(tree_model_without)
plot(tree_model_without)   # Visualize tuning parameter (cp) performance

rpart.plot(tree_model_without$finalModel, extra = 106)
var_imp <- varImp(tree_model_without)
print(var_imp)
plot(var_imp)

print(tree_model_without$results)     # All tuning results including ROC scores
print(tree_model_without$bestTune)    # Best tuning parameter (e.g., cp)
print(max(tree_model_without$results$ROC))


###### Max AUC across tuning
train_control <- trainControl(
  method = "cv",     # Cross-validation
  number = 10,       # 10 folds
  classProbs = TRUE, # Needed for ROC/AUC
  summaryFunction = twoClassSummary  # For binary classification metrics
)

tree_model_with <- train(
  adh_yes_factor ~ sd_waso  +age2_a + sex_numeric_a + mmrc_a + charlson_a + fev1_updated_a,
  data = data_matched,
  method = "rpart",       # decision tree method
  trControl = train_control,
  metric = "ROC"          # optimize for AUC
)

print(tree_model_with)
plot(tree_model_with)   # Visualize tuning parameter (cp) performance

rpart.plot(tree_model_with$finalModel, extra = 106)
var_imp <- varImp(tree_model_with)
print(var_imp)
plot(var_imp)

print(tree_model_with$results)     # All tuning results including ROC scores
print(tree_model_with$bestTune)    # Best tuning parameter (e.g., cp)
print(max(tree_model_with$results$ROC))


######
###### Max AUC across tuning
train_control <- trainControl(
  method = "cv",     # Cross-validation
  number = 10,       # 10 folds
  classProbs = TRUE, # Needed for ROC/AUC
  summaryFunction = twoClassSummary  # For binary classification metrics
)

tree_model_withtime <- train(
  adh_yes_factor ~ sd_time  + age2_a + sex_numeric_a + mmrc_a + charlson_a + fev1_updated_a,
  data = data_matched,
  method = "rpart",       # decision tree method
  trControl = train_control,
  metric = "ROC"          # optimize for AUC
)

print(tree_model_withtime)
plot(tree_model_withtime)   # Visualize tuning parameter (cp) performance

rpart.plot(tree_model_withtime$finalModel, extra = 106)
var_imp <- varImp(tree_model_withtime)
print(var_imp)
plot(var_imp)

print(tree_model_withtime$results)     # All tuning results including ROC scores
print(tree_model_withtime$bestTune)    # Best tuning parameter (e.g., cp)
print(max(tree_model_withtime$results$ROC))

##### 
###### Max AUC across tuning
train_control <- trainControl(
  method = "cv",     # Cross-validation
  number = 10,       # 10 folds
  classProbs = TRUE, # Needed for ROC/AUC
  summaryFunction = twoClassSummary  # For binary classification metrics
)

tree_model_withtime <- train(
  adh_yes_factor ~ sd_time + sd_waso + bmi_a + age2_a + sex_numeric_a + mmrc_a + charlson_a + fev1_updated_a,
  data = data_matched,
  method = "rpart",       # decision tree method
  trControl = train_control,
  metric = "ROC"          # optimize for AUC
)

print(tree_model_withtime)
plot(tree_model_withtime)   # Visualize tuning parameter (cp) performance

rpart.plot(tree_model_withtime$finalModel, extra = 106)
var_imp <- varImp(tree_model_withtime)
print(var_imp)
plot(var_imp)

print(tree_model_withtime$results)     # All tuning results including ROC scores
print(tree_model_withtime$bestTune)    # Best tuning parameter (e.g., cp)
print(max(tree_model_withtime$results$ROC))













write.csv(data_ml, "forbaseline.csv", row.names = FALSE)


##############

cor(data_matched$sd_waso, data_matched$weeks_adherent, use = "complete.obs")
cor.test(data_matched$sd_waso, data_matched$weeks_adherent)
cor(data_matched$sd_time, data_matched$weeks_adherent, use = "complete.obs")
cor.test(data_matched$sd_time, data_matched$weeks_adherent)
cor(data_matched$mean_waso, data_matched$weeks_adherent, use = "complete.obs")
cor.test(data_matched$mean_waso, data_matched$weeks_adherent)
cor(data_matched$mean_time, data_matched$weeks_adherent, use = "complete.obs")
cor.test(data_matched$mean_time, data_matched$weeks_adherent)
cor(data_matched$bmi_a, data_matched$weeks_adherent, use = "complete.obs")
cor.test(data_matched$bmi_a, data_matched$weeks_adherent)
cor(data_matched$fev1_updated_a, data_matched$weeks_adherent, use = "complete.obs")
cor.test(data_matched$fev1_updated_a, data_matched$weeks_adherent)

cor.test(as.numeric(data_matched$sleep_waso_level), data_matched$weeks_adherent, method = "spearman")
cor.test(as.numeric(data_matched$sleep_dur_level), data_matched$weeks_adherent, method = "spearman")
cor.test(as.numeric(data_matched$bmi_level), data_matched$weeks_adherent, method = "spearman")
cor.test(as.numeric(data_matched$mmrc_a), data_matched$weeks_adherent, method = "spearman")
cor.test(as.numeric(data_matched$charlson_a), data_matched$weeks_adherent, method = "spearman")

###this is key
t.test(sd_time ~ adh_yes, data = data_matched)
t.test(sd_waso ~ adh_yes, data = data_matched)
t.test(bmi_a ~ adh_yes, data = data_matched)
t.test(mmrc_a ~ adh_yes, data = data_matched)
t.test(charlson_a ~ adh_yes, data = data_matched)

#don't use - normal LR
model <- glm(adh_yes ~ sd_waso *bmi_level + age2_a + sex_numeric_a + mmrc_a + charlson_a + fev1_updated_a, data = data_matched, family = binomial)
summary(model)
exp(coef(model))
exp(confint(model))  # Uses profile likelihood by default

# Run logistic regression
model <- glm(adh_yes ~ sd_waso * bmi_level +age2_a + sex_numeric_a + mmrc_a + charlson_a + fev1_updated_a,
             data = data_matched,
             family = binomial)

summary(model)
exp(coef(model))
exp(confint(model))  # Uses profile likelihood by default

####################
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
  adh_yes_factor ~ sd_waso +  age2_a + sex_numeric_a + mmrc_a  + fev1_updated_a +charlson_a ,
  data = data_ml,
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
  adh_yes_factor ~ age2_a  + sex_numeric_a + mmrc_a  + fev1_updated_a +charlson_a ,
  data = data_ml,
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

######sd_time
# -------------------------
# Model 1: with all variables
# -------------------------
model_all <- train(
  adh_yes_factor ~ sd_time + bmi_factor+  age2_a + sex_numeric_a + mmrc_a  + fev1_updated_a +charlson_a ,
  data = data_ml,
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
  adh_yes_factor ~ bmi_factor + age2_a  + sex_numeric_a + mmrc_a  + fev1_updated_a +charlson_a ,
  data = data_ml,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

cat("AUC with sd_exercise_time:", model_all$results$ROC, "\n")
cat("AUC without sd_exercise_time:", model_reduced$results$ROC, "\n")
resamples_all <- model_all$resample$ROC
resamples_reduced <- model_reduced$resample$ROC
t.test(resamples_all, resamples_reduced, paired = TRUE)

######sd_time
# -------------------------
# Model 1: with all variables
# -------------------------
model_all <- train(
  adh_yes_factor ~ sd_waso + sd_time + bmi_factor+  age2_a + sex_numeric_a + mmrc_a  + fev1_updated_a +charlson_a ,
  data = data_ml,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

# -------------------------
# Model 2: without sd_exercise_time
# -------------------------
model_reduced <- train(
  adh_yes_factor ~ age2_a  + sex_numeric_a + mmrc_a  + fev1_updated_a +charlson_a ,
  data = data_ml,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

cat("AUC with sd_exercise_time:", model_all$results$ROC, "\n")
cat("AUC without sd_exercise_time:", model_reduced$results$ROC, "\n")
resamples_all <- model_all$resample$ROC
resamples_reduced <- model_reduced$resample$ROC
t.test(resamples_all, resamples_reduced, paired = TRUE)
