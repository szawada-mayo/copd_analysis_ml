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

split <- createDataPartition(data_matched$adh_yes, p = 0.7, list = FALSE)
train <- data_matched[split, ]
test  <- data_matched[-split, ]

tree_model_with <- rpart(adh_yes ~ sd_waso  + age2_a + sex_numeric_a + mmrc_a + charlson_a + fev1_updated_a,
             data = data_matched,
             method = "class",
             control = rpart.control(cp = 0.01, xval = 10))  # xval = 10 for 10-fold CV

printcp(tree_model_with)
plotcp(tree_model_with)
best_cp <- tree_model_with$cptable[which.min(tree_model_with$cptable[, "xerror"]), "CP"]
pruned_fit <- prune(tree_model_with, cp = best_cp)

rpart.plot(pruned_fit,
           type = 3,
           extra = 104,
           fallen.leaves = TRUE,
           tweak = 1.2,
           main = "Classification Tree for Diabetes")

# Predict probabilities and classes
pred_probs <- predict(pruned_fit, test, type = "prob")[, 2]
pred_class <- predict(pruned_fit, test, type = "class")
actual_class <- test$adh_yes_factor  # <- use your real column name
pred_class_char <- ifelse(pred_class == "1", "Yes", "No")
pred_class <- factor(pred_class_char, levels = levels(actual_class))
library(caret)
conf <- confusionMatrix(pred_class, actual_class)
print(conf)
accuracy <- conf$overall["Accuracy"]
roc_obj <- roc(test$adh_yes, pred_probs)
auc_val <- auc(roc_obj)
cat("Accuracy:", round(accuracy, 3), "\n")
cat("AUC:", round(auc_val, 3), "\n")

# Number of terminal nodes
num_leaves <- sum(pruned_fit$frame$var == "<leaf>")
cat("Number of terminal nodes:", num_leaves, "\n")
# Tree depth
max_depth <- max(pruned_fit$frame$depth)
cat("Max depth:", max_depth, "\n")
# Variable importance
importance <- pruned_fit$variable.importance
print(importance)





library(rpart)
tree_model_with <- rpart(adh_yes ~ sd_waso + age2_a + sex_numeric_a + mmrc_a + charlson_a + fev1_updated_a, data = data_matched, method = "class")
summary(tree_model_with)
#install.packages("rpart.plot")  # Only once
library(rpart.plot)
rpart.plot(tree_model_with, type = 2, extra = 104, fallen.leaves = TRUE,
           cex = 0.7,             # Adjust **text size**
           faclen = 0,            # Show full variable names
           tweak = 2.0,           # Scale node text (alternative to cex)
           box.palette = "Blues") # Add color palette

tree_model <- rpart(adh_yes ~  bmi_a + age2_a + sex_numeric_a + mmrc_a + charlson_a + fev1_updated_a, data = data_matched, method = "class")
summary(tree_model)
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE,
           cex = 0.7,             # Adjust **text size**
           faclen = 0,            # Show full variable names
           tweak = 2.0,           # Scale node text (alternative to cex)
           box.palette = "Blues") # Add color palette




pred_with_sd <- predict(tree_model_with, newdata = data_matched, type = "prob")[, 2]
actual <- data_matched$adh_yes  # Ensure this is 0/1, not a factor
roc_with_sd <- roc(actual, pred_with_sd)
auc_with_sd <- auc(roc_with_sd)
print(auc_with_sd)

pred_without_sd <- predict(tree_model, newdata = data_matched, type = "prob")[, 2]
roc_without_sd <- roc(actual, pred_without_sd)
auc_without_sd <- auc(roc_without_sd)
print(auc_without_sd)

###
tree_model_with <- rpart(adh_yes ~ bmi_level + sd_time + age2_a + sex_numeric_a + mmrc_a + charlson_a + fev1_updated_a, data = data_matched, method = "class")
summary(tree_model_with)
tree_model <- rpart(adh_yes ~  bmi_level + age2_a + sex_numeric_a + mmrc_a + charlson_a + fev1_updated_a, data = data_matched, method = "class")
summary(tree_model)

pred_with_sd <- predict(tree_model_with, newdata = data_matched, type = "prob")[, 2]
actual <- data_matched$adh_yes  # Ensure this is 0/1, not a factor
roc_with_sd <- roc(actual, pred_with_sd)
auc_with_sd <- auc(roc_with_sd)
print(auc_with_sd)

pred_without_sd <- predict(tree_model, newdata = data_matched, type = "prob")[, 2]
roc_without_sd <- roc(actual, pred_without_sd)
auc_without_sd <- auc(roc_without_sd)
print(auc_without_sd)



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
  adh_yes_factor ~ sd_waso + bmi_factor+  age2_a + sex_numeric_a + mmrc_a  + fev1_updated_a +charlson_a ,
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
