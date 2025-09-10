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
data <- read.csv("df_foradherence.csv")
#data_adh <- read.csv("adherence_metrics1.csv")

#names(data_adh)[names(data_adh) == "ID"] <- "Subject.Name"
#data_matched <- inner_join(data_adh, data, by = "Subject.Name")


data$adherence_a[is.na(data$adherence_a)] <- 0
data$adherent_weeks_clean_a[is.na(data$adherent_weeks_clean_a)] <- 0
#data$adherent_weeks_clean <- ifelse(data$adherence_a > 11, 1, 0)
#data$adherent_weeks_clean_a <- ifelse(data_ml$adherence_a > 11, 1, 0)
#data$adherent_weeks_clean <- as.numeric(as.character(data$adherent_weeks_clean))
#data_matched$adh_yes <- ifelse(data_matched$weeks_adherent == 12, 1, 0)

#data_matched$bmi_level <- factor(data_matched$bmi_level,
#                                       levels = c(1, 2, 3),
#                                       labels = c("healthy", "overweight", "obese"))


#########
#data_matched$adh_yes <- as.numeric(as.character(data_matched$adh_yes))
data$adh_yes <- data$adherence_a
data$adh_yes_factor <- factor(ifelse(data$adh_yes == 1, "Yes", "No"), levels = c("No", "Yes"))
#data_matched$bmi_factor<- factor(data_matched$bmi_level, 
#                                 levels = c("healthy", "overweight", "obese"), 
#                                 labels = c("healthy", "overweight", "obese"))

# Convert variables if needed
data_ml <- data %>%
  mutate(
    sex_numeric_a = factor(sex_numeric_a),
  )

data_ml <- data_ml %>%
  mutate(
    current_smoker_a = factor(current_smoker_a),
  )


data_ml$education_num <- as.numeric(factor(data_ml$education_a))
data_ml <- data_ml %>%
  mutate(edu_level = if_else(education_num %in% c(2, 4), 1, 0))


#data_ml <- data_ml[data_ml$age2_a >= 65, ]

##############
#data_ml$charlson_a_level <- ifelse(data_ml$charlson_a < 3, 1,
#                                     ifelse(data_ml$charlson_a <5, 2, 3))

train_control <- trainControl(
  method = "cv",     # Cross-validation
  number = 10,       # 10 folds
  classProbs = TRUE, # Needed for ROC/AUC
  summaryFunction = twoClassSummary  # For binary classification metrics
)

###check for missing values in columns
sum(is.na(data_ml$age2_a))
sum(is.na(data_ml$sex_numeric_a))
sum(is.na(data_ml$mmrc_a))
sum(is.na(data_ml$charlson_a))
sum(is.na(data_ml$fev1_updated))
sum(is.na(data_ml$edu_level))
sum(is.na(data_ml$current_smoker_a))

data_ml <- data_ml[!is.na(data_ml$sd_waso), ]
#3 dropped from no waso
data_ml <- data_ml[, !(colnames(data_ml) %in% "weight_a")]
data_ml <- data_ml[, !(colnames(data_ml) %in% "sleep_pheno_3")]
data_ml <- data_ml[, !(colnames(data_ml) %in% "sleep_pheno_2")]
data_ml <- data_ml[, !(colnames(data_ml) %in% "sleep_pheno_1")]
data_ml <- data_ml[, !(colnames(data_ml) %in% "sleep_pheno_0")]
data_ml <- data_ml[, !(colnames(data_ml) %in% "sleep_waso_level_1")]
data_ml <- data_ml[, !(colnames(data_ml) %in% "sleep_waso_level_2")]
data_ml <- data_ml[, !(colnames(data_ml) %in% "sleep_waso_level_3")]
data_ml <- data_ml[, !(colnames(data_ml) %in% "charlson_a_level")]
data_ml <- data_ml[, !(colnames(data_ml) %in% "mcn_a")]
data_ml$adh_yes <- data_ml$adherence_a
data_ml$adh_yes_factor <- factor(ifelse(data_ml$adh_yes == 1, "Yes", "No"), levels = c("No", "Yes"))
data_ml$sleep_waso_level <- ifelse(data_ml$sd_waso > 25, 1,0)

data_ml <- data_ml %>%
  mutate(summary_c = case_when(
    sleep_waso_level == 0 & sd_level == 0 ~ 0,
    sleep_waso_level == 1 & sd_level == 0 ~ 1,
    sleep_waso_level == 0 & sd_level == 1 ~ 2,
    sleep_waso_level == 1 & sd_level == 1 ~ 3
  ),
  summary_c = as.factor(summary_c))


cols_with_na <- colnames(data_ml)[colSums(is.na(data_ml)) > 0]
print(cols_with_na)
df_clean <- data_ml

init <- mice(df_clean, maxit = 0)
cols_with_na <- colnames(df_clean)[colSums(is.na(df_clean)) > 0]
print(cols_with_na)
# Modify methods for specific columns
init$method["charlson_a"] <- "pmm"
init$method["mmrc_a"] <- "pmm"
init$method["age2_a"] <- "pmm"
#init$method["weight_a"] <- "pmm"
init$method["fev1_updated_a"] <- "pmm"
init$method["sex_numeric_a"] <- "logreg"
init$method["current_smoker_a"] <- "logreg"
print(init$method)

length(init$method)
ncol(df_clean)
#init <- init[1:22]

length(init)
#imp <- mice(df_clean, method = init$method, m = 5)

init$method

imp <- mice(df_clean,
            method = init$method,
            predictorMatrix = init$predictorMatrix,
            post = init$post,
            blocks = init$blocks,
            m = 5)

df_imputed <- complete(imp, action = 1)

df_imputed <- df_imputed %>% select(-education_a)
cols_with_na <- colnames(df_imputed)[colSums(is.na(df_imputed)) > 0]
print(cols_with_na)

write.csv(df_imputed, "imputeddf.csv", row.names = FALSE)
