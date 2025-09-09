#install.packages("sandwich")
library(sandwich)
library(dplyr)
library(lubridate)
#data <- read.csv("df_baselinecharacteristics.csv")
data <- read.csv("forbaseline.csv")
data_dur <- read.csv("df_baselinecharacteristics.csv")

library(dplyr)

df_merged <- data %>%
  left_join(data_dur %>% select(Subject.Name_a, race_a), by = "Subject.Name_a")

df_merged <- df_merged %>%
  left_join(data_dur %>% select(Subject.Name_a, ethnicity_a), by = "Subject.Name_a")

df_merged <- df_merged %>%
  left_join(data_dur %>% select(Subject.Name_a, education_a), by = "Subject.Name_a")

df_merged <- df_merged %>%
  left_join(data_dur %>% select(Subject.Name_a, marital_status_a), by = "Subject.Name_a")

df_merged <- df_merged %>%
  left_join(data_dur %>% select(Subject.Name_a, previous_smoker_a), by = "Subject.Name_a")

df_merged <- df_merged %>%
  left_join(data_dur %>% select(Subject.Name_a, current_smoker_a), by = "Subject.Name_a")

sum(df_merged$adh_yes == 1)
sum(df_merged$adh_yes == 0)

median(df_merged$age2_a[df_merged$adh_yes == 1], na.rm = TRUE)
median(df_merged$age2_a[df_merged$adh_yes == 0], na.rm = TRUE)

t.test(age2_a ~ adh_yes, data = df_merged, var.equal = FALSE)  # Welch's t-test (default)

df_merged %>%
  group_by(adh_yes) %>%
  summarise(
    Q1 = quantile(age2_a, 0.25, na.rm = TRUE),
    Q3 = quantile(age2_a, 0.75, na.rm = TRUE),
    IQR = IQR(age2_a, na.rm = TRUE)
  )

#data$sex_numeric <- ifelse(data$Gender == "Male", 1, 0)
#mean(df_merged$sex_numeric_a == 1, na.rm = TRUE) * 100

mean(df_merged$sex_numeric_a[df_merged$adh_yes == 1], na.rm = TRUE) * 100
mean(df_merged$sex_numeric_a[df_merged$adh_yes == 0], na.rm = TRUE) * 100
chisq.test(df_merged$sex_numeric_a, df_merged$adh_yes)  # Welch's t-test (default)

df_merged %>%
  #filter(race_a) %>%  # optional filter
  group_by(adh_yes, race_a) %>%
  summarise(count = n()) %>%
  arrange(adh_yes, desc(count))

df_merged$race_numeric <- as.numeric(factor(df_merged$race_a))
chisq.test(df_merged$race_numeric, df_merged$adh_yes)  

#table(df_merged$race[df_merged$adh_yes == 1], na.rm = TRUE)
#table(data$ethnicity)
df_merged %>%
  #filter(race_a) %>%  # optional filter
  group_by(adh_yes, ethnicity_a) %>%
  summarise(count = n()) %>%
  arrange(adh_yes, desc(count))

df_merged$ethnic_numeric <- as.numeric(factor(df_merged$ethnicity_a))
chisq.test(df_merged$ethnic_numeric, df_merged$adh_yes)  

median(data$bmi_a, na.rm = FALSE)
quantile(data$bmi_a, probs = c(0.25, 0.75), na.rm = TRUE)
median(df_merged$bmi_a[df_merged$adh_yes == 1], na.rm = TRUE)
median(df_merged$bmi_a[df_merged$adh_yes == 0], na.rm = TRUE)
wilcox.test(bmi_a ~ adh_yes, data = df_merged, var.equal = FALSE)

df_merged %>%
  group_by(adh_yes) %>%
  summarise(
    Q1 = quantile(bmi_a, 0.25, na.rm = TRUE),
    Q3 = quantile(bmi_a, 0.75, na.rm = TRUE),
    IQR = IQR(bmi_a, na.rm = TRUE)
  )

#mean(data$bmi_level == 2, na.rm = TRUE) * 100
#mean(data$bmi_level == 3, na.rm = TRUE) * 100
df_merged$bmi_level_numeric <- as.numeric(factor(df_merged$bmi_level))
df_merged %>%
  #filter(race_a) %>%  # optional filter
  group_by(adh_yes, bmi_level) %>%
  summarise(count = n()) %>%
  arrange(adh_yes, desc(count))

chisq.test(df_merged$bmi_level_numeric, df_merged$adh_yes)  # Welch's t-test (default)


median(df_merged$mmrc_a[df_merged$adh_yes == 1], na.rm = TRUE)
median(df_merged$mmrc_a[df_merged$adh_yes == 0], na.rm = TRUE)
wilcox.test(mmrc_a ~ adh_yes, data = df_merged, var.equal = FALSE)

df_merged %>%
  group_by(adh_yes) %>%
  summarise(
    Q1 = quantile(mmrc_a, 0.25, na.rm = TRUE),
    Q3 = quantile(mmrc_a, 0.75, na.rm = TRUE),
    IQR = IQR(mmrc_a, na.rm = TRUE)
  )
#table(data$mmrc_a)
df_merged %>%
  #filter(race_a) %>%  # optional filter
  group_by(adh_yes, mmrc_a) %>%
  summarise(count = n()) %>%
  arrange(adh_yes, desc(count))

df_merged$mmrc_a_num <- as.numeric(factor(df_merged$mmrc_a))
t.test(mmrc_a_num ~ adh_yes, data = df_merged)  

df_merged$charlson_a_level <- 0
df_merged$charlson_a_level <- ifelse(df_merged$charlson_a < 3, 1,
                                   ifelse(df_merged$charlson_a <5, 2, 3))
df_merged %>%
  #filter(race_a) %>%  # optional filter
  group_by(adh_yes, charlson_a_level) %>%
  summarise(count = n()) %>%
  arrange(adh_yes, desc(count))
#table(charlsonlevels)
df_merged$charlson_a_level_num <- as.numeric(factor(df_merged$charlson_a_level))
wilcox.test(df_merged$charlson_a_level, df_merged$adh_yes) 

df_merged$fev1_updated_level <- ifelse(data$fev1_updated_a < 50, 1,
                                     ifelse(data$fev1_updated_a <70, 2, 3))
#table(fev1_levels)
df_merged %>%
  #filter(race_a) %>%  # optional filter
  group_by(adh_yes, fev1_updated_level) %>%
  summarise(count = n()) %>%
  arrange(adh_yes, desc(count))
#table(charlsonlevels)
df_merged$fev1_updated_num <- as.numeric(factor(df_merged$fev1_updated_level))
wilcox.test(fev1_updated_num ~ adh_yes, data = df_merged)  

#table(data$education_a)
df_merged %>%
  #filter(race_a) %>%  # optional filter
  group_by(adh_yes, education_a) %>%
  summarise(count = n()) %>%
  arrange(adh_yes, desc(count))
#table(charlsonlevels)

df_merged$education_num <- as.numeric(factor(df_merged$education_a))
df_merged <- df_merged %>%
  mutate(edu_level = if_else(education_num %in% c(2, 4), 1, 0))
chisq.test(df_merged$edu_level, df_merged$adh_yes)

#table(data$marital_status_a)
df_merged %>%
  #filter(race_a) %>%  # optional filter
  group_by(adh_yes, marital_status_a) %>%
  summarise(count = n()) %>%
  arrange(adh_yes, desc(count))
#table(charlsonlevels)
df_merged$marital_status_num <- as.numeric(factor(df_merged$marital_status_a))
df_merged <- df_merged %>%
  mutate(marital_status_level = if_else(marital_status_num %in% c(2), 1, 0))
chisq.test(df_merged$marital_status_level, df_merged$adh_yes) 

#table(data$previous_smoker_a)
df_merged %>%
  #filter(race_a) %>%  # optional filter
  group_by(adh_yes, previous_smoker_a) %>%
  summarise(count = n()) %>%
  arrange(adh_yes, desc(count))
#table(charlsonlevels)
df_merged$previous_smoker_num <- as.numeric(factor(df_merged$previous_smoker_a))
chisq.test(df_merged$previous_smoker_num, df_merged$adh_yes)


#table(data$current_smoker_a)
df_merged %>%
  #filter(race_a) %>%  # optional filter
  group_by(adh_yes, current_smoker_a) %>%
  summarise(count = n()) %>%
  arrange(adh_yes, desc(count))
#table(charlsonlevels)
df_merged$current_smoker_num <- as.numeric(factor(df_merged$current_smoker_a))
t.test(df_merged$current_smoker_num, df_merged$adh_yes) 


###sleep
#median(data$sd_time, na.rm = FALSE)
#quantile(data$sd_time, probs = c(0.25, 0.75), na.rm = TRUE)
median(df_merged$sd_time[df_merged$adh_yes == 1], na.rm = TRUE)
median(df_merged$sd_time[df_merged$adh_yes == 0], na.rm = TRUE)

wilcox.test(sd_time ~ adh_yes, data = df_merged, var.equal = FALSE)  # Welch's t-test (default)

df_merged %>%
  group_by(adh_yes) %>%
  summarise(
    Q1 = quantile(sd_time, 0.25, na.rm = TRUE),
    Q3 = quantile(sd_time, 0.75, na.rm = TRUE),
    IQR = IQR(sd_time, na.rm = TRUE)
  )

data_waso <- read.csv("df_wasobaseline.csv")
data_waso <- data_waso[data_waso$Subject.Name_a %in% data$Subject.Name_a, ]

#median(data_waso$sd_waso, na.rm = FALSE)
#quantile(data_waso$sd_waso, probs = c(0.25, 0.75), na.rm = TRUE)
median(df_merged$sd_waso[df_merged$adh_yes == 1], na.rm = TRUE)
median(df_merged$sd_waso[df_merged$adh_yes == 0], na.rm = TRUE)

wilcox.test(sd_waso ~ adh_yes, data = df_merged, var.equal = FALSE)  # Welch's t-test (default)

df_merged %>%
  group_by(adh_yes) %>%
  summarise(
    Q1 = quantile(sd_waso, 0.25, na.rm = TRUE),
    Q3 = quantile(sd_waso, 0.75, na.rm = TRUE),
    IQR = IQR(sd_waso, na.rm = TRUE)
  )


###phenotypes

data$sd_waso <- data_waso$sd_waso
data$sleep_waso_level <- ifelse(data$sd_waso > 25, 1,0)

data <- data %>%
  mutate(sd_level = if_else(sleep_dur_level %in% c(2, 3), 1, 0))

which(is.na(data$sd_level))

data <- data %>%
  mutate(summary_c = case_when(
    sleep_waso_level == 0 & sd_level == 0 ~ 0,
    sleep_waso_level == 1 & sd_level == 0 ~ 1,
    sleep_waso_level == 0 & sd_level == 1 ~ 2,
    sleep_waso_level == 1 & sd_level == 1 ~ 3
  ),
  summary_c = as.factor(summary_c))

table(data$summary_c)

#table(data$current_smoker_a)
data %>%
  #filter(race_a) %>%  # optional filter
  group_by(adh_yes, summary_c) %>%
  summarise(count = n()) %>%
  arrange(adh_yes, desc(count))
#table(charlsonlevels)
data$summary_c <- as.numeric(factor(data$summary_c))
wilcox.test(summary_c ~ adh_yes, data = data) 

############

hist_data <- hist(df_merged$sd_waso[df_merged$adh_yes == 1],
     main = "Adherent Participants",
     xlab = "Standard Deviation of WASO (min)",
     col = "lightgray",
     border = "white")

dens <- density(df_merged$sd_waso[df_merged$adh_yes == 1], na.rm = TRUE)
scale_factor <- max(hist_data$counts) / max(dens$y)
lines(dens$x, dens$y * scale_factor,
      col = "blue", lwd = 2)

hist_data <- hist(df_merged$sd_waso[df_merged$adh_yes == 0],
                  main = "Non-adherent Participants",
                  xlab = "Standard Deviation of WASO (min)",
                  col = "lightgray",
                  border = "white")

dens <- density(df_merged$sd_waso[df_merged$adh_yes == 0], na.rm = TRUE)
scale_factor <- max(hist_data$counts) / max(dens$y)
lines(dens$x, dens$y * scale_factor,
      col = "blue", lwd = 2)

####
hist_data <- hist(df_merged$sd_time[df_merged$adh_yes == 1],
                  main = "Adherent Participants",
                  xlab = "Standard Deviation of Duration (min)",
                  col = "lightgray",
                  border = "white")

dens <- density(df_merged$sd_time[df_merged$adh_yes == 1], na.rm = TRUE)
scale_factor <- max(hist_data$counts) / max(dens$y)
lines(dens$x, dens$y * scale_factor,
      col = "blue", lwd = 2)

hist_data <- hist(df_merged$sd_time[df_merged$adh_yes == 0],
                  main = "Non-adherent Participants",
                  xlab = "Standard Deviation of Duration (min)",
                  col = "lightgray",
                  border = "white")

dens <- density(df_merged$sd_time[df_merged$adh_yes == 0], na.rm = TRUE)
scale_factor <- max(hist_data$counts) / max(dens$y)
lines(dens$x, dens$y * scale_factor,
      col = "blue", lwd = 2)


