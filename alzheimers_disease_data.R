#Created by: Hugh Ayara
#Date: 22th January 2025
#Exploring the 🧠 Alzheimer's Disease Dataset 🧠

###=========================== TABLE OF CONTENTS ===================================###
### [A] Libraries
### [B] Load Dataset
### [C] Import Dataset
### [D] Cleaning
###   [D.1] Gender
###   [D.2] Age
###   [D.3] Ethnicity
###   [D.4] Education Level
###   [D.5] Diet Quality
###   [D.6] Sleep Quality
###   [D.7] BMI
###   [D.8] Alcohol Consumption
###   [D.9] Physical Activity
###   [D.10] ADL
###   



###########################
### [A] Libraries
###########################

##Pacman| Once installed you don't need to call in other packages
install.packages("pacman")
library(pacman)
pacman::p_load(reshape, foreign, lubridate, stringr, readxl, dplyr, openxlsx, writexl, naniar, 
               labelled, parsedate, ggplot2, maps, sf)

###########################
### [B] Load Dataset
###########################
setwd("~/GitHub/Intro--Hugh-Ayara")

#############################
### [C] Import Dataset
##############################
alzheimers_disease<- read_excel("alzheimers_disease.xlsx", col_types = "text")

## Check Number of Observations
dim(alzheimers_disease)
## Number of records: 2,149   Number of variables: 35

#############################
### [D] Cleaning
##############################

#---------------------------------------#
### [D.1] Gender
#---------------------------------------#
### Gender: Gender of the patients, where 0 represents Male and 1 represents Female
table(alzheimers_disease$Gender)
# 0    1 
# 1061 1088 

### Convert Gender to a factor with labels
alzheimers_disease$Gender <- factor(alzheimers_disease$Gender, levels = c(0, 1), labels = c("Male", "Female"))
table(alzheimers_disease$Gender)
# Male  Female 
# 1061  1088 

#---------------------------------------#
### [D.2] Age
#---------------------------------------#
###  Age: The age of the patients ranges from 60 to 90 years.

## Make age variable an integer
alzheimers_disease$Age <- as.numeric(alzheimers_disease$Age)

table(alzheimers_disease$Age)
 
# Create age categories
num_categories <- 3 # Number of categories
alzheimers_disease$Age_category <- cut(
  alzheimers_disease$Age,
  breaks = seq(60, 90, length.out = num_categories + 1), # Define breaks: 60, 70, 80, 90
  include.lowest = TRUE,         # Include the lowest value in the first category
  labels = paste0("Group ", 1:num_categories) # Optional: Custom labels
)

# Rename the levels
levels(alzheimers_disease$Age_category) <- c("60-70", "70-80", "80-90")

table(alzheimers_disease$Age_category)
## 60-70 70-80 80-90 
##  775   697   677 

#---------------------------------------#
### [D.3] Ethnicity
#---------------------------------------#
## Ethnicity: The ethnicity of the patients, coded as follows:
##  0: Caucasian
##  1: African American
##  2: Asian
##  3: Other

## Make ethnicity variable an integer
alzheimers_disease$Ethnicity <- as.numeric(alzheimers_disease$Ethnicity)

# Assign labels to Ethnicity variable
alzheimers_disease$ethnicity <- factor(
  alzheimers_disease$Ethnicity,
  levels = c(0,1, 2, 3),               # Numeric codes representing ethnicities
  labels = c("Caucasian", "African American", "Asian", "Other") # Corresponding labels
)

table(alzheimers_disease$ethnicity)
## Caucasian    African American      Asian    Other 
##      1278              454          206      211 

#---------------------------------------#
### [D.4] Education Level
#---------------------------------------#
## EducationLevel: The education level of the patients, coded as follows:
## 0: None
## 1: High School
## 2: Bachelor's
## 3: Higher

## Make education level variable an integer
alzheimers_disease$EducationLevel <- as.numeric(alzheimers_disease$EducationLevel)

# Assign labels to Education Level variable
alzheimers_disease$education_level <- factor(
  alzheimers_disease$EducationLevel,
  levels = c(0,1, 2, 3),               # Numeric codes representing education level
  labels = c("None", "High School", "Bachelor's", "Higher") # Corresponding labels
)

table(alzheimers_disease$education_level)
## None  High School  Bachelor's   Higher 
##  446         854         636     213

#---------------------------------------#
### [D.5] Diet Quality
#---------------------------------------#
## DietQuality: Diet quality score, ranging from 0 to 10.

## Make Diet quality variable an integer
alzheimers_disease$DietQuality <- as.numeric(alzheimers_disease$DietQuality)

# Create categories for DietQuality
alzheimers_disease$diet_quality <- cut(
  alzheimers_disease$DietQuality,
  breaks = c(0, 3, 6, 8, 10),                   # Define category boundaries
  labels = c("Poor", "Fair", "Good", "Excellent"), # Assign descriptive labels
  include.lowest = TRUE                         # Include the lowest score in the first category
)

table(alzheimers_disease$diet_quality)
##  Poor    Fair  Good Excellent 
##  659     611   449     430 

#---------------------------------------#
### [D.6] Sleep Quality
#---------------------------------------#
## SleepQuality: Sleep quality score, ranging from 4 to 10

## Make Sleep quality variable an integer
alzheimers_disease$SleepQuality <- as.numeric(alzheimers_disease$SleepQuality)

# Create categories for SleepQuality
alzheimers_disease$sleep_quality <- cut(
  alzheimers_disease$SleepQuality,
  breaks = c(4, 6, 8, 10),                    # Define category boundaries
  labels = c("Poor", "Moderate", "Good"),     # Assign descriptive labels
  include.lowest = TRUE                       # Include the lowest score in the first category
)
table(alzheimers_disease$sleep_quality)
##  Poor   Moderate    Good  
##   687      713      749
#---------------------------------------#
### [D.7] BMI
#---------------------------------------#
alzheimers_disease$BMI <- as.numeric(alzheimers_disease$BMI)
alzheimers_disease$bmi<- round(alzheimers_disease$BMI, 2)
### [D.8] Alcohol Consumption
#---------------------------------------#
alzheimers_disease$AlcoholConsumption <- as.numeric(alzheimers_disease$AlcoholConsumption)
alzheimers_disease$alcoholConsumption<- round(alzheimers_disease$AlcoholConsumption, 2)
### [D.9] Physical Activity
#---------------------------------------#
alzheimers_disease$PhysicalActivity <- as.numeric(alzheimers_disease$PhysicalActivity)
alzheimers_disease$physicalActivity<- round(alzheimers_disease$PhysicalActivity, 2)
### [D.9] Physical Activity
#---------------------------------------#
alzheimers_disease$ADL <- as.numeric(alzheimers_disease$ADL)
alzheimers_disease$adl<- round(alzheimers_disease$ADL, 2)
#---------------------------------------#
### [D.11] Smoking
#---------------------------------------#
### Smoking: Smoking patients, where 0 represents No and 1 represents Yes
table(alzheimers_disease$Smoking)
# 0    1 
# 1529 620 
### Convert Gender to a factor with labels
alzheimers_disease$Smoking <- factor(alzheimers_disease$Smoking, levels = c(0, 1), labels = c("No", "Yes"))
table(alzheimers_disease$Smoking)
# Male  Female 
# 1529  620 

#############################
### [E] Analysis
##############################

################################################################################
### [E.0] Sleep Quality
################################################################################

plot(
  density(alzheimers_disease$SleepQuality),
  xlab = "Sleep Quality",
  main = "Density Plot of Sleep Quality",
  col = "purple",
  lwd = 2
)

## Interpretations
## [1] The density plot of sleep quality has a peak of 5 and 9, showing that most patients have a sleep quality of 5 and 9
## [2] Interestingly, there is a dip at 6, showing that there are few patients who have a sleep quality of 6 

################################################################################
### [E.1] Diet Quality
################################################################################

plot(
  density(alzheimers_disease$DietQuality),
  xlab = "Diet Quality",
  main = "Density Plot of Diet Quality",
  col = "purple",
  lwd = 2
)

## Interpretations
## [1] The density plot of sleep quality has a peak of 1 and 8. 
## [2] The highest peak is 8 showing that most patients have a sleep quality of 8
## [3] Interestingly, there is a dip at 5, showing that there are few patients who have a sleep quality of 5 

################################################################################
### [E.2] Relationship between Sleep Quality and Age
################################################################################

plot(
  alzheimers_disease$Age_category, alzheimers_disease$SleepQuality,
  xlab = "Age",
  ylab = "Sleep Quality",
  main = "Box Plot of Age vs Sleep Quality",
  pch = 19, col = "lightblue"
)

model <- lm(SleepQuality ~ Age, data = alzheimers_disease)
abline(model, col = "red", lwd = 2)

## Interpretations
## [1] The median sleep quality increases with age
## [2] Patients between 70 and 80 years have higher variability in sleep quality (taller boxes) compared to other age groups


################################################################################
### [E.3] Relationship between Sleep Quality and Gender
################################################################################

# Compare sleep quality across gender
boxplot(
  SleepQuality ~ Gender, data = alzheimers_disease,
  xlab = "Gender",
  ylab = "Sleep Quality",
  main = "Sleep Quality Across Gender",
  col = "lightblue"
)

################################################################################
### [E.4] Relationship between Sleep Quality and Ethnicity
################################################################################

# Compare sleep quality across ethnicity
boxplot(
  SleepQuality ~ ethnicity, data = alzheimers_disease,
  xlab = "Ethnicity",
  ylab = "Sleep Quality",
  main = "Sleep Quality Across Ethnicity",
  col = "lightblue"
)

# Calculate mean sleep quality per group
library(dplyr)
grouped_data <- alzheimers_disease %>%
  group_by(Age_category) %>%
  summarize(MeanSleepQuality = mean(SleepQuality, na.rm = TRUE ))

# Bar plot
barplot(
  grouped_data$MeanSleepQuality,
  names.arg = grouped_data$Age_category,
  xlab = "Age Category",
  ylab = "Mean Sleep Quality",
  main = "Mean Sleep Quality by Age Group",
  col = "lightblue",
)

################################################################################
### [E.5] Relationship between Diet Quality and Gender
################################################################################

ggplot(alzheimers_disease, aes(x = Age_category, y = sleep_quality)) +
  geom_point(color = "lightblue", alpha = 0.7) + # Scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Regression line
  labs(
    title = "Relationship Between Sleep Quality and Gender",
    x = "Gender",
    y = "Sleep Quality (Poor to Moderate)"
  ) +
  theme_minimal()

################################################################################
### [E.6] Relationship between BMI and Ethnicity
################################################################################
# Relationship between BMI and Ethnicity for Patients between 60 and 90 Years
bmi_60_80 <- subset(alzheimers_disease, Age>=60 & Age<=80)
bmi_60_80$Ethnicity <- factor(bmi_60_80$Ethnicity, 
                              levels = c(0, 1, 2, 3), 
                              labels = c("Caucasian", "African American", "Asian", "Other"))
ggplot(bmi_60_80, aes(x = bmi, y = Age, colour = as.factor(Ethnicity))) +
  geom_point(alpha= 0.7, size = 3) + # Scatter points
  geom_smooth(method = "lm", se= TRUE, colour= "black",linetype= "dashed")+
  scale_color_manual(values = c("Caucasian"= "blue",
                                "African American"= "red",
                                "Asian"= "green",
                                "Other"= "purple"))+
  labs(
    x = "BMI",
    y = "Age ",
    color= "Ethnicity",
    title = "Scatter Plot of BMI by Ethnicity"
  ) +
  theme_minimal() # Clean theme
theme(
  plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
  axis.title = element_text(size = 14),
  legend.title = element_text(size = 12),
  legend.position = "right"
)
