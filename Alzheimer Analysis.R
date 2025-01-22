str(alzheimer)
summary(alzheimer)
head(alzheimer)
library(tidyr)
if(!require(pacman)){
  install.packages('pacman')
}
if(!require(shiny)){
  install.packages('shiny')
}
#choose a data set
data("alzheimer")
#create a subdate set with Age, BMI,Educationlevel, Smoking, AlcoholConsumption, PhysicalActivity, DietQuality, SleepQuality
Analysisdata<-cbind(alzheimer$Age,alzheimer$BMI,alzheimer$EducationLevel,alzheimer$Gender,alzheimer$Smoking,alzheimer$AlcoholConsumption,alzheimer$PhysicalActivity,alzheimer$DietQuality,alzheimer$SleepQuality)
#drop DoctorInCharge using libraries
Analysisdata<-drop(alzheimer$DoctorInCharge)

View(Analysisdata)

#add names to data columns
colnames(Analysisdata)<-c("Age","BMI","EducationLevel","Gender","Smoking","AlcoholConsumption","PhysicalActivity","DietQuality","SleepQuality")

#limit data to age to above 65 and drop NA
Analysisdata[,1]<-ifelse(Analysisdata[,1]>64,Analysisdata[,1],NA)

#drop data of any age below 65
Analysisdata<-na.omit(Analysisdata)








