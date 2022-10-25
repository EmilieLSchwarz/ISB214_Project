# First exploration of the data 
library(pacman)
p_load(dplyr, ggplot2, tidyverse, MASS, tidyr, visdat, DataExplorer, expss, gtsummary, knitr,ggpubr,broom.helpers,broom, epiDisplay, tidymodels, yardstick)
inca2 <- read.csv("inca2_survey.csv")

inca2$diet <- as.factor(inca2$diet)
inca2$education <- as.factor(inca2$education)
inca2$household_4c <- as.factor(inca2$household_4c)
inca2$bmiclass <- as.factor(inca2$household_4c)
inca2$supplements <- as.factor(inca2$supplements)
inca2$bmiclass <- as.factor(inca2$bmiclass)
inca2$ipaqnx <- as.factor(inca2$ipaqnx)
inca2$income <- as.factor(inca2$income)
inca2$age_categories <- as.factor(inca2$age_categories)
inca2$smoking_status <- as.factor(inca2$smoking_status)

catvars <- (c("diet","disease", "season", "smoking_status", "age_categories", "income", "ipaqnx", "bmiclass", "education", "household_4c"))

inca2[, catvars] <- lapply(inca2[, catvars], factor)

# making group 1 (average of group 1)
group1_fun <- function(data){
  for(i in 1:nrow(data)){
    data[i,"grp1"]= (data$food_gp_1s[i]+data$food_gp_3s[i]+data$food_gp_4s[i]+data$food_gp_5s[i])/(4)
    }
return(data$grp1)
  }
inca2$grp1 <- group1_fun(inca2)

# making group 2
group2_fun <- function(data){
  for(i in 1:nrow(data)){
    data[i,"grp2"]= (data$food_gp_2s[i]+data$food_gp_6s[i]+data$food_gp_7s[i]+data$food_gp_8s[i]+data$food_gp_28s[i]+data$food_gp_29s[i]+data$food_gp_30s[i]+data$food_gp_41s[i]+data$food_gp_42s[i]+data$food_gp_43s[i])/(10)
  }
  return(data$grp2)
}
inca2$grp2 <- group2_fun(inca2)

#making group 3 
group3_fun <- function(data){
  for(i in 1:nrow(data)){
    data[i,"grp3"]= (data$food_gp_9s[i]+data$food_gp_10s[i]+data$food_gp_11s[i])/(3)
  }
  return(data$grp3)
}
inca2$grp3 <- group3_fun(inca2)

#making group 4 

group4_fun <- function(data){
  for(i in 1:nrow(data)){
    data[i,"grp4"]= (data$food_gp_12s[i]+data$food_gp_17s[i]+data$food_gp_18s[i]+data$food_gp_19s[i]+data$food_gp_21s[i]+data$food_gp_22s[i])/(6)
  }
  return(data$grp4)
}
inca2$grp4 <- group4_fun(inca2)
