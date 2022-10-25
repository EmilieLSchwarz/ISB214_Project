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




