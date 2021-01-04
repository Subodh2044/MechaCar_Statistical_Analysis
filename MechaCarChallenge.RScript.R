#Import package
library(dplyr)
library(tidyverse)


# Deliverable 1
dataframe <- read.csv("MechaCar_mpg.csv")
View(dataframe)

# Linear Regression
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD , data = dataframe)

# Summary of linear Regression
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD , data = dataframe))

# Remove spoiler angle 
summary(lm(mpg~ vehicle_length + vehicle_weight  + ground_clearance + 
             AWD, data=dataframe))
#Remove AWD
summary(lm(mpg~ vehicle_length + vehicle_weight + ground_clearance,
           data=dataframe))
#Remove vehicle_weight
summary(lm(mpg~ vehicle_length  + ground_clearance,
           data=dataframe))

# Check multicollinearity
mecha_matrix <- as.matrix(mecha[,c("vehicle_length", "vehicle_weight","spoiler_angle",
                                   "ground_clearance", "AWD")]) 
cor(mecha_matrix)


# Deliverable 2

dataframe2 <- read.csv("Suspension_Coil.csv", sep = ",")
 total_summary <- dataframe2 %>%
   summarise(Mean = mean(PSI),
             Median = median(PSI),
             Variance = var(PSI),
             SD = sd(PSI))
   
total_summary

lot_summary <- dataframe2 %>% group_by(Manufacturing_Lot) %>% 
  summarise(Mean = mean(PSI),
            Median = median(PSI),
            Variance = var(PSI),
            SD = sd(PSI), .groups = 'keep')
            

lot_summary

#Deliverable 3

t.test(lot_summary$Mean, mu=1500) 

# Each lots
lot1_summary <- subset(dataframe2,Manufacturing_Lot=='Lot1')
t.test(lot1_summary$PSI, mu=1500)

lot1_summary <- subset(dataframe2,Manufacturing_Lot=='Lot2')
t.test(lot1_summary$PSI, mu=1500)

lot1_summary <- subset(dataframe2,Manufacturing_Lot=='Lot3')
t.test(lot1_summary$PSI, mu=1500)

#mtcars
#mecha
t.test(dataframe$mpg, mtcars$mpg)