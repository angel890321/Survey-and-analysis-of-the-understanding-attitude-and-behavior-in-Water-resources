######################################
####  Cronbach's alpha value test ####
######################################

#getwd()
#setwd("C:/Users/映均/Desktop/統計實務(cronbach's alpha value)")


#install.packages("psych")
library(psych)
library(psych)
library(dplyr)
data <- read.csv("C:/Users/映均/水資源資料/water_連續資料.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
View(data)
library(dplyr)
## 態度題
test_a <- select(data,c(Attitude_1,Attitude_2,
                             Attitude_3,Attitude_4,Attitude_5,Attitude_6,
                             Attitude_7,Attitude_8,Attitude_9))
factor1_a <-select(data,c(Attitude_1,Attitude_2,Attitude_3))
factor2_a <-select(data,c(Attitude_4,Attitude_5,Attitude_6))
factor3_a <-select(data,c(Attitude_7,Attitude_8,Attitude_9))

select(data,c(Attitude_1,Attitude_2,
              Attitude_3,Attitude_4,Attitude_5,Attitude_6,
              Attitude_7,Attitude_8,Attitude_9))
## 行為題
test_b <- select(data,c(Behavior_1,Behavior_2,Behavior_3,Behavior_4,
                       Behavior_5,Behavior_6,Behavior_7,Behavior_8))
factor1_b <-select(data,c(Behavior_1,Behavior_2,Behavior_3,Behavior_4))
factor2_b <-select(data,c( Behavior_5,Behavior_6,Behavior_7,Behavior_8))
## test Attitude
alpha(test_a) #0.53

# factor1_a
alpha(factor1_a) #0.45  
# factor2_a
alpha(factor2_a) #-0.075   
# factor3_a
alpha(factor3_a) #0.66 



## test Behavior
alpha(test_b) #0.77 且沒有warning
# factor1_b
alpha(factor1_b) #0.84  
alpha(factor2_b) # 0.45 
alpha(factor1_a) #0.45  


