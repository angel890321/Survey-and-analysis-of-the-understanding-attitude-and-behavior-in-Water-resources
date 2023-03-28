library(dplyr)
data <- read.csv("C:/Users/映均/水資源資料/water_連續資料.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
cor(data$Knowledge_score,data$Attitude_score) #0.084
#Pearson's product-moment correlation
cor.test(data$Knowledge_score,data$Attitude_score) # p-value = 0.1434不顯著，無相關

cor(data$Knowledge_score,data$Behavior_score) #0.183
cor.test(data$Knowledge_score,data$Behavior_score) # p-value = 0.001435顯著，有相關

cor(data$Attitude_score,data$Behavior_score) #0.264
cor.test(data$Attitude_score,data$Behavior_score) # p-value = 3.69e-06顯著，有相關

interest_T <- apply(data[,c("Attitude_1","Attitude_2")], 1, sum) 
interest2_T <- data[,c("Attitude_3")]
interest3_T <- data[,c("Attitude_4")]
values_T <- data[,c("Attitude_5")]
values2_T <- apply(data[,c("Attitude_6","Attitude_7")], 1, sum) 
values3_T <- apply(data[,c("Attitude_8","Attitude_9","Attitude_10")], 1, sum) 
behavior_T <- apply(data[,c("Behavior_1","Behavior_2")], 1, sum) 
behavior2_T <- data[,c("Behavior_3")] 
attempt_T <- apply(data[,c("Behavior_4","Behavior_5","Behavior_6")], 1, sum)  
attempt2_T <- data[,c("Behavior_7")]

mean(interest_T) # 2.84
mean(interest2_T)# 4.10
mean(interest3_T)# 4.05
mean(values_T) # 3.82
mean(values2_T)#  7.23
mean(values3_T)# 11.30
mean(behavior_T)# 10.13
mean(behavior2_T)# 3.13
mean(attempt_T)# 13.78
mean(attempt2_T)# 3.54

var(interest_T) # 1.48
var(interest2_T) # 0.82
var(interest3_T) # 0.75
var(values_T) # 1.32
var(values2_T)# 1.84
var(values3_T)# 6.06
var(behavior_T) #  6.37
var(behavior2_T) # 1.09
var(attempt_T) # 7.58
var(attempt2_T) #1.39

cor.test(data$Behavior_score,interest_T) #r=0.372 p-value = 2.952e-10顯著，有相關
cor.test(data$Behavior_score,interest2_T) #r=0.416 p-value = 0.0002888顯著，有相關
cor.test(data$Behavior_score,interest3_T) #r=0.563 p-value = 4.359e-15顯著，有相關
cor.test(data$Behavior_score,values_T) #r=0.037 p-value = 0.479不顯著，無相關
cor.test(data$Behavior_score,values2_T) #r=0.317 p-value = 9.727e-09顯著，有相關
cor.test(data$Behavior_score,values3_T) #r=-0.09 p-value =  0.4531不顯著，無相關

cor.test(data$Attitude_score,behavior_T) #r=0.5444 p-value = 0.0001909顯著，有相關
cor.test(interest_T,behavior_T) # r=0.293，p-value = 1.209e-0顯著，有相關
cor.test(interest2_T,behavior_T) # r=0.397  p-value =  0.005715顯著，有相關
cor.test(interest3_T,behavior_T) # r=0.580，p-value = 3.394e-14顯著，有相關
cor.test(values_T,behavior_T) #r=-.005  p-value = 0.9859不顯著，無相關
cor.test(values2_T,behavior_T) #r=0.225  p-value = 5.061e-07顯著，有相關
cor.test(values3_T,behavior_T) #r=-0.046  p-value = 0.6174不顯著，無相關

cor.test(data$Attitude_score,behavior2_T) #r=0.435 p-value = 0.0001412顯著，有相關
cor.test(interest_T,behavior2_T) # r=0.182，p-value = 3.239e-08顯著，有相關
cor.test(interest2_T,behavior2_T) # r=0.338  p-value = 0.0202顯著，有相關
cor.test(interest3_T,behavior2_T) # r=0.461，p-value = 8.579e-10顯著，有相關
cor.test(values_T,behavior2_T) #r=0.01  p-value = 4.779e-09顯著，有相關
cor.test(values2_T,behavior2_T) #r=0.306  p-value = 0.4972不顯著，無相關
cor.test(values3_T,behavior2_T) #r=-0.157  p-value = 0.5418不顯著，無相關

cor.test(data$Attitude_score,attempt_T) #r=0.512 p-value = 8.088e-05顯著，有相關
cor.test(interest_T,attempt_T) # r=0.329，p-value = 5.344e-09顯著，有相關
cor.test(interest2_T,attempt_T) # r=0.200  p-value = 0.0004641顯著，有相關
cor.test(interest3_T,attempt_T) # r=0.312，p-value = 3.424e-08顯著，有相關
cor.test(values_T,attempt_T) #r=0.063  p-value = 0.279不顯著，無相關
cor.test(values2_T,attempt_T) #r=0.270  p-value = 2.149e-06顯著，有相關
cor.test(values3_T,attempt_T) #r=-0.043  p-value = 0.4554不顯著，無相關

cor.test(data$Attitude_score,attempt2_T) #r=0.125 p-value =0.02994顯著，有相關
cor.test(interest_T,attempt2_T) # r=0.163，p-value = 0.004576顯著，有相關
cor.test(interest2_T,attempt2_T) # r=0.116  p-value = 0.04443顯著，有相關
cor.test(interest3_T,attempt2_T) # r=0.134，p-value = 0.02055顯著，有相關
cor.test(values_T,attempt2_T) #r=0.019  p-value = 0.7398不顯著，無相關
cor.test(values2_T,attempt2_T) #r=0.190  p-value = 0.0009327顯著，有相關
cor.test(values3_T,attempt2_T) #r=-0.010  p-value = 0.8669不顯著，無相關

