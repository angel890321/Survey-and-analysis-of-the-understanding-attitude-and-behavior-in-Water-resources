library(dplyr)
data <- read.csv("C:/Users/映均/水資源資料/water_連續資料.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
View(data)
## 知識作答情形
Knowledge.1_0_data <- subset(data,Knowledge_1=="0") 
Knowledge.1_1_data <- subset(data,Knowledge_1=="1") 
Knowledge.2_0_data <- subset(data,Knowledge_2=="0") 
Knowledge.2_1_data <- subset(data,Knowledge_2=="1") 
Knowledge.3_0_data <- subset(data,Knowledge_3=="0") 
Knowledge.3_1_data <- subset(data,Knowledge_3=="1") 
Knowledge.4_0_data <- subset(data,Knowledge_4=="0") 
Knowledge.4_1_data <- subset(data,Knowledge_4=="1") 
Knowledge.5_0_data <- subset(data,Knowledge_5=="0") 
Knowledge.5_1_data <- subset(data,Knowledge_5=="1") 
Knowledge.6_0_data <- subset(data,Knowledge_6=="0") 
Knowledge.6_1_data <- subset(data,Knowledge_6=="1") 
Knowledge.7_0_data <- subset(data,Knowledge_7=="0") 
Knowledge.7_1_data <- subset(data,Knowledge_7=="1")
Knowledge.8_0_data <- subset(data,Knowledge_8=="0") 
Knowledge.8_1_data <- subset(data,Knowledge_8=="1")
Knowledge.9_0_data <- subset(data,Knowledge_9=="0") 
Knowledge.9_1_data <- subset(data,Knowledge_9=="1") 
count(Knowledge.1_0_data) #224
count(Knowledge.1_1_data) #76
count(Knowledge.2_0_data) #198
count(Knowledge.2_1_data) #102
count(Knowledge.3_0_data) #193
count(Knowledge.3_1_data) #107
count(Knowledge.4_0_data) #123
count(Knowledge.4_1_data) #177
count(Knowledge.5_0_data) #214
count(Knowledge.5_1_data) #86
count(Knowledge.6_0_data) #171
count(Knowledge.6_1_data) #129
count(Knowledge.7_0_data) #228
count(Knowledge.7_1_data) #72
count(Knowledge.8_0_data) #115
count(Knowledge.8_1_data) #185
count(Knowledge.9_0_data) #237
count(Knowledge.9_1_data) #63

## 態度作答情形
Attitude.1_1_data <- subset(data,Attitude_1=="1") 
Attitude.1_2_data <- subset(data,Attitude_1=="2") 
Attitude.1_3_data <- subset(data,Attitude_1=="3") 
Attitude.1_4_data <- subset(data,Attitude_1=="4") 
Attitude.1_5_data <- subset(data,Attitude_1=="5") 
Attitude.2_1_data <- subset(data,Attitude_2=="1") 
Attitude.2_2_data <- subset(data,Attitude_2=="2") 
Attitude.2_3_data <- subset(data,Attitude_2=="3") 
Attitude.2_4_data <- subset(data,Attitude_2=="4") 
Attitude.2_5_data <- subset(data,Attitude_2=="5") 
Attitude.3_1_data <- subset(data,Attitude_3=="1") 
Attitude.3_2_data <- subset(data,Attitude_3=="2") 
Attitude.3_3_data <- subset(data,Attitude_3=="3") 
Attitude.3_4_data <- subset(data,Attitude_3=="4") 
Attitude.3_5_data <- subset(data,Attitude_3=="5") 
Attitude.4_1_data <- subset(data,Attitude_4=="1") 
Attitude.4_2_data <- subset(data,Attitude_4=="2") 
Attitude.4_3_data <- subset(data,Attitude_4=="3") 
Attitude.4_4_data <- subset(data,Attitude_4=="4") 
Attitude.4_5_data <- subset(data,Attitude_4=="5") 
Attitude.5_1_data <- subset(data,Attitude_5=="1") 
Attitude.5_2_data <- subset(data,Attitude_5=="2") 
Attitude.5_3_data <- subset(data,Attitude_5=="3") 
Attitude.5_4_data <- subset(data,Attitude_5=="4") 
Attitude.5_5_data <- subset(data,Attitude_5=="5") 
Attitude.6_1_data <- subset(data,Attitude_6=="1") 
Attitude.6_2_data <- subset(data,Attitude_6=="2") 
Attitude.6_3_data <- subset(data,Attitude_6=="3") 
Attitude.6_4_data <- subset(data,Attitude_6=="4") 
Attitude.6_5_data <- subset(data,Attitude_6=="5") 
Attitude.7_1_data <- subset(data,Attitude_7=="1") 
Attitude.7_2_data <- subset(data,Attitude_7=="2") 
Attitude.7_3_data <- subset(data,Attitude_7=="3") 
Attitude.7_4_data <- subset(data,Attitude_7=="4") 
Attitude.7_5_data <- subset(data,Attitude_7=="5") 
Attitude.8_1_data <- subset(data,Attitude_8=="1") 
Attitude.8_2_data <- subset(data,Attitude_8=="2") 
Attitude.8_3_data <- subset(data,Attitude_8=="3") 
Attitude.8_4_data <- subset(data,Attitude_8=="4") 
Attitude.8_5_data <- subset(data,Attitude_8=="5") 
Attitude.9_1_data <- subset(data,Attitude_9=="1") 
Attitude.9_2_data <- subset(data,Attitude_9=="2") 
Attitude.9_3_data <- subset(data,Attitude_9=="3") 
Attitude.9_4_data <- subset(data,Attitude_9=="4") 
Attitude.9_5_data <- subset(data,Attitude_9=="5") 
count(Attitude.1_1_data) #49
count(Attitude.1_2_data) #68
count(Attitude.1_3_data) #99
count(Attitude.1_4_data) #50
count(Attitude.1_5_data) #34
count(Attitude.2_1_data) #5
count(Attitude.2_2_data) #9
count(Attitude.2_3_data) #52
count(Attitude.2_4_data) #118
count(Attitude.2_5_data) #116
count(Attitude.3_1_data) #1
count(Attitude.3_2_data) #12
count(Attitude.3_3_data) #63
count(Attitude.3_4_data) #119
count(Attitude.3_5_data) #105
count(Attitude.4_1_data) #11
count(Attitude.4_2_data) #30
count(Attitude.4_3_data) #72
count(Attitude.4_4_data) #75
count(Attitude.4_5_data) #112
count(Attitude.5_1_data) #22
count(Attitude.5_2_data) #53
count(Attitude.5_3_data) #108
count(Attitude.5_4_data) #80
count(Attitude.5_5_data) #37
count(Attitude.6_1_data) #1
count(Attitude.6_2_data) #10
count(Attitude.6_3_data) #65
count(Attitude.6_4_data) #125
count(Attitude.6_5_data) #99
count(Attitude.7_1_data) #38
count(Attitude.7_2_data) #49
count(Attitude.7_3_data) #81
count(Attitude.7_4_data) #94
count(Attitude.7_5_data) #38
count(Attitude.8_1_data) #15
count(Attitude.8_2_data) #29
count(Attitude.8_3_data) #58
count(Attitude.8_4_data) #123
count(Attitude.8_5_data) #75
count(Attitude.9_1_data) #5
count(Attitude.9_2_data) #6
count(Attitude.9_3_data) #26
count(Attitude.9_4_data) #78
count(Attitude.9_5_data) #185


Behavior.1_1_data <- subset(data,Behavior_1=="1") 
Behavior.1_2_data <- subset(data,Behavior_1=="2") 
Behavior.1_3_data <- subset(data,Behavior_1=="3") 
Behavior.1_4_data <- subset(data,Behavior_1=="4") 
Behavior.1_5_data <- subset(data,Behavior_1=="5") 
Behavior.2_1_data <- subset(data,Behavior_2=="1") 
Behavior.2_2_data <- subset(data,Behavior_2=="2") 
Behavior.2_3_data <- subset(data,Behavior_2=="3") 
Behavior.2_4_data <- subset(data,Behavior_2=="4") 
Behavior.2_5_data <- subset(data,Behavior_2=="5") 
Behavior.3_1_data <- subset(data,Behavior_3=="1") 
Behavior.3_2_data <- subset(data,Behavior_3=="2") 
Behavior.3_3_data <- subset(data,Behavior_3=="3") 
Behavior.3_4_data <- subset(data,Behavior_3=="4") 
Behavior.3_5_data <- subset(data,Behavior_3=="5") 
Behavior.4_1_data <- subset(data,Behavior_4=="1") 
Behavior.4_2_data <- subset(data,Behavior_4=="2") 
Behavior.4_3_data <- subset(data,Behavior_4=="3") 
Behavior.4_4_data <- subset(data,Behavior_4=="4") 
Behavior.4_5_data <- subset(data,Behavior_4=="5") 
Behavior.5_1_data <- subset(data,Behavior_5=="1") 
Behavior.5_2_data <- subset(data,Behavior_5=="2") 
Behavior.5_3_data <- subset(data,Behavior_5=="3") 
Behavior.5_4_data <- subset(data,Behavior_5=="4") 
Behavior.5_5_data <- subset(data,Behavior_5=="5") 
Behavior.6_1_data <- subset(data,Behavior_6=="1") 
Behavior.6_2_data <- subset(data,Behavior_6=="2") 
Behavior.6_3_data <- subset(data,Behavior_6=="3") 
Behavior.6_4_data <- subset(data,Behavior_6=="4") 
Behavior.6_5_data <- subset(data,Behavior_6=="5") 
Behavior.7_1_data <- subset(data,Behavior_7=="1") 
Behavior.7_2_data <- subset(data,Behavior_7=="2") 
Behavior.7_3_data <- subset(data,Behavior_7=="3") 
Behavior.7_4_data <- subset(data,Behavior_7=="4") 
Behavior.7_5_data <- subset(data,Behavior_7=="5") 
Behavior.8_1_data <- subset(data,Behavior_8=="1") 
Behavior.8_2_data <- subset(data,Behavior_8=="2") 
Behavior.8_3_data <- subset(data,Behavior_8=="3") 
Behavior.8_4_data <- subset(data,Behavior_8=="4") 
Behavior.8_5_data <- subset(data,Behavior_8=="5") 
count(Behavior.1_1_data) #13
count(Behavior.1_2_data) #26
count(Behavior.1_3_data) #136
count(Behavior.1_4_data) #101
count(Behavior.1_5_data) #24
count(Behavior.2_1_data) #15
count(Behavior.2_2_data) #35
count(Behavior.2_3_data) #129
count(Behavior.2_4_data) #95
count(Behavior.2_5_data) #26
count(Behavior.3_1_data) #16
count(Behavior.3_2_data) #27
count(Behavior.3_3_data) #94
count(Behavior.3_4_data) #107
count(Behavior.3_5_data) #56
count(Behavior.4_1_data) #22
count(Behavior.4_2_data) #49
count(Behavior.4_3_data) #129
count(Behavior.4_4_data) #73
count(Behavior.4_5_data) #30
count(Behavior.5_1_data) #20
count(Behavior.5_2_data) #54
count(Behavior.5_3_data) #104
count(Behavior.5_4_data) #73
count(Behavior.5_5_data) #49
count(Behavior.6_1_data) #9
count(Behavior.6_2_data) #17
count(Behavior.6_3_data) #59
count(Behavior.6_4_data) #115
count(Behavior.6_5_data) #100
count(Behavior.7_1_data) #23
count(Behavior.7_2_data) #82
count(Behavior.7_3_data) #93
count(Behavior.7_4_data) #63
count(Behavior.7_5_data) #39
count(Behavior.8_1_data) #16
count(Behavior.8_2_data) #46
count(Behavior.8_3_data) #74
count(Behavior.8_4_data) #87
count(Behavior.8_5_data) #77

## 知識統計摘要
Knowledge_T <- data[,c("Knowledge_1")]
Knowledge2_T <- apply(data[,c("Knowledge_2","Knowledge_3","Knowledge_4")],1,sum)
Knowledge3_T <- apply(data[,c("Knowledge_5","Knowledge_6","Knowledge_7")],1,sum)
Knowledge4_T <- apply(data[,c("Knowledge_8","Knowledge_9")],1,sum)

mean(Knowledge_T) # 0.25
mean(Knowledge2_T)# 1.29
mean(Knowledge3_T)# 0.96
mean(Knowledge4_T) # 0.83

var(Knowledge_T) # 0.19
var(Knowledge2_T)# 0.63
var(Knowledge3_T)# 0.65
var(Knowledge4_T) # 0.40

## 內涵統計摘要
interest_T <- data[,c("Attitude_1")]
interest2_T <- data[,c("Attitude_2")]
interest3_T <- data[,c("Attitude_3")]
values_T <- data[,c("Attitude_4")]   
values2_T <- apply(data[,c("Attitude_5","Attitude_6")], 1, sum) 
values3_T <- apply(data[,c("Attitude_7","Attitude_8","Attitude_9")], 1, sum)
behavior_T <- apply(data[,c("Behavior_1","Behavior_2","Behavior_3")], 1, sum) 
behavior2_T <- data[,c("Behavior_4")] 
attempt_T <- apply(data[,c("Behavior_5","Behavior_6","Behavior_7","Behavior_8")], 1, sum)  
attempt2_T <- data[,c("Behavior_8")]

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