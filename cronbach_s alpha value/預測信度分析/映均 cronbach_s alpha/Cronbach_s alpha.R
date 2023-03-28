######################################
####  Cronbach's alpha value test ####
######################################
######################################
####  Cronbach's alpha value test ####
######################################
getwd()
setwd("C:/Users/映均/Desktop/統計實務(cronbach's alpha value)")


#install.packages("psych")
library(psych)


## input the data (new_water&new_water2)
data1 <- read.csv("./new_water.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
data2 <- read.csv("./new_water23.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
## 也可使用
## read.table("./new_water.csv", header=TRUE, sep=",", fileEncoding = "UTF-8-BOM", fill=TRUE)


data3 <- merge(x=data1, y=data2, by.x=c("電話末四碼"),  ## merge two dataset by numbers
            by.y=c("電話後四碼是."), all=TRUE)
write.csv(data3, file = "前後測統整.csv")

data3 <- data3[complete.cases(data3),]   # delete NA
data3 <- data3[!duplicated(data3[,1]),]  # select the unique obs
data3
write.csv(data3, file = "前後測統整(刪NA+重複值).csv")

library(dplyr)
## 前測知識題
pre_test_k <- select(data3,c("電話末四碼",Knowledge_1.x,Knowledge_2.x,Knowledge_3.x,
                       Knowledge_4.x,Knowledge_5.x,Knowledge_6.x,Knowledge_7.x,
                       Knowledge_8.x,Knowledge_9.x))
## 前測態度題
pre_test_a <- select(data3,c("電話末四碼",Attitude_1.x,Attitude_2.x,
                             Attitude_3.x,Attitude_4.x,Attitude_5.x,Attitude_6.x,
                             Attitude_7.x,Attitude_8.x,Attitude_9.x,Attitude_10.x,
                             Attitude_11.x))
## 前測行為題
pre_test_b <- select(data3,c("電話末四碼",Behavior_1.x,Behavior_2.x,Behavior_3.x,Behavior_4.x,
                       Behavior_5.x,Behavior_6.x,Behavior_7.x,Behavior_8.x,Behavior_9.x))

## 後測知識題
post_test_k <- select(data3,c("電話末四碼",Knowledge_1.y,Knowledge_2.y,Knowledge_3.y,
                             Knowledge_4.y,Knowledge_5.y,Knowledge_6.y,Knowledge_7.y,
                             Knowledge_8.y,Knowledge_9.y))
## 後測態度題
post_test_a <- select(data3,c("電話末四碼",Attitude_1.y,Attitude_2.y,
                             Attitude_3.y,Attitude_4.y,Attitude_5.y,Attitude_6.y,
                             Attitude_7.y,Attitude_8.y,Attitude_9.y,Attitude_10.y,
                             Attitude_11.y))
## 後測行為題
post_test_b <- select(data3,c("電話末四碼",Behavior_1.y,Behavior_2.y,Behavior_3.y,Behavior_4.y,
                             Behavior_5.y,Behavior_6.y,Behavior_7.y,Behavior_8.y,Behavior_9.y))

## Cronbach's alpha value
## standard solution
## pre-test knowledge
s1 <- apply(pre_test_k[,-1],1,sum)  # find the sum of each row
vs1 <- var(s1)                 # find the variance of the sum of each row
v1 <- apply(pre_test_k[,-1],2,var)   # find the variance of each column
sv1 <- sum(v1)                  # find the sum of the variance of each column
n = 9                       # the numbers of the quest.
cronbach <- (n/(n-1))*(1-(sv1/vs1)) 
cronbach
#### cronbbach's alpha = 0.09276475
alpha(pre_test_k[,-1]) # 0.099
alpha(pre_test_k[,-1],check.keys=TRUE) # 0.34

## pre-test Attitude
s2 <- apply(pre_test_a[,-1],1,sum)  # find the sum of each row
vs2 <- var(s2)                 # find the variance of the sum of each row
v2 <- apply(pre_test_a[,-1],2,var)   # find the variance of each column
sv2 <- sum(v2)                  # find the sum of the variance of each column
n = 11                       # the numbers of the quest.
cronbach <- (n/(n-1))*(1-(sv2/vs2)) 
cronbach
#### cronbbach's alpha = 0.3984569
alpha(pre_test_a[,-1]) #0.4
alpha(pre_test_a[,-1],check.keys=TRUE) #0.63
#install.packages("CTT")
library(CTT)
x2 <- itemAnalysis(pre_test_a[,-1])
x2$itemReport
## pre-test Behavior
s3 <- apply(pre_test_b[,-1],1,sum)  # find the sum of each row
vs3 <- var(s3)                 # find the variance of the sum of each row
v3 <- apply(pre_test_b[,-1],2,var)   # find the variance of each column
sv3 <- sum(v3)                  # find the sum of the variance of each column
n = 9                       # the numbers of the quest.
cronbach <- (n/(n-1))*(1-(sv3/vs3)) 
cronbach
#### cronbbach's alpha = 0.7211485
alpha(pre_test_b[,-1]) #0.72且沒有warning
alpha(pre_test_b[,-1],check.keys=TRUE) #0.72且沒有warning
####
x3 <- itemAnalysis(pre_test_b[,-1]) #0.74
x3$itemReport

## post-test knowledge
ps1 <- apply(post_test_k[,-1],1,sum)  # find the sum of each row
pvs1 <- var(ps1)                 # find the variance of the sum of each row
pv1 <- apply(post_test_k[,-1],2,var)   # find the variance of each column
psv1 <- sum(pv1)                  # find the sum of the variance of each column
n = 9                       # the numbers of the quest.
cronbach <- (n/(n-1))*(1-(psv1/pvs1)) 
cronbach
#### cronbbach's alpha = -0.05002365   *** negative
alpha(post_test_k[,-1]) # -0.053
alpha(post_test_k[,-1],check.keys=TRUE) #0.32

## post-test Attitude
ps2 <- apply(post_test_a[,-1],1,sum)  # find the sum of each row
pvs2 <- var(ps2)                 # find the variance of the sum of each row
pv2 <- apply(post_test_a[,-1],2,var)   # find the variance of each column
psv2 <- sum(pv2)                  # find the sum of the variance of each column
n = 11                       # the numbers of the quest.
cronbach <- (n/(n-1))*(1-(psv2/pvs2)) 
cronbach
#### cronbbach's alpha = 0.4705694
alpha(post_test_a[,-1]) #0.47
alpha(post_test_a[,-1],check.keys=TRUE) #0.63
###
y2 <- itemAnalysis(post_test_a[,-1]) 
y2$itemReport
## post-test Behavior
ps3 <- apply(post_test_b[,-1],1,sum)  # find the sum of each row
pvs3 <- var(ps3)                 # find the variance of the sum of each row
pv3 <- apply(post_test_b[,-1],2,var)   # find the variance of each column
psv3 <- sum(pv3)                  # find the sum of the variance of each column
n = 9                       # the numbers of the quest.
cronbach <- (n/(n-1))*(1-(psv3/pvs3)) 
cronbach
#### cronbbach's alpha = 0.7390038
alpha(post_test_b[,-1]) #0.74
alpha(post_test_b[,-1],check.keys=TRUE) #0.76
####
y3 <- itemAnalysis(post_test_b[,-1])
y3$itemReport



