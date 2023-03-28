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
#write.csv(data3, file = "前後測統整.csv")

data3 <- data3[complete.cases(data3),]   # delete NA
data3 <- data3[!duplicated(data3[,1]),]  # select the unique obs
data3
#write.csv(data3, file = "前後測統整(刪NA+重複值).csv")

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

## 將反向題目的選項做反向
re_pre_test_a <- within(pre_test_a,{
  Attitude_7.x <- recode(pre_test_a$Attitude_7.x, "1" = 5, "2" = 4 , "3" = 3, "4"= 2, "5" = 1)
  Attitude_9.x <- recode(pre_test_a$Attitude_9.x, "1" = 5, "2" = 4 , "3" = 3, "4"= 2, "5" = 1)
  Attitude_10.x <- recode(pre_test_a$Attitude_10.x, "1" = 5, "2" = 4 , "3" = 3, "4"= 2, "5" = 1)
  Attitude_11.x <- recode(pre_test_a$Attitude_11.x, "1" = 5, "2" = 4 , "3" = 3, "4"= 2, "5" = 1)
})

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
                              Behavior_5.y,Behavior_6.y,Behavior_7.y,Behavior_8.y,Behavior_9.y,))

library(psych)
library(CTT)

## pre-test Attitude
alpha(pre_test_a[,-1])   ## 0.4
alpha(pre_test_a[,-1],check.keys=TRUE)   ## 0.63
re_pre_test_a <- within(pre_test_a,{
  Attitude_7.x <- recode(pre_test_a$Attitude_7.x, "1" = 5, "2" = 4 , "3" = 3, "4"= 2, "5" = 1)
  Attitude_9.x <- recode(pre_test_a$Attitude_9.x, "1" = 5, "2" = 4 , "3" = 3, "4"= 2, "5" = 1)
  Attitude_10.x <- recode(pre_test_a$Attitude_10.x, "1" = 5, "2" = 4 , "3" = 3, "4"= 2, "5" = 1)
  Attitude_11.x <- recode(pre_test_a$Attitude_11.x, "1" = 5, "2" = 4 , "3" = 3, "4"= 2, "5" = 1)
})
alpha(re_pre_test_a[,c(-1,-2,-7)]) ## 0.66
alpha(re_pre_test_a[,c(-1,-2,-7)],check.keys = T) ## 0.66

## pre-test Behavior
alpha(pre_test_b[,c(-1,-10)])  ## 0.76
alpha(pre_test_b[,c(-1,-10)],check.keys=TRUE)  ## 0.76

## post-test Attitude
alpha(post_test_a[,-1])  ## 0.47
alpha(post_test_a[,-1],check.keys=TRUE)  ## 0.63


## post-test Behavior
alpha(post_test_b[,c(-1,-10,-9,-6,-8)])  ## 0.88(還是一直有比0.88大的題項出現)
alpha(post_test_b[,-1],check.keys=TRUE) ## 0.76(有說第九題為反向題，但我覺得還好，所以不動)


