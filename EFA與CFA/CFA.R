getwd()
setwd("C:/Users/USER/Desktop/統計實務")
data <- read.csv("./water_連續資料.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
#View(data)

fa1 <- subset(data, select = c(Attitude_1:Attitude_9,Attitude_score))
fa2 <- subset(data, select = c(Behavior_1:Behavior_8,Behavior_score))
anyNA(fa1)
anyNA(fa2)
# make sure there are no missing values
# or the correlation matrix can't be calculted


library(tidySEM); library(lavaan)

## 態度的CFA (第1題和第6題是刪掉的，所以attitude1 為第二題以此類推)
HS.model1 <- ' interest  =~ Attitude_1 + Attitude_2 + Attitude_3      
              concept =~ Attitude_4 + Attitude_5 + Attitude_6
              values   =~ Attitude_7 + Attitude_8 + Attitude_9 '
HS.cfa1 <- cfa(HS.model1, data = fa1,
              estimator = "WLSMV")      ## 跑出了lavaan WARNING: some estimated lv variances are negative 
summary(HS.cfa1)   
lay1 <- get_layout("", "", "", "", "", "","","", "",
                   "", "concept", "", "", "values", "", "", "interest", "",
                   "", "", "", "", "", "","","", "",
                   "Attitude_4", "Attitude_5", "Attitude_6", "Attitude_7", "Attitude_8", "Attitude_9", "Attitude_1", "Attitude_2","Attitude_3", 
                   rows = 4)
graph_sem(HS.cfa1, layout = lay1)
## 模型有問題，都不顯著
## concept =~ Attitude_4 + Attitude_5 + Attitude_6

## 行為的CFA (第9題是刪掉的)
HS.model2 <- ' behavior =~ Behavior_1 + Behavior_2 + Behavior_3 + Behavior_4      
              attempt =~  Behavior_5 + Behavior_6 + Behavior_7 + Behavior_8'
HS.cfa2 <- cfa(HS.model2, data = fa2,
              estimator = "WLSMV")   
summary(HS.cfa2)   
graph_sem(HS.cfa2)
lay2 <- get_layout("", "attempt", "", "", "", "", "behavior", "",
                   "", "", "", "", "", "","", "",
                   "Behavior_5", "Behavior_6", "Behavior_7", "Behavior_8", "Behavior_1", "Behavior_2","Behavior_3","Behavior_4",  
                   rows = 3)
graph_sem(HS.cfa2, layout = lay2)
## 模型沒問題，全部都顯著
