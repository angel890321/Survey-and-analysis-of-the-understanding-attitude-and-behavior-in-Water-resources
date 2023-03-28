library(dplyr)
data <- read.csv("C:/Users/映均/水資源資料/water_連續資料.csv",header=TRUE, fileEncoding = "UTF-8-BOM")
#View(data)
North_data <- subset(data,居住地=="北部地區") #54筆
#View(North_data)
Middle_data <- subset(data,居住地=="中部地區") #181筆
#View(Middle_data)
South_data <- subset(data,居住地=="南部地區") #61筆
#View(South_data)
Earth_data <- subset(data,居住地=="東部地區") #0筆
#View(Earth_data)
Offshore_data <- subset(data,居住地=="離島") #0筆
#View(Offshore_data)


### 居住地與知識題向總分
mean(North_data$Knowledge_score)
mean(Middle_data$Knowledge_score)
mean(South_data$Knowledge_score)
var(North_data$Knowledge_score)
var(Middle_data$Knowledge_score)
var(South_data$Knowledge_score)
# 檢查是否為常態檢定
ks.test(North_data$Knowledge_score, pnorm, mean(North_data$Knowledge_score), sd(North_data$Knowledge_score)) 
#p-value = 0.06675大於0.05，服從常態分配
ks.test(Middle_data$Knowledge_score, pnorm, mean(Middle_data$Knowledge_score), sd(Middle_data$Knowledge_score))
#p-value = 2.426e-07，不服從常態分配
ks.test(South_data$Knowledge_score, pnorm, mean(South_data$Knowledge_score), sd(South_data$Knowledge_score))
#p-value = 0.02422，不服從常態分配

# 檢查三組資料是否為相同變異數
nk1 <- North_data$Knowledge_score
mk1 <- Middle_data$Knowledge_score
sk1 <- South_data$Knowledge_score
nk2 <- rep("nk", 54)
mk2 <- rep("mk", 181)
sk2 <- rep("sk", 61)
score_k <- c(nk1, mk1,sk1)
region_k <- c(nk2, mk2,sk2)
region_knowledge <- data.frame(score_k, region_k)
View(region_knowledge)
# 使用 Levene's Test
library(car)
leveneTest(score_k ~ region_k, data = region_knowledge)
# 在Levene檢定中，Pr(>F)的數值0.4915大於顯著水準0.05，表示三組資料為同質性
#非常態且變異同質性，使用的是Kruskal–Wallis檢定
kruskal.test(score_k ~ region_k, data = region_knowledge)
# Kruskal–Wallis檢定中，p-value = 0.3246大於顯著水準0.05，表示三組資料知識分數平均相等

### 居住地與態度題向總分
mean(North_data$Attitude_score)
mean(Middle_data$Attitude_score)
mean(South_data$Attitude_score)
var(North_data$Attitude_score)
var(Middle_data$Attitude_score)
var(South_data$Attitude_score)
# 檢查是否為常態檢定
ks.test(North_data$Attitude_score, pnorm, mean(North_data$Attitude_score), sd(North_data$Attitude_score)) 
#p-value = 0.6831大於0.05，服從常態分配
ks.test(Middle_data$Attitude_score, pnorm, mean(Middle_data$Attitude_score), sd(Middle_data$Attitude_score))
#p-value = 0.2517，服從常態分配
ks.test(South_data$Attitude_score, pnorm, mean(South_data$Attitude_score), sd(South_data$Attitude_score))
#p-value = 0.6168，服從常態分配

# 檢查三組資料是否為相同變異數
nA1 <- North_data$Attitude_score
mA1 <- Middle_data$Attitude_score
sA1 <- South_data$Attitude_score
nA2 <- rep("nA", 54)
mA2 <- rep("mA", 181)
sA2 <- rep("sA", 61)
score_A <- c(nA1, mA1,sA1)
region_A <- c(nA2, mA2,sA2)
region_Attitude <- data.frame(score_A, region_A)
View(region_Attitude)
# 使用 Levene's Test(center=mean)
leveneTest(score_A ~ region_A, data = region_Attitude,center=mean)
# 在Levene檢定中，Pr(>F)的數值0.6528大於顯著水準0.05，表示三組資料為同質性
#常態且變異同質性，使用的是ANOVA檢定(參考資料:https://blog.pulipuli.info/2018/01/ranovawelchs-anova-non-parametric-tests.html)
# fit a linear model to the data
model <- lm(score_A ~ region_A, data = region_Attitude)
#run the ANOVA on the model
anova(model)
# anova檢定中，p-value = 0.5432大於顯著水準0.05，表示三組資料態度分數平均相等

### 居住地與行為題向總分
mean(North_data$Behavior_score)
mean(Middle_data$Behavior_score)
mean(South_data$Behavior_score)
var(North_data$Behavior_score)
var(Middle_data$Behavior_score)
var(South_data$Behavior_score)
# 檢查是否為常態檢定
ks.test(North_data$Behavior_score,pnorm, mean(North_data$Behavior_score), sd(North_data$Behavior_score)) 
#p-value = 0.5432大於0.05，服從常態分配
ks.test(Middle_data$Behavior_score,pnorm, mean(Middle_data$Behavior_score), sd(Middle_data$Behavior_score))
#p-value = 0.242，服從常態分配
ks.test(South_data$Behavior_score,pnorm, mean(South_data$Behavior_score), sd(South_data$Behavior_score))
#p-value = 0.7133，服從常態分配

# 檢查三組資料是否為相同變異數
nB1 <- North_data$Behavior_score
mB1 <- Middle_data$Behavior_score
sB1 <- South_data$Behavior_score
nB2 <- rep("nB", 54)
mB2 <- rep("mB", 181)
sB2 <- rep("sB", 61)
score_B <- c(nB1, mB1,sB1)
region_B <- c(nB2, mB2,sB2)
region_Behavior <- data.frame(score_B, region_B)
View(region_Behavior)
# 使用 Levene's Test
leveneTest(score_B ~ region_B, data = region_Behavior)
# 在Levene檢定中，Pr(>F)的數值0.3717大於顯著水準0.05，表示三組資料為同質性
# 常態且變異同質性，使用的是ANOVA檢定
model <- lm(score_B ~ region_B, data = region_Behavior)
#run the ANOVA on the model
anova(model)
# anova檢定中，p-value = 0.3071大於顯著水準0.05，表示三組資料行為分數平均相等



# 結論
# 三組資料知識分數平均相等
# 三組資料態度分數平均相等
# 三組資料行為分數平均相等