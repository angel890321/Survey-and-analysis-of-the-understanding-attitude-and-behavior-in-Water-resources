library(dplyr)
data <- read.csv("C:/Users/映均/水資源資料/water_連續資料.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
#View(data)
man_data <- subset(data,性別=="男") #129筆
#View(man_data)
woman_data <- subset(data,性別=="女") #171筆
#View(woman_data)
mean(man_data$Knowledge_score)
mean(woman_data$Knowledge_score)
var(man_data$Knowledge_score)
var(woman_data$Knowledge_score)
### 性別與知識題向總分
# 檢查是否為常態檢定(因為樣本數大於50，所以使用Kolmogorov-Smirnov (K-S)常態檢定)
ks.test(man_data$Knowledge_score, pnorm, mean(man_data$Knowledge_score), sd(man_data$Knowledge_score)) 
#p-value = 0.000119小於0.05，不服從常態分配
ks.test(woman_data$Knowledge_score, pnorm, mean(woman_data$Knowledge_score), sd(woman_data$Knowledge_score))
#p-value =  4.826e-05小於0.05，不服從常態分配

# 檢查兩組資料是否為相同變異數
mk1 <- man_data$Knowledge_score
wk1 <- woman_data$Knowledge_score
mk2 <- rep("mk", 129)
wk2 <- rep("wk", 171)
score_k <- c(mk1, wk1)
sex_k <- c(mk2, wk2)
sex_knowledge <- data.frame(score_k, sex_k)
View(sex_knowledge)
# 使用 Levene's Test
library(car)
leveneTest(score_k ~ sex_k, data = sex_knowledge)
# 在Levene檢定中，Pr(>F)的數值0.01798小於顯著水準0.05，表示兩組資料為異質性
wilcox.test(score_k ~ sex_k, data = sex_knowledge, var.equal = FALSE)
# 在wilcox檢定中，Pr(>F)的數值0.03141小於顯著水準0.05，表示兩組資料平均不相等
# 程式碼參考連結:https://blog.pulipuli.info/2018/01/rkruskalwalliswelchs-anova-non.html

mean(man_data$Attitude_score)
mean(woman_data$Attitude_score)
var(man_data$Attitude_score)
var(woman_data$Attitude_score)
### 性別與態度題向總分
ks.test(man_data$Attitude_score, pnorm, mean(man_data$Attitude_score), sd(man_data$Attitude_score)) 
# p-value = 0.3674大於0.05，服從常態分配
ks.test(woman_data$Attitude_score, pnorm, mean(woman_data$Attitude_score), sd(woman_data$Attitude_score))
# p-value = 0.2782大於0.05，服從常態分配

# 檢查兩組資料是否為相同變異數
mA1 <- man_data$Attitude_score
wA1 <- woman_data$Attitude_score
mA2 <- rep("mA", 129)
wA2 <- rep("wA", 171)
score_A <- c(mA1, wA1)
sex_A <- c(mA2, wA2)
sex_Attitude <- data.frame(score_A, sex_A)
View(sex_Attitude)
# 使用 Levene's Test(因為都服從常態分配，所以要加center=mean)
leveneTest(score_A ~ sex_A, data = sex_Attitude,center=mean)
# 在Levene檢定中，Pr(>F)的數值0.06776大於顯著水準0.05，表示兩組資料為同質性
# 因此接下來將使用t-test進行分析
t.test(man_data$Attitude_score,woman_data$Attitude_score, var.equal = TRUE)
# 在t檢定中，p-value =0.8612大於顯著水準0.05，表示兩組資料平均相等

mean(man_data$Behavior_score)
mean(woman_data$Behavior_score)
var(man_data$Behavior_score)
var(woman_data$Behavior_score)
### 性別與行為題向總分
ks.test(man_data$Behavior_score, pnorm, mean(man_data$Behavior_score), sd(man_data$Behavior_score)) # p-value = 0.2032大於0.05，服從常態分配
ks.test(woman_data$Behavior_score, pnorm, mean(man_data$Behavior_score), sd(man_data$Behavior_score))# p-value = 0.02963小於0.05，不服從常態分配
# 檢查兩組資料是否為相同變異數
mB1 <- man_data$Behavior_score
wB1 <- woman_data$Behavior_score
mB2 <- rep("mB", 129)
wB2 <- rep("wB", 171)
score_B <- c(mB1, wB1)
sex_B <- c(mB2, wB2)
sex_Behavior <- data.frame(score_B, sex_B)
View(sex_Behavior)
# 使用 Levene's Test
leveneTest(score_B ~ sex_B, data = sex_Behavior)
# 在Levene檢定中，Pr(>F)的數值0.04748小於顯著水準0.05，表示兩組資料為異質性
wilcox.test(score_B ~ sex_B, data = sex_Behavior, var.equal = FALSE)
# 在Welch's anova檢定中，Pr(>F)的數值0.6869大於顯著水準0.05，表示兩組資料平均相等

### 結論
# 兩個性別下的知識題向總分的應該平均數不相等
# 兩個性別下的態度題向總分的應該平均數相等
# 兩個性別下的行為題向總分的應該平均數相等

#  t test
### 性別與知識題向總分
var.test(man_data$Knowledge_score,woman_data$Knowledge_score)
# p-value = 0.003826 代表有顯著證據顯示兩個變異數不同
#因此使用變異數不同的的獨立雙樣本 t 檢定
t.test(man_data$Knowledge_score,woman_data$Knowledge_score, var.equal = FALSE)
# p-value = 0.01176代表有顯著證據顯示不同性別下的知識題平均分數不相同

### 性別與態度題向總分
var.test(man_data$Attitude_score,woman_data$Attitude_score)
# p-value = 0.2105代表沒有顯著證據顯示兩個變異數不同
t.test(man_data$Attitude_score,woman_data$Attitude_score, var.equal = TRUE)
# p-value = 0.8612代表沒有顯著證據顯示不同性別下的態度題平均分數不相同

### 性別與行為題向總分
var.test(man_data$Behavior_score,woman_data$Behavior_score)
# p-value = 0.0051代表有顯著證據顯示兩個變異數不同
t.test(man_data$Behavior_score,woman_data$Behavior_score, var.equal = FALSE)
# p-value = 0.6869代表沒有顯著證據顯示不同性別下的行為題平均分數不相同

### 結論(與無母數檢定結果相同)
# 兩個性別下的知識題向總分的應該平均數不相等
# 兩個性別下的態度題向總分的應該平均數相等
# 兩個性別下的行為題向總分的應該平均數相等