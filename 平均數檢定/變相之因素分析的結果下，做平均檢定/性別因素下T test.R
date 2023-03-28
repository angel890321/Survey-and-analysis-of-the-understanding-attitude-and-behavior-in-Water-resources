#getwd()
#setwd("C:/Users/USER/Desktop/統計實務")
data <- read.csv("C:/Users/映均/水資源資料/water_連續資料.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
#View(data)
data <- within(data, {
  interest_T <- apply(data[,c("Attitude_1","Attitude_2")], 1, sum) 
  interest2_T <- data[,c("Attitude_3")]
  interest3_T <- data[,c("Attitude_4")]
  values_T <- data[,c("Attitude_5")]
  values2_T <- apply(data[,c("Attitude_6","Attitude_7")], 1, sum) 
  values3_T <- apply(data[,c("Attitude_8","Attitude_9","Attitude_10")], 1, sum) 
})

data <- within(data, {
  behavior_T <- apply(data[,c("Behavior_1","Behavior_2")], 1, sum) 
  behavior2_T <- data[,c("Behavior_3")] 
  attempt_T <- apply(data[,c("Behavior_4","Behavior_5","Behavior_6")], 1, sum)  
  attempt2_T <- data[,c("Behavior_7")]
})
#write.csv(data, file = "./water_連續資料(因素總和).csv")
man_data <- subset(data,性別=="男") # 129筆
View(man_data)
woman_data <- subset(data,性別=="女") # 171筆


###### 檢查兩組資料是否為相同平均數

### attitude 因素
## 對 data 男女的interest_T interest2_T interest3_Tvalues_T values2_T values3_T做常態檢定
ks.test(man_data$interest_T, pnorm, mean(man_data$interest_T), sd(man_data$interest_T))   # p-value = 0.003802小於0.05，不服從常態分配
ks.test(man_data$interest2_T, pnorm, mean(man_data$interest2_T), sd(man_data$interest2_T))   # p-value = 4.405e-06小於0.05，不服從常態分配
ks.test(man_data$interest3_T, pnorm, mean(man_data$interest3_T), sd(man_data$interest3_T))    # 8.481e-07小於0.05，不服從常態分配
ks.test(man_data$values_T, pnorm, mean(man_data$values_T), sd(man_data$values_T))    # 5.242e-06小於0.05，不服從常態分配
ks.test(man_data$values2_T, pnorm, mean(man_data$values2_T), sd(man_data$values2_T))   # p-value = 0.0008692小於0.05，不服從常態分配
ks.test(man_data$values3_T, pnorm, mean(man_data$values3_T), sd(man_data$values3_T))    # p-value =6.02e-06小於0.05，不服從常態分配

ks.test(woman_data$interest_T, pnorm, mean(woman_data$interest_T), sd(woman_data$interest_T))   # p-value = 0.00241小於0.05，不服從常態分配
ks.test(woman_data$interest2_T, pnorm, mean(woman_data$interest2_T), sd(woman_data$interest2_T))   # p-value = 1.518e-08小於0.05，不服從常態分配
ks.test(woman_data$interest3_T, pnorm, mean(woman_data$interest3_T), sd(woman_data$interest3_T))    # p-value = 5.063e-06小於0.05，不服從常態分配
ks.test(woman_data$values_T, pnorm, mean(woman_data$values_T), sd(woman_data$values_T))    # p-value = 1.662e-07小於0.05，不服從常態分配
ks.test(woman_data$values2_T, pnorm, mean(woman_data$values2_T), sd(woman_data$values2_T))   # p-value = 0.0004287小於0.05，不服從常態分配
ks.test(woman_data$values3_T, pnorm, mean(woman_data$values3_T), sd(woman_data$values3_T))    # p-value =5.617e-09小於0.05，不服從常態分配

# interest_T
mi1 <- man_data$interest_T
wi1 <- woman_data$interest_T
mi2 <- rep("mi", 129)
wi2 <- rep("wi", 171)
score_i <- c(mi1, wi1)
sex_i <- c(mi2, wi2)
sex_interest <- data.frame(score_i, sex_i)
# 使用 Levene's Test
library(car)
leveneTest(score_i ~ sex_i, data = sex_interest)   # p-value = 0.4112 不顯著，表示變異數同質性
# 因為同質性，所以用wilcox
wilcox.test(score_i ~ sex_i,data = sex_interest, var.equal = TRUE)# p-value = 0.1097 不顯著，表示兩群平均數相等

# interest2_T
mi21 <- man_data$interest2_T
wi21 <- woman_data$interest2_T
mi22 <- rep("mi21", 129)
wi22 <- rep("wi21", 171)
score_i2 <- c(mi21, wi21)
sex_i2 <- c(mi22, wi22)
sex_interest2 <- data.frame(score_i2, sex_i2)
# 使用 Levene's Test
library(car)
leveneTest(score_i2 ~ sex_i2, data = sex_interest2)   # p-value = 0.7073 不顯著，表示變異數同質性
# 因為同質性，所以用wilcox
wilcox.test(mi21,wi21)    # p-value = 0.8131 不顯著，表示兩群平均數相等

# interest3_T
mi31 <- man_data$interest3_T
wi31 <- woman_data$interest3_T
mi32 <- rep("mi31", 129)
wi32 <- rep("wi31", 171)
score_i3 <- c(mi31, wi31)
sex_i3 <- c(mi32, wi32)
sex_interest3 <- data.frame(score_i3, sex_i3)
# 使用 Levene's Test
library(car)
leveneTest(score_i3 ~ sex_i3, data = sex_interest3)   # p-value = 0.9771 不顯著，表示變異數同質性
# 因為同質性，所以用wilcox
wilcox.test(mi31,wi31)    # p-value = 0.8131 不顯著，表示兩群平均數相等


# values_T
mv1 <- man_data$values_T
wv1 <- woman_data$values_T
mv2 <- rep("mv", 129)
wv2 <- rep("wv", 171)
score_v <- c(mv1, wv1)
sex_v <- c(mv2, wv2)
sex_values <- data.frame(score_v, sex_v)

leveneTest(score_v ~ sex_v, data = sex_values)  # p-value = 0.6061 不顯著，表示變異數同質性
# 因為同質性，所以用wilcox
wilcox.test(mv1,wv1)    # p-value = 0.6417 不顯著，表示兩群平均數相等


# values2_T
mvs1 <- man_data$values2_T
wvs1 <- woman_data$values2_T
mvs2 <- rep("mvs", 129)
wvs2 <- rep("wvs", 171)
score_vs <- c(mvs1, wvs1)
sex_vs <- c(mvs2, wvs2)
sex_values2 <- data.frame(score_vs, sex_vs)
leveneTest(score_vs ~ sex_vs, data = sex_values2)  # p-value = 0.5711 不顯著，表示變異數同質性
# 因為同質性，所以用wilcox
wilcox.test(mvs1,wvs1)  # p-value = 0.123 不顯著，表示兩群平均數相等

# values3_T
mvs31 <- man_data$values3_T
wvs31 <- woman_data$values3_T
mvs32 <- rep("mvs3", 129)
wvs32 <- rep("wvs3", 171)
score_vs3 <- c(mvs31, wvs31)
sex_vs3 <- c(mvs32, wvs32)
sex_values32 <- data.frame(score_vs3, sex_vs3)
leveneTest(score_vs3 ~ sex_vs3, data = sex_values32)  # 0.7446不顯著，表示變異數同質性
# 因為同質性，所以用wilcox
wilcox.test(mvs31, wvs31)  # p-value = 0.4937 不顯著，表示兩群平均數相等
wilcox.test(score_vs3 ~ sex_vs3, data = sex_values32,var.equal = TRUE)




################
## conclusion ##
################
## 性別在對循環系統關注程度的分數有差異
## 性別在對質與量議題的關注程度的分數沒有差異
## 性別在對相關議題永續利用的關注程度的分數沒有差異
## 性別在對相關經濟議題的觀點的分數沒有差異
## 性別在對循環系統的觀點的分數沒有差異
## 性別在個人外控觀點的分數沒有差異

### behavior 因素
## 對 data 男女的behavior_T behavior2_T attempt_T  attempt2_T 做常態檢定
ks.test(man_data$behavior_T, pnorm, mean(man_data$behavior_T), sd(man_data$behavior_T))   # p-value = 1.146e-05小於0.05，不服從常態分配
ks.test(man_data$behavior2_T, pnorm, mean(man_data$behavior2_T), sd(man_data$behavior2_T))   # p-value = 0.0002721小於0.05，不服從常態分配
ks.test(man_data$attempt_T, pnorm, mean(man_data$attempt_T), sd(man_data$attempt_T))    # p-value =  0.02024小於0.05，服從常態分配
ks.test(man_data$attempt2_T, pnorm, mean(man_data$attempt2_T), sd(man_data$attempt2_T))   # p-value = 0.001991小於0.05，不服從常態分配

ks.test(woman_data$behavior_T, pnorm, mean(woman_data$behavior_T), sd(woman_data$behavior_T))   # p-value = 1.734e-05小於0.05，不服從常態分配
ks.test(woman_data$behavior2_T, pnorm, mean(woman_data$behavior2_T), sd(woman_data$behavior2_T))   # p-value = 1.854e-09小於0.05，不服從常態分配
ks.test(woman_data$attempt_T, pnorm, mean(woman_data$attempt_T), sd(woman_data$attempt_T))    # p-value = 0.02685小於0.05，服從常態分配
ks.test(woman_data$attempt2_T, pnorm, mean(woman_data$attempt2_T), sd(woman_data$attempt2_T))   # p-value = 1.275e-07小於0.05，不服從常態分配

# behavior_T
mb1 <- man_data$behavior_T
wb1 <- woman_data$behavior_T
mb2 <- rep("mb", 129)
wb2 <- rep("wb", 171)
score_b <- c(mb1, wb1)
sex_b <- c(mb2, wb2)
sex_behavior <- data.frame(score_b, sex_b)
leveneTest(score_b ~ sex_b, data = sex_behavior)    # p-value = 0.6691不顯著，表示變異數同質性
# 因為同質性，所以用wilcox
wilcox.test(mb1,wb1)    # p-value = 0.3431不顯著，表示兩群平均數相等
# behavior2_T
mb21 <- man_data$behavior2_T
wb21 <- woman_data$behavior2_T
mb22 <- rep("mb2", 129)
wb22 <- rep("wb2", 171)
score_b2 <- c(mb21, wb21)
sex_b2 <- c(mb22, wb22)
sex_behavior2 <- data.frame(score_b2, sex_b2)
leveneTest(score_b2 ~ sex_b2, data = sex_behavior2)    # p-value = 0.1165不顯著，表示變異數同質性
# 因為同質性，所以用wilcox
wilcox.test(mb21,wb21)    # p-value = 0.07271不顯著，表示兩群平均數相等

# attempt_T
ma1 <- man_data$attempt_T
wa1 <- woman_data$attempt_T
ma2 <- rep("ma", 129)
wa2 <- rep("wa", 171)
score_a <- c(ma1, wa1)
sex_a <- c(ma2, wa2)
sex_attempt <- data.frame(score_a, sex_a)
leveneTest(score_a ~ sex_a, data = sex_attempt)    # p-value = 0.2423不顯著，表示變異數同質性
# 因為異質性，所以用welch's test
wilcox.test(score_a ~ sex_a, data = sex_attempt, var.equal = TRUE)  # p-value = 0.6295 0.1151，表示兩群平均數相等

# attempt2_T
ma21 <- man_data$attempt2_T
wa21 <- woman_data$attempt2_T
ma22 <- rep("ma2", 129)
wa22 <- rep("wa2", 171)
score_a2 <- c(ma21, wa21)
sex_a2 <- c(ma22, wa22)
sex_attempt2 <- data.frame(score_a2, sex_a2)
leveneTest(score_a2 ~ sex_a2, data = sex_attempt2)    # p-value = 0.05106 不顯著，表示變異數同質性
# 因為同質性，所以用wilcox
wilcox.test(ma21,wa21) # p-value = 0.1064 不顯著，表示兩群平均數相等

################
## conclusion ##
################
## 性別在能向他人傳遞水資源及相關公共建設議題知識的分數沒有差異
## 性別在說服行動的表現的分數沒有差異
## 性別在友善水資源之生活習慣的分數沒有差異
## 性別在友善水資源的消費行動的分數沒有差異
