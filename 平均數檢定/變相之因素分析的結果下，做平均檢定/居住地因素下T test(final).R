#getwd()
#setwd("C:/Users/USER/Desktop/統計實務")
data <- read.csv("C:/Users/映均/水資源資料/water_連續資料.csv", header=TRUE, fileEncoding = "UTF-8-BOM")
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

North_data <- subset(data,居住地=="北部地區") #54筆
Middle_data <- subset(data,居住地=="中部地區") #181筆
South_data <- subset(data,居住地=="南部地區") #61筆
Earth_data <- subset(data,居住地=="東部地區") #0筆
Offshore_data <- subset(data,居住地=="離島") #0筆

###### 檢查居住地下是否有相同平均數

### attitude 因素
## 對 data 居住地的interest_T interest2_T interest3_Tvalues_T values2_T values3_T做常態檢定
ks.test(North_data$interest_T, pnorm, mean(North_data$interest_T), sd(North_data$interest_T))   # p-value = 0.06083大於0.05，服從常態分配
ks.test(Middle_data$interest_T, pnorm, mean(Middle_data$interest_T), sd(Middle_data$interest_T))   # p-value = 0.004464小於0.05，不服從常態分配
ks.test(South_data$interest_T, pnorm, mean(South_data$interest_T), sd(South_data$interest_T))  # p-value = 0.009544小於0.05，不服從常態分配

ks.test(North_data$interest2_T, pnorm, mean(North_data$interest2_T), sd(North_data$interest2_T))   # p-value = 0.003852小於0.05，不服從常態分配
ks.test(Middle_data$interest2_T, pnorm, mean(Middle_data$interest2_T), sd(Middle_data$interest2_T))   # p-value = .474e-08小於0.05，不服從常態分配
ks.test(South_data$interest2_T, pnorm, mean(South_data$interest2_T), sd(South_data$interest2_T))   # p-value = 0.003818小於0.05，不服從常態分配

ks.test(North_data$interest3_T, pnorm, mean(North_data$interest3_T), sd(North_data$interest3_T))   # p-value =  0.03208小於0.05，不服從常態分配
ks.test(Middle_data$interest3_T, pnorm, mean(Middle_data$interest3_T), sd(Middle_data$interest3_T))   # p-value = 2.178e-08小於0.05，不服從常態分配
ks.test(South_data$interest3_T, pnorm, mean(South_data$interest3_T), sd(South_data$interest3_T))   # p-value = 0.0009635小於0.05，不服從常態分配

ks.test(North_data$values_T, pnorm, mean(North_data$values_T), sd(North_data$values_T))   # p-value = 0.002814小於0.05，不服從常態分配
ks.test(Middle_data$values_T, pnorm, mean(Middle_data$values_T), sd(Middle_data$values_T))   # p-value = 3.337e-08小於0.05，不服從常態分配
ks.test(South_data$values_T, pnorm, mean(South_data$values_T), sd(South_data$values_T))  # p-value = 0.002378小於0.05，不服從常態分配

ks.test(North_data$values2_T, pnorm, mean(North_data$values2_T), sd(North_data$values2_T))   # p-value = 0.09957大於0.05，服從常態分配
ks.test(Middle_data$values2_T, pnorm, mean(Middle_data$values2_T), sd(Middle_data$values2_T))   # p-value = 1.826e-05小於0.05，不服從常態分配
ks.test(South_data$values2_T, pnorm, mean(South_data$values2_T), sd(South_data$values2_T))   # p-value = 0.2217大於0.05，服從常態分配

ks.test(North_data$values3_T, pnorm, mean(North_data$values3_T), sd(North_data$values3_T))   # p-value =  3.966e-05小於0.05，不服從常態分配
ks.test(Middle_data$values3_T, pnorm, mean(Middle_data$values3_T), sd(Middle_data$values3_T))   # p-value = 1.608e-07小於0.05，不服從常態分配
ks.test(South_data$values3_T, pnorm, mean(South_data$values3_T), sd(South_data$values3_T))   # p-value = 0.0005965小於0.05，不服從常態分配

# interest_T
ni1 <- North_data$interest_T
mi1 <- Middle_data$interest_T
si1 <- South_data$interest_T
ni2 <- rep("ni", 54)
mi2 <- rep("mi", 181)
si2 <- rep("si", 61)
score_i <- c(ni1, mi1,si1)
region_i <- c(ni2, mi2,si2)
region_interest <- data.frame(score_i, region_i)
# 使用 Levene's Test
library(car)
leveneTest(score_i ~ region_i, data = region_interest)   # p-value =  0.105 不顯著，表示變異數同質性
# 多樣本非常態且變異同質性，使用的是Kruskal–Wallis檢定
kruskal.test(score_i ~ region_i, data = region_interest)   # p-value = 0.8371 不顯著，表示平均數相同

# interest2_T
ni21 <- North_data$interest2_T
mi21 <- Middle_data$interest2_T
si21 <- South_data$interest2_T
ni22 <- rep("ni2", 54)
mi22 <- rep("mi2", 181)
si22 <- rep("si2", 61)
score_i2 <- c(ni21, mi21,si21)
region_i2 <- c(ni22, mi22,si22)
region_interest2 <- data.frame(score_i2, region_i2)
# 使用 Levene's Test
library(car)
leveneTest(score_i2 ~ region_i2, data = region_interest2)   # p-value = 0.5334 不顯著，表示變異數同質性
# 多樣本非常態且變異同質性，使用的是Kruskal–Wallis檢定
kruskal.test(score_i2 ~ region_i2, data = region_interest2)   # p-value = 0.7746 不顯著，表示平均數相同

# interest_T
ni31 <- North_data$interest3_T
mi31 <- Middle_data$interest3_T
si31 <- South_data$interest3_T
ni32 <- rep("ni3", 54)
mi32 <- rep("mi3", 181)
si32 <- rep("si3", 61)
score_i3 <- c(ni31, mi31,si31)
region_i3 <- c(ni32, mi32,si32)
region_interest3 <- data.frame(score_i3, region_i3)
# 使用 Levene's Test
library(car)
leveneTest(score_i3 ~ region_i3, data = region_interest3)   # p-value = 0.6671 不顯著，表示變異數同質性
# 多樣本非常態且變異同質性，使用的是Kruskal–Wallis檢定
kruskal.test(score_i3 ~ region_i3, data = region_interest3)   # p-value = 0.03399 顯著，表示平均數不相同
# 若Kruskal–Wallis檢定有顯著差異，則繼續查看Dunn檢定的結果
### Dunn test for multiple comparisons
### Order groups by median
region_interest3$region_i3 = factor(region_interest3$region_i3, levels = c("ni3", "mi3", "si3"),
                                  ordered = T)

### Dunn test
library(FSA)
dunnTest(score_i3 ~ region_i3, data = region_interest3,
         method="bh")
# mi3 - sis 的P.adj=0.02095462(<0.05)，z=-2.3088011(<0)代表南部地區分數顯著比中部地區高

# values_T
nv1 <- North_data$values_T
mv1 <- Middle_data$values_T
sv1 <- South_data$values_T
nv2 <- rep("nv", 54)
mv2 <- rep("mv", 181)
sv2 <- rep("sv", 61)
score_v <- c(nv1, mv1,sv1)
region_v <- c(nv2, mv2,sv2)
region_values <- data.frame(score_v, region_v)
# 使用 Levene's Test
library(car)
leveneTest(score_v ~ region_v, data = region_values)   # p-value = 0.7886不顯著，表示變異數同質性
# 多樣本非常態且變異同質性，使用的是Kruskal–Wallis檢定
kruskal.test(score_v ~ region_v, data = region_values)   # p-value = 0.36 不顯著，表示平均數相同

# values2_T
nvs1 <- North_data$values2_T
mvs1 <- Middle_data$values2_T
svs1 <- South_data$values2_T
nvs2 <- rep("nvs", 54)
mvs2 <- rep("mvs", 181)
svs2 <- rep("svs", 61)
score_vs <- c(nvs1, mvs1,svs1)
region_vs <- c(nvs2, mvs2,svs2)
region_values2 <- data.frame(score_vs, region_vs)
# 使用 Levene's Test
library(car)
leveneTest(score_vs ~ region_vs, data = region_values2)   # p-value = 0.8967 不顯著，表示變異數同質性
# 多樣本非常態且變異同質性，使用的是Kruskal–Wallis檢定
kruskal.test(score_vs ~ region_vs, data = region_values2)   # p-value = 0.03458 顯著，表示平均數不相同
# 若Kruskal–Wallis檢定有顯著差異，則繼續查看Dunn檢定的結果
### Dunn test for multiple comparisons
### Order groups by median
region_values2$region_vs = factor(region_values2$region_vs, levels = c("nvs", "mvs", "svs"),
                                    ordered = T)
#View(region_values2)
### Dunn test
library(FSA)
dunnTest(score_vs ~ region_vs,
         data=region_values2,
         method="bh")
# mvs - svs 的P.adj=0.0326(<0.05)，z=-2.5461682(<0)代表南部地區分數顯著比中部地區高

# values3_T
nv31 <- North_data$values3_T
mv31 <- Middle_data$values3_T
sv31 <- South_data$values3_T
nv32 <- rep("nv3", 54)
mv32 <- rep("mv3", 181)
sv32 <- rep("sv3", 61)
score_v3 <- c(nv31, mv31,sv31)
region_v3 <- c(nv32, mv32,sv32)
region_values3 <- data.frame(score_v3, region_v3)
# 使用 Levene's Test
library(car)
leveneTest(score_v3 ~ region_v3, data = region_values3)   # p-value = 0.02295不顯著，表示變異數異質性
#多樣本非常態且變異同質性，使用的是Kruskal–Wallis檢定
kruskal.test(score_v3 ~ region_v3, data = region_values3)
# fit a linear model to the data

################
## conclusion ##
################
## 居住地在對循環系統關注程度的分數有差異
## 居住地在對質與量議題的關注程度的分數沒有差異
## 居住地在對相關議題永續利用的關注程度的分數沒有差異
## 居住地在對相關經濟議題的觀點的分數沒有差異
## 居住地在對循環系統的觀點的分數沒有差異
## 居住地在個人外控觀點的分數沒有差異

### behavior 因素
## 對 data 居住地的behavior_T behavior2_T attempt_T  attempt2_T 做常態檢定
ks.test(North_data$behavior_T, pnorm, mean(North_data$behavior_T), sd(North_data$behavior_T))   # p-value = 0.01221小於0.05，不服從常態分配
ks.test(Middle_data$behavior_T, pnorm, mean(Middle_data$behavior_T), sd(Middle_data$behavior_T))   # p-value = 3.145e-05小於0.05，不服從常態分配
ks.test(South_data$behavior_T, pnorm, mean(South_data$behavior_T), sd(South_data$behavior_T))  # p-value = 0.01391小於0.05，不服從常態分配

ks.test(North_data$behavior2_T, pnorm, mean(North_data$behavior2_T), sd(North_data$behavior2_T))   # p-value = 0.004079小於0.05，不服從常態分配
ks.test(Middle_data$behavior2_T, pnorm, mean(Middle_data$behavior2_T), sd(Middle_data$behavior2_T))   # p-value = 1.992e-06小於0.05，不服從常態分配
ks.test(South_data$behavior2_T, pnorm, mean(South_data$behavior2_T), sd(South_data$behavior2_T))  # p-value =  0.001146小於0.05，不服從常態分配

ks.test(North_data$attempt_T, pnorm, mean(North_data$attempt_T), sd(North_data$attempt_T))   # p-value = 0.4677大於0.05，服從常態分配
ks.test(Middle_data$attempt_T, pnorm, mean(Middle_data$attempt_T), sd(Middle_data$attempt_T))   # p-value = 0.002127小於0.05，不服從常態分配
ks.test(South_data$attempt_T, pnorm, mean(South_data$attempt_T), sd(South_data$attempt_T))  # p-value = 0.4027大於0.05，服從常態分配

ks.test(North_data$attempt2_T, pnorm, mean(North_data$attempt2_T), sd(North_data$attempt2_T))   # p-value = 0.004136小於0.05，不服從常態分配
ks.test(Middle_data$attempt2_T, pnorm, mean(Middle_data$attempt2_T), sd(Middle_data$attempt2_T))   # p-value = 2.317e-06小於0.05，不服從常態分配
ks.test(South_data$attempt2_T, pnorm, mean(South_data$attempt2_T), sd(South_data$attempt2_T))  # p-value = 0.002981小於0.05，不服從常態分配


# behavior_T
nb1 <- North_data$behavior_T
mb1 <- Middle_data$behavior_T
sb1 <- South_data$behavior_T
nb2 <- rep("nb", 54)
mb2 <- rep("mb", 181)
sb2 <- rep("sb", 61)
score_b <- c(nb1, mb1,sb1)
region_b <- c(nb2, mb2,sb2)
region_behavior <- data.frame(score_b, region_b)
# 使用 Levene's Test
library(car)
leveneTest(score_b ~ region_b, data = region_behavior)   # p-value = 0.4275 不顯著，表示變異數同質性
# 多樣本非常態且變異同質性，使用的是Kruskal–Wallis檢定
kruskal.test(score_b ~ region_b, data = region_behavior)   # p-value = 0.4729 不顯著，表示平均數相同

# behavior2_T
nb21 <- North_data$behavior2_T
mb21 <- Middle_data$behavior2_T
sb21 <- South_data$behavior2_T
nb22 <- rep("nb2", 54)
mb22 <- rep("mb2", 181)
sb22 <- rep("sb2", 61)
score_b2 <- c(nb21, mb21,sb21)
region_b2 <- c(nb22, mb22,sb22)
region_behavior2 <- data.frame(score_b2, region_b2)
# 使用 Levene's Test
library(car)
leveneTest(score_b2 ~ region_b2, data = region_behavior2)   # p-value = 0.6671 不顯著，表示變異數同質性
# 多樣本非常態且變異同質性，使用的是Kruskal–Wallis檢定
kruskal.test(score_b2 ~ region_b2, data = region_behavior2)   # p-value = 0.9464不 顯著，表示平均數相同

# attempt_T
na1 <- North_data$attempt_T
ma1 <- Middle_data$attempt_T
sa1 <- South_data$attempt_T
na2 <- rep("na", 54)
ma2 <- rep("ma", 181)
sa2 <- rep("sa", 61)
score_a <- c(na1, ma1,sa1)
region_a <- c(na2, ma2,sa2)
region_attempt <- data.frame(score_a, region_a)
# 使用 Levene's Test
library(car)
leveneTest(score_a ~ region_a, data = region_attempt)   # p-value =  0.2181 不顯著，表示變異數同質性
# 多樣本非常態且變異同質性，使用的是Kruskal–Wallis檢定
kruskal.test(score_a ~ region_a, data = region_attempt)   # p-value = 0.5063 不顯著，表示平均數相同

# attempt2_T
na21 <- North_data$attempt2_T
ma21 <- Middle_data$attempt2_T
sa21 <- South_data$attempt2_T
na22 <- rep("na2", 54)
ma22 <- rep("ma2", 181)
sa22 <- rep("sa2", 61)
score_a2 <- c(na21, ma21,sa21)
region_a2 <- c(na22, ma22,sa22)
region_attempt2 <- data.frame(score_a2, region_a2)
# 使用 Levene's Test
library(car)
leveneTest(score_a2 ~ region_a2, data = region_attempt2)   # p-value =  0.7323 不顯著，表示變異數同質性
# 多樣本非常態且變異同質性，使用的是Kruskal–Wallis檢定
kruskal.test(score_a2 ~ region_a2, data = region_attempt2)   # p-value = 0.3148 不顯著，表示平均數相同

################
## conclusion ##
################
## 居住地在能向他人傳遞水資源及相關公共建設議題知識的分數沒有差異
## 居住地在說服行動的表現的分數沒有差異
## 居住地在友善水資源之生活習慣的分數沒有差異
## 居住地在友善水資源的消費行動的分數沒有差異






