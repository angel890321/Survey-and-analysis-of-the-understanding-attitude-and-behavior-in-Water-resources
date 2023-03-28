getwd()
setwd("C:/Users/qu32k/Desktop/R/water")
water <- read.csv("C:/Users/qu32k/Desktop/R/water/water_類別資料.csv")
warning(water)
library(dplyr)
View(water)



water <- within(water,{請問您是否為大學生或研究生 <- recode(water$請問您是否為大學生或研究生,"是" = 1, "不是" = 0)})
water <- within(water,{性別 <- recode(water$性別,"男" = 1, "女" = 0)})           
water <- within(water,{出生地 <- recode(water$出生地,"北部地區" = 1, "中部地區" = 2,'南部地區' = 3,'東部地區'= 4,'其他'= 0)})           
water <- within(water,{居住地 <- recode(water$居住地,"北部地區" = 1, "中部地區" = 2,'南部地區' = 3,'東部地區'= 4,'其他'= 0)}) 
water <- within(water,{濾水裝置 <- recode(water$濾水裝置,"有濾水壺或Ro逆滲透、飲水機等等" = 1, "無" = 0)}) 
water <- within(water,{飲用水來源 <- recode(water$飲用水來源,"加水車" = 1, "加水站" = 2,'市售礦泉水' = 3,'地下水'= 4 ,'自來水'=5,'其他'=0)}) 
water <- within(water,{經歷水災 <- recode(water$經歷水災,"是" = 1, "否" = 0)}) 
water <- within(water,{經歷限水 <- recode(water$經歷限水,"是" = 1, "否" = 0)}) 
water <- within(water,{平均水價 <- recode(water$平均水價,"答對" = 1, "答錯" = 0)}) 
water <- within(water,{自來水普及率 <- recode(water$自來水普及率,"答對" = 1, "答錯" = 0)}) 
water <- within(water,{汙水下水道接管率 <- recode(water$汙水下水道接管率,"答對" = 1, "答錯" = 0)}) 
water <- within(water,{汙水來源 <- recode(water$汙水來源,"答對" = 1, "答錯" = 0)}) 
water <- within(water,{高用水地區 <- recode(water$高用水地區,"答對" = 1, "答錯" = 0)}) 
water <- within(water,{一度水幾公升 <- recode(water$一度水幾公升,"答對" = 1, "答錯" = 0)}) 
water <- within(water,{家庭用水比例最大 <- recode(water$家庭用水比例最大,"答對" = 1, "答錯" = 0)}) 
water <- within(water,{河川汙染最嚴重 <- recode(water$河川汙染最嚴重,"答對" = 1, "答錯" = 0)}) 
water <- within(water,{愛河水質 <- recode(water$愛河水質,"答對" = 1, "答錯" = 0)}) 
write.csv(water,file="water數值_1.csv",row.names = F)

water_num <- read.csv("C:/Users/qu32k/Desktop/R/water/water數值_1.csv")
View(water_num)
ggplot(water_num,aes(x=請問您是否為大學生或研究生,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=性別,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=出生地,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=居住地,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=濾水裝置,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=飲用水來源,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=經歷水災,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=經歷限水,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=平均水價,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=自來水普及率,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=汙水下水道接管率,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=汙水來源,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=高用水地區,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=一度水幾公升,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=家庭用水比例最大,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=河川汙染最嚴重,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=愛河水質,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=會留意瓶裝水水源,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=旱災意識,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=水危機訊息,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=認為經過高級處理的淨水使用在澆花.洗車等方面是浪費的,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=認為回收水再利用麻煩,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=韓國首爾清溪川引漢江水打造都市水岸是永續的作法,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=認為臺灣水資源受到威脅是我們無力改善的事實,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=認為民眾無法干涉政府的治水政策,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=認為臺灣缺水不會發生在自己身上,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=可以主動地告訴大家如何正確用水,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=會告訴大家怎麼做才能讓水的使用更永續,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=會告訴身邊的人平時盡量自備水壺,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=會關心使用清潔劑對水資源造成的污染,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=家中的自來水塔有定期清理,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=減少淋浴時間與次數,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=水回收再利用,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))
ggplot(water_num,aes(x=紙餐盒清洗後再回收,bins = 30,binwidth = 0.3))+
  geom_bar(aes(y=..prop..))

