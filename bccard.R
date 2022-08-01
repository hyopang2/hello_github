#데이터 불러오기 및 데이터 확인

bccard_data <- read.csv("bccard.csv", encoding="EUC-KR",
                        fileEncoding="UTF-8")
str(bccard_data)
colnames(bccard_data) <- c("city","gubun","category","sex",
                           "age","bill","count")
View(bccard_data)
head(bccard_data)

#데이터 분석 
library(dplyr)
library(ggplot2)

# 시별 제주 내국인관광객 카드 사용금액 평균
city_bill <- bccard_data %>% 
  group_by(city) %>% 
  summarise(mean_bill=round(mean(bill),-1))

city_bill

ggplot(city_bill,aes(x=city,y=mean_bill,fill=city))+
  geom_col(width=0.7)+
  labs(x="시",y="카드사용금액평균",title="시별 제주 내국인 관광객 카드사용금액 평균")+
  theme_classic()+geom_text(aes(label=mean_bill,vjust=-1))

#성별 카드 사용금액 평균
sex_bill <- bccard_data %>% 
  group_by(sex) %>% 
  summarise(mean_bill=round(mean(bill),-1))

sex_bill

ggplot(sex_bill,aes(x=sex,y=mean_bill,fill=sex))+geom_col(width=0.7)+
  labs(x="성별",y="카드사용금액평균",title="성별 제주 내국인 관광객 카드사용금액 평균")+
  theme_classic()+geom_text(aes(label=mean_bill,vjust=-1))+
  scale_fill_manual(values=c("#66CDE6","#E2649F" ))


#연령별 카드 사용금액 평균
age_bill <- bccard_data %>% 
  group_by(age) %>% 
  summarise(mean_bill=round(mean(bill),-1))

age_bill

ggplot(age_bill,aes(x=age,y=mean_bill,fill=age))+geom_col(width=0.7)+
  labs(x="연령대",y="카드사용금액평균",title="연령대별 제주 내국인관광객 카드사용금액 평균")+
  theme_classic()+geom_text(aes(label=mean_bill,vjust=-1))


#연령대 및 성별 카드 사용금액 차이
age_sex_bill <- bccard_data %>% 
  group_by(age,sex) %>% 
  summarise(mean_bill=round(mean(bill),-1))
  
age_sex_bill

ggplot(age_sex_bill,aes(x=age,y=mean_bill,fill=sex))+geom_col(width=0.7)+
  labs(x="연령대",y="카드사용금액평균",title="연령대 및 성별 제주 내국인관광객 카드사용금액 평균")+
  theme_classic()+scale_fill_manual(values=c("#66CDE6","#E2649F" ))

#업종별 카드결제금액 차이
category_bill <- bccard_data %>% 
  group_by(category) %>% 
  summarise(mean_bill=round(mean(bill),-1)) %>% 
  arrange(desc(mean_bill)) %>% 
  head(10)

category_bill

ggplot(category_bill,aes(x=reorder(category,-mean_bill),y=mean_bill,fill=category))+
  geom_col(width=0.7)+
  labs(x="업종",y="카드사용금액평균",title="업종별 제주 내국인 관광객 카드사용금액 평균")+
  theme_classic()+geom_text(aes(label=mean_bill,vjust=-1))

#업종별 성별 카드 사용금액 차이
category_sex_bill <- bccard_data %>% 
  group_by(category,sex) %>% 
  summarise(mean_bill=round(mean(bill),-1))

ggplot(category_sex_bill,aes(x=reorder(category,-mean_bill),y=mean_bill,fill=sex))+geom_col(width=0.7)+
  labs(x="업종",y="카드사용금액평균",title="업종 및 성별 제주 내국인관광객 카드사용금액 평균")+
  theme_classic()+scale_fill_manual(values=c("#66CDE6","#E2649F" ))

#업종별 연령대별 카드 사용금액 차이
category_age_bill <- bccard_data %>% 
  group_by(category,age) %>% 
  summarise(mean_bill=round(mean(bill),-1))

ggplot(category_age_bill,aes(x=reorder(category,-mean_bill),y=mean_bill,fill=age))+geom_col(width=0.7)+
  labs(x="업종",y="카드사용금액평균",title="업종 및 연령대별 제주 내국인관광객 카드사용금액 평균")+
  theme_classic()
