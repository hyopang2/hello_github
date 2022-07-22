walkingtour_data <- read.csv("walkingtour.csv")
head(walkingtour_data)
str(walkingtour_data)

colnames(walkingtour_data) <- c("area","age","sex")

# 오름별 방문객수 순위


library(ggplot2)
library(dplyr)

table(walkingtour_data$area)
top3 <- count(walkingtour_data,area) %>% arrange(desc(n)) %>% head(3)
top3

ggplot(top3,aes(x=reorder(area,-n),y=n,fill=area))+geom_col(width=0.5)+
  labs(x="오름",y="방문횟수",title="오름별 도보여행객 방문횟수")+
  theme_classic()


#성별 오름 방문횟수
table(walkingtour_data$sex)

ggplot(walkingtour_data,aes(x=sex))+geom_bar(width=0.5)+
  labs(x="성별",y="방문횟수",title="성별 도보여행객 오름 방문횟수")+
  theme_classic()

#연령대별 오름 방문횟수
table(walkingtour_data$age)

walkingtour_data$age <- factor(walkingtour_data$age,
                               level=c("아동","청년","중장년","어르신"))

ggplot(walkingtour_data,aes(x=age,fill=age))+geom_bar(width=0.5)+
  labs(x="연령대",y="방문횟수",title="연령대별 도보여행객 오름 방문횟수")+
  theme_classic()
  
