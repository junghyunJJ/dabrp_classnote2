rm(list = ls())

# 1."./recomen" 폴더에 있는 데이터를 사용해 주세요.
# "chennel.csv"를 사용해서 x축은 chennel, y축은 각 체널별 사용량의 합으로 bar 차트를 그려주세요.
# "competitor.csv"를 사용해서 x축은 useDate, y축은 그 달의 데이터 수로 line 차트를 그려주세요
# "competitor.csv"를 사용해서 x축은 useDate, y축은 그 달의 데이터 수로 line 차트를 그려주세요
# 위와 같은 조건에서 partner 별로 라인과 색을 구분해서 그려주세요.
# 축 데이터가 날짜인 경우 scale_x_date() 함수를 참고하세요


library(ggplot2)
##############################
########    classic    #######
##############################
chennel <- read.csv("./recomen/chennel.csv")
tar_chennel <- tapply(chennel[,3],chennel[,2],sum)
tar_chennel <- data.frame(chennel = names(tar_chennel), V1 = tar_chennel)

g_tar_chennel <- ggplot(tar_chennel,aes(chennel,V1))+
  geom_bar(stat = "identity")



competitor <- read.csv("./recomen/competitor.csv")
tar_competitor <- tapply(as.factor(competitor[,1]),competitor[,4],length)
tar_competitor <- data.frame(useDate = names(tar_competitor), N = tar_competitor)
tar_competitor$useDate <- as.Date(paste0(tar_competitor$useDate,"01"),format="%Y%m%d")

g_tar_competitor <- ggplot(tar_competitor,aes(useDate,N))+
  geom_line()+
  scale_x_date(date_labels = "%Y%m%d")



tar_competitor2 <- tapply(as.factor(competitor[,1]),competitor[,c(2,4)],length)
tar_competitor2 <- melt(tar_competitor2, value.name = "N")
tar_competitor2$useDate <- as.Date(paste0(tar_competitor2$useDate,"01"),format="%Y%m%d")

g_tar_competitor2 <- ggplot(tar_competitor2,aes(useDate,N,group=partner,color = partner))+
  geom_line()+
  scale_x_date(date_labels = "%Y%m%d")+
  theme(legend.position="top")


library(gridExtra)
grid.arrange(g_tar_chennel, g_tar_competitor,g_tar_competitor2,nrow=1, ncol=3)




rm(list = ls())
##############################
######    data.table    ######
##############################
library(data.table)
chennel <- fread("./recomen/chennel.csv")
tar_chennel_dt <- chennel[,sum(useCnt),by=chennel]

g_tar_chennel_dt <- ggplot(tar_chennel_dt,aes(chennel,V1))+
  geom_bar(stat = "identity")



competitor <- fread("./recomen/competitor.csv")
competitor$useDate <- as.Date(paste0(competitor$useDate,"01"),format="%Y%m%d")
tar_competitor_dt <- competitor[,.N,by=useDate][order(useDate)]

g_tar_competitor_dt <- ggplot(tar_competitor_dt,aes(useDate,N))+
  geom_line()+
  scale_x_date(date_labels = "%Y%m%d")



tar_competitor_dt2 <- competitor[,.N,by=.(partner,useDate)][order(useDate)]

g_tar_competitor_dt2 <- ggplot(tar_competitor_dt2,aes(useDate,N,group=partner,color = partner))+
  geom_line()+
  scale_x_date(date_labels = "%Y%m%d")+
  theme(legend.position="top")



grid.arrange(g_tar_chennel_dt, g_tar_competitor_dt,g_tar_competitor_dt2,nrow=1, ncol=3)




rm(list = ls())
##############################
########    dplyr    #######
##############################
library(tidyverse)
chennel <- read_csv("./recomen/chennel.csv")
tar_chennel_dp <- chennel %>% 
  group_by(chennel) %>% 
  summarise(V1 = sum(useCnt))

p_tar_chennel_dp <- ggplot(tar_chennel_dp,aes(chennel,V1))+
  geom_bar(stat = "identity")



competitor <- read_csv("./recomen/competitor.csv")
competitor$useDate <-
  competitor$useDate %>% 
  paste0(.,"01") %>%
  as.Date(format="%Y%m%d")

tar_competitor_dp <- competitor %>% 
  group_by(useDate) %>% 
  summarise(N = length(cusID))

p_tar_competitor_dp <- ggplot(tar_competitor_dp,aes(useDate,N))+
  geom_line()+
  scale_x_date(date_labels = "%Y%m%d")

tar_competitor_dp2 <- competitor %>% 
  group_by(useDate,partner) %>%
  summarise(N = length(cusID))



p_tar_competitor_dp2 <- ggplot(tar_competitor_dp2,aes(useDate,N,group=partner,color = partner))+
  geom_line()+
  scale_x_date(date_labels = "%Y%m%d")+
  theme(legend.position="top")



grid.arrange(p_tar_chennel_dp, p_tar_competitor_dp,p_tar_competitor_dp2,nrow=1, ncol=3)


################################################################################
################################################################################
################################################################################
# 2.ggfortify 패키지와 과제 5에서 진행한 회귀분석을 사용해 주세요.
# 링크를 참고해서 fit 객체를 시각화해주세요. 링크의 2번째 그림을 따라하면 됩니다.

## 어떠한걸 진행하라고 하시는지 모르겠습니다.

par(mfrow = c(1, 2))
m <- lm(Petal.Width ~ Petal.Length, data = iris)

autoplot(m, which = 1:6, ncol = 3, label.size = 3)
################################################################################
################################################################################
################################################################################
# "./data/wifi.csv" 데이터를 사용해주세요.
# 서울중 각 구에 wifi가 몇개 잇는지 세고 
# 각 구 이름으로 geocoding한 위치에 갯수를 size로 하는 버블 차트를 그려주세요.
rm(list = ls())

library(ggmap)
library(data.table)
wifi <- fread("./data/wifi.csv",encoding = "UTF-8")
tar_wifi <- wifi[grep("^서울",wifi$설치시도명), 설치시군구명] %>% table %>% data.frame

# library(tidyverse)
# wifi <- read_csv("./data/wifi.csv")
# tar_wifi <- wifi %>% 
#   filter(grepl("^서울",설치시도명)) %>%
#   group_by(설치시군구명) %>%
#   summarise(N=n())%>%
#   arrange(desc(N))


colnames(tar_wifi) <- c("location","N")


for(i in 1:nrow(tar_wifi)){
  if( i ==1) latlon <- c()
  latlon <- rbind(latlon,
                  tar_wifi$location[i] %>% 
                    as.character %>% 
                    enc2utf8 %>% 
                    URLencode %>% 
                    geocode)}

f_tar_wifi <- cbind(tar_wifi,latlon)

get_googlemap(URLencode(enc2utf8("서울")),maptype = "roadmap",zoom = 11) %>% ggmap+
  geom_point(data = f_tar_wifi, aes(x = lon, y=lat, size=N))



# 시도를 기준으로 위와 같은 버블 차트를 그려주세요.

tar_wifi2 <- paste(wifi$설치시도명,wifi$설치시군구명) %>% table %>% data.frame
colnames(tar_wifi2) <- c("location","N")


for(i in 1:nrow(tar_wifi2)){
  if( i ==1) latlon2 <- c()
  latlon2 <- rbind(latlon2,
                   tar_wifi2$location[i] %>% 
                    as.character %>% 
                    enc2utf8 %>% 
                    URLencode %>% 
                    geocode)}

f_tar_wifi2 <- cbind(tar_wifi2,latlon2)


get_googlemap(center = c(lon=128, lat=35.9),zoom=7, maptype='roadmap') %>% ggmap + 
  geom_point(data = f_tar_wifi2, aes(x = lon, y=lat, size=N))


# 시도를 기준으로 wifi의 수가 기간에 따라 얼마나 늘어났는지 확인할 수 있는 라인 차트를 그려주세요.
tar_wifi3 <- wifi[, .(설치시도명,데이터기준일자)]
tar_wifi3$데이터기준일자 <- gsub("-..$","",tar_wifi3$데이터기준일자)
f_tar_wifi3 <- tar_wifi3 %>% table %>% data.frame

ggplot(f_tar_wifi3,aes(데이터기준일자,Freq,group = 설치시도명,color =설치시도명 )) + 
  geom_line()+
  geom_point()

# 서비스제공사명을 기준으로 통신 3사가 아닌 제공사는 기타로 처리하고, 
# 여러 개로 작성된 것은 각 개수 만큼 분리하여 독립 wifi로 처리하여 위의 차트를 서비스제공사명으로 그룹지어서 그려주세요. 
# 총 4개 차트가 함께 나오면 됩니다.

tar_wifi4 <- data.frame(wifi[, .(설치시도명,서비스제공사명)])
tar_wifi4$서비스제공사명 <- paste0(tar_wifi4$서비스제공사명,",")
tar_wifi4 <- as.data.table(tar_wifi4)
tar_wifi4 %>%
  separate(서비스제공사명,sep = ",",into = c("cases", "population"))

tmp <- t(tar_wifi4[서비스제공사명 == "LGU+,SKT"])

colnames(tmp) <- "tmp"
tmp %>%  
  separate(tmp,into = c("cases", "population"),sep = ",")

a <- tmp[2]
strsplit(a,",")

View(wifi$서비스제공사명 %>% table %>% data.frame())







