if (!require("gapminder")){install.packages("gapminder")}
library(ggplot2)

p <-ggplot(gapminder,
           aes(x = gdpPercap, y = lifeExp))
p

summary(p)

p_point <- p + geom_point()
summary(p_point)

(p_point_color <- p + 
    geom_point(aes(color = continent)))
summary(p_point_color)
###########################################
###########################################
###########################################

gap_af <- gapminder[gapminder$country == "Afghanistan",]

p <-ggplot(gap_af,aes(x = year, y = lifeExp))+
  geom_line()

p
###########################################
###########################################
###########################################

p_1 <- ggplot(gapminder,aes(pop))+
  geom_histogram()

p_2 <- ggplot(gapminder,aes(continent,lifeExp))+
  geom_violin()+
  stat_summary(fun.y="mean", geom="point",color="blue",size = 3)

gapminder


gapminder_freq <- data.frame(table(gapminder$continent))
# dt_gapminder <- data.table::as.data.table(gapminder)
# dt_gapminder[,.N,by = continent][order(continent,decreasing = T)]


# gapminder_freq <- gapminder %>% count(continent)
# p_3 <- ggplot(gapminder_freq,aes(continent,n))+
#   geom_bar(stat = "identity")

p_3 <- ggplot(gapminder,aes(continent))+
  geom_bar()



p_4 <- ggplot(gapminder[gapminder$country %in% c("Canada", "Rwanda", "Cambodia", "Mexico"),],aes(year,lifeExp,color = country))+
  geom_point()+
  geom_line()

# library(cowplot)
# plot_grid(p_1, p_2, p_3, p_4, labels=c(1,2,3,4), ncol = 2, nrow = 2)


library(gridExtra)
grid.arrange(p_1, p_2, p_3, p_4, ncol=2)


###########################################
###########################################
###########################################
library(ggmap)
loc<-URLencode(enc2utf8("서울"))
                 
wifi <- data.table::fread("./data/wifi.csv",encoding="UTF-8")
s_wifi <- wifi[grep("서울",wifi$소재지도로명주소)]

# s_wifi <- wifi[grep("서울",wifi$소재지도로명주소)];dim(s_wifi)
# s_wifi <- wifi %>% filter(grepl("서울",wifi$소재지도로명주소));dim(s_wifi)

s_wifi <- wifi[grep("서울",소재지도로명주소),.(관리기관명,위도,경도)]
colnames(s_wifi) <- c("ser","lat","lon")
s_wifi

get_googlemap(loc, zoom = 11,maptype = "roadmap") %>% 
  ggmap() + 
  geom_point(as.data.frame(s_wifi), aes(lat,lon,color =ser))



ggplot(s_wifi,aes(x=위도, y=경도))+geom_point()
