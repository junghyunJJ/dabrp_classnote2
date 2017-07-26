str(iris)

train<-iris[,-5]

res_kmeans <- kmeans(train,centers = 3,iter.max = 100)
res_scale_kmeans <- kmeans(scale(train),centers = 3,iter.max = 100)


res_kmeans$cluster

table(iris[,5],res_kmeans$cluster)
table(iris[,5],res_scale_kmeans$cluster)
################################################################################
################################################################################
################################################################################
library(MASS)

s_birthwt <- birthwt[,-1]
summary(s_birthwt)
res_birthwt_lm <- lm(bwt~.,data = )
################################################################################
################################################################################
################################################################################
if (!require(arules)) install.packages("arules") 
data(Groceries)
summary(Groceries)


dvdread<-read.csv("./data/dvdtrans.csv")
head(dvdread)
table(dvdread)


tmp <- tapply(dvdread[,2],dvdread[,1],function(x) paste0("{",paste(x,collapse = ","),"}"))


t_dvdread <- data.frame(items = tmp,
                        transactionID = names(tmp))

library(dplyr)
dvdread %>%
  group_by(ID) %>%
  summarise()



# dvdtran<-read.transactions("./data/dvdtrans.csv")
# inspect(dvdtran[1:10,])


load("./data/dvdTranSample.RData")
inspect(dvdTranSample)



split(dvdread$Item,dvdread$ID)
