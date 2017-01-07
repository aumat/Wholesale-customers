getwd()
cust<-read.csv("Wholesale customers data.csv")
str(cust)

#Nominal Variables-CHANNEL: customersâ€™ Channel - Horeca (Hotel/Restaurant/CafÃ©) or Retail channel,REGION: customersâ€™ Region â€“ Lisnon, Oporto or Other

cust$Channel[cust$Channel==1] <- "Hotel/Rest/Cafe"
cust$Channel[cust$Channel==2] <- "Retail Channel"
cust$Region[cust$Region==1] <- "Lisnon"
cust$Region[cust$Region==2] <-"Oporto"
cust$Region[cust$Region==3] <-"Others"

cust$Channel <- as.factor(cust$Channel)
cust$Region <- as.factor(cust$Region)

summary(cust)
table(cust$Channel,cust$Region)

# Checking for missing variables
colSums(is.na(cust))

#Scaling Data
Scale<- scale(cust[-1:-2])

#Checking the right number of clusters

library(NbClust)
nc <- NbClust(Scale,method="kmeans")
nc$All.index
library(fpc)
pamkClus<-pamk(Scale,krange=2:10,criterion = "multiasw",critout = T)
cat("number of clusters estimated by optimum average silhouette width:", pamkClus$nc, "\n")
#Both these indexes estimated the optimal clusters to be '2'


#Performing Clustering on the scaled data with K=2

set.seed(122)
fit<- kmeans(Scale,2)
fit

fit.cust<-cbind(cust,Cluster=as.factor(fit$cluster))
a<- fit.cust [,3:9]
summary(a)
library(dplyr)
Mean <- a %>% group_by(Cluster) %>% summarise_each(funs(mean)) 

y <-table (fit.cust$Cluster,fit.cust$Region)
y
library(ggplot2)

#Cluster Analysis of Milk Vs. Grocery across Channels and Regions
p<-ggplot(fit.cust,aes(x=Milk,y=Grocery))
p+geom_point(aes(colour=Cluster))+
  facet_wrap((~Channel+Region),labeller = "label_both",scales="free")+ggtitle("Cluster Analysis of Milk Vs. Grocery across Channels and Regions")+
  theme(plot.title = element_text(hjust=0.5))

#Cluster Analysis of Fresh Vs. Frozen across Channels and Regions
p1<-ggplot(fit.cust,aes(x=Fresh,y=Frozen))
p1+geom_point(aes(colour=Cluster))+facet_wrap((~Channel+Region),labeller = "label_both",scales="free")+ggtitle("Cluster Analysis of Fresh Vs. Frozen across Channels and Regions")+
  theme(plot.title = element_text(hjust=0.5))

#Cluster Analysis of Detergents_Paper Vs. Delicassen across Channels and Regions
p2<-ggplot(fit.cust,aes(x=Detergents_Paper,y=Delicassen))
p2+geom_point(aes(colour=Cluster))+facet_wrap((~Channel+Region),labeller = "label_both",scales="free")+ggtitle("Cluster Analysis of Detergents_Paper Vs. Delicassen across Channels and Regions")+
  theme(plot.title = element_text(hjust=0.5))



