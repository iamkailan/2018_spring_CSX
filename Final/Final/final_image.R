library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('rpart')
library('randomForest') # classification algorithm
data <-read.csv('sample.csv')
head(data)
methods <- data$method
head(methods)
unique(methods)
unique(hr)
hr <- data$hr
library(ggplot2)
rpbytes <- data$rpbytes
ggplot(mtcars, aes(x=hr, y=rpbytes)) + geom_point(size=2, shape=23)
library(ggplot2)
ggplot
ggplot(qq, aes(x=hr, y=rpbytes)) +geom_point() #+ geom_text(label=rownames(mtcars))
qq<- data.frame(
  hr =hr,
  rpbytes = rpbytes)
colSums (qq)
aggregated<-aggregate(rpbytes~hr,qq,function(x) sum(as.numeric(x)))
ggplot(aggregated, aes(x=hr, y=rpbytes))+geom_point(size=2, shape=23)+geom_line(color="red")+
  geom_point()
devices<-data$device            
unique(devices)
dts<-data$dt            
unique(dts)
qq2<- data.frame(
  devices =device,
  dts = dt)
colSums (qq2)
ggplot(data=qq2, aes(x=dts, y=devices)) +
  geom_bar(stat="identity", width=0.5)

devices<-data$device  
counts<-data.frame(table(devices))
tpo<-subset(counts, as.numeric(Freq)>500)
ggplot(tpo,aes(x=devices, y=Freq))+geom_bar(stat = "identity") +
  geom_bar(stat="identity", fill="steelblue")+theme_minimal()+coord_flip()

osfamily<-data$osfamily  
osfamily_counts<-data.frame(table(osfamily))
head(osfamily)
head(osfamily_counts)
osfamily_tpo<-subset(osfamily_counts, as.numeric(Freq)>500)
ggplot(osfamily_tpo,aes(x=osfamily, y=Freq))+geom_bar(stat = "identity") +
  geom_bar(stat="identity", fill="steelblue")+theme_minimal()+ coord_flip()
