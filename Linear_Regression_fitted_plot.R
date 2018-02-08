# Example: 

# display scatterplot and fitted line from linear regression 
# combine multiple group in one figure using facet_grid

install.packages("xlsx")
library(xlsx)
data1 <- read.xlsx("Z:/Donglei/20170329-slope/data/modified.xlsx", sheetName="CerS 5_6")[,1:4]
data2 <- read.xlsx("Z:/Donglei/20170329-slope/data/modified.xlsx", sheetName="CerS 2")[,1:4]
data3 <- read.xlsx("Z:/Donglei/20170329-slope/data/modified.xlsx", sheetName="DeS1")[,1:4]
data4 <- read.xlsx("Z:/Donglei/20170329-slope/data/modified.xlsx", sheetName="Hexosylceramide")[1:27,1:4]
data5 <- read.xlsx("Z:/Donglei/20170329-slope/data/modified.xlsx", sheetName="Sphingomyelin")[1:27,1:4]

data1$Sheet<-"CerS 5_6"
data2$Sheet<-"CerS 2"
data3$Sheet<-"DeS1"
data4$Sheet<-"Hexosylceramide"
data5$Sheet<-"Sphingomyelin"

data.merge<-rbind(data1,data2,data3,data4,data5)
data.merge$Sheet_f = factor(data.merge$Sheet, levels=c('CerS 5_6','CerS 2','DeS1','Hexosylceramide','Sphingomyelin'))
colnames(data.merge)[3] <- "Treatment"


plot <- ggplot(data=data.merge, aes(x=Time.Point, y=pmole, group = Treatment, colour=Treatment,shape=Treatment))+
  geom_point()+
  geom_smooth(method = "lm", fill=NA,size=0.5)+
  facet_grid(.~Sheet_f,scales="free")+
  theme(legend.position="bottom")+
  labs(x="Time",y="Pmole")

ggsave("Z:/Donglei/20170329-slope/slope_raw2.png", height=3, width=10, units='in', dpi=2000)
