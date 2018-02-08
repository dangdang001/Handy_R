# 1. Histogram for continuous variable(e.g., conti_var) and Barplot for categorical variable(e.g., cat_var) 

## Basic R plots

hist(data$conti_var,labels=TRUE,freq=TRUE,xlab="conti_var", ylab="Number of ...") 

barplot(table(data$cat_var),freq=TRUE,xlab="cat_var", ylab="Number of ...") 

dev.off()

## Using ggplots

library(ggplot2)


g1<-ggplot(data, aes(conti_var)) + geom_histogram(binwidth = 5,colour="white") +
  stat_bin(aes(y=..count.., label=..count..),binwidth = 5,geom="text", vjust=-0.5)+
  labs(x = "conti_var",y="Number of ...")

g2<-ggplot(data, aes(cat_var))+
  geom_bar(stat="count", width=0.5)+geom_text(stat='count',aes(label=..count..),vjust=-0.5)+
  labs(x = "cat_var",y="Number of ...")

ggsave("./histogram.jpg", g1, height=6, width=8, units='in', dpi=2000)
ggsave("./barplot.jpg", g2, height=6, width=8, units='in', dpi=2000)


# 2. Scatterplot with correlation coefficient p-value added

library(grid)
library(gridExtra)

# Continuous vs Continuous
cor<-cor.test(data$conti_var, data$conti_out, method="spearman")
text<-paste0("Spearman's rho=",round(cor$estimate,4),", ", "p=",round(cor$p.value,4))

g1<-ggplot(data, aes(x = conti_var, y = conti_out)) +
  geom_point(shape = 21) +
  ggtitle(paste0("title1","\n",text)) +
  labs(x = "conti_var", y = "conti_out") +
  scale_x_continuous(breaks = seq(45, 90, 5)) 

# Categorical vs Continuous
cor<-kruskal.test(data$cat_var, data$conti_out)
text<-paste("Kruskal-Wallis's p=",round(cor$p.value,4))

g2<-ggplot(data, aes(x = cat_var, y = conti_out)) +
  geom_point(shape = 21) +
  ggtitle(paste0("title2","\n",text)) +
  labs(x = "cat_var", y = "conti_out") +
  scale_x_continuous(breaks = seq(1, 5, 1)) 

png("./scatterplot.png",height=5, width=8,units='in',res = 600)
grid.arrange(g1, g2, ncol = 2)
dev.off()
