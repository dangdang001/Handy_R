# Date: 01/25/2018

# Project: MU prediction

# Objective: 1. Drawing cumulative incidence curve for smoking vs non-smoking vs overall  
#            2. Cumulative incidence table by year

# Note: Modified based on Lizhou's code

library(cmprsk)
library(ggplot2)
library(dplyr)
library(grid)
library(scales)
library(survival)

setwd("//med-cloud01/osa$/Cores/Biostatistics/Donglei/20161003-Marginal ulcers following bariatric surgery");
mu.data.all<-read.csv("./temp/time_to_mu_all.csv", header=T); 


sig.cox<-c("gender","race_new","payer_new","length_of_stay","CHF","HTN_C","CHRNLUNG","Diabetes","RENLFAIL","LIVER","LYTES","ALCOHOL","DRUG","PSYCH","DEPRESS","tobacco","Anastomotic","Ventilation",
           "systemicinflamm","digestive","surgicalerror");

which(is.na(mu.data.all[sig.cox]), arr.ind=TRUE) #row 24343:gender missing

dat<-mu.data.all[is.na(mu.data.all$gender)!=TRUE,] #delete na, now 35074 oberservations

# 1. Drawing the incidence curve

dat$tobacco=as.factor(dat$tobacco)

# Compute incidence rates.
inc.all <- cuminc(ftime = dat$time_mu/365.25, fstatus = dat$mu, rho = 0, cencode = 0, na.action = na.omit)
inc <- data.frame(Group = "Overall", inc.all[[1]])

inc.surg <- cuminc(ftime = dat$time_mu/365.25, fstatus = dat$mu, group = dat$tobacco, rho = 0, cencode = 0, na.action = na.omit)
for (i in 1:(length(inc.surg)-1)) {
  inc <- rbind(inc, data.frame(Group = gsub("^([[:alpha:]]+)(\\s+)([[:digit:]])", "\\1", names(inc.surg)[i]), inc.surg[[i]]))
}


# Compute number of patients at risk.
# Overall first
riskall <- table(dat$time_mu)
riskdat <- data.frame(Time = as.numeric(names(riskall)), RiskNum = NA)
for (i in 1:length(riskall)) {
  if (i == 1) {
    riskdat$RiskNum[i] <- nrow(dat)
  } else {
    riskdat$RiskNum[i] <- riskdat$RiskNum[i-1] - riskall[i-1]
  }
}

# Each specific level
riskdat2 <- list()
for (i in 1:length(levels(dat$tobacco))) {
  temp <- dat[dat$tobacco == levels(dat$tobacco)[i],]
  risktemp <- table(temp$time_mu)
  riskdat2[[i]] <- data.frame(Time = as.numeric(names(risktemp)), RiskNum = NA)
  for (j in 1:length(risktemp)) {
    if (j == 1) {
      riskdat2[[i]]$RiskNum[j] <- nrow(temp)
    } else {
      riskdat2[[i]]$RiskNum[j] <- riskdat2[[i]]$RiskNum[j-1] - risktemp[j-1]
    }
  }
}
names(riskdat2) <- levels(dat$tobacco)

# Merge all numbers of patients at risk together.
riskfinal <- c(list(Overall = riskdat), riskdat2) %>% Reduce(function(x, y) full_join(x, y, by = "Time"), .)
for (i in nrow(riskfinal):1) {
  for (j in 1:length(levels(dat$tobacco))+2) {
    if (is.na(riskfinal[i, j])) {
      if (i == nrow(riskfinal)) {riskfinal[i, j] <- riskfinal[i, 2] - sum(riskfinal[i, 3:ncol(riskfinal)], na.rm = TRUE)
      } else {riskfinal[i, j] <- riskfinal[i+1, j]}
    }
  }
}
names(riskfinal)[-1] <- c("Overall", as.character(levels(dat$tobacco)))

# Summarize risk of patients at selected data points, which are years 0-10.
seltime <- 0:8
riskfinalsel <- riskfinal[1:length(seltime),]
for (i in 1:length(seltime)) {
  temp <- riskfinal[riskfinal$Time/365.25 <= seltime[i],]
  riskfinalsel[i,] <- temp[nrow(temp),]
}
riskfinalsel$Timey <- riskfinalsel$Time/365.25
riskfinalsel$Select <- seltime


inc$Group_new[inc$Group=="Overall"]<-"Overall"
inc$Group_new[inc$Group=="0 1"]<-"Non-smoking"
inc$Group_new[inc$Group=="1 1"]<-"Smoking"

inc$Group=inc$Group_new

# output incidence curve.
png("./temp/Incidence Curve.png", height = 6, width = 8, units = 'in', res = 600)

incplot <- ggplot(data = inc, aes(x = time, y = est, group = Group,color = Group))+
  geom_line()+
  theme(legend.position=c(0.9, 0.2),plot.margin = unit(c(1, 1, 5, 1), "lines"))+
  scale_x_continuous(limits=c(0, 10),breaks = 0:10)+
  geom_errorbar(mapping = aes(x=time,ymin=estimate-1.96*sqrt(Variance), ymax=estimate+1.96*sqrt(Variance),group=score,color=score), width=.15,
                position=position_dodge(0.05),
                data=est_var,inherit.aes = FALSE)+
  scale_color_discrete(name="Group")+
  ylab("Cumulative Incidence Rate")+
  xlab("Year after Bypass ")

incplot

dev.off()



# output incidence curve (black and white).
png("./temp/Incidence Curve_Black_White.png", height = 6, width = 8, units = 'in', res = 600)

incplot <- ggplot(data = inc, aes(x = time, y = est, group = Group,linetype = Group))+
  geom_line()+
  theme(legend.position=c(0.9, 0.2),plot.margin = unit(c(1, 1, 5, 1), "lines"))+
  scale_x_continuous(limits=c(0, 10),breaks = 0:10)+
  geom_errorbar(mapping = aes(x=time,ymin=estimate-1.96*sqrt(Variance), ymax=estimate+1.96*sqrt(Variance),group=score,linetype=score), width=.15,
                position=position_dodge(0.05),
                data=est_var,inherit.aes = FALSE)+
  ylab("Cumulative Incidence Rate")+
  xlab("Year after Bypass ")

incplot

dev.off()








