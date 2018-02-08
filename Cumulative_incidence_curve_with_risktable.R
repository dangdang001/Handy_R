# Date: 12/20/2017

# Project: 20171009-nissen followed incisional hernia

# Objective: 1. Drawing the incidence curve of developing incisional/ventral hernia repair 
#            among nissen and paraesophageal hernia repair patients.  
#            2. Cumulative incidence table by year

# Note: Modified based on Lizhou's code

library(cmprsk)
library(ggplot2)
library(dplyr)
library(grid)
library(scales)
library(survival)

setwd("//med-cloud01/osa$/Cores/Biostatistics/Donglei/20171009-nissen followed incisional hernia");
dat<-read.csv("./temp/ci_curve.csv", header=T); 

# 1. Drawing the incidence curve

# Compute incidence rates.
inc.all <- cuminc(ftime = dat$time_to_event/365.25, fstatus = dat$incisional_ventral_hernia, rho = 0, cencode = 0, na.action = na.omit)
inc <- data.frame(Surgery = "Overall", inc.all[[1]])

inc.surg <- cuminc(ftime = dat$time_to_event/365.25, fstatus = dat$incisional_ventral_hernia, group = dat$comb, rho = 0, cencode = 0, na.action = na.omit)
for (i in 1:(length(inc.surg)-1)) {
  inc <- rbind(inc, data.frame(Surgery = gsub("^([[:alpha:]]+)(\\s+)([[:digit:]])", "\\1", names(inc.surg)[i]), inc.surg[[i]]))
}


# Compute number of patients at risk.
# Overall first
riskall <- table(dat$time_to_event)
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
for (i in 1:length(levels(dat$comb))) {
  temp <- dat[dat$comb == levels(dat$comb)[i],]
  risktemp <- table(temp$time_to_event)
  riskdat2[[i]] <- data.frame(Time = as.numeric(names(risktemp)), RiskNum = NA)
  for (j in 1:length(risktemp)) {
    if (j == 1) {
      riskdat2[[i]]$RiskNum[j] <- nrow(temp)
    } else {
      riskdat2[[i]]$RiskNum[j] <- riskdat2[[i]]$RiskNum[j-1] - risktemp[j-1]
    }
  }
}
names(riskdat2) <- levels(dat$comb)

# Merge all numbers of patients at risk together.
riskfinal <- c(list(Overall = riskdat), riskdat2) %>% Reduce(function(x, y) full_join(x, y, by = "Time"), .)
for (i in nrow(riskfinal):1) {
  for (j in 1:length(levels(dat$comb))+2) {
    if (is.na(riskfinal[i, j])) {
      if (i == nrow(riskfinal)) {riskfinal[i, j] <- riskfinal[i, 2] - sum(riskfinal[i, 3:ncol(riskfinal)], na.rm = TRUE)
      } else {riskfinal[i, j] <- riskfinal[i+1, j]}
    }
  }
}
names(riskfinal)[-1] <- c("Overall", as.character(levels(dat$comb)))

# Summarize risk of patients at selected data points, which are years 0-10.
seltime <- 0:10
riskfinalsel <- riskfinal[1:length(seltime),]
for (i in 1:length(seltime)) {
  temp <- riskfinal[riskfinal$Time/365.25 <= seltime[i],]
  riskfinalsel[i,] <- temp[nrow(temp),]
}
riskfinalsel$Timey <- riskfinalsel$Time/365.25
riskfinalsel$Select <- seltime

# output incidence curve.
png("./temp/Incidence Curve.png", height = 6, width = 8, units = 'in', res = 600)

incplot <- ggplot(data = inc, aes(x = time, y = est, group = Surgery, color = Surgery))+
  geom_line()+
  scale_color_discrete(name="Surgery type",labels = c("Overall","N", "R"))+
  theme(legend.position=c(0.8, 0.2),plot.margin = unit(c(1, 1, 5, 1), "lines"))+
  scale_x_continuous(limits=c(0, 10),breaks = 0:10)+
  ylab("Cumulative Incidence Rate")+
  xlab("Year after Initial Surgery ")

# Add number of patients at risk at each time points.
for (i in 1:length(seltime)) {
  incplot <- incplot + annotation_custom(grob = textGrob(riskfinalsel[i, 2],
                                                         gp = gpar(cex = 0.7, col = hue_pal()(length(levels(dat$comb))+1)[1])),
                                         xmin = riskfinalsel$Select[i], xmax = riskfinalsel$Select[i],
                                         ymin = -0.04, ymax = -0.04)
  for (j in 1:length(levels(dat$comb))) {
    incplot <- incplot + annotation_custom(grob = textGrob(riskfinalsel[i, 2+j],
                                                           gp = gpar(cex = 0.7, col = hue_pal()(length(levels(dat$comb))+1)[1+j])),
                                           xmin = riskfinalsel$Select[i], xmax = riskfinalsel$Select[i],
                                           ymin = -0.04 - 0.01*j, ymax = -0.04 - 0.01*j)
  }
}

# Add labels for each type of patients at risk.
incplot <- incplot + annotation_custom(grob = textGrob("Overall:",
                                                       gp = gpar(cex = 0.7, col = hue_pal()(length(levels(dat$comb))+1)[1])),
                                       xmin = -0.85, xmax = -0.85, ymin = -0.04, ymax = -0.04)
for (j in 1:length(levels(dat$comb))) {
  incplot <- incplot + annotation_custom(grob = textGrob(paste(levels(dat$comb)[j], ":", sep = ""),
                                                         gp = gpar(cex = 0.7, col = hue_pal()(length(levels(dat$comb))+1)[1+j])),
                                         xmin = -0.85, xmax = -0.85, ymin = -0.04 - 0.01*j, ymax = -0.04 - 0.01*j)
}

incplot <- incplot + annotation_custom(grob = textGrob("Number of Patients at Risk", gp = gpar(cex = 0.7)),
                                       xmin = 0, xmax = 0, ymin = -0.03, ymax = -0.03)

# Add log-rank test p-value

logrank<-survdiff(Surv(dat$time_to_event, dat$incisional_ventral_hernia) ~ dat$comb,rho=0)

logrank_pvalue<-pchisq(logrank$chisq, length(logrank$n)-1, lower.tail = FALSE)

logrank_pvalue<-format.pval(logrank_pvalue,eps = .0001, digits = 5)

incplot <- incplot + annotate("text", x=1, y=max(inc$est)-max(inc$est)/10, label= paste0("Log-rank test:","\n",'P ',logrank_pvalue))
  
# Plot the final output.
gt <- ggplot_gtable(ggplot_build(incplot))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

dev.off()



# Cumulativei incidence table by year 

inc.all <- cuminc(ftime = dat$time_to_event/365.25, fstatus = dat$incisional_ventral_hernia, rho = 0, cencode = 0, na.action = na.omit)
inc <- data.frame(Surgery = "Overall", inc.all[[1]])

inc.surg <- cuminc(ftime = dat$time_to_event/365.25, fstatus = dat$incisional_ventral_hernia, group = dat$comb, rho = 0, cencode = 0, na.action = na.omit)


#overall
est_var.overall<-c()
t<-seq(1,10,1)
surg=c("N","R")

for (i in 1:length(t))
{
  est_var.overall<-rbind(est_var.overall,data.frame(group="Overall",time=t[i],estimate=timepoints(inc.all, times=t[i])$est[[1]],variance=timepoints(inc.all, times=t[i])$var[[1]]))
  
}

#by surgery
est_var.score<-NULL

for(j in 1:2)
{
  
  for (i in 1:length(t))
  {
    est_var.score<-rbind(est_var.score,data.frame(group=surg[j],time=t[i],estimate=timepoints(inc.surg, times=t[i])$est[[j]],variance=timepoints(inc.surg, times=t[i])$var[[j]]))
    
  }
  
}

est_var<-rbind(est_var.overall,est_var.score)

write.csv(est_var, file="./temp/CI_est_var.csv")









