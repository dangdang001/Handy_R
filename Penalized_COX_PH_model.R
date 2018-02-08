# Date:     20171211
# Project:  Marginal ulcers following bariatric surgery
# Purpose:  Use penalized cox proportional hazard model to predict patients' risk of developing Marginal Ulcer 
#           after bypass surgery


# Definition of Time to MU/Censored time:
#   
# For patients having MU later on, time to MU were defined as the time interval between their original bypass to the first MU diagnosis;
# 
# For patients not having Mu later on, their censored time were defined as the time interval between their original bypass to the last follow-up records during the study period;
# 
# For patients not having any follow-up during the study period, their censored time were defined as the time interval between their discharged date and the admission date of their original bypass records.


source("//med-cloud01/osa$/Cores/Biostatistics/Donglei/20161003-Marginal ulcers following bariatric surgery/code/setup.R")

library(penalized)

# Prepare all covariates to be considered in the model

cov.f<-c("age", "gender.f","race_new.f","region1.f","payer_new.f", "CHF.f","VALVE.f","PULMCIRC.f","PERIVASC.f","HTN_C.f","PARA.f","NEURO.f","CHRNLUNG.f",
         "Diabetes.f","HYPOTHY.f","RENLFAIL.f","LIVER.f","ULCER.f","LYMPH.f","METS.f","TUMOR.f","ARTH.f","COAG.f","WGHTLOSS.f","LYTES.f",
         "BLDLOSS.f","ANEMDEF.f","ALCOHOL.f","DRUG.f","PSYCH.f","DEPRESS.f","tobacco.f")

cox.full.f<-mu.data.all.new[,c(cov.f,"mu","time_mu")]



# 1. Run 5-repeated 5 fold cross validation to select the optimal parameter lambda

cox.ridge.cv<-function(data,k,p)  
  
{
  lambda=seq(from=1,to=100,by=5)
  temp<-0
  lambda.best=lambda[1]
  c<-numeric(0)
  temp<-0
  rank<-NULL
  ave.rank<-NULL
  var.list<-NULL
  
  for(j in 1:(length(lambda)))
  {
    
    ave.c = numeric(0)
    
    for(r in 1:p)
    {
      
      for(i in 1:k)
      {
        
        set.seed(r)
        
        index<-split(sample(1:nrow(data),nrow(data),replace = F),1:k)[[i]]; # left for validation each time: index
        
        train<-data[-index,] #training set
        test<-data[index,] #testing set
        
        fit <- coxph(Surv(time_mu, mu) ~ age+gender.f+race_new.f+region1.f+payer_new.f+CHF.f+VALVE.f+PULMCIRC.f+PERIVASC.f+HTN_C.f+PARA.f+NEURO.f
                     +CHRNLUNG.f+Diabetes.f+HYPOTHY.f+RENLFAIL.f+LIVER.f+ULCER.f+LYMPH.f+METS.f+TUMOR.f+ARTH.f+COAG.f+WGHTLOSS.f+LYTES.f+
                       BLDLOSS.f+ANEMDEF.f+ALCOHOL.f+DRUG.f+PSYCH.f+DEPRESS.f+tobacco.f, data=data)
        
        xfit <- model.matrix(fit)
        
        coxmodel <- penalized(Surv(train$time_mu,train$mu), penalized=xfit[-index,],standardize=TRUE,model = 'cox',lambda2=lambda[j])
        
        
        pred = as.matrix(predict(coxmodel, penalized=xfit[index,], data=test))
        
        coef<-coef(coxmodel)
        
        c1 = rcorr.cens(x=pred[,dim(pred)[2]],S=Surv(test$time_mu, test$mu))["C Index"]
        
        c=c(c,c1)
        
        rank<-rbind(rank,rank(-abs(coef)))
        
        var.list<-names(coef)
        
      }
      
    }
    
    ave.rank<-colMeans(rank)
    ave.c = mean(c)
    
    if(ave.c>temp) 
    {
      temp<-ave.c
      rank.best<-ave.rank
      lambda.best=lambda[j]
      var.best=var.list
    }
    
    
  }
  
  
  result<-list(maxc=temp,lambda.best=lambda.best,var.best=var.best,rank.best=rank.best)
  
  return(result)
  
}



system.time(cox.ridge.mu<-cox.ridge.cv(data=cox.full.f,k=5,p=5))

# lambda.best=96


# 2. Use the optimal lambda to perform the penalized cox model

# 3-fold cross-validations were used to evaluate predictive accuracy of each model, of which 100 times bootstraps were applied 
# to calculate the averaged estimated C-statistics and corresponding 95% confidence interval; 
# 4 different approaches (AUC.cd, AUC.sh, AUC.uno, UnoC, from "survAUC" package) were used to calculated the c-statistics 
# for each model.


fit <- coxph(Surv(time_mu, mu) ~ age+gender.f+race_new.f+region1.f+payer_new.f+CHF.f+VALVE.f+PULMCIRC.f+PERIVASC.f+HTN_C.f+PARA.f+NEURO.f
             +CHRNLUNG.f+Diabetes.f+HYPOTHY.f+RENLFAIL.f+LIVER.f+ULCER.f+LYMPH.f+METS.f+TUMOR.f+ARTH.f+COAG.f+WGHTLOSS.f+LYTES.f+
               BLDLOSS.f+ANEMDEF.f+ALCOHOL.f+DRUG.f+PSYCH.f+DEPRESS.f+tobacco.f, data=cox.full.f)

xfit <- model.matrix(fit)

set.seed(99)
ind<-split(sample(1:nrow(mu.data.all.new),nrow(mu.data.all.new),replace = F),1:3)

train.1<-cox.full.f[-ind[[1]],] #training set:2/3 (fixed) 23382
test.1<-cox.full.f[ind[[1]],] #testing set:1/3 (fixed) 11692
train.2<-cox.full.f[-ind[[2]],] #training set:2/3 (fixed) 23382
test.2<-cox.full.f[ind[[2]],] #testing set:1/3 (fixed) 11692
train.3<-cox.full.f[-ind[[3]],] #training set:2/3 (fixed) 23382
test.3<-cox.full.f[ind[[3]],] #testing set:1/3 (fixed) 11692

xfit.train.all<-list()
xfit.test.all<-list()

for (i in 1:3)
{
  xfit.train.all[[i]]<-xfit[-ind[[i]],]
  xfit.test.all[[i]]<-xfit[ind[[i]],]
}

xfit.train<-NULL
xfit.test<-NULL

times <- c(365, 731,1096,1464,365*5,2191,365*7,365*8)

my.run.pl<-function(train.all, test.all, R, year,ind_seed,ind,xfit,xfit.train.all,xfit.test.all)
  
{
  
  xfit.train<-xfit.train.all[[ind_seed]]
  xfit.test<-xfit.test.all[[ind_seed]]
  
  coxmodel <- penalized(Surv(train.all$time_mu,train.all$mu), penalized=xfit.train,standardize=TRUE,model = 'cox',lambda2=96)
  
  train.fit<-coxmodel
  
  # define the 5 bootstrap functions:
  
  AUC.cd.fn<-function(testdata,index,time)
  {
    test=testdata[index,]
    temp=auc.suv.caculator.pl(train.fit =train.fit, train.all, test, method=AUC.cd, times=time,xfit.train=xfit.train, xfit.test=xfit.test)
    return(temp)
  }
  
  AUC.hc.fn<-function(testdata,index,time)
  {
    test=testdata[index,]
    temp=auc.suv.caculator.pl(train.fit =train.fit, train.all, test, method=AUC.hc, times=time,xfit.train=xfit.train, xfit.test=xfit.test)
    return(temp)
  }
  
  AUC.sh.fn<-function(testdata,index,time)
  {
    test=testdata[index,]
    temp=auc.suv.caculator.pl(train.fit =train.fit, train.all, test, method=AUC.sh, times=time,xfit.train=xfit.train, xfit.test=xfit.test)
    return(temp)
  }
  
  AUC.uno.fn<-function(testdata,index,time)
  {
    test=testdata[index,]
    temp=auc.suv.caculator.pl(train.fit =train.fit, train.all, test, method=AUC.uno, times=time,xfit.train=xfit.train, xfit.test=xfit.test)
    return(temp)
  }
  
  UnoC.fn<-function(testdata,index,time)
  {
    test=testdata[index,]
    temp=auc.suv.caculator.pl(train.fit =train.fit, train.all, times=time,test, method=UnoC,xfit.train=xfit.train, xfit.test=xfit.test)
    return(temp)
  }
  
  
  # Perform bootstrap
  
  method_list=c("AUC.cd.fn","AUC.hc.fn","AUC.sh.fn","AUC.uno.fn","UnoC.fn")
  est.list<-NULL
  upperCL.list<-NULL
  lowerCL.list<-NULL
  
  
  my.boot<-function(R,year)
  {
    for (m in 1:length(method_list))
    {
      est.seq=c()
      upperCL.seq=c()
      lowerCL.seq=c()
      times <- c(365, 731,1096,1464,365*5,2191,365*7,365*8)
      
      for (i in 1:year)
      {
        func=match.fun(method_list[m])
        a<-boot(test.all,func,R,time=times[i])
        est=mean(a$t)
        upperCL=quantile(a$t,0.975)
        lowerCL=quantile(a$t,0.025)
        est.seq=c(est.seq, est)
        upperCL.seq=c(upperCL.seq, upperCL)
        lowerCL.seq=c(lowerCL.seq, lowerCL)
      }
      est.list[[m]]=est.seq
      upperCL.list[[m]]=upperCL.seq
      lowerCL.list[[m]]=lowerCL.seq
    }
    result<-list(est.out=est.list,lowerCL.out=lowerCL.list,upperCL.out=upperCL.list)
    return(result)
  }
  return(my.boot(R=R, year=year))
}



system.time(boot.output.1.1<-my.run.pl(train.all=train.1, test.all=test.1,R=100, year=8,ind_seed = 1,ind=ind,xfit.train.all=xfit.train.all,xfit.test.all=xfit.test.all))
save(boot.output.1.1, file = "./boot.output.1.1.RData")

load("./boot.output.1.2.RData")
load("./boot.output.1.3.RData")


est<-rowMeans(cbind(unlist(boot.output.1.1$est.out),unlist(boot.output.1.2$est.out),unlist(boot.output.1.3$est.out)))
upper<-rowMeans(cbind(unlist(boot.output.1.1$upperCL.out),unlist(boot.output.1.2$upperCL.out),unlist(boot.output.1.3$upperCL.out)))
lower<-rowMeans(cbind(unlist(boot.output.1.1$lowerCL.out),unlist(boot.output.1.2$lowerCL.out),unlist(boot.output.1.3$lowerCL.out)))

est<-unlist(round(est, 3))
upper<-unlist(round(upper, 3))
lower<-unlist(round(lower, 3))

my.paste<-function(x,l,u)
{
  paste0(x," (", l, ",", u, ")")
}

output<-data.frame(matrix(my.paste(est,lower,upper),nrow=5,byrow = T))
colnames(output)<-c("1st Year","2nd Year","3rd Year","4th Year","5th Year","6th Year","7th Year","8th Year")
rownames(output)<-c("AUC.cd","AUC.hc","AUC.sh","AUC.uno","UnoC")

write.csv(output, file = "./auc_est_ci_penalized_cox.csv")










