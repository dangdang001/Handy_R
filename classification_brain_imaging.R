# Evaluating the performances of brain imaging measures, 2016.06

# Penalized logistic regression models and Random Forest were applied to separate Multiple Sclerosis(MS) 
# from Health Control(HC) patients using three different brain imaging measures (SUV, CBF and MRGlu). 

# Repeated (# of repetition=10) 4-fold cross-validation methods were used to evaluate predictive accuracy of different models 
# such as AUC, sensitivity, specificity, positive predictive value (PPV) and negative predictive value (NPV).

# Grid search was applied to tune model parameters

install.packages("glmnet")
install.packages("caret")
install.packages("pROC")
install.packages('e1071', dependencies=TRUE)
install.packages("randomForest")


setwd("Z:/Donglei/20160608-Brain");
patient.suv<-read.csv("./temp/brain_class_suv.csv", header=T); # patient-based data
patient.cbf<-read.csv("./temp/brain_class_cbf.csv", header=T);
patient.mrglu<-read.csv("./temp/brain_class_mrglu.csv", header=T);
patient.cbf.suv<-read.csv("./temp/brain_class_cbf_suv.csv", header=T);
patient.cbf.mrglu<-read.csv("./temp/brain_class_cbf_mrglu.csv", header=T);
head(patient.suv)


library(glmnet)
library(caret)
library(e1071)
library(pROC)
library(randomForest)

# Chapter 1: Penalized logistic model
# 1.1 SUV
# 1.2 CBF
# 1.3 MRGlu
# 1.4 CBF+SUV
# 1.5 CBF+MRGlu

# Chapter 2: Random forest
# 1.1 SUV
# 1.2 CBF
# 1.3 MRGlu
# 1.4 CBF+SUV
# 1.5 CBF+MRGlu

#data preparation:

#Categorical variables are usually first transformed into factors, then a dummy variable matrix of predictors is created and along with the continuous predictors, is passed to the model.

Gender<-as.factor(patient.suv$Gender)

factor <- model.matrix(patient.suv$Status ~ Gender)[,-1]  # necessary!

suv.all<-data.frame(patient.suv[-c(2)],Gender=factor)[,c(1,8,2:7)]

Gender<-as.factor(patient.cbf$Gender)

factor <- model.matrix(patient.cbf$Status ~ Gender)[,-1]  # necessary!

cbf.all<-data.frame(patient.cbf[-c(2)],Gender=factor)[,c(1,8,2:7)]

Gender<-as.factor(patient.mrglu$Gender)

factor <- model.matrix(patient.mrglu$Status ~ Gender)[,-1]  # necessary!

mrglu.all<-data.frame(patient.mrglu[-c(2)],Gender=factor)[,c(1,8,2:7)]

Gender<-as.factor(patient.cbf.suv$Gender)

factor <- model.matrix(patient.cbf.suv$Status ~ Gender)[,-1]  # necessary!

cbf.suv.all<-data.frame(patient.cbf.suv[-c(2)],Gender=factor)[,c(1,12,2:11)]

Gender<-as.factor(patient.cbf.mrglu$Gender)

factor <- model.matrix(patient.cbf.mrglu$Status ~ Gender)[,-1]  # necessary!

cbf.mrglu.all<-data.frame(patient.cbf.mrglu[-c(2)],Gender=factor)[,c(1,12,2:11)]



# Chapter 1: Penalized logistic model

# function: pl.cv:
# d:input dataset;
# k:k-fold;
# p:repeated p times
# print.detail: print the details or not for each set of tuning parameters: =1: yes

pl.cv<-function(d,k,p,print.detail)  
  
{
  
  glmGrid<-expand.grid(alpha=c(0,0.2,0.4,0.6,0.8, 1),lambda=seq(from=0,to=0.2,by=0.01))
  
  temp<-0
  lambda.best=glmGrid[1,]$lambda
  alpha.best=glmGrid[1,]$alpha
  
  acc.best.mean<-0;sen.best.mean<-0;spe.best.mean<-0;ppv.best.mean<-0;npv.best.mean<-0
  auc.best.sd<-0;acc.best.sd<-0;sen.best.sd<-0;spe.best.sd<-0;ppv.best.sd<-0;npv.best.sd<-0;rank.best<-0;
  
  for(j in 1:(nrow(glmGrid)))
  {
    
    lambda=glmGrid[j,]$lambda
    alpha=glmGrid[j,]$alpha
    index<-list()
    train.x<-list()
    train.y<-list()
    test.x<-list()
    test.y<-list()
    glmnetModel<-list()
    pl.coef<-list()
    pl.pred.class<-list()
    pl.pred.prob<-list()
    pl.true<-list()
    rocCurve<-list()
    auc<-list()
    con<-list()
    accuracy<-list()
    Sensitivity<-list()
    Specificity<-list()
    PPV<-list()
    NPV<-list()
    ave.auc=0;ave.acc=0;ave.sen=0;ave.spe=0;ave.ppv=0;ave.npv=0
    sd.auc<-0;sd.acc<-0;sd.sen<-0;sd.spe<-0;sd.ppv<-0;sd.npv<-0;
    auc.matrix<-matrix(, nrow = p, ncol = k)
    acc.matrix<-matrix(, nrow = p, ncol = k)
    sen.matrix<-matrix(, nrow = p, ncol = k)
    spe.matrix<-matrix(, nrow = p, ncol = k)
    ppv.matrix<-matrix(, nrow = p, ncol = k)
    npv.matrix<-matrix(, nrow = p, ncol = k)
    rank<-NULL
    ave.rank<-NULL
    var.list<-NULL
    r=1
    m=1
    
    while(m<=p)
      
    {
      
      for(i in 1:k)
      {
        set.seed(r)
        
        index[[i]]<-split(sample(1:nrow(d),nrow(d),replace = F),1:k)[[i]]; # left for validation each time: index[[i]]
        
        train.x[[i]]<-as.matrix(d[-index[[i]],2:(ncol(d)-1)]) #training set, input as matrix
        train.y[[i]]<-d[-index[[i]],ncol(d)] #training set, y
        
        test.x[[i]]<-as.matrix(d[index[[i]],2:(ncol(d)-1)]) #testing set, input as matrix
        test.y[[i]]<-d[index[[i]],ncol(d)] #testing set, y
        
        if(length(levels(factor(test.y[[i]])))==1)
        {
          r=r+1
          break
        }
        
        
        glmnetModel[[i]]<-glmnet(x=train.x[[i]],y=train.y[[i]],family="binomial",alpha=alpha)
        
        pl.coef[[i]]<-predict(glmnetModel[[i]], newx = test.x[[i]], s=lambda, type = "coef")
        
        pl.pred.class[[i]]<-predict(glmnetModel[[i]],newx=test.x[[i]],type="class",s=lambda)
        
        pl.pred.prob[[i]]<-predict(glmnetModel[[i]],newx=test.x[[i]],type="response",s=lambda)
        
        pl.true[[i]]<-test.y[[i]]
        
        con[[i]]<-confusionMatrix(data=pl.pred.class[[i]],reference=pl.true[[i]],positive="MS")
        
        accuracy[[i]]<-con[[i]]$overall["Accuracy"]
        Sensitivity[[i]]<-con[[i]]$byClass["Sensitivity"]
        Specificity[[i]]<-con[[i]]$byClass["Specificity"]
        PPV[[i]]<-con[[i]]$byClass["Pos Pred Value"]
        NPV[[i]]<-con[[i]]$byClass["Neg Pred Value"]
        
        
        #ROC and AUC
        
        rocCurve[[i]]<-pROC::roc(response=pl.true[[i]],predictor=as.numeric(pl.pred.prob[[i]]),levels=levels(pl.true[[i]]))
        
        auc[[i]]<-as.numeric(auc(rocCurve[[i]]))
        
        # print(auc[[i]]) 
        
        # sum.auc=sum.auc+auc[[i]]
        # sum.acc=sum.acc+accuracy[[i]]
        # sum.sen=sum.sen+Sensitivity[[i]]
        # sum.spe=sum.spe+Specificity[[i]]
        # sum.ppv=sum.ppv+PPV[[i]]
        # sum.npv=sum.npv+NPV[[i]]
        auc.matrix[m,i] <- auc[[i]]
        acc.matrix[m,i] <- accuracy[[i]]
        sen.matrix[m,i] <- Sensitivity[[i]]
        spe.matrix[m,i] <- Specificity[[i]]
        ppv.matrix[m,i] <- PPV[[i]]
        npv.matrix[m,i] <- NPV[[i]]
        
        rank<-rbind(rank,rank(-abs(pl.coef[[i]][-1])))
        var.list<-rownames(pl.coef[[i]])[-1]
        
      }
      r=r+1
      m=m+1
      
    }
    
    
    # ave.auc<-sum.auc/(k*p)
    # ave.acc<-sum.acc/(k*p)
    # ave.sen<-sum.sen/(k*p)
    # ave.spe<-sum.spe/(k*p)
    # ave.ppv<-sum.ppv/(k*p)
    # ave.npv<-sum.npv/(k*p)
    ave.auc<-mean(auc.matrix,na.rm=TRUE)
    ave.acc<-mean(acc.matrix,na.rm=TRUE)
    ave.sen<-mean(sen.matrix,na.rm=TRUE)
    ave.spe<-mean(spe.matrix,na.rm=TRUE)
    ave.ppv<-mean(ppv.matrix,na.rm=TRUE)
    ave.npv<-mean(npv.matrix,na.rm=TRUE)
    
    ave.rank<-colMeans(rank)
    
    sd.auc<-sd(auc.matrix,na.rm=TRUE)
    sd.acc<-sd(acc.matrix,na.rm=TRUE)
    sd.sen<-sd(sen.matrix,na.rm=TRUE)
    sd.spe<-sd(spe.matrix,na.rm=TRUE)
    sd.ppv<-sd(ppv.matrix,na.rm=TRUE)
    sd.npv<-sd(npv.matrix,na.rm=TRUE)
    
    if(print.detail==1) 
    {
      print(ave.auc)
      print(ave.acc)
      print(ave.sen)
      print(ave.spe)
      print(ave.ppv)
      print(ave.npv)
      print(ave.rank)
      print(var.list)
      print(r)
    }
    
    if(ave.auc>temp) 
    {
      temp<-ave.auc
      acc.best.mean<-ave.acc
      sen.best.mean<-ave.sen
      spe.best.mean<-ave.spe
      ppv.best.mean<-ave.ppv
      npv.best.mean<-ave.npv
      auc.best.sd<-sd.auc
      acc.best.sd<-sd.acc
      sen.best.sd<-sd.sen
      spe.best.sd<-sd.spe
      ppv.best.sd<-sd.ppv
      npv.best.sd<-sd.npv
      rank.best<-ave.rank
      lambda.best=lambda
      alpha.best=alpha
      var.best=var.list
    }
    
  }
  result<-list(auc.max=temp,lambda.best=lambda.best,alpha.best=alpha.best, 
               acc.best.mean=acc.best.mean,sen.best.mean=sen.best.mean,spe.best.mean=spe.best.mean,ppv.best.mean=ppv.best.mean,npv.best.mean=npv.best.mean,
               auc.best.sd=auc.best.sd,acc.best.sd=acc.best.sd,sen.best.sd=sen.best.sd,spe.best.sd=spe.best.sd,ppv.best.sd=ppv.best.sd,npv.best.sd=npv.best.sd, rank.best=rank.best,var.best=var.best)
  
  return(result)
  
}


pl.suv.all<-pl.cv(k=4,d=suv.all,p=10,print.detail=0)
pl.cbf.all<-pl.cv(k=4,d=cbf.all,p=10,print.detail=0)
pl.mrglu.all<-pl.cv(k=4,d=mrglu.all,p=10,print.detail=0)
pl.cbf.suv.all<-pl.cv(k=4,d=cbf.suv.all,p=10,print.detail=0)
pl.cbf.mrglu.all<-pl.cv(k=4,d=cbf.mrglu.all,p=10,print.detail=0) 

pl.suv.rank<-pl.suv.all$rank.best
pl.suv.var<-pl.suv.all$var.best
rank(pl.suv.rank)

pl.cbf.rank<-pl.cbf.all$rank.best
pl.cbf.var<-pl.cbf.all$var.best
rank(pl.cbf.rank)

pl.mrglu.rank<-pl.mrglu.all$rank.best
pl.mrglu.var<-pl.mrglu.all$var.best
rank(pl.mrglu.rank)

pl.suv.all[which(names(pl.suv.all) %in% c("rank.best","var.best"))] <- NULL
pl.cbf.all[which(names(pl.cbf.all) %in% c("rank.best","var.best"))] <- NULL
pl.mrglu.all[which(names(pl.mrglu.all) %in% c("rank.best","var.best"))] <- NULL
pl.cbf.suv.all[which(names(pl.cbf.suv.all) %in% c("rank.best","var.best"))] <- NULL
pl.cbf.mrglu.all[which(names(pl.cbf.mrglu.all) %in% c("rank.best","var.best"))] <- NULL

pl.suv.out<-data.frame(Measure="SUV",as.data.frame(pl.suv.all))
pl.cbf.out<-data.frame(Measure="CBF",as.data.frame(pl.cbf.all))
pl.mrglu.out<-data.frame(Measure="MRGlu",as.data.frame(pl.mrglu.all))
pl.cbf.suv.out<-data.frame(Measure="CBF+SUV",as.data.frame(pl.cbf.suv.all))
pl.cbf.mrglu.out<-data.frame(Measure="CBF+MRGlu",as.data.frame(pl.cbf.mrglu.all))

pl.out<-data.frame(Method="Penalized Logistic Regression",rbind(pl.suv.out,pl.cbf.out,pl.mrglu.out,pl.cbf.suv.out,pl.cbf.mrglu.out))

# Chapter 2: Random forest

# function: rf.cv:
# d:input dataset;
# k:k-fold;
# p:repeated p times
# print.auc: print the ave.auv or not for each set of tuning parameters: =1: yes


rf.cv<-function(d,k,p,print.detail)
  
{
  
  rfGrid <- expand.grid(mtry=c(1:10))
  temp<-0
  mtry.best=rfGrid[1,]
  
  acc.best.mean<-0;sen.best.mean<-0;spe.best.mean<-0;ppv.best.mean<-0;npv.best.mean<-0
  auc.best.sd<-0;acc.best.sd<-0;sen.best.sd<-0;spe.best.sd<-0;ppv.best.sd<-0;npv.best.sd<-0
  
  for(j in 1:(nrow(rfGrid)))
  {
    
    mtry=rfGrid[j,]
    index<-list()
    train.x<-list()
    train.y<-list()
    test.x<-list()
    test.y<-list()
    rf.model<-list()
    rf.pred.class<-list()
    rf.pred.prob<-list()
    rf.true<-list()
    rf.importance<-list()
    rocCurve<-list()
    auc<-list()
    con<-list()
    accuracy<-list()
    Sensitivity<-list()
    Specificity<-list()
    PPV<-list()
    NPV<-list()
    ave.auc=0;ave.acc=0;ave.sen=0;ave.spe=0;ave.ppv=0;ave.npv=0
    sd.auc<-0;sd.acc<-0;sd.sen<-0;sd.spe<-0;sd.ppv<-0;sd.npv<-0;
    auc.matrix<-matrix(, nrow = p, ncol = k)
    acc.matrix<-matrix(, nrow = p, ncol = k)
    sen.matrix<-matrix(, nrow = p, ncol = k)
    spe.matrix<-matrix(, nrow = p, ncol = k)
    ppv.matrix<-matrix(, nrow = p, ncol = k)
    npv.matrix<-matrix(, nrow = p, ncol = k)
    rank<-NULL
    ave.rank<-NULL
    var.list<-NULL
    r=1
    m=1
    
    while(m<=p)
      
    {
      
      for(i in 1:k)
      {
        set.seed(r)
        
        index[[i]]<-split(sample(1:nrow(d),nrow(d),replace = F),1:k)[[i]]; # left for validation each time: index[[i]]
        
        
        train.x[[i]]<-as.matrix(d[-index[[i]],2:(ncol(d)-1)]) #training set, input as matrix
        train.y[[i]]<-d[-index[[i]],ncol(d)] #training set, y
        
        test.x[[i]]<-as.matrix(d[index[[i]],2:(ncol(d)-1)]) #testing set, input as matrix
        test.y[[i]]<-d[index[[i]],ncol(d)] #testing set, y
        
        if(length(levels(factor(test.y[[i]])))==1)
        {
          r=r+1
          break
        }
        
        rf.model[[i]]<-randomForest(x=train.x[[i]],y=train.y[[i]],mtry=mtry)
        
        rf.pred.class[[i]]<-predict(rf.model[[i]],test.x[[i]],type="class")
        
        rf.pred.prob[[i]]<-predict(rf.model[[i]],test.x[[i]],type="response")
        
        rf.true[[i]]<-test.y[[i]]
        
        rf.importance[[i]]<-importance(rf.model[[i]],typr=1)
        
        con[[i]]<-confusionMatrix(data=rf.pred.class[[i]],reference=rf.true[[i]],positive="MS")
        
        accuracy[[i]]<-con[[i]]$overall["Accuracy"]
        Sensitivity[[i]]<-con[[i]]$byClass["Sensitivity"]
        Specificity[[i]]<-con[[i]]$byClass["Specificity"]
        PPV[[i]]<-con[[i]]$byClass["Pos Pred Value"]
        NPV[[i]]<-con[[i]]$byClass["Neg Pred Value"]
      
        
        
        #ROC and AUC
        
        rocCurve[[i]]<-pROC::roc(response=rf.true[[i]],predictor=as.numeric(rf.pred.prob[[i]]),levels=levels(rf.true[[i]]))
        
        auc[[i]]<-as.numeric(auc(rocCurve[[i]]))
        
        # print(auc[[i]]) 
        
        # sum.auc=sum.auc+auc[[i]]
        # sum.acc=sum.acc+accuracy[[i]]
        # sum.sen=sum.sen+Sensitivity[[i]]
        # sum.spe=sum.spe+Specificity[[i]]
        # sum.ppv=sum.ppv+PPV[[i]]
        # sum.npv=sum.npv+NPV[[i]]
        auc.matrix[m,i] <- auc[[i]]
        acc.matrix[m,i] <- accuracy[[i]]
        sen.matrix[m,i] <- Sensitivity[[i]]
        spe.matrix[m,i] <- Specificity[[i]]
        ppv.matrix[m,i] <- PPV[[i]]
        npv.matrix[m,i] <- NPV[[i]]
        
        rank<-rbind(rank,rank(-rf.importance[[i]]))
        var.list<-rownames(rf.importance[[i]])
        
        # print(PPV.matrix)
      }
      r=r+1
      m=m+1
      
    }
    
    
    # ave.auc<-sum.auc/(k*p)
    # ave.acc<-sum.acc/(k*p)
    # ave.sen<-sum.sen/(k*p)
    # ave.spe<-sum.spe/(k*p)
    # ave.ppv<-sum.ppv/(k*p)
    # ave.npv<-sum.npv/(k*p)
    ave.auc<-mean(auc.matrix,na.rm=TRUE)
    ave.acc<-mean(acc.matrix,na.rm=TRUE)
    ave.sen<-mean(sen.matrix,na.rm=TRUE)
    ave.spe<-mean(spe.matrix,na.rm=TRUE)
    ave.ppv<-mean(ppv.matrix,na.rm=TRUE)
    ave.npv<-mean(npv.matrix,na.rm=TRUE)
    ave.rank<-colMeans(rank)
    
    sd.auc<-sd(auc.matrix,na.rm=TRUE)
    sd.acc<-sd(acc.matrix,na.rm=TRUE)
    sd.sen<-sd(sen.matrix,na.rm=TRUE)
    sd.spe<-sd(spe.matrix,na.rm=TRUE)
    sd.ppv<-sd(ppv.matrix,na.rm=TRUE)
    sd.npv<-sd(npv.matrix,na.rm=TRUE)
    
    if(print.detail==1) 
    {
      print(ave.auc)
      print(ave.acc)
      print(ave.sen)
      print(ave.spe)
      print(ave.ppv)
      print(ave.npv)
      print(ave.rank)
      print(var.list)
      print(r)
    }
    
    
    if(ave.auc>temp) 
    {
      temp<-ave.auc
      acc.best.mean<-ave.acc
      sen.best.mean<-ave.sen
      spe.best.mean<-ave.spe
      ppv.best.mean<-ave.ppv
      npv.best.mean<-ave.npv
      auc.best.sd<-sd.auc
      acc.best.sd<-sd.acc
      sen.best.sd<-sd.sen
      spe.best.sd<-sd.spe
      ppv.best.sd<-sd.ppv
      npv.best.sd<-sd.npv
      mtry.best=mtry
      rank.best<-ave.rank
      var.best=var.list
    }
    
  }
  result<-list(auc.max=temp,mtry.best=mtry.best, 
               acc.best.mean=acc.best.mean,sen.best.mean=sen.best.mean,spe.best.mean=spe.best.mean,ppv.best.mean=ppv.best.mean,npv.best.mean=npv.best.mean,
               auc.best.sd=auc.best.sd,acc.best.sd=acc.best.sd,sen.best.sd=sen.best.sd,spe.best.sd=spe.best.sd,ppv.best.sd=ppv.best.sd,npv.best.sd=npv.best.sd,rank.best=rank.best,var.best=var.best)
  
  return(result)
  
  
}


rf.suv.all<-rf.cv(k=4,d=suv.all,p=10,print.detail=1)
rf.cbf.all<-rf.cv(k=4,d=cbf.all,p=10,print.detail=0)
rf.mrglu.all<-rf.cv(k=4,d=mrglu.all,p=10,print.detail=0)
rf.cbf.suv.all<-rf.cv(k=4,d=cbf.suv.all,p=10,print.detail=0)
rf.cbf.mrglu.all<-rf.cv(k=4,d=cbf.mrglu.all,p=10,print.detail=0) 

rf.suv.out<-data.frame(Measure="SUV",as.data.frame(rf.suv.all))
rf.cbf.out<-data.frame(Measure="CBF",as.data.frame(rf.cbf.all))
rf.mrglu.out<-data.frame(Measure="MRGlu",as.data.frame(rf.mrglu.all))
rf.cbf.suv.out<-data.frame(Measure="CBF+SUV",as.data.frame(rf.cbf.suv.all))
rf.cbf.mrglu.out<-data.frame(Measure="CBF+MRGlu",as.data.frame(rf.cbf.mrglu.all))

rf.out<-data.frame(Method="Random Forest",rbind(rf.suv.out,rf.cbf.out,rf.mrglu.out,rf.cbf.suv.out,rf.cbf.mrglu.out))

pl.out1<-pl.out[,c(1,2,3,11,6,12,7,13,8,14,9,15,10,16,4,5)]
rf.out1<-rf.out[,c(1,2,3,10,5,11,6,12,7,13,8,14,9,15,4)]

write.csv(pl.out1, file="./memo/pl.out.csv")
write.csv(rf.out1, file="./memo/rf.out.csv")

save.image(file="./code/20160914.RData")
