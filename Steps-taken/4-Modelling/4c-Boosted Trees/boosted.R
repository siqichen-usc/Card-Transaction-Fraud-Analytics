library(gbm)
library(dplyr)

data<-read.csv('0325_vars_final_zscale.csv')
oot<-read.csv('0325_oot_final_zscale.csv')

data$Fraud=as.character(data$Fraud)
oot$Fraud=as.character(oot$Fraud)
trainIndex = createDataPartition(data$Fraud,
                                 p=0.75, list=FALSE,times=1)

train =data[trainIndex,]
test =data[-trainIndex,]



lg.reg<-glm(Fraud~.-Recnum,data=train,family=binomial)
lg.pre<-predict(lg.reg,test,type='response')
#timeend<-Sys.time()
#rank test based on lg.res, calculate 30% fraud
summary(lg.reg)
#runningtime<-timeend-timestart
#print(runningtime) 
##train FDR
train_res<-data.frame(Recnum=train$Recnum,pro=lg.reg$fitted.values,Fraud=train$Fraud)
num=floor(0.03*nrow(train_res))#2142
train_res<-train_res[order(-train_res$pro),][0:num,]
sum(train_res$Fraud)/nrow(train_res)#0.1990471-train
##test FDR
test_res<-data.frame(Recnum=test$Recnum,pro=lg.pre,Fraud=test$Fraud)
num=floor(0.03*nrow(test_res))
test_res<-test_res[order(-test_res$pro),][0:num,]
sum(test_res$Fraud)/nrow(test_res)#0.1875994-test
##oot
lg.pre1<-predict(lg.reg,oot,type='response')
oot_res<-data.frame(Recnum=oot$Recnum,pro=lg.pre1,Fraud=oot$Fraud)
num=floor(0.03*nrow(oot_res))
oot_res<-oot_res[order(-oot_res$pro),][0:num,]
sum(oot_res$Fraud)/nrow(oot_res)#0.1155914-oot




gra_boost<- gbm(formula = Fraud~ .-Recnum,
                distribution ='bernoulli', 
                data =train,
                n.trees=1000) 

ntree_opt<- gbm.perf(object =gra_boost, 
                     method = 'OOB', 
                     oobag.curve = TRUE)
gra_boost<- gbm(formula = Fraud~ .-Recnum, 
                distribution ='bernoulli', 
                data =train,
                n.trees =ntree_opt)

gra_boost<- gbm(formula = Fraud~ .-Recnum,
                distribution ='bernoulli', 
                data =train,
                n.trees=500,                # number of trees
                shrinkage=0.001,              # shrinkage or learning rate,
                # 0.001 to 0.1 usually work
                interaction.depth=5) # 1: additive model, 2: two-way interactions, etc.
                #n.minobsinnode = 10)         # minimum total weight needed in each node

gb_pre_train<- predict(object =gra_boost, 
                       newdata =train,type='response',
                       n.trees=500,                # number of trees
                       shrinkage=0.001,              # shrinkage or learning rate,
                       # 0.001 to 0.1 usually work
                       interaction.depth=5)# 1: additive model, 2: two-way interactions, etc.
#n.minobsinnode = 10)         # minimum total weight needed in each node

gb_pre_test<- predict(object =gra_boost, 
                      newdata =test,type='response',
                      n.trees=500,                # number of trees
                      shrinkage=0.001,              # shrinkage or learning rate,
                      # 0.001 to 0.1 usually work
                      interaction.depth=5)
#,         # 1: additive model, 2: two-way interactions, etc.
#n.minobsinnode = 10)         # minimum total weight needed in each node

gb_pre_oot<- predict(object =gra_boost, 
                     newdata =oot,type='response',
                     n.trees=500,                # number of trees
                     shrinkage=0.001,              # shrinkage or learning rate,
                     # 0.001 to 0.1 usually work
                     interaction.depth=5)# 1: additive model, 2: two-way interactions, etc.
#n.minobsinnode = 10)         # minimum total weight needed in each node

#train FDR
train_res_bt<-data.frame(Recnum=train$Recnum,pro=gb_pre_train,Fraud=as.numeric(train$Fraud))
num=floor(0.03*nrow(train_res_bt))#1889
train_res_bt<-train_res_bt[order(-train_res_bt$pro),][0:num,]
sum(train_res_bt$Fraud)/nrow(train_res_bt)#0.2435151
#test FDR
test_res_bt<-data.frame(Recnum=test$Recnum,pro=gb_pre_test,Fraud=as.numeric(test$Fraud))
num=floor(0.03*nrow(test_res_bt))#629
test_res_bt<-test_res_bt[order(-test_res_bt$pro),][0:num,]

sum(test_res_bt$Fraud)/nrow(test_res_bt)#0.2448331
#oot FDR
oot_res_bt<-data.frame(Recnum=oot$Recnum,pro=gb_pre_oot,Fraud=as.numeric(oot$Fraud))
num=floor(0.03*nrow(oot_res_bt))#372
oot_res_bt<-oot_res_bt[order(-oot_res_bt$pro),][0:num,]

sum(oot_res_bt$Fraud)/nrow(oot_res_bt)#0.1478495

