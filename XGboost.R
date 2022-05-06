library(xgboost)
library(tidyverse)
library(dplyr)
library(pROC)
library(RSNNS)
library(caret)
set.seed(1)

# 3-class
#----------
## simple classification
simple_class<- function(data){
  ## partition data 
  train_index=createDataPartition(y=data$popularity_class,p=0.8,list=F)
  train_index2=sample(train_index,round(0.7*length(data$popularity_class)))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(data[train_index2,])
  validation_data=na.omit(data[valid_index,])
  test_data=na.omit(data[-train_index,])
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity_class)-1
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity_class)-1
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity_class)-1
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 num_class=3,
                                 objective="multi:softprob", 
                                 eval_metric="mlogloss",
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  hitsong_xgb_model
  
  ## predict results for test data
  testpred_prob=predict(hitsong_xgb_model,newdata=dtest)
  testpred_prob2=as.data.frame(matrix(testpred_prob,ncol=3,byrow=TRUE))
  colnames(testpred_prob2)=c("[0,30]","(30,60]","(60,100]")
  
  testpred_prob3=testpred_prob2
  testpred_prob3$label=encodeClassLabels(testpred_prob3,method="WTA",l=0,h=0)
  testpred_prob3$label=colnames(testpred_prob2)[testpred_prob3$label]
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=testpred_prob2)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=testpred_prob2)
  
  list(result1,result2)
}

#---------
## classification with oversampling of weight1
oversample_class1<- function(data){
  train_index=createDataPartition(y=data$popularity_class,p=0.8,list=F)
  train_index2=sample(train_index,round(0.7*length(data$popularity_class)))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(data[train_index2,])
  validation_data=na.omit(data[valid_index,])
  test_data=na.omit(data[-train_index,])
  ## balance data
  oversample_label=createDataPartition_my_version1(y=train_data$popularity,list=F)
  train_data_new=rbind(train_data,train_data[oversample_label,])
  train_data=train_data_new
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity_class)-1
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity_class)-1
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity_class)-1
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 num_class=3,
                                 objective="multi:softprob", 
                                 eval_metric="mlogloss",
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  hitsong_xgb_model
  
  ## predict results for test data
  testpred_prob=predict(hitsong_xgb_model,newdata=dtest)
  testpred_prob2=as.data.frame(matrix(testpred_prob,ncol=3,byrow=TRUE))
  colnames(testpred_prob2)=c("[0,30]","(30,60]","(60,100]")
  
  testpred_prob3=testpred_prob2
  testpred_prob3$label=encodeClassLabels(testpred_prob3,method="WTA",l=0,h=0)
  testpred_prob3$label=colnames(testpred_prob2)[testpred_prob3$label]
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=testpred_prob3$label)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=testpred_prob3$label)
  
  list(result1,result2)
}

#---------
## classification with oversampling of weight1
oversample_class2<- function(data){
  train_index=createDataPartition(y=data$popularity_class,p=0.8,list=F)
  train_index2=sample(train_index,round(0.7*length(data$popularity_class)))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(data[train_index2,])
  validation_data=na.omit(data[valid_index,])
  test_data=na.omit(data[-train_index,])
  ## balance data
  oversample_label=createDataPartition_my_version2(y=train_data$popularity,list=F)
  train_data_new=rbind(train_data,train_data[oversample_label,])
  train_data=train_data_new
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity_class)-1
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity_class)-1
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity_class)-1
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 num_class=3,
                                 objective="multi:softprob", 
                                 eval_metric="mlogloss",
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  hitsong_xgb_model
  
  ## predict results for test data
  testpred_prob=predict(hitsong_xgb_model,newdata=dtest)
  testpred_prob2=as.data.frame(matrix(testpred_prob,ncol=3,byrow=TRUE))
  colnames(testpred_prob2)=c("[0,30]","(30,60]","(60,100]")
  
  testpred_prob3=testpred_prob2
  testpred_prob3$label=encodeClassLabels(testpred_prob3,method="WTA",l=0,h=0)
  testpred_prob3$label=colnames(testpred_prob2)[testpred_prob3$label]
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=testpred_prob3$label)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=testpred_prob3$label)
  
  list(result1,result2)
}





# 4 class
#-------------
## simple classification
simple_class2<- function(data){
  ## partition data 
  train_index=createDataPartition(y=data$popularity_class,p=0.8,list=F)
  train_index2=sample(train_index,round(0.7*length(data$popularity_class)))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(data[train_index2,])
  validation_data=na.omit(data[valid_index,])
  test_data=na.omit(data[-train_index,])
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity_class)-1
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity_class)-1
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity_class)-1
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 num_class=4,
                                 objective="multi:softprob", 
                                 eval_metric="mlogloss",
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  hitsong_xgb_model
  
  ## predict results for test data
  testpred_prob=predict(hitsong_xgb_model,newdata=dtest)
  testpred_prob2=as.data.frame(matrix(testpred_prob,ncol=4,byrow=TRUE))
  colnames(testpred_prob2)=c("[0,30]","(30,46]","(46,60]","(60,100]")
  
  testpred_prob3=testpred_prob2
  testpred_prob3$label=encodeClassLabels(testpred_prob3,method="WTA",l=0,h=0)
  testpred_prob3$label=colnames(testpred_prob2)[testpred_prob3$label]
  
  predict=testpred_prob3$label
  predict[predict=="(30,46]"]="(30,60]"
  predict[predict=="(46,60]"]="(30,60]"
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=predict)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=predict)
  
  list(result1,result2)
}

#----------------
## classification with 3-oversampling of weight1
oversample_class3<- function(data){
  train_index=createDataPartition(y=data$popularity_class,p=0.8,list=F)
  train_index2=sample(train_index,round(0.7*length(data$popularity_class)))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(data[train_index2,])
  validation_data=na.omit(data[valid_index,])
  test_data=na.omit(data[-train_index,])
  ## balance data
  oversample_label=createDataPartition_my_version3(y=train_data$popularity,list=F)
  train_data_new=rbind(train_data,train_data[oversample_label,])
  train_data=train_data_new
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity_class)-1
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity_class)-1
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity_class)-1
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 num_class=4,
                                 objective="multi:softprob", 
                                 eval_metric="mlogloss",
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  hitsong_xgb_model
  
  ## predict results for test data
  testpred_prob=predict(hitsong_xgb_model,newdata=dtest)
  testpred_prob2=as.data.frame(matrix(testpred_prob,ncol=4,byrow=TRUE))
  colnames(testpred_prob2)=c("[0,30]","(30,46]","(46,60]","(60,100]")
  
  testpred_prob3=testpred_prob2
  testpred_prob3$label=encodeClassLabels(testpred_prob3,method="WTA",l=0,h=0)
  testpred_prob3$label=colnames(testpred_prob2)[testpred_prob3$label]
  
  predict=testpred_prob3$label
  predict[predict=="(30,46]"]="(30,60]"
  predict[predict=="(46,60]"]="(30,60]"
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=predict)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=predict)
  
  list(result1,result2)
}


#----------------
## classification with 3-oversampling of weight2
oversample_class4<- function(data){
  train_index=createDataPartition(y=data$popularity_class,p=0.8,list=F)
  train_index2=sample(train_index,round(0.7*length(data$popularity_class)))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(data[train_index2,])
  validation_data=na.omit(data[valid_index,])
  test_data=na.omit(data[-train_index,])
  ## balance data
  oversample_label=createDataPartition_my_version4(y=train_data$popularity,list=F)
  train_data_new=rbind(train_data,train_data[oversample_label,])
  train_data=train_data_new
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity_class)-1
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity_class)-1
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity_class)-1
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 num_class=4,
                                 objective="multi:softprob", 
                                 eval_metric="mlogloss",
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  hitsong_xgb_model
  
  ## predict results for test data
  testpred_prob=predict(hitsong_xgb_model,newdata=dtest)
  testpred_prob2=as.data.frame(matrix(testpred_prob,ncol=4,byrow=TRUE))
  colnames(testpred_prob2)=c("[0,30]","(30,46]","(46,60]","(60,100]")
  
  testpred_prob3=testpred_prob2
  testpred_prob3$label=encodeClassLabels(testpred_prob3,method="WTA",l=0,h=0)
  testpred_prob3$label=colnames(testpred_prob2)[testpred_prob3$label]
  
  predict=testpred_prob3$label
  predict[predict=="(30,46]"]="(30,60]"
  predict[predict=="(46,60]"]="(30,60]"
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=predict)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=predict)
  
  list(result1,result2)
}


#---------------
## classification with 3-oversampling of weight1
oversample_class5<- function(data){
  train_index=createDataPartition(y=data$popularity_class,p=0.8,list=F)
  train_index2=sample(train_index,round(0.7*length(data$popularity_class)))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(data[train_index2,])
  validation_data=na.omit(data[valid_index,])
  test_data=na.omit(data[-train_index,])
  ## balance data
  oversample_label=createDataPartition_my_version1(y=train_data$popularity,list=F)
  train_data_new=rbind(train_data,train_data[oversample_label,])
  train_data=train_data_new
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity_class)-1
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity_class)-1
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity_class)-1
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 num_class=4,
                                 objective="multi:softprob", 
                                 eval_metric="mlogloss",
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  hitsong_xgb_model
  
  ## predict results for test data
  testpred_prob=predict(hitsong_xgb_model,newdata=dtest)
  testpred_prob2=as.data.frame(matrix(testpred_prob,ncol=4,byrow=TRUE))
  colnames(testpred_prob2)=c("[0,30]","(30,46]","(46,60]","(60,100]")
  
  testpred_prob3=testpred_prob2
  testpred_prob3$label=encodeClassLabels(testpred_prob3,method="WTA",l=0,h=0)
  testpred_prob3$label=colnames(testpred_prob2)[testpred_prob3$label]
  
  predict=testpred_prob3$label
  predict[predict=="(30,46]"]="(30,60]"
  predict[predict=="(46,60]"]="(30,60]"
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=predict)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=predict)
  
  list(result1,result2)
}

#-------------
## classification with 3-oversampling of weight2
oversample_class6<- function(data){
  train_index=createDataPartition(y=data$popularity_class,p=0.8,list=F)
  train_index2=sample(train_index,round(0.7*length(data$popularity_class)))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(data[train_index2,])
  validation_data=na.omit(data[valid_index,])
  test_data=na.omit(data[-train_index,])
  ## balance data
  oversample_label=createDataPartition_my_version2(y=train_data$popularity,list=F)
  train_data_new=rbind(train_data,train_data[oversample_label,])
  train_data=train_data_new
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity_class)-1
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity_class)-1
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity_class)-1
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 num_class=4,
                                 objective="multi:softprob", 
                                 eval_metric="mlogloss",
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  hitsong_xgb_model
  
  ## predict results for test data
  testpred_prob=predict(hitsong_xgb_model,newdata=dtest)
  testpred_prob2=as.data.frame(matrix(testpred_prob,ncol=4,byrow=TRUE))
  colnames(testpred_prob2)=c("[0,30]","(30,46]","(46,60]","(60,100]")
  
  testpred_prob3=testpred_prob2
  testpred_prob3$label=encodeClassLabels(testpred_prob3,method="WTA",l=0,h=0)
  testpred_prob3$label=colnames(testpred_prob2)[testpred_prob3$label]
  
  predict=testpred_prob3$label
  predict[predict=="(30,46]"]="(30,60]"
  predict[predict=="(46,60]"]="(30,60]"
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=predict)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=predict)
  
  list(result1,result2)
}

#--------
# classification with Regression of weight1
oversample_class7<- function(data){
  train_index=createDataPartition(y=data$popularity_class,p=0.8,list=F)
  train_index2=sample(train_index,round(0.7*length(data$popularity_class)))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(data[train_index2,])
  validation_data=na.omit(data[valid_index,])
  test_data=na.omit(data[-train_index,])
  ## balance data
  oversample_label=createDataPartition_my_version1(y=train_data$popularity,list=F)
  train_data_new=rbind(train_data,train_data[oversample_label,])
  train_data=train_data_new
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity)
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity)
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity)
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 objective="reg:squarederror", 
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  vd_xgb <- xgb.importance(model=hitsong_xgb_model)
  
  
  ## predict results for test data
  testpred=predict(hitsong_xgb_model,newdata=dtest)
  testpred_label=cut(testpred,breaks=c(0,30,60,100),include.lowest=TRUE)
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=testpred_label)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=testpred_label)
  
  result3=list(Feature=vd_xgb$Feature,Gain=vd_xgb$Gain)
  
  list(result1,result2,result3)
}

# classification with Regression of weight2
oversample_class8<- function(data){
  train_index=createDataPartition(y=data$popularity_class,p=0.8,list=F)
  train_index2=sample(train_index,round(0.7*length(data$popularity_class)))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(data[train_index2,])
  validation_data=na.omit(data[valid_index,])
  test_data=na.omit(data[-train_index,])
  ## balance data
  oversample_label=createDataPartition_my_version2(y=train_data$popularity,list=F)
  train_data_new=rbind(train_data,train_data[oversample_label,])
  train_data=train_data_new
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity)
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity)
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity)
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 objective="reg:squarederror", 
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  vd_xgb <- xgb.importance(model=hitsong_xgb_model)
  
  
  ## predict results for test data
  testpred=predict(hitsong_xgb_model,newdata=dtest)
  testpred_label=cut(testpred,breaks=c(0,30,60,100),include.lowest=TRUE)
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=testpred_label)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=testpred_label)
  
  result3=list(Feature=vd_xgb$Feature,Gain=vd_xgb$Gain)
  
  list(result1,result2,result3)
}




#-------
oversample_class_test1<- function(training_data,data){
  train_index=1:dim(training_data)[1]
  train_index2=sample(train_index,0.8*length(train_index))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(training_data[train_index2,])
  validation_data=na.omit(training_data[valid_index,])
  test_data=testing_data
  ## balance data
  oversample_label=createDataPartition_my_version1(y=train_data$popularity,list=F)
  train_data_new=rbind(train_data,train_data[oversample_label,])
  train_data=train_data_new
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity_class)-1
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity_class)-1
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity_class)-1
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 num_class=4,
                                 objective="multi:softprob", 
                                 eval_metric="mlogloss",
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  vd_xgb <- xgb.importance(model=hitsong_xgb_model)
  
  ## predict results for test data
  testpred_prob=predict(hitsong_xgb_model,newdata=dtest)
  testpred_prob2=as.data.frame(matrix(testpred_prob,ncol=4,byrow=TRUE))
  colnames(testpred_prob2)=c("[0,30]","(30,46]","(46,60]","(60,100]")
  
  testpred_prob3=testpred_prob2
  testpred_prob3$label=encodeClassLabels(testpred_prob3,method="WTA",l=0,h=0)
  testpred_prob3$label=colnames(testpred_prob2)[testpred_prob3$label]
  
  predict=testpred_prob3$label
  predict[predict=="(30,46]"]="(30,60]"
  predict[predict=="(46,60]"]="(30,60]"
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=predict)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=predict)
  
  result3=list(Feature=vd_xgb$Feature,Gain=vd_xgb$Gain)
  
  list(result1,result2,result3)
}

oversample_class_test2<- function(training_data,testing_data){
  train_index=1:dim(training_data)[1]
  train_index2=sample(train_index,0.8*length(train_index))
  valid_index=setdiff(train_index,train_index2)
  train_data=na.omit(training_data[train_index2,])
  validation_data=na.omit(training_data[valid_index,])
  test_data=testing_data
  ## balance data
  oversample_label=createDataPartition_my_version3(y=train_data$popularity,list=F)
  train_data_new=rbind(train_data,train_data[oversample_label,])
  train_data=train_data_new
  ## prepare data
  dv=dummyVars(~., data=train_data%>%select(-1,-popularity_class,-popularity),fullRank = T)
  data_trainx=predict(dv,newdata=train_data%>%select(-1,-popularity_class,-popularity))
  data_trainy=as.numeric(train_data$popularity_class)-1
  data_validationx=predict(dv,newdata=validation_data%>%select(-1,-popularity_class,-popularity))
  data_validationy=as.numeric(validation_data$popularity_class)-1
  data_testx=predict(dv,newdata=test_data%>%select(-1,-popularity_class,-popularity))
  data_testy=as.numeric(test_data$popularity_class)-1
  
  dtrain=xgb.DMatrix(data=data_trainx,label=data_trainy)
  dvalid=xgb.DMatrix(data=data_validationx,label=data_validationy)
  dtest=xgb.DMatrix(data=data_testx,label=data_testy)
  
  watchlist=list(train=dtrain,test=dvalid)
  
  ## train the model
  hitsong_xgb_model <- xgb.train(data= dtrain,
                                 eta=0.3, # learning rate
                                 gamma=0.001, # large gamma means low splitting times
                                 max_depth=2, # weak classifiers
                                 subsample=0.7, # just like bagging-- sample 70% when splitting the tree
                                 colsample_bytree=0.4, # just like rqndom forest(control correlation)--sample 40% variables when splitting the tree
                                 
                                 num_class=4,
                                 objective="multi:softprob", 
                                 eval_metric="mlogloss",
                                 
                                 nrounds = 2000,
                                 watchlist=watchlist,
                                 verbose=0, # show modelling or not
                                 print_every_n = 100,
                                 early_stopping_rounds = 200  # if in more 200 rounds the precision on validation set is not good, then stop
  )
  
  vd_xgb <- xgb.importance(model=hitsong_xgb_model)
  
  ## predict results for test data
  testpred_prob=predict(hitsong_xgb_model,newdata=dtest)
  testpred_prob2=as.data.frame(matrix(testpred_prob,ncol=4,byrow=TRUE))
  colnames(testpred_prob2)=c("[0,30]","(30,46]","(46,60]","(60,100]")
  
  testpred_prob3=testpred_prob2
  testpred_prob3$label=encodeClassLabels(testpred_prob3,method="WTA",l=0,h=0)
  testpred_prob3$label=colnames(testpred_prob2)[testpred_prob3$label]
  
  predict=testpred_prob3$label
  predict[predict=="(30,46]"]="(30,60]"
  predict[predict=="(46,60]"]="(30,60]"
  
  result1=list(response=cut(test_data$popularity,breaks=c(0,30,60,100),include.lowest=TRUE),predictor=predict)
  
  result2=list(response=cut(test_data$popularity,breaks=c(0,20,30,45,60,70,100),include.lowest=TRUE),predictor=predict)
  
  result3=list(Feature=vd_xgb$Feature,Gain=vd_xgb$Gain)
  
  list(result1,result2,result3)
}
