source('oversampling methods.R', echo=TRUE)
source('XGboost.R', echo=TRUE)

# comparisons of each oversampling method

## simple classification without oversampling
set.seed(1)
reference=factor(c(),levels=c("[0,30]","(30,60]","(60,100]"))
pred=c()
for(i in 1:10){
  data=full_join(full_join(data_70s,data_80s),data_90s)%>%na.omit()%>%mutate(popularity_class=cut(popularity,breaks=c(0,30,60,100),include.lowest=TRUE))%>%mutate(covering_density1=X1,covering_density2=X2,covering_density3=X3,covering_density4=X4)%>%select(-X1,-X2,-X3,-X4)
  result=simple_class(data)
  reference=c(reference,result[[1]]$response)
  pred=rbind(pred,result[[1]]$predictor)
}
reference=c("[0,30]","(30,60]","(60,100]")[reference]
testpred_label=encodeClassLabels(pred,method="WTA",l=0,h=0)
testpred_label=c("[0,30]","(30,60]","(60,100]")[testpred_label]
indicator=confusionMatrix(data=factor(testpred_label,levels=c("[0,30]","(30,60]","(60,100]")),reference=factor(reference,levels=c("[0,30]","(30,60]","(60,100]")),mode="everything")

## classification with oversampling, weight1, learn 3 classes
set.seed(1)
pred=c()
reference=c()
pred2=c()
reference2=c()
for(i in 1:10){
  result=oversample_class1(data)
  reference=c(reference,result[[1]]$response)
  pred=c(pred,result[[1]]$predictor)
  reference2=c(reference2,result[[2]]$response)
  pred2=c(pred2,result[[2]]$predictor)
}
reference=c("[0,30]","(30,60]","(60,100]")[reference]
reference2=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")[reference2]
indicator=confusionMatrix(data=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reference=factor(reference,levels=c("[0,30]","(30,60]","(60,100]")),mode="everything")
indicator$table/10
tab=data.frame(predict=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reality=factor(reference2,levels=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")))
t1=xtabs(~predict+reality,tab)/10
Adjusted_Precision=c(sum(t1[1,1:3])/sum(t1[1,1:6]),sum(t1[2,2:5])/sum(t1[2,1:6]),sum(t1[3,4:6])/sum(t1[3,1:6]))
Extreme_Error=c(sum(t1[1,5:6])/sum(t1[1,1:6]),NA,sum(t1[3,1:2])/sum(t1[3,1:6]))
Extreme_Sensitivity=c(sum(t1[1,1])/sum(t1[1:3,1]),NA,sum(t1[3,6])/sum(t1[1:3,6]))
t1=round(rbind(t(indicator$byClass[,c(11,1)]),"Extreme Sensitivity"=Extreme_Sensitivity,"Precision"=indicator$byClass[,c(5)],"Adjusted Precision"=Adjusted_Precision,"Extreme Error"=Extreme_Error,"Specificty"=indicator$byClass[,c(2)]),digits=3)
t1=as.data.frame(cbind("evaluation measures"=c("Balanced Accuracy","Sensitivity",  "Extreme Sensitivity","Precision","Adjusted Precision","Extreme Error","Specificty"),t1))
t2=apply(matrix(as.numeric(as.matrix(t1[,2:4])),ncol=3),1,mean,na.rm=TRUE)
t2

f1=qflextable(t1)
f1=bold(f1,i=1:7,j=1)
f1=bold(f1,j=1:4,part="header")
f1=bg(f1,i=c(3,5,6),j=1:4,bg="skyblue")
f1=color(f1,i=c(3,5,6),j=2:4,"white")
f1

## classification with oversampling, weight2, learn 3 classes
set.seed(1)
pred=c()
reference=c()
pred2=c()
reference2=c()
for(i in 1:10){
  result=oversample_class2(data)
  reference=c(reference,result[[1]]$response)
  pred=c(pred,result[[1]]$predictor)
  reference2=c(reference2,result[[2]]$response)
  pred2=c(pred2,result[[2]]$predictor)
}
reference=c("[0,30]","(30,60]","(60,100]")[reference]
reference2=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")[reference2]
indicator=confusionMatrix(data=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reference=factor(reference,levels=c("[0,30]","(30,60]","(60,100]")),mode="everything")
indicator$table/10
tab=data.frame(predict=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reality=factor(reference2,levels=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")))
t1=xtabs(~predict+reality,tab)/10
Adjusted_Precision=c(sum(t1[1,1:3])/sum(t1[1,1:6]),sum(t1[2,2:5])/sum(t1[2,1:6]),sum(t1[3,4:6])/sum(t1[3,1:6]))
Extreme_Error=c(sum(t1[1,5:6])/sum(t1[1,1:6]),NA,sum(t1[3,1:2])/sum(t1[3,1:6]))
Extreme_Sensitivity=c(sum(t1[1,1])/sum(t1[1:3,1]),NA,sum(t1[3,6])/sum(t1[1:3,6]))
t1=round(rbind(t(indicator$byClass[,c(11,1)]),"Extreme Sensitivity"=Extreme_Sensitivity,"Precision"=indicator$byClass[,c(5)],"Adjusted Precision"=Adjusted_Precision,"Extreme Error"=Extreme_Error,"Specificty"=indicator$byClass[,c(2)]),digits=3)
t1=as.data.frame(cbind("evaluation measures"=c("Balanced Accuracy","Sensitivity",  "Extreme Sensitivity","Precision","Adjusted Precision","Extreme Error","Specificty"),t1))
t2=apply(matrix(as.numeric(as.matrix(t1[,2:4])),ncol=3),1,mean,na.rm=TRUE)
t2

f1=qflextable(t1)
f1=bold(f1,i=1:7,j=1)
f1=bold(f1,j=1:4,part="header")
f1=bg(f1,i=c(3,5,6),j=1:4,bg="skyblue")
f1=color(f1,i=c(3,5,6),j=2:4,"white")
f1

## classification with oversampling, weight1, learn 4 balanced classes
set.seed(1)
pred=c()
reference=c()
pred2=c()
reference2=c()
for(i in 1:10){
  data2=data%>%mutate(popularity_class=cut(popularity,breaks=c(0,30,46,60,100),include.lowest=TRUE))
  result=oversample_class3(data2)
  reference=c(reference,result[[1]]$response)
  pred=c(pred,result[[1]]$predictor)
  reference2=c(reference2,result[[2]]$response)
  pred2=c(pred2,result[[2]]$predictor)
}
reference=c("[0,30]","(30,60]","(60,100]")[reference]
reference2=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")[reference2]
indicator=confusionMatrix(data=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reference=factor(reference,levels=c("[0,30]","(30,60]","(60,100]")),mode="everything")
indicator$table/10
tab=data.frame(predict=factor(pred2,levels=c("[0,30]","(30,60]","(60,100]")),reality=factor(reference2,levels=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")))
t1=xtabs(~predict+reality,tab)/10
t1
Adjusted_Precision=c(sum(t1[1,1:3])/sum(t1[1,1:6]),NA,sum(t1[3,4:6])/sum(t1[3,1:6]))
Extreme_Error=c(sum(t1[1,5:6])/sum(t1[1,1:6]),NA,sum(t1[3,1:2])/sum(t1[3,1:6]))
Extreme_Sensitivity=c(sum(t1[1,1])/sum(t1[1:3,1]),NA,sum(t1[3,6])/sum(t1[1:3,6]))
t1=round(rbind(t(indicator$byClass[,c(11,1)]),"Extreme Sensitivity"=Extreme_Sensitivity,"Precision"=indicator$byClass[,c(5)],"Adjusted Precision"=Adjusted_Precision,"Extreme Error"=Extreme_Error,"Specificty"=indicator$byClass[,c(2)]),digits=3)
t1=as.data.frame(cbind("evaluation measures"=c("Balanced Accuracy","Sensitivity",  "Extreme Sensitivity","Precision","Adjusted Precision","Extreme Error","Specificty"),t1))
t2=apply(matrix(as.numeric(as.matrix(t1[,2:4])),ncol=3),1,mean,na.rm=TRUE)
t2
f1=qflextable(t1)
f1=bold(f1,i=1:7,j=1)
f1=bold(f1,j=1:4,part="header")
f1=bg(f1,i=c(3,5,6),j=1:4,bg="skyblue")
f1=color(f1,i=c(3,5,6),j=2:4,"white")
f1

## classification with oversampling, weight2, learn 4 balanced classes
set.seed(1)
pred=c()
reference=c()
pred2=c()
reference2=c()
for(i in 1:10){
  data2=data%>%mutate(popularity_class=cut(popularity,breaks=c(0,30,46,60,100),include.lowest=TRUE))
  result=oversample_class4(data2)
  reference=c(reference,result[[1]]$response)
  pred=c(pred,result[[1]]$predictor)
  reference2=c(reference2,result[[2]]$response)
  pred2=c(pred2,result[[2]]$predictor)
}
reference=c("[0,30]","(30,60]","(60,100]")[reference]
reference2=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")[reference2]
indicator=confusionMatrix(data=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reference=factor(reference,levels=c("[0,30]","(30,60]","(60,100]")),mode="everything")
indicator$table/10
tab=data.frame(predict=factor(pred2,levels=c("[0,30]","(30,60]","(60,100]")),reality=factor(reference2,levels=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")))
t1=xtabs(~predict+reality,tab)/10
t1
Adjusted_Precision=c(sum(t1[1,1:3])/sum(t1[1,1:6]),NA,sum(t1[3,4:6])/sum(t1[3,1:6]))
Extreme_Error=c(sum(t1[1,5:6])/sum(t1[1,1:6]),NA,sum(t1[3,1:2])/sum(t1[3,1:6]))
Extreme_Sensitivity=c(sum(t1[1,1])/sum(t1[1:3,1]),NA,sum(t1[3,6])/sum(t1[1:3,6]))
t1=round(rbind(t(indicator$byClass[,c(11,1)]),"Extreme Sensitivity"=Extreme_Sensitivity,"Precision"=indicator$byClass[,c(5)],"Adjusted Precision"=Adjusted_Precision,"Extreme Error"=Extreme_Error,"Specificty"=indicator$byClass[,c(2)]),digits=3)
t1=as.data.frame(cbind("evaluation measures"=c("Balanced Accuracy","Sensitivity",  "Extreme Sensitivity","Precision","Adjusted Precision","Extreme Error","Specificty"),t1))
t2=apply(matrix(as.numeric(as.matrix(t1[,2:4])),ncol=3),1,mean,na.rm=TRUE)
t2

f1=qflextable(t1)
f1=bold(f1,i=1:7,j=1)
f1=bold(f1,j=1:4,part="header")
f1=bg(f1,i=c(3,5,6),j=1:4,bg="skyblue")
f1=color(f1,i=c(3,5,6),j=2:4,"white")
f1

## classification with oversampling, weight1, learn 4 adjusted classes
set.seed(1)
pred=c()
reference=c()
pred2=c()
reference2=c()
for(i in 1:10){
  data2=data%>%mutate(popularity_class=cut(popularity,breaks=c(0,30,46,60,100),include.lowest=TRUE))
  result=oversample_class5(data2)
  reference=c(reference,result[[1]]$response)
  pred=c(pred,result[[1]]$predictor)
  reference2=c(reference2,result[[2]]$response)
  pred2=c(pred2,result[[2]]$predictor)
}
reference=c("[0,30]","(30,60]","(60,100]")[reference]
reference2=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")[reference2]
indicator=confusionMatrix(data=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reference=factor(reference,levels=c("[0,30]","(30,60]","(60,100]")),mode="everything")
indicator$table/10
tab=data.frame(predict=factor(pred2,levels=c("[0,30]","(30,60]","(60,100]")),reality=factor(reference2,levels=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")))
t1=xtabs(~predict+reality,tab)/10
t1
Adjusted_Precision=c(sum(t1[1,1:3])/sum(t1[1,1:6]),NA,sum(t1[3,4:6])/sum(t1[3,1:6]))
Extreme_Error=c(sum(t1[1,5:6])/sum(t1[1,1:6]),NA,sum(t1[3,1:2])/sum(t1[3,1:6]))
Extreme_Sensitivity=c(sum(t1[1,1])/sum(t1[1:3,1]),NA,sum(t1[3,6])/sum(t1[1:3,6]))
t1=round(rbind(t(indicator$byClass[,c(11,1)]),"Extreme Sensitivity"=Extreme_Sensitivity,"Precision"=indicator$byClass[,c(5)],"Adjusted Precision"=Adjusted_Precision,"Extreme Error"=Extreme_Error,"Specificty"=indicator$byClass[,c(2)]),digits=3)
t1=as.data.frame(cbind("evaluation measures"=c("Balanced Accuracy","Sensitivity",  "Extreme Sensitivity","Precision","Adjusted Precision","Extreme Error","Specificty"),t1))
t2=apply(matrix(as.numeric(as.matrix(t1[,2:4])),ncol=3),1,mean,na.rm=TRUE)
t2
f1=qflextable(t1)
f1=bold(f1,i=1:7,j=1)
f1=bold(f1,j=1:4,part="header")
f1=bg(f1,i=c(3,5,6),j=1:4,bg="skyblue")
f1=color(f1,i=c(3,5,6),j=2:4,"white")
f1

## classification with oversampling, weight2, learn 4 adjusted classes
set.seed(1)
pred=c()
reference=c()
pred2=c()
reference2=c()
for(i in 1:10){
  data2=data%>%mutate(popularity_class=cut(popularity,breaks=c(0,30,46,60,100),include.lowest=TRUE))
  result=oversample_class6(data2)
  reference=c(reference,result[[1]]$response)
  pred=c(pred,result[[1]]$predictor)
  reference2=c(reference2,result[[2]]$response)
  pred2=c(pred2,result[[2]]$predictor)
}
reference=c("[0,30]","(30,60]","(60,100]")[reference]
reference2=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")[reference2]
indicator=confusionMatrix(data=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reference=factor(reference,levels=c("[0,30]","(30,60]","(60,100]")),mode="everything")
indicator$table/10
tab=data.frame(predict=factor(pred2,levels=c("[0,30]","(30,60]","(60,100]")),reality=factor(reference2,levels=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")))
t1=xtabs(~predict+reality,tab)/10
t1
Adjusted_Precision=c(sum(t1[1,1:3])/sum(t1[1,1:6]),NA,sum(t1[3,4:6])/sum(t1[3,1:6]))
Extreme_Error=c(sum(t1[1,5:6])/sum(t1[1,1:6]),NA,sum(t1[3,1:2])/sum(t1[3,1:6]))
Extreme_Sensitivity=c(sum(t1[1,1])/sum(t1[1:3,1]),NA,sum(t1[3,6])/sum(t1[1:3,6]))
t1=round(rbind(t(indicator$byClass[,c(11,1)]),"Extreme Sensitivity"=Extreme_Sensitivity,"Precision"=indicator$byClass[,c(5)],"Adjusted Precision"=Adjusted_Precision,"Extreme Error"=Extreme_Error,"Specificty"=indicator$byClass[,c(2)]),digits=3)
t1=as.data.frame(cbind("evaluation measures"=c("Balanced Accuracy","Sensitivity",  "Extreme Sensitivity","Precision","Adjusted Precision","Extreme Error","Specificty"),t1))
t2=apply(matrix(as.numeric(as.matrix(t1[,2:4])),ncol=3),1,mean,na.rm=TRUE)
t2
f1=qflextable(t1)
f1=bold(f1,i=1:7,j=1)
f1=bold(f1,j=1:4,part="header")
f1=bg(f1,i=c(3,5,6),j=1:4,bg="skyblue")
f1=color(f1,i=c(3,5,6),j=2:4,"white")
f1


# Testing on 75s data
## classification with oversampling, weight2, learn 4 balanced classes
training_data=data%>%mutate(popularity_class=cut(popularity,breaks=c(0,30,46,60,100),include.lowest=TRUE))
testing_data=data_75s%>%mutate(popularity_class=cut(popularity,breaks=c(0,30,46,60,100),include.lowest=TRUE))

set.seed(1)
pred=c()
reference=c()
pred2=c()
feature=c()
feature_gain=c()
reference2=c()
for(i in 1:50){
  result=oversample_class_test2(training_data,testing_data)
  reference=c(reference,result[[1]]$response)
  pred=c(pred,result[[1]]$predictor)
  reference2=c(reference2,result[[2]]$response)
  pred2=c(pred2,result[[2]]$predictor)
  feature=c(feature,result[[3]]$Feature)
  feature_gain=c(feature_gain,result[[3]]$Gain)
}
reference=c("[0,30]","(30,60]","(60,100]")[reference]
reference2=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")[reference2]
indicator=confusionMatrix(data=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reference=factor(reference,levels=c("[0,30]","(30,60]","(60,100]")),mode="everything")
feature=matrix(feature,ncol=35,byrow=TRUE)
feature_gain=matrix(feature_gain,ncol=35,byrow=TRUE)

indicator$table/50
tab=data.frame(predict=factor(pred2,levels=c("[0,30]","(30,60]","(60,100]")),reality=factor(reference2,levels=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")))
t1=xtabs(~predict+reality,tab)/50
t1
Adjusted_Precision=c(sum(t1[1,1:3])/sum(t1[1,1:6]),NA,sum(t1[3,4:6])/sum(t1[3,1:6]))
Extreme_Error=c(sum(t1[1,5:6])/sum(t1[1,1:6]),NA,sum(t1[3,1:2])/sum(t1[3,1:6]))
Extreme_Sensitivity=c(sum(t1[1,1])/sum(t1[1:3,1]),NA,sum(t1[3,6])/sum(t1[1:3,6]))
t1=round(rbind(t(indicator$byClass[,c(11,1)]),"Extreme Sensitivity"=Extreme_Sensitivity,"Precision"=indicator$byClass[,c(5)],"Adjusted Precision"=Adjusted_Precision,"Extreme Error"=Extreme_Error,"Specificty"=indicator$byClass[,c(2)]),digits=3)
t1=as.data.frame(cbind("evaluation measures"=c("Balanced Accuracy","Sensitivity",  "Extreme Sensitivity","Precision","Adjusted Precision","Extreme Error","Specificty"),t1))
f1=qflextable(t1)
f1=bold(f1,i=1:7,j=1)
f1=bold(f1,j=1:4,part="header")
f1=bg(f1,i=c(3,5,6),j=1:4,bg="skyblue")
f1=color(f1,i=c(3,5,6),j=2:4,"white")
f1
times=feature[,1:10]
times=sort(table(factor(times)),decreasing = TRUE)
show=c()
for(i in 1:length(names(times))){
  gain_temp=feature_gain[,1:10][which(feature[,1:10]==names(times)[i])]
  show=rbind(show,data.frame(gain=gain_temp,feature_name=names(times)[i],times=times[i]))
}
show$times=factor(show$times)
ggplot(show,aes(y=gain))+geom_boxplot(aes(fill=factor(times)))+facet_wrap(~feature_name,nrow=4)

## classification with oversampling, weight2, learn 4 adjusted classes
training_data=data%>%mutate(popularity_class=cut(popularity,breaks=c(0,30,46,60,100),include.lowest=TRUE))
testing_data=data_75s%>%mutate(popularity_class=cut(popularity,breaks=c(0,30,46,60,100),include.lowest=TRUE))

set.seed(1)
pred=c()
reference=c()
pred2=c()
reference2=c()
feature=c()
feature_gain=c()
for(i in 1:50){
  result=oversample_class_test1(training_data,testing_data)
  reference=c(reference,result[[1]]$response)
  pred=c(pred,result[[1]]$predictor)
  reference2=c(reference2,result[[2]]$response)
  pred2=c(pred2,result[[2]]$predictor)
  feature=c(feature,result[[3]]$Feature)
  feature_gain=c(feature_gain,result[[3]]$Gain)
}
reference=c("[0,30]","(30,60]","(60,100]")[reference]
reference2=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")[reference2]
indicator=confusionMatrix(data=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reference=factor(reference,levels=c("[0,30]","(30,60]","(60,100]")),mode="everything")
feature=matrix(feature,ncol=35,byrow=TRUE)
feature_gain=matrix(feature_gain,ncol=35,byrow=TRUE)

indicator$table/50
tab=data.frame(predict=factor(pred2,levels=c("[0,30]","(30,60]","(60,100]")),reality=factor(reference2,levels=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")))
t1=xtabs(~predict+reality,tab)/50
t1
Adjusted_Precision=c(sum(t1[1,1:3])/sum(t1[1,1:6]),NA,sum(t1[3,4:6])/sum(t1[3,1:6]))
Extreme_Error=c(sum(t1[1,5:6])/sum(t1[1,1:6]),NA,sum(t1[3,1:2])/sum(t1[3,1:6]))
Extreme_Sensitivity=c(sum(t1[1,1])/sum(t1[1:3,1]),NA,sum(t1[3,6])/sum(t1[1:3,6]))
t1=round(rbind(t(indicator$byClass[,c(11,1)]),"Extreme Sensitivity"=Extreme_Sensitivity,"Precision"=indicator$byClass[,c(5)],"Adjusted Precision"=Adjusted_Precision,"Extreme Error"=Extreme_Error,"Specificty"=indicator$byClass[,c(2)]),digits=3)
t1=as.data.frame(cbind("evaluation measures"=c("Balanced Accuracy","Sensitivity",  "Extreme Sensitivity","Precision","Adjusted Precision","Extreme Error","Specificty"),t1))
f1=qflextable(t1)
f1=bold(f1,i=1:7,j=1)
f1=bold(f1,j=1:4,part="header")
f1=bg(f1,i=c(3,5,6),j=1:4,bg="skyblue")
f1=color(f1,i=c(3,5,6),j=2:4,"white")
f1
times=feature[,1:10]
times=sort(table(factor(times)),decreasing = TRUE)
show=c()
for(i in 1:length(names(times))){
  gain_temp=feature_gain[,1:10][which(feature[,1:10]==names(times)[i])]
  show=rbind(show,data.frame(gain=gain_temp,feature_name=names(times)[i],times=times[i]))
}
show$times=factor(show$times)
ggplot(show,aes(y=gain))+geom_boxplot(aes(fill=factor(times)))+facet_wrap(~feature_name,nrow=4)

## classification with oversampling, weight1, regression
set.seed(1)
pred=c()
reference=c()
pred2=c()
reference2=c()
for(i in 1:10){
  data2=data
  result=oversample_class7(data2)
  reference=c(reference,result[[1]]$response)
  pred=c(pred,result[[1]]$predictor)
  reference2=c(reference2,result[[2]]$response)
  pred2=c(pred2,result[[2]]$predictor)
}
reference=c("[0,30]","(30,60]","(60,100]")[reference]
reference2=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")[reference2]
pred=c("[0,30]","(30,60]","(60,100]")[pred]
pred2=c("[0,30]","(30,60]","(60,100]")[pred2]
indicator=confusionMatrix(data=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reference=factor(reference,levels=c("[0,30]","(30,60]","(60,100]")),mode="everything")

indicator$table/10
tab=data.frame(predict=factor(pred2,levels=c("[0,30]","(30,60]","(60,100]")),reality=factor(reference2,levels=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")))
t1=xtabs(~predict+reality,tab)/10
t1
Adjusted_Precision=c(sum(t1[1,1:3])/sum(t1[1,1:6]),NA,sum(t1[3,4:6])/sum(t1[3,1:6]))
Extreme_Error=c(sum(t1[1,5:6])/sum(t1[1,1:6]),NA,sum(t1[3,1:2])/sum(t1[3,1:6]))
Extreme_Sensitivity=c(sum(t1[1,1])/sum(t1[1:3,1]),NA,sum(t1[3,6])/sum(t1[1:3,6]))
t1=round(rbind(t(indicator$byClass[,c(11,1)]),"Extreme Sensitivity"=Extreme_Sensitivity,"Precision"=indicator$byClass[,c(5)],"Adjusted Precision"=Adjusted_Precision,"Extreme Error"=Extreme_Error,"Specificty"=indicator$byClass[,c(2)]),digits=3)
t1=as.data.frame(cbind("evaluation measures"=c("Balanced Accuracy","Sensitivity",  "Extreme Sensitivity","Precision","Adjusted Precision","Extreme Error","Specificty"),t1))
t2=apply(matrix(as.numeric(as.matrix(t1[,2:4])),ncol=3),1,mean,na.rm=TRUE)
t2

f1=qflextable(t1)
f1=bold(f1,i=1:7,j=1)
f1=bold(f1,j=1:4,part="header")
f1=bg(f1,i=c(3,5,6),j=1:4,bg="skyblue")
f1=color(f1,i=c(3,5,6),j=2:4,"white")
f1

## classification with oversampling, weight2, regression
set.seed(1)
pred=c()
reference=c()
pred2=c()
reference2=c()
for(i in 1:10){
  data2=data
  result=oversample_class8(data2)
  reference=c(reference,result[[1]]$response)
  pred=c(pred,result[[1]]$predictor)
  reference2=c(reference2,result[[2]]$response)
  pred2=c(pred2,result[[2]]$predictor)
}
reference=c("[0,30]","(30,60]","(60,100]")[reference]
reference2=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")[reference2]
pred=c("[0,30]","(30,60]","(60,100]")[pred]
pred2=c("[0,30]","(30,60]","(60,100]")[pred2]
indicator=confusionMatrix(data=factor(pred,levels=c("[0,30]","(30,60]","(60,100]")),reference=factor(reference,levels=c("[0,30]","(30,60]","(60,100]")),mode="everything")

indicator$table/10
tab=data.frame(predict=factor(pred2,levels=c("[0,30]","(30,60]","(60,100]")),reality=factor(reference2,levels=c("[0,20]","(20,30]","(30,45]","(45,60]","(60,70]","(70,100]")))
t1=xtabs(~predict+reality,tab)/10
t1
Adjusted_Precision=c(sum(t1[1,1:3])/sum(t1[1,1:6]),NA,sum(t1[3,4:6])/sum(t1[3,1:6]))
Extreme_Error=c(sum(t1[1,5:6])/sum(t1[1,1:6]),NA,sum(t1[3,1:2])/sum(t1[3,1:6]))
Extreme_Sensitivity=c(sum(t1[1,1])/sum(t1[1:3,1]),NA,sum(t1[3,6])/sum(t1[1:3,6]))
t1=round(rbind(t(indicator$byClass[,c(11,1)]),"Extreme Sensitivity"=Extreme_Sensitivity,"Precision"=indicator$byClass[,c(5)],"Adjusted Precision"=Adjusted_Precision,"Extreme Error"=Extreme_Error,"Specificty"=indicator$byClass[,c(2)]),digits=3)
t1=as.data.frame(cbind("evaluation measures"=c("Balanced Accuracy","Sensitivity",  "Extreme Sensitivity","Precision","Adjusted Precision","Extreme Error","Specificty"),t1))
t2=apply(matrix(as.numeric(as.matrix(t1[,2:4])),ncol=3),1,mean,na.rm=TRUE)
t2

f1=qflextable(t1)
f1=bold(f1,i=1:7,j=1)
f1=bold(f1,j=1:4,part="header")
f1=bg(f1,i=c(3,5,6),j=1:4,bg="skyblue")
f1=color(f1,i=c(3,5,6),j=2:4,"white")
f1

# interpretation
### extremely misclassified conditions 
a1=testing_data[which(pred2=="[0,30]"&reference2=="(70,100]")%%624,]
n=(a1%>%group_by(name)%>%count())
kable(full_join(n,unique(a1))%>%filter(n>=10)%>%select(n,popularity,artist_popularity,covering_intensity4,year_board,count)%>%arrange(desc(n)))

a2=testing_data[which(pred2=="(60,100]"&(reference2=="[0,20]"))%%624,]
n=(a2%>%group_by(name)%>%count())
kable(full_join(n,unique(a2))%>%filter(n>=20)%>%select(n,popularity,artist_popularity,covering_intensity4,year_board,count)%>%arrange(desc(n)))

### High popularity songs picked out
a3=testing_data[which(pred2=="(60,100]"&(reference2=="(70,100]"))%%624,]
a4=testing_data[which(pred2=="(60,100]"&(reference2=="(60,70]"))%%624,]
n=(a3%>%group_by(name)%>%count())
kable(full_join(n,unique(a3))%>%filter(n>=30)%>%select(n,popularity,artist_popularity,covering_intensity4,year_board,count)%>%arrange(desc(n)))
n=(a4%>%group_by(name)%>%count())
kable(full_join(n,unique(a4))%>%filter(n>=30)%>%select(n,popularity,artist_popularity,covering_intensity4,year_board,count)%>%arrange(desc(n)))