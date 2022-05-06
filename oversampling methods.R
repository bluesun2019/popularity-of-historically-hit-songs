count_no <- function(y){
  weight_label=y
  weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
  if(weight_mean<=30){
    result=1
  }else if(weight_mean>=60){
    result=2
  }else{
    result=3
  }
  result
}
mean_label<-function(y){
  weight_label=y
  weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
  weight_mean
}
createDataPartition_my_version <- function (y, times = 1, p = 0.5, list = TRUE, groups = min(10, length(y))) 
{
  if (class(y)[1] == "Surv") 
    y <- y[, "time"]
  out <- vector(mode = "list", times)
  if (length(y) < 2) 
    stop("y must have at least 2 data points")
  if (groups < 2) 
    groups <- 2
  if (is.numeric(y)) {
    y <- cut(y, unique(quantile(y, probs = seq(0, 1, length = groups))), 
             include.lowest = TRUE)
  }
  else {
    xtab <- table(y)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", 
                    paste(names(xtab)[xtab == 0], sep = "", 
                          collapse = ", "), ") and these will be ignored"))
      y <- factor(as.character(y))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (", 
                    paste(names(xtab)[xtab == 1], sep = "", 
                          collapse = ", "), ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat, p) {
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- ceiling(nrow(dat) * p)
      out <- sample(dat$index, size = num, replace=TRUE)
    }
    out
  }
  for (j in 1:times) {
    tmp <- plyr::dlply(data.frame(y = y, index = seq(along = y)), 
                       plyr::.(y), subsample, p = p)
    tmp <- sort(as.vector(unlist(tmp)))
    out[[j]] <- tmp
  }
  if (!list) {
    out <- matrix(unlist(out), ncol = times)
  }
  out
}

#-------
# 3 classes
## plot1
plot_my_version1 <- function(z, times = 1,list = TRUE, groups = min(50, length(z))){
  z=na.omit(z)
  if (class(z)[1] == "Surv") 
    z <- z[, "time"]
  out <- vector(mode = "list", times)
  if (length(z) < 2) 
    stop("z must have at least 2 data points")
  if (groups < 2) 
    groups <- 2
  if (is.numeric(z)) {
    z <- cut(z, unique(quantile(z, probs = seq(0, 1, length = groups))), 
             include.lowest = TRUE)
  }else {
    xtab <- table(z)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", 
                    paste(names(xtab)[xtab == 0], sep = "", 
                          collapse = ", "), ") and these will be ignored"))
      z <- factor(as.character(z))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (", 
                    paste(names(xtab)[xtab == 1], sep = "", 
                          collapse = ", "), ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat,a,b) {
    weight_label=dat$y[1]
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    label=85/groups
    if(weight_mean<=30){
      if(weight_mean<=20){
        weight=a
      }else if(weight_mean>=25){
        weight=0
      }else{
        weight=a-a/5*(weight_mean-20)
      }
    }
    if(weight_mean>=60){
      if(weight_mean>=70){
        weight=b
      }else if(weight_mean<=65){
        weight=0
      }else{
        weight=b/5*(weight_mean-65)
      }
    }
    if(weight_mean<60&&weight_mean>30){
      weight=0
    }
    p=weight
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- floor(nrow(dat) * p)
      out <- sample(dat$index, size = num, replace=TRUE)
    }
    out
  }
  parameter_calculate<-function(table){
    right1_use=filter(table,label==3)
    right1=sum(right1_use$n)
    left1_use1=filter(table,label==1&mean>20&mean<25)
    left11=sum((5-left1_use1$mean/5)*left1_use1$n)
    left1_use2=filter(table,label==1&mean<=20)
    left12=sum(left1_use2$n)
    left1_use3=filter(table,label==1&mean>=25)
    left13=sum(left1_use3$n)
    a=(right1-sum(left1_use1$n)-sum(left1_use2$n)-sum(left1_use3$n))/(left11+left12)
    left2_use1=filter(table,label==2&mean<70&mean>65)
    left21=sum((left2_use1$mean-65)/5*left2_use1$n)
    left2_use2=filter(table,label==2&mean>=70)
    left22=sum(left2_use2$n)
    left2_use3=filter(table,label==2&mean<=65)
    left23=sum(left2_use3$n)
    b=(right1-sum(left2_use1$n)-sum(left2_use2$n)-sum(left2_use3$n))/(left21+left22)
    c(a=a,b=b)
  }
  count_no <- function(y){
    weight_label=y
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    if(weight_mean<=30){
      result=1
    }else if(weight_mean>=60){
      result=2
    }else{
      result=3
    }
    result
  }
  mean_label<-function(y){
    weight_label=y
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    weight_mean
  }
  label=sapply(z,count_no)
  temp=data.frame(y=z,index=seq(along = z),label=label)
  temp2=temp%>%group_by(label,y)%>%count()%>%mutate(mean=mean_label(y))
  temp3=as.data.frame(temp2)%>%group_by(label)
  parameter=parameter_calculate(temp3)
  a=parameter[1]
  b=parameter[2]
  x=c(0,20,25,65,70,100)
  y=c(a+1,a+1,1,1,b+1,b+1)
  y2=c(1,1,1,1,1,1)
  dataframe=data.frame(popularity=x,oversampling_probability=y,y2=y2)
  p=ggplot(data=dataframe,aes(popularity,oversampling_probability))+geom_area(aes(y=y2),fill="#f47983",alpha=0.7)+geom_line(color="#bf3d22",size=1.2)+geom_area(fill="pink",alpha=0.5)+ scale_y_continuous(breaks=c(0,1,1.5),minor_breaks=NULL)+ scale_x_continuous(breaks=c(0,20,25,65,70,100), minor_breaks=NULL)
  p=p+geom_vline(xintercept = c(30,60),size=1,alpha=0.8,linetype="dashed")
  p
}

createDataPartition_my_version1 <- function (y, times = 1,  list = TRUE, groups = min(50, length(y))) 
{
  if (class(y)[1] == "Surv") 
    y <- y[, "time"]
  out <- vector(mode = "list", times)
  if (length(y) < 2) 
    stop("y must have at least 2 data points")
  if (groups < 2) 
    groups <- 2
  if (is.numeric(y)) {
    y <- cut(y, unique(quantile(y, probs = seq(0, 1, length = groups))), 
             include.lowest = TRUE)
  }else {
    xtab <- table(y)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", 
                    paste(names(xtab)[xtab == 0], sep = "", 
                          collapse = ", "), ") and these will be ignored"))
      y <- factor(as.character(y))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (", 
                    paste(names(xtab)[xtab == 1], sep = "", 
                          collapse = ", "), ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat,a,b) {
    weight_label=dat$y[1]
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    label=85/groups
    if(weight_mean<=30){
      if(weight_mean<=20){
        weight=a
      }else if(weight_mean>=25){
        weight=0
      }else{
        weight=a-a/5*(weight_mean-20)
      }
    }
    if(weight_mean>=60){
      if(weight_mean>=70){
        weight=b
      }else if(weight_mean<=65){
        weight=0
      }else{
        weight=b/5*(weight_mean-65)
      }
    }
    if(weight_mean<60&&weight_mean>30){
      weight=0
    }
    p=weight
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- floor(nrow(dat) * p)
      out <- sample(dat$index, size = num, replace=TRUE)
    }
    out
  }
  parameter_calculate<-function(table){
    right1_use=filter(table,label==3)
    right1=sum(right1_use$n)
    left1_use1=filter(table,label==1&mean>20&mean<25)
    left11=sum((5-left1_use1$mean/5)*left1_use1$n)
    left1_use2=filter(table,label==1&mean<=20)
    left12=sum(left1_use2$n)
    left1_use3=filter(table,label==1&mean>=25)
    left13=sum(left1_use3$n)
    a=(right1-sum(left1_use1$n)-sum(left1_use2$n)-sum(left1_use3$n))/(left11+left12)
    left2_use1=filter(table,label==2&mean<70&mean>65)
    left21=sum((left2_use1$mean-65)/5*left2_use1$n)
    left2_use2=filter(table,label==2&mean>=70)
    left22=sum(left2_use2$n)
    left2_use3=filter(table,label==2&mean<=65)
    left23=sum(left2_use3$n)
    b=(right1-sum(left2_use1$n)-sum(left2_use2$n)-sum(left2_use3$n))/(left21+left22)
    c(a=a,b=b)
  }
  mean_label<-function(y){
    weight_label=y
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    weight_mean
  }
  for (j in 1:times) {
    label=sapply(y,count_no)
    temp=data.frame(y=y,index=seq(along = y),label=label)
    temp2=temp%>%group_by(label,y)%>%count()%>%mutate(mean=mean_label(y))
    temp3=as.data.frame(temp2)%>%group_by(label)
    parameter=parameter_calculate(temp3)
    tmp <- plyr::dlply(data.frame(y = y, index = seq(along = y)), 
                       plyr::.(y), subsample,parameter[1],parameter[2])
    tmp <- sort(as.vector(unlist(tmp)))
    out[[j]] <- tmp
  }
  if (!list) {
    out <- matrix(unlist(out), ncol = times)
  }
  out
}

#-------
## plot2
plot_my_version2 <- function(z, times = 1,list = TRUE, groups = min(50, length(z))){
  z=na.omit(z)
  if (class(z)[1] == "Surv") 
    z <- z[, "time"]
  out <- vector(mode = "list", times)
  if (length(z) < 2) 
    stop("z must have at least 2 data points")
  if (groups < 2) 
    groups <- 2
  if (is.numeric(z)) {
    z <- cut(z, unique(quantile(z, probs = seq(0, 1, length = groups))), 
             include.lowest = TRUE)
  }else {
    xtab <- table(z)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", 
                    paste(names(xtab)[xtab == 0], sep = "", 
                          collapse = ", "), ") and these will be ignored"))
      z <- factor(as.character(z))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (", 
                    paste(names(xtab)[xtab == 1], sep = "", 
                          collapse = ", "), ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat,a,b) {
    weight_label=dat$y[1]
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    label=85/groups
    if(weight_mean<=30){
      if(weight_mean<=20){
        weight=a
      }else if(weight_mean>=25){
        weight=0
      }else{
        weight=a-a/5*(weight_mean-20)
      }
    }
    if(weight_mean>=60){
      if(weight_mean>=70){
        weight=b
      }else if(weight_mean<=65){
        weight=0
      }else{
        weight=b/5*(weight_mean-65)
      }
    }
    if(weight_mean<60&&weight_mean>30){
      if(weight_mean<50&&weight_mean>40){
        weight=0.5
      }else if(weight_mean<=40&&weight_mean>35){
        weight=0.5/5*(weight_mean-35)
      }else if(weight_mean>=50&&weight_mean<55){
        weight=0.5-0.5/5*(weight_mean-50)
      }else{
        weight=0
      }
    }
    p=weight
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- floor(nrow(dat) * p)
      out <- sample(dat$index, size = num, replace=TRUE)
    }
    out
  }
  parameter_calculate<-function(table){
    right1_use=filter(table,label==3&mean<40&mean>=35)
    right1=sum((1+(right1_use$mean-35)/10)*right1_use$n)
    right2_use=filter(table,label==3&mean>50&mean<=55)
    right2=sum((1.5-(right2_use$mean-50)/10)*right2_use$n)
    right3_use=filter(table,label==3&(mean<=50&mean>=40))
    right3=sum(1.5*right3_use$n)
    right4_use=filter(table,label==3&(mean<=35|mean>=55))
    right4=sum(right4_use$n)
    left1_use1=filter(table,label==1&mean>20&mean<25)
    left11=sum((5-left1_use1$mean/5)*left1_use1$n)
    left1_use2=filter(table,label==1&mean<=20)
    left12=sum(left1_use2$n)
    left1_use3=filter(table,label==1&mean>=25)
    left13=sum(left1_use3$n)
    a=(right1+right2+right3+right4-sum(left1_use1$n)-sum(left1_use2$n)-sum(left1_use3$n))/(left11+left12)
    left2_use1=filter(table,label==2&mean<70&mean>65)
    left21=sum((left2_use1$mean-65)/5*left2_use1$n)
    left2_use2=filter(table,label==2&mean>=70)
    left22=sum(left2_use2$n)
    left2_use3=filter(table,label==2&mean<=65)
    left23=sum(left2_use3$n)
    b=(right1+right2+right3+right4-sum(left2_use1$n)-sum(left2_use2$n)-sum(left2_use3$n))/(left21+left22)
    c(a=a,b=b)
  }
  label=sapply(z,count_no)
  temp=data.frame(y=z,index=seq(along = z),label=label)
  temp2=temp%>%group_by(label,y)%>%count()%>%mutate(mean=mean_label(y))
  temp3=as.data.frame(temp2)%>%group_by(label)
  parameter=parameter_calculate(temp3)
  a=parameter[1]
  b=parameter[2]
  x=c(0,20,25,35,40,50,55,65,70,100)
  y=c(a+1,a+1,1,1,1.5,1.5,1,1,b+1,b+1)
  y2=c(1,1,1,1,1,1,1,1,1,1)
  dataframe=data.frame(popularity=x,oversampling_probability=y,y2=y2)
  p=ggplot(data=dataframe,aes(popularity,oversampling_probability))+geom_area(aes(y=y2),fill="skyblue",alpha=0.7)+geom_line(color="dodgerblue",size=1.2)+geom_area(fill="lightblue",alpha=0.5)+ scale_y_continuous(breaks=c(0,1,1.5),minor_breaks=NULL)+ scale_x_continuous(breaks=c(0,20,25,35,40,50,55,65,70,100), minor_breaks=NULL)
  p=p+geom_vline(xintercept = c(30,60),size=1,alpha=0.8,linetype="dashed")
  p
}

createDataPartition_my_version2 <- function (y, times = 1,  list = TRUE, groups = min(50, length(y))) 
{
  if (class(y)[1] == "Surv") 
    y <- y[, "time"]
  out <- vector(mode = "list", times)
  if (length(y) < 2) 
    stop("y must have at least 2 data points")
  if (groups < 2) 
    groups <- 2
  if (is.numeric(y)) {
    y <- cut(y, unique(quantile(y, probs = seq(0, 1, length = groups))), 
             include.lowest = TRUE)
  }else {
    xtab <- table(y)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", 
                    paste(names(xtab)[xtab == 0], sep = "", 
                          collapse = ", "), ") and these will be ignored"))
      y <- factor(as.character(y))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (", 
                    paste(names(xtab)[xtab == 1], sep = "", 
                          collapse = ", "), ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat,a,b) {
    weight_label=dat$y[1]
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    label=85/groups
    if(weight_mean<=30){
      if(weight_mean<=20){
        weight=a
      }else if(weight_mean>=25){
        weight=0
      }else{
        weight=a-a/5*(weight_mean-20)
      }
    }
    if(weight_mean>=60){
      if(weight_mean>=70){
        weight=b
      }else if(weight_mean<=65){
        weight=0
      }else{
        weight=b/5*(weight_mean-65)
      }
    }
    if(weight_mean<60&&weight_mean>30){
      if(weight_mean<50&&weight_mean>40){
        weight=0.5
      }else if(weight_mean<=40&&weight_mean>35){
        weight=0.5/5*(weight_mean-35)
      }else if(weight_mean>=50&&weight_mean<55){
        weight=0.5-0.5/5*(weight_mean-50)
      }else{
        weight=0
      }
    }
    p=weight
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- floor(nrow(dat) * p)
      out <- sample(dat$index, size = num, replace=TRUE)
    }
    out
  }
  parameter_calculate<-function(table){
    right1_use=filter(table,label==3&mean<40&mean>=35)
    right1=sum((1+(right1_use$mean-35)/10)*right1_use$n)
    right2_use=filter(table,label==3&mean>50&mean<=55)
    right2=sum((1.5-(right2_use$mean-50)/10)*right2_use$n)
    right3_use=filter(table,label==3&(mean<=50&mean>=40))
    right3=sum(1.5*right3_use$n)
    right4_use=filter(table,label==3&(mean<=35|mean>=55))
    right4=sum(right4_use$n)
    left1_use1=filter(table,label==1&mean>20&mean<25)
    left11=sum((5-left1_use1$mean/5)*left1_use1$n)
    left1_use2=filter(table,label==1&mean<=20)
    left12=sum(left1_use2$n)
    left1_use3=filter(table,label==1&mean>=25)
    left13=sum(left1_use3$n)
    a=(right1+right2+right3+right4-sum(left1_use1$n)-sum(left1_use2$n)-sum(left1_use3$n))/(left11+left12)
    left2_use1=filter(table,label==2&mean<70&mean>65)
    left21=sum((left2_use1$mean-65)/5*left2_use1$n)
    left2_use2=filter(table,label==2&mean>=70)
    left22=sum(left2_use2$n)
    left2_use3=filter(table,label==2&mean<=65)
    left23=sum(left2_use3$n)
    b=(right1+right2+right3+right4-sum(left2_use1$n)-sum(left2_use2$n)-sum(left2_use3$n))/(left21+left22)
    c(a=a,b=b)
  }
  mean_label<-function(y){
    weight_label=y
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    weight_mean
  }
  for (j in 1:times) {
    label=sapply(y,count_no)
    temp=data.frame(y=y,index=seq(along = y),label=label)
    temp2=temp%>%group_by(label,y)%>%count()%>%mutate(mean=mean_label(y))
    temp3=as.data.frame(temp2)%>%group_by(label)
    parameter=parameter_calculate(temp3)
    tmp <- plyr::dlply(data.frame(y = y, index = seq(along = y)), 
                       plyr::.(y), subsample,parameter[1],parameter[2])
    tmp <- sort(as.vector(unlist(tmp)))
    out[[j]] <- tmp
  }
  if (!list) {
    out <- matrix(unlist(out), ncol = times)
  }
  out
}

#---------
## plot1 for 4
plot_my_version3 <- function(z, times = 1,list = TRUE, groups = min(50, length(z))){
  z=na.omit(z)
  if (class(z)[1] == "Surv") 
    z <- z[, "time"]
  out <- vector(mode = "list", times)
  if (length(z) < 2) 
    stop("z must have at least 2 data points")
  if (groups < 2) 
    groups <- 2
  if (is.numeric(z)) {
    z <- cut(z, unique(quantile(z, probs = seq(0, 1, length = groups))), 
             include.lowest = TRUE)
  }else {
    xtab <- table(z)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", 
                    paste(names(xtab)[xtab == 0], sep = "", 
                          collapse = ", "), ") and these will be ignored"))
      z <- factor(as.character(z))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (", 
                    paste(names(xtab)[xtab == 1], sep = "", 
                          collapse = ", "), ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat,a,b) {
    weight_label=dat$y[1]
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    label=85/groups
    if(weight_mean<=30){
      if(weight_mean<=20){
        weight=a
      }else if(weight_mean>=25){
        weight=0
      }else{
        weight=a-a/5*(weight_mean-20)
      }
    }
    if(weight_mean>=60){
      if(weight_mean>=70){
        weight=b
      }else if(weight_mean<=65){
        weight=0
      }else{
        weight=b/5*(weight_mean-65)
      }
    }
    if(weight_mean<60&&weight_mean>30){
      weight=0
    }
    p=weight
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- floor(nrow(dat) * p)
      out <- sample(dat$index, size = num, replace=TRUE)
    }
    out
  }
  parameter_calculate<-function(table){
    right1_use=filter(table,label==3)
    right1=sum(right1_use$n)
    left1_use1=filter(table,label==1&mean>20&mean<25)
    left11=sum((5-left1_use1$mean/5)*left1_use1$n)
    left1_use2=filter(table,label==1&mean<=20)
    left12=sum(left1_use2$n)
    left1_use3=filter(table,label==1&mean>=25)
    left13=sum(left1_use3$n)
    a=(0.55*right1-sum(left1_use1$n)-sum(left1_use2$n)-sum(left1_use3$n))/(left11+left12)
    left2_use1=filter(table,label==2&mean<70&mean>65)
    left21=sum((left2_use1$mean-65)/5*left2_use1$n)
    left2_use2=filter(table,label==2&mean>=70)
    left22=sum(left2_use2$n)
    left2_use3=filter(table,label==2&mean<=65)
    left23=sum(left2_use3$n)
    b=(0.55*right1-sum(left2_use1$n)-sum(left2_use2$n)-sum(left2_use3$n))/(left21+left22)
    c(a=a,b=b)
  }
  label=sapply(z,count_no)
  temp=data.frame(y=z,index=seq(along = z),label=label)
  temp2=temp%>%group_by(label,y)%>%count()%>%mutate(mean=mean_label(y))
  temp3=as.data.frame(temp2)%>%group_by(label)
  parameter=parameter_calculate(temp3)
  a=parameter[1]
  b=parameter[2]
  x=c(0,20,25,65,70,100)
  y=c(a+1,a+1,1,1,b+1,b+1)
  y2=c(1,1,1,1,1,1)
  dataframe=data.frame(popularity=x,oversampling_probability=y,y2=y2)
  p=ggplot(data=dataframe,aes(popularity,oversampling_probability))+geom_area(aes(y=y2),fill="#f47983",alpha=0.7)+geom_line(color="#bf3d22",size=1.2)+geom_area(fill="pink",alpha=0.5)+ scale_y_continuous(breaks=c(0,1,1.5),minor_breaks=NULL)+ scale_x_continuous(breaks=c(0,20,25,65,70,100), minor_breaks=NULL)
  p=p+geom_vline(xintercept = c(30,60),size=1,alpha=0.8,linetype="dashed")+geom_vline(xintercept =45,size=1,alpha=0.5,linetype="dotted")
  p
}

createDataPartition_my_version3 <- function (y, times = 1,  list = TRUE, groups = min(50, length(y))) 
{
  if (class(y)[1] == "Surv") 
    y <- y[, "time"]
  out <- vector(mode = "list", times)
  if (length(y) < 2) 
    stop("y must have at least 2 data points")
  if (groups < 2) 
    groups <- 2
  if (is.numeric(y)) {
    y <- cut(y, unique(quantile(y, probs = seq(0, 1, length = groups))), 
             include.lowest = TRUE)
  }else {
    xtab <- table(y)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", 
                    paste(names(xtab)[xtab == 0], sep = "", 
                          collapse = ", "), ") and these will be ignored"))
      y <- factor(as.character(y))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (", 
                    paste(names(xtab)[xtab == 1], sep = "", 
                          collapse = ", "), ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat,a,b) {
    weight_label=dat$y[1]
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    label=85/groups
    if(weight_mean<=30){
      if(weight_mean<=20){
        weight=a
      }else if(weight_mean>=25){
        weight=0
      }else{
        weight=a-a/5*(weight_mean-20)
      }
    }
    if(weight_mean>=60){
      if(weight_mean>=70){
        weight=b
      }else if(weight_mean<=65){
        weight=0
      }else{
        weight=b/5*(weight_mean-65)
      }
    }
    if(weight_mean<60&&weight_mean>30){
      weight=0
    }
    p=weight
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- floor(nrow(dat) * p)
      out <- sample(dat$index, size = num, replace=TRUE)
    }
    out
  }
  parameter_calculate<-function(table){
    right1_use=filter(table,label==3)
    right1=sum(right1_use$n)
    left1_use1=filter(table,label==1&mean>20&mean<25)
    left11=sum((5-left1_use1$mean/5)*left1_use1$n)
    left1_use2=filter(table,label==1&mean<=20)
    left12=sum(left1_use2$n)
    left1_use3=filter(table,label==1&mean>=25)
    left13=sum(left1_use3$n)
    a=(0.55*right1-sum(left1_use1$n)-sum(left1_use2$n)-sum(left1_use3$n))/(left11+left12)
    left2_use1=filter(table,label==2&mean<70&mean>65)
    left21=sum((left2_use1$mean-65)/5*left2_use1$n)
    left2_use2=filter(table,label==2&mean>=70)
    left22=sum(left2_use2$n)
    left2_use3=filter(table,label==2&mean<=65)
    left23=sum(left2_use3$n)
    b=(0.55*right1-sum(left2_use1$n)-sum(left2_use2$n)-sum(left2_use3$n))/(left21+left22)
    c(a=a,b=b)
  }
  mean_label<-function(y){
    weight_label=y
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    weight_mean
  }
  for (j in 1:times) {
    label=sapply(y,count_no)
    temp=data.frame(y=y,index=seq(along = y),label=label)
    temp2=temp%>%group_by(label,y)%>%count()%>%mutate(mean=mean_label(y))
    temp3=as.data.frame(temp2)%>%group_by(label)
    parameter=parameter_calculate(temp3)
    tmp <- plyr::dlply(data.frame(y = y, index = seq(along = y)), 
                       plyr::.(y), subsample,parameter[1],parameter[2])
    tmp <- sort(as.vector(unlist(tmp)))
    out[[j]] <- tmp
  }
  if (!list) {
    out <- matrix(unlist(out), ncol = times)
  }
  out
}

#-------
## plot2 for 4
plot_my_version4 <- function(z, times = 1,list = TRUE, groups = min(50, length(z))){
  z=na.omit(z)
  if (class(z)[1] == "Surv") 
    z <- z[, "time"]
  out <- vector(mode = "list", times)
  if (length(z) < 2) 
    stop("z must have at least 2 data points")
  if (groups < 2) 
    groups <- 2
  if (is.numeric(z)) {
    z <- cut(z, unique(quantile(z, probs = seq(0, 1, length = groups))), 
             include.lowest = TRUE)
  }else {
    xtab <- table(z)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", 
                    paste(names(xtab)[xtab == 0], sep = "", 
                          collapse = ", "), ") and these will be ignored"))
      z <- factor(as.character(z))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (", 
                    paste(names(xtab)[xtab == 1], sep = "", 
                          collapse = ", "), ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat,a,b) {
    weight_label=dat$y[1]
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    label=85/groups
    if(weight_mean<=30){
      if(weight_mean<=20){
        weight=a
      }else if(weight_mean>=25){
        weight=0
      }else{
        weight=a-a/5*(weight_mean-20)
      }
    }
    if(weight_mean>=60){
      if(weight_mean>=70){
        weight=b
      }else if(weight_mean<=65){
        weight=0
      }else{
        weight=b/5*(weight_mean-65)
      }
    }
    if(weight_mean<60&&weight_mean>30){
      if(weight_mean<50&&weight_mean>40){
        weight=0.5
      }else if(weight_mean<=40&&weight_mean>35){
        weight=0.5/5*(weight_mean-35)
      }else if(weight_mean>=50&&weight_mean<55){
        weight=0.5-0.5/5*(weight_mean-50)
      }else{
        weight=0
      }
    }
    p=weight
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- floor(nrow(dat) * p)
      out <- sample(dat$index, size = num, replace=TRUE)
    }
    out
  }
  parameter_calculate<-function(table){
    right1_use=filter(table,label==3&mean<40&mean>=35)
    right1=sum((1+(right1_use$mean-35)/10)*right1_use$n)
    right2_use=filter(table,label==3&mean>50&mean<=55)
    right2=sum((1.5-(right2_use$mean-50)/10)*right2_use$n)
    right3_use=filter(table,label==3&(mean<=50&mean>=40))
    right3=sum(1.5*right3_use$n)
    right4_use=filter(table,label==3&(mean<=35|mean>=55))
    right4=sum(right4_use$n)
    left1_use1=filter(table,label==1&mean>20&mean<25)
    left11=sum((5-left1_use1$mean/5)*left1_use1$n)
    left1_use2=filter(table,label==1&mean<=20)
    left12=sum(left1_use2$n)
    left1_use3=filter(table,label==1&mean>=25)
    left13=sum(left1_use3$n)
    a=(0.5*(right1+right2+right3+right4)-sum(left1_use1$n)-sum(left1_use2$n)-sum(left1_use3$n))/(left11+left12)
    left2_use1=filter(table,label==2&mean<70&mean>65)
    left21=sum((left2_use1$mean-65)/5*left2_use1$n)
    left2_use2=filter(table,label==2&mean>=70)
    left22=sum(left2_use2$n)
    left2_use3=filter(table,label==2&mean<=65)
    left23=sum(left2_use3$n)
    b=(0.5*(right1+right2+right3+right4)-sum(left2_use1$n)-sum(left2_use2$n)-sum(left2_use3$n))/(left21+left22)
    c(a=a,b=b)
  }
  label=sapply(z,count_no)
  temp=data.frame(y=z,index=seq(along = z),label=label)
  temp2=temp%>%group_by(label,y)%>%count()%>%mutate(mean=mean_label(y))
  temp3=as.data.frame(temp2)%>%group_by(label)
  parameter=parameter_calculate(temp3)
  a=parameter[1]
  b=parameter[2]
  x=c(0,20,25,35,40,50,55,65,70,100)
  y=c(a+1,a+1,1,1,1.5,1.5,1,1,b+1,b+1)
  y2=c(1,1,1,1,1,1,1,1,1,1)
  dataframe=data.frame(popularity=x,oversampling_probability=y,y2=y2)
  p=ggplot(data=dataframe,aes(popularity,oversampling_probability))+geom_area(aes(y=y2),fill="skyblue",alpha=0.7)+geom_line(color="dodgerblue",size=1.2)+geom_area(fill="lightblue",alpha=0.5)+ scale_y_continuous(breaks=c(0,1,1.5),minor_breaks=NULL)+ scale_x_continuous(breaks=c(0,20,25,35,40,50,55,65,70,100), minor_breaks=NULL)
  p=p+geom_vline(xintercept = c(30,60),size=1,alpha=0.8,linetype="dashed")+geom_vline(xintercept =46,size=1,alpha=0.5,linetype="dotted")
  p
}

createDataPartition_my_version4 <- function (y, times = 1,  list = TRUE, groups = min(50, length(y))) 
{
  if (class(y)[1] == "Surv") 
    y <- y[, "time"]
  out <- vector(mode = "list", times)
  if (length(y) < 2) 
    stop("y must have at least 2 data points")
  if (groups < 2) 
    groups <- 2
  if (is.numeric(y)) {
    y <- cut(y, unique(quantile(y, probs = seq(0, 1, length = groups))), 
             include.lowest = TRUE)
  }else {
    xtab <- table(y)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", 
                    paste(names(xtab)[xtab == 0], sep = "", 
                          collapse = ", "), ") and these will be ignored"))
      y <- factor(as.character(y))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (", 
                    paste(names(xtab)[xtab == 1], sep = "", 
                          collapse = ", "), ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat,a,b) {
    weight_label=dat$y[1]
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    label=85/groups
    if(weight_mean<=30){
      if(weight_mean<=20){
        weight=a
      }else if(weight_mean>=25){
        weight=0
      }else{
        weight=a-a/5*(weight_mean-20)
      }
    }
    if(weight_mean>=60){
      if(weight_mean>=70){
        weight=b
      }else if(weight_mean<=65){
        weight=0
      }else{
        weight=b/5*(weight_mean-65)
      }
    }
    if(weight_mean<60&&weight_mean>30){
      if(weight_mean<50&&weight_mean>40){
        weight=0.5
      }else if(weight_mean<=40&&weight_mean>35){
        weight=0.5/5*(weight_mean-35)
      }else if(weight_mean>=50&&weight_mean<55){
        weight=0.5-0.5/5*(weight_mean-50)
      }else{
        weight=0
      }
    }
    p=weight
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- floor(nrow(dat) * p)
      out <- sample(dat$index, size = num, replace=TRUE)
    }
    out
  }
  parameter_calculate<-function(table){
    right1_use=filter(table,label==3&mean<40&mean>=35)
    right1=sum((1+(right1_use$mean-35)/10)*right1_use$n)
    right2_use=filter(table,label==3&mean>50&mean<=55)
    right2=sum((1.5-(right2_use$mean-50)/10)*right2_use$n)
    right3_use=filter(table,label==3&(mean<=50&mean>=40))
    right3=sum(1.5*right3_use$n)
    right4_use=filter(table,label==3&(mean<=35|mean>=55))
    right4=sum(right4_use$n)
    left1_use1=filter(table,label==1&mean>20&mean<25)
    left11=sum((5-left1_use1$mean/5)*left1_use1$n)
    left1_use2=filter(table,label==1&mean<=20)
    left12=sum(left1_use2$n)
    left1_use3=filter(table,label==1&mean>=25)
    left13=sum(left1_use3$n)
    a=(0.5*(right1+right2+right3+right4)-sum(left1_use1$n)-sum(left1_use2$n)-sum(left1_use3$n))/(left11+left12)
    left2_use1=filter(table,label==2&mean<70&mean>65)
    left21=sum((left2_use1$mean-65)/5*left2_use1$n)
    left2_use2=filter(table,label==2&mean>=70)
    left22=sum(left2_use2$n)
    left2_use3=filter(table,label==2&mean<=65)
    left23=sum(left2_use3$n)
    b=(0.5*(right1+right2+right3+right4)-sum(left2_use1$n)-sum(left2_use2$n)-sum(left2_use3$n))/(left21+left22)
    c(a=a,b=b)
  }
  mean_label<-function(y){
    weight_label=y
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    weight_mean
  }
  for (j in 1:times) {
    label=sapply(y,count_no)
    temp=data.frame(y=y,index=seq(along = y),label=label)
    temp2=temp%>%group_by(label,y)%>%count()%>%mutate(mean=mean_label(y))
    temp3=as.data.frame(temp2)%>%group_by(label)
    parameter=parameter_calculate(temp3)
    tmp <- plyr::dlply(data.frame(y = y, index = seq(along = y)), 
                       plyr::.(y), subsample,parameter[1],parameter[2])
    tmp <- sort(as.vector(unlist(tmp)))
    out[[j]] <- tmp
  }
  if (!list) {
    out <- matrix(unlist(out), ncol = times)
  }
  out
}

#-------
## plot1 for 3
plot_my_version5 <- function(z, times = 1,list = TRUE, groups = min(50, length(z))){
  z=na.omit(z)
  if (class(z)[1] == "Surv") 
    z <- z[, "time"]
  out <- vector(mode = "list", times)
  if (length(z) < 2) 
    stop("z must have at least 2 data points")
  if (groups < 2) 
    groups <- 2
  if (is.numeric(z)) {
    z <- cut(z, unique(quantile(z, probs = seq(0, 1, length = groups))), 
             include.lowest = TRUE)
  }else {
    xtab <- table(z)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", 
                    paste(names(xtab)[xtab == 0], sep = "", 
                          collapse = ", "), ") and these will be ignored"))
      z <- factor(as.character(z))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (", 
                    paste(names(xtab)[xtab == 1], sep = "", 
                          collapse = ", "), ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat,a,b) {
    weight_label=dat$y[1]
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    label=85/groups
    if(weight_mean<=30){
      if(weight_mean<=20){
        weight=a
      }else if(weight_mean>=25){
        weight=0
      }else{
        weight=a-a/5*(weight_mean-20)
      }
    }
    if(weight_mean>=60){
      if(weight_mean>=70){
        weight=b
      }else if(weight_mean<=65){
        weight=0
      }else{
        weight=b/5*(weight_mean-65)
      }
    }
    if(weight_mean<60&&weight_mean>30){
      weight=0
    }
    p=weight
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- floor(nrow(dat) * p)
      out <- sample(dat$index, size = num, replace=TRUE)
    }
    out
  }
  parameter_calculate<-function(table){
    right1_use=filter(table,label==3)
    right1=sum(right1_use$n)
    left1_use1=filter(table,label==1&mean>20&mean<25)
    left11=sum((5-left1_use1$mean/5)*left1_use1$n)
    left1_use2=filter(table,label==1&mean<=20)
    left12=sum(left1_use2$n)
    left1_use3=filter(table,label==1&mean>=25)
    left13=sum(left1_use3$n)
    a=(right1-sum(left1_use1$n)-sum(left1_use2$n)-sum(left1_use3$n))/(left11+left12)
    left2_use1=filter(table,label==2&mean<70&mean>65)
    left21=sum((left2_use1$mean-65)/5*left2_use1$n)
    left2_use2=filter(table,label==2&mean>=70)
    left22=sum(left2_use2$n)
    left2_use3=filter(table,label==2&mean<=65)
    left23=sum(left2_use3$n)
    b=(right1-sum(left2_use1$n)-sum(left2_use2$n)-sum(left2_use3$n))/(left21+left22)
    c(a=a,b=b)
  }
  label=sapply(z,count_no)
  temp=data.frame(y=z,index=seq(along = z),label=label)
  temp2=temp%>%group_by(label,y)%>%count()%>%mutate(mean=mean_label(y))
  temp3=as.data.frame(temp2)%>%group_by(label)
  parameter=parameter_calculate(temp3)
  a=parameter[1]
  b=parameter[2]
  x=c(0,20,25,65,70,100)
  y=c(a+1,a+1,1,1,b+1,b+1)
  y2=c(1,1,1,1,1,1)
  dataframe=data.frame(popularity=x,oversampling_probability=y,y2=y2)
  p=ggplot(data=dataframe,aes(popularity,oversampling_probability))+geom_area(aes(y=y2),fill="#f47983",alpha=0.7)+geom_line(color="#bf3d22",size=1.2)+geom_area(fill="pink",alpha=0.5)+ scale_y_continuous(breaks=c(0,1,1.5),minor_breaks=NULL)+ scale_x_continuous(breaks=c(0,20,25,65,70,100), minor_breaks=NULL)
  p=p+geom_vline(xintercept = c(30,60),size=1,alpha=0.8,linetype="dashed")+geom_vline(xintercept =45,size=1,alpha=0.5,linetype="dotted")
  p
}


#-------
## plot2 for 3
plot_my_version6 <- function(z, times = 1,list = TRUE, groups = min(50, length(z))){
  z=na.omit(z)
  if (class(z)[1] == "Surv") 
    z <- z[, "time"]
  out <- vector(mode = "list", times)
  if (length(z) < 2) 
    stop("z must have at least 2 data points")
  if (groups < 2) 
    groups <- 2
  if (is.numeric(z)) {
    z <- cut(z, unique(quantile(z, probs = seq(0, 1, length = groups))), 
             include.lowest = TRUE)
  }else {
    xtab <- table(z)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", 
                    paste(names(xtab)[xtab == 0], sep = "", 
                          collapse = ", "), ") and these will be ignored"))
      z <- factor(as.character(z))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (", 
                    paste(names(xtab)[xtab == 1], sep = "", 
                          collapse = ", "), ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat,a,b) {
    weight_label=dat$y[1]
    weight_mean=mean(as.numeric(strsplit(gsub('[^0-9.]',',',weight_label),split=",")[[1]][2:3]))
    label=85/groups
    if(weight_mean<=30){
      if(weight_mean<=20){
        weight=a
      }else if(weight_mean>=25){
        weight=0
      }else{
        weight=a-a/5*(weight_mean-20)
      }
    }
    if(weight_mean>=60){
      if(weight_mean>=70){
        weight=b
      }else if(weight_mean<=65){
        weight=0
      }else{
        weight=b/5*(weight_mean-65)
      }
    }
    if(weight_mean<60&&weight_mean>30){
      if(weight_mean<50&&weight_mean>40){
        weight=0.5
      }else if(weight_mean<=40&&weight_mean>35){
        weight=0.5/5*(weight_mean-35)
      }else if(weight_mean>=50&&weight_mean<55){
        weight=0.5-0.5/5*(weight_mean-50)
      }else{
        weight=0
      }
    }
    p=weight
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- floor(nrow(dat) * p)
      out <- sample(dat$index, size = num, replace=TRUE)
    }
    out
  }
  parameter_calculate<-function(table){
    right1_use=filter(table,label==3&mean<40&mean>=35)
    right1=sum((1+(right1_use$mean-35)/10)*right1_use$n)
    right2_use=filter(table,label==3&mean>50&mean<=55)
    right2=sum((1.5-(right2_use$mean-50)/10)*right2_use$n)
    right3_use=filter(table,label==3&(mean<=50&mean>=40))
    right3=sum(1.5*right3_use$n)
    right4_use=filter(table,label==3&(mean<=35|mean>=55))
    right4=sum(right4_use$n)
    left1_use1=filter(table,label==1&mean>20&mean<25)
    left11=sum((5-left1_use1$mean/5)*left1_use1$n)
    left1_use2=filter(table,label==1&mean<=20)
    left12=sum(left1_use2$n)
    left1_use3=filter(table,label==1&mean>=25)
    left13=sum(left1_use3$n)
    a=(right1+right2+right3+right4-sum(left1_use1$n)-sum(left1_use2$n)-sum(left1_use3$n))/(left11+left12)
    left2_use1=filter(table,label==2&mean<70&mean>65)
    left21=sum((left2_use1$mean-65)/5*left2_use1$n)
    left2_use2=filter(table,label==2&mean>=70)
    left22=sum(left2_use2$n)
    left2_use3=filter(table,label==2&mean<=65)
    left23=sum(left2_use3$n)
    b=(right1+right2+right3+right4-sum(left2_use1$n)-sum(left2_use2$n)-sum(left2_use3$n))/(left21+left22)
    c(a=a,b=b)
  }
  label=sapply(z,count_no)
  temp=data.frame(y=z,index=seq(along = z),label=label)
  temp2=temp%>%group_by(label,y)%>%count()%>%mutate(mean=mean_label(y))
  temp3=as.data.frame(temp2)%>%group_by(label)
  parameter=parameter_calculate(temp3)
  a=parameter[1]
  b=parameter[2]
  x=c(0,20,25,35,40,50,55,65,70,100)
  y=c(a+1,a+1,1,1,1.5,1.5,1,1,b+1,b+1)
  y2=c(1,1,1,1,1,1,1,1,1,1)
  dataframe=data.frame(popularity=x,oversampling_probability=y,y2=y2)
  p=ggplot(data=dataframe,aes(popularity,oversampling_probability))+geom_area(aes(y=y2),fill="skyblue",alpha=0.7)+geom_line(color="dodgerblue",size=1.2)+geom_area(fill="lightblue",alpha=0.5)+ scale_y_continuous(breaks=c(0,1,1.5),minor_breaks=NULL)+ scale_x_continuous(breaks=c(0,20,25,35,40,50,55,65,70,100), minor_breaks=NULL)
  p=p+geom_vline(xintercept = c(30,60),size=1,alpha=0.8,linetype="dashed")+geom_vline(xintercept =46,size=1,alpha=0.5,linetype="dotted")
  p
}



