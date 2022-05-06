library(ggplot2)
library(GGally)
library(cowplot)
library(dplyr)
library(tidyverse)
library(skimr)
library(mgsub)
library(RColorBrewer)

# EDA on PopUSA dataset
## song number each year
PopUSA_num_everyyear = group_by(PopUSA,year)%>%select(track_name,artist_name)%>%unique()%>%summarise(length(track_name))%>%rename(number=2)
(p1=ggplot(PopUSA_num_everyyear,aes(x=year,y=number))+geom_point(color="midnightblue")+geom_line(color="dodgerblue")+ggtitle("The number of songs taken into account")) +theme(plot.title = element_text(hjust = 0.5))  # song number accounted in the dataset for every year is not the same.

## song number based on cluster    
(p2<- ggplot(PopUSA,aes(x=cluster))+geom_bar(width=0.5)+theme(plot.title = element_text(hjust = 0.5)))

#------------------------------
# EDA on the feature dataset
## a skim on data
head(data)
skim(data)

## popularity distribution
(p3<- ggplot(data,aes(x=popularity))+geom_histogram(bins=15,fill="lightblue",color="white",aes(y=..density..))+geom_density(color="dodgerblue")+labs(title="popularity distribution"))+theme(plot.title = element_text(hjust = 0.5))

## correlation of popularity and common used variables
ggpairs(select(data,duration_ms,loudness,speechiness,danceability,valence,energy,key,popularity),lower=list(continuous = wrap("points", alpha = 0.2), combo = wrap("dot_no_facet", alpha = 0.4)))+theme(axis.text = element_text(colour = "black", size = 5),strip.text = element_text(colour = "white", size =11,face = "bold"),strip.background = element_rect(fill = "#d63d2d"))

p4<-ggcorr(select(as.data.frame(data),popularity,hTopic_01,hTopic_02,hTopic_03,hTopic_04,hTopic_05,hTopic_06,hTopic_07,hTopic_08),size=2.5,label=TRUE,label_size=3)
p5<-ggcorr(select(as.data.frame(data),popularity,tTopic_01,tTopic_02,tTopic_03,tTopic_04,tTopic_05,tTopic_06,tTopic_07,tTopic_08),size=2.5,label=TRUE,label_size=3)
p6<-plot_grid(p4,p5,ncol=2)
title<- ggdraw()+draw_label("correlation between popularity and topic variables",fontface="bold",size=15)
plot_grid(title,p6,nrow=2,rel_heights=c(0.2,2))

## correlation of popularity and some newly introduced variables 
ggpairs(select(data,year_board,count,covering_density1,name_complexity,artist_popularity,popularity)%>%rename(baseline=covering_density1),lower=list(continuous = wrap("points", alpha = 0.2), combo = wrap("dot_no_facet", alpha = 0.4)))+theme(axis.text = element_text(colour = "black", size = 5),strip.text = element_text(colour = "white", size =11,face = "bold"),strip.background = element_rect(fill = "#d63d2d"))
ggplot(data,aes(y=name_complexity,x=artist_popularity,color=popularity),alpha=0.01)+geom_point()+scale_color_gradient2(low = "dodgerblue",mid="white",high = "red",midpoint = 40)
ggplot(data,aes(y=covering_density1,x=artist_popularity,color=popularity),alpha=0.01)+geom_point()+scale_color_gradient2(low = "dodgerblue",mid="white",high = "red",midpoint = 40)

### subsequent covering version cannot be modelled as homogeneous Poisson process
#### At least 15% of the processes cannot be vied as Poisson process. And take a superficial look at the temporal distribution of events occurrence for some sampling tracks:
PoisP_test<-function(timelist){
  intervallist=unique(as.numeric(diff(timelist))/365)
  ks.test(intervallist,"pexp")$p.value
}
covering_info<-full_join(full_join(full_join(song_70s,song_80s),song_80s),song_75s)%>%as.data.frame()%>%group_by(number)%>%filter(length(unique(release_date))>1)%>%summarize(fitness=PoisP_test(unique(release_date)))%>%arrange(fitness)%>%mutate(test_no=1:2221,adjusted_p_value=(2221-test_no+1)*fitness)%>%mutate(color=(adjusted_p_value<=0.05))
ggplot(covering_info,aes(x=test_no,y=adjusted_p_value))+geom_segment(aes(x=279,y=0,xend=279,yend=0.05),color="black",linetype="dotted")+geom_point(aes(color=color))+geom_hline(yintercept =0.05,color="red")+scale_x_continuous(breaks=c(0,279,500,1000,1500,2000,2221)) +scale_y_log10(breaks=c(1e-10,1e-06,1e-02,0.05,1e+02))+ggtitle("Hochberg multiple hypothesis testing for Poisson process")+theme(plot.title = element_text(hjust = 0.5))+labs(fill="name")
