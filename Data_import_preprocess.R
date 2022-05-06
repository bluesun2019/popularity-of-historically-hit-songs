# It takes 6 hours or more to extract data from Spotify.
# Load covering.RData instead. 


# extract the covering dataset.
install.packages("spotifyr")
library(spotifyr)
library(dplyr)
library(lubridate)
library(billboard)

PopUSA <- read_csv("EvolutionPopUSA_MainData.csv")
PopUSA <- PopUSA%>%arrange(first_entry)
PopUSA <- PopUSA[1:27]

Sys.setenv(SPOTIFY_CLIENT_ID = "0c8667a9cf724c1fbb6dc85e583b448b")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "534929bc286244afa8e7f6c3899f6e77")

access_token <- get_spotify_access_token()

covering=c()
for(i in c(1:859,3765:4370,6741:7249,5012:5886,1671:2788)){   # 1970-1972,1980-1982,1990-1992,1974-1976
  tryCatch({
    record=search_spotify(PopUSA$track_name[i],"track",authorization = get_spotify_access_token())
    popularity=c()
    release_date=c()
    track_name=c()
    artist_name= c()
    for(j in 1:dim(record)[1]){
      track_name=c(track_name,record$name[j])
      artist_name=c(artist_name,paste(record[j,][[1]][[1]]$name,collapse=","))
      popularity=c(popularity,record$popularity[j])
      release_date=c(release_date,record[j,]$album.release_date)
    }
    covering_temp=get_track_audio_features(record$id,authorization = get_spotify_access_token())%>%select(-uri,-track_href,-analysis_url,-time_signature)
    covering_temp=data.frame(covering_temp,popularity,release_date,track_name,artist_name)
    covering=rbind(covering,covering_temp)
    cat(i,"\n")
  },error=function(e){cat("error\n")})
}




# data cleaning and variable generation (More specifically, popularity and point process related predictors)
# Note there is no need to normalize variables in the prediction using XGBoost.
Harr_wavelet<-function(time_point,release_date,J){
  T=as.numeric(Sys.Date()-release_date)
  time_point=as.numeric(time_point-release_date)
  alpha=c()
  for(k in 0:(2^J-1)){
    psi_sum=0
    for(i in 1:length(time_point)){
      if(2^J*time_point[i]/T-k>=0&2^J*time_point[i]/T-k<1){
        psi=2^{J/2}/sqrt(T)
      }else{
        psi=0
      }
      psi_sum=psi+psi_sum
    }
    alpha=c(alpha,psi_sum)
  }
  alpha
}

track_search=function(no){
  i=which(PopUSA_75s$number==no)
  songs_after=as.data.frame(filter(song_75s,number==no))
  temp2=filter(covering,grepl(tolower(PopUSA$track_name[no]),tolower(as.character(track_name)),fixed=TRUE))
  temp=grepl(tolower(PopUSA_75s$artist_name[i]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
  A=temp2%>%filter(grepl("-.*-",as.character(release_date)))%>%mutate(release_date=as.Date(release_date))
  B=temp2%>%filter(!grepl("-",as.character(release_date)))
  C=B%>%filter(as.numeric(as.character(release_date))==year(PopUSA$first_entry[no]))%>%mutate(release_date=PopUSA$first_entry[no]+30)
  D=B%>%filter(as.numeric(as.character(release_date))!=year(PopUSA$first_entry[no]))%>%mutate(release_date=as.Date(release_date,"%Y"))
  E=temp2%>%filter(!grepl("-.*-",as.character(release_date)))%>%filter(grepl("-",as.character(release_date)))
  if(dim(E)[1]!=0){
    E=E%>%mutate(release_date=as.Date(paste(as.character(release_date),"-01",sep = ""))+30 )
    temp2<-full_join(full_join(full_join(A,C),D),E)%>%mutate(number=no)
  }else{
    temp2<-full_join(full_join(A,C),D)%>%mutate(number=no)
  }}

tmp=count(PopUSA,artist_hit= artist_name)
tmp=count(PopUSA,artist_name)
PopUSA$artist_popularity=NA
for(i in 1:dim(PopUSA)[1]){
  temp=search_spotify(PopUSA$artist_name[i])
  if(!is.null(temp$artists$items$popularity[1])){
    PopUSA$artist_popularity[i]=temp$artists$items$popularity[1]
  }
  cat(i,"\n")
}

## select exactly songs that match each other
song_70s<-c()
for(i in 1:859){
  temp=filter(covering,grepl(tolower(PopUSA$track_name[i]),tolower(as.character(track_name)),fixed=TRUE))
  A=temp%>%filter(grepl("-.*-",as.character(release_date)))%>%filter(as.Date(release_date)>PopUSA$first_entry[i])%>%mutate(release_date=as.Date(release_date))
  B=temp%>%filter(!grepl("-",as.character(release_date)))%>%filter(as.numeric(as.character(release_date))>=year(PopUSA$first_entry[i]))
  C=B%>%filter(as.numeric(as.character(release_date))==year(PopUSA$first_entry[i]))%>%mutate(release_date=PopUSA$first_entry[i]+30)
  D=B%>%filter(as.numeric(as.character(release_date))>year(PopUSA$first_entry[i]))%>%mutate(release_date=as.Date(release_date,"%Y"))
  E=temp%>%filter(!grepl("-.*-",as.character(release_date)))%>%filter(grepl("-",as.character(release_date)))
  if(dim(E)[1]!=0){
    E=E %>%filter(as.Date(paste(as.character(release_date),"-01",sep = ""))+30>PopUSA$first_entry[i])
    if(dim(E)[1]!=0){
      E=E%>%mutate(release_date=as.Date(paste(as.character(release_date),"-01",sep = ""))+30 )
      song_70s<-rbind(song_70s,full_join(full_join(full_join(A,C),D),E)%>%mutate(number=i))
    }else{
      song_70s<-rbind(song_70s,full_join(full_join(A,C),D)%>%mutate(number=i))
    }
  }else{
    song_70s<-rbind(song_70s,full_join(full_join(A,C),D)%>%mutate(number=i))
  }
  cat(i,"\n")
}
song_70s<-song_70s%>%group_by(number)%>%arrange(number,release_date)
song_70s<-filter(song_70s,!number%in%c(3,5))


PopUSA_70s=PopUSA[1:859,]
popularity=c()
speechiness=c()
danceability=c()
duration_ms=c()
for(i in 1:859){
  temp=filter(covering,grepl(tolower(PopUSA$track_name[i]),tolower(as.character(track_name)),fixed=TRUE))
  if(dim(temp)[1]!=0){
    A=temp%>%filter(grepl("-.*-",as.character(release_date)))%>%mutate(release_date=as.Date(release_date))
    B=temp%>%filter(!grepl("-",as.character(release_date)))
    C=B%>%filter(as.numeric(as.character(release_date))==year(PopUSA$first_entry[i]))%>%mutate(release_date=PopUSA$first_entry[i]-30)
    D=B%>%filter(as.numeric(as.character(release_date))!=year(PopUSA$first_entry[i]))%>%mutate(release_date=as.Date(release_date,"%Y"))
    E=temp%>%filter(!grepl("-.*-",as.character(release_date)))%>%filter(grepl("-",as.character(release_date)))
    if(dim(E)[1]!=0){
      E=E %>%mutate(release_date=as.Date(paste(as.character(release_date),"-01",sep = ""))-30)
    }else{
      songs_before<-full_join(full_join(A,C),D)%>%mutate(number=i)
    }
    songs_before=songs_before%>%arrange(desc(popularity),desc(release_date))
    temp=grepl(tolower(PopUSA_70s$artist_name[i]),tolower(select(songs_before,artist_name)[,1]),fixed=TRUE)
    if(dim(songs_before)[1]!=0){
      if(any(temp)){
        popularity=c(popularity,filter(songs_before,temp)$popularity[1])
        speechiness=c(speechiness,filter(songs_before,temp)$speechiness[1])
        duration_ms=c(duration_ms,filter(songs_before,temp)$duration_ms[1])
        danceability=c(danceability,filter(songs_before,temp)$danceability[1])
      }else{
        songs_after=as.data.frame(filter(song_70s,number==i))%>%arrange(desc(popularity),release_date)
        temp=grepl(tolower(PopUSA_70s$artist_name[i]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
        if(is.na(tolower(PopUSA_70s$artist_name[i]))){
          popularity=c(popularity,NA)
          speechiness=c(speechiness,NA)
          duration_ms=c(duration_ms,NA)
          danceability=c(danceability,NA)
        }else{
          if(any(temp)){
            popularity=c(popularity,filter(songs_after,temp)$popularity[1])
            speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
            duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
            danceability=c(danceability,filter(songs_after,temp)$danceability[1])
          }else{
            popularity=c(popularity,NA)
            speechiness=c(speechiness,NA)
            duration_ms=c(duration_ms,NA)
            danceability=c(danceability,NA)
          }
        }
      }
    }else{
      songs_after=as.data.frame(filter(song_70s,number==i))%>%arrange(desc(popularity),release_date)
      temp=grepl(tolower(PopUSA_70s$artist_name[i]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
      if(any(temp)){
        popularity=c(popularity,filter(songs_after,temp)$popularity[1])
        speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
        duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
        danceability=c(danceability,filter(songs_after,temp)$danceability[1])
      }else{
        popularity=c(popularity,NA)
        speechiness=c(speechiness,NA)
        duration_ms=c(duration_ms,NA)
        danceability=c(danceability,NA)
      }
    }
  }else{
    popularity=c(popularity,NA)
    speechiness=c(speechiness,NA)
    duration_ms=c(duration_ms,NA)
    danceability=c(danceability,NA)
  }
  cat(i,"\n")
}
PopUSA_70s=as.data.frame(cbind(PopUSA_70s,popularity,speechiness,duration_ms,number=1:859))

#------------------------------
song_80s<-c()
for(i in 3765:4370){
  temp=filter(covering,grepl(tolower(PopUSA$track_name[i]),tolower(as.character(track_name)),fixed=TRUE))
  A=temp%>%filter(grepl("-.*-",as.character(release_date)))%>%filter(as.Date(release_date)>PopUSA$first_entry[i])%>%mutate(release_date=as.Date(release_date))
  B=temp%>%filter(!grepl("-",as.character(release_date)))%>%filter(as.numeric(as.character(release_date))>=year(PopUSA$first_entry[i]))
  C=B%>%filter(as.numeric(as.character(release_date))==year(PopUSA$first_entry[i]))%>%mutate(release_date=PopUSA$first_entry[i]+30)
  D=B%>%filter(as.numeric(as.character(release_date))>year(PopUSA$first_entry[i]))%>%mutate(release_date=as.Date(release_date,"%Y"))
  E=temp%>%filter(!grepl("-.*-",as.character(release_date)))%>%filter(grepl("-",as.character(release_date)))
  if(dim(E)[1]!=0){
    E=E %>%filter(as.Date(paste(as.character(release_date),"-01",sep = ""))+30>PopUSA$first_entry[i])
    if(dim(E)[1]!=0){
      E=E%>%mutate(release_date=as.Date(paste(as.character(release_date),"-01",sep = ""))+30 )
      song_80s<-rbind(song_80s,full_join(full_join(full_join(A,C),D),E)%>%mutate(number=i))
    }else{
      song_80s<-rbind(song_80s,full_join(full_join(A,C),D)%>%mutate(number=i))
    }
  }else{
    song_80s<-rbind(song_80s,full_join(full_join(A,C),D)%>%mutate(number=i))
  }
  cat(i,"\n")
}
song_80s<-song_80s%>%group_by(number)%>%arrange(number,release_date)
song_80s<-filter(song_80s,!number%in%c(3,5))

PopUSA_80s=PopUSA[3765:4370,]
popularity=c()
speechiness=c()
danceability=c()
duration_ms=c()
for(i in 3765:4370){
  temp=filter(covering,grepl(tolower(PopUSA$track_name[i]),tolower(as.character(track_name)),fixed=TRUE))
  if(dim(temp)[1]!=0){
    A=temp%>%filter(grepl("-.*-",as.character(release_date)))%>%mutate(release_date=as.Date(release_date))
    B=temp%>%filter(!grepl("-",as.character(release_date)))
    C=B%>%filter(as.numeric(as.character(release_date))==year(PopUSA$first_entry[i]))%>%mutate(release_date=PopUSA$first_entry[i]-30)
    D=B%>%filter(as.numeric(as.character(release_date))!=year(PopUSA$first_entry[i]))%>%mutate(release_date=as.Date(release_date,"%Y"))
    E=temp%>%filter(!grepl("-.*-",as.character(release_date)))%>%filter(grepl("-",as.character(release_date)))
    if(dim(E)[1]!=0){
      E=E %>%mutate(release_date=as.Date(paste(as.character(release_date),"-01",sep = ""))-30)
    }else{
      songs_before<-full_join(full_join(A,C),D)%>%mutate(number=i)
    }
    songs_before=songs_before%>%arrange(desc(popularity),desc(release_date))
    temp=grepl(tolower(PopUSA_80s$artist_name[i-3764]),tolower(select(songs_before,artist_name)[,1]),fixed=TRUE)
    if(dim(songs_before)[1]!=0){
      if(any(temp)){
        popularity=c(popularity,filter(songs_before,temp)$popularity[1])
        speechiness=c(speechiness,filter(songs_before,temp)$speechiness[1])
        duration_ms=c(duration_ms,filter(songs_before,temp)$duration_ms[1])
        danceability=c(danceability,filter(songs_before,temp)$danceability[1])
      }else{
        songs_after=as.data.frame(filter(song_80s,number==i))%>%arrange(desc(popularity),release_date)
        temp=grepl(tolower(PopUSA_80s$artist_name[i]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
        if(is.na(tolower(PopUSA_80s$artist_name[i]))){
          popularity=c(popularity,NA)
          speechiness=c(speechiness,NA)
          duration_ms=c(duration_ms,NA)
          danceability=c(danceability,NA)
        }else{
          if(any(temp)){
            popularity=c(popularity,filter(songs_after,temp)$popularity[1])
            speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
            duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
            danceability=c(danceability,filter(songs_after,temp)$danceability[1])
          }else{
            popularity=c(popularity,NA)
            speechiness=c(speechiness,NA)
            duration_ms=c(duration_ms,NA)
            danceability=c(danceability,NA)
          }
        }
      }
    }else{
      songs_after=as.data.frame(filter(song_80s,number==i))%>%arrange(desc(popularity),release_date)
      temp=grepl(tolower(PopUSA_80s$artist_name[i-3764]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
      if(any(temp)){
        popularity=c(popularity,filter(songs_after,temp)$popularity[1])
        speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
        duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
        danceability=c(danceability,filter(songs_after,temp)$danceability[1])
      }else{
        popularity=c(popularity,NA)
        speechiness=c(speechiness,NA)
        duration_ms=c(duration_ms,NA)
        danceability=c(danceability,NA)
      }
    }
  }else{
    popularity=c(popularity,NA)
    speechiness=c(speechiness,NA)
    duration_ms=c(duration_ms,NA)
    danceability=c(danceability,NA)
  }
  cat(i,"\n")
}
PopUSA_80s=as.data.frame(cbind(PopUSA_80s,popularity,speechiness,duration_ms,number=3765:4370))

#------------------------------
song_90s<-c()
for(i in 6741:7249){
  temp=filter(covering,grepl(tolower(PopUSA$track_name[i]),tolower(as.character(track_name)),fixed=TRUE))
  A=temp%>%filter(grepl("-.*-",as.character(release_date)))%>%filter(as.Date(release_date)>PopUSA$first_entry[i])%>%mutate(release_date=as.Date(release_date))
  B=temp%>%filter(!grepl("-",as.character(release_date)))%>%filter(as.numeric(as.character(release_date))>=year(PopUSA$first_entry[i]))
  C=B%>%filter(as.numeric(as.character(release_date))==year(PopUSA$first_entry[i]))%>%mutate(release_date=PopUSA$first_entry[i]+30)
  D=B%>%filter(as.numeric(as.character(release_date))>year(PopUSA$first_entry[i]))%>%mutate(release_date=as.Date(release_date,"%Y"))
  E=temp%>%filter(!grepl("-.*-",as.character(release_date)))%>%filter(grepl("-",as.character(release_date)))
  if(dim(E)[1]!=0){
    E=E %>%filter(as.Date(paste(as.character(release_date),"-01",sep = ""))+30>PopUSA$first_entry[i])
    if(dim(E)[1]!=0){
      E=E%>%mutate(release_date=as.Date(paste(as.character(release_date),"-01",sep = ""))+30 )
      song_90s<-rbind(song_90s,full_join(full_join(full_join(A,C),D),E)%>%mutate(number=i))
    }else{
      song_90s<-rbind(song_90s,full_join(full_join(A,C),D)%>%mutate(number=i))
    }
  }else{
    song_90s<-rbind(song_90s,full_join(full_join(A,C),D)%>%mutate(number=i))
  }
  cat(i,"\n")
}
song_90s<-song_90s%>%group_by(number)%>%arrange(number,release_date)
song_90s<-filter(song_90s,!number%in%c(3,5))

PopUSA_90s=PopUSA[6741:7249,]
popularity=c()
speechiness=c()
danceability=c()
duration_ms=c()
for(i in 6741:7249){
  temp=filter(covering,grepl(tolower(PopUSA$track_name[i]),tolower(as.character(track_name)),fixed=TRUE))
  if(dim(temp)[1]!=0){
    A=temp%>%filter(grepl("-.*-",as.character(release_date)))%>%mutate(release_date=as.Date(release_date))
    B=temp%>%filter(!grepl("-",as.character(release_date)))
    C=B%>%filter(as.numeric(as.character(release_date))==year(PopUSA$first_entry[i]))%>%mutate(release_date=PopUSA$first_entry[i]-30)
    D=B%>%filter(as.numeric(as.character(release_date))!=year(PopUSA$first_entry[i]))%>%mutate(release_date=as.Date(release_date,"%Y"))
    E=temp%>%filter(!grepl("-.*-",as.character(release_date)))%>%filter(grepl("-",as.character(release_date)))
    if(dim(E)[1]!=0){
      E=E %>%mutate(release_date=as.Date(paste(as.character(release_date),"-01",sep = ""))-30)
    }else{
      songs_before<-full_join(full_join(A,C),D)%>%mutate(number=i)
    }
    songs_before=songs_before%>%arrange(desc(popularity),desc(release_date))
    temp=grepl(tolower(PopUSA_90s$artist_name[i-6740]),tolower(select(songs_before,artist_name)[,1]),fixed=TRUE)
    if(dim(songs_before)[1]!=0){
      if(any(temp)){
        popularity=c(popularity,filter(songs_before,temp)$popularity[1])
        speechiness=c(speechiness,filter(songs_before,temp)$speechiness[1])
        duration_ms=c(duration_ms,filter(songs_before,temp)$duration_ms[1])
        danceability=c(danceability,filter(songs_before,temp)$danceability[1])
      }else{
        songs_after=as.data.frame(filter(song_90s,number==i))%>%arrange(desc(popularity),release_date)
        temp=grepl(tolower(PopUSA_90s$artist_name[i]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
        if(is.na(tolower(PopUSA_90s$artist_name[i]))){
          popularity=c(popularity,NA)
          speechiness=c(speechiness,NA)
          duration_ms=c(duration_ms,NA)
          danceability=c(danceability,NA)
        }else{
          if(any(temp)){
            popularity=c(popularity,filter(songs_after,temp)$popularity[1])
            speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
            duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
            danceability=c(danceability,filter(songs_after,temp)$danceability[1])
          }else{
            popularity=c(popularity,NA)
            speechiness=c(speechiness,NA)
            duration_ms=c(duration_ms,NA)
            danceability=c(danceability,NA)
          }
        }
      }
    }else{
      songs_after=as.data.frame(filter(song_90s,number==i))%>%arrange(desc(popularity),release_date)
      temp=grepl(tolower(PopUSA_90s$artist_name[i-6740]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
      if(any(temp)){
        popularity=c(popularity,filter(songs_after,temp)$popularity[1])
        speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
        duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
        danceability=c(danceability,filter(songs_after,temp)$danceability[1])
      }else{
        popularity=c(popularity,NA)
        speechiness=c(speechiness,NA)
        duration_ms=c(duration_ms,NA)
        danceability=c(danceability,NA)
      }
    }
  }else{
    popularity=c(popularity,NA)
    speechiness=c(speechiness,NA)
    duration_ms=c(duration_ms,NA)
    danceability=c(danceability,NA)
  }
  cat(i,"\n")
}

PopUSA_90s=as.data.frame(cbind(PopUSA_90s,popularity,speechiness,duration_ms,number=6741:7249))

#####   -------------------------------
song_75s<-c()
for(i in 1671:2788){
  temp=filter(covering,grepl(tolower(PopUSA$track_name[i]),tolower(as.character(track_name)),fixed=TRUE))
  A=temp%>%filter(grepl("-.*-",as.character(release_date)))%>%filter(as.Date(release_date)>PopUSA$first_entry[i])%>%mutate(release_date=as.Date(release_date))
  B=temp%>%filter(!grepl("-",as.character(release_date)))%>%filter(as.numeric(as.character(release_date))>=year(PopUSA$first_entry[i]))
  C=B%>%filter(as.numeric(as.character(release_date))==year(PopUSA$first_entry[i]))%>%mutate(release_date=PopUSA$first_entry[i]+30)
  D=B%>%filter(as.numeric(as.character(release_date))>year(PopUSA$first_entry[i]))%>%mutate(release_date=as.Date(release_date,"%Y"))
  E=temp%>%filter(!grepl("-.*-",as.character(release_date)))%>%filter(grepl("-",as.character(release_date)))
  if(dim(E)[1]!=0){
    E=E %>%filter(as.Date(paste(as.character(release_date),"-01",sep = ""))+30>PopUSA$first_entry[i])
    if(dim(E)[1]!=0){
      E=E%>%mutate(release_date=as.Date(paste(as.character(release_date),"-01",sep = ""))+30 )
      song_75s<-rbind(song_75s,full_join(full_join(full_join(A,C),D),E)%>%mutate(number=i))
    }else{
      song_75s<-rbind(song_75s,full_join(full_join(A,C),D)%>%mutate(number=i))
    }
  }else{
    song_75s<-rbind(song_75s,full_join(full_join(A,C),D)%>%mutate(number=i))
  }
  cat(i,"\n")
}
song_75s<-song_75s%>%group_by(number)%>%arrange(number,release_date)
song_75s<-filter(song_75s,!number%in%c(3,5))

PopUSA_75s=PopUSA[1671:2788,]
popularity=c()
speechiness=c()
danceability=c()
duration_ms=c()
for(i in 1671:2788){
  temp=filter(covering,grepl(tolower(PopUSA$track_name[i]),tolower(as.character(track_name)),fixed=TRUE))
  if(dim(temp)[1]!=0){
    A=temp%>%filter(grepl("-.*-",as.character(release_date)))%>%mutate(release_date=as.Date(release_date))
    B=temp%>%filter(!grepl("-",as.character(release_date)))
    C=B%>%filter(as.numeric(as.character(release_date))==year(PopUSA$first_entry[i]))%>%mutate(release_date=PopUSA$first_entry[i]-30)
    D=B%>%filter(as.numeric(as.character(release_date))!=year(PopUSA$first_entry[i]))%>%mutate(release_date=as.Date(release_date,"%Y"))
    E=temp%>%filter(!grepl("-.*-",as.character(release_date)))%>%filter(grepl("-",as.character(release_date)))
    if(dim(E)[1]!=0){
      E=E%>%mutate(release_date=as.Date(paste(as.character(release_date),"-01",sep = "")))
      songs_before<-full_join(full_join(full_join(A,C),D),E)%>%mutate(number=i)
    }else{
      songs_before<-full_join(full_join(A,C),D)%>%mutate(number=i)
    }
    songs_before=songs_before%>%arrange(desc(popularity),release_date)
    temp=grepl(tolower(PopUSA_75s$artist_name[i-1670]),tolower(select(songs_before,artist_name)[,1]),fixed=TRUE)
    if(dim(songs_before)[1]!=0){
      if(any(temp)){
        popularity=c(popularity,filter(songs_before,temp)$popularity[1])
        speechiness=c(speechiness,filter(songs_before,temp)$speechiness[1])
        duration_ms=c(duration_ms,filter(songs_before,temp)$duration_ms[1])
        danceability=c(danceability,filter(songs_before,temp)$danceability[1])
      }else{
        songs_after=as.data.frame(filter(song_75s,number==i))%>%arrange(desc(popularity),release_date)
        temp=grepl(tolower(PopUSA_75s$artist_name[i]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
        if(is.na(tolower(PopUSA_75s$artist_name[i]))){
          popularity=c(popularity,NA)
          speechiness=c(speechiness,NA)
          duration_ms=c(duration_ms,NA)
          danceability=c(danceability,NA)
        }else{
          if(any(temp)){
            popularity=c(popularity,filter(songs_after,temp)$popularity[1])
            speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
            duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
            danceability=c(danceability,filter(songs_after,temp)$danceability[1])
          }else{
            popularity=c(popularity,NA)
            speechiness=c(speechiness,NA)
            duration_ms=c(duration_ms,NA)
            danceability=c(danceability,NA)
          }
        }
      }
    }else{
      songs_after=as.data.frame(filter(song_75s,number==i))%>%arrange(desc(popularity),release_date)
      temp=grepl(tolower(PopUSA_75s$artist_name[i-1670]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
      if(any(temp)){
        popularity=c(popularity,filter(songs_after,temp)$popularity[1])
        speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
        duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
        danceability=c(danceability,filter(songs_after,temp)$danceability[1])
      }else{
        popularity=c(popularity,NA)
        speechiness=c(speechiness,NA)
        duration_ms=c(duration_ms,NA)
        danceability=c(danceability,NA)
      }
    }
  }else{
    popularity=c(popularity,NA)
    speechiness=c(speechiness,NA)
    duration_ms=c(duration_ms,NA)
    danceability=c(danceability,NA)
  }
  cat(i,"\n")
}
PopUSA_75s=as.data.frame(cbind(PopUSA_75s,popularity,speechiness,duration_ms,number=1671:2788))


####   ----------------------------------------------------

name=c()
popularity=c()
count=c()
artist_popularity=c()
count_self=c()
cluster=c()
name_complexity=c()
wavelet=c()
loudness=c()
liveness=c()
energy=c()
danceability=c()
valence=c()
duration_ms=c()
speechiness=c()
key=c()
year_board=c()
time_duration=c()
htopic=c()
ttopic=c()
year_hot=billboard::wiki_hot_100s%>%filter(year=="1970"|year=="1971"|year=="1972")
for(i in 1:length(PopUSA_70s$number)){
  no=PopUSA_70s$number[i]
  track=as.data.frame(song_70s)%>%filter(number==no)
  track_point=c(track$release_date,recursive=TRUE)
  track_diff=as.numeric(diff(track$release_date))
  if(length(track_diff)!=0&length(track_diff)!=1){
    songs_after=as.data.frame(filter(song_70s,number==no))
    temp=grepl(tolower(PopUSA_70s$artist_name[i]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
    if(any(temp)){
      name=c(name,PopUSA_70s$track_name[i])
      popularity=c(popularity,PopUSA_70s$popularity[i])
      count=c(count,length(track_diff))
      self=filter(track,artist_name==PopUSA_70s$artist_name[i])
      not_self=filter(track,artist_name!=PopUSA_70s$artist_name[i])
      count_self=c(count_self,dim(self)[1])
      cluster=c(cluster,PopUSA_70s$cluster[i])
      artist_popularity=c(artist_popularity,PopUSA_70s$artist_popularity[i])
      name_complexity=c(name_complexity,nchar(PopUSA_70s$track_name[i]))
      wavelet=rbind(wavelet, Harr_wavelet(track_point,PopUSA_70s$first_entry[i],2))
      time_duration=c(time_duration,as.numeric(today()-PopUSA_70s$first_entry[i])/365)
      htopic=rbind(htopic,PopUSA_70s[i,]%>%select(starts_with("hTopic")))
      ttopic=rbind(ttopic,PopUSA_70s[i,]%>%select(starts_with("tTopic")))
      
      if(any(year_hot$title%in%PopUSA_70s$track_name[i])){
        year_board=c(year_board,1)
      }else{
        year_board=c(year_board,0)
      }
      duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
      danceability=c(danceability,filter(songs_after,temp)$danceability[1])
      loudness=c(loudness,filter(songs_after,temp)$loudness[1])
      liveness=c(liveness,filter(songs_after,temp)$liveness[1])
      speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
      energy=c(energy,filter(songs_after,temp)$energy[1])
      valence=c(valence,filter(songs_after,temp)$valence[1])
      key=c(key,filter(songs_after,temp)$key[1])
    }
  }
}
data_70s=data.frame(name,popularity,artist_popularity,count,count_self,n,cluster,name_complexity,wavelet,duration_ms,danceability,loudness,liveness,speechiness,energy,key,valence,year_board,time_duration,htopic,ttopic)

name=c()
popularity=c()
count=c()
artist_popularity=c()
count_self=c()
cluster=c()
name_complexity=c()
wavelet=c()
loudness=c()
liveness=c()
energy=c()
danceability=c()
valence=c()
duration_ms=c()
speechiness=c()
key=c()
year_board=c()
time_duration=c()
htopic=c()
ttopic=c()
year_hot=billboard::wiki_hot_100s%>%filter(year=="1980"|year=="1981"|year=="1982")
for(i in 1:length(PopUSA_80s$number)){
  no=PopUSA_80s$number[i]
  track=as.data.frame(song_80s)%>%filter(number==no)
  track_point=c(track$release_date,recursive=TRUE)
  track_diff=as.numeric(diff(track$release_date))
  if(length(track_diff)!=0&length(track_diff)!=1){
    songs_after=as.data.frame(filter(song_80s,number==no))
    temp=grepl(tolower(PopUSA_80s$artist_name[i]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
    if(any(temp)){
      name=c(name,PopUSA_80s$track_name[i])
      popularity=c(popularity,PopUSA_80s$popularity[i])
      count=c(count,length(track_diff))
      self=filter(track,artist_name==PopUSA_80s$artist_name[i])
      not_self=filter(track,artist_name!=PopUSA_80s$artist_name[i])
      count_self=c(count_self,dim(self)[1])
      cluster=c(cluster,PopUSA_80s$cluster[i])
      artist_popularity=c(artist_popularity,PopUSA_80s$artist_popularity[i])
      name_complexity=c(name_complexity,nchar(PopUSA_80s$track_name[i]))
      wavelet=rbind(wavelet, Harr_wavelet(track_point,PopUSA_80s$first_entry[i],2))
      time_duration=c(time_duration,as.numeric(today()-PopUSA_80s$first_entry[i])/365)
      htopic=rbind(htopic,PopUSA_80s[i,]%>%select(starts_with("hTopic")))
      ttopic=rbind(ttopic,PopUSA_80s[i,]%>%select(starts_with("tTopic")))
      
      if(any(year_hot$title%in%PopUSA_80s$track_name[i])){
        year_board=c(year_board,1)
      }else{
        year_board=c(year_board,0)
      }
      
      duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
      danceability=c(danceability,filter(songs_after,temp)$danceability[1])
      loudness=c(loudness,filter(songs_after,temp)$loudness[1])
      liveness=c(liveness,filter(songs_after,temp)$liveness[1])
      speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
      energy=c(energy,filter(songs_after,temp)$energy[1])
      valence=c(valence,filter(songs_after,temp)$valence[1])
      key=c(key,filter(songs_after,temp)$key[1])
    }
  }
}
data_80s=data.frame(name,popularity,artist_popularity,count,count_self,n,cluster,name_complexity,wavelet,duration_ms,danceability,loudness,liveness,speechiness,energy,key,valence,year_board,time_duration,htopic,ttopic)

name=c()
popularity=c()
count=c()
artist_popularity=c()
count_self=c()
cluster=c()
name_complexity=c()
wavelet=c()
loudness=c()
liveness=c()
energy=c()
danceability=c()
valence=c()
duration_ms=c()
speechiness=c()
key=c()
year_board=c()
time_duration=c()
htopic=c()
ttopic=c()
year_hot=billboard::wiki_hot_100s%>%filter(year=="1990"|year=="1991"|year=="1992")
for(i in 1:length(PopUSA_90s$number)){
  no=PopUSA_90s$number[i]
  track=as.data.frame(song_90s)%>%filter(number==no)
  track_point=c(track$release_date,recursive=TRUE)
  track_diff=as.numeric(diff(track$release_date))
  if(length(track_diff)!=0&length(track_diff)!=1){
    songs_after=as.data.frame(filter(song_90s,number==no))
    temp=grepl(tolower(PopUSA_90s$artist_name[i]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
    if(any(temp)){
      name=c(name,PopUSA_90s$track_name[i])
      popularity=c(popularity,PopUSA_90s$popularity[i])
      count=c(count,length(track_diff))
      self=filter(track,artist_name==PopUSA_90s$artist_name[i])
      not_self=filter(track,artist_name!=PopUSA_90s$artist_name[i])
      count_self=c(count_self,dim(self)[1])
      cluster=c(cluster,PopUSA_90s$cluster[i])
      artist_popularity=c(artist_popularity,PopUSA_90s$artist_popularity[i])
      name_complexity=c(name_complexity,nchar(PopUSA_90s$track_name[i]))
      wavelet=rbind(wavelet, Harr_wavelet(track_point,PopUSA_90s$first_entry[i],2))
      time_duration=c(time_duration,as.numeric(today()-PopUSA_90s$first_entry[i])/365)
      htopic=rbind(htopic,PopUSA_90s[i,]%>%select(starts_with("hTopic")))
      ttopic=rbind(ttopic,PopUSA_90s[i,]%>%select(starts_with("tTopic")))
      
      if(any(year_hot$title%in%PopUSA_90s$track_name[i])){
        year_board=c(year_board,1)
      }else{
        year_board=c(year_board,0)
      }
      
      duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
      danceability=c(danceability,filter(songs_after,temp)$danceability[1])
      loudness=c(loudness,filter(songs_after,temp)$loudness[1])
      liveness=c(liveness,filter(songs_after,temp)$liveness[1])
      speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
      energy=c(energy,filter(songs_after,temp)$energy[1])
      valence=c(valence,filter(songs_after,temp)$valence[1])
      key=c(key,filter(songs_after,temp)$key[1])
    }
  }
}
data_90s=data.frame(name,popularity,artist_popularity,count,count_self,n,cluster,name_complexity,wavelet,duration_ms,danceability,loudness,liveness,speechiness,energy,key,valence,year_board,time_duration,htopic,ttopic)

name=c()
popularity=c()
count=c()
artist_popularity=c()
count_self=c()
cluster=c()
name_complexity=c()
wavelet=c()
loudness=c()
liveness=c()
energy=c()
danceability=c()
valence=c()
duration_ms=c()
speechiness=c()
key=c()
year_board=c()
time_duration=c()
htopic=c()
ttopic=c()
year_hot=billboard::wiki_hot_100s%>%filter(year=="1975"|year=="1974"|year=="1976")
for(i in 1:length(PopUSA_75s$number)){
  no=PopUSA_75s$number[i]
  track=as.data.frame(song_75s)%>%filter(number==no)
  track_point=c(track$release_date,recursive=TRUE)
  track_diff=as.numeric(diff(track$release_date))
  if(length(track_diff)!=0&length(track_diff)!=1){
    songs_after=as.data.frame(filter(song_75s,number==no))
    temp=grepl(tolower(PopUSA_75s$artist_name[i]),tolower(select(songs_after,artist_name)[,1]),fixed=TRUE)
    if(any(temp)){
      name=c(name,PopUSA_75s$track_name[i])
      popularity=c(popularity,PopUSA_75s$popularity[i])
      count=c(count,length(track_diff))
      self=filter(track,artist_name==PopUSA_75s$artist_name[i])
      not_self=filter(track,artist_name!=PopUSA_75s$artist_name[i])
      count_self=c(count_self,dim(self)[1])
      cluster=c(cluster,PopUSA_75s$cluster[i])
      artist_popularity=c(artist_popularity,PopUSA_75s$artist_popularity[i])
      name_complexity=c(name_complexity,nchar(PopUSA_75s$track_name[i]))
      wavelet=rbind(wavelet, Harr_wavelet(track_point,PopUSA_75s$first_entry[i],2))
      time_duration=c(time_duration,as.numeric(today()-PopUSA_75s$first_entry[i])/365)
      htopic=rbind(htopic,PopUSA_75s[i,]%>%select(starts_with("hTopic")))
      ttopic=rbind(ttopic,PopUSA_75s[i,]%>%select(starts_with("tTopic")))
      
      if(any(year_hot$title%in%PopUSA_75s$track_name[i])){
        year_board=c(year_board,1)
      }else{
        year_board=c(year_board,0)
      }
      
      duration_ms=c(duration_ms,filter(songs_after,temp)$duration_ms[1])
      danceability=c(danceability,filter(songs_after,temp)$danceability[1])
      loudness=c(loudness,filter(songs_after,temp)$loudness[1])
      liveness=c(liveness,filter(songs_after,temp)$liveness[1])
      speechiness=c(speechiness,filter(songs_after,temp)$speechiness[1])
      energy=c(energy,filter(songs_after,temp)$energy[1])
      valence=c(valence,filter(songs_after,temp)$valence[1])
      key=c(key,filter(songs_after,temp)$key[1])
    }
  }
}
data_75s=data.frame(name,popularity,artist_popularity,count,count_self,cluster,name_complexity,wavelet,duration_ms,danceability,loudness,liveness,speechiness,energy,key,valence,year_board,time_duration,htopic,ttopic)

data=full_join(full_join(data_70s,data_80s),data_90s)%>%na.omit()%>%mutate(popularity_class=cut(popularity,breaks=c(0,30,60,100),include.lowest=TRUE))%>%mutate(covering_intensity1=X1,covering_intensity2=X2,covering_intensity3=X3,covering_intensity4=X4)%>%select(-X1,-X2,-X3,-X4)
data_75s=data_75s%>%na.omit()%>%mutate(popularity_class=cut(popularity,breaks=c(0,30,60,100),include.lowest=TRUE))%>%mutate(covering_intensity1=X1,covering_intensity2=X2,covering_intensity3=X3,covering_intensity4=X4)%>%select(-X1,-X2,-X3,-X4)


a <- ls()
rm(list=a[which(a != 'covering' & a != 'data_70s'& a != 'data_75s'& a != 'data_80s'& a != 'data_90s'& a != 'PopUSA'& a != 'PopUSA_70s'& a != 'PopUSA_80s'& a != 'PopUSA_90s'& a != 'PopUSA_75s'& a != 'song_70s'& a != 'song_80s'& a != 'song_90s'& a != 'song_75s'& a != 'data')])
rm(a)

save.image("Covering.RData")
