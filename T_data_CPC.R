#process temperature data from CPC Global Daily Temperature dataset
require(ncdf4)
library(ncdf4)
ncfilemax <- "tmax.2019"
ncfilemin <- "tmin.2019"

open_nc_byname <- function(filename){
  ncfname <- paste(filename, ".nc", sep = "")
  ncin <- nc_open(ncfname)
  data <- ncvar_get(ncin)
  return(data)
}

datamax <- open_nc_byname(ncfilemax)
datamin <- open_nc_byname(ncfilemin)
dim(datamax)
lastdaymax<-datamax[,,204]
lastdaymin<-datamin[,,204]


retrieve_temprature <- function(day_data,lon,lat){
  if (lon < 0){
    lon_f<- 360 + as.numeric(lon)
  }
  else{
    lon_f<-as.numeric(lon)
  }
  
  idx_lon = as.integer((lon_f)*2)
  idx_lat = as.integer((90-as.numeric(lat))*2)
  
  return (day_data[idx_lon,idx_lat])
}
news <- read.csv("news_raw_contents.csv",header = FALSE,sep='|',stringsAsFactors = FALSE)

str(news)
df_news <- data.frame(news)
names(df_news) <-c('id','lat','lon','city','txt_content')
df_loc_news<-df_news[!is.na(df_news$lon),]
one_record <- df_loc_news[3, 5]

resmax<-list()
resmin<-list()
for (i in 1:length(df_loc_news$lon)){
  lon = df_loc_news[i,]$lon
  lat = df_loc_news[i,]$lat
  low<-retrieve_temprature(lastdaymin,lon,lat)
  high<-retrieve_temprature(lastdaymax,lon,lat)
  resmin[i]<-low
  resmax[i]<-high
}


df_max <- data.frame(matrix(unlist(resmax), nrow=length(resmax), byrow=T))
names(df_max) <- 'Tmax'
df_min <- data.frame(matrix(unlist(resmin), nrow=length(resmin), byrow=T))
names(df_min) <- 'Tmin'

combineT <- cbind(df_max,df_min)
allData <- cbind(df_loc_news,combineT)

allData <- allData[!is.na(allData[,'Tmax']),]

#visualization
library(reshape2)
library(ggplot2)

longData<-melt(lastdaymax)
longData<-longData[!is.na(longData$value),]

ggplot(longData, aes(x = Var1, y = Var2)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="letters", y="LETTERS", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))



# 
# 
# t <- ncvar_get(ncin,"time")
# t
# lat <- ncvar_get(ncin,"lat",verbose=F)
# nlat <- dim(lat)
# head(lat)
# 
# lon <- ncvar_get(ncin,"lon")
# nlon <- dim(lon)
# lon
# 
# tunits <- ncatt_get(ncin,"time","units")
# tunits
# data<-ncvar_get(ncin)
# dim(data)
# lastday<-data[,,204]
# lastday[397,113]
# 
# 
# test<-retrieve_temprature(lastday,18.87,-33.3505)
# 
# 
# news <- read.csv("news_raw_contents.csv",header = FALSE,sep='|')
# typeof(news)
# df_news <- data.frame(news)
# df_loc_news<-df_news[!is.na(df_news$V2),]
# 
# length(df_loc_news$V1)
# df_loc_news[4,]$V2
# 
# library(dplyr)
# res<-as.data.frame((res))
# df_loc_news <- bind_cols(df_loc_news,res)