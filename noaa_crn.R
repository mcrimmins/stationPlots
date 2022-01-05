

## libraries
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)


(yrs<-2002:2021) # set range of years to download
#prefix<-"CRND0103-"
#suffix<-"-AZ_Elgin_5_S.txt"

prefix<-"CRND0103-"
suffix<-"-AZ_Tucson_11_W.txt"


  #paste0("https://www.ncei.noaa.gov/pub/data/uscrn/products/daily01/",yrs[i],"/",prefix,yrs[i],suffix)
  
  datalist = list()
  # download and build stacks
  for(i in 1:length(yrs)){
    URL<-paste0("https://www.ncei.noaa.gov/pub/data/uscrn/products/daily01/",yrs[i],"/",prefix,yrs[i],suffix)
    download.file(URL, destfile = "temp.txt")
    datalist[[i]] <- read.table("temp.txt",sep="") # add it to your list
    print(yrs[i])
  }
  
  allData = do.call(rbind, datalist)
  
  # add in colnames
  colnames(allData)<-c("WBANNO", "LST_DATE", "CRX_VN", "LONGITUDE", "LATITUDE", "T_DAILY_MAX", 
                "T_DAILY_MIN", "T_DAILY_MEAN", "T_DAILY_AVG", "P_DAILY_CALC", "SOLARAD_DAILY",
                "SUR_TEMP_DAILY_TYPE", "SUR_TEMP_DAILY_MAX", "SUR_TEMP_DAILY_MIN", "SUR_TEMP_DAILY_AVG",
                "RH_DAILY_MAX", "RH_DAILY_MIN", "RH_DAILY_AVG", "SOIL_MOISTURE_5_DAILY", "SOIL_MOISTURE_10_DAILY",
                "SOIL_MOISTURE_20_DAILY", "SOIL_MOISTURE_50_DAILY", "SOIL_MOISTURE_100_DAILY", "SOIL_TEMP_5_DAILY", 
                "SOIL_TEMP_10_DAILY", "SOIL_TEMP_20_DAILY", "SOIL_TEMP_50_DAILY", "SOIL_TEMP_100_DAILY" )
  
  # develop date
  allData$LST_DATE<-as.character(allData$LST_DATE)
  allData<-transform(allData, Year = substr(LST_DATE, 1, 4), Month = substr(LST_DATE, 5, 6), Day = substr(LST_DATE, 7, 8))
  allData$date<-as.Date(paste0(allData$Month,"-",allData$Day,"-",allData$Year), "%m-%d-%Y")
  allData$doy<-as.numeric(format(allData$date,"%j"))
  
  # clean up missing
  allData[allData<(-98)]<-NA
  

  # ----- functions
  wtr_yr <- function(dates, start_month=10) {
    # Convert dates into POSIXlt
    dates.posix = as.POSIXlt(dates)
    # Year offset
    offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
    # Water year
    adj.year = dates.posix$year + 1900 + offset
    # Return the water year
    adj.year
  }
  
  allData$wtr_yr<-wtr_yr(allData$date)
  
  # WaterYear Lookup table
  waterDates<-as.data.frame(seq(as.Date("1999/10/1"), as.Date("2000/9/30"), "days"))
  colnames(waterDates)[1]<-"date"
  waterDates$month<-as.numeric(format(waterDates$date, "%m"))
  waterDates$day<-as.numeric(format(waterDates$date, "%d"))
  waterDates$year<-as.numeric(format(waterDates$date, "%Y"))
  waterDates$doy<-as.numeric(format(waterDates$date, "%j"))
  waterDates$waterYear<-wtr_yr(waterDates$date)
  waterDates <- waterDates %>%
    group_by(waterYear) %>% 
    mutate(wtr_day = (as.integer(difftime(date,ymd(paste0(waterYear - 1 ,'-09-30')), units = "days"))))
  
  # merge to get water year DOY
  waterDates<-waterDates[,c("date","doy","wtr_day")]
  allData<-merge(allData, waterDates, by="doy", all.y=TRUE)
  allData<-allData[!duplicated(allData$date.x),]
  

  # develop quantiles
  # calculate stats by day
  sm5climo<- ddply(allData,.(wtr_day),summarise,
                   q33 = quantile(SOIL_MOISTURE_5_DAILY,0.33,na.rm='TRUE'),
                   q50 = quantile(SOIL_MOISTURE_5_DAILY,0.50,na.rm='TRUE'),
                   q66 = quantile(SOIL_MOISTURE_5_DAILY,0.66,na.rm='TRUE'),
                   min = min(SOIL_MOISTURE_5_DAILY,na.rm='TRUE'),
                   max = max(SOIL_MOISTURE_5_DAILY,na.rm='TRUE'),
                   avg = mean(SOIL_MOISTURE_5_DAILY,na.rm='TRUE'))
  
  doyDates<-as.POSIXct(strptime(paste(2014, sm5climo$wtr_day), format="%Y %j"))
  currYear$date<-as.POSIXct(strptime(paste(2014, currYear$doy), format="%Y %j"))
  
  
  # current year
  plotYr<-2020
  currYear<-filter(allData, wtr_yr==plotYr) # CHOOSE YEAR to Plot
  
  
  
  
  # make a plot
  barWidth<-1
  p<-ggplot(sm5climo,aes(wtr_day,q50))+
    theme_bw()+
    #theme(plot.background = element_blank(),
    #      panel.grid.minor = element_blank(),
    #      panel.grid.major = element_blank(),
    #      panel.border = element_blank(),
    #      panel.background = element_blank()) +
    geom_line(colour='grey',size=0.5)+
    geom_linerange(sm5climo, mapping=aes(x=wtr_day, ymin=min, ymax=q33), colour = "tan",alpha=0.4, size=barWidth, show.legend = NA)+
    geom_linerange(sm5climo, mapping=aes(x=wtr_day, ymin=q33, ymax=q66), colour = "gray0",alpha=0.2, size=barWidth)+
    geom_linerange(sm5climo, mapping=aes(x=wtr_day, ymin=q66, ymax=max), colour = "palegreen4",alpha=0.4, size=barWidth)
  p + geom_line(data=currYear,aes(date.x,SOIL_MOISTURE_5_DAILY, colour=as.character(plotYr)), size=1) +
    scale_colour_manual(values=c("red"),name='Year')+
    theme(legend.position=c(0.92,0.90),
          legend.title=element_blank(),
          legend.background = element_rect(fill=alpha('white', 0)))+
    scale_x_date(labels = date_format("%m/%d"), limits = as.POSIXct(c("2014-06-01","2014-09-01")))+
    labs(x='day of year', y='Soil Moisture (%)',title='Daily Average 5-cm Soil Moisture (2011-2020) - Tucson CRN')
    #ylim(-10,75)
  
  
  
  
  
  