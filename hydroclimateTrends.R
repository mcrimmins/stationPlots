# trying out a hydroclimate extremes tracker
# MAC 04/08/21
# adapted from seasonalPlot.R

# load libraries
library(RCurl)
library(jsonlite)
library(tidyverse)
library(lubridate)
# library(cowplot)
# library(scales)
# library(magick)
# library(plotly)
# library(leaflet)
# library(DT)
#library(grid)
#library(gridExtra)

# global options
options(dplyr.summarise.inform = FALSE)

# load station list from getStations.R
load("/home/crimmins/RProjects/StationPlots/stationList.RData")

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
#perc.rank <- function(x) trunc(rank(x,ties.method = "first"))/length(x)
#perc.rank <- function(x) (x)/sum(x, na.rm = TRUE)

source("/home/crimmins/RProjects/StationPlots/rle2_function.R")

# indices
#library(rsoi)
#library(rpdo)
#enso<-download_enso()
#pdo<-download_pdo()
# ----

# ggplot inset data
#states <- map_data("state")
# -----

# choose station and season
# get station info 
i=36
  stationID<-stationThin$V1[i]
  stationLatLon<-stationThin[i,c("lat","lon")]
  stationName<-stationThin$names[i]


out <- NULL
# download data in JSON format and convert - extend 
#stationID<- "USW00023104" #"028820", Flagstaff 023010 phoenix 026481
#stationID <- "028468 2"
jsonQuery=paste0('{"sid":"',stationID,'","meta":"name,ll,elev","sdate":"por","edate":"por","elems":"1,2,43,4,10,11"}') # sid = station id, 029439=Winslow, arizona

# out<-postForm("http://data.rcc-acis.org/StnData",
#               .opts = list(postfields = jsonQuery,
#                            httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))

# try again for timeouts
attempt <- 1
while( is.null(out) && attempt <= 3 ) {
  try(
    out<-postForm("http://data.rcc-acis.org/StnData", 
                  .opts = list(postfields = jsonQuery, 
                               httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  )
  if(is.null(out)){
    print(paste0("Download attempt #: ",attempt))
    attempt <- attempt + 1
    Sys.sleep(30)
  }
} 



# extract from JSON
out<-fromJSON(out)

##### ADD IN SEAS TYPE LOOP HERE
# specify season
# water year
seasTypes<-c("waterYear","calYear","monsoon","coolSeas")
# choose season
j=1

  type<-seasTypes[j]
  # switches for vars
  if (type=="waterYear") {
    seas1mo<-10; seas1dy<-1
    seas2mo<-9; seas2dy<-30
    titleType<-"Water Year"
    titleCol<-"orangered4"
  } else if ( type=="calYear") {
    seas1mo<-1; seas1dy<-1
    seas2mo<-12; seas2dy<-31
    titleType<-"Calendar Year"
    titleCol<-"navyblue"
  } else if ( type=="monsoon") {
    seas1mo<-6; seas1dy<-15
    seas2mo<-9; seas2dy<-30
    titleType<-"Monsoon Season"
    titleCol<-"red1"
  } else if ( type=="coolSeas"){
    seas1mo<-10; seas1dy<-1
    seas2mo<-6; seas2dy<-14
    titleType<-"Cool Season"
    titleCol<-"royalblue4"
    # add in 'other' customized time period here  
  }
  print(type)  
  
  # create/switch to directories
 # dir.create(paste0("/home/crimmins/RProjects/StationPlots/plots/",type),showWarnings = FALSE)
#  dir.create(paste0("/home/crimmins/RProjects/StationPlots/plots/",type,"/",stationName),showWarnings = FALSE)
#  plotDir<-paste0("/home/crimmins/RProjects/StationPlots/plots/",type,"/",stationName)
  
  
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
  # get water doy 
  seas1doy_wtr<-waterDates[which(waterDates$month==seas1mo & waterDates$day==seas1dy),7] 
  seas2doy_wtr<-waterDates[which(waterDates$month==seas2mo & waterDates$day==seas2dy),7]
  
  
  # wrangle data - get data from list
  data<-data.frame(out$data)
  colnames(data)<-c("date","t_max","t_min","t_mean","precip","snow","snowD")
  data$date<-as.Date(as.character(data$date))
  # replace T trace values
  data$precip<-recode(data$precip, "T" = "0.001")
  # convert columns to numeric
  unfactorize<-c("t_max","t_min","t_mean","precip","snow","snowD")
  data[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(data[,x])))
  
  # add in month, day, years...
  data$month<-as.numeric(format(data$date, "%m"))
  data$day<-as.numeric(format(data$date, "%d"))
  data$year<-as.numeric(format(data$date, "%Y"))
  data$doy<-as.numeric(format(data$date, "%j"))
  # dummy date
  data$dummyDate<-as.Date(paste0("2000-",data$month,"-",data$day), format="%Y-%m-%d")
  
  # add rain days
  #data$rain<-ifelse(data$snow==0,data$precip,0) 
  #data$rain<-ifelse(is.na(data$snow) & !is.na(data$precip),data$precip,data$rain)
  data$precipDay<-ifelse(data$precip>=0.01,1,0) 
  
  # freeze days
  data$frzDay<-ifelse(data$t_min<=32,1,0)
  
  # find optimal period of record - longest period to present - using daily data
  # por<-na.contiguous(data$precip)
  # por<-attr(por,"tsp")
  #   beginyr<-data$year[por[[1]]]
  # # find last year in POR
  #   lastyr<-data$year[por[[2]]]
  #   # trim to por
  # data<-subset(data, year>beginyr)
  # using yearly counts of NAs
  temp<-data %>% 
    group_by(year) %>%
    summarize(missing = sum(is.na(precip)))
  temp$missNA<-ifelse(temp$missing>120, NA, temp$missing)
  missYrs<-temp
  por<-na.contiguous(temp$missNA)
  por<-attr(por,"tsp")
  beginyr<-temp$year[por[[1]]]
  # find last year in POR
  lastyr<-temp$year[por[[2]]]
  # check to make sure first year is full year
  if(length(which(data$year==beginyr))<=364){
    beginyr<-beginyr+1
  }else{
    beginyr<-beginyr
  }
  # trim to por
  data<-subset(data, year>=beginyr)
  
  
  # add water year/days
  data$waterYear<-wtr_yr(data$date)
  data <- data %>%
    group_by(waterYear) %>% 
    mutate(wtr_day = (as.integer(difftime(date,ymd(paste0(waterYear - 1 ,'-09-30')), units = "days"))))
  
  # SEASONAL SUMMARIES  
  # calendar year vs water year switch
  if (seas1mo>seas2mo) {
    # crossing calendar year - water year
    dataSeas<-data[data$wtr_day>=seas1doy_wtr$wtr_day & data$wtr_day<=seas2doy_wtr$wtr_day,]
    # cumulative precip on season/yr using dplyr
    cumPrecip <- dataSeas %>% 
      group_by(waterYear, wtr_day) %>% # still doesn't quite work adjYear kicks off before adjDOY
      summarise(value = sum(precip, na.rm = T)) %>%
      mutate(csum = cumsum(value), cumPerc=csum/sum(value, na.rm=TRUE)*100)
    dataSeas$cumPrecip<-cumPrecip$csum
    dataSeas$cumPerc<-cumPrecip$cumPerc
    dataSeas$precipNA<-dataSeas$precip
    dataSeas$precipNA[dataSeas$precipNA == 0] <- NA
    dataSeas$precipNA[dataSeas$precipNA == 0.001] <- NA
    quantPrecip<-round(quantile(dataSeas$precipNA, probs = c(33, 66)/100, na.rm = T),2)
    # average cumulative sum
    avgCumPrecip <- dataSeas %>%
      group_by(wtr_day) %>%
      summarise(meanCumPrecip = mean(cumPrecip, na.rm = T))
    # similar years matrix
    precipMatrix<-dataSeas[,c("wtr_day","waterYear","cumPrecip")]
    # set missing years to NA
    missYrs$year<-missYrs$year+1 # adjust to water year match
    precipMatrix$cumPrecip[precipMatrix$waterYear %in% (missYrs[which(is.na(missYrs$missNA)==TRUE),1]$year)]<-NA
    precipMatrix<-t(spread(precipMatrix, key = wtr_day,value = cumPrecip))
    # doy/year for plots
    dataSeas$doyX<-dataSeas$wtr_day
    dataSeas$yearX<-dataSeas$waterYear
    # adjust dummy date 
    dataSeas$dummyDate<-ifelse(dataSeas$dummyDate>=as.Date(paste0("2000-",seas1mo,"-",seas1dy)),
                               as.Date(paste0("1999-",dataSeas$month,"-",dataSeas$day), format="%Y-%m-%d"),
                               dataSeas$dummyDate)
    dataSeas$dummyDate<-as.Date(dataSeas$dummyDate, origin="1970-01-01")
    ####  
    #
    ## WATER YEAR
    seasSummary <- dataSeas %>% 
      group_by(waterYear) %>% # 
      summarise(totalPrecip = sum(precip, na.rm = T),
                totalRainDays = sum(precipDay, na.rm = T),
                meanTmin = mean(t_min, na.rm = T),
                meanTmax = mean(t_max, na.rm = T),
                meanTmean = mean(t_mean, na.rm = T),
                totalFrzDays = sum(frzDay, na.rm = T),
                totalSnow = sum(snow, na.rm = T),
                firstRain =wtr_day[min(which(precipDay==1))],
                lastRain =wtr_day[max(which(precipDay==1))],
                precip25 =wtr_day[min(which(cumPerc>=25))],
                precip50 =wtr_day[min(which(cumPerc>=50))],
                precip75 =wtr_day[min(which(cumPerc>=75))],
                maxDrySpell = max(rle2(precipDay)$lengths),
                avgDrySpell = mean(rle2(precipDay)$lengths, na.rm = T),
                lightRain = sum(precipNA<=quantPrecip[1], na.rm = T),
                modRain = sum(precipNA>quantPrecip[1] & precipNA<quantPrecip[2], na.rm = T),
                hvyRain = sum(precipNA>=quantPrecip[2], na.rm = T),
                precipNA = sum(is.na(precip)),
                tmeanNA = sum(is.na(t_mean)),
                yearX = max(yearX, na.rm = T)
      )
    
  }else{
    # within calendar year
    dataSeas<-data[data$dummyDate>=paste0("2000-",seas1mo,"-",seas1dy) & data$dummyDate<=paste0("2000-",seas2mo,"-",seas2dy),]
    # cumulative precip on season/yr using dplyr
    cumPrecip <- dataSeas %>% 
      group_by(year, doy) %>% # still doesn't quite work adjYear kicks off before adjDOY
      summarise(value = sum(precip, na.rm = T)) %>%
      mutate(csum = cumsum(value), cumPerc=csum/sum(value, na.rm=TRUE)*100)
    dataSeas$cumPrecip<-cumPrecip$csum
    dataSeas$cumPerc<-cumPrecip$cumPerc
    dataSeas$precipNA<-dataSeas$precip
    dataSeas$precipNA[dataSeas$precipNA == 0] <- NA
    dataSeas$precipNA[dataSeas$precipNA == 0.001] <- NA
    quantPrecip<-quantile(dataSeas$precipNA,  probs = c(33, 66)/100, na.rm = T)
    # average cumulative sum
    avgCumPrecip <- dataSeas %>%
      group_by(doy) %>%
      summarise(meanCumPrecip = mean(cumPrecip, na.rm = T))
    # similar years matrix
    precipMatrix<-dataSeas[,c("doy","year","cumPrecip")]
    # set missing years to NA
    precipMatrix$cumPrecip[precipMatrix$year %in% (missYrs[which(is.na(missYrs$missNA)==TRUE),1]$year)]<-NA
    precipMatrix<-t(spread(precipMatrix, key = doy,value = cumPrecip))
    # doy/year for plots
    dataSeas$doyX<-dataSeas$doy
    dataSeas$yearX<-dataSeas$year
    ## CALENDAR YEAR
    seasSummary <- dataSeas %>% 
      group_by(year) %>% # still doesn't quite work adjYear kicks off before adjDOY
      summarise(totalPrecip = sum(precip, na.rm = T),
                totalRainDays = sum(precipDay, na.rm = T),
                meanTmin = mean(t_min, na.rm = T),
                meanTmax = mean(t_max, na.rm = T),
                meanTmean = mean(t_mean, na.rm = T),
                totalFrzDays = sum(frzDay, na.rm = T),
                totalSnow = sum(snow, na.rm = T),
                firstRain =doy[min(which(precipDay==1))],
                lastRain =doy[max(which(precipDay==1))],
                precip25 =doy[min(which(cumPerc>=25))],
                precip50 =doy[min(which(cumPerc>=50))],
                precip75 =doy[min(which(cumPerc>=75))],
                maxDrySpell = max(rle2(precipDay)$lengths),
                avgDrySpell = mean(rle2(precipDay)$lengths, na.rm = T),
                lightRain = sum(precipNA<=quantPrecip[1], na.rm = T),
                modRain = sum(precipNA>quantPrecip[1] & precipNA<quantPrecip[2], na.rm = T),
                hvyRain = sum(precipNA>=quantPrecip[2], na.rm = T),
                precipNA = sum(is.na(precip)),
                precipNA = sum(is.na(precip)),
                tmeanNA = sum(is.na(t_mean)),
                yearX = max(yearX, na.rm = T)
      )
  }
  
  # seasonal means accounting for missing vals 
  temp<-subset(seasSummary, tmeanNA<=30)
  seasMeans<-t(as.data.frame(colMeans(temp, na.rm = T)))
  
  # daily quantiles  
  # temperatures
  dayTemps <- dataSeas %>% 
    group_by(dummyDate) %>% 
    summarise(doyX = min(doyX, na.rm = TRUE),
              maxTmax = max(t_max,na.rm='TRUE'),
              avgTmax = mean(t_max,na.rm='TRUE'),
              minTmin = min(t_min,na.rm='TRUE'),
              avgTmin = mean(t_min,na.rm='TRUE'))
  # temperatures
  dayPrecip <- dataSeas %>% 
    group_by(dummyDate) %>% 
    summarise(doyX = min(doyX, na.rm = TRUE),
              maxPrecip = max(cumPrecip,na.rm='TRUE'),
              minPrecip = min(cumPrecip,na.rm='TRUE'),
              maxPrecipDay = max(precip, na.rm = 'TRUE'))
  
  # trend analysis
  library(trend)
  
  trendTS<-seasSummary[-(nrow(seasSummary)),]
  test<-sens.slope(trendTS$maxDrySpell, conf.level = 0.95)$estimates
  
  # all combos
  #yrCombos<-expand.grid(1:nrow(trendTS),1:nrow(trendTS))
  #yrCombos$Sens<-NA
  yrCombos<-as.data.frame(t(combn(1:nrow(trendTS),2)))
  yrCombos$Sens<-NA
  
  for(i in 1:nrow(yrCombos)){
      yrCombos$Sens[i]<-sens.slope(trendTS$maxDrySpell[yrCombos$V1[i]:yrCombos$V2[i]], conf.level = 0.95)$estimates
  }
  
  # heat map trends https://www.r-graph-gallery.com/79-levelplot-with-ggplot2.html
  library(ggplot2)
  library(scales)
    # adjust to years
      adjYr<-trendTS$waterYear[1]-1
      yrCombos$V1<-yrCombos$V1+adjYr
      yrCombos$V2<-yrCombos$V2+adjYr
      yrCombos$periodLength<-yrCombos$V2-yrCombos$V1
      yrCombos<-subset(yrCombos, periodLength>=15)
  
  p<-ggplot(yrCombos, aes(V1, V2, fill= Sens, text=Sens)) + 
    geom_tile() +
    #scale_fill_distiller(palette = "RdPu") 
    scale_fill_gradient2(low = "red",
                         mid = "white",
                         high = "blue",
                         midpoint = 0,
                         limits=c(-0.5, 0.5), oob=squish)
  
  library(plotly)
  ggplotly(p, tooltip="text")
  
  
  
  