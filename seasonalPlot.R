# ACIS JSON station download and creation of seasonal summary plots
# MAC 10/16/19

# load libraries
library(RCurl)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(cowplot)
library(scales)
library(magick)
library(plotly)
library(leaflet)
library(DT)
#library(grid)
#library(gridExtra)

# global options
options(dplyr.summarise.inform = FALSE)

# TO DO: add PET

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
states <- map_data("state")
# -----

out <- NULL
# download data in JSON format and convert - extend 
#stationID<- "USW00023104" #"028820", Flagstaff 023010 phoenix 026481
#stationID <- "028468 2"
jsonQuery=paste0('{"sid":"',stationID,'","meta":"name,ll,elev","sdate":"por","edate":"',endDate,'","elems":"1,2,43,4,10,11"}') # sid = station id, 029439=Winslow, arizona

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

for(j in 1:length(seasTypes)) {
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
  dir.create(paste0("/home/crimmins/RProjects/StationPlots/plots/",type),showWarnings = FALSE)
  dir.create(paste0("/home/crimmins/RProjects/StationPlots/plots/",type,"/",stationName),showWarnings = FALSE)
  plotDir<-paste0("/home/crimmins/RProjects/StationPlots/plots/",type,"/",stationName)


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
      
    
    
    # DEVELOP PLOTS
      # set years to plot - add in 'other' condition if using customized period
      if(type=='waterYear' & format(Sys.Date(),"%m")>=10 || type=='coolSeas' & format(Sys.Date(),"%m")>=10 ){
        last<-as.numeric(format(Sys.Date(),"%Y"))+1
        firstFull<-beginyr+1
      }else{
        last<-as.numeric(format(Sys.Date(),"%Y"))
        firstFull<-beginyr
      }
      # set update type from masterScript.R
      if(updateType=="historic" &&  type=='waterYear' || updateType=="historic" &&  type=='coolSeas' ){
        first<-beginyr+1
      } else if ( updateType=="historic" &&  type=='monsoon' || updateType=="historic" &&  type=='calYear') {
        first<-beginyr
      } else{
        first<-last
      }
      yrListFull<-seq(firstFull,last,1)
      yrList<-seq(first,last,1)
      #yrList<-seq(beginyr,last,1)
      
      for(k in 1:length(yrList)){
          currYear<-yrList[k] # loop here for all plots
          if (seas1mo>seas2mo) {
            currYearData<-dataSeas[which(dataSeas$waterYear==currYear),]
            }else{
            currYearData<-dataSeas[which(dataSeas$year==currYear),]
          }
        
        # deal with leap year-doy issue if needed
          TEMPavgCumPrecip<-avgCumPrecip
          
          if(nrow(TEMPavgCumPrecip) != nrow(currYearData)){
            TEMPavgCumPrecip<-TEMPavgCumPrecip[-nrow(TEMPavgCumPrecip),]
          }
        
        # cumulative precip plot
          currYearData$avgCumPrecip<-TEMPavgCumPrecip$meanCumPrecip
          currYearData$diffAvg<-currYearData$cumPrecip-currYearData$avgCumPrecip
          currYearData$missPrecip<-ifelse(is.na(currYearData$precip)==TRUE, 1, NA)
          currYearData$snowDay<-ifelse(currYearData$snow>0, 0.1, NA)
          
        # grab temp data fram for stacked precip plot  
          temp<-currYearData[,c("date","avgCumPrecip","diffAvg")]
            temp$abvAvg<-temp$diffAvg
            temp$beloAvg<-temp$diffAvg
              temp$abvAvg[temp$abvAvg<0]<-0
              temp$beloAvg[temp$beloAvg>=0]<-0
              temp$avgCumPrecip<-temp$avgCumPrecip+temp$beloAvg
              temp$beloAvg<-abs(temp$beloAvg)
              temp$abvAvg<-abs(temp$abvAvg)
              temp<-temp[,-c(3)]
          temp<-gather(temp, "precipCat","value", -date)
          #temp$precipCat<-factor(temp$precipCat, c("avgCumPrecip","abvAvg", "beloAvg"))
          temp$precipCat<-factor(temp$precipCat, c("beloAvg","abvAvg","avgCumPrecip"))
          # for min/max lines
          #dayPrecip<-dayPrecip[1:nrow(currYearData),]
          #dayPrecip$currDate<-currYearData$date
          # missing data text
          missText<-paste0("(*=missing, data available through ", currYearData$date[max(which(!is.na(currYearData$precip)))],")")
         
          # get enso oni values --- from download_enso function
          # if(enso$Date[1]>=currYearData$date[1]){
          #   oni<-"N/A"
          #   }else if (enso$Date[nrow(enso)]<currYearData$date[nrow(currYearData)]) {
          #   oni<-"N/A"
          # } else {
          #   oni<-round(mean(enso$ONI[min(which(enso$Date>=currYearData$date[1])):max(which(enso$Date<currYearData$date[nrow(currYearData)]))], na.rm = TRUE), 2)
          # }
         
          # get enso oni values --- from download_enso function
          if(enso$Date[1]>=currYearData$date[1]){
            oni<-"N/A"
          }else if (enso$Date[nrow(enso)]<currYearData$date[nrow(currYearData)]) {
            oni<-"N/A"
          } else {
            oni<-round(mean(enso$ANOM[min(which(enso$Date>=currYearData$date[1])):max(which(enso$Date<currYearData$date[nrow(currYearData)]))], na.rm = TRUE), 2)
          }
           
        # stacked bar precip plot
        pPrecip<- ggplot()+
            geom_bar(data=temp, aes(x=date,y=value, fill=as.factor(precipCat)),stat = "identity", width = 1,
                     show.legend = FALSE)+
            scale_fill_manual(values = c("orange4", "green4", "grey88"))+
            geom_step(data=currYearData, aes(x=date,y=avgCumPrecip), color="black", position = position_nudge(x = -0.5))+
            geom_step(data=currYearData, aes(x=date,y=cumPrecip), color="blue", position = position_nudge(x = -0.5))+
            geom_bar(data=currYearData, aes(x=date,y=precip), stat = "identity",fill="dodgerblue2",color="blue")+
          xlab('Date') +
          ylab('Inches')+
          scale_x_date(date_labels = "%b-%d", date_breaks = "1 month", expand = c(0, 0))+
          scale_y_continuous(expand = c(0, 0, 0.1, 0))+
            expand_limits(y=2)+
          annotate(geom="text", x=currYearData$date[nrow(currYearData)]-2, hjust=1, y=Inf, label="Daily total and cumulative seasonal precipitation",
                   color="black", size=3, vjust=2)+
          annotate(geom="text", x=currYearData$date[nrow(currYearData)]-2, hjust=1, y=Inf, label=missText,
                   color="red", size=3, vjust=4)+
          annotate(geom="text", x=currYearData$date[round(nrow(currYearData)/2.8,0)], hjust=1, y=Inf, label=paste0("ONI: ",oni),
                   color="black", size=3, vjust=3)+
          theme_bw()+
          theme(#panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                axis.text.x=element_blank(),
                axis.title.x=element_blank(),
                axis.text.y = element_text(size = 9),
                panel.border = element_rect(colour = "black", fill=NA, size=1))
        # add missing value dots
            if(sum(currYearData$missPrecip, na.rm = TRUE)!=0){
              pPrecip<-pPrecip+ geom_point(data=currYearData, aes(date,missPrecip),shape=8, color="red", 
                                           size=1,show.legend = FALSE)
            }
        # add snow dots
            if(sum(currYearData$snow, na.rm = TRUE)!=0){
              pPrecip<-pPrecip+ geom_point(data=currYearData, aes(date,snowDay),shape=8, color="skyblue", 
                                           size=1,show.legend = FALSE)
            }
        
        # add record precip
        # Daily temperature plot
        dayPrecipTEMP<-dayPrecip[1:nrow(currYearData),]
        dayPrecipTEMP$currDate<-currYearData$date
        # record temp marking
          dayPrecipTEMP<-merge(dayPrecipTEMP, currYearData[,c("date","precip")], by.x="currDate",by.y="date")
          dayPrecipTEMP$recordPPT<-NA
          dayPrecipTEMP$recordPPT<-ifelse(dayPrecipTEMP$precip>=dayPrecipTEMP$maxPrecipDay, dayPrecipTEMP$precip, NA)
          dayPrecipTEMP$recordPPT<-as.numeric(dayPrecipTEMP$recordPPT)
          currYearData$recordPPT<-dayPrecipTEMP$recordPPT
          currYearData$recordPPT[currYearData$recordPPT<0.01]<-NA 
          pPrecip<-pPrecip+ geom_point(data=currYearData, aes(date,recordPPT),shape=18, color="orange", 
                                       size=2,show.legend = FALSE)+
            annotate(geom="text", x=currYearData$date[nrow(currYearData)], y=-Inf, label=length(which(!is.na(currYearData$recordPPT))),
                     color="orange", size=3, vjust=-1.5, hjust=2)
        ####  
        
          # add max/min lines?
          #geom_line(data=dayPrecip, aes(currDate, maxPrecip), linetype=2)+
          #geom_line(data=dayPrecip, aes(currDate, minPrecip), linetype=2)
         
        # similar years plot
        #corrYears<-cor(precipMatrix[2:nrow(precipMatrix),], method = "pearson", use = "pairwise.complete.obs")
        #corrYears<-cov(precipMatrix[2:nrow(precipMatrix),], method = "pearson", use = "pairwise.complete.obs")
          
          corrYears<-as.matrix(stats::dist(t(precipMatrix[2:nrow(precipMatrix),]), method = "euclidean"))
          topYears<-(cbind(corrYears[which(precipMatrix[1,]==currYear),],precipMatrix[1,]))
          topYears<-topYears[order(topYears[,1], decreasing = FALSE),]
          topYears<-topYears[1:3,2]
        # plot top 3 years
        pTop3<-  ggplot(subset(dataSeas, yearX %in% topYears)) + 
            geom_step(aes(dummyDate, cumPrecip, color=as.factor(yearX)))+
            scale_color_brewer(palette = "Set1")+
            scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0))+
            scale_y_continuous(expand = c(0, 0))+
            ylab("inches")+
            xlab("date")+
            theme_bw(base_size = 9)+
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.background = element_rect(fill = "transparent"),
                  legend.position = c(0.15, 0.72),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 7, face = "bold"),
                  legend.spacing.y = unit(0, 'cm'),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
                  axis.text.y = element_text(face="bold"))+
            geom_line(data=currYearData, aes(dummyDate, avgCumPrecip), linetype = "dashed")
        
        # Precip events intensity counts - bar plot
          temp<-seasSummary[which(seasSummary[,1]==currYear),16:18]
          temp<-gather(temp)
            temp$key<-factor(temp$key, levels=c("lightRain","modRain","hvyRain"),
                             labels = c("Light", "Moderate","Heavy"))
            temp$label<-c(paste0("<",quantPrecip[1]),paste0(quantPrecip[1],"-",quantPrecip[2]),paste0(">",quantPrecip[2]))
        pIntens<-  ggplot(temp, aes(key,value, fill=key))+
            geom_bar(stat = "identity", color="black")+
            scale_fill_manual(values = c("deepskyblue", "deepskyblue2", "deepskyblue3"))+
            scale_y_continuous(breaks=pretty_breaks())+
            theme_bw()+
            ggtitle("Precip Events (days)")+
            theme(
                  legend.position = "none",
                  axis.text.y = element_text(face="bold"),
                  axis.text.x = element_text(face="bold"),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  text = element_text(size=9),
                  plot.title = element_text(hjust = 0.5, size=8)
                  )+
            geom_text(data=temp,aes(x=key,y=value,label=label),vjust=1.5, size=3)+
            labs(title="Precip Events (days)", size=1)
            
        # Precip Timing Gannt chart 
          temp<-seasSummary[which(seasSummary[,1]==currYear),11:13]
            temp<-currYearData$date[as.integer(temp)-(currYearData$doyX[1]-1)]
          temp2<-seasMeans[,11:13]
            temp2<-currYearData$date[as.integer(temp2)-(currYearData$doyX[1]-1)]
          temp<-as.data.frame(rbind(temp,temp2)) 
          colnames(temp)<-c("25%","50%","75%")
          temp$cat<-c(currYear,"Avg")
          temp<-gather(temp, key, value, 1:3)
          temp$value<-as.Date(temp$value, origin = "1970-01-01")
          # first/last dates
          temp3<-seasSummary[which(seasSummary[,1]==currYear),9:10]
            temp3<-currYearData$date[as.integer(temp3)-(currYearData$doyX[1]-1)]
          temp4<-seasMeans[,9:10]
            temp4<-currYearData$date[as.integer(temp4)-(currYearData$doyX[1]-1)]
          temp3<-as.data.frame(rbind(temp3,temp4)) 
          colnames(temp3)<-c("First","Last")
          temp3$cat<-c(currYear,"Avg")
          temp3<-gather(temp3, key, value, 1:2)
          temp3$value<-as.Date(temp3$value, origin = "1970-01-01")
            
          # plot timing gannt chart
        pTiming<-  ggplot()+
            geom_line(data=temp, aes(x=as.factor(cat),y=value, color=as.factor(key), group=as.factor(cat)), stat = "identity",
                      size=10)+
             coord_flip()+
            scale_y_date(limits = c(currYearData$date[1],currYearData$date[nrow(currYearData)]),
                         date_breaks = "1 month", date_labels = "%b",expand = c(0, 0))+
            scale_color_manual(values = c("skyblue", "skyblue3", "skyblue4"))+
            geom_text(data=temp,aes(x=as.factor(cat),y=value,label=key),vjust=0.5, size=3)+
            geom_point(data=temp3, aes(x=as.factor(cat),y=value), shape=25, fill="red")+
            geom_text(data=temp3, aes(x=as.factor(cat), y=value, label=key), vjust=1.5, size=3)+
            annotate(geom="text", x=Inf, y=currYearData$date[1], label="Timing of precip events/totals",
                   color="black", size=3, vjust=1.5, hjust=-0.1)+
            theme_bw()+
            theme(#panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.y = element_text(size = 9),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              legend.position = "none",
              axis.title.y = element_blank())
        
        # Daily temperature plot
          dayTempsTEMP<-dayTemps[1:nrow(currYearData),]
          dayTempsTEMP$currDate<-currYearData$date
          # record temp marking
          dayTempsTEMP<-merge(dayTempsTEMP, currYearData[,c("date","t_max","t_min")], by.x="currDate",by.y="date")
            dayTempsTEMP$recordTmax<-NA
            dayTempsTEMP$recordTmax<-ifelse(dayTempsTEMP$t_max>=dayTempsTEMP$maxTmax, dayTempsTEMP$maxTmax, NA)
            dayTempsTEMP$recordTmin<-NA
            dayTempsTEMP$recordTmin<-ifelse(dayTempsTEMP$t_min<=dayTempsTEMP$minTmin, dayTempsTEMP$minTmin, NA)
                dayTempsTEMP$recordTmin<-as.numeric(dayTempsTEMP$recordTmin)
                dayTempsTEMP$recordTmax<-as.numeric(dayTempsTEMP$recordTmax)
            
        pTemp<-  ggplot()+
            geom_linerange(data=currYearData, aes(x=date, ymin=t_min, ymax=t_max),color="goldenrod2", size=1)+
            geom_line(data=dayTempsTEMP, aes(x=currDate,y=avgTmax), color="red")+
            geom_step(data=dayTempsTEMP, aes(x=currDate,y=maxTmax), color="red", size=0.1)+
            geom_line(data=dayTempsTEMP, aes(x=currDate,y=avgTmin), color="blue")+
            geom_step(data=dayTempsTEMP, aes(x=currDate,y=minTmin), color="blue", size=0.1)+
            # records
            geom_point(data=dayTempsTEMP, aes(x=currDate,y=recordTmin), color="blue", shape=18, size=1.5)+
            geom_point(data=dayTempsTEMP, aes(x=currDate,y=recordTmax), color="red", shape=18, size=1.5)+
          
            geom_hline(yintercept=32, color='dodgerblue4', size=0.5, linetype=2)+
            scale_x_date(limits = c(currYearData$date[1],currYearData$date[nrow(currYearData)]),
                         date_breaks = "1 month", date_labels = "%b", expand = c(0, 0))+
            ylab("deg F")+
            theme_bw()+  
            theme(legend.position = "none",
                  panel.grid.minor = element_blank(),
                  axis.title.x = element_blank(),
                  #axis.text.x=element_blank(),
                  axis.text.y = element_text(size = 9),
                  panel.border = element_rect(colour = "black", fill=NA, size=1))+
            annotate(geom="text", x=currYearData$date[1], y=Inf, label="Daily Min/Max Temps",
                   color="black", size=3, vjust=1.5, hjust=-0.1)+
            annotate(geom="text", x=currYearData$date[nrow(currYearData)], y=Inf, label=length(which(!is.na(dayTempsTEMP$recordTmax))),
                   color="red", size=3, vjust=1.5, hjust=2)+
            annotate(geom="text", x=currYearData$date[nrow(currYearData)], y=-Inf, label=length(which(!is.na(dayTempsTEMP$recordTmin))),
                   color="blue", size=3, vjust=-1.5, hjust=2)+
            # annotate(geom="text", x=dayTemps$currDate[1]+round(nrow(dayTemps)*.05), y=dayTemps$avgTmax[1]+4, label="T-max",
            #          color="red")+
            # annotate(geom="text", x=dayTemps$currDate[1]+round(nrow(dayTemps)*.05), y=dayTemps$avgTmin[1]-4, label="T-min",
            #          color="blue")+
            annotate(geom="text", x=currYearData$date[1], y=25, 
                     label=paste0("Freeze Days: ",as.integer(seasSummary[which(seasSummary[,1]==currYear),7]),
                                  " (Avg:",as.integer(seasMeans[1,7]),")" ),
                     color="dodgerblue4", size=3, hjust=-0.1)+
            ylim(min(dayTempsTEMP$minTmin),max(dayTempsTEMP$maxTmax))
          # add records as dots
        
        # bar gauges for precip, rain days, intensity, temps...
        precipBar<-  ggplot(subset(seasSummary, yearX %in% currYear))+
                      geom_bar(aes(1,totalPrecip), stat = "identity", fill="dodgerblue", color="black")+
                      geom_hline(yintercept = seasMeans[2], color="red")+
                      #scale_y_continuous(limits=c(0,seasMeans[2]*2), expand = c(0, 0))+
                      coord_cartesian(ylim=c(0,seasMeans[2]*2),expand = c(0, 0))+  # added on 10/5/2021 to deal with very wet years
                      theme_bw()+
                      theme(text = element_text(size=9),
                            legend.position = "none",
                            axis.ticks.x = element_blank(),
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x=element_blank(),
                            axis.text.y = element_text(face="bold"),
                            panel.border = element_rect(colour = "black", fill=NA, size=1))+
                      ylab("inches")+
                      xlab("Precip")+
                      geom_text(aes(1,(seasMeans[2]*2)-((seasMeans[2]*2)*0.1), label=paste0(round(totalPrecip,2)," in")),
                                size=3)
          # rainDays
        raindayBar<-  ggplot(subset(seasSummary, yearX %in% currYear))+
                      geom_bar(aes(1,totalRainDays), stat = "identity", fill="green", color="black")+
                      geom_hline(yintercept = seasMeans[3], color="red")+
                      scale_y_continuous(limits=c(0,seasMeans[3]*2), expand = c(0, 0))+          
                      theme_bw()+
                      theme(text = element_text(size=9),
                            legend.position = "none",
                            axis.ticks.x = element_blank(),
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x=element_blank(),
                            axis.text.y = element_text(face="bold"),
                            panel.border = element_rect(colour = "black", fill=NA, size=1))+
                      ylab("days")+
                      xlab("Rain days")+
                      geom_text(aes(1,(seasMeans[3]*2)-((seasMeans[3]*2)*0.1), label=paste0(totalRainDays," days")),
                                size=3)
        # rainDays
        intenseBar<-  ggplot(subset(seasSummary, yearX %in% currYear))+
                        geom_bar(aes(1,totalPrecip/totalRainDays), stat = "identity", fill="yellow", color="black")+
                        geom_hline(yintercept =  seasMeans[2]/seasMeans[3], color="red")+
                        scale_y_continuous(limits=c(0,((seasMeans[2]/seasMeans[3])*2)), expand = c(0, 0))+
                        theme_bw()+
                        theme(text = element_text(size=9),
                              legend.position = "none",
                              axis.ticks.x = element_blank(),
                              panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              axis.title.y = element_blank(),
                              axis.text.x=element_blank(),
                              axis.text.y = element_text(face="bold"),
                              panel.border = element_rect(colour = "black", fill=NA, size=1))+
                        ylab("in/day")+
                        xlab("Intensity")+
                        geom_text(aes(1,((seasMeans[2]/seasMeans[3])*2)-(((seasMeans[2]/seasMeans[3])*2)*0.1),
                                      label=paste0(round(totalPrecip/totalRainDays,2)," \nin/day")),
                                  size=3, vjust=1)
        # tmin
        tminBar<-  ggplot(subset(seasSummary, yearX %in% currYear))+
                      geom_bar(aes(1,meanTmin), stat = "identity", fill="blue", color="black")+
                      geom_hline(yintercept = seasMeans[4], color="orange")+
                      theme_bw()+
                      theme(text = element_text(size=9),
                            legend.position = "none",
                            axis.ticks.x = element_blank(),
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x=element_blank(),
                            axis.text.y = element_text(face="bold"),
                            panel.border = element_rect(colour = "black", fill=NA, size=1))+
                      ylab("deg F")+
                      xlab("T-min")+
                      scale_y_continuous(limits=c(seasMeans[4]-5,seasMeans[4]+5),oob = rescale_none, breaks = pretty_breaks())+
                      geom_text(aes(1,(seasMeans[4]+5), label=paste0(round(meanTmin, 1)," F")), size=3, vjust=1)
        # tmax
        tmaxBar<-  ggplot(subset(seasSummary, yearX %in% currYear))+
                    geom_bar(aes(1,meanTmax), stat = "identity", fill="red", color="black")+
                    geom_hline(yintercept = seasMeans[5], color="orange")+
                    theme_bw()+
                    theme(text = element_text(size=9),
                          legend.position = "none",
                          axis.ticks.x = element_blank(),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x=element_blank(),
                          axis.text.y = element_text(face="bold"),
                          panel.border = element_rect(colour = "black", fill=NA, size=1))+
                    ylab("deg F")+
                    xlab("T-max")+
                    scale_y_continuous(limits=c(seasMeans[5]-5,seasMeans[5]+5),oob = rescale_none, breaks = pretty_breaks())+
                    geom_text(aes(1,(seasMeans[5]+5), label=paste0(round(meanTmax, 1)," F")), size=3, vjust=1)              
        # tmean
        tmeanBar<-  ggplot(subset(seasSummary, yearX %in% currYear))+
                      geom_bar(aes(1,meanTmean), stat = "identity", fill="dodgerblue", color="black")+
                      geom_hline(yintercept = seasMeans[6], color="orange")+
                      theme_bw()+
                      theme(text = element_text(size=9),
                            legend.position = "none",
                            axis.ticks.x = element_blank(),
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x=element_blank(),
                            axis.text.y = element_text(face="bold"),
                            panel.border = element_rect(colour = "black", fill=NA, size=1))+
                      ylab("deg F")+
                      xlab("T-mean")+
                      scale_y_continuous(limits=c(seasMeans[6]-5,seasMeans[6]+5),oob = rescale_none, breaks = pretty_breaks())+
                      geom_text(aes(1,(seasMeans[6]+5), label=paste0(round(meanTmean, 1), " F")), size=3, vjust=1)  
        
        # Station Info text block
          # get temp and precip rankings - revisit ties
          # temp<-subset(seasSummary, tmeanNA<=30) # filter out incomplete years?
            temp<-seasSummary
            # clip off last year
            temp<-temp[1:(nrow(temp)-1),]
            temp$pRank<-rank(temp$totalPrecip)
            temp$tRank<-rank(temp$meanTmean)
          pRank<-((max(temp$yearX)-min(temp$yearX))+1)-(temp$pRank[which(temp$yearX==currYear)]-1)
          tRank<-((max(temp$yearX)-min(temp$yearX))+1)-(temp$tRank[which(temp$yearX==currYear)]-1)
          
          
        # text = paste("\n   The following is text that'll appear in a plot window.\n",
        #              "       As you can see, it's in the plot window\n",
        #              "       One might imagine useful informaiton here")
        stationText<-ggplot() + 
                      geom_blank()+
                      # annotate("text", x = 4, y = 25, 
                      #          label = 'atop(bold("This should be bold"),"this should not", "Another thing")',
                      #          colour = "red", parse = TRUE) +
                      annotate("text", x=0, y=-0, label=paste("  "),
                                hjust=0, size=2)+
                      annotate("text", x=0, y=-0.3, label=paste(strwrap(out$meta$name, width=25),collapse="\n"),
                               fontface="bold",hjust=0, size=3.5)+
                      annotate("text", x=0, y=-1, label=paste0("Elevation (ft): ", out$meta$elev),hjust=0, size=3.5)+
                      annotate("text", x=0, y=-1.5, label=paste0("Period of record: ", min(temp$yearX),"-",max(temp$yearX)), hjust=0, size=3.5)+
                      annotate("text", x=0, y=-2, label=paste0("Years in record: ", ((max(temp$yearX))-min(temp$yearX))+1), hjust=0, size=3.5)+
                      annotate("text", x=0, y=-2.5, label=paste0("Precip rank: ", pRank, " (1-wettest)"), hjust=0, size=3.5)+
                      annotate("text", x=0, y=-3, label=paste0("Temp rank: ", tRank, " (1-warmest)"), hjust=0, size=3.5)+
                      annotate("text", x=0, y=-3.5, label=paste0("Missing in ",currYear,": ", seasSummary$precipNA[which(seasSummary$yearX==currYear)] ), hjust=0, size=3.5)+
                      annotate("text", x=0, y=-4, label=paste0("Total snow (in): ", seasSummary$totalSnow[which(seasSummary$yearX==currYear)], " (", 
                                                               round(seasSummary$totalSnow[which(seasSummary$yearX==currYear)]/seasMeans[8]*100)," % avg)"), hjust=0, size=3.5)+
                      theme_bw() +
                      theme(panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank(),
                            axis.title.x = element_blank(),
                            axis.text.x=element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.y=element_blank(),
                            axis.ticks.y=element_blank(),
                            axis.ticks.x=element_blank())+
                      xlim(0,1)
          
        # Dry spells text block
        drySpellText<-ggplot() + 
                        geom_blank()+
                        # annotate("text", x = 4, y = 25, 
                        #          label = 'atop(bold("This should be bold"),"this should not", "Another thing")',
                        #          colour = "red", parse = TRUE) +
                        annotate("text", x=0, y=0, label="   ", fontface="bold",hjust=0, color="brown", size=2)+
                        annotate("text", x=0, y=-0.05, label="Dry Spells", fontface="bold",hjust=0, color="brown")+
                        annotate("text", x=0, y=-0.2, label=paste0("Avg length: ", round(seasSummary$avgDrySpell[which(seasSummary$yearX==currYear)]), " days (avg: ", 
                                                                 round(seasMeans[15]),")"), hjust=0, color="brown")+
                        annotate("text", x=0, y=-0.3, label=paste0("Max length: ", round(seasSummary$maxDrySpell[which(seasSummary$yearX==currYear)]), " days (avg: ", 
                                                                 round(seasMeans[14]),")"), hjust=0, color="brown")+
                        annotate("text", x=0, y=-0.4, label="   ", fontface="bold",hjust=0, color="brown", size=2)+
                        theme_bw() +
                        theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank(),
                              axis.title.x = element_blank(),
                              axis.text.x=element_blank(),
                              axis.title.y = element_blank(),
                              axis.text.y=element_blank(),
                              axis.ticks.y=element_blank(),
                              axis.ticks.x=element_blank())+
                        xlim(0,1)
        
        # INSET MAP - OPTIONAL
        # point<-as.data.frame(t((out$meta$ll)))
        # # inset map:
        # zoomLev<-5
        insetmap<-ggplot() +
          geom_polygon(data = states, aes(x = long, y = lat, group = group), fill="lightgrey", color="grey")  + # get the state border back on top
          #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
          coord_fixed(xlim=c(-115, -102.8), ylim=c(31, 37.5), ratio = 1) +
          geom_point(data = stationLatLon, aes(x = lon, y = lat), size=1, color='red')+
          theme_bw(base_size=5)+
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
        
        
        # CREATE MASTER PLOT USING COWPLOT
          # combine main plots
          mainPlot<-plot_grid(pTiming,pPrecip,pTemp, rel_heights=c(0.4, 2, 0.7),
                   ncol = 1, nrow = 3, align = "v")
          # add top 3 inset to main plot
          mainPlot<-ggdraw(mainPlot) + draw_plot(pTop3, x=-0.30, y=0.22, scale=0.25) 
            #draw_plot(insetmap, x=-0, y=0.3, scale=0.1)
              
          #mainPlot<-ggdraw(mainPlot) + draw_plot(pTop3, x=-0.30, y=0.20, width = 1, height = 1)
          # create precip bar block
          precipPlot<-plot_grid(precipBar,raindayBar,intenseBar, align="h", ncol=3 )
          # create temp bar block
          tempPlot<-plot_grid(tminBar,tmeanBar,tmaxBar, align="h", ncol=3 )
          # create right side info block
         # infoPlot <- plot_grid(stationText, precipPlot, pIntens, drySpellText, tempPlot, ncol=1, 
        #                        rel_heights = c(1,1,0.75,0.5,1), rel_widths = c(1,1.5,0.5,1,1.5))
          
          infoPlot <- plot_grid(stationText, insetmap, precipPlot, pIntens, drySpellText, tempPlot, ncol=1, 
                                rel_heights = c(1,0.33,0.66,0.66,0.5,1), rel_widths = c(1,1,1.5,0.5,1,1.5))
          
          # combine into final plot
          mainPlot <- plot_grid(mainPlot, infoPlot, ncol=2, rel_widths =c(1,0.3))
        
          # plot title
          title_theme <- ggdraw() +
            draw_label(paste0("Station Climate Summary: ",format(currYearData$date[1], "%m-%d-%Y")," to ",format(currYearData$date[nrow(currYearData)], "%m-%d-%Y")," (" ,titleType,")"),
                       fontface = "bold",colour=titleCol,x = 0.05, hjust = 0)
          mainPlot<-plot_grid(title_theme, mainPlot, ncol = 1, rel_heights = c(0.03, 1))
         
          # add margin
          mainPlot = mainPlot + theme(plot.margin = unit(c(0.25, 0, 0.5, 0), "in")) 
          
          
          
          # add caption
          captionString <- c( "Data from http://www.rcc-acis.org/",
                              paste0("Plot created: ", format(Sys.Date(), "%m-%d-%Y")),
                              "The University of Arizona",
                              "https://cals.arizona.edu/climate/")
          mainPlot<-ggdraw(mainPlot) + draw_text(captionString, x =0.83, 
                                                 y = c(0.0500,0.0375,0.0250,0.0125), hjust = 0,vjust=-0.25, size=8)
        
            
          # write out file
          plotFileName<-paste0(plotDir,"/",stationName,"_",type,"_",currYear,".png")
          png(plotFileName, width = 11, height = 8.5, units = "in", res = 300L)
          #grid.newpage()
          print(mainPlot, newpage = FALSE)
          dev.off()
          
          # add logos
          # Call back the plot
          plot <- image_read(plotFileName)
          # And bring in a logo
          #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png") 
          logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
          logo <- image_resize(logo_raw, geometry_size_percent(width=95,height = 95))
          # Stack them on top of each other
          #final_plot <- image_append((c(plot, logo)), stack = TRUE)
          #final_plot <- image_mosaic((c(plot, logo)))
          final_plot <- image_composite(plot, logo, offset = "+160+2375")
          # And overwrite the plot without a logo
          image_write(final_plot, plotFileName)  
      
          print(currYear)
          
          # DEVELOP SEASONAL MEANS THAT CHANGE OVER TIME, interactive plotly versions?
          
      }
      
      # # current seas stats
      # tempSummary<-seasSummary[which(seasSummary[,1]==last),]
      # 
      # # seasSummary table for historical station summary page
      # load test data for development
      # load("~/RProjects/StationPlots/rmdHistoryData.RData")
      
     # if(updateType=="historic"){
        # ONLY INCLUDE MOST RECENT COMPLETE YEAR
          tempTable<-seasSummary[,c(1,2,3,6,7,8,14,15,18,19,20)]
          colnames(tempTable)<-c( "Year", "Total Precip (in)", "Total Precip Days", "Avg Temp (F)","Freeze Days", "Total Snow (in)", "Max Dry Spell (days)", 
                                  "Avg Dry Spell (days)", "Heavy Rain Days","Missing Precip (days)","Missing Temp (days)")
          #tempTable<-tempTable[which(tempTable$Year %in% yrList[1:(length(yrList)-1)]),]
          tempTable<-tempTable[which(tempTable$Year %in% yrListFull[1:(length(yrListFull))]),]
          
          # round 
          tempTable$`Total Precip (in)`<-round(tempTable$`Total Precip (in)`,1)
          tempTable$`Avg Temp (F)`<-round(tempTable$`Avg Temp (F)`,1)
          tempTable$`Avg Dry Spell (days)`<-round(tempTable$`Avg Dry Spell (days)`,0)
                
          # create image links in table - MATCH IMAGE LINKS with YEARS IN TABLE
          tempImgLinks<-as.data.frame(list.files(plotDir, pattern = "*.png", full.names = FALSE))
            colnames(tempImgLinks)<-"fileName"
            tempImgLinks$Year<-as.numeric(regmatches(tempImgLinks$fileName, regexpr("(\\d{4})(?=.([a-zA-Z]+))",tempImgLinks$fileName, perl=T)))
            tempImgLinks$imageLinks<-paste0('<a href="',tempImgLinks$fileName,'"><img alt="Thumb" src="',tempImgLinks$fileName,'"width=150" height="70"></a>')
            tempImgLinks<-tempImgLinks[,c("Year","imageLinks")]
            tempTable<-merge(tempTable,tempImgLinks, by="Year")
            #tempImgLinks<-tempImgLinks[1:(length(tempImgLinks)-1)]
            #tempTable$imageLinks<-paste0('<a href="',tempImgLinks,'"><img alt="Thumb" src="',tempImgLinks,'"width=150" height="70"></a>')
            colnames(tempTable)[ncol(tempTable)]<-"Plot"
          # anomaly table
          center_apply <- function(x) {
            apply(x, 2, function(y) round(y - mean(y),1))
          }
          tempAnom<-as.data.frame(center_apply(tempTable[,2:9]))
          tempAnom<-cbind.data.frame(tempTable$Year,tempAnom)
          colnames(tempAnom)[1]<-"Year"
          # column means
          tempMean<-as.data.frame(t(round(colMeans(tempTable[,2:9], na.rm = TRUE),1)))
          
          # station info
          markdownTitle<-paste0(titleType," Station Climate Summaries: ", stationName)
          #datatable(tempTable, class = 'cell-border compact stripe', escape = FALSE)
          
          # plotly cumulative plots
          # subset to tempTable years
          tempSeas<-subset(dataSeas, yearX %in%  unique(tempTable$Year))
            # trim to present data
            tempSeas$cumPrecip<-(ifelse(is.na(tempSeas$precip)==TRUE,NA,1))*tempSeas$cumPrecip
          tempSeas<-tempSeas[,c("yearX","doyX","cumPrecip","date","dummyDate","t_max","t_min")]
          colnames(tempSeas)<-c("Year","Day of Year","Cumulative Precip","Date","dummyDate","T-max","T-min")
          tempSeas$Year<-as.factor(tempSeas$Year)
            tempSeas$Date<-format(tempSeas$Date, "%b-%d")
          
          # get averages
          seasAverage<- tempSeas %>%
                        group_by(`Day of Year`) %>%
                                summarize(`Year` = first(Year),
                                          `Day of Year` = min(`Day of Year`, na.rm = TRUE),
                                          `Cumulative Precip`=mean(`Cumulative Precip`, na.rm=TRUE),
                                          `Date` = min(`Date`, na.rm = TRUE),
                                          `dummyDate` = min(`dummyDate`, na.rm = TRUE),
                                          `T-max` = mean(`T-max`, na.rm=TRUE),
                                          `T-min` = mean(`T-min`, na.rm=TRUE))
          seasAverage$Year<-"Average"
          tempSeas<-rbind.data.frame(tempSeas,seasAverage)
          # fix dummy date to just m/d
                    
            # color ramp
            library(RColorBrewer)
            colourCount = length(unique(tempSeas$Year))
            getPalette = colorRampPalette(brewer.pal(9, "Set1"))
          
              pCum<-ggplot(tempSeas, aes(`Day of Year`,`Cumulative Precip`,color=Year, group=1,text=Date))+
                geom_step()+
                scale_x_continuous(breaks=seq(0,nrow(tempSeas),15),
                                   labels=format(seq.Date(tempSeas$dummyDate[1],tempSeas$dummyDate[1]+nrow(tempSeas),by="15 days"),"%m-%d"))+
                scale_color_manual(name="Year",values = getPalette(colourCount))+
                xlab("Day of Year")+
                ylab("Inches")+
                ggtitle(paste0("Daily Cumulative Precipitation "))+
                theme_bw()+
                theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
       
              
              # pCum <- ggplotly(pCum)
              # temps
              pTemps<-ggplot(tempSeas)+
                geom_line(aes(`Day of Year`,`T-max`,color=Year, group=1,text=Date))+
                geom_line(aes(`Day of Year`,`T-min`,color=Year, group=1,text=Date))+
                scale_x_continuous(breaks=seq(0,nrow(tempSeas),15),
                                   labels=format(seq.Date(tempSeas$dummyDate[1],tempSeas$dummyDate[1]+nrow(tempSeas),by="15 days"),"%m-%d"))+
                scale_color_manual(name="Year",values = getPalette(colourCount))+
                xlab("Day of Year")+
                ylab("Deg F")+
                ggtitle(paste0("Daily Min/Max Temperatures (F) "))+
                theme_bw()+
                theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
          
          # RENDER NAV PAGE
          library(rmarkdown)
          library(knitr)
          
          render('/home/crimmins/RProjects/StationPlots/stationHistory.Rmd', output_file='stationHistory.html',
                 output_dir=paste0(plotDir), clean=TRUE, quiet=TRUE)
          
     # }else{
    #        
    #  }
  
  }
  
  