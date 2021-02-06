# master script to run station plots with station list
# MAC 01/21/21

library(RCurl)
library(jsonlite)
library(tidyverse)
library(rmarkdown)
library(knitr)


# load station list from getStations.R
load("~/RProjects/StationPlots/stationList.RData")

# specify update type
#updateType<-"historic"
updateType<-"realtime"

# specify season
# water year
seasTypes<-c("waterYear","calYear","monsoon","coolSeas")

# Start the clock!
ptm <- proc.time()

# the seasType loop could be put inside seasonalPlot.R to download data only once
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
    # loop through station
    for(i in 1:nrow(stationThin)){   # nrow(stationThin)
      # get station info 
      stationID<-stationThin$V1[i]
      stationLatLon<-stationThin[i,c("lat","lon")]
      stationName<-stationThin$names[i]
      
      # end date logic in JSON request
      endDate<-paste0(as.numeric(format(Sys.Date(),"%Y"))+1,"-12-31")
      
      # year
      #currYear<-2020
      print(as.character(stationName))
      source('seasonalPlot.R')
      #rm(list = ls())
      
    }
  
  # # build and update the current values page
  # sidString<-paste(stationThin$V1, collapse=",")
  # 
  # currMo<-as.numeric(format(Sys.Date(),"%m"))
  #   if((seas1mo>seas2mo)& ((currMo >= 1) & (currMo <= 9))){
  #     year1<-as.numeric(format(Sys.Date(),"%Y"))-1
  #     year2<-as.numeric(format(Sys.Date(),"%Y"))
  #   } else {
  #     year1<-as.numeric(format(Sys.Date(),"%Y"))
  #     year2<-as.numeric(format(Sys.Date(),"%Y"))
  #   }  
  # # create download date
  # Date1<-paste0(year1,"-",seas1mo,"-",seas1dy)
  # Date2<-format(Sys.Date(),"%Y-%m-%d")
  # 
  # jsonQuery=paste0('{"sids":"',sidString,'","sdate":"',Date1,'","edate":"',Date2,'","elems":[{"name":"pcpn","interval":"dly","duration":1,"smry":{"add":"mcnt","reduce":"sum"},"smry_only":"1"},{"name":"pcpn","interval":"dly","duration":1,"smry":{"add":"mcnt","reduce":"sum"},"smry_only":"1","normal":"departure"},{"name":"pcpn","interval":"dly","duration":1,"smry":{"add":"date","reduce":"max"},"smry_only":"1"},{"name":"avgt","interval":"dly","duration":1,"smry":{"add":"mcnt","reduce":"mean"},"smry_only":"1"},{"name":"avgt","interval":"dly","duration":1,"smry":{"add":"mcnt","reduce":"mean"},"smry_only":"1","normal":"departure"}]}')
  #                  
  # out<-postForm("http://data.rcc-acis.org/MultiStnData", 
  #               .opts = list(postfields = jsonQuery, 
  #                            httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  # out<-fromJSON(out)               
  # 
  # summary<-cbind.data.frame(out$data$meta$name,data.frame(matrix(unlist(out$data$smry), nrow=nrow(out$data), byrow=T)))
  # # thin out table
  # colnames(summary)<-c("Station","Total Precip (in)","Precip Anomaly (in)",
  #                      "Max Daily Precip (in)","Avg Temp (F)","Temp Anomaly (F)",
  #                      "Missing Precip (days)","Miss PAnom","Max Precip Date",
  #                      "Missing Temp (days)","Miss TAnom")
  # 
  # summary<-summary[,c("Station","Total Precip (in)","Precip Anomaly (in)",
  #                     "Avg Temp (F)","Temp Anomaly (F)","Max Daily Precip (in)","Max Precip Date",
  #                     "Missing Precip (days)","Missing Temp (days)")]
  # pageTitle<-paste0(titleType," Station Climate Summaries - ",year2)                 
  # 
  # render('/home/crimmins/RProjects/StationPlots/currentMarkdown.Rmd', output_file='current.html',
  #        output_dir=paste0("/home/crimmins/RProjects/StationPlots/plots/",type), clean=TRUE)
  # 
  # 
  # #
  # 
}

# Stop the clock
proc.time() - ptm
