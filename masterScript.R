# master script to run station plots with station list
# MAC 01/21/21

library(RCurl)
library(jsonlite)
library(tidyverse)
library(rmarkdown)
library(knitr)
library(leaflet)
library(DT)
library(htmltools)

# load station list from getStations.R
load("/home/crimmins/RProjects/StationPlots/stationList.RData")

# DEAL WITH PANDOC ERROR
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

##### 
# Get ENSO data directly
enso<-read.table("https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt", header = TRUE)
enso$Date<-as.Date(paste0(enso$YR,"-",enso$MON,"-01"))

# specify update type
updateType<-"historic"
#updateType<-"realtime"

# Start the clock!
ptm <- proc.time()

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
      print(paste0(i," of ",nrow(stationThin)))
      source('/home/crimmins/RProjects/StationPlots/seasonalPlot.R')
      #rm(list = ls())
    }
  
# Stop the clock
proc.time() - ptm

# the seasType loop could be put inside seasonalPlot.R to download data only once
seasTypes<-c("waterYear","calYear","monsoon","coolSeas")
linkList<-list()
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

  # build and update the current values page
  sidString<-paste(stationThin$V1, collapse=",")

  currMo<-as.numeric(format(Sys.Date(),"%m"))
    if((seas1mo>seas2mo)& ((currMo >= 1) & (currMo <= 9))){
      year1<-as.numeric(format(Sys.Date(),"%Y"))-1
      year2<-as.numeric(format(Sys.Date(),"%Y"))
    } else {
      year1<-as.numeric(format(Sys.Date(),"%Y"))
      year2<-as.numeric(format(Sys.Date(),"%Y"))
    }
  # create download date
  Date1<-paste0(year1,"-",seas1mo,"-",seas1dy)
  Date2<-format(Sys.Date(),"%Y-%m-%d")
  plotYear<-year2
  
  # if Date2 is earlier than Date1, go to previous year
  if(as.Date(Date1)>as.Date(Date2)){
    Date1<-paste0((year1-1),"-",seas1mo,"-",seas1dy)
    Date2<-paste0((year2-1),"-",seas2mo,"-",seas2dy)
    plotYear<-(year2-1)
  }else{}

  jsonQuery=paste0('{"sids":"',sidString,'","sdate":"',Date1,'","edate":"',Date2,'","elems":[{"name":"pcpn","interval":"dly","duration":1,"smry":{"add":"mcnt","reduce":"sum"},"smry_only":"1"},{"name":"pcpn","interval":"dly","duration":1,"smry":{"add":"mcnt","reduce":"sum"},"smry_only":"1","normal":"departure"},{"name":"pcpn","interval":"dly","duration":1,"smry":{"add":"date","reduce":"max"},"smry_only":"1"},{"name":"avgt","interval":"dly","duration":1,"smry":{"add":"mcnt","reduce":"mean"},"smry_only":"1"},{"name":"avgt","interval":"dly","duration":1,"smry":{"add":"mcnt","reduce":"mean"},"smry_only":"1","normal":"departure"}]}')

  out<-postForm("http://data.rcc-acis.org/MultiStnData",
                .opts = list(postfields = jsonQuery,
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)

  summary<-cbind.data.frame(out$data$meta$name,data.frame(matrix(unlist(out$data$smry), nrow=nrow(out$data), byrow=T)))
  # thin out table
  colnames(summary)<-c("Station","Total Precip (in)","Precip Anomaly (in)",
                       "Max Daily Precip (in)","Avg Temp (F)","Temp Anomaly (F)",
                       "Missing Precip (days)","Miss PAnom","Max Precip Date",
                       "Missing Temp (days)","Miss TAnom")

  summary<-summary[,c("Station","Total Precip (in)","Precip Anomaly (in)",
                      "Avg Temp (F)","Temp Anomaly (F)","Max Daily Precip (in)","Max Precip Date",
                      "Missing Precip (days)","Missing Temp (days)")]
  pageTitle<-paste0(titleType," Station Climate Summaries")
  
  #table links - RED RIVER PASS link breaks   
  dirList<-list.dirs(path = paste0("/home/crimmins/RProjects/StationPlots/plots/",type), full.names = FALSE, recursive = TRUE)
    dirList<-dirList[2:length(dirList)]
  links<-paste0(dirList,"/",dirList,"_",type,"_",plotYear,".png")
  links<-paste0('<a href="',links,'"><img alt="Thumb" src="',links,'"width=150" height="70"></a>')
  # history page links
  histLinks<-paste0(dirList,"/stationHistory.html")
  histLinks<-paste0('<a href="',histLinks,'">',dirList,'</a>')
  linksDF<-cbind.data.frame(dirList,links,histLinks)
  summary<-merge(summary, linksDF, by.x="Station", by.y="dirList")
  colnames(summary)[(ncol(summary)-1):ncol(summary)]<- c("Current Plot","Historical Plots")
  # leaflet table
  leafletTable<-merge(stationThin, summary, by.x="names",by.y = "Station")
    # color pal for map
      leafletTable$`Total Precip (in)`<-as.numeric(as.character(leafletTable$`Total Precip (in)`))
      pal <- colorNumeric(
      palette = colorRampPalette(c('#edf8b1','#1d91c0','#081d58'))(length(leafletTable$`Total Precip (in)`)), 
      domain = leafletTable$`Total Precip (in)`)
      # labels
      stnLabs <- lapply(seq(nrow(leafletTable)), function(i) {
        paste0( '<p> <b>', leafletTable[i, "Historical Plots"], '</b></p>', 
                '<p> Current Plot</p>',
                '<p>',leafletTable[i, "Current Plot"], '</p>')
                })
      
  render('/home/crimmins/RProjects/StationPlots/currentMarkdown.Rmd', output_file='current.html',
         output_dir=paste0("/home/crimmins/RProjects/StationPlots/plots/",type), clean=TRUE)
  
  # links for station summary pages
  links<-paste0(dirList,"/",dirList,"_",type,"_",plotYear,".png")
  links<-paste0('<a href="../',type,'/',links,'"><img alt="Thumb" src="plots/',type,'/',links,'"></a>')
  # history page links
  histLinks<-paste0("../",type,"/",dirList,"/stationHistory.html")
  #histLinks<-paste0('<a href="',histLinks,'">',dirList,'</a>')
  histLinks<-paste0('<a href="',histLinks,'">',type,'- Station History </a>')
  linksDF<-cbind.data.frame(dirList,links,histLinks)
  
  linkList[[j]]<-linksDF

}

# create station pages with current plots
for(i in 1:nrow(stationThin)){   # nrow(stationThin)
  # get station info 
  stationID<-stationThin$V1[i]
  stationLatLon<-stationThin[i,c("lat","lon")]
  stationName<-stationThin$names[i]
  markdownTitle<-paste0("Station Climate Summaries: ", stationName)
  
  # pull links for current station
    k<-which(linkList[[1]]$dirList==stationName)
  
    plotWY<-as.character(linkList[[1]]$links[k])
    plotWYhist<-as.character(linkList[[1]]$histLinks[k])
    plotCal<-as.character(linkList[[2]]$links[k])
    plotCalhist<-as.character(linkList[[2]]$histLinks[k])
    plotMon<-as.character(linkList[[3]]$links[k])
    plotMonhist<-as.character(linkList[[3]]$histLinks[k])
    plotCool<-as.character(linkList[[4]]$links[k])
    plotCoolhist<-as.character(linkList[[4]]$histLinks[k])
    
    
    render('/home/crimmins/RProjects/StationPlots/stationPage.Rmd', output_file=paste0(stationName,'.html'),
           output_dir=paste0("/home/crimmins/RProjects/StationPlots/plots/stn/"), clean=TRUE)
    
}


#### build station access page -- ONLY NEEDED WHEN ADDING/REMOVING STATIONS
# get station list from waterYear directory
dirList<-list.dirs(path = paste0("/home/crimmins/RProjects/StationPlots/plots/waterYear"), full.names = FALSE, recursive = TRUE)
  dirList<-dirList[2:length(dirList)]
stnLinks<-paste0("stn/",dirList,".html")
stnLinks<-paste0('<a href="',stnLinks,'">',dirList,'</a>')
stnlinksDF<-cbind.data.frame(dirList,stnLinks)
stnlinksDF<-merge(stationThin[,c("names","lat","lon","V1")],stnlinksDF, by.x="names",by.y="dirList")
colnames(stnlinksDF)<-c("Station","lat","lon","Station Code","Station Page Link")

render('/home/crimmins/RProjects/StationPlots/allStations.Rmd', output_file='stn.html',
       output_dir=paste0("/home/crimmins/RProjects/StationPlots/plots/"), clean=TRUE)

##### update home page

# sample map
#states <- map_data("state")
# INSET MAP - OPTIONAL
# sampMap<-ggplot() +
#   geom_polygon(data = states, aes(x = long, y = lat, group = group), fill="lightgrey", color="grey")  + # get the state border back on top
#   #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
#   coord_fixed(xlim=c(-115, -102.8), ylim=c(31, 37.5), ratio = 1) +
#   geom_point(data = leafletTable, aes(x = lon, y = lat), size=1, color='red')+
#   ggtitle("Locations of Station Summaries in AZ and NM")+
#   theme_bw()+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         plot.margin=grid::unit(c(0,0,0,0), "mm"))
# 
# png('plots/plot.png', width = 400, height = 225)
# sampMap
# dev.off()

TucsonPlot<-paste0("plots/waterYear/TUCSON INTERNATIONAL AIRPORT/TUCSON INTERNATIONAL AIRPORT_waterYear_",plotYear,".png")

render('/home/crimmins/RProjects/StationPlots/homePage.Rmd', output_file='index.html',
       output_dir=paste0("/home/crimmins/RProjects/StationPlots/plots/"), clean=TRUE)

# interpretation guide
#render('/home/crimmins/RProjects/StationPlots/plotGuide.Rmd', output_file='guide.html',
#       output_dir=paste0("/home/crimmins/RProjects/StationPlots/plots/"), clean=TRUE)


# Stop the clock
ptm <- proc.time() - ptm
ptm <- round((ptm[3])/60,1)
# push notification of completion
source('/home/crimmins/RProjects/StationPlots/pushNotify.R')
print("update completed")
print(ptm)

