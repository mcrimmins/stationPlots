# master script to run station plots with station list
# MAC 01/21/21

# load station list from getStations.R
load("~/RProjects/StationPlots/stationList.RData")

# specify update type
updateType<-"historic"
#updateType<-"realtime"

# specify season
# water year
seasTypes<-c("waterYear","calYear","monsoon","coolSeas")

for(j in 2:length(seasTypes)) {
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
    for(i in 3:3){   # nrow(stationThin)
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
}