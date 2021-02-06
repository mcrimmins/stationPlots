# Get station meta data from RCC ACIS
# MAC 11/18/19

# to do: figure out way to gauge missing days?, station types/threadex stations

# load libraries
library(RCurl)
library(jsonlite)
library(tidyverse)
library(tidygeocoder)
library(leaflet)

#library(tidyr)
#library(stringr)

# get valid date ranges
jsonQuery='{"state":"NM,AZ","meta":"sids,name,valid_daterange,ll","elems":"pcpn"}'
out<-postForm("http://data.rcc-acis.org/StnMeta", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
outList<-fromJSON(out, flatten = FALSE) # 

# get lat lons
temp<-outList[["meta"]]
temp<-separate(data = temp, col = ll, into = c("lon", "lat"), sep = ",")
  temp$lon<-as.numeric(str_remove(temp$lon,fixed("c(")))
  temp$lat<-as.numeric(str_remove(temp$lat,fixed(")")))
  
# wrangle into dataframe
  dates<-data.frame(matrix(unlist(outList$meta$valid_daterange), nrow=nrow(outList$meta), byrow=T))
  sids<-as.data.frame(t(sapply(outList$meta$sids, '[', seq(max(sapply(outList$meta$sids, length))))))
  names<-outList$meta$name
  stations<-cbind(names,dates,temp$lat,temp$lon,sids)
  colnames(stations)[2:5]<-c("beginYr","endYr","lat","lon")
  stations$beginYr<-as.Date(stations$beginYr, format="%Y-%m-%d")
  stations$endYr<-as.Date(stations$endYr, format="%Y-%m-%d")
  stations$obsN<-stations$endYr-stations$beginYr
  stations$obsYears<-as.numeric(stations$obsN/365)

# find stations with data in current year
stations<-stations[which(stations$endYr>=as.Date(paste0(format(Sys.Date(), "%Y"),"-01-01")) & stations$obsN/365>=30 ),]

# geocode threadEx stations

# threadEX<-as.character(stations$names[which(is.na(stations$lon))])
# threadEX<-str_remove(threadEX, fixed("Area"))
# temp<-geo(city=threadEX,method = 'osm')
# temp<-cbind.data.frame(temp,as.character(stations$names[which(is.na(stations$lon))]))
# colnames(temp)[4]<-"names"
for(i in 1:nrow(stations)){
  if(is.na(stations$lat[i])==TRUE){
      temp<-geo(city=str_remove(stations$names[i], fixed("Area")),method = 'osm')
      stations$lat[i]<-temp$lat
      stations$lon[i]<-temp$long
  }
}

# manually fix lat/lon of Roswell and Clayton NM
#stations<-subset(stations, names!="Clayton Area")
#stations<-subset(stations, names!="Roswell Area")  

# find station type,  find ThreadEx stations too
# test<-mapply(grepl,"US", stations)

# plot points on map

leaflet(data = stations) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(names), label = ~as.character(names))


# find out missing day counts by station and contiguous blocks of years
stations$contigYr1<-NA
stations$contigYr2<-NA
    
    # download data in JSON format and convert in loop
  endDate<-paste0(as.numeric(format(Sys.Date(),"%Y")),"-12-31")
  for(i in 1:nrow(stations)){
    jsonQuery=paste0('{"sid":"',stations$V1[i],'","meta":"name,ll,elev","sdate":"por","edate":"',endDate,'","elems":"1,2,43,4,10,11"}') # sid = station id, 029439=Winslow, arizona
    out<-postForm("http://data.rcc-acis.org/StnData", 
                  .opts = list(postfields = jsonQuery, 
                               httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
    out<-fromJSON(out)
    
    # meta data
    #out$meta
    
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
    stations$contigYr1[i]<-temp$year[por[[1]]]
    # find last year in POR
    stations$contigYr2[i]<-temp$year[por[[2]]]
    # trim to por
    #data<-subset(data, year>=beginyr)

    print(paste(i, stations$names[i]))
    Sys.sleep(sample(10, 1) * 0.25)
}

# thin stations to real-time reporting and long records
  stations$POR<-stations$contigYr2-stations$contigYr1
  
stationThin<-subset(stations, contigYr2>=2020)

# plot to check
leaflet(data = stationThin) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(names), label = ~as.character(names))

# fix roswell area lat/lon
temp<-geo(city="Roswell",state="NM",method = 'osm')
idx<-which(stationThin$names=="Roswell Area")
stationThin$lat[idx]<-temp$lat
stationThin$lon[idx]<-temp$long

# final station ID - manually add/delete to this list as well
# load existing station list
stationThin<-stationThin[,c("names","lat","lon","V1","contigYr1","contigYr2")]

save(stationThin, file="stationList.RData")

# manually add stations - info from https://wrcc.dri.edu/coopmap/#
load("~/RProjects/StationPlots/stationList.RData")
stationThin$names<-as.character(stationThin$names)
stationThin$V1<-as.character(stationThin$V1)

  # station info
  newrow<-c("KARTCHNER CAVERNS",31.83528,-110.35528, "024534", 2008,2020)
  # bind to dataframe
  stationThin<-rbind.data.frame(stationThin,newrow)
  # fix var types
  vars<-c(2,3,5,6)
  stationThin[ , vars] <- apply(stationThin[ , vars], 2,            # Specify own function within apply
                      function(x) as.numeric(as.character(x)))
  
  save(stationThin, file="stationList.RData")
  
