library(stringr)
library(plyr)
library (dplyr)
library(data.table)
library(maptools)
library (rgdal)

### Read in Data ###
### Read in Data ###
### Read in Data ###
fmRawAll<-read.csv('https://apps.ams.usda.gov/FarmersMarketsExport/ExcelExport.aspx',na.strings=c("", "NA"),
                strip.white=TRUE) 
fmRawCompare<-fmRawAll[c(1,8,9,11,12)]
fmRawLocation<-fmRawAll[c(1:4,8,13:22,24:59)]
PopRaw<-read.csv('http://www.census.gov/popest/data/cities/totals/2014/files/SUB-EST2014_ALL.csv',
                 strip.white=TRUE)[c(1,2,4,9,10,17)]
zctaRaw<-read.csv('http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_place_rel_10.txt', na.strings=c("", "NA"),
                  strip.white=TRUE)

### Transform Data ###
### Transform Data ###
### Transform Data ###
#subset to incorporated level
cityPop<-subset(PopRaw,SUMLEV==162)
cityPop$Ccity<-gsub(" city","",cityPop$NAME,ignore.case=T)
cityPop<-transform(cityPop, State=as.character(tolower(cityPop$STNAME)),
                   Ccity=tolower(cityPop$Ccity))[c(2:3,6:8)]
cityPop$placeState<- paste(cityPop$STATE,cityPop$PLACE)
#only keep cities with population over 500K
cityPop500<-cityPop[which(cityPop$POPESTIMATE2014 > 500000),]
states<-cityPop500$State
unqState<-states[!duplicated(states)]
cityPop500<-cityPop500[order(cityPop500$Ccity), ]

### Make geographies consistent and match census population places###
fmRawCompare$State<-tolower(fmRawCompare$State)
fmRawCompare$city<-tolower(as.character(fmRawCompare$city))
fmRawCompare$city<-ifelse(fmRawCompare$State=='district of columbia', 'washington',
                 ifelse((fmRawCompare$State=='new york' & fmRawCompare$city=='brooklyn') | (fmRawCompare$State=='new york' & fmRawCompare$city=='queens') |
                          (fmRawCompare$State=='new york' &  fmRawCompare$city=='bronx') | (fmRawCompare$State=='new york' & fmRawCompare$city=='manhattan') |
                          (fmRawCompare$State=='new york' & fmRawCompare$city=='new york city'),'new york', fmRawCompare$city))
fm<-fmRawCompare[!is.na(fmRawCompare$zip), ]
fmnozip<-fmRawCompare[is.na(fmRawCompare$zip), ]

#match where zip
ZCTA<-merge(zctaRaw, cityPop, by=c("PLACE", "STATE"))
ZCTA$zip<-ZCTA$ZCTA5

fm.ZCTA<-join(fm, ZCTA, by="zip", type='left',match="all")

fm.500place<-subset(fm.ZCTA,fm.ZCTA$placeState %in% cityPop500$placeState)[c(1:5,31,32,34)]

#match where no zip
fmnozip2<-fmnozip[!is.na(fmnozip$city), ]
popNoZip<-subset(cityPop500,select=c(Ccity, State,PLACE,STATE, placeState))

fm.NoZipmatch<-merge(fmnozip2, popNoZip, by.x=c("city", "State"),by.y=c("Ccity","State"))
fm.NoZipmatch<-fm.NoZipmatch[c('FMID','city','State','placeState')]

#mismatches from zcta side
Zmismatch<-subset(fm.500place,fm.500place$Ccity != fm.500place$city)

remove<-c('nashville','indianapolis','louisville', 'oklahoma city')
Zmismatch2<-Zmismatch[!Zmismatch$city  %in%  remove,]

keep<-c(1010105,1006021,1005208,1009715,1009714,1000213,1000093,1000094,1010401,
        1000710,1002082,1002081,1009778,1003546,1003932,1007684,1009304,1003273)
toremove<-Zmismatch2[!Zmismatch2$FMID  %in%  keep,]

#explore mismatches on farmers market data side
Fmismatch<-merge(fm, popNoZip, by.x=c("city", "State"),by.y=c("Ccity","State"))
cityMismatch<-Fmismatch[!Fmismatch$FMID %in% fm.500place$FMID,]

remove<-c(1003313,1001297,1002940,1002273,1007758,1007153,1004539,1008870,1000820,1002993,1008562,
          1010997,1003552,1003540,1002290,1007020)
addIn<-cityMismatch[!cityMismatch$FMID %in% remove,]
addIn<-addIn[c('FMID','city','State','placeState')]

#put it all together
fm.city<-fm.500place[!fm.500place$FMID %in% toremove$FMID,][c('FMID','city','State','placeState')]
fm.all<-rbind(fm.city,addIn,fm.NoZipmatch)

### Calculate people per farmers market and farmers market per capita###
### Calculate people per farmers market and farmers market per capita###
### Calculate people per farmers market and farmers market per capita###
fmSum<-ddply(fm.all, c("placeState"), nrow)
placesort<-fmSum[order(fmSum$placeState), ]

fmCityPop<-merge(x=fmSum, y=cityPop500, by= c("placeState"),all.x=TRUE)

fmCityPop$capitaperFM<-fmCityPop$POPESTIMATE2014/fmCityPop$V1
fmCityPop$percapita<-round(((fmCityPop$V1/fmCityPop$POPESTIMATE2014)*100000),digits=2)
fmCityPop<-fmCityPop[order(-fmCityPop$percapita),]

### Clean up output files ###
### Clean up output files ###
### Clean up output files ###
rm(list=setdiff(ls(), "fmCityPop"))

fmCityPop$Ccity<-ifelse(fmCityPop$Ccity=='louisville/jefferson county metro government (balance)', 'Louisville',
                    ifelse(fmCityPop$Ccity=='indianapolis (balance)', 'Indianapolis',
                           ifelse(fmCityPop$Ccity=='nashville-davidson metropolitan government (balance)', 'Nashville', fmCityPop$Ccity)))

fmCityPop$city1<-gsub( " .*$", "", fmCityPop$Ccity)
fmCityPop$city2<-gsub("[A-z ]* ","", fmCityPop$Ccity)
fmCityPop$city2<-ifelse(fmCityPop$city2==fmCityPop$Ccity,"",fmCityPop$city2)
fmCityPop$city1a<-substr(fmCityPop$city1,1,1)
fmCityPop$city1b<-substr(fmCityPop$city1,2,100)
fmCityPop$city2a<-substr(fmCityPop$city2,1,1)
fmCityPop$city2b<-substr(fmCityPop$city2,2,100)

fmCityPop$Ccity<-paste(toupper(fmCityPop$city1a),fmCityPop$city1b," ", toupper(fmCityPop$city2a),sep="",fmCityPop$city2b)

fmCityPop<-fmCityPop[c(6,9)]
write.csv(fmCityPop, 
          file="/Users/katerabinowitz/Documents/DataLensDC/Farmer's Market City Comparison/Farmers-Market-City-Comp/Output/Capita for FM.csv")

top10FMCity<-head(fmCityPop,10)
write.csv(top10FMCity, 
file="/Users/katerabinowitz/Documents/DataLensDC/Farmer's Market City Comparison/Farmers-Market-City-Comp/Output/top10FMCity.csv")


### Create KML file for DC locations ###
### Create KML file for DC locations ###
### Create KML file for DC locations ###
#geocode DC address
DCFM<-fm.all[fm.all$city=="washington",]
DCFM.all<-merge(DCFM, fmRawLocation, by=c("FMID"))
rm(list=setdiff(ls(), "DCFM.all"))
DCFM.all$name<-DCFM.all$MarketName
DCFM.all$date<-DCFM.all$Season1Date
DCFM.all$time<-DCFM.all$Season1Time
DCFM.all$address<-DCFM.all$street
DCFM2kml<-DCFM.all[c('name','date','time','address','x','y')]
write.csv(DCFM2kml, 
          file="/Users/katerabinowitz/Documents/DataLensDC/Farmer's Market City Comparison/Farmers-Market-City-Comp/Output/DCFM.csv")

attach(DCFM2kml)
coordinates(DCFM2kml) <- c("x", "y")
proj4string(DCFM2kml) <- CRS("+init=EPSG:4326")
DCFM2kml_ll <- spTransform(DCFM2kml, CRS("+proj=longlat +datum=WGS84"))
writeOGR(DCFM2kml_ll, "/Users/katerabinowitz/Documents/DataLensDC/Farmer's Market City Comparison/Farmers-Market-City-Comp/Output/CDFM-expanded.kml",
         "DCFM", "KML")
