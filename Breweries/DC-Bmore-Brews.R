setwd("/Users/katerabinowitz/Documents/DataLensDC/Breweries")
library(stringr)
library(plyr)
library(reshape)
library(ggmap)
### Read in Data ###
### Read in Data ###
### Read in Data ###
brew<-read.csv("/Users/katerabinowitz/Documents/DataLensDC/brewery_data113.csv",stringsAsFactors=FALSE, strip.white=TRUE)
DCBMBrew<-subset(brew,(grepl("MD",brew$Towns) | grepl("VA",brew$Towns) | grepl("DC",brew$Towns)))
zip2cbsa<-read.csv("zip2cbsa.csv",stringsAsFactors=FALSE, strip.white=TRUE)
cbsaPop<-read.csv("https://www.census.gov/popest/data/metro/totals/2012/tables/CBSA-EST2012-01.csv",
                  stringsAsFactors=FALSE, strip.white=TRUE, skip=5, header=FALSE)[c(1:3,8)]

### Shape and Factor ###
### Shape and Factor ###
### Shape and Factor ###
colnames(cbsaPop)<-c("CBSA","GeoArea","CBSAName","Pop2012")
cbsaPop<-cbsaPop[-c(412:955), ]
DCBM<-subset(cbsaPop, cbsaPop$CBSA=="12580" | cbsaPop$CBSA=="47900")
geoArea<-subset(DCBM,!(is.na(DCBM$GeoArea)))[c(1:2)]
cbsa<-subset(DCBM,is.na(DCBM$GeoArea))[c(1,3)]

DCBMBrew$BreweryType<-ifelse(grepl("Type:",DCBMBrew$Type),DCBMBrew$Type,DCBMBrew$Type2)
breweries<-subset(DCBMBrew, DCBMBrew$Towns != ",  ")[c(2:4,8)]

breweries$ZIP<-as.numeric(str_extract(breweries$Towns,"[[:digit:]]+"))
breweries$Unq<-paste(breweries$Names,breweries$ZIP)

breweriesAdd<-subset(breweries,breweries$Streets!="")

### Join Breweries to largest CBSAs ###
### Join Breweries to largest CBSAs ###
### Join Breweries to largest CBSAs ###
brewCBSA<-join(breweriesAdd, zip2cbsa, by="ZIP",type="left",match="all")
brewCBSA<-subset(brewCBSA, brewCBSA$BUS_RATIO>0.5)[c(1:7)]

brewGeoCBSA<-brewCBSA
brewGeoCBSA$GeoArea<-brewGeoCBSA$CBSA
brewGeoCBSA<-brewGeoCBSA[c(1:6,8)]
brewGeo1<-join(brewGeoCBSA, geoArea, by="GeoArea",type="inner")[c(1:6,8)]
brewGeo2<-subset(brewCBSA, !(brewCBSA$Unq %in% brewGeo1$Unq))
brewGeo<-rbind(brewGeo1,brewGeo2)

final<-join(brewGeo, cbsa, by="CBSA",type="inner")

final$City<-str_trim(gsub("\\|.*","",final$Towns))
final$Address<-paste(final$Street,', ',final$City, sep="")
DCB<-final$Address

Address<-geocode(DCB, source=c("google"))
DCBB<-cbind(final,Address)[c(1,4,10:12)]

DCBB$Names<-gsub("Great American Restaurants, ", "", DCBB$Names)
DCBB$Names<-ifelse(grepl("509 7th St NW",DCBB$Address),"District Chophouse & Brewery",
                     ifelse(grepl("4238 Wilson Blvd Ste 1256",DCBB$Address),"Rock Bottom",
                            ifelse(grepl("7900 Norfolk Ave",DCBB$Address),"Rock Bottom",
                                   ifelse(grepl("200 E Middle Ln Unit A",DCBB$Address),"Gordon Biersch",
                                          ifelse(grepl("7861 L. Chain Bridge Rd.",DCBB$Address),"Gordon Biersch",
                                                 ifelse(grepl("900 F St NW",DCBB$Address),"Gordon Biersch",
                                                        ifelse(grepl("100 M St SE",DCBB$Address),"Gordon Biersch",DCBB$Names)))))))
DCBB$BreweryType<-gsub("Type: ", "", DCBB$BreweryType)
DCBB<-subset(DCBB,DCBB$BreweryType!="Contract" & DCBB$BreweryType!="Proprietor")
write.csv(DCBB, "DCBMoreBrew.csv",row.names=FALSE)
