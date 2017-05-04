setwd("/Users/katerabinowitz/Documents/DataLensDC/Breweries")
library(stringr)
library(plyr)
library(reshape)
library(ggmap)
### Read in Data ###
### Read in Data ###
### Read in Data ###
brew<-read.csv("brewery_data.csv",stringsAsFactors=FALSE, strip.white=TRUE)
zip2cbsa<-read.csv("zip2cbsa.csv",stringsAsFactors=FALSE, strip.white=TRUE)
cbsaPop<-read.csv("https://www.census.gov/popest/data/metro/totals/2012/tables/CBSA-EST2012-01.csv",
                  stringsAsFactors=FALSE, strip.white=TRUE, skip=5, header=FALSE)[c(1:3,8)]

### Shape and Factor ###
### Shape and Factor ###
### Shape and Factor ###
colnames(cbsaPop)<-c("CBSA","GeoArea","CBSAName","Pop2012")
cbsaPop$Pop<-as.numeric(gsub(",","",cbsaPop$Pop2012))
cbsaPop<-cbsaPop[-c(412:955), ]
geoArea<-subset(cbsaPop,!(is.na(cbsaPop$GeoArea)))[c(1:2)]
cbsaPop<-subset(cbsaPop,is.na(cbsaPop$GeoArea))[c(1,3:5)]
cbsaPop<-subset(cbsaPop,cbsaPop$Pop>2000000)

brew$BreweryType<-ifelse(grepl("Type:",brew$Type),brew$Type,brew$Type2)
breweries<-subset(brew, grepl("Type:",brew$BreweryType) & brew$Towns != ",  ")[c(2:4,8)]

breweries$ZIP<-as.numeric(str_extract(breweries$Towns,"[[:digit:]]+"))
breweries$Unq<-paste(breweries$Names,breweries$ZIP)

breweriesPro<-subset(breweries,breweries$BreweryType!="Type: Planning" & 
                       breweries$BreweryType!="Type: Contract" & breweries$BreweryType!="Type: Proprietor")

brewZip<-subset(breweriesPro, !is.na(breweriesPro$ZIP))
noZip<-subset(breweriesPro, is.na(breweriesPro$ZIP))
noZip$ZIP<-ifelse(noZip$Towns=="Mesa, AZ  | Map", "85204",
              ifelse(noZip$Towns=="Queens, NY ", "11385",
                ifelse(noZip$Towns=="Raytown, MO ","64133",
                 ifelse(noZip$Towns=="Glenside, PA ","19038",
                  ifelse(noZip$Towns=="San Francisco, CA ","94101",
                    ifelse(noZip$Towns=="Portland, OR ","97201",
                      ifelse(noZip$Towns=="Milwaukee, WI ","53202",
                        ifelse(noZip$Towns=="Oyster Bay, NY ","11771",
                          ifelse(noZip$Towns=="Milwaukie, OR  | Map","97222",
                            ifelse(noZip$Towns=="Plant City, FL ","33565",
                              ifelse(noZip$Towns=="Beaverton, OR  | Map","97223",NA)))))))))))
breweriesPro<-rbind(brewZip,noZip)
breweriesPro<-subset(breweriesPro, !(is.na(breweriesPro$ZIP)))

### Join Breweries to largest CBSAs ###
### Join Breweries to largest CBSAs ###
### Join Breweries to largest CBSAs ###
brewCBSA<-join(breweriesPro, zip2cbsa, by="ZIP",type="left",match="all")
brewCBSA<-subset(brewCBSA, brewCBSA$BUS_RATIO>0.5)[c(1:7)]

brewGeoCBSA<-brewCBSA
brewGeoCBSA$GeoArea<-brewGeoCBSA$CBSA
brewGeoCBSA<-brewGeoCBSA[c(1:6,8)]
brewGeo1<-join(brewGeoCBSA, geoArea, by="GeoArea",type="inner")[c(1:6,8)]
brewGeo2<-subset(brewCBSA, !(brewCBSA$Unq %in% brewGeo1$Unq))
brewGeo<-rbind(brewGeo1,brewGeo2)

brewCBSAPop<-join(brewGeo, cbsaPop, by="CBSA",type="left",match="all")
final<-subset(brewCBSAPop, !is.na(brewCBSAPop$Pop))

### Brewery Summaries ###
#Brew Count
brewSum<-ddply(final, c("CBSAName", "Pop"), nrow)
brewSum<-brewSum[order(-brewSum$V1), ]
brew20<-brewSum[c(1:20),]
#Brew Type Count
brewTypeSum<-ddply(final, c("CBSAName", "BreweryType"), nrow)
brewTSum<- cast(brewTypeSum, CBSAName~BreweryType)
colnames(brewTSum)<-c("CBSA","Brewpub","Large","Micro","Regional")
brewTSum[is.na(brewTSum)] <- 0
brewTSum$Total<-brewTSum$Brewpub + brewTSum$Large + brewTSum$Micro + brewTSum$Regional
brewTSum<-brewTSum[order(-brewTSum$Total), ]

brewTSum$Name<-str_trim(gsub("\\,.*","",brewTSum$CBSA))
brewTSum$Place<-str_trim(gsub("\\-.*","",brewTSum$Name))
brewTSum<-brewTSum[c(1:20),c(8,4,2,5,3)]
brewTSum$Place<-ifelse(brewTSum$Place=="Washington","Washington, D.C.",brewTSum$Place)
write.csv(brewTSum,"brewTSum.csv",row.names=FALSE)

### Planned Breweries with Set Names and Locations ###
### Planned Breweries with Set Names and Locations ###
### Planned Breweries with Set Names and Locations ###
Plan<-subset(breweries,breweries$BreweryType=="Type: Planning")
Plan<-subset(Plan,!(grepl("Brewery In Planning", Plan$Names)))
Plan$town<-str_trim(gsub("\\,.*","",Plan$Towns))
Plan<-subset(Plan,Plan$town!="")
PlanNoZip<-subset(Plan, is.na(Plan$ZIP))
PlanZip<-subset(Plan, !(is.na(Plan$ZIP)))
PlanNoZip$State<-str_trim(gsub(".*, ","",PlanNoZip$Towns))
PlanNoZip<-subset(PlanNoZip,PlanNoZip$State %in% c("WA","IL","IN","WI","OR","CO","CA","NJ","NY","PA","DC","VA",
                                                   "MD","WV","WI","MN", "DE", "MA","NH","FL","TX","AZ","MO","GA"))
PlanNoZip$CBSA<-ifelse(PlanNoZip$town=="Chicago" | PlanNoZip$town=="Downers Grove" 
                       | PlanNoZip$town=="Oak Park" | PlanNoZip$town=="Portage" | PlanNoZip$town=="Highland","16980",
                  ifelse(PlanNoZip$town=="Fairfax"| PlanNoZip$town=="Montgomery County"
                         | PlanNoZip$town=="Arlington"| PlanNoZip$town=="Alexandria" 
                         | PlanNoZip$town=="Washington","47900",
                    ifelse(PlanNoZip$town=="Brooklyn" | PlanNoZip$town=="Northport"
                           | (PlanNoZip$State=="NJ" & PlanNoZip$town!="Egg Harbor"),"35620",
                      ifelse(PlanNoZip$town=="San Francisco" | PlanNoZip$Towns=="Lafayette, CA "
                             | PlanNoZip$town=="Castro Valley","41860",
                        ifelse(PlanNoZip$town=="Denver"| PlanNoZip$town=="Aurora" | PlanNoZip$town=="Castle Rock"
                               | PlanNoZip$town=="Westminster" | PlanNoZip$town=="Littleton" 
                               | PlanNoZip$town=="Evergreen","19740",
                          ifelse(PlanNoZip$town=="Anaheim" | PlanNoZip$town=="Los Angeles"
                                 | PlanNoZip$town=="Sherman Oaks" | PlanNoZip$town=="Glendale"
                                 | PlanNoZip$town=="Palmdale" | PlanNoZip$town=="Burbank","31080",
                            ifelse(PlanNoZip$town=="Portland" | PlanNoZip$town=="Vancouver","38900",
                              ifelse(PlanNoZip$town=="Santee","41740",
                                ifelse(PlanNoZip$town=="St. Louis Park","33460",
                                  ifelse(PlanNoZip$town=="Lynwood" | PlanNoZip$town=="Bellevue"
                                         | PlanNoZip$town=="Snohomish","42660",
                                    ifelse(PlanNoZip$town=="Sacramento","40900",
                                      ifelse(PlanNoZip$town=="Denton"| PlanNoZip$town=="Dallas"
                                             | PlanNoZip$town=="Fort Worth","19100",
                                        ifelse(PlanNoZip$town=="Phoenix"| PlanNoZip$town=="Mesa"
                                               | PlanNoZip$town=="Gilbert","38060",
                                          ifelse(PlanNoZip$town=="Middletown" | PlanNoZip$town=="Philadelphia","37980",
                                            ifelse(PlanNoZip$town=="Brandon","45300",
                                              ifelse(ifelse(PlanNoZip$town=="Peachtree City","12060",
                                                ifelse(PlanNoZip$town=="Cottleville","41180",NA))))))))))))))))))
PlanNoZip<-PlanNoZip[c(1:7,9)]
PlanCBSA<-join(PlanZip, zip2cbsa, by="ZIP",type="left",match="all")
PlanCBSA<-subset(PlanCBSA, PlanCBSA$BUS_RATIO>0.5)[c(1:8)]
Plan<-rbind(PlanNoZip,PlanCBSA)
Plan<-subset(Plan,!(is.na(Plan$CBSA)))
planGeoCBSA<-Plan
planGeoCBSA$GeoArea<-planGeoCBSA$CBSA
planGeoCBSA<-planGeoCBSA[c(1:7,9)]
Geo1<-join(planGeoCBSA, geoArea, by="GeoArea",type="inner")[c(1:6,9)]
Geo2<-subset(PlanCBSA, !(PlanCBSA$Unq %in% Geo1$Unq))[c(1:6,8)]
Geo<-rbind(Geo1,Geo2)

plan<-join(Geo, cbsaPop, by="CBSA",type="left",match="all")
plan<-subset(plan, !is.na(plan$Pop))
plan<-subset(plan,plan$CBSAName %in% brew20$CBSAName)

planSum<-ddply(plan, c("CBSAName", "Pop"), nrow)
planSum<-planSum[order(-planSum$V1), ]
colnames(planSum)[c(3)]<-c("Planned")
planSum$Name<-str_trim(gsub("\\,.*","",planSum$CBSAName))
planSum$Place<-str_trim(gsub("\\-.*","",planSum$Name))
planSum<-planSum[c(3,5)]
planSum$Place<-ifelse(planSum$Place=="Washington","Washington, D.C.",planSum$Place)
write.csv(planSum, "planSum.csv",row.names=FALSE)
All<-join(planSum,brewTSum,by="Place",type="inner")[c(2,3,4,5,6,1)]
All$Total<-All$Planned+All$Micro+All$Regional+All$Large+All$Brewpub
All<-All[order(-All$Total), ]
All<-All[c(1:6)]
write.csv(All, "currentNPlan.csv",row.names=FALSE)

#DC breweries with geolocation for mapping
DCbrew<-subset(final,final$CBSAName=="Washington-Arlington-Alexandria, DC-VA-MD-WV")
DCbrew$City<-str_trim(gsub("\\|.*","",DCbrew$Towns))
DCbrew$Address<-paste(DCbrew$Street,', ',DCbrew$City, sep="")
DCBrews<-DCbrew$Address

Address<-geocode(DCBrews, source=c("google"))
DCbrew<-cbind(DCbrew,Address)[c(1,4,12:14)]

DCbrew$Names<-gsub("Great American Restaurants, ", "", DCbrew$Names)
DCbrew$Names<-ifelse(grepl("509 7th St NW",DCbrew$Address),"District Chophouse & Brewery",
                ifelse(grepl("4238 Wilson Blvd Ste 1256",DCbrew$Address),"Rock Bottom",
                  ifelse(grepl("7900 Norfolk Ave",DCbrew$Address),"Rock Bottom",
                    ifelse(grepl("200 E Middle Ln Unit A",DCbrew$Address),"Gordon Biersch",
                      ifelse(grepl("7861 L. Chain Bridge Rd.",DCbrew$Address),"Gordon Biersch",
                        ifelse(grepl("900 F St NW",DCbrew$Address),"Gordon Biersch",
                          ifelse(grepl("100 M St SE",DCbrew$Address),"Gordon Biersch",DCbrew$Names)))))))
DCbrew$BreweryType<-gsub("Type: ", "", DCbrew$BreweryType)
write.csv(DCbrew, "DCbrew.csv",row.names=FALSE)

