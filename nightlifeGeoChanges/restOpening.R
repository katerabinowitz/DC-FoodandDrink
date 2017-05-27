library(plyr)
library(reshape2)
library(ggmap)
library(ggplot2)
library(rgdal)
library(RColorBrewer)

setwd("/Users/katerabinowitz/Documents/DataLensDC/DC-FoodandDrink/nightlifeGeoChanges")

###Read in Data###
###Read in Data###
###Read in Data###
cof <- read.csv("dataIn/COFO_to_DMPED-Table 1.csv", fill = FALSE, strip.white = TRUE,
                skip=1) 
abra<-read.csv("dataIn/tabula-ABCLicenseeList1212015Update_Nightlift.csv",
               fill = FALSE, strip.white = TRUE,stringsAsFactors=FALSE)
###Show, Scope### 
###Show, Scope### 
###Show, Scope### 
table(cof$PROPOSED.USE.OF.PREMISES)
restaurant<-subset(cof,
                   grepl("RESTAURANT|Restaurant|Taverns|Night Club|Fast Food|Deli|Dance Hall|COCKTAIL|Beverages (alcoholic)|Bar|BAR|Bakery",cof$PROPOSED.USE.OF.PREMISES))
restaurant$PROPOSED.USE.OF.PREMISES<-as.character(restaurant$PROPOSED.USE.OF.PREMISES)
table(restaurant$PROPOSED.USE.OF.PREMISES)

table(abra$DESCRIPTION)
nightLife<-subset(abra,(abra$DESCRIPTION %in% c("Nightclub","Restaurant","Tavern")) &
                    (abra$ADDRESS!="TBD") & (abra$STATUS=="Active"))
nightLife$street<-gsub("WASHINGTON.*|UNIT.*|STE.*|#.*|,|\n","",nightLife$ADDRESS)
head(nightLife$street)
nightLife$street<-trimws(nightLife$street)
open<-merge(nightLife,restaurant,by.x="street",by.y="Address")[c(1:9,17:19,31:32)]

pre01<-subset(nightLife,!(nightLife$ADDRESS %in% open$ADDRESS))

nopurpose<-subset(cof,grepl("other|OTHER|Other|  ",cof$PROPOSED.USE.OF.PREMISES)|grepl("RESTAURANT|LOUNGE|BAR|TAVERN",cof$Description.of.Work))
notpre01<-merge(pre01,nopurpose,by.x="street",by.y="Address")
prepre01<-subset(pre01,!(pre01$ADDRESS %in% notpre01$ADDRESS))

post01<-subset(nightLife,(nightLife$ADDRESS %in% open$ADDRESS))

dups<-open[duplicated(open$street),]
turnover<-subset(open,open$street %in% dups$street)
unqRest<-subset(open,!(open$street %in% dups$street))

# check restaurants that have multiple CoFs against Yelp for first review
write.csv(turnover,"dataMid/turnover.csv")
turnoverUnq<-read.csv("dataMid/turnoverUnq.csv",
                      fill = FALSE, strip.white = TRUE,stringsAsFactors=FALSE)[c(3:12)]
turnoverUnq$selectionYr<-as.numeric(substr(turnoverUnq$Selection.DATE,
                                           nchar(turnoverUnq$Selection.DATE)-4,nchar(turnoverUnq$Selection.DATE)))
turnoverUnq$selectionYr<-ifelse(substr(turnoverUnq$Selection.DATE,1,3)=="DEC",turnoverUnq$selectionYr+1,
                                turnoverUnq$selectionYr)
turnoverUnq$YearOpen<-ifelse(!is.na(turnoverUnq$YelpCorrect),turnoverUnq$YelpCorrect,turnoverUnq$selectionYr)
turnoverUnq<-turnoverUnq[c(1:8,12)]

### right about here is where I realize COF proposed premise is not a quality field
### and going against the whole cof dataset just means multiples and yelp check
### so i abandon cof and just use yelp/Google for the remainder of nightLife data
remain<-subset(nightLife,!(nightLife$LICENSE.. %in% turnoverUnq$LICENSE..))
remain<-remain[order(remain$TRADE.NAME),]
write.csv(remain,"dataMid/remainingOpenDates.csv")
#and now with year open. 
#years 2007, 2006, 2005 should be intrepreted as "anytime before 2008"
remain<-read.csv("dataMid/remainingOpenDates-Done.csv")[c(2:10)]

openYr<-rbind(turnoverUnq,remain)

#create new abc licensees dataset with open year column
abraNew<-openYr
abraNew$YearOpen<-ifelse(abraNew$YearOpen=="","out of scope",
                         ifelse(grepl("c|C",abraNew$YearOpen),"closed",
                                ifelse(abraNew$YearOpen=="m","moved",
                                       ifelse(abraNew$YearOpen=="no","not open",
                                              ifelse(grepl("2001|2002|2003|2004|2005|2006|2007",abraNew$YearOpen),"2007 or earlier",
                                                     abraNew$YearOpen)))))
write.csv(abraNew,"dataOut/ABC Licensees with Open Year.csv",row.names=FALSE)

#group years
toMap<-subset(openYr,nchar(openYr$YearOpen)==4)
toMap$YearOpen<-as.numeric(toMap$YearOpen)
toMap$OpenCat<-ifelse(toMap$YearOpen<2008,"2007 or earlier",
                      ifelse(toMap$YearOpen<2010 & toMap$YearOpen>2007,"2008-9",
                             ifelse(toMap$YearOpen<2012 & toMap$YearOpen>2009,"2010-11",
                                    ifelse(toMap$YearOpen<2014 & toMap$YearOpen>2011,"2012-13",
                                           "2014-15"))))
table(toMap$OpenCat)

### Geocode addresses ###
### Geocode addresses ###
### Geocode addresses ###
toMap$street<-trimws(gsub("WASHINGTON.*|,|\n|  ","",toMap$ADDRESS))
toMap<-toMap[c(4,10:11)]
address<-paste0(toMap$street,", Washington, DC")
latlong<-geocode(address,source="google")
address<-as.data.frame(address)
out<-cbind(toMap,latlong)

#fix where apt/ste number screwed up the geocoding
wrong<-subset(out,!(round(out$lat)==39 & round(out$lon)==-77))
wrong$street1<-ifelse(grepl(" NW ",wrong$street),paste(gsub(' NW.*', '', wrong$street),"NW"),
                      ifelse(grepl(" NE ",wrong$street),paste(gsub(' NE.*', '', wrong$street),"NE"),
                             ifelse(grepl(" SW ",wrong$street),paste(gsub(' SW.*', '', wrong$street),"SW"),
                                    ifelse(grepl(" SE ",wrong$street),paste(gsub(' SE.*', '', wrong$street),"SE"),
                                           wrong$street))))
fixAddress<-paste0(wrong$street1,", Washington, DC")
fixLL<-geocode(fixAddress,source="google")
fix<-cbind(wrong[c(1:3)],fixLL)
right<-subset(out,(round(out$lat)==39 & round(out$lon)==-77))

#slightly move multiple points at same address, i.e. union market
out2<-rbind(fix,right)
out2$concat<-paste(out2$lat,out2$lon)
dups<-out2[duplicated(out2$concat),]
dups$randoms <- runif(81, min=0.0000001, max=0.000001)
dups$lat<-dups$lat+dups$randoms
dups$lon<-dups$lon-dups$randoms
dups<-dups[c(1:6)]
single<-out2[!duplicated(out2$concat),]
restOpenFin<-rbind(dups,single)[c(1:5)]
rm(right, wrong, fix, fixLL,toMap,latlong,dups,single)

#single point map
table(restOpenFin$OpenCat)
write.csv(restOpenFin,"dataOut/restOpenGeo.csv",row.names=FALSE)

### Create neighborhood summaries ###
### Create neighborhood summaries ###
### Create neighborhood summaries ###
hood = readOGR("https://raw.githubusercontent.com/benbalter/dc-maps/master/maps/neighborhood-names.geojson", "OGRGeoJSON")
coords<-restOpenFin[c(4,5)]
latLong<-SpatialPoints(coords, proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")))
hoodID <- over(latLong, hood)
restOpenHood<-cbind(restOpenFin,hoodID)

hoodCount<-data.frame(table(restOpenHood$NAME))

restSum<-count(restOpenHood, c("OpenCat", "NAME"))
restSumD<-dcast(restSum,NAME~OpenCat)
hood<-restSumD[c(1)]
count<-restSumD[c(2:6)]
rest.percent <- count / rowSums(count, na.rm = T)
restSumD<-cbind(hood,rest.percent)

colnames(restSumD)[c(2:6)]<-c("p07","p089","p1011","p1213","p1415")
restSumD[is.na(restSumD)] <- 0
attach(restSumD)
restSumD$pre10<-p07+p089
restSumD$post10<-p1011+p1213+p1415

restOpSum<-merge(hoodCount,restSumD,by.x="Var1",by.y="NAME",all.x=TRUE)
[c(1:2,8:9)]
restOpSum$pre10<-ifelse(restOpSum$Freq<10,NA,restOpSum$pre10)
restOpSum$post10<-ifelse(restOpSum$Freq<10,NA,restOpSum$post10)
restOpSum<-restOpSum[order(restOpSum$post10),]
restOpSum$post10Cat<-ifelse(is.na(restOpSum$post10),NA,
                            ifelse(restOpSum$post10<.35,"Less than 35%",
                                   ifelse(restOpSum$post10>.35 & restOpSum$post10<.5,"35%-50%",
                                          ifelse(restOpSum$post10>=.50 & restOpSum$post10<.75,"50%-75%",
                                                 "75%-100%"))))
colnames(restOpSum)[c(1)]<-"NAME"
restOpMap<-restOpSum[c(1,8:10)]

#bind neighborhood sums to map and output
restOpenHoodGeo<-merge(hood,restOpMap,by="NAME",all.x=TRUE)
writeOGR(restOpenHoodGeo, 'restOpenHoodGeo.geojson','restOpenHoodGeo', driver='GeoJSON',check_exists = FALSE)
rm(coords,count,hood,hoodCount,hoodID,rest.percent,restOpenFin,restOpenHood,restOpMap,restSum,restSumD)

### Create stacked bar chart of youngest and oldest nightlife neighborhoods ###
### Create stacked bar chart of youngest and oldest nightlife neighborhoods ###
### Create stacked bar chart of youngest and oldest nightlife neighborhoods ###
restBar<-subset(restOpSum,!(is.na(restOpSum$post10)))[c(1,3:7)]
restBar<-restBar[order(-restBar$p07),]
oldest<-restBar[(1:5),]
colnames(oldest)<-c("Neighborhood","Pre-2008","2008-9","2010-11","2012-13","2014-15")
oldestBar<-melt(oldest)
oldestBar$Neighborhood<-ifelse(oldestBar$Neighborhood=="Mount Pleasant","Mt. Pleasant",
                               as.character(oldestBar$Neighborhood))
oldestBar$Neighborhood<-factor(oldestBar$Neighborhood,
                               c("Woodley Park","Chevy Chase","West End","Stanton Park","Mt. Pleasant"))

restBar<-restBar[order(-restBar$p1415),]
youngest<-restBar[(1:5),]
colnames(youngest)<-c("Neighborhood","Pre-2008","2008-9","2010-11","2012-13","2014-15")
youngestBar<-melt(youngest)
youngestBar$Neighborhood<-ifelse(youngestBar$Neighborhood=="Mount Vernon Square","Mt. Vernon\nSquare",
                                 ifelse(youngestBar$Neighborhood=="Logan Circle/Shaw","Logan Circle\nShaw",
                                        ifelse(youngestBar$Neighborhood=="Ivy City","Ivy City\nUnion Market",
                                               as.character(youngestBar$Neighborhood))))
youngestBar$Neighborhood<-factor(youngestBar$Neighborhood,
                                 c("Logan Circle\nShaw","Mt. Vernon\nSquare","Petworth","Navy Yard","Ivy City\nUnion Market"))

color<-c("#d0d1e6","#a6bddb","#67a9cf","#1c9099","#016c59")

ggplot(data = oldestBar, aes(x = Neighborhood, y = value, fill = variable)) + 
  geom_bar(stat="identity") + scale_fill_manual(values=color) + coord_flip() + 
  scale_y_continuous(limits = c(0,1.02),labels = scales::percent,expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  theme(legend.position="bottom") + 
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(color="#505050")) +
  theme(legend.background = element_rect(fill = '#EFF0F1')) +
  theme(legend.key = element_rect(color = '#EFF0F1')) +
  theme(legend.text=element_text(color="#505050")) +  
  theme(axis.text.x = element_text()) +
  theme(plot.background = element_rect(fill = '#EFF0F1'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=13)) +
  theme(axis.title.x = element_text(color="#505050")) + 
  theme(plot.title=element_text(family="Helvetica", size=15, face="bold")) +
  theme(plot.subtitle=element_text(color="#505050")) +
  theme(plot.caption=element_text(color="#505050")) +
  labs(x="", y="Nightlife by year opened", 
       title="DC's Oldest Nightlife Neighborhoods",
       subtitle="Highest proportion of nightlife opened before 2008",
       caption="Source: DC ABRA, Yelp")
ggsave("/Users/katerabinowitz/Documents/DataLensDC/Website/DataLensDCsite/Images/oldNightlife.png")



ggplot(data = youngestBar, aes(x = Neighborhood, y = value, fill = variable)) + 
  geom_bar(stat="identity") + scale_fill_manual(values=color) + coord_flip() + 
  scale_y_continuous(limits = c(0,1.02),labels = scales::percent,expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0)) +
  theme(legend.position="bottom") + 
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(color="#505050")) +
  theme(legend.background = element_rect(fill = '#EFF0F1')) +
  theme(legend.key = element_rect(color = '#EFF0F1')) +
  theme(legend.text=element_text(color="#505050")) +  
  theme(axis.text.x = element_text()) +
  theme(plot.background = element_rect(fill = '#EFF0F1'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text = element_text(size=13)) +
  theme(axis.title.x = element_text(color="#505050")) + 
  theme(plot.title=element_text(family="Helvetica", size=15, face="bold")) +
  theme(plot.subtitle=element_text(color="#505050")) +
  theme(plot.caption=element_text(color="#505050")) +
  labs(x="", y="Nightlife by year opened", 
       title="DC's Newest Nightlife Neighborhoods",
       subtitle="Highest proportion of nightlife opened after 2014",
       caption="Source: DC ABRA, Yelp")
ggsave("/Users/katerabinowitz/Documents/DataLensDC/Website/DataLensDCsite/Images/newNightlife.png")