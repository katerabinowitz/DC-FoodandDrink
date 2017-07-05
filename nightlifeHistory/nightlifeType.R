require(tidyr)
require(dplyr)
require(rgdal)
require(ggplot2)

### Read in data, add license type from previous dataset ###
### Read in data, add license type from previous dataset ###
### Read in data, add license type from previous dataset ###
liquorFinal <- read.csv("./nightlife0816.csv", 
                        stringsAsFactors = FALSE)

#allTy from read-in code. originally didn't have type in final data csv
toMerge <- allTy %>% group_by(license) %>%
  arrange(year) %>%
  slice(n()) %>%
  select (license, type) %>%
  mutate(type = ifelse(grepl("CN|DN", type), "Nightclub",
                  ifelse(grepl("CR|DR", type), "Restaurant", 
                    ifelse(grepl("CT|DT", type), "Tavern", 
                           type))))

merged <- left_join(liquorFinal, toMerge, by="license")

why <- llType %>% subset(is.na(type))

# places with the same license number but different names were given ".1" to end of liquor license
# remove .1 to pull in type match and then revert back
out <- merged %>% subset(is.na(type)) %>%
  select(-type) %>%
  rename(license2 = license)

match <- merged %>% subset(!(is.na(type)))

out$license <- gsub("\\..*","", out$license2)
outMerge <- left_join(out, toMerge, by="license") %>%
            select(-license) %>%
            rename(license=license2)

llType <- rbind(match, outMerge)

### Tag neighborhood cluster and ANCs ###
### Tag neighborhood cluster and ANCs ###
### Tag neighborhood cluster and ANCs ###
cluster = readOGR(dsn="https://opendata.arcgis.com/datasets/f6c703ebe2534fc3800609a07bad8f5b_17.geojson", 
                  layer="OGRGeoJSON")
latLon <- llType %>% select(lon, lat)

points <- SpatialPoints(latLon, proj4string=
                          CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
clusterID <- over(points, cluster)

nlCluster <- cbind(llType, clusterID)

ancs = readOGR("https://opendata.arcgis.com/datasets/890415458c4c40c3ada2a3c48e3d9e59_21.geojson", layer="OGRGeoJSON")
ancID <- over(points, ancs)
nlANC <- cbind(llType, ancID)[c(1:10, 12:13)]

rm(cluster, latLon, points, clusterID, ancs, ancID)

### Summary, exploratory stats for neighborhood level ###
### Summary, exploratory stats for neighborhood level ###
### Summary, exploratory stats for neighborhood level ###
nlCluster %>% group_by(year, type) %>%
  summarise(n = n()) %>%
  subset(year %in% c(2008,2016))

nlCount <- nlCluster %>% group_by(NBH_NAMES, year, type) %>%
  summarise(n = n()) %>%
  subset(year %in% c(2008,2016)) 

nlChg <- nlCount %>% spread(year, n) 

colnames(nlChg) <- c("NBH_NAMES", "type", "yr08", "yr16")

nlChg <- nlChg %>% mutate(growth = (yr16 - yr08) / yr08,
                              growthN = yr16 - yr08)

nlSum <- nlCount %>% group_by(NBH_NAMES, year) %>%
  summarise(sum = sum(n)) %>%
  subset(year==2016 & sum > 10)

nlFreq <- data.frame(left_join(nlSum, nlCount, by=c("NBH_NAMES", "year")) %>%
          mutate(type = ifelse(type == "Tavern", "Bar", type),
                 type = factor(type, levels = c("Restaurant", "Bar", "Nightclub"))) %>%
          group_by(NBH_NAMES) %>%
          arrange(NBH_NAMES, desc(type)) %>%
          mutate(prop = (n / sum)*100, 
          propSum = cumsum(prop)))

unq <- nlFreq %>% group_by(NBH_NAMES) %>%
  slice(n()) %>%
  select(NBH_NAMES)

unq$NBH_SHORT <- c("Petworth", "Brookland", "Capitol Hill (SE)", 
                   "Glover Park", "Cleveland/Woodley Park", "Columbia Heights", 
                   "Downtown, Chinatown", "Dupont Circle, K St.", "Bloomingdale",                                        
                   "Friendship Heights", "Georgetown", "Chevy Chase", "Shaw", "Ivy City", 
                   "Adams Morgan", "Navy Yard", "Logan Circle", "Palisades", "Takoma, Brightwood", 
                   "Union Station, H St.", "West End")

nlFreq <- left_join(nlFreq, unq, by="NBH_NAMES")

#reorder so neighborhood w highest restuarant prop on top
nlFreq$NBH_SHORT <-factor(nlFreq$NBH_SHORT,levels=levels(reorder(nlFreq[nlFreq$type=="Restaurant",]$NBH_SHORT,
                                                             nlFreq[nlFreq$type=="Restaurant",]$prop)))
nlFreq <- nlFreq %>% arrange(NBH_SHORT, type)

colors <- c("#1affbe", "#B693FE", "#FF9DE2")

#proportion of restaurants, bars, nightclubs per neighborhood
prop <- ggplot(data=nlFreq, aes(x=NBH_SHORT, y=prop, fill=type, color=type)) +
  geom_bar(stat="identity", width=0.2) +
  geom_point(aes(x=NBH_SHORT, y=propSum, fill=type, color=type), size=3) +
  coord_flip() +
  scale_colour_manual(values=colors) + 
  scale_fill_manual(values=colors) + 
  scale_y_continuous(limits=c(0,100), expand=c(0.02,0.02)) +
  labs(x = "", y="Percent of liquor licenses", colour = "", fill="") +
  theme_bw() +
  theme(panel.background = element_rect(fill = '#F2F2F2'),
        strip.background = element_rect(fill="#cccccc", color = "#cccccc"),
        panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line=element_blank(),
        axis.ticks = element_blank(),
        legend.position="top")
ggsave(prop,height=5,width=9, filename="prop2.pdf", useDingbats=FALSE)

### Look at license count pre/post moratorim relaxations ###
### Look at license count pre/post moratorim relaxations ###
### Look at license count pre/post moratorim relaxations ###

#use ANCs as starter moratorium geographies. remove locations that are outside range of moratorium (PDF maps on ABRA website)
wDupont <- nlANC %>% subset(SMD_ID %in% c("2B02", "2B06")) %>%
                    arrange(SMD_ID, address) %>%
                    subset(!(grepl("17th|19th|18TH|19TH|H ST|I ST|K ST|L ST|M ST|PENNSYLVANIA AVE NW|1150 CONNECTICUT|1140 CONNECTICUT", address)))

eDupont <- nlANC %>% subset(SMD_ID %in% c("2B03", "2B04", "2B05", "2B07")) %>%
                     arrange(address) %>%
                     subset(!grepl("CONNECTICUT|15TH|19TH|K ST|M ST|Connecticut|I ST|20TH|DE SALES|M ST|N ST|JEFFERSON|15TH|DC, ", address)) %>%
                     mutate(block = substr(address, 1, 2),
                       keep = ifelse(grepl("17TH|17th|16TH|18TH", address) & block %in% c("15", "16", "17"), 1,
                                     ifelse(block %in% c("16", "17"), 1, 0))) %>%
                    subset(keep==1) %>%
                    select(-block, -keep)

adMo <- nlANC %>% subset(SMD_ID %in% c("1C01", "1C02", "1C04", "1C07")) %>%
                  arrange(address) %>%
                  subset(!grepl("1967 CALVERT",address))

ancMor <- rbind(wDupont, eDupont, adMo) %>%
          subset(type=="Restaurant") %>%
          mutate(hood = ifelse(SMD_ID %in% c("1C01", "1C02", "1C04", "1C07"), "AdMo",
                               ifelse(SMD_ID %in% c("2B02", "2B06"), "wDupont", "eDupont"))) %>%
          group_by(year, hood) %>%
          summarise(n = n()) %>%
          mutate(mor = ifelse(hood == "wDupont", 
                                      ifelse(year >= 2010, 'Relaxed Moratorium', 'Moratorium'),
                        ifelse(hood == "eDupont", 
                                      ifelse(year >= 2013, 'Relaxed Moratorium', 'Moratorium'), 
                        ifelse(hood == "AdMo", 
                                      ifelse(year >= 2014, 'Relaxed Moratorium', 'Moratorium'), NA))),
                 morDot = ifelse(hood == "wDupont", 
                                 ifelse(year == 2010, 'Moratorium', mor),
                          ifelse(hood == "eDupont", 
                                  ifelse(year == 2013, 'Moratorium', mor), 
                          ifelse(hood == "AdMo", 
                                  ifelse(year == 2014, 'Moratorium', mor), NA))))

colors <- c("#4dffcc","#00b380")

mor <- ggplot(ancMor, aes(x=year, y=n, group=hood, colour=mor)) +
  geom_line(size=1) +
  geom_point(aes(color = morDot), size=3) +
  labs(x = "", y="Restaurants", colour = "", fill="") +
  scale_colour_manual(values=colors) + 
  scale_fill_manual(values=colors) + 
  theme_bw() +
  theme(panel.background = element_rect(fill = '#F2F2F2'),
        strip.background = element_rect(fill="#cccccc", color = "#cccccc"),
        panel.border = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line=element_blank(),
        axis.ticks = element_blank(),
        legend.position="top")
ggsave(mor,height=5,width=9, filename="moratorium.pdf", useDingbats=FALSE)
