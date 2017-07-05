require(tidyr)
require(dplyr)
require(rgdal)
require(ggplot2)

### Read in data, tag neighborhood cluster ###
### Read in data, tag neighborhood cluster ###
### Read in data, tag neighborhood cluster ###
liquorFinal <- read.csv("./nightlife0816.csv", 
                        stringsAsFactors = FALSE)

cluster = readOGR(dsn="https://opendata.arcgis.com/datasets/f6c703ebe2534fc3800609a07bad8f5b_17.geojson", 
                  layer="OGRGeoJSON")
latLon <- liquorFinal %>% select(lon, lat)

points <- SpatialPoints(latLon, proj4string=
                          CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
clusterID <- over(points, cluster)

nlCluster <- cbind(liquorFinal, clusterID)
rm(latLon, points, clusterID)

### Create dataset with counts of openings and closing per year per neighborhood ###
### Create dataset with counts of openings and closing per year per neighborhood ###
### Create dataset with counts of openings and closing per year per neighborhood ###
hoodTimeline <- data.frame(NBH_NAMES = rep(cluster@data$NBH_NAMES, 8)) %>%
                          arrange(NBH_NAMES) %>%
                          mutate(year = rep(c(2009,2010,2011,2012,2013,2014,2015,2016), 46))

#create last open year
last <- nlCluster %>%
  group_by(license) %>%
  arrange(year) %>%
  slice(n())   %>%
  rename(lastYr = year) %>%
  select(license, lastYr)

all <- nlCluster %>% left_join(last, by="license") %>%
                      group_by(license) %>%
                      arrange(year) %>%
                      slice(n())

openYr <- all %>% group_by(NBH_NAMES, firstYr) %>%
                     summarise(firstYrCount = n()) %>%
                     rename(year = firstYr)

closeYr <- all %>% group_by(NBH_NAMES, lastYr) %>%
                    summarise(lastYrCount = -n()) %>%
                    mutate(year = lastYr + 1) %>%
                    select(-lastYr)

openClose <- hoodTimeline %>% left_join(openYr, by=c("NBH_NAMES", "year")) %>%
                              left_join(closeYr, by=c("NBH_NAMES", "year"))
openClose[is.na(openClose)] <- 0
rm(openYr, closeYr, last)

hoodCount <- openClose %>% subset(firstYrCount != 0 | lastYrCount != 0) %>%
                           count(NBH_NAMES) %>%
                           subset(n>4)

hoodChangeSum <- openClose %>% group_by(NBH_NAMES) %>%
                               summarise(totalOpen = sum(firstYrCount), 
                                         totalClose = sum(lastYrCount)) %>%
                               subset(totalOpen > 10)

openClose <- openClose %>% subset((NBH_NAMES %in% hoodCount$NBH_NAMES) &
                                    (NBH_NAMES %in% hoodChangeSum$NBH_NAMES) & year != 2008)
rm(hoodCount, hoodChangeSum)

unq <- openClose %>% group_by(NBH_NAMES) %>%
                     slice(n()) %>%
                     select(NBH_NAMES)

unq$NBH_SHORT <- c("Petworth", "Brookland", "Capitol Hill (SE)", 
                  "Glover Park", "Cleveland/Woodley Park", "Columbia Heights", 
                  "Downtown, Chinatown", "Dupont Circle, K St.", "Bloomingdale",                                        
                  "Friendship Heights", "Georgetown", "Shaw", "Ivy City", 
                  "Adams Morgan", "Navy Yard", "Logan Circle", "Takoma, Brightwood", 
                  "Union Station, H St.", "West End")
                  
openClose <-openClose %>% left_join(unq, by="NBH_NAMES")
toPlot <- gather(openClose, action, count, firstYrCount:lastYrCount, factor_key=TRUE)


hoodOpenClose <- ggplot(toPlot, aes(x=year, y=count, fill=action, width=1)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#8da0cb","#fc8d62")) +
  scale_y_continuous(limits = c(-40, 40)) +
  scale_x_discrete(limits = c(2009, 2016)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = '#F2F2F2'),
        strip.background = element_rect(fill="#cccccc", color = "#cccccc"),
        strip.text.x = element_text(size = 11),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none") +
  facet_wrap(~NBH_SHORT)
ggsave(hoodOpenClose,height=5,width=9, filename="hoodOpenClose.pdf", useDingbats=FALSE)


### Create dataset with counts of openings and closing per year per neighborhood ###
### Create dataset with counts of openings and closing per year per neighborhood ###
### Create dataset with counts of openings and closing per year per neighborhood ###
liquorFinal10 <- liquorFinal %>% subset(firstYr==2010) %>%
                                 group_by(firstYr, year) %>%
                                 summarise(count = n())                            
liquorFinal11 <- liquorFinal %>% subset(firstYr==2011) %>%
                                 group_by(firstYr, year) %>%
                                 summarise(count = n())  
liquorFinal12 <- liquorFinal %>% subset(firstYr==2012) %>%
                                 group_by(firstYr, year) %>%
                                 summarise(count = n())  
liquorFinal13 <- liquorFinal %>% subset(firstYr==2013) %>%
                                 group_by(firstYr, year) %>%
                                 summarise(count = n())  
liquorFinal14 <- liquorFinal %>% subset(firstYr==2014) %>%
                                 group_by(firstYr, year) %>%
                                 summarise(count = n())  
liquorFinal15 <- liquorFinal %>% subset(firstYr==2015) %>%
                                 group_by(firstYr, year) %>%
                                 summarise(count = n())  

turnover <- as.data.frame(rbind(liquorFinal10, liquorFinal11, liquorFinal12, 
                  liquorFinal13, liquorFinal14, liquorFinal15)) %>%
            mutate(firstYr = as.factor(firstYr), 
                   lagCount = ifelse(firstYr != year, lag(count), NA), 
                   change = (count-lagCount)/lagCount)


color <- c('#4d004b', '#810f7c','#88419d','#8c6bb1','#8c96c6','#9ebcda')

turnoverPlot <- ggplot(data=turnover, aes(x=year, y=count, group=firstYr, color=firstYr)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = count)) +
      scale_x_discrete(name ="", 
                     limits=c(2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
      scale_colour_manual(values=color) + 
      theme_bw() +
      theme(panel.background = element_rect(fill = '#F2F2F2'),
        strip.background = element_rect(fill="#cccccc", color = "#cccccc"),
        strip.text.x = element_text(size = 11),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line=element_blank(),
        legend.position = "none")
ggsave(turnoverPlot,height=5,width=9, filename="turnoverPlot.pdf", useDingbats=FALSE)

