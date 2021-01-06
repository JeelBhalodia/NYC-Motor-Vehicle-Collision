setwd('C:/Users/laptop/Desktop/BEUTH/SEMESTER3/Urban Technology')

### Read the clean data file from Python
accidents <- read.csv('vehicle_col.csv')

library(ggplot2)
library(dplyr)
library(leaflet)
library(shiny)
library(ggpubr)
library(leaflet.extras)
library(shinythemes)
library(choroplethrZip)


Boro <- c('Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island')

### Accidents by Borough
ggplot(data=subset(accidents, !is.na(BOROUGH)), aes(BOROUGH)) + geom_bar(fill="lightblue", color="black") + 
  xlab("Borough") + ylab('Count of Accidents') + ggtitle('Accidents by Borough (2013 - 2019)') + 
  theme(plot.title = element_text(hjust = 0.5))

###Accidents by Zip code in NYC
accidents_zip_other <- accidents %>% filter(!is.na(`ZIP.CODE`)) %>% 
  group_by(`ZIP.CODE`,BOROUGH) %>% 
  summarise(NUMBER_OF_ACCIDENTS=n())
names(accidents_zip_other) <- c("region", "BOROUGH","value") 
accidents_zip_other$region <- factor(accidents_zip_other$region)
accidents_zip_other <- accidents_zip_other[!duplicated(accidents_zip_other$region),]
zip_choropleth(accidents_zip_other,
               state_zoom = "new york",
               county_zoom = c(36005,36047,36061,36081,36085),
               title      = "Accidents by Zip Code in New York City",
               legend     = "Number of Accidents") + 
  theme(plot.title = element_text(hjust = 0.5, size=15)) + 
  viridis::scale_fill_viridis(name="# Accidents",discrete = TRUE) + coord_map()


#### Accidents in Manhattan by Zip codes
accidents_zip <- accidents %>% filter(!is.na(`ZIP.CODE`)) %>% filter(BOROUGH=="MANHATTAN") %>% group_by(`ZIP.CODE`) %>% summarise(NUMBER_OF_ACCIDENTS=n()) 
names(accidents_zip) <- c("region", "value")
accidents_zip$region <- factor(accidents_zip$region)
zip_choropleth(accidents_zip,
               state_zoom = "new york",
               county_zoom = 36061,
               title      = "Accidents by Zip Code in Manhattan",
               legend     = "Number of Accidents") +theme(plot.title = element_text(hjust = 0.5, size=15))+
  viridis::scale_fill_viridis(name="# Accidents",discrete = TRUE)+coord_map()

###Trend by Year
ggplot(data=accidents) +
  geom_bar(aes(x=YEAR,fill=BOROUGH), position='dodge') +
  labs(title='Collisions by Borough over Years',
       x=NULL, 
       y='Collisions') +
  scale_fill_brewer(palette='Set1', name=NULL)

###Accidents by the hour of the day
ggplot(data=accidents) +
  geom_freqpoly(aes(x=HOUR, color=BOROUGH), binwidth=1) +
  labs(title='Collisions by Hour of a Day',
       x='Hour of a Day',
       y='Collisions') + 
  scale_color_discrete(name=NULL, labels=Boro) +
  annotate("text", x = c(8, 16), y = c(2800, 25000), 
           label = c('8 AM', '4 PM'), size=5) +
  annotate("rect", xmin = 7, xmax = 17, ymin = 0, ymax = 30000,
           alpha = .05, fill='darkred')

########################
box_2019 <- accidents %>% 
  filter(YEAR %in% c('2019')) %>% 
  filter(BOROUGH != "") %>% 
  group_by(MONTH) %>% 
  summarize(monthly_total = n()) %>%  
  ggplot() + geom_histogram(aes(MONTH, y=monthly_total), stat="identity", color = 'blue', fill = 'lightblue', bins = 12) 
+ xlab("month") + ylab("monthly total") + ggtitle('2019') + 
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_x_continuous(breaks=seq(1,12,1), labels=c("1","2","3","4","5","6","7","8","9","10","11","12")) + 
  scale_y_continuous(limits=c(0,16000))


box_2018 <- accidents %>% 
  filter(YEAR %in% c('2018')) %>% 
  filter(BOROUGH != "") %>% group_by(MONTH) %>% 
  summarize(monthly_total = n()) %>% 
  ggplot() + geom_histogram(aes(MONTH, y=monthly_total), stat="identity", color = 'blue', fill = 'lightblue', bins = 12) + 
  xlab("month") + ylab("monthly total") + ggtitle('2018') + 
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_x_continuous(breaks=seq(1,12,1), labels=c("1","2","3","4","5","6","7","8","9","10","11","12")) + 
  scale_y_continuous(limits=c(0,16000))


box_2017 <- accidents %>% 
  filter(YEAR %in% c('2017')) %>% 
  filter(BOROUGH != "") %>% 
  group_by(MONTH) %>% 
  summarize(monthly_total = n()) %>% 
  ggplot() + geom_histogram(aes(MONTH, y=monthly_total), stat="identity", color = 'blue', fill = 'lightblue', bins = 12) + 
  xlab("month") + ylab("monthly total") + ggtitle('2017') + 
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_x_continuous(breaks=seq(1,12,1), labels=c("1","2","3","4","5","6","7","8","9","10","11","12")) + 
  scale_y_continuous(limits=c(0,16000))


box_2016 <- accidents %>% 
  filter(YEAR %in% c('2016')) %>% 
  filter(BOROUGH != "") %>% 
  group_by(MONTH) %>% 
  summarize(monthly_total = n()) %>% 
  ggplot() + geom_histogram(aes(MONTH, y=monthly_total), stat="identity", color = 'blue', fill = 'lightblue', bins = 12) + 
  xlab("month") + ylab("monthly total") + ggtitle('2016') + 
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_x_continuous(breaks=seq(1,12,1), labels=c("1","2","3","4","5","6","7","8","9","10","11","12")) + 
  scale_y_continuous(limits=c(0,16000))


box_2015 <- accidents %>% 
  filter(YEAR %in% c('2015')) %>% 
  filter(BOROUGH != "") %>% 
  group_by(MONTH) %>% 
  summarize(monthly_total = n()) %>% 
  ggplot() + geom_histogram(aes(MONTH, y=monthly_total), stat="identity", color = 'blue', fill = 'lightblue', bins = 12) + 
  xlab("month") + ylab("monthly total") + ggtitle('2015') + 
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_x_continuous(breaks=seq(1,12,1), labels=c("1","2","3","4","5","6","7","8","9","10","11","12")) + 
  scale_y_continuous(limits=c(0,16000))


box_2014 <- accidents %>% 
  filter(YEAR %in% c('2014')) %>% 
  filter(BOROUGH != "") %>% 
  group_by(MONTH) %>% 
  summarize(monthly_total = n()) %>% 
  ggplot() + geom_histogram(aes(MONTH, y=monthly_total), stat="identity", color = 'blue', fill = 'lightblue', bins = 12) + 
  xlab("month") + ylab("monthly total") + ggtitle('2014') + 
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_x_continuous(breaks=seq(1,12,1), labels=c("1","2","3","4","5","6","7","8","9","10","11","12")) + 
  scale_y_continuous(limits=c(0,16000))


box_2013 <- accidents %>% 
  filter(YEAR %in% c('2013')) %>% 
  filter(BOROUGH != "") %>% 
  group_by(MONTH) %>% 
  summarize(monthly_total = n()) %>% 
  ggplot() + geom_histogram(aes(MONTH, y=monthly_total), stat="identity", color = 'blue', fill = 'lightblue', bins = 12) + 
  xlab("month") + ylab("monthly total") + ggtitle('2013') + 
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_x_continuous(breaks=seq(1,12,1), labels=c("1","2","3","4","5","6","7","8","9","10","11","12")) + 
  scale_y_continuous(limits=c(0,16000))


figure_monthly_grid <- ggarrange(box_2013, box_2014, box_2015, box_2016, box_2017, box_2018, box_2019, ncol=4, nrow=2)

annotate_figure(figure_monthly_grid,
                top = text_grob("Monthly Motor vehicle collisions", color = "blue", size = 14))

### Pedestrain fatality spread in 2019
accidents %>% filter(YEAR %in% c('2019') ) %>% 
  filter(NUMBER.OF.PERSONS.KILLED > 0) %>% 
  filter(LATITUDE != "" ) %>% filter(LONGITUDE != "") %>% 
  ggplot() + geom_point(aes(x=LATITUDE, y=LONGITUDE, color=NUMBER.OF.PEDESTRIANS.KILLED)) + 
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) + 
  ggtitle('Pedestrians killed in fatal motor vehicle collisions in 2019') + 
  theme(plot.title = element_text(hjust=0.5))

### Pedestrain fatalities over the year
accidents %>% filter(YEAR %in% c('2013','2014','2015','2016','2017','2018','2019') ) %>% 
  filter(NUMBER.OF.PEDESTRIANS.KILLED > 0) %>% group_by(YEAR) %>% summarize(count=n()) %>% 
  ggplot() + geom_histogram(aes(YEAR, y=count), bins=7, binwidth = 1, color = 'blue', fill = 'lightblue', stat="identity") + 
  ggtitle('Number of pedestrian fatalities') + theme(plot.title = element_text(hjust=0.5)) + xlab('YEAR')

