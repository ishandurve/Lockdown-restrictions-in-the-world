#Installing Libraries
install.packages("tidyverse")
install.packages("gridExtra")  
install.packages("ggplot2")
install.packages("lubridate")
library(tidyverse)
library(rgl)                  #3D graph
library(lubridate)            #Using date format ymd,etc
library(dplyr)
library(ggplot2)
library(gridExtra)            #using grid to plot 2 graphs side by side
library(gganimate)            #animation for graph
library(av)                   #handling frames
library(gifski)               #gif encoding
library(ggthemes)             #Provides themes for ggplot
library(plotly)               #Hovering causes detailed view-used in line graph
library(data.table)           #For the use of %between% function        

#Importing Dataset
relief<-read.csv("C:/Users/ishan/OneDrive/Desktop/Introduction to Data Science/Income Support.csv",header=TRUE)
covid_full<-read.csv("C:/Users/ishan/OneDrive/Desktop/Introduction to Data Science/covid-data.csv",header=TRUE)


#Converting Date to appropriate format 
DateC<- data.frame(dates=relief$Date)
newDate<-ymd(DateC$dates)

#reading only the required columns from the whole data set of Income Support, 
#this includes-
#Name of the Country,Date
#Workplace Restrictions, travel restrictions, Stay at Home requirements,
#Income Support and Debt Relief

indices<-data.frame(relief$CountryName, newDate,relief$Jurisdiction, 
                    relief$C2_Workplace.closing,
                    relief$C5_Close.public.transport, 
                    relief$C6_Stay.at.home.requirements,
                    relief$C7_Restrictions.on.internal.movement,
                    relief$E1_Income.support, relief$E2_Debt.contract.relief)

View(indices)

#Renaming columns for relief dataset
indices <- indices %>%
  rename(
    location = relief.CountryName,
    Jurisdiction = relief.Jurisdiction,
    Workplace_closing = relief.C2_Workplace.closing,
    Public_transport = relief.C5_Close.public.transport,
    Stay_at_home = relief.C6_Stay.at.home.requirements,
    Internal_movement = relief.C7_Restrictions.on.internal.movement,
    Income_support =  relief.E1_Income.support,
    Debt_relief = relief.E2_Debt.contract.relief
  )

#Removing State-wise jurisdictions
countrywise<-subset(indices, Jurisdiction!="STATE_TOTAL")
View(countrywise)

#As for this visualization, we are working only on the G-20 countries
g20 <- subset(countrywise, subset= location %in% c("United States",
                                                   "China","Japan","Germany",
                                                   "United Kingdom","France",
                                                   "India","Italy","Canada",
                                                   "South Korea","Russia",
                                                   "Brazil","Australia","Spain",
                                                   "Mexico","Argentina",
                                                  "Indonesia","Saudi Arabia",
                                                  "South Africa","Turkey"))
View(g20)

#Bar plot with Workplace Status of G20 countries
#Considering for  2020,2021,2022 for these countries
g20plot <- subset(g20,subset=(newDate=="2020-05-03")|(newDate=="2020-12-14")|
                    (newDate=="2021-11-07"))

#Forming data frame for barplot
groupedbar <- data.frame(country = rep(c("Argentina","Australia","Brazil",
                                       "Canada","China","Germany","Spain",
                                       "France","United Kingdom","Indonesia",
                                       "India","Italy","Japan","South Korea",
                                   "Mexico","Russia","Saudi Arabia","Turkey",
                                   "United States","South Africa"), each=3),
                dateOfData = rep(c("2020-05-03","2020-12-14","2021-11-07"),20),
                workplace = g20plot$Workplace_closing)

View(groupedbar)

#Using ggplot2 to plot the graph
p <- ggplot(groupedbar, aes(x=country, y=workplace, fill=dateOfData))+
  theme_solarized_2(light=FALSE)+
  geom_bar(stat="identity", position=position_dodge() )
p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, 
          color = 'white'),
          title=element_text(color = "#EEEEEE"),
          panel.background = element_rect(fill=NA),
          plot.background = element_rect(fill="#111111"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
          ggtitle("Workplace Restrictions in G20 Countries") + 
          labs(y="Workplace Restrictions", x = "G20 Country")+
          labs(fill = "Date")


#Visualization 2- With Covid Dataset
#Making Animated Plot with Number of Cases vs Home Stay restrictions on 3 dates-
#3 May 2020, 14 December 2020, 7 November 2021
#Need to combine 2 datasets here- 

#converting Date of the covid dataset to an appropriate format using lubridate
Dateinymd <- data.frame(dates=covid_full$date)
newDateCovid <- ymd(Dateinymd$dates)
View(covid_full)

#Changing column names and extracting data for the required dates
names(countrywise)[names(countrywise) == 'newDate'] <- 'date'
countrywise_date <- subset(countrywise, subset = (date=="2020-05-03")|
                           (date=="2020-12-14")|(date=="2021-11-07"))
View(countrywise_date)

#Extracting number of cases and other important columns from covid dataset
gdp_cases_vaccination <- covid_full[c('location','date',
                                      'total_cases','new_cases'
                                      )]

#Selecting the data for the 3 dates
gdp_cases_vaccination_date <- subset(gdp_cases_vaccination,
                                     subset = (date == "2020-05-03")|
                                     (date=="2020-12-14")|
                                     (date=="2021-11-07"))

#Converting char(date) to a date object
gdp_cases_vaccination_date$date = as.Date(gdp_cases_vaccination_date$date)
View(gdp_cases_vaccination_date)

#Now we have to join data frames- countrywise and gdp_cases_vaccination_date
allcountry <- full_join(countrywise_date,gdp_cases_vaccination_date,
                        by=c("location"="location","date"="date"))

#Selecting only NAT_TOTAL i.e. only the countries and not states
allcountry <- subset( allcountry, Jurisdiction == "NAT_TOTAL")
View(allcountry)

#Plotting the Animated Scatter plot 
options(scipen=99999)                   #Removing Scientific Notation
anim <- ggplot( allcountry, aes( x = Stay_at_home,
                               y = log(total_cases,31),
                             size = new_cases, color = location)) +
  geom_point(show.legend=F)+
  labs( x="Stay at Home Level", y = "total Number of Cases")+
  ggtitle("Number of Cases VS Lockdown Restrictions")+
  theme( axis.title.x = element_text( vjust = 0, size = 13, face = "bold"),
         axis.title.y = element_text( vjust = 2, size = 13, face= "bold"),
         plot.subtitle = element_text( size = 13, hjust = 0.5 ),
         plot.title = element_text( face = "bold", hjust = 0.5,
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))+
  scale_size(range=c(2,15))

#Transition elements
anim + transition_time(allcountry$date) + labs(subtitle = 'Date: {frame_time}')


#Visualization3 - Jitter plot of Workplace Restrictions VS Income Support
#Here we compare two dates - 15 March 2020 and 15 May 2021

jitterplot1 <- subset(countrywise, subset=(date=="2020-03-15")|
                        (date=="2021-05-15"))
View(jitterplot1)

ggplot(jitterplot1, aes(x = Workplace_closing, y = Income_support, 
                        color=location)) +
  geom_point( size = 2) + geom_jitter( width = 0.1) + ylim(0, 3)+
  theme(legend.position = "none",
        panel.background = element_rect( fill = "#111111"),
        panel.grid.minor  = element_blank(),
        axis.text = element_text( size = 12),
        axis.title.x = element_text( vjust = 0, size = 13),
        axis.title.y = element_text( vjust = 2, size = 13),
        plot.title = element_text(face = "bold",hjust=0.5,
                                  margin = margin(10, 0, 10, 0),
                                  size = 14)
        ) + facet_wrap(~date) +
  labs( x= "Workplace Closed-Levels", y = "Income Support Levels", 
        title = "15 March 2020 VS 15 May 2021")


#Visualization 4- Working on Unemployment Related Data
#First we read the csv file containing data for G7 countries
unemployment <- read.csv("C:/Users/ishan/OneDrive/Desktop/Introduction to Data Science/unemployment.csv",header=TRUE,fileEncoding="UTF-8-BOM")
View(unemployment)

unemployment$TIME <- dmy(unemployment$TIME)

plotall <- unemployment %>%
  ggplot( aes(x = TIME, y = Value, color = LOCATION)) +
  geom_path() + 
  ggtitle("Unemployment Rate over the years 2020 and 2021 in G7 countries") + 
  theme(plot.background = element_rect(fill="#111111"),
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "white"),
        plot.title = element_text(colour = "white"))
  
plotall <- ggplotly(plotall)          #plotly shows additional data on hovering
plotall


#Visualization 5 - We see the most movement for United States and Canada
#We will correlate this movement for unemployment w.r.t Workplace restrictions
#March 2020 to August 2021

#Dividing g20 dataset to get rows for Canada for the required dates
canada <- subset(g20, subset = (location == "Canada"))
canada$newDate <- as.Date(canada$newDate, format = "%Y-%m-%d") #Date format

canada_date <- subset(canada, subset = (
  newDate %between% c("2020-03-01", "2021-08-01")))
View(canada_date)

canada_left <- ggplot(canada_date, aes(newDate, Workplace_closing)) + 
  geom_line(color = "chartreuse", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Workplace restrictions in Canada") + 
  labs(y="Workplace Restrictions", x = "Date")


canada_unemp <- subset(unemployment, subset = (LOCATION == "Canada"))
canada_right <- ggplot(canada_unemp, aes(x = TIME, y = Value)) + 
  geom_line(color = "chartreuse", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Unemployment Rate in Canada") + 
  labs(y="Rate", x = "Date")

grid.arrange(canada_left, canada_right, ncol = 2)


#Dividing g20 dataset to get rows for United States for the required dates
us <- subset(g20, subset = (location == "United States"))
us$newDate <- as.Date(us$newDate, format = "%Y-%m-%d") #Date format

us_date <- subset(us, subset = (
  newDate %between% c("2020-03-01", "2021-08-01")))
View(us_date)

us_left <- ggplot(us_date, aes(newDate, Workplace_closing)) + 
  geom_line(color = "skyblue", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Workplace restrictions in United States") + 
  labs(y="Workplace Restrictions", x = "Date")


us_unemp <- subset(unemployment, subset = (LOCATION == "United States"))
us_right <- ggplot(us_unemp, aes(x = TIME, y = Value)) + 
  geom_line(color = "skyblue", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Unemployment Rate in The United States") + 
  labs(y="Rate", x = "Date")

grid.arrange(us_left, us_right, ncol = 2)



#Dividing g20 dataset to get rows for Italy for the required dates
italy <- subset(g20, subset = (location == "Italy"))
italy$newDate <- as.Date(italy$newDate, format = "%Y-%m-%d") #Date format

italy_date <- subset(italy, subset = (
  newDate %between% c("2020-03-01", "2021-08-01")))
View(italy_date)

italy_left <- ggplot(italy_date, aes(newDate, Workplace_closing)) + 
  geom_line(color = "yellow", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Workplace restrictions in Italy") + 
  labs(y="Workplace Restrictions", x = "Date")


italy_unemp <- subset(unemployment, subset = (LOCATION == "Italy"))
italy_right <- ggplot(italy_unemp, aes(x = TIME, y = Value)) + 
  geom_line(color = "yellow", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Unemployment Rate in Italy") + 
  labs(y="Rate", x = "Date")

grid.arrange(italy_left, italy_right, ncol = 2)



#Dividing g20 dataset to get rows for France for the required dates
france <- subset(g20, subset = (location == "France"))
france$newDate <- as.Date(france$newDate, format = "%Y-%m-%d") #Date format

france_date <- subset(france, subset = (
  newDate %between% c("2020-03-01", "2021-08-01")))
View(france_date)

france_left <- ggplot(france_date, aes(newDate, Workplace_closing)) + 
  geom_line(color = "plum1", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Workplace restrictions in France") + 
  labs(y="Workplace Restrictions", x = "Date")


france_unemp <- subset(unemployment, subset = (LOCATION == "France"))
france_right <- ggplot(france_unemp, aes(x = TIME, y = Value)) + 
  geom_line(color = "plum1", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Unemployment Rate in France") + 
  labs(y="Rate", x = "Date")

grid.arrange(france_left, france_right, ncol = 2)




#Dividing g20 dataset to get rows for Japan for the required dates
japan <- subset(g20, subset = (location == "Japan"))
japan$newDate <- as.Date(japan$newDate, format = "%Y-%m-%d") #Date format

japan_date <- subset(japan, subset = (
  newDate %between% c("2020-03-01", "2021-08-01")))
View(japan_date)

japan_left <- ggplot(japan_date, aes(newDate, Workplace_closing)) + 
  geom_line(color = "cyan", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Workplace restrictions in Japan") + 
  labs(y="Workplace Restrictions", x = "Date")


japan_unemp <- subset(unemployment, subset = (LOCATION == "Japan"))
japan_right <- ggplot(japan_unemp, aes(x = TIME, y = Value)) + 
  geom_line(color = "cyan", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Unemployment Rate in Japan") + 
  labs(y="Rate", x = "Date")

grid.arrange(japan_left, japan_right, ncol = 2)




#Dividing g20 dataset to get rows for Germany for the required dates
germany <- subset(g20, subset = (location == "Germany"))
germany$newDate <- as.Date(germany$newDate, format = "%Y-%m-%d") #Date format

germany_date <- subset(germany, subset = (
  newDate %between% c("2020-03-01", "2021-08-01")))
View(germany_date)

germany_left <- ggplot(germany_date, aes(newDate, Workplace_closing)) + 
  geom_line(color = "deeppink", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Workplace restrictions in Germany") + 
  labs(y="Workplace Restrictions", x = "Date")


germany_unemp <- subset(unemployment, subset = (LOCATION == "Germany"))
germany_right <- ggplot(germany_unemp, aes(x = TIME, y = Value)) + 
  geom_line(color = "deeppink", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Unemployment Rate in Germany") + 
  labs(y="Rate", x = "Date")

grid.arrange(germany_left, germany_right, ncol = 2)



#Dividing g20 dataset to get rows for United Kingdom for the required dates
uk <- subset(g20, subset = (location == "United Kingdom"))
uk$newDate <- as.Date(uk$newDate, format = "%Y-%m-%d") #Date format

uk_date <- subset(uk, subset = (
  newDate %between% c("2020-03-01", "2021-08-01")))
View(uk_date)

uk_left <- ggplot(uk_date, aes(newDate, Workplace_closing)) + 
  geom_line(color = "thistle1", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Workplace restrictions in United Kingdom") + 
  labs(y="Workplace Restrictions", x = "Date")


uk_unemp <- subset(unemployment, subset = (LOCATION == "United Kingdom"))
uk_right <- ggplot(uk_unemp, aes(x = TIME, y = Value)) + 
  geom_line(color = "thistle1", size = 1.2) +
  theme(axis.text.x = element_text(vjust = 1, hjust=1, 
                                   color = 'white'),
        title=element_text(color = "#EEEEEE"),
        panel.background = element_rect(fill=NA),
        plot.background = element_rect(fill="#111111"),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "white"),
        axis.text.y = element_text(colour = "white"),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Unemployment Rate in United Kingdom") + 
  labs(y="Rate", x = "Date")

grid.arrange(uk_left, uk_right, ncol = 2)



# Visualisation 6 -3D graph
#Here we compare the 3 factors- Stay at home Restrictions VS
                                #Income Support VS
                                #Debt/Contract Relief

#For G7 countries
g73ddec <- subset(g20, subset = (newDate == "2021-12-01"))
                
View(g73ddec)

plot3d(
  x = g73ddec$Public_transport, y = g73ddec$Income_support, 
  z = g73ddec$Debt_relief,
  type = 's', 
  radius = .05,
  xlab = "Transport Restrictions", ylab = "Income Support", 
  zlab = "Debt and Contract Relief")


bgplot3d({
  plot.new()
  title(main = "Restrictions in G7 Countries- Latest", line = 2)
})


