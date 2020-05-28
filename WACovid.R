#Author: Christopher Willett
#Date of Completion: May 27, 2020
#Purpose: To explore some data relating to the Covid-19 outbreak in Washington state.
#Notes: This is my first addition to github. The script produces three basic plots.


#Some packages needed.
library(readxl)
library(tidyverse)
library(ggthemes)

#I can't make the download work. The file is always corrupted this way.
#src <- "https://www.doh.wa.gov/Portals/1/Documents/1600/coronavirus/data-tables/PUBLIC_CDC_Event_Date_SARS.xlsx?ver=20200527000952"
#lc <- basename(src)
#download.file(url = src, destfile=lc)

#Load the data from WA DOH. Needs to be in Excel and current.
cases <- read_excel("WaDOH.xlsx",sheet=1)
hospitalizations <- read_excel("WaDOH.xlsx",sheet=2)
deaths <- read_excel("WaDOH.xlsx",sheet=3)

#Pull 2020 estimated populations. Forgot the source file.
counties <- read_excel("WACounty.xlsx")

#Create infection rates (per 100000) using populations.
cases <- mutate(cases,all_rate=NewPos_All*100000/counties$Pop[match(County,counties$CTYNAME)])

#Look only at the three main Puget Sound Counties.
psdat <- cases %>% filter(County=="Pierce County" | County == "King County" | County =="Snohomish County")

#Plot the infection rate as a time series across these counties.
psdat %>% ggplot(aes(x=WeekStartDate,y=all_rate))+
  geom_bar(stat="identity")+facet_grid(County~.)+
  theme_few()+theme(axis.text.x=element_text(angle=90))+
  xlab("Week Starting")+ylab("Infections Per 100,000 Residents")+
  ggtitle("Time Series of Infection Rates Across Puget Sound Counties")


#Find the most dangerous counties in WA. Not all counties report a death.
#The deaths list also include an Unassigned county, which we don't want to use.
tdeaths <- group_by(deaths,County) %>% summarise(total_deaths =sum(Deaths)) 
reporting_counties <- filter(counties, CTYNAME %in% intersect(tdeaths$County,counties$CTYNAME))
tdeaths <- filter(tdeaths,County %in% reporting_counties$CTYNAME)
tdeaths <- mutate(tdeaths,death_rate = tdeaths$total_deaths/(reporting_counties$Pop)*100000)

#Plot the death rate for all reporting counties as a horizontal bar graph.
tdeaths %>% ggplot(aes(x=reorder(County,-death_rate),y=death_rate))+
  geom_bar(stat="identity")+theme_few()+coord_flip()+xlab("Deaths per 100,000 Residents")+
  ylab("County")+ggtitle("Death Rates Washington From Covid-19 by County")


#Examine the ratio of deaths to hospitilizations across counties. Trying to 
#shed light on how lethal the virus is. Assumption: If a person died of the virus
#they probably were hospitilized. If the ratio is close to 0, then most people
#who were hospitilized did not die. If it is close to 1, then most people who 
#were hospitilized did die. See assumption.

#Wrangle the data. Make sure the counties in the hospitalizations and deaths match.
h <- group_by(hospitalizations,County) %>% summarise(total_hosp = sum(Hospitalizations)) %>%
  filter(County %in% tdeaths$County) %>% mutate(dh_ratio=tdeaths$total_deaths/total_hosp)

h %>% ggplot(aes(x=reorder(County,-dh_ratio),y=dh_ratio))+geom_bar(stat="identity")+
  theme_few()+coord_flip()+ylab("Death to Hospitalization Ratio")+
  xlab("County")+ggtitle("Deaths per Hospitalization Across Washington Counties")

#Is the d/h ratio normally distributed? Asotin County is a clear outlier, 
#so let's clean that off and plot a historgram. :

filter(h,County!="Asotin County") %>% ggplot(aes(dh_ratio))+geom_histogram(binwidth=0.1)+
  xlab("Death to Hospitalization Ratio")+ggtitle("Histogram of D/H Ratio Across WA Counties")+
  theme_few()

#This is approximately normal, but with Whatcom County a very clear outlier as well.
#So, let's create a qqplot to see.

filter(h,County!="Asotin County") %>% ggplot(aes(sample=dh_ratio))+stat_qq()+
  stat_qq_line()+ggtitle("A Quantile-Quantile Plot for d/h Ratio Across WA Counties")+
  theme_few()

#This is approximately normal, with Whatcom County a major outlier. 
#Let's see how far out.

#Let's pull out Asotin County and calculate z-scores.
h <- filter(h,County!="Asotin County")
avg <- mean(h$dh_ratio)
s <- sd(h$dh_ratio)
h <- mutate(h,z_score = (dh_ratio-avg)/s)

#And display the z-scores
h %>% ggplot(aes(x=reorder(County,-abs(z_score)),y=z_score))+geom_bar(stat="identity")+
  coord_flip()+ylab("Z score")+xlab("County")+
  ggtitle("Z-scores of d/h Ratio Across WA Counties")+theme_few()
