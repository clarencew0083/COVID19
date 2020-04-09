#Loads in all necessary packages for the shiny app
library(plyr)
library(dplyr)
library(ggplot2)
#library(tidyverse)
library(tidyr)
library(shinydashboard)
library(shiny)
library(geosphere)
library(scales)
library(googleVis)
#library(reshape2)
library(usmap)
library(data.table)
library(plotly)

###################################################################################################################################################
#Define Variables and load in data up front if necessary.
#This data updates daily with CovidConfirmedCases and CovidDeaths. The CDC updates these numbers every day.
#The static data (countyinfo, hospitalinfo, AFBaseLocations) is used to for lat and long coordinates to measure distance.
#Hospital Data allows us to determine the bed capacity of all hospitals in the nation
#AFBaseLocations provide names and coordinates of base.
#CountyInfo is used to measure population of a county and coordinates.

CovidConfirmedCases <- data.table::fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")
AFBaseLocations <- as.data.frame(data.table::fread("data/baseinfo.rda"))
CountyInfo <- as.data.frame(data.table::fread("data/countyinfo.rda"))
HospitalInfo <- as.data.frame(data.table::fread("data/hospitalinfo.rda"))
CovidDeaths<-as.data.frame(data.table::fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"))
himd = as.data.frame(data.table::fread("data/himd.rda"))
cimd = as.data.frame(data.table::fread("data/cimd.rda"))
colnames(CovidConfirmedCases)[1]<-"CountyFIPS"
colnames(CovidDeaths)[1]<-"CountyFIPS"
#Minus<-ncol(CovidDeaths)
#CovidDeaths<-CovidDeaths[,-(Minus)]
HospitalInfo$BEDS <- ifelse(HospitalInfo$BEDS < 0, 0, HospitalInfo$BEDS)

# #Create list of hospitals, bases, and counties to reference in the shiny app input
BaseList<-sort(AFBaseLocations$Base, decreasing = FALSE)


#Calculate counties includes the base and radius the user chooses in the UI on the app. Included Cont
CalculateCounties<-function(ChosenBase, Radius, IncludedCounties){
    #Finds which counties in given radius. Also Give county statistics
    TotalPopulation <-  sum(IncludedCounties$Population)
    TotalPopulation
}

# Finds Covid Cases and statistics on covid per county
CalculateCovid<-function(ChosenBase, Radius, IncludedCounties){
    #Finds which counties in given radius. Also Give county statistics
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    sum(CovidCounties[,ncol(CovidCounties)])
}

CalculateDeaths<-function(ChosenBase, Radius, IncludedCounties){
    #Finds which counties in given radius. Also Give county statistics
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    sum(CovidCountiesDeath[,ncol(CovidCountiesDeath)])
}

HospitalIncreases<-function(ChosenBase, Radius, IncludedCounties, IncludedHospitals){
    #Finds number of hospitals in radius
    TotalBeds<-sum(IncludedHospitals$BEDS)
    
    #Finds which counties in given radius. Also Give county statistics
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    n<-ncol(CovidCounties)-6
    TotalHospital<-sum(CovidCounties[,ncol(CovidCounties)])
    NotHospital<-sum(CovidCounties[,n])
    StillHospital<-ceiling((TotalHospital-NotHospital))
    Upper<-(signif((StillHospital/TotalBeds*.314+.6)*100,3))
    Lower<-(signif((StillHospital/TotalBeds*.207+.6)*100,3))
    if(Upper-Lower >= 1){
        
        Lower = round(Lower)
        Upper = round(Upper)
        paste(paste(Lower, Upper, sep = "-"),"%")
        
    } else {
        
        paste(mean(Upper,Lower),"%")  
        
    }
}

#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesPerDayChart<-function(ChosenBase, Radius, IncludedCounties, IncludedHospitals){
    #Finds number of hospitals in radius
    TotalBeds<-sum(IncludedHospitals$BEDS)
    
    #Finds which counties in given radius. Also Give county statistics
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    VectDailyCovid<-colSums(CovidCounties[29:length(CovidCounties)])
    DailyNewCases<-VectDailyCovid[2:length(VectDailyCovid)]-VectDailyCovid[1:(length(VectDailyCovid)-1)]
    DailyNewCases
    DailyNewHospitalizations<-ceiling(DailyNewCases*.26)
    DailyNewHospitalizations
    
    
    #Find New Deaths
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    VectDailyDeaths<-colSums(CovidCountiesDeath[29:ncol(CovidCountiesDeath)])
    DailyNewDeaths<-VectDailyDeaths[2:length(VectDailyDeaths)]-VectDailyDeaths[1:(length(VectDailyDeaths)-1)]
    
    
    ForecastDate<- seq(as.Date("2020-02-15"), length=(length(DailyNewDeaths)), by="1 day")
    Chart1Data<-cbind.data.frame(ForecastDate,DailyNewCases,DailyNewHospitalizations,DailyNewDeaths)
    colnames(Chart1Data)<-c("ForecastDate","New Cases","New Hospitalizations","New Deaths")
    Chart1DataSub <- melt(data.table(Chart1Data), id=c("ForecastDate"))
    
    #Plot the forecasts from above but include the actual values from the test data to compare accuracy.
    #plot for local area daily
    ggplot(Chart1DataSub) + geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 1) +
        scale_colour_manual(values=c("Blue", "Orange", "Red"))+
        xlab('Date') +
        ylab('Number of People') +
        theme(text = element_text(size = 15)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(color='')+
        theme_bw()+
        theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
        ) +
        theme(axis.line = element_line(color = "black"))+
        theme(legend.position = "top")
}

#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesCumChart<-function(ChosenBase, Radius, IncludedCounties, IncludedHospitals){
    #Finds number of hospitals in radius
    TotalBeds<-sum(IncludedHospitals$BEDS)
    
    #Finds which counties in given radius. Also Give county statistics
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CumDailyCovid<-colSums(CovidCounties[29:length(CovidCounties)])
    CumHospitalizations<-ceiling(CumDailyCovid*.26)
    
    
    
    #Find New Deaths
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CumDailyDeaths<-colSums(CovidCountiesDeath[29:ncol(CovidCountiesDeath)])
    
    
    
    ForecastDate<- seq(as.Date("2020-02-15"), length=(length(CumDailyDeaths)), by="1 day")
    Chart2Data<-cbind.data.frame(ForecastDate,CumDailyCovid,CumHospitalizations,CumDailyDeaths)
    colnames(Chart2Data)<-c("ForecastDate","Total Cases","Total Hospitalizations","Total Deaths")
    Chart2DataSub <- melt(data.table(Chart2Data), id=c("ForecastDate"))
    
    #Plot the forecasts from above but include the actual values from the test data to compare accuracy.
    #plot for local area cumulative
    ggplot(Chart2DataSub) + geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 1) +
        scale_colour_manual(values=c("Blue", "Orange", "Red"))+
        xlab('Date') +
        ylab('Number of People') +
        theme(text = element_text(size = 15)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(color='')+
        theme_bw()+
        theme(
            plot.background = element_blank()
            ,panel.grid.major = element_blank()
            ,panel.grid.minor = element_blank()
            ,panel.border = element_blank()
        ) +
        theme(axis.line = element_line(color = "black"))+
        theme(legend.position = "top")
}


# CovidCountyChoropleth<-function(ChosenBase, Radius){
# 
#     #Finds which counties in given radius. Also Give county statistics
#     BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
#     for (i in 1:3143) {
#         CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
#     }
#     IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= Radius)
#     CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
#     
#     
# 
#     
# 
#     
#     
#     
#     
# }
