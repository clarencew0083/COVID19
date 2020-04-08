##################
##### Global #####
##################

# Layout
##############################################################################################################################################
# The global introduces all libraries, functions, datasets, and formatting that is necessary to pass through the server.
# First:  The libraries are loaded which have built in functions are used throughout the app.
# Second: We load data from https://github.com/treypujats/COVID19/tree/master/covid19/data ,  usafacts.org , and covid19.healthdata.org
#         The github data has static information on on all air force bases, US counties, and US hospitals. 
#         The usafacts data has dynamic information that is updated daily reporting the number of cases and number of deaths daily.
#         The IHME provides projection data of the pandemic
#         After loading the data, we format headers, establish data tables for printing, and do any static changes to the dataset for the app.
# Third:  Functions are used to execute the tasks in the server. Functions in the global are not dynamic, but they take in dynamic inputs
#         Global functions are used to calculate statistics, make data tables, plot graphs, and create visuals.
##############################################################################################################################################       



# Step One
###################################################################################################################################################
#Loads in all necessary packages for the shiny app





library(markdown)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(shiny)
library(geosphere)
library(scales)
library(googleVis)
library(usmap)
library(data.table)
library(plyr)
library(DT)
library(mapproj)
library(viridis)
#library(tidyverse)
library(zoo) #used for rollsum function 
library(rmarkdown)
library(rvest)
library(maps)
library(plotly)




# Step Two
###################################################################################################################################################
#Define Variables and load in data up front if necessary.
#This data updates daily with CovidConfirmedCases and CovidDeaths. The CDC updates these numbers every day.
#The static data (countyinfo, hospitalinfo, AFBaseLocations) is used to for lat and long coordinates to measure distance.
#Hospital Data allows us to determine the bed capacity of all hospitals in the nation
#AFBaseLocations provide names and coordinates of base.
#CountyInfo is used to measure population of a county and coordinates.

CovidConfirmedCases <- as.data.frame(data.table::fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"))
AFBaseLocations <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/raw/master/covid19/data/baseinfo.rda"))
CountyInfo <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/raw/master/covid19/data/countyinfo.rda"))
HospitalInfo <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/blob/master/covid19/data/hospitalinfo.rda?raw=true"))
CovidDeaths<-as.data.frame(data.table::fread("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"))
himd <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/blob/master/covid19/data/himd.rda?raw=true"))
cimd <- as.data.frame(data.table::fread("https://github.com/treypujats/COVID19/blob/master/covid19/data/cimd.rda?raw=true"))
#AFBaseLocations[154,]<-c("Pentagon", "DC", "HQ", "Something", "AF", 38.8719, -77.0563 )

#Updating data frames to ensure they are filled and match the data we reference later in the scripts
colnames(CovidConfirmedCases)[1]<-"CountyFIPS"
colnames(CovidDeaths)[1]<-"CountyFIPS"
HospitalInfo$BEDS <- ifelse(HospitalInfo$BEDS < 0, 0, HospitalInfo$BEDS)
CovidConfirmedCases[is.na(CovidConfirmedCases)]<-0
CovidDeaths[is.na(CovidDeaths)]<-0
colnames(CovidConfirmedCases)[1]<-"CountyFIPS"


#Read in IHME data for projecting data in the future
temp <- tempfile()
download.file("https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip", temp, mode="wb")
zipdf <- unzip(temp, list = TRUE)
csv_file <- zipdf$Name[2]
IHME_Model <- read.table(unz(temp, csv_file), header = T, sep = ",")
unlink(temp)
IHME_Model$date <- as.Date(IHME_Model$date, format = "%Y-%m-%d")
StateList <- data.frame(state.name, state.abb)
IHME_Model <- merge(IHME_Model, StateList, by.x = names(IHME_Model)[2], by.y = names(StateList)[1])
names(IHME_Model)[names(IHME_Model)=="state.abb"] <- "State"


#Create list of hospitals, bases, and counties.
BaseList<-sort(AFBaseLocations$Base, decreasing = FALSE)
HospitalList <- HospitalInfo$NAME
CountyList <- CountyInfo$County


#Calculate county case doubling rate for most recent day
CovidConfirmedCases <- dplyr::filter(CovidConfirmedCases, CountyFIPS != 0)
CovidConfirmedCases <- head(CovidConfirmedCases,-1)

currCount = 0

v <- rep(0, as.numeric(ncol(CovidConfirmedCases)))

for (i in 1:nrow(CovidConfirmedCases)){
    
    j = 0
    cases = CovidConfirmedCases[i,ncol(CovidConfirmedCases)]
    
    if (cases != 0){
        
        while (cases/2 < CovidConfirmedCases[i,ncol(CovidConfirmedCases)-j])
        {
            j = j + 1
        }
        
        days = j
    } 
    else{
        days = 0
    }
    
    v[i] <- days
    
}

CovidConfirmedCasesRate <- cbind(CovidConfirmedCases,v)


######################Data Specific to plotting counties and states as choropleth
#Input the Included Counties as factors
PlottingCountyData<- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv",
                              header = TRUE, stringsAsFactors = FALSE)
PlottingCountyData$county <- tolower(gsub("([A-Za-z]+).*", "\\1", PlottingCountyData$County.Name))
PlottingCountyData$county <- gsub("^(.*) parish, ..$","\\1", PlottingCountyData$county)
#Creating state name in addition to state abb
PlottingCountyData<-PlottingCountyData %>% 
    mutate(state_name = tolower(state.name[match(State, state.abb)]))
#Calling in county data to merge and match, that way we have the correct coordinates when creating the map.
county_df <- map_data("county")
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
county_df$state_name <- NULL
#Calling in state data so we can map it correctly
state_df <- map_data("state", projection = "albers", parameters = c(39, 45))
colnames(county_df)[6]<-"State"


#Create National Data table on summary page
NationalDataTable<-CovidConfirmedCases
NationalDataTable$State<-as.factor(NationalDataTable$State)
NationalDataTable<-NationalDataTable[,-c(1,2,4)]
NationalDataTable<-aggregate(.~State, NationalDataTable, sum)
RateofCovidChange<-rev(NationalDataTable)[c(1:7)]
RateofCovidChange<-ceiling(rowSums(RateofCovidChange[1:6]-RateofCovidChange[2:7])/6)

NationalDeathTable<-CovidDeaths
NationalDeathTable$State<-as.factor(NationalDeathTable$State)
NationalDeathTable<-NationalDeathTable[,-c(1,2,4)]
NationalDeathTable<-aggregate(.~State, NationalDeathTable, sum)
RateofDeathChange<-rev(NationalDeathTable)[c(1:7)]
RateofDeathChange<-ceiling(rowSums(RateofDeathChange[1:6]-RateofDeathChange[2:7])/6)

NationalDataTable<-data.frame(NationalDataTable$State, NationalDataTable[,length(NationalDataTable)],RateofCovidChange, NationalDeathTable[,length(NationalDeathTable)], RateofDeathChange)
colnames(NationalDataTable)<-c("State","Total Cases","Average New Cases Per Day", "Total Deaths","Average New Deaths Per Day")
NationalDataTable$`Cases Per 100,000 People`<-c(731449,4822023,2949131,6553255,38041430,5187582,3590347,633427,917092,19317568,9919945,1392313,3074186,1595728,12875255,6537334,2885905,4380415,4601893,6646144,5884563,1329192,9883360,5379139,6021988,2984926,1005141,9752073,699628,1855525,1320718,8864590,2085538,2758931,19570261,11544225,3814820,3899353,12763536,1050292,4723723,833354,6456243,26059203,2855287,8185867,626011,6897012,5726398,1855413,576412)
NationalDataTable$`Cases Per 100,000 People`<-round(NationalDataTable$`Total Cases`/(NationalDataTable$`Cases Per 100,000 People`/100000))

# beds <- read.csv('beds.csv')
# pops <- read.csv('pops.csv')
# source('acme_support.R')



GetCounties<-function(base,radius){
    #BaseStats<-dplyr::filter(AFBaseLocations, Base == input$Base)
    
    CountyInfo$DistanceMiles = cimd[,as.character(base)]
    #for (i in 1:3143) {
    #    CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
    #}
    IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= radius)
    IncludedCounties
}

GetHospitals<-function(base,radius){
    #Finds number of hospitals in radius
    #BaseStats<-dplyr::filter(AFBaseLocations, Base == input$Base)
    
    HospitalInfo$DistanceMiles = himd[,as.character(base)]
    
    IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= radius))
    IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
    IncludedHospitals
}






#Step Three
###################################################################################################################################################

# Establish Local Counties ---------------------------------------------------------------------------------------------------------------------------------------------------------------

#These Functions establishes which counties are going to be included in the analysis determined by the base and radius.
CalculateCounties<-function(IncludedCounties){
    #Finds which counties in given radius. Also Give county statistics
    TotalPopulation <-  sum(IncludedCounties$Population)
    TotalPopulation
}


# Create Numerical Statistics for the dashboard -------------------------------------------------------------------------------------------------------------------------------------

# Finds Covid Cases and statistics on covid per county
CalculateCovid<-function(IncludedCounties){
    #Finds which counties in given radius. Also Give county statistics
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    sum(rev(CovidCounties)[,1])
}

CalculateDeaths<-function(IncludedCounties){
    #Finds which counties in given radius. Also Give county statistics
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    sum(CovidCountiesDeath[,ncol(CovidCountiesDeath)])
}

HospitalIncreases<-function(IncludedCounties, IncludedHospitals){
    #Finds number of hospitals in radius
    TotalBeds<-sum(IncludedHospitals$BEDS)
    #Finds which counties in given radius. Also Give county statistics
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
    TotalHospital<-sum(CovidCounties[,ncol(CovidCounties)])
    NotHospital<-sum(rev(CovidCounties)[,7])
    StillHospital<-ceiling((TotalHospital-NotHospital))
    Upper<- round(((StillHospital+changeC*.1)/TotalBeds+.5)*100,1)
    #Lower<- round(((StillHospital+changeC*.207)/TotalBeds+.55)*100,1)
    paste(Upper," %", sep = "") 
}

CalculateCHIMEPeak<-function(IncludedCounties, ChosenBase, ChosenRadius, SocialDistance, ProjectedDays){
    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases","Maximum Daily Cases")
    
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
    
    #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
    #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
    ActiveCases<-rev(CovidCounties)[1:7]
    ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
    colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
    SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
    colnames(SIRinputs)<-c("cases","pop","doubling")
    
    
    ####################################################################################
    #Mean Estimate
    
    #Next we use the calculated values, along with estimated values from the CDC. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-ProjectedDays
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                               ventilatortime,daysforecasted,Ro, .5)
    
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    DailyData<-DailyData[-1,]
    DailyData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
    
    Peak<-which.max(DailyData$`Expected Daily Cases`)
    Peak<-DailyData[Peak,2]
    round(Peak)
    
}

# CalculateCHIMEMinMax<-function(IncludedCounties, ChosenBase, ChosenRadius, SocialDistance, ProjectedDays){
#     BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
#     #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
#     #We include State, county, population in those counties, cases, fatalities, doubling rate
#     CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
#     HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
#     HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
#     HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
#     colnames(HistoricalData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases","Maximum Daily Cases")
#     
#     DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
#     CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
#     CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
#     CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
#     colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
#     
#     #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
#     #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
#     ActiveCases<-rev(CovidCounties)[1:7]
#     ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
#     colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
#     SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
#     colnames(SIRinputs)<-c("cases","pop","doubling")
#     
#     
#     ####################################################################################
#     #Mean Estimate
#     
#     #Next we use the calculated values, along with estimated values from the CDC. 
#     #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
#     cases<-SIRinputs$cases
#     pop<-SIRinputs$pop
#     doubling<-8
#     
#     #Established Variables at the start for every county or populations
#     Ro<-2.5
#     incubationtime<-5
#     latenttime<-2
#     recoverydays<-14
#     socialdistancing<-SocialDistance
#     hospitalizationrate<-5
#     icurate<-6
#     ventilatorrate<-3
#     hospitaltime<-3.5
#     icutime<-4
#     ventilatortime<-7
#     daysforecasted<-ProjectedDays
#     
#     
#     #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
#     #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
#     SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
#                                socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
#                                ventilatortime,daysforecasted,Ro, .5)
#     
#     MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
#     DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
#     TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
#     colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
#     colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
#     DailyData<-DailyData[-1,]
#     DailyData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
#     
#     Peak<-which.max(DailyData$`Expected Daily Cases`)
#     Max<-DailyData[Peak,4]
#     Min<-DailyData[Peak,3]
#     paste("(Min: ", Min, ", Max: ", Max, ")")
#     
# }

CalculateIHMEPeak<-function(ChosenBase, IncludedHospitals, radius){
    
    #Creating the stats and dataframes determined by the base we choose to look at.
    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
    TotalBedsCounty <- sum(IncludedHospitals$BEDS)
    
    #Get regional and state populations
    CountyInfo$DistanceMiles = cimd[,as.character(ChosenBase)]
    IncludedCounties <- dplyr::filter(CountyInfo, DistanceMiles <= radius)
    StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
    RegPop <- sum(IncludedCounties$Population)
    StPop <- sum(StPopList$Population)
    
    # Use Population ratio to scale IHME
    PopRatio <- RegPop/StPop
    
    # Get total hospital bed number across state
    IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
    TotalBedsState <- sum(IncludedHospitalsST$BEDS)
    
    # Calculate bed ratio
    BedProp <- TotalBedsCounty/TotalBedsState
    
    # Apply ratio's to IHME data
    IHME_Region <- IHME_State
    IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
    IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean)
    
    PeakDate<-which.max(IHME_Data$IHME_Region.allbed_mean)
    Peak<-IHME_Data[PeakDate,2]
    round(Peak)
}

# CalculateIHMEMinMax<-function(ChosenBase, IncludedHospitals, radius){
# 
#     #Creating the stats and dataframes determined by the base we choose to look at.
#     BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
#     IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
#     TotalBedsCounty <- sum(IncludedHospitals$BEDS)
# 
#     #Get regional and state populations
#     CountyInfo$DistanceMiles = cimd[,as.character(ChosenBase)]
#     IncludedCounties <- dplyr::filter(CountyInfo, DistanceMiles <= radius)
#     StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
#     RegPop <- sum(IncludedCounties$Population)
#     StPop <- sum(StPopList$Population)
# 
#     # Use Population ratio to scale IHME
#     PopRatio <- RegPop/StPop
# 
#     # Get total hospital bed number across state
#     IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
#     TotalBedsState <- sum(IncludedHospitalsST$BEDS)
# 
#     # Calculate bed ratio
#     BedProp <- TotalBedsCounty/TotalBedsState
# 
#     # Apply ratio's to IHME data
#     IHME_Region <- IHME_State
#     IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
#     IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean)
# 
#     PeakDate<-which.max(IHME_Data$IHME_Region.allbed_mean)
#     Min<-IHME_Data[PeakDate,3]
#     Max<-IHME_Data[PeakDate,4]
#     paste("Min: ", Min, ", Max: ", Max)
# }



# Create Charts for plotting lines showing trends among the virus  ------------------------------------------------------------------------------------------------------------------


#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesPerDayChart<-function(ChosenBase, Radius, IncludedCounties, IncludedHospitals){
    
    #Find counties in radius
    CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    
    #Recalculate datframe to have daily cases instead of cumulative
    n<-as.numeric(length(CovidCountiesCases))
    VectDailyCovid<-colSums(CovidCountiesCases[,29:n])
    DailyNewCases<-VectDailyCovid[2:length(VectDailyCovid)] -
        VectDailyCovid[1:(length(VectDailyCovid)-1)]
    
    #Estimation for new hospitalizations
    DailyNewHospitalizations<-ceiling(DailyNewCases*.21)
    
    #Find New Deaths
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    VectDailyDeaths<-colSums(CovidCountiesDeath[29:ncol(CovidCountiesDeath)])
    DailyNewDeaths<-VectDailyDeaths[2:length(VectDailyDeaths)] -
        VectDailyDeaths[1:(length(VectDailyDeaths)-1)]
    
    #Clean up the dataset to prepare for plotting
    ForecastDate<- seq(as.Date("2020-02-17"), length=(length(DailyNewDeaths)), by="1 day")
    Chart1Data<-cbind.data.frame(ForecastDate,DailyNewCases,DailyNewHospitalizations,DailyNewDeaths)
    colnames(Chart1Data)<-c("ForecastDate","New Cases","New Hospitalizations","New Fatalities")
    Chart1DataSub <- melt(data.table(Chart1Data), id=c("ForecastDate"))
    
    #Plot for local area daily cases, hospitalizations, and deaths
    p1 <- ggplot(Chart1DataSub) + 
        geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 0.5) +
        scale_colour_manual(values=c("Blue", "Orange", "Red")) +
        xlab('Date') +
        ylab('Number of People') +
        theme_bw() + 
        theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
              axis.title = element_text(face = "bold", size = 11, family = "sans"),
              axis.text.x = element_text(angle = 60, hjust = 1), 
              axis.line = element_line(color = "black"),
              legend.position = "top",
              plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
        scale_x_date(date_breaks = "1 week") +
        labs(color='')
    
    p1 <- ggplotly(p1)
    
    p1 <- p1 %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                      xanchor = "center",  # use center of legend as anchor
                                      x = 0.5,
                                      y = 1.2)) %>% config(displayModeBar = FALSE)
  
    p1
}



#Begin function to create chart of new cases for COVID-19 is a specified region around a specified base
CovidCasesCumChart<-function(ChosenBase, Radius, IncludedCounties, IncludedHospitals){
    
    #Find counties in radius
    CovidCountiesCases<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    CovidCountiesDeath<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    
    #Compute cumlative cases and deaths in selected counties
    CumDailyCovid<-colSums(CovidCountiesCases[,29:length(CovidCountiesCases)])
    CumDailyDeaths<-colSums(CovidCountiesDeath[29:ncol(CovidCountiesDeath)])
    
    #Estimation for total hospitalizations
    CumHospitalizations<-ceiling(CumDailyCovid*0.21)
    
    #Clean up the dataset to get ready to plot it
    ForecastDate<- seq(as.Date("2020-02-17"), length=(length(CumDailyDeaths)), by="1 day")
    Chart2Data<-cbind.data.frame(ForecastDate,CumDailyCovid,CumHospitalizations,CumDailyDeaths)
    colnames(Chart2Data)<-c("ForecastDate","Total Cases","Total Hospitalizations","Total Fatalities")
    Chart2DataSub <- melt(data.table(Chart2Data), id=c("ForecastDate"))
    
    #Plot for local area cumulative cases
    p2 <- ggplot(Chart2DataSub,height = 250) + 
        geom_line(aes(x=ForecastDate, y=value, colour = variable), size = 0.5) +
        scale_colour_manual(values=c("Blue", "Orange", "Red"))+
        xlab('Date') +
        ylab('Number of People') +
        theme_bw() + 
        theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
              axis.title = element_text(face = "bold", size = 11, family = "sans"),
              axis.text.x = element_text(angle = 60, hjust = 1), 
              axis.line = element_line(color = "black"),
              plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              legend.position = c(0, 1),) +
        scale_x_date(date_breaks = "1 week")

    p2 <- ggplotly(p2)
    p2 <- p2 %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                      xanchor = "center",  # use center of legend as anchor
                                      x = 0.5,
                                      y = 1.2)) %>% config(displayModeBar = FALSE)
    p2 <- p2 %>% layout(xaxis = list(showgrid = F),
                       yaxis = list(gridcolor = "lightgray"))
    
    p2
}


############################################################################################################################################
#Use army models to create projections for the local area around the base
#Establish function for army SEIAR model. This allows us to pass though a simple function to gather all statistics when we plot
SEIAR_Model_Run<-function(num_init_cases, Pop.At.Risk, incub_period, latent_period, 
                          doubling, recovery_days, social_rate, hospital_rate,
                          icu_rate, ventilated_rate, hospital_dur, icu_dur, ventilated_dur, n_days, 
                          secondary_cases = 2.5, distribution_e_to_a = 0.5){
    
    ###DEFINING COMPARTMENTS OF THE MODEL
    total_infections <- num_init_cases / (hospital_rate/100) 
    I <- total_infections 
    S <- (Pop.At.Risk - I)
    E <- (total_infections*secondary_cases) * distribution_e_to_a #Assuming that each infectious person will generate 2.5 secondary cases
    A <- (total_infections*secondary_cases) * (1-distribution_e_to_a) ##Distributing the secondary cases between the E and A compartments
    R <- 0
    
    ###DEFINING PARAMETERS
    intrinsic_growth_rate <- 2 ^(1 / doubling) -1
    sigma <- 1/latent_period #Latent period (approx 2 days)
    gamma_1 <- 1/(incub_period - latent_period)
    gamma_2 <- 1/(recovery_days - latent_period - (incub_period - latent_period))
    beta <- (intrinsic_growth_rate + (1/recovery_days)) / S * (1-social_rate/100)
    r_t <- beta / (1/recovery_days) * S 
    r_0 <- r_t / (1 - social_rate/100)
    doubling_time_t <- 1 / log2(beta*S - (1/recovery_days) + 1)
    
    ###ITERATIVE LISTING OF MODEL
    myList <- list()
    myList$total_infections <- total_infections
    myList$S <- S
    myList$E <- E
    myList$A <- A
    myList$I <- I
    myList$R <- R
    myList$intrinsic_growth_rate <- intrinsic_growth_rate
    myList$sigma <- sigma
    myList$gamma_1 <- gamma_1
    myList$gamma_2 <- gamma_2
    myList$beta <- beta
    myList$r_t <- r_t
    myList$r_0 <- r_0
    myList$doubling_time_t <- doubling_time_t
    
    #initial values
    N = S + E + A + I + R
    hos_add <- ((A+I) * hospital_rate/100)
    hos_cum <- ((A+I) * hospital_rate/100)
    icu_add <- (hos_add * icu_rate/100)
    icu_cum <- (hos_cum * icu_rate/100)
    
    #create the data frame
    sir_data <- data.frame(t = 1,
                           S = S,
                           E = E,
                           A = A,
                           I = I,
                           R = R,
                           hos_add = hos_add,
                           hos_cum = hos_cum,
                           icu_add = hos_add * icu_rate/100,
                           icu_cum = hos_cum * icu_rate/100,
                           vent_add = icu_add * ventilated_rate/100,
                           vent_cum = icu_cum * ventilated_rate/100,
                           Id = 0
    )
    
    for(i in 2:n_days){
        y <- seiar(S,E,A,I,R, beta, sigma, gamma_1, gamma_2, N)
        S <- y$S
        E <- y$E
        A <- y$A
        I <- y$I
        R <- y$R
        
        #calculate new infections
        Id <- (sir_data$S[i-1] - S)
        
        #portion of the the newly infected that are in the hospital, ICU, and Vent
        hos_add <- Id * hospital_rate/100
        hos_cum <- sir_data$hos_cum[i-1] + hos_add
        
        icu_add <- hos_add * icu_rate/100
        icu_cum <- sir_data$icu_cum[i-1] + icu_add
        
        vent_add <- icu_add * ventilated_rate/100 
        vent_cum <- sir_data$vent_cum[i-1] + vent_add
        
        temp <- data.frame(t = i,
                           S = S,
                           E = E,
                           A = A,
                           I = I,
                           R = R,
                           hos_add = hos_add,
                           hos_cum = hos_cum,
                           icu_add = icu_add,
                           icu_cum = icu_cum,
                           vent_add = vent_add,
                           vent_cum = vent_cum,
                           Id = Id
        )
        
        sir_data <- rbind(sir_data,temp)
    }
    
    #doing some weird stuff to get a rolling sum of hospital impacts based on length of stay (los)
    if(n_days > hospital_dur){
        h_c <- rollsum(sir_data$hos_add,hospital_dur)
        sir_data$hos_cum <- c(sir_data$hos_cum[1:(n_days - length(h_c))],h_c)
    } 
    if(n_days > icu_dur){
        i_c <- rollsum(sir_data$icu_add,icu_dur)
        sir_data$icu_cum <- c(sir_data$icu_cum[1:(n_days - length(i_c))],i_c)
    } 
    if(n_days > ventilated_dur){
        v_c <- rollsum(sir_data$vent_add,ventilated_dur)
        sir_data$vent_cum <- c(sir_data$vent_cum[1:(n_days - length(v_c))],v_c)
    } 
    #write.csv(sir_data, file = 'test.csv') # for testing
    h_m <- round(max(sir_data$hos_cum), 0)
    i_m <- round(max(sir_data$icu_cum), 0)
    v_m <- round(max(sir_data$vent_cum), 0)
    myList$sir <- sir_data
    myList$hos_max <- h_m
    myList$icu_max <- i_m
    myList$vent_max <- v_m 
    
    h_m <- sir_data$t[which.max(sir_data$hos_cum)][1]
    i_m <- sir_data$t[which.max(sir_data$icu_cum)][1]
    v_m <- sir_data$t[which.max(sir_data$vent_cum)][1]
    myList$hos_t_max <- h_m
    myList$icu_t_max <- i_m
    myList$vent_t_max <- v_m 
    
    h_m <- round(max(sir_data$hos_add), 0)
    i_m <- round(max(sir_data$icu_add), 0)
    v_m <- round(max(sir_data$vent_add), 0)
    
    myList$hos_add <- h_m
    myList$icu_add <- i_m
    myList$vent_add <- v_m 
    return(myList)
}

seiar<-function(S,E,A,I,R, beta, sigma, gamma_1, gamma_2, N){
    Sn <- (-beta * S * (A + I)) + S
    En <- ((beta * S * (A + I)) - (sigma * E)) + E
    An <- ((sigma * E) - (gamma_1 * A)) + A
    In <- ((gamma_1 * A) - (gamma_2 * I)) + I
    Rn <- (gamma_2 * I) + R
    
    if(Sn < 0) Sn = 0
    if(En < 0) En = 0
    if(An < 0) An = 0
    if(In < 0) In = 0
    if(Rn < 0) Rn = 0
    
    scale = N / (Sn + En + An + In + Rn)
    myListSIR <- list()
    myListSIR$S <- (Sn * scale)
    myListSIR$E <- (En * scale)
    myListSIR$A <- (An * scale)
    myListSIR$I <- (In * scale)
    myListSIR$R <- (Rn * scale)
    return(myListSIR)
}


SIR_Model_Run<-function(num_init_cases, Pop.At.Risk, detect_prob, 
                        doubling, recovery_days, social_rate, hospital_rate,
                        icu_rate, ventilated_rate, hospital_dur, icu_dur, ventilated_dur, n_days){
    #create parameters for model
    total_infections <- num_init_cases / (hospital_rate/100)
    I <- total_infections / (detect_prob / 100) 
    S <- (Pop.At.Risk - I)
    R <- 0
    intrinsic_growth_rate = 2 ^(1 / doubling) -1
    recovery_days <- recovery_days
    gamma <- 1 / recovery_days
    beta <- (intrinsic_growth_rate + gamma) / S * (1-social_rate/100)
    r_t <- beta / gamma * S 
    r_0 <- r_t / (1 - social_rate/100)
    doubling_time_t <- 1 / log2(beta*S - gamma + 1)
    myList <- list()
    myList$total_infections <- total_infections
    myList$S <- S
    myList$I <- I
    myList$R <- R
    myList$intrinsic_growth_rate <- intrinsic_growth_rate
    myList$recovery_days <- recovery_days
    myList$gamma <- gamma
    myList$beta <- beta
    myList$r_t <- r_t
    myList$r_0 <- r_0
    myList$doubling_time_t <- doubling_time_t
    
    
    
    
    #initial values
    N = S + I + R
    hos_add <- (I * hospital_rate/100)
    hos_cum <- (I * hospital_rate/100)
    icu_add <- (hos_add * icu_rate/100)
    icu_cum <- (hos_cum * icu_rate/100)
    
    #create the data frame
    sir_data <- data.frame(t = 1,
                           S = S,
                           I = I,
                           R = R,
                           hos_add = hos_add,
                           hos_cum = hos_cum,
                           icu_add = hos_add * icu_rate/100,
                           icu_cum = hos_cum * icu_rate/100,
                           vent_add = icu_add * ventilated_rate/100,
                           vent_cum = icu_cum * ventilated_rate/100,
                           Id = 0
    )
    
    for(i in 2:n_days){
        y <- sir(S,I,R, beta, gamma, N)
        S <- y$S
        I <- y$I
        R <- y$R
        
        #calculate new infections
        Id <- (sir_data$S[i-1] - S)
        
        #portion of the the newly infected that are in the hospital, ICU, and Vent
        hos_add <- Id * hospital_rate/100
        hos_cum <- sir_data$hos_cum[i-1] + hos_add
        
        icu_add <- hos_add * icu_rate/100
        icu_cum <- sir_data$icu_cum[i-1] + icu_add
        
        vent_add <- icu_add * ventilated_rate/100 
        vent_cum <- sir_data$vent_cum[i-1] + vent_add
        
        temp <- data.frame(t = i,
                           S = S,
                           I = I,
                           R = R,
                           hos_add = hos_add,
                           hos_cum = hos_cum,
                           icu_add = icu_add,
                           icu_cum = icu_cum,
                           vent_add = vent_add,
                           vent_cum = vent_cum,
                           Id = Id
        )
        
        sir_data <- rbind(sir_data,temp)
    }
    
    #doing some weird stuff to get a rolling sum of hospital impacts based on length of stay (los)
    if(n_days > hospital_dur){
        h_c <- rollsum(sir_data$hos_add,hospital_dur)
        sir_data$hos_cum <- c(sir_data$hos_cum[1:(n_days - length(h_c))],h_c)
    } 
    if(n_days > icu_dur){
        i_c <- rollsum(sir_data$icu_add,icu_dur)
        sir_data$icu_cum <- c(sir_data$icu_cum[1:(n_days - length(i_c))],i_c)
    } 
    if(n_days > ventilated_dur){
        v_c <- rollsum(sir_data$vent_add,ventilated_dur)
        sir_data$vent_cum <- c(sir_data$vent_cum[1:(n_days - length(v_c))],v_c)
    } 
    #write.csv(sir_data, file = 'test.csv') # for testing
    h_m <- round(max(sir_data$hos_cum), 0)
    i_m <- round(max(sir_data$icu_cum), 0)
    v_m <- round(max(sir_data$vent_cum), 0)
    myList$sir <- sir_data
    myList$hos_max <- h_m
    myList$icu_max <- i_m
    myList$vent_max <- v_m 
    
    h_m <- sir_data$t[which.max(sir_data$hos_cum)][1]
    i_m <- sir_data$t[which.max(sir_data$icu_cum)][1]
    v_m <- sir_data$t[which.max(sir_data$vent_cum)][1]
    myList$hos_t_max <- h_m
    myList$icu_t_max <- i_m
    myList$vent_t_max <- v_m 
    
    h_m <- round(max(sir_data$hos_add), 0)
    i_m <- round(max(sir_data$icu_add), 0)
    v_m <- round(max(sir_data$vent_add), 0)
    
    myList$hos_add <- h_m
    myList$icu_add <- i_m
    myList$vent_add <- v_m 
    return(myList)
}

sir<-function(S,I,R, beta, gamma, N){
    Sn <- (-beta * S * I) + S
    In = (beta * S * I - gamma * I) + I
    Rn = gamma * I + R
    if(Sn < 0) Sn = 0
    if(In < 0) In = 0
    if(Rn < 0) Rn = 0
    
    scale = N / (Sn + In + Rn )
    myListSIR <- list()
    myListSIR$S <- (Sn * scale)
    myListSIR$I <- (In * scale)
    myListSIR$R <- (Rn * scale)
    return(myListSIR)
}

#Overlay CHIME and IHME models
PlotOverlay<-function(ChosenBase, IncludedCounties, IncludedHospitals, SocialDistance, DaysProjected){
    
    #Creating the stats and dataframes determined by the base we choose to look at.
    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
    TotalBedsCounty <- sum(IncludedHospitals$BEDS)
    
    #Get regional and state populations
    StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
    RegPop <- sum(IncludedCounties$Population)
    StPop <- sum(StPopList$Population)
    
    # Use Population ratio to scale IHME
    PopRatio <- RegPop/StPop
    
    # Get total hospital bed number across state
    IncludedHospitalsST <- dplyr::filter(HospitalInfo, STATE == toString(BaseState$State[1]))
    TotalBedsState <- sum(IncludedHospitalsST$BEDS)
    
    # Calculate bed ratio
    BedProp <- TotalBedsCounty/TotalBedsState
    
    # Apply ratio's to IHME data
    IHME_Region <- IHME_State
    IHME_Region$allbed_mean = round(IHME_State$allbed_mean*PopRatio)
    IHME_Region$allbed_lower = round(IHME_State$allbed_lower*PopRatio)
    IHME_Region$allbed_upper = round(IHME_State$allbed_upper*PopRatio)
    IHME_Data<-data.frame(IHME_Region$date,IHME_Region$allbed_mean, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
    
    BaseState<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
    
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
    
    #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
    #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
    ActiveCases<-rev(CovidCounties)[1:7]
    ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
    colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
    SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
    colnames(SIRinputs)<-c("cases","pop","doubling")
    
    
    ####################################################################################
    #Mean Estimate
    
    #Next we use the calculated values, along with estimated values from the CDC. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysProjected
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                               ventilatortime,daysforecasted,Ro, .5)
    
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    
    
    ####################################################################################
    #Lower Estimate
    
    #Next we use the calculated values, along with estimated values from the CDC. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-10
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysProjected
    
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays, 
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                               icutime,ventilatortime,daysforecasted,Ro, .5)
    
    DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
    
    ####################################################################################
    #Upper Estimate
    #Next we use the calculated values, along with estimated values from the CDC. 
    
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-7
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5.5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysProjected
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                               icutime,ventilatortime,daysforecasted,Ro, .5)
    
    DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations","Lower Bound Hospitalizations","Upper Bound Hospitalizations")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases","Maximum Total Cases")
    
    DailyData$`Expected Hospitalizations` <- round(DailyData$`Expected Hospitalizations`,0)
    DailyData$`Lower Bound Hospitalizations` <- round(DailyData$`Lower Bound Hospitalizations`,0)
    DailyData$`Upper Bound Hospitalizations` <- round(DailyData$`Upper Bound Hospitalizations`,0)
    DailyData<-DailyData[-1,]
    colnames(IHME_Data)<-c("ForecastDate", "Expected Hospitalizations", "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
    DailyData$ID<-rep("CHIME",nrow(DailyData))
    IHME_Data$ID<-rep("IHME",nrow(IHME_Data))
    HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    OverlayData<-rbind(DailyData,IHME_Data)
    OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
    
    OverlayData<- dplyr::filter(OverlayData, ForecastDate >= Sys.Date())
    
    OverlayData<-rbind(HistoricalData, OverlayData)
    
    
    
    
    projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID)) +
        geom_line() + 
        scale_colour_manual(values=c("tan", "blue", "black"))+
        scale_fill_manual(values = c("tan4", "cadetblue", "gray"))+
        geom_ribbon(aes(ymin = `Lower Bound Hospitalizations`, ymax = `Upper Bound Hospitalizations`), 
                    alpha = .2) +
        ggtitle("Projected Hospitalizations")+
        theme_bw() + 
        theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
              axis.title = element_text(face = "bold", size = 11, family = "sans"),
              axis.text.x = element_text(angle = 60, hjust = 1), 
              axis.line = element_line(color = "black"),
              legend.position = "top",
              plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
        scale_x_date(date_breaks = "2 week")+
        labs(color = "ID")
    
    
    ggplotly(projections)
    
}




# Create data tables for analysis ---------------------------------------------------------------------------------------------------------------------------------------------------

#Create Data Table for local statistics
GetLocalDataTable<-function(IncludedCounties){
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1],round(CountyDataTable$DistanceMiles,0))
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)", "Distance" )
    CountyDataTable1 <- CountyDataTable[order(CountyDataTable$Distance),]
    CountyDataTable1
}



# Create choropleth functions -------------------------------------------------------------------------------------------------------------------------------------------------------

#Create plot of Covid Cases by County
PlotLocalChoro<-function(IncludedCounties, ChosenBase, TypofPlot){
    
    if (TypofPlot == "County") {
        BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        #Creating the choropleth dataset so we have all info in one data set and can plot it together
        choropleth <- merge(county_df, PlottingCountyData, by = c("county", "State"))
        colnames(choropleth)[7]<-"CountyFIPS"
        choropleth <- choropleth[order(choropleth$order), ]
        choropleth$state_name<-NULL
        choropleth<-data.frame(choropleth$county, choropleth$State, choropleth$CountyFIPS, choropleth$group, choropleth$lat, choropleth$long, rev(choropleth)[,1])
        colnames(choropleth)<-c("County","State","CountyFIPS","group","lat","long","Cases")
        choropleth<-subset(choropleth, CountyFIPS %in% IncludedCounties$FIPS)
        
        #Plot the data
        PlotCovidLocal<-ggplot(choropleth, aes(long, lat, group = group)) +
            geom_polygon(aes(fill = Cases)) +
            coord_fixed() +
            theme_minimal() +
            ggtitle("COVID-19 Cases by County (County View)") +
            geom_point(data = BaseStats, aes(x=Long, y=Lat, group = 1),
                       color = 'red', size = 5)+
            theme(axis.line = element_blank(), axis.text = element_blank(),
                  axis.ticks = element_blank(), axis.title = element_blank()) +
            scale_fill_viridis("Cases")
        
        ggplotly(PlotCovidLocal)
        
    } else  {
        BaseStats<-dplyr::filter(AFBaseLocations, Base == ChosenBase)
        #Creating the choropleth dataset so we have all info in one data set and can plot it together
        choropleth <- merge(county_df, PlottingCountyData, by = c("county", "State"))
        colnames(choropleth)[7]<-"CountyFIPS"
        choropleth <- choropleth[order(choropleth$order), ]
        choropleth$state_name<-NULL
        choropleth<-data.frame(choropleth$county, choropleth$State, choropleth$CountyFIPS, choropleth$group, choropleth$lat, choropleth$long, rev(choropleth)[,1])
        colnames(choropleth)<-c("County","State","CountyFIPS","group","lat","long","Cases")
        choropleth<-subset(choropleth, State %in% IncludedCounties$State)
        
        #Plot the data
        PlotCovidLocal<-ggplot(choropleth, aes(long, lat, group = group)) +
            geom_polygon(aes(fill = log(Cases))) +
            coord_fixed() +
            theme_minimal() +
            ggtitle("COVID-19 Cases by County (State View)") +
            geom_point(data = BaseStats, aes(x= Long, y= Lat, group = 1),
                       color = 'red', size = 5)+
            theme(axis.line = element_blank(), axis.text = element_blank(),
                  axis.ticks = element_blank(), axis.title = element_blank()) +
            scale_fill_viridis("log(Cases)")
        
        ggplotly(PlotCovidLocal)
        
    }
    
    
}


NationalOverlayPlot<-function(SocialDistance, DaysForecasted){
    
    
    #Get IHME Data upper lower and mean combined by date
    Dataframe1<-IHME_Model %>% 
        group_by(date) %>% 
        summarise(allbed_mean = sum(allbed_mean))
    Dataframe2<-IHME_Model %>% 
        group_by(date) %>% 
        summarise(allbed_lower = sum(allbed_lower))
    Dataframe3<-IHME_Model %>% 
        group_by(date) %>% 
        summarise(allbed_upper = sum(allbed_upper))
    IHMENationalData<-cbind(Dataframe1, Dataframe2$allbed_lower, Dataframe3$allbed_upper)
    
    HistoricalData<-colSums(CovidConfirmedCases[,5:length(CovidConfirmedCases)])
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
    
    
    ####################################################################################
    #Mean Estimate
    NationalPop <-  sum(CountyInfo$Population)
    NationalCases<-sum(rev(CovidConfirmedCases)[1]-rev(CovidConfirmedCases)[8])
    
    
    #Next we use the calculated values, along with estimated values from the CDC. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-NationalCases*.05
    pop<-NationalPop
    doubling<-8
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysForecasted
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                               ventilatortime,daysforecasted,Ro, .5)
    
    MyDates<-seq(Sys.Date()-(length(CovidConfirmedCases)-20), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    
    
    ####################################################################################
    #Lower Estimate
    
    #Next we use the calculated values, along with estimated values from the CDC. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-NationalCases*.05
    pop<-NationalPop
    doubling<-10
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysForecasted
    
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays, 
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                               icutime,ventilatortime,daysforecasted,Ro, .5)
    
    DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
    
    ####################################################################################
    #Upper Estimate
    #Next we use the calculated values, along with estimated values from the CDC. 
    
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-NationalCases*.05
    pop<-NationalPop
    doubling<-7
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5.5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysForecasted
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                               icutime,ventilatortime,daysforecasted,Ro, .5)
    
    DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations","Lower Bound Hospitalizations","Upper Bound Hospitalizations")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases","Maximum Total Cases")
    
    DailyData$`Expected Hospitalizations` <- round(DailyData$`Expected Hospitalizations`,0)
    DailyData$`Lower Bound Hospitalizations` <- round(DailyData$`Lower Bound Hospitalizations`,0)
    DailyData$`Upper Bound Hospitalizations` <- round(DailyData$`Upper Bound Hospitalizations`,0)
    DailyData<-DailyData[-1,]
    colnames(IHMENationalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
    DailyData$ID<-rep("CHIME",nrow(DailyData))
    IHMENationalData$ID<-rep("IHME",nrow(IHMENationalData))
    HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    OverlayData<-rbind(DailyData,IHMENationalData)
    OverlayData$ForecastDate<-as.Date(OverlayData$ForecastDate)
    
    OverlayData<- dplyr::filter(OverlayData, ForecastDate >= Sys.Date())
    
    OverlayData<-rbind(HistoricalData, OverlayData)
    
    
    projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID)) +
        geom_line() + 
        scale_colour_manual(values=c("tan", "blue", "black"))+
        scale_fill_manual(values = c("tan4", "cadetblue", "gray"))+
        geom_ribbon(aes(ymin = `Lower Bound Hospitalizations`, ymax = `Upper Bound Hospitalizations`), 
                    alpha = .2) +
        ggtitle("Projected Hospitalizations")+
        theme_bw() + 
        theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
              axis.title = element_text(face = "bold", size = 11, family = "sans"),
              axis.text.x = element_text(angle = 60, hjust = 1), 
              axis.line = element_line(color = "black"),
              legend.position = "top",
              plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
        scale_x_date(date_breaks = "2 week")+
        labs(color = "ID")+
        scale_y_continuous(labels = comma)
    
    
    ggplotly(projections)
    
}



CHIMENationalPlot<-function(SocialDistance, DaysForecasted){
    ####################################################################################
    #Mean Estimate
    NationalPop <-  sum(CountyInfo$Population)
    NationalCases<-sum(rev(CovidConfirmedCases)[1]-rev(CovidConfirmedCases)[8])
    
    HistoricalData<-colSums(CovidConfirmedCases[,5:length(CovidConfirmedCases)])
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
    
    
    #Next we use the calculated values, along with estimated values from the CDC. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-NationalCases*.05
    pop<-NationalPop
    doubling<-8
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysForecasted
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                               ventilatortime,daysforecasted,Ro, .5)
    
    MyDates<-seq(Sys.Date()-(length(CovidConfirmedCases)-20), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    
    
    ####################################################################################
    #Lower Estimate
    
    #Next we use the calculated values, along with estimated values from the CDC. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-NationalCases*.05
    pop<-NationalPop
    doubling<-10
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysForecasted
    
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays, 
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                               icutime,ventilatortime,daysforecasted,Ro, .5)
    
    DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
    
    ####################################################################################
    #Upper Estimate
    #Next we use the calculated values, along with estimated values from the CDC. 
    
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-NationalCases*.05
    pop<-NationalPop
    doubling<-7
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5.5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-DaysForecasted
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                               icutime,ventilatortime,daysforecasted,Ro, .5)
    
    DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Hospitalizations","Lower Bound Hospitalizations","Upper Bound Hospitalizations")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases","Maximum Total Cases")
    
    DailyData$`Expected Hospitalizations` <- round(DailyData$`Expected Hospitalizations`,0)
    DailyData$`Lower Bound Hospitalizations` <- round(DailyData$`Lower Bound Hospitalizations`,0)
    DailyData$`Upper Bound Hospitalizations` <- round(DailyData$`Upper Bound Hospitalizations`,0)
    DailyData<-DailyData[-1,]
    DailyData$ID<-rep("CHIME", nrow(DailyData))
    HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    
    OverlayData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
    
    OverlayData<-rbind(HistoricalData, OverlayData)
    
    
    projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID)) +
        geom_line(linetype = "dashed", size = 0.75) +
        scale_colour_manual(values=c("tan","black"))+
        scale_fill_manual(values = c("tan4", "gray"))+
        geom_ribbon(aes(ymin = `Lower Bound Hospitalizations`, ymax = `Upper Bound Hospitalizations`), 
                    alpha = .2) +
        #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
        xlab('Date') +
        ylab('Daily Hospitalizations') +
        ggtitle("CHIME Projected Daily Hospitalizations") +
        theme_bw() + 
        theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
              axis.title = element_text(face = "bold", size = 11, family = "sans"),
              axis.text.x = element_text(angle = 60, hjust = 1), 
              axis.line = element_line(color = "black"),
              legend.position = "top",
              plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
        scale_x_date(date_breaks = "3 week")+
        labs(color='')+
        scale_y_continuous(labels = comma)
    
    
    ggplotly(projections)
}

IHMENationalProjections<-function(){
    #Get IHME Data upper lower and mean combined by date
    Dataframe1<-IHME_Model %>% 
        group_by(date) %>% 
        summarise(allbed_mean = sum(allbed_mean))
    Dataframe2<-IHME_Model %>% 
        group_by(date) %>% 
        summarise(allbed_lower = sum(allbed_lower))
    Dataframe3<-IHME_Model %>% 
        group_by(date) %>% 
        summarise(allbed_upper = sum(allbed_upper))
    IHMENationalData<-cbind(Dataframe1, Dataframe2$allbed_lower, Dataframe3$allbed_upper)
    colnames(IHMENationalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
    
    HistoricalData<-colSums(CovidConfirmedCases[,5:length(CovidConfirmedCases)])
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
    
    IHMENationalData$ID<-rep("IHME", nrow(IHMENationalData))
    HistoricalData$ID<-rep("Past Data", nrow(HistoricalData))
    OverlayData<- dplyr::filter(IHMENationalData, ForecastDate >= Sys.Date())
    OverlayData<-rbind(HistoricalData, OverlayData)
    
    projections <-  ggplot(OverlayData, aes(x=ForecastDate, y=`Expected Hospitalizations`, color = ID, fill = ID)) +
        geom_line(linetype = "dashed", size = 0.75) +
        scale_colour_manual(values=c("blue","black"))+
        scale_fill_manual(values = c("cadetblue", "gray"))+
        geom_ribbon(aes(ymin = `Lower Bound Hospitalizations`, ymax = `Upper Bound Hospitalizations`), 
                    alpha = .2) +
        #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
        xlab('Date') +
        ylab('Daily Hospitalizations') +
        ggtitle("IHME Projected Daily Hospitalizations") +
        theme_bw() + 
        theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
              axis.title = element_text(face = "bold", size = 11, family = "sans"),
              axis.text.x = element_text(angle = 60, hjust = 1), 
              axis.line = element_line(color = "black"),
              legend.position = "top",
              plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
        scale_x_date(date_breaks = "2 week")+
        labs(color='')+
        scale_y_continuous(labels = comma)
    
    
    ggplotly(projections)
}



CHIMELocalPlot<-function(SocialDistance, ForecastedDays, IncludedCounties){
    
    #Get data for counties with covid cases. We want number of cases, the rate of the cases and maybe other data.
    #We include State, county, population in those counties, cases, fatalities, doubling rate
    CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% IncludedCounties$FIPS)
    HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
    HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
    HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
    colnames(HistoricalData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases","Maximum Daily Cases")
    
    DeathCounties<-subset(CovidDeaths, CountyFIPS %in% IncludedCounties$FIPS)
    CaseRate <- subset(CovidConfirmedCasesRate, CountyFIPS %in% IncludedCounties$FIPS)
    CountyDataTable<-cbind(IncludedCounties,rev(CovidCounties)[,1],rev(DeathCounties)[,1],rev(CaseRate)[,1])
    CountyDataTable<-data.frame(CountyDataTable$State,CountyDataTable$County,CountyDataTable$Population, rev(CountyDataTable)[,3], rev(CountyDataTable)[,2],rev(CountyDataTable)[,1])
    colnames(CountyDataTable)<-c("State","County","Population","Total Confirmed Cases","Total Fatalities", "Case Doubling Rate (days)" )
    
    #Cleaning it up to input into the SEIAR model, we include countyFIPS, CountyName, State, State FIPS, number of cases, population, and doubling rate
    #We take the data and create a dataframe called SIR inputs. It checks out by total cases, total population, and average doubling rate
    ActiveCases<-rev(CovidCounties)[1:7]
    ActiveCases<-data.frame(CovidCounties[,1:4],ActiveCases[,1], IncludedCounties$Population, CountyDataTable$`Case Doubling Rate (days)`)
    colnames(ActiveCases)<-c("CountyFIPS","CountyName","State","StateFIPS","CurrentCases", "Population", "Doubling Rate")
    SIRinputs<-data.frame(sum(ActiveCases$CurrentCases),sum(ActiveCases$Population), mean(ActiveCases$`Doubling Rate`))
    colnames(SIRinputs)<-c("cases","pop","doubling")
    
    
    ####################################################################################
    #Mean Estimate
    
    #Next we use the calculated values, along with estimated values from the CDC. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-8
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    socialdistancing<-SocialDistance
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    daysforecasted<-ForecastedDays
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,icutime,
                               ventilatortime,daysforecasted,Ro, .5)
    
    MyDates<-seq(Sys.Date()-(length(CovidCounties)-80), length=daysforecasted, by="1 day")
    DailyData<-data.frame(MyDates, SEIARProj$sir$hos_add)
    TotalData<-data.frame(MyDates, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases")
    
    
    ####################################################################################
    #Lower Estimate
    
    #Next we use the calculated values, along with estimated values from the CDC. 
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-10
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    
    hospitalizationrate<-5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays, 
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                               icutime,ventilatortime,daysforecasted,Ro, .5)
    
    DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases")
    
    ####################################################################################
    #Upper Estimate
    #Next we use the calculated values, along with estimated values from the CDC. 
    
    #The only input we want from the user is the social distancing rate. For this example, we just use 0.5
    cases<-SIRinputs$cases
    pop<-SIRinputs$pop
    doubling<-7
    
    #Established Variables at the start for every county or populations
    Ro<-2.5
    incubationtime<-5
    latenttime<-2
    recoverydays<-14
    
    hospitalizationrate<-5.5
    icurate<-6
    ventilatorrate<-3
    hospitaltime<-3.5
    icutime<-4
    ventilatortime<-7
    
    
    #Now we throw the values above into the SEIAR model, and we create dates for the number of days we decided to forecast as well (place holder for now).
    #With the outputs, we grab the daily hospitalized people and the cumulative hospitalizations. Then we name the columns
    SEIARProj<-SEIAR_Model_Run(cases, pop, incubationtime, latenttime,doubling,recoverydays,
                               socialdistancing,hospitalizationrate, icurate,ventilatorrate,hospitaltime,
                               icutime,ventilatortime,daysforecasted,Ro, .5)
    
    DailyData<-data.frame(DailyData, SEIARProj$sir$hos_add)
    TotalData<-data.frame(TotalData, SEIARProj$sir$hos_cum)
    colnames(DailyData)<-c("ForecastDate", "Expected Daily Cases","Minimum Daily Cases","Maximum Daily Cases")
    colnames(TotalData)<-c("ForecastDate", "Total Daily Cases", "Minimum Total Cases","Maximum Total Cases")
    
    DailyProjectionsSub <- melt(data.table(DailyData), id=c("ForecastDate"))
    TotalProjectionsSub <- melt(data.table(TotalData), id=c("ForecastDate"))
    
    DailyData$`Expected Daily Cases` <- round(DailyData$`Expected Daily Cases`,0)
    DailyData$`Minimum Daily Cases` <- round(DailyData$`Minimum Daily Cases`,0)
    DailyData$`Maximum Daily Cases` <- round(DailyData$`Maximum Daily Cases`,0)
    DailyData<-DailyData[-1,]
    
    
    DailyData<- dplyr::filter(DailyData, ForecastDate >= Sys.Date())
    
    PlottingData<-rbind(HistoricalData, DailyData)
    
    
    #Plot for local area cumulative cases
    projections <- ggplot(data = PlottingData, 
                          aes(x=ForecastDate,
                              y=`Expected Daily Cases`,
                              ymin = `Minimum Daily Cases`,
                              ymax = `Maximum Daily Cases`)) + 
        #geom_line(aes(x=ForecastDate, y=DailyData$`Expected Daily Cases`, ymin = DailyData$`Minimum Daily Cases` , ymax = DailyData$`Maximum Daily Cases`)) +
        geom_line(linetype = "dashed", size = 0.75) +
        geom_ribbon(alpha=0.3, fill = "tan4") +
        #scale_colour_manual(values=c("Blue", "Orange", "Red"))+
        xlab('Date') +
        ylab('Daily Hospitalizations') +
        ggtitle("CHIME Projected Daily Hospitalizations") +
        theme_bw() + 
        theme(plot.title = element_text(face = "bold", size = 15, family = "sans"),
              axis.title = element_text(face = "bold", size = 11, family = "sans"),
              axis.text.x = element_text(angle = 60, hjust = 1), 
              axis.line = element_line(color = "black"),
              legend.position = "top",
              plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
        scale_x_date(date_breaks = "3 week")+
        labs(color='')
    
    ggplotly(projections)
}


# Identify Info Pages
# Inputs
InfoLink <- includeMarkdown("https://raw.githubusercontent.com/treypujats/COVID19/master/covid19/InputsInfo.md")
CalcLink <- includeMarkdown("https://raw.githubusercontent.com/treypujats/COVID19/master/covid19/CalcInfo.md")
SourceLink <- includeMarkdown("https://raw.githubusercontent.com/treypujats/COVID19/master/covid19/SourceInfo.md")
OverviewLink <- includeMarkdown("https://raw.githubusercontent.com/treypujats/COVID19/master/covid19/OverviewInfo.md")
