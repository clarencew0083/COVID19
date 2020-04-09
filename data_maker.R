library(usmap)
library(geosphere)
library(readr)

AFBaseLocations <- readr::read_csv("https://gitlab.com/messer06/covid/-/raw/master/AFB_Locs.csv?inline=false")
CountyInfo <- readr::read_csv("https://gitlab.com/messer06/covid/-/raw/master/County_Info.csv?inline=false")
HospitalInfo <- readr::read_csv("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D")

# Add other information to data objects here

cimd = matrix(NA, 
              ncol = nrow(AFBaseLocations),
              nrow = nrow(CountyInfo))

for(c in 1:nrow(AFBaseLocations)) {
  
    for(r in 1:nrow(CountyInfo)) {
      
        cimd[r,c] = distm(c(AFBaseLocations$Long[c], 
                            AFBaseLocations$Lat[c]), 
                          c(CountyInfo$Longitude[r], 
                            CountyInfo$Latitude[r]), 
                            fun = distHaversine)/1609.34
        
    }
  
}

himd = matrix(NA, 
              ncol = nrow(AFBaseLocations),
              nrow = nrow(HospitalInfo))

for(c in 1:nrow(AFBaseLocations)) {
  
    for(r in 1:nrow(HospitalInfo)) {
      
        himd[r,c] = distm(c(AFBaseLocations$Long[c], 
                            AFBaseLocations$Lat[c]), 
                          c(HospitalInfo$LONGITUDE[r], 
                            HospitalInfo$LATITUDE[r]), 
                            fun = distHaversine)/1609.34

    }
  
}

save(cimd, file = "covid19/data/cimd.rda")
save(himd, file = "covid19/data/himd.rda")
