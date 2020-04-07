##################
##### Server #####
##################

# Layout
##############################################################################################################################################
# The server is used to generate outputs based on the functions in the global. These outputs are then referenced in the UI and diplayed in the app
# First:  Creating reactive functions that change based on radius and Base. The reactive functions are the most important functions in the app.
#         Reactive functions change every time a new base is chosen or a radius is chosen. This updated the app automatically.
# Second: This creates the output variables that can be referenced in the user interface. Each plot, statistic or map needs to have an output.
#         There are 5 sub categories included: Common statistics, line plots, choropleth charts, projections, and data tables.
# Third:  This creates the help settings in the app so that users can see documentation of inputs, sources, and calculations.
##############################################################################################################################################       


# Define server logic, within this all ouputs and reactive variables are generated. 
server <- function(input, output) {
    
    
    # Step One
    ###################################################################################################################################################
    
    
    GetCounties<-reactive({
        #BaseStats<-dplyr::filter(AFBaseLocations, Base == input$Base)
        
        CountyInfo$DistanceMiles = cimd[,as.character(input$Base)]
        #for (i in 1:3143) {
        #    CountyInfo$DistanceMiles[i]<-(distm(c(BaseStats$Long, BaseStats$Lat), c(CountyInfo$Longitude[i], CountyInfo$Latitude[i]), fun = distHaversine)/1609.34)
        #}
        IncludedCounties<-dplyr::filter(CountyInfo, DistanceMiles <= input$Radius)
        IncludedCounties
    })
    
    GetHospitals<-reactive({
        #Finds number of hospitals in radius
        #BaseStats<-dplyr::filter(AFBaseLocations, Base == input$Base)
        
        HospitalInfo$DistanceMiles = himd[,as.character(input$Base)]
        
        IncludedHospitals<-dplyr::filter(HospitalInfo, (DistanceMiles <= input$Radius))
        IncludedHospitals<-dplyr::filter(IncludedHospitals, (TYPE=="GENERAL ACUTE CARE") | (TYPE=="CRITICAL ACCESS"))
        IncludedHospitals
    })
    
    # Step Two
    ###################################################################################################################################################
    
    
    # Output common statistics -------------------------------------------------------------------------------------------------------------------------------------------
    
    #Finds which counties in given radius. Also Give county statistics
    output$TotalPopulation <- renderValueBox({
        MyCounties<-GetCounties()
        valueBox(subtitle = "Total Regional Population",
                 comma(CalculateCounties(input$Base,input$Radius, MyCounties)),
                 icon = icon("list-ol"),
                 color = "light-blue"
        )
        
    })
    
    # Finds Covid Cases and statistics on covid per county
    output$CovidCases <- renderValueBox({
        MyCounties<-GetCounties()
        valueBox(subtitle = "Local Cases",
                 comma(CalculateCovid(input$Base,input$Radius,MyCounties)),
                 icon = icon("list-ol"),
                 color = "light-blue"
        )
        
    })
    
    #Outputs change in covid cases per day
    output$CaseChangeLocal <- renderValueBox({
        MyCounties<-GetCounties()
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
        changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
        
        valueBox(paste("+",toString(changeC)),
                 subtitle = "New Confirmed Cases", 
                 color = "light-blue")
    })
    
    
    # Finds Covid deaths and statistics on covid per county
    output$LocalCovidDeaths <- renderValueBox({
        MyCounties<-GetCounties()
        valueBox(subtitle = "Local Fatalities",
                 comma(CalculateDeaths(input$Base, input$Radius, MyCounties)),
                 icon = icon("skull"),
                 color = "blue"
        )
    })
    
    #Outputs change in deaths per day   
    output$DeathChangeLocal <- renderValueBox({
        MyCounties<-GetCounties()
        CovidCounties<-subset(CovidDeaths, CountyFIPS %in% MyCounties$FIPS)
        changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
        
        valueBox(paste("+",toString(changeC)),
                 subtitle = "New Confirmed Fatalities", 
                 color = "blue")
    })
    
    #Finds hospital information within a given 100 mile radius. Calculates number of total hospital beds. Can compare to number of cases
    output$HospitalUtilization <- renderValueBox({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        valueBox(subtitle = "Local Hospital Utilization *Partially Notional*",
                 HospitalIncreases(input$Base,input$Radius, MyCounties, MyHospitals),
                 icon = icon("hospital"),
                 color = "navy")
    })
    
    
    
    output$HospUtlzChange <- renderValueBox({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        
        #Finds number of hospitals in radius
        TotalBeds<-sum(MyHospitals$BEDS)
        #Finds which counties in given radius. Also Give county statistics
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
        changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
        TotalHospital<-sum(CovidCounties[,ncol(CovidCounties)])
        NotHospital<-sum(rev(CovidCounties)[,7])
        StillHospital<-ceiling((TotalHospital-NotHospital))
        Upper<- round(((StillHospital+changeC*.1)/TotalBeds+.5)*100,1)
        #Lower<- round(((StillHospital+changeC*.207)/TotalBeds+.55)*100,1)
        paste(Upper," %", sep = "") 
        
        
        TotalBeds<-sum(MyHospitals$BEDS)
        
        #Finds which counties in given radius. Also Give county statistics
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
        n <- ncol(CovidCounties)-6
        x <- length(CovidCounties)
        changeC <- sum(rev(CovidCounties)[,1] - rev(CovidCounties)[,2])
        changey <- sum(rev(CovidCounties)[,2] - rev(CovidCounties)[,3])
        # Today
        TotalHospital<-sum(rev(CovidCounties)[,1])
        NotHospital<-sum(rev(CovidCounties)[,6])
        StillHospital<-ceiling((TotalHospital-NotHospital))
        Upper<-(signif(((StillHospital+changeC*.1)/TotalBeds+.5)*100,3))
        #Lower<-(signif(((StillHospital+changeC*.207)/TotalBeds+.6)*100,3))
        # Yesterday
        TotalHospitaly<-sum(CovidCounties[,ncol(CovidCounties)-1])
        NotHospitaly<-sum(CovidCounties[,n-1])
        StillHospitaly<-ceiling((TotalHospitaly-NotHospitaly))
        Uppery<-(signif(((StillHospitaly+changey*.1)/TotalBeds+.5)*100,3))
        #Lowery<-(signif(((StillHospitaly+changey*.207)/TotalBeds+.6)*100,3))
        chng <- round((Upper-Uppery)/2, 1)
        
        if (chng < 0) {
            sign <- ""
        } else {
            sign <- "+"
        }
        
        valueBox(paste(sign,toString(chng),"%"),
                 subtitle = "Hospital Utilization Change", 
                 color = "navy")
    })
    
    output$CHIMEPeakDate<-renderValueBox({
        MyCounties<-GetCounties()
        Peak<-CalculateCHIMEPeak(MyCounties, input$Base, input$Radius, input$social_dist, input$proj_days)
        Peak<-format(Peak)
        valueBox(subtitle = "CHIME Predicted Peak Hospitalizations",
                 paste(Peak),
                 icon = icon("hospital"),
                 color = "blue")
    })
    
    # output$CHIMEMinMax<-renderValueBox({
    #     MyCounties<-GetCounties()
    #     Peak<-CalculateCHIMEMinMax(MyCounties, input$Base, input$Radius, input$social_dist, input$proj_days)
    #     Peak<-format(Peak)
    #     valueBox(subtitle = "CHIME Predicted Peak Hospitalizations",
    #              paste(Peak),
    #              icon = icon("hospital"),
    #              color = "blue")
    # })
    
    output$IHMEPeakDate<-renderValueBox({
        MyHospitals<-GetHospitals()
        Peak<-CalculateIHMEPeak(input$Base, MyHospitals, input$Radius)
        Peak<-format(Peak)
        valueBox(subtitle = "IHME Predicted Peak Hospitalizations",
                 paste(Peak),
                 icon = icon("hospital"),
                 color = "navy")
    })
    
    
    # output$IHMEMinMax<-renderValueBox({
    #     MyHospitals<-GetHospitals()
    #     Peak<-CalculateIHMEMinMax(input$Base, MyHospitals, input$Radius)
    #     valueBox(subtitle = "IHME Predicted Min/Max Hospitalizations",
    #              paste(Peak),
    #              icon = icon("hospital"),
    #              color = "navy")
    # })
    # Output line plots for the dashboard ----------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Create first plot of local health population 
    output$LocalHealthPlot1<-renderPlotly({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        CovidCasesPerDayChart(input$Base, input$Radius, MyCounties,MyHospitals)
    })
    
    #Create second plot of local health population 
    output$LocalHealthPlot2<-renderPlotly({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        CovidCasesCumChart(input$Base, input$Radius, MyCounties, MyHospitals)
    })
    
    
    
    # Output Choropleth Charts ----------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Create Country Plot on Summary page
    output$SummaryPlot<-renderGvis({
        DF<-cbind.data.frame(CovidConfirmedCases$State, rev(CovidConfirmedCases)[,1])
        colnames(DF)<-c("state","Value")
        ChlorData<-plyr::ddply(DF, "state", numcolwise(sum))
        
        ChlorData<-ChlorData %>% 
            mutate(state_name = state.name[match(state, state.abb)])
        ChlorData<-ChlorData[complete.cases(ChlorData$state_name), ]
        states <- data.frame(ChlorData$state_name, ChlorData$Value)
        colnames(states)<-c("state_name","COVID-19 Cases")
        
        gvisGeoChart(states, "state_name", "COVID-19 Cases", 
                     options=list(region="US",
                                  colors="['#D3D3D3', 'red']",
                                  displayMode="regions", 
                                  resolution="provinces",
                                  width=1200,
                                  height = 600))
    })
    
    
    #Creates the local choropleth charts that change based on which base and radius.
    output$LocalChoroPlot<-renderPlotly({
        MyCounties<-GetCounties()
        PlotLocalChoro(MyCounties, input$Base, input$TypeLocal)
    })
    
    
    
    
    # Output Projections  ---------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Create IHME plot by State projected hospitalization 
    output$IHME_State_Hosp<-renderPlotly({
        
        #Creating the stats and dataframes determined by the base we choose to look at.
        BaseState<-dplyr::filter(AFBaseLocations, Base == input$Base)
        IncludedHospitals<-GetHospitals()
        IHME_State <- dplyr::filter(IHME_Model, State == toString(BaseState$State[1]))
        TotalBedsCounty <- sum(IncludedHospitals$BEDS)

        #Get regional and state populations
        MyCounties <- GetCounties()
        CovidCounties<-subset(CovidConfirmedCases, CountyFIPS %in% MyCounties$FIPS)
        HistoricalData<-colSums(CovidCounties[,5:length(CovidCounties)])
        HistoricalDates<-seq(as.Date("2020-01-22"), length=length(HistoricalData), by="1 day")
        HistoricalData<-data.frame(HistoricalDates, HistoricalData*.21, HistoricalData*.15, HistoricalData*.27)
        colnames(HistoricalData)<-c("ForecastDate", "Expected Hospitalizations", "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
        
        
        StPopList <- dplyr::filter(CountyInfo, State == toString(BaseState$State[1]))
        RegPop <- sum(MyCounties$Population)
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
        IHME_Region<-data.frame(IHME_Region$date, IHME_Region$allbed_mean, IHME_Region$allbed_lower, IHME_Region$allbed_upper)
        colnames(IHME_Region)<-c("ForecastDate", "Expected Hospitalizations", "Lower Bound Hospitalizations","Upper Bound Hospitalizations")
        IHME_Region<- dplyr::filter(IHME_Region, ForecastDate >= Sys.Date())
        
        IHME_Region<-rbind(HistoricalData,IHME_Region)
        IHME_Region$ForecastDate<-as.Date(IHME_Region$ForecastDate)

        r1 <- ggplot(data=IHME_Region, aes(x=ForecastDate, y=`Expected Hospitalizations`, ymin=`Lower Bound Hospitalizations`, ymax=`Upper Bound Hospitalizations`)) +
            geom_line(linetype = "dashed", size = 0.75) +
            geom_ribbon(alpha=0.3, fill = "cadetblue2") +
            # geom_hline(yintercept = TotalBedsCounty * 0.5,
            #            linetype = "solid",
            #            color = "red") +
            labs(title = paste("IHME Projected Daily Hospitalizations"),
                 x = "Date", y = "Daily Hospitalizations") +
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
            scale_x_date(date_breaks = "2 week")

        ggplotly(r1)
    })
    
    
    #Output the SEIAR CHIME projections with a max, min, and expected value
    output$SEIARProjection<-renderPlotly({
        BaseState<-dplyr::filter(AFBaseLocations, Base == input$Base)
        IncludedCounties<-GetCounties()
        CHIMELocalPlot(input$social_dist, input$proj_days, IncludedCounties)
    })
    
    output$CHIMENationalProj<-renderPlotly({
        
        CHIMENationalPlot(input$social_dist_national, input$proj_days_national)
    })
    
    output$NationalPlotOverlay<-renderPlotly({
        NationalOverlayPlot(input$social_dist_national, input$proj_days_national)
    })
    
    output$IHMENationaProj<-renderPlotly({
        
        IHMENationalProjections()
    })
    
    #Overlay Projected Plots
    output$OverlayPlots<-renderPlotly({
        MyCounties<-GetCounties()
        MyHospitals<-GetHospitals()
        PlotOverlay(input$Base, MyCounties, MyHospitals, input$social_dist, input$proj_days)
    })
    
    
    # Output any data tables ------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Render National Data Table on summary page
    output$NationalDataTable1<-DT::renderDataTable({
        NationalDataTable <- DT::datatable(data.frame(NationalDataTable),rownames = FALSE, options = list(dom = 'ft',ordering = F,"pageLength" = 51))
        NationalDataTable
    })
    
    output$CountyDataTable1<-DT::renderDataTable({
        MyCounties<-GetCounties()
        dt<-GetLocalDataTable(MyCounties)
        dt<-DT::datatable(dt, rownames = FALSE, options = list(dom = 't',ordering = F, "pageLength"=100))
        dt
    })
    
    
    
    # Output Report ------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    
    
    
    
    
    # Step Three
    ###################################################################################################################################################
    
    #Step three provides input information for annotation of the overall app such as inputs, sources, and calculations.
    observeEvent(input$overviewInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "OVERVIEW",
                OverviewLink)
        )
    })
    
    observeEvent(input$inputInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "USER INPUTS",
                InfoLink)
        )
    })
    
    observeEvent(input$calcInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "CALCULATIONS",
                CalcLink)
        )
    })
    observeEvent(input$sourceInfo, {
        showModal(
            modalDialog(
                size = "l",fade = TRUE, easyClose = TRUE, title = "SOURCES",
                SourceLink)
        )
    })

    # output$report <- downloadHandler(
    #     # For PDF output, change this to "report.pdf"
    #     filename = "report.html",
    #     content = function(file) {
    #         # Copy the report file to a temporary directory before processing it, in
    #         # case we don't have write permissions to the current working dir (which
    #         # can happen when deployed).
    #         tempReport <- file.path(tempdir(), "TestReport.Rmd")
    #         file.copy("TestReport.Rmd", tempReport, overwrite = TRUE)
    #         
    #         # Set up parameters to pass to Rmd document
    #         params <- list(radius = input$Radius,
    #                        base = input$Base,
    #                        pjDays = input$proj_days,
    #                        socDis = input$social_dist)
    #         
    #         # Knit the document, passing in the `params` list, and eval it in a
    #         # child of the global environment (this isolates the code in the document
    #         # from the code in this app).
    #         rmarkdown::render(tempReport, output_file = file,
    #                           params = params,
    #                           envir = new.env(parent = globalenv())
    #         )
    #     }
    # )
    
    
    
    
    
}
