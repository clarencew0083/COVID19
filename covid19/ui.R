##########################
##### User Interface #####
##########################

# Layout
##############################################################################################################################################
# The User Interface generates the visual of the application. It establishes location and layout of all outputs and inputs from server and user
# First:  The dashboard header shows the main title and introduction to the application
# Second: The sidebar shows all inputs that the user can change
# Third:  The body provides all visual outputs, statistics, and charts. It is updated every time a user changes the inputs
##############################################################################################################################################     




# Begin User Interface ------------------------------------------------------------------------------------------------------------------------------------------------------------



#Build UI
#Establishes the layout of the overall dashboard and how items are displayed
ui <- tagList(
<<<<<<< HEAD
    dashboardPage(
        dashboardHeader(title = "COVID-19 Risk Dashboard",
                        titleWidth = 300,
                        dropdownMenu(
                            headerText = "Want to know more?",
                            icon = icon("info-circle"),
                            tags$li(actionLink("inputInfo", label = "User Inputs", icon = icon("sliders-h")),
                                    class = "dropdown"),
                            tags$li(actionLink("calcInfo", label = "Calculations", icon = icon("calculator")),
                                    class = "dropdown"),
                            tags$li(actionLink("sourceInfo", label = "Sources", icon = icon("user-secret")),
                                    class = "dropdown")
                        )
        ),
        dashboardSidebar(width = 300, 
                         sidebarMenu(
                             selectInput("Base",
                                         "Choose your base:", 
                                         choices = unique(BaseList),
                                         selectize = FALSE),
                             sliderInput("Radius",
                                         "Choose your local radius (miles):",
                                         min = 10,
                                         max = 100,
                                         value = 25),
                             br(),
                             menuItem(
                                 "Extra Inputs",
                                 tabName = "dashboard",
                                 icon = icon("sliders-h"),
                                 div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                 sliderInput("proj_days",
                                             "Projection days:",
                                             min = 7,
                                             max = 90,
                                             value = 14),
                                 sliderInput("social_dist",
                                             "% Social distancing in your area:",
                                             min = 0,
                                             max = 100,
                                             value = 60)),
                             br(),
                             actionButton("refresh", "Refresh", width = "90%"),
                             hr())),
        
        dashboardBody(
            tags$head(tags$style(HTML(
                '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
        }
        '))),
            tags$script(HTML('
                       $(document).ready(function() {
                       $("header").find("nav").append(\'<span class="myClass"> Tailored Risk Assesments </span>\');
                       })
                       ')),
            fluidRow(
                valueBox("LOW RISK", subtitle ="Mission Risk",color= "green",icon = icon("smile")),
                valueBox("MEDIUM RISK", subtitle ="Installation Health Risk",color= "yellow",icon = icon("meh")),
                valueBox("HIGH RISK", subtitle ="Local Health Risk",color= "red",icon = icon("frown"))
            ),
            
            tabsetPanel(id = "tabs",
                        ####### START OVERALL RISK TAB #######
                        tabPanel(
                            title = "Summary",
                            box(status = "primary", width = 13, solidHeader = T, htmlOutput("SummaryPlot"), align = "center")
                            
                        ),
                        ####### END OVERALL RISK TAB #######
                        ####### START MISSION RISK TAB #######
                        tabPanel(
                            title = "Mission",
                            value = plotOutput("plot")
                        ),
                        ####### END MISSION RISK TAB #######
                        ####### START INSTALLATION HEALTH RISK TAB #######
                        tabPanel(
                            title = "Installation Health", 
                            fluidRow(
                                # A static valueBox
                                valueBoxOutput("TotalPopulation"),
                                valueBox(2, subtitle ="Installation Specific Deaths", color= "red",icon = icon("skull")),
                                valueBox("85%", subtitle = "Installation Medical Utilization", color = "teal", icon = icon("hospital"))
                            ),
                            box(status = "primary", width = 13, solidHeader = T, "Current Risk Level: LOW ", align = "center"),
                            fluidRow( 
                                box(title = "Chart 1 Here", "Box content"),
                                box(title = "Chart 2 Here", "Box content")
                            )
                        ),
                        ####### END INSTALLATION HEALTH RISK TAB #######
                        ####### START LOCAL HEALTH RISK TAB #######
                        tabPanel(
                            title = "Local Health",
                            fluidRow(
                                # A static valueBox
                                valueBoxOutput("CovidCases"),
                                valueBoxOutput("LocalCovidDeaths"),
                                valueBoxOutput("HospitalUtilization")
                            ),
                            fluidRow( 
                                box(title = "Daily Impact",plotOutput("LocalHealthPlot1")),
                                box(title = "Total Impact",plotOutput("LocalHealthPlot2"))
                            )
                        )
                        ####### END LOCAL HEALTH RISK TAB #######
            )
        )
=======
    dashboardPage(skin = "black",title="COVID-19 Health Assessment Dashboard",
                  
                  # Step One - Header
                  ###################################################################################################################################################
                  dashboardHeader(title = div(img(src="AFIT_Emblem_Blue.png",height = '50',width = '110')),
                                  titleWidth = 300,
                                  dropdownMenu(
                                      headerText = "Want to know more?",
                                      icon = icon("info-circle"),
                                      tags$li(actionLink("overviewInfo", label = "Overview", icon = icon("globe")),
                                              class = "dropdown"),
                                      tags$li(actionLink("inputInfo", label = "User Inputs", icon = icon("sliders-h")),
                                              class = "dropdown"),
                                      tags$li(actionLink("projInfo", label = "Projections", icon = icon("chart-line")),
                                              class = "dropdown"),
                                      tags$li(actionLink("calcInfo", label = "Calculations", icon = icon("calculator")),
                                              class = "dropdown"),
                                      tags$li(actionLink("sourceInfo", label = "Sources", icon = icon("user-secret")),
                                              class = "dropdown")
                                  )
                  ),
                  
                  # Step Two - Sidebar
                  ###################################################################################################################################################
                  dashboardSidebar(width = 300,
                                   sidebarMenu(
                                       selectInput(
                                           "Base",
                                           "Choose your base:", 
                                           list(`Installation` = sort(BaseList) ), 
                                           selectize = FALSE),
                                       sliderInput("Radius",
                                                   "Choose your local radius (miles):",
                                                   min = 10,
                                                   max = 100,
                                                   value = 50),
                                       br(),
                                       menuItem(
                                           "Current Local Health Inputs",
                                           tabName = "dashboard",
                                           icon = icon("map-marker-alt"),
                                           div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                           radioButtons("TypeLocal", "State or County Plot:",
                                                        c("County"="County",
                                                          "State"="State"),)
                                       ),
                                       br(),
                                       menuItem(
                                           "Local Health Projection Inputs",
                                           tabName = "dashboard",
                                           icon = icon("sliders-h"),
                                           div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                           radioButtons("StatisticType", "Choose projected statistic:",
                                                        c("Hospitalizations"="Hospitalizations",
                                                          "Fatalities"="Fatalities")),
                                           sliderInput("proj_days",
                                                       "Projection days:",
                                                       min = 14,
                                                       max = 365,
                                                       value = 120),
                                           sliderInput("social_dist",
                                                       "% Social distancing in your area:",
                                                       min = 0,
                                                       max = 40,
                                                       value = 15)
                                       ),
                                       br(),
                                       menuItem(
                                           "National Health Projection Inputs",
                                           tabName = "dashboard",
                                           icon = icon("sliders-h"),
                                           div(id = "single", style="display: none;", numericInput("tckt", "Ticket Number : ", 12345,  width = 300)),
                                           sliderInput("proj_days_national",
                                                       "Projection days:",
                                                       min = 14,
                                                       max = 365,
                                                       value = 120),
                                           sliderInput("social_dist_national",
                                                       "% Social distancing in your area:",
                                                       min = 0,
                                                       max = 40,
                                                       value = 15)
                                       ),
                                       br()
                                       
                                       # div(style="text-align:center", tags$hr(style="border-color: #444;"), "Generate & Download Report:"),
                                       # br(),
                                       # fluidRow(
                                       #     downloadButton("report", "Generate Report", class = "butt"),
                                       #     tags$style(".skin-black .sidebar .butt{background-color:#15824d;color: white;border-color:white;}"),
                                       #     align = "center"
                                       # )

                                       # fluidRow(
                                       #     valueBox("LOW RISK", subtitle ="Mission Risk **notional ex.**",color= "green",width = 12)
                                       # ),
                                       # fluidRow(h()
                                       #     valueBox("MEDIUM RISK", subtitle ="Installation Health Risk **notional ex.**",color= "yellow", width = 12)
                                       # ),
                                       # fluidRow(
                                       #     valueBox("HIGH RISK", subtitle ="Local Health Risk **notional ex.**",color= "red",width = 12)
                                       # )
                                   )
                                   
                  ),
                  
                  
                  # Step Three - Body
                  ###################################################################################################################################################
                  dashboardBody(
                      tags$head(tags$style(HTML(
                          '.myClass { 
                    font-size: 20px;
                    line-height: 50px;
                    text-align: left;
                    font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                    padding: 0 15px;
                    overflow: hidden;
                    color: black;
                    }
                    '))),
                      tags$script(HTML('
                                   $(document).ready(function() {
                                   $("header").find("nav").append(\'<span class="myClass"> COVID-19 Health Assessment Dashboard v0.3</span>\');
                                   })
                                   ')),
                      
                      tabsetPanel(id = "tabs",
                                  
                                  # Summary Tab -------------------------------------------------------------
                                  tabPanel(
                                      title = "National Summary",
                                      
                                      box(title = "National Impact Map",solidHeader = T, align = "center", htmlOutput("SummaryPlot"),width = 13),
                                      
                                      box(title = "National Statistics", solidHeader=T, align = "left", column(width = 12, DT::dataTableOutput("NationalDataTable1"), style = "height:240px;overflow-y: scroll;overflow-x:scroll"),width = 13)
                                      
                                  ),
                                  ####### END SUMMARY TAB #######
                                  
                                  
                                  # Current Local Health ----------------------------------------------------
                                  tabPanel(
                                      title = "Current Local Health",
                                      fluidRow(
                                          # A static valueBox
                                          valueBoxOutput("CovidCases"),
                                          valueBoxOutput("LocalCovidDeaths"),
                                          valueBoxOutput("HospitalUtilization")
                                      ),
                                      fluidRow(
                                          valueBoxOutput("CaseChangeLocal", width = 4),
                                          valueBoxOutput("DeathChangeLocal", width = 4),
                                          valueBoxOutput("HospUtlzChange", width = 4)
                                      ),
                                      fluidRow( 
                                          box(title = "Daily Reports",plotlyOutput("LocalHealthPlot1",height = 300)),
                                          box(title = "Total Reports",plotlyOutput("LocalHealthPlot2",height = 300))
                                      ),
                                      fluidRow(
                                          box(title = "Local Impact Map", plotlyOutput("LocalChoroPlot", height = 250),height = 300),
                                          box(title = "Local County Statistics", solidHeader=T, align = "left", column(width = 12, DT::dataTableOutput("CountyDataTable1"), style = "height:240px;overflow-y: scroll"), height = 300)
                                      )
                                  ),
                                  ####### END CURRENT LOCAL HEALTH TAB #######
                                  
                                  ####### BEGIN LOCAL PROJECTION TAB #########
                                  # Local Health Projections ------------------------------------------------
                                  tabPanel(
                                      title = "Local Health Projections",
                                      fluidRow(
                                          valueBoxOutput("TotalPopulation"),
                                          valueBoxOutput("IHMEPeakDate"),
                                          valueBoxOutput("CHIMEPeakDate"),
                                          #valueBoxOutput("TotalPopulation"),
                                          #valueBoxOutput("IHMEMinMax"),
                                          #valueBoxOutput("CHIMEMinMax")
                                          
                                      ),
                                      fluidRow(
                                          box(plotlyOutput("IHME_State_Hosp",height = 400)),
                                          box(plotlyOutput("SEIARProjection"),height = 400)),
                                          box(plotlyOutput("OverlayPlots"), width =  900)
                                  ),
                                  ####### END PROJECTION TAB #######
                                  
                                  ####### BEGIN National PROJECTION TAB #########
                                  # Local Health Projections ------------------------------------------------
                                  tabPanel(
                                      title = "National Health Projections",
                                      # fluidRow(
                                      #     valueBoxOutput("TotalPopulation_National"),
                                      #     valueBoxOutput("CHIMEPeakDate_National"),
                                      #     valueBoxOutput("IHMEPeakDate_National")
                                      #),
                                      fluidRow(
                                          box(plotlyOutput("IHMENationaProj",height = 400)),
                                          box(plotlyOutput("CHIMENationalProj"),height = 400)),
                                      box(plotlyOutput("NationalPlotOverlay"), width =  900)
                                  ),
                                  ####### END PROJECTION TAB #######
                                  
                                  ####### BEGIN SUMMARY TAB #########
                                  # Mission Risk ------------------------------------------------------------
                                  tabPanel(
                                      title = "Summary",
                                      box(title = "Projected Daily Hospitalizations",
                                          solidHeader=T, 
                                          align = "left", 
                                          column(width = 12, 
                                                 DT::dataTableOutput("ForecastDataTable"), 
                                                 style = "height:720px;overflow-y: scroll"), 
                                          height = 900, 
                                          width =13,
                                          downloadButton('downloadData', 'Download data'))
                                      )

                                  ####### END Mission Risk #######
                                  
                                  
                      )
                  )
>>>>>>> f6685044121c177de7bfe32fe6caea3abcf6c470
    ),
    
    #The footer is just showing the information on the main contributors to the app
    tags$footer("created by Nick Forrest, Trey Pujats, Garrett Alarcon, James Deitschel", align = "center", style = "
              position:absolute;
              bottom:50;
              width:100%;
              height:25px;   /* Height of the footer */
              color: grey;
              padding: 0px;
              background-color: transparent;
              z-index: 1000;")
)
#Close UI 
