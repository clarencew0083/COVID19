ui <- tagList(
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
    ),
    tags$footer("created by the 4 AFITeers", align = "center", style = "
              position:absolute;
              bottom:50;
              width:100%;
              height:25px;   /* Height of the footer */
              color: grey;
              padding: 0px;
              background-color: transparent;
              z-index: 1000;")
)
