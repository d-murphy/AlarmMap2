
library(shinythemes)
library(shiny)
library(leaflet)


fluidPage(theme = shinytheme("darkly"),
          
          tags$style(HTML("
            body {
              color: #898989;
            }
          ")),

navbarPage("CIFD Alarm Data Dashboard",
           tabPanel("Map",
                    sidebarLayout(
                      sidebarPanel(  
                        
                        wellPanel(
                                  em(textOutput("alarmCoverage")),br(),
                                  passwordInput('password', 
                                                label = "Password", 
                                                value = ""),  br(),
                          
                                        
                                                  
                                                 
                                                  
                                                  # Turned off month count                                  
                                                  
                                                  #                                  h4(textOutput("MthCt")),
                                                  #                                  textOutput("JanCt"),
                                                  #                                  textOutput("FebCt"),
                                                  #                                  textOutput("MarCt"),
                                                  #                                  textOutput("AprCt"),
                                                  #                                  textOutput("MayCt"),
                                                  #                                  textOutput("JunCt"),
                                                  #                                  textOutput("JulCt"),
                                                  #                                  textOutput("AugCt"),
                                                  #                                  textOutput("SepCt"),
                                                  #                                  textOutput("OctCt"),
                                                  #                                  textOutput("NovCt"),
                                                  #                                  textOutput("DecCt")
                                  
                                  
                                  
                                  dateRangeInput('dates',
                                                 label = "Enter Date Range (YYYY-MM-DD)",
                                                 start = "2015-01-01",
                                                 end = "2015-12-31"),
                                  
                                  sliderInput('hours',
                                              label = "Enter Time Range (Hours)",
                                              min = 0,
                                              max = 23,
                                              value = c(0,23),
                                              animate = TRUE),
                                  fluidRow(
                                    column(6,
                                           checkboxGroupInput("alarmFilter", 
                                                              label = "Alarm Type", 
                                                              choices = list("Brush Fire" = "Brush Fire",
                                                                             "Structure"  = "Structure",
                                                                             "Working Fire" = "Working Fire",
                                                                             "Automatic Alarm" = "Automatic Alarm",
                                                                             "Car Fire"   = "Car Fire",
                                                                             "Miscellaneous" = "Miscellaneous",
                                                                             "MVA"        = "MVA",
                                                                             "Mutual Aid" = "Mutual Aid",
                                                                             "Fire Marshal Investigation" = "Fire Marshal Investigation"),
                                                              selected = c("Brush Fire", "Structure", "Working Fire",
                                                                           "Automatic Alarm", "Car Fire", "Miscellaneous",
                                                                           "MVA", "Mutual Aid")
                                           )
                                    ),column(6,
                                             
                                             checkboxGroupInput("wdayFilter", 
                                                                label = "Day of Week", 
                                                                choices = list("Monday" = "2",
                                                                               "Tuesday"  = "3",
                                                                               "Wednesday" = "4",
                                                                               "Thursday" = "5",
                                                                               "Friday"   = "6",
                                                                               "Saturday" = "7",
                                                                               "Sunday" = "1"),
                                                                selected = c("2","3","4","5","6","7","1")),
                                             
                                             br(),br(),
                                             checkboxInput("respOn",label = "Turn on Arrival Time Filter?",value = FALSE),
                                             sliderInput('respRange',
                                                         label = "First Apparatus Arrival Time (seconds)",
                                                         min = 0,
                                                         max = 1800,
                                                         value = c(0,1800),
                                                         animate = TRUE)
                                                                
                                                                                 
                                              #
#                                               sliderInput('respRange',
#                                                           label = "First Apparatus Arrival Time (seconds)",
#                                                           min = 0,
#                                                           max = 1800,
#                                                           value = c(0,1800),
#                                                           animate = TRUE)
#                                                                 
                                             
                                             
                                    )
                                  )
                                  
                        )  #closes wellPanel
                      ), #closes sidebarPanel
                      mainPanel(
                        
                        wellPanel(fluidRow(leafletOutput("map"))),
                        br(),
                        fluidRow(
                          column(3, plotOutput("timeHist")),
                          column(3, plotOutput("freqHist")),
                          column(3, plotOutput("resHist")),
                          column(3, 
                            
                            h4(textOutput("FreqTitle")),
                            textOutput("TenPct"),
                            textOutput("QtrPct"),
                            textOutput("HlfPct"),
                            textOutput("ThreeQrtsPct"),
                            textOutput("AllPct"), br(),br(),
                          
                            h4(textOutput("alarmTypeSubtitle")),
                            textOutput("Totct"),br(),
                            textOutput("AAct"), 
                            textOutput("BrFct"), 
                            textOutput("CFct"), 
                            textOutput("FMIct"), 
                            textOutput("Strct"),
                            textOutput("WFct"), 
                            textOutput("Miscct"), 
                            textOutput("MAct"),
                            textOutput("MVAct")
                            
                          )
                                 
                        )
                               
                        )
                        )),
           
            tabPanel("Apparatus Response",
                     column(3,plotOutput("Eng1")),
                     column(3,plotOutput("Eng2")),
                     column(3,plotOutput("Eng3")),
                     column(3,plotOutput("Eng4"))
                     ),
            tabPanel("About",
                        
                          p("Some text later on.")
                        
                      )
           
           )    #closes navBarPage

  ) #closes fluidPage

#  fluidRow(  #main section row 
#       
#    column(3, br(), # first column








#### Code used to allow data upload
# fileInput('fileUpload',"Upload Run Data as CSV file",
#          accept=c(".csv",".xlsx"))