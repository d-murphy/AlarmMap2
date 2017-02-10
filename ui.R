
# Add 3 other graphs
# Add action button for Location
# Connect Location Zoom
# Update text for counts
# Make colors consistent accross all graphs
# Make option to turn off colors
# Make filter for alarm types

# Fix size of center window so that 3rd column can't appear on top of it


library(shiny)

navbarPage("CIFD Alarm Data Dashboard",
           tabPanel("Controls",
                    sidebarLayout(
                      sidebarPanel(  
                        
                        wellPanel(style = "background-color: #ffffff;",
                                  em(textOutput("alarmCoverage")),br(),
                                  passwordInput('password', 
                                                label = "Password", 
                                                value = ""),  br(),
                                  
                                  
                                  
                                  
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
                                                                selected = c("2","3","4","5","6","7","1")
                                             )
                                             
                                    )
                                  )
                                  
                        )  #closes wellPanel
                      ), #closes column
                      mainPanel(
                        fluidRow(column(3,
                                        br(),
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
                                        textOutput("MVAct"),br(),br(),
                                        
                                        h4(textOutput("FreqTitle")),
                                        textOutput("TenPct"),
                                        textOutput("QtrPct"),
                                        textOutput("HlfPct"),
                                        textOutput("ThreeQrtsPct"),
                                        textOutput("AllPct"), br(),br()
                                        
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
                        ),
                        column(9,
                               wellPanel(plotOutput("timeHist",height = 150)),
                               wellPanel(plotOutput("freqHist",height = 150)),
                               wellPanel(plotOutput("wdayHist", height = 150)),
                               wellPanel(plotOutput("MonthHist",height = 150)),
                               wellPanel(plotOutput("arrHist", height = 150))
                               
                        )
                        )
                        
                      )
                    ) 
           ),
           tabPanel("Map",
                    sidebarLayout(
                      sidebarPanel(
                        
                        wellPanel(style = "background-color: #ffffff;",
                                  textInput('Map Center', 
                                            label = "Location (City Name or Coordinates)", 
                                            value = "Central Islip, NY")
                        ),
                        
                        wellPanel(style = "background-color: white;
                                  font-family: 'arial';
                                  font-size: 8", 
                                  
                                  htmlOutput("alarmSelected")) 
                      ),
                      mainPanel(
                        
                        leafletOutput("map")  
                        
                      ))
           )
           )    #closes navBarPage

#  fluidRow(  #main section row 
#       
#    column(3, br(), # first column








#### Code used to allow data upload
# fileInput('fileUpload',"Upload Run Data as CSV file",
#          accept=c(".csv",".xlsx"))