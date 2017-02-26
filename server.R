

library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(stringr)
library(leaflet)
library(ggthemes)

filePath <- "MapDownload.csv"
df <- read.csv(filePath, stringsAsFactors = FALSE)     
df$Date <- mdy(df$Date)
df$Time <- hms(df$Time)
df$Time <- hour(df$Time)
df$Password <- "CIP"
df$Weekday <- wday(df$Date)
df$Text <- paste0(month(df$Date),"/",day(df$Date),"/",year(df$Date)," - ",df$Time,"00 hours:  ",
                  df$SignalType, " at ",tools::toTitleCase(tolower(df$Location)))

df$SignalTypeAbbr <- 0


for(i in 1:dim(df)[1]){
  
  if(grepl("9",df$SignalType[i])){
    df$SignalTypeAbbr[i] <- "Miscellaneous"
  }
  if(grepl("12",df$SignalType[i])){
    df$SignalTypeAbbr[i] <- "Brush Fire"
  }
  if(grepl("13",df$SignalType[i])){
    df$SignalTypeAbbr[i] <- "Structure"
  }
  if(grepl("13-A",df$SignalType[i])){
    df$SignalTypeAbbr[i] <- "Automatic Alarm"
  }
  if(grepl("13-35",df$SignalType[i])){
    df$SignalTypeAbbr[i] <- "Working Fire"
  }
  if(grepl("14",df$SignalType[i])){
    df$SignalTypeAbbr[i] <- "Car Fire"
  }
  if(grepl("23",df$SignalType[i])){
    df$SignalTypeAbbr[i] <- "Miscellaneous"
  }
  if(grepl("MVA",df$SignalType[i])){
    df$SignalTypeAbbr[i] <- "MVA"
  }
  if(grepl("24",df$SignalType[i])){
    df$SignalTypeAbbr[i] <- "Mutual Aid"
  }
  if(grepl("marshal",tolower(df$SignalType[i]))){
    df$SignalTypeAbbr[i] <- "Fire Marshal Investigation"
  }
  
}


resdf <- read.csv("AggResponseData.csv", stringsAsFactors = FALSE)


shinyServer(function(input, output) {
  
  
  data <- reactive({
    
    df <- df %>% filter(Date >= ymd(input$dates[1]) &
                          Date <= ymd(input$dates[2]) &
                          Time >= input$hours[1] &
                          Time <= input$hours[2] &
                          Password == input$password &
                          SignalTypeAbbr %in% input$alarmFilter &
                          Weekday %in% input$wdayFilter) %>% 
      arrange(Date,Time) %>% 
      mutate(HourDiff = ((as.numeric(Date) - as.numeric(lag(Date)))/3600 + Time - lag(Time)))
    
  })
  
  RspData <- reactive({
    
    temp <- data()
    
    resdf <- resdf %>% filter(RunNumber %in% temp$RunNumber)
    
  })
  
  
  # The counts below are used in the left column    
  
  BrFct <- reactive({ data() %>% filter(SignalTypeAbbr == "Brush Fire") %>% summarize(ct = n()) })
  Strct <- reactive({ data() %>% filter(SignalTypeAbbr == "Structure") %>% summarize(ct = n()) })
  WFct <- reactive({ data() %>% filter(SignalTypeAbbr == "Working Fire") %>% summarize(ct = n()) })
  AAct <- reactive({ data() %>% filter(SignalTypeAbbr == "Automatic Alarm") %>% summarize(ct = n()) })
  CFct <- reactive({ data() %>% filter(SignalTypeAbbr == "Car Fire") %>% summarize(ct = n()) })
  Miscct <- reactive({ data() %>% filter(SignalTypeAbbr == "Miscellaneous") %>% summarize(ct = n()) })
  MVAct <- reactive({ data() %>% filter(SignalTypeAbbr == "MVA") %>% summarize(ct = n()) })
  MAct <- reactive({ data() %>% filter(SignalTypeAbbr == "Mutual Aid") %>% summarize(ct = n()) })
  FMIct <- reactive({ data() %>% filter(SignalTypeAbbr == "Fire Marshal Investigation") %>% summarize(ct = n()) })
  Totct <- reactive({ data() %>% summarize(ct = n()) })
  
  #    JanCt <- reactive({ data() %>% filter(month(Date) == 1) %>% summarize(ct = n()) })    
  #    FebCt <- reactive({ data() %>% filter(month(Date) == 2) %>% summarize(ct = n()) })    
  #    MarCt <- reactive({ data() %>% filter(month(Date) == 3) %>% summarize(ct = n()) })    
  #    AprCt <- reactive({ data() %>% filter(month(Date) == 4) %>% summarize(ct = n()) })    
  #    MayCt <- reactive({ data() %>% filter(month(Date) == 5) %>% summarize(ct = n()) })    
  #    JunCt <- reactive({ data() %>% filter(month(Date) == 6) %>% summarize(ct = n()) })    
  #    JulCt <- reactive({ data() %>% filter(month(Date) == 7) %>% summarize(ct = n()) })    
  #    AugCt <- reactive({ data() %>% filter(month(Date) == 8) %>% summarize(ct = n()) })    
  #    SepCt <- reactive({ data() %>% filter(month(Date) == 9) %>% summarize(ct = n()) })    
  #    OctCt <- reactive({ data() %>% filter(month(Date) == 10) %>% summarize(ct = n()) })    
  #    NovCt <- reactive({ data() %>% filter(month(Date) == 11) %>% summarize(ct = n()) })    
  #    DecCt <- reactive({ data() %>% filter(month(Date) == 12) %>% summarize(ct = n()) })    
  
  TenPct <- reactive({ data() %>% filter(is.na(HourDiff)==FALSE) %>% 
      summarise('10%' = round(quantile(HourDiff, probs = c(0.1))[[1]])) })
  QtrPct <- reactive({ data() %>% filter(is.na(HourDiff)==FALSE) %>% 
      summarise('25%' = round(quantile(HourDiff, probs = c(0.25))[[1]])) })
  HlfPct <- reactive({ data() %>% filter(is.na(HourDiff)==FALSE) %>% 
      summarise('50%' = round(quantile(HourDiff, probs = c(0.5))[[1]])) })
  ThreeQrtsPct <- reactive({ data() %>% filter(is.na(HourDiff)==FALSE) %>% 
      summarise('75%' = round(quantile(HourDiff, probs = c(0.75))[[1]])) })
  AllPct <- reactive({ data() %>% filter(is.na(HourDiff)==FALSE) %>% 
      summarise('100%' = round(quantile(HourDiff, probs = c(1))[[1]])) })
  
  
  
  
  output$map <- renderLeaflet({
    
    leaflet() %>% addTiles() %>% fitBounds(-73.262117, 40.743767, -73.140066, 40.820066)
    
  })
  
  observe({
    
    leafletProxy("map", data = data()) %>% 
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addMarkers(clusterOptions = markerClusterOptions(),
                 popup = ~as.character(Text)) 
                                         
  })
  
  
  output$timeHist <- renderPlot({
    
    ggplot(data(),aes(x=Time)) +
      geom_bar(fill = "light blue",width = 1)   + 
      ggtitle("Count of Runs for Hour of Day") + 
      xlab("Time of Day") + 
      ylab("Count") + 
    #  xlim(0,23) +
      scale_x_continuous(limits = c(0,23), breaks=seq(0,23,4))+
      theme_hc(bgcolor = "darkunica")
  })
  
  output$freqHist <- renderPlot({
    
    ggplot(data(),aes(x=HourDiff)) +
      geom_histogram(fill = "light green", bins = 15) + 
      ggtitle("Run Frequency") + 
      xlab("Hours since Last Run") + 
      ylab("Count")  +
      theme_hc(bgcolor = "darkunica")
    
  })
  
  output$resHist <- renderPlot({
    
    ggplot(RspData(),aes(x=AT)) +
      geom_histogram(fill = "yellow", bins = 15) + 
      ggtitle("First Apparatus Arrival") + 
      xlab("Seconds into Alarm") + 
      ylab("Count")  +
      theme_hc(bgcolor = "darkunica")
    
  })
  
  

  
  output$Totct <- renderText({ if(input$password != df$Password[1]) 
  {""} else 
  {paste("Total Alarm Count: ", Totct())}
  })
  output$AAct <- renderText({ if(AAct() >0) { paste("Automatic Alarm: ", AAct()) } })
  output$BrFct <- renderText({ if(BrFct() >0) { paste("Brush Fire: ", BrFct()) } })
  output$CFct <- renderText({ if(CFct() >0) { paste("Car Fire: ", CFct()) } })    
  output$FMIct <- renderText({ if(FMIct() >0) { paste("Fire Marshal Investigation: ", FMIct()) } })
  output$Strct <- renderText({ if(Strct() >0) { paste("Structure: ", Strct()) } })
  output$WFct <- renderText({ if(WFct() >0) { paste("Working Fire: ", WFct()) } })
  output$Miscct <- renderText({ if(Miscct() >0) { paste("Miscellaneous: ", Miscct()) } })
  output$MAct <- renderText({ if(MAct() >0) { paste("Mutual Aid: ", MAct()) } })
  output$MVAct <- renderText({ if(MVAct() >0) { paste("MVA: ", MVAct()) } })
  
  #    output$MthCt <- renderText({ if(input$password != df$Password[1])
  #                                    {""} else
  #                                        {paste("Monthly Alarm Totals: ")}
  #                              })
  #    output$JanCt <- renderText({ if(JanCt() > 0 ) {paste("January: ", JanCt()) } })
  #    output$FebCt <- renderText({ if(FebCt() > 0 ) {paste("February: ", FebCt()) } })
  #    output$MarCt <- renderText({ if(MarCt() > 0 ) {paste("March: ", MarCt()) } })
  #    output$AprCt <- renderText({ if(AprCt() > 0 ) {paste("April: ", AprCt()) } })
  #    output$MayCt <- renderText({ if(MayCt() > 0 ) {paste("May: ", MayCt()) } })
  #    output$JunCt <- renderText({ if(JunCt() > 0 ) {paste("June: ", JunCt()) } })
  #    output$JulCt <- renderText({ if(JulCt() > 0 ) {paste("July: ", JulCt()) } })
  #    output$AugCt <- renderText({ if(AugCt() > 0 ) {paste("August: ", AugCt()) } })
  #    output$SepCt <- renderText({ if(SepCt() > 0 ) {paste("September: ", SepCt()) } })
  #    output$OctCt <- renderText({ if(OctCt() > 0 ) {paste("October: ", OctCt()) } })
  #    output$NovCt <- renderText({ if(NovCt() > 0 ) {paste("November: ", NovCt()) } })
  #    output$DecCt <- renderText({ if(DecCt() > 0 ) {paste("December: ", DecCt()) } })
  
  output$FreqTitle <- renderText({ if(input$password != df$Password[1])
  {""} else
  {paste("Alarm Frequency: ")} })
  
  
  output$TenPct <- renderText({ if(input$password != df$Password[1]) 
  {""} else {paste("10%: ", TenPct()," hours")} }) 
  
  output$QtrPct <- renderText({ if(input$password != df$Password[1]) 
  {""} else {paste("25%: ", QtrPct()," hours")} })
  output$HlfPct <- renderText({ if(input$password != df$Password[1]) 
  {""} else {paste("50%: ", HlfPct()," hours")} })
  output$ThreeQrtsPct <- renderText({ if(input$password != df$Password[1]) 
  {""} else {paste("75%: ", ThreeQrtsPct()," hours")} })
  output$AllPct <- renderText({ if(input$password != df$Password[1]) 
  {""} else {paste("100%: ", AllPct()," hours")} })
  
  
  
  output$alarmTypeSubtitle <- renderText({
    
    if(input$password != df$Password[1]) {
      ""
    } else { "Alarm Type Totals"
    }})
  
  
  
  
  output$alarmCoverage <- renderText({
    
    if(input$password != df$Password[1]) {
      "Please enter the password"
    } else {
      paste("Dataset includes alarms from ",
            min(df$Date),"to ",max(df$Date),".")
    }
    
  })
  
  

})  #closes Shiny Server



#### Code used if upload widget is included.  

#      inFile <- input$fileUpload

#      if(is.null(inFile))
#        return(NULL)

#      df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)



