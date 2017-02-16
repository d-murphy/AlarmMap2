
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(dplyr)
library(ggmap)
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




### need to create reactive set.  
### need to create data filter
### need to create interface.  Its really good though.  

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


shinyServer(function(input, output) {
  
  
  data <- reactive({
    
    ### try to figure out how to do this with      
    #      brushdf <- brushedPoints(df , input$plot_brush,  
    #                           xvar = "lon", yvar = "lat")
    
    
    #          if(is.na(brushdf[1,1])){
    
    df <- df %>% filter(Date >= ymd(input$dates[1]) &
                          Date <= ymd(input$dates[2]) &
                          Time >= input$hours[1] &
                          Time <= input$hours[2] &
                          Password == input$password &
                          SignalTypeAbbr %in% input$alarmFilter &
                          Weekday %in% input$wdayFilter) %>% 
      arrange(Date,Time) %>% 
      mutate(HourDiff = ((as.numeric(Date) - as.numeric(lag(Date)))/3600 + Time - lag(Time)))
    
    #          }else{
    
    #         df <- df %>% filter(Date >= ymd(input$dates[1]) &
    #                               Date <= ymd(input$dates[2]) &
    #                               Time >= input$hours[1] &
    #                               Time <= input$hours[2] &
    #                               Password == input$password &
    #                               SignalTypeAbbr %in% input$alarmFilter &
    #                               Weekday %in% input$wdayFilter &
    #                               RunNumber %in% brushdf$RunNumber) %>% 
    #                           arrange(Date,Time) %>% 
    #                           mutate(HourDiff = ((as.numeric(Date) - as.numeric(lag(Date)))/3600 + Time - lag(Time)))
    #         
    #                 }       
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
    
    leaflet() %>% addTiles() 
    
  })
  
  observe({
    
    leafletProxy("map", data = data()) %>% 
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addMarkers(clusterOptions = markerClusterOptions(),
                 popup = ~as.character(Text))#," at ",tools::toTitleCase(tolower(df$Location)))))
                                     #  month(Date),"/",day(Date),"/",year(Date),", ",Time,"00 hours:  ",       
  })
  
  
  output$timeHist <- renderPlot({
    
    ggplot(data(),aes(x=Time)) +
      geom_bar(fill = "light blue",width = 1)   + 
      ggtitle("Count of Runs for Hour of Day") + 
      xlab("Time of Day") + 
      ylab("Count") + 
      xlim(0,23) + 
      theme_hc(bgcolor = "darkunica")
  })
  
  output$freqHist <- renderPlot({
    
    ggplot(data(),aes(x=HourDiff)) +
      geom_histogram(fill = "light green", binwidth = 2) + 
      ggtitle("Run Frequency") + 
      xlab("Hours since Last Run") + 
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
  
  output$alarmSelected <- renderUI({
    
    ReturnStr <- as.character()  
    
    tempdf <- nearPoints(data() , input$plot_click,  
                         xvar = "lon", yvar = "lat")
    
    if(is.na(tempdf[1,1])){
      HTML(paste0("Click a point on the map to see Alarm Info."))
    } else {
      
      for(i in 1:dim(tempdf)[1]){
        
        text1 <- paste0("Run Number:  ",tempdf$RunNumber[i])
        text2 <- paste0("Alarm Type:  ", tempdf$SignalTypeAbbr[i])
        text3 <- paste0("Date:  ", month(tempdf$Date[i]),"/",day(tempdf$Date[i]),"/",year(tempdf$Date[i]))
        text4 <- paste0("Hour of Day: ", tempdf$Time[i])
        text5 <- paste0("Address:  ", tempdf$Location[i])
        text6 <- paste0("")
        
        NewStr <- as.character()
        NewStr <- paste(text1, text5, text2, text3, text4, text6, sep="</br>")
        
        ReturnStr <- paste(ReturnStr, NewStr, sep="</br>")
        
      }
      
      
      HTML(ReturnStr)
      
    }
    
  })
  
  
  #     output$alarmSelected <- renderPrint({
  #       
  #       tempdf <- nearPoints(data() , input$plot_click,  
  #                  xvar = "lon", yvar = "lat")
  #       
  #       if(is.null(tempdf)){
  #         cat("", "\n")
  #       } else {
  #       
  #         ReturnStr <- as.character()
  #       
  #         for(i in 1:dim(tempdf)[1]){
  #       
  #         ReturnStr <- paste0(ReturnStr,
  #             cat("Run Number:  ",tempdf$RunNumber[i],  
  #                 "\rAlarm Type:  ", tempdf$SignalTypeAbbr[i], " - " ,
  #                 month(tempdf$Date[i]),"/",day(tempdf$Date[i]),"/",year(tempdf$Date[i]), " - ",
  #                 "Hour:  ", tempdf$Time[i]
  #                 ))
  #         
  #               }
  # 
  #         cat(ReturnStr, "\n")
  #       
  #       }
  #     })
  
})  #closes Shiny Server



#### Code used if upload widget is included.  

#      inFile <- input$fileUpload

#      if(is.null(inFile))
#        return(NULL)

#      df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)

###  This step shortened the dataset, if dataset needed lon lat
#df <- df[1:10,]

### This section adds lon/lat if missing, or checks that it is not NA and adds

#if(ncol(df)==5){
#  df <- cbind(df, geocode(df$Location))
#} else {

#  for(i in 1:dim(df)[1]){

#    if(is.na(df$lon[i]) | is.na(df$lat[i]) ){
#      returnDF <- geocode(df$Location[i])
#      df$lon[i] <- returnDF$lon[1]
#      df$lat[i] <- returnDF$lat[1]
#    }

#  }
# }

### This step writes the downloaded lon/lat data so it doesn't need to be retrieved again
#    write.csv(df, file = "C:/Users/murph/Desktop/MapDownload.csv",row.names = FALSE)




### Code used to create the Aggregate Response Data

# resdf <- read.csv("ResponseData.csv", stringsAsFactors = FALSE)
# resdf <- resdf %>% select(Unit, Run, Response...2.Pr, Response...21.A)
# colnames(resdf) <- c("Unit", "RunNumber","DepartureTime","ArrivalTime")
# 
# resdf <- resdf %>% filter(grepl("3/7",Unit))
# resdf$Unit <- str_sub(resdf$Unit, -2,-1)
# resdf$Aggr <- "Actual"
# 
# #Frontline Units
# 
# resSum <- resdf %>% filter(Unit %in% c("01","02","03","04","05","06","07","08","09","15")) %>% group_by(RunNumber) %>% 
#   mutate(DeptRank = min_rank(DepartureTime))
# resSum$Aggr <- "FrontlineUnitsBestDeparture"
# 
# resSum <- resSum %>% filter(DeptRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- data.frame()
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# #Chiefs
# 
# resSum <- resdf %>% filter(Unit %in% c("30","31","32","33")) %>% group_by(RunNumber) %>% 
#   mutate(DeptRank = min_rank(DepartureTime))
# resSum$Aggr <- "ChiefsBestDeparture"
# resSum <- resSum %>% filter(DeptRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# resSum <- resdf %>% filter(Unit %in% c("01","03","06","07")) %>% group_by(RunNumber) %>% 
#   mutate(DeptRank = min_rank(DepartureTime))
# resSum$Aggr <- "MainBestDeparture"
# resSum <- resSum %>% filter(DeptRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# resSum <- resdf %>% filter(Unit %in% c("02","05","15","09")) %>% group_by(RunNumber) %>% 
#   mutate(DeptRank = min_rank(DepartureTime))
# resSum$Aggr <- "Sta2BestDeparture"
# resSum <- resSum %>% filter(DeptRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# resSum <- resdf %>% filter(Unit %in% c("04","08")) %>% group_by(RunNumber) %>% 
#   mutate(DeptRank = min_rank(DepartureTime))
# resSum$Aggr <- "Sta3BestDeparture"
# resSum <- resSum %>% filter(DeptRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# resSum <- resdf %>% filter(Unit %in% c("01","02","03","04","07","08","09")) %>% group_by(RunNumber) %>% 
#   mutate(DeptRank = min_rank(DepartureTime))
# resSum$Aggr <- "FirstEngineBestDeparture"
# resSum <- resSum %>% filter(DeptRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# resSum <- resdf %>% filter(Unit %in% c("05","06","15")) %>% group_by(RunNumber) %>% 
#   mutate(DeptRank = min_rank(DepartureTime))
# resSum$Aggr <- "FirstTruckBestDeparture"
# resSum <- resSum %>% filter(DeptRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# 
# resdf <- bind_rows(resdf,resAdd)
# 
# ### Now best Arrival
# 
# resSum <- resdf %>% filter(Unit %in% c("01","02","03","04","05","06","07","08","09","15") & Aggr =="Actual" & ArrivalTime != "") %>% 
#   group_by(RunNumber) %>% 
#   mutate(ArrRank = min_rank(ArrivalTime))
# resSum$Aggr <- "FrontlineUnitsBestArrival"
# 
# resSum <- resSum %>% filter(ArrRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- data.frame()
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# #Chiefs
# 
# resSum <- resdf %>% filter(Unit %in% c("30","31","32","33")& Aggr =="Actual"& ArrivalTime != "") %>% 
#   group_by(RunNumber) %>% 
#   mutate(ArrRank = min_rank(ArrivalTime))
# resSum$Aggr <- "ChiefsBestArrival"
# resSum <- resSum %>% filter(ArrRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# resSum <- resdf %>% filter(Unit %in% c("01","03","06","07")& Aggr =="Actual" & ArrivalTime != "") %>% 
#   group_by(RunNumber) %>% 
#   mutate(ArrRank = min_rank(ArrivalTime))
# resSum$Aggr <- "MainBestArrival"
# resSum <- resSum %>% filter(ArrRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# resSum <- resdf %>% filter(Unit %in% c("02","05","15","09")& Aggr =="Actual" & ArrivalTime != "") %>% 
#   group_by(RunNumber) %>% 
#   mutate(ArrRank = min_rank(ArrivalTime))
# resSum$Aggr <- "Sta2BestArrival"
# resSum <- resSum %>% filter(ArrRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# resSum <- resdf %>% filter(Unit %in% c("04","08")& Aggr =="Actual" & ArrivalTime != "") %>% 
#   group_by(RunNumber) %>% 
#   mutate(ArrRank = min_rank(ArrivalTime))
# resSum$Aggr <- "Sta3BestArrival"
# resSum <- resSum %>% filter(ArrRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# resSum <- resdf %>% filter(Unit %in% c("01","02","03","04","07","08","09")& Aggr =="Actual" & ArrivalTime != "") %>% 
#   group_by(RunNumber) %>% 
#   mutate(ArrRank = min_rank(ArrivalTime))
# resSum$Aggr <- "FirstEngineBestArrival"
# resSum <- resSum %>% filter(ArrRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# resSum <- resdf %>% filter(Unit %in% c("05","06","15")& Aggr =="Actual" & ArrivalTime != "") %>% 
#   group_by(RunNumber) %>% 
#   mutate(ArrRank = min_rank(ArrivalTime))
# resSum$Aggr <- "FirstTruckBestArrival"
# resSum <- resSum %>% filter(ArrRank==1) %>% select(Unit, RunNumber,DepartureTime,ArrivalTime,Aggr)
# resAdd <- bind_rows(resAdd,ungroup(resSum))
# 
# 
# resdf <- bind_rows(resdf,resAdd)
# 
# write.csv(resdf,"C:/Users/murph/Desktop/R/AlarmMap/AggResponseData.csv", row.names = FALSE)
# 
# 
