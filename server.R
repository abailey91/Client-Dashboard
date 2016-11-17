
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(rCharts)

salesData <- read.csv("Sales.csv",sep = ",",header=TRUE)
footfallData <- read.csv("Footfall.csv",sep = ",",header=TRUE)
webvisitData <- read.csv("WebVisits.csv",sep = ",",header=TRUE)
brandMetricData <- read.csv("BrandMetrics.csv",sep = ",",header=TRUE)
storeInformationData <- read.csv("StoreInformation.csv",sep = ",",header=TRUE)
competitorData <- read.csv("Competitors.csv",sep = ",",header=TRUE)


shinyServer(function(input, output,session) {

  #Dynamic date selection
  output$calandar_selectDate <- renderUI(
    
    dateRangeInput("calandar_selectDate",label = "Select a Date Range:",min=min(as.Date(salesData$Date,"%d/%m/%Y")),start=min(as.Date(salesData$Date,"%d/%m/%Y")),max=max(as.Date(salesData$Date,"%d/%m/%Y")),end = max(as.Date(salesData$Date,"%d/%m/%Y")))
    
  )
  
  #dashboard Screen
  output$valueBox_ytdSales <- renderValueBox(
    {
      
      #find current FY based on system time
      currentFY <- "FY16"
      
      #calculate sales for current FY
      salesbyFY <- aggregate(sales_excl_VAT~FY, data=salesData, sum, na.rm=TRUE)
      
      valueBox(paste("£",format(subset(salesbyFY,FY==currentFY)$sales_excl_VAT,big.mark = ",")),"Year to Date Sales",icon = icon("shopping-cart"),color="light-blue")
    }
  )
  
  
  ######## sales box ######### 
  
  #### Variables #####
  #calculates current total sales based on the selected date range 
  totalSales <- reactive({
    currentSelectedSales <- subset(salesData,as.Date(Date,"%d/%m/%Y")>=format(input$calandar_selectDate[1]) & as.Date(Date,"%d/%m/%Y")<=format(input$calandar_selectDate[2]))
    return((currentSelectedSales))
  })
  
  weeklySalesChange <- reactive({
    
    
    latestWeekSales <- totalSales()$sales_excl_VAT[length(totalSales()$sales_excl_VAT)]
    previousWeekSales <- totalSales()$sales_excl_VAT[length(totalSales()$sales_excl_VAT)-1]
      
      return(paste(round(latestWeekSales/previousWeekSales-1,digits=2)*100,"%",sep=""))
  })
  
  #### Ouputs ####
  output$plot_sales <- renderChart2(
    {
      #plot(totalSales()$sales_excl_VAT,type="l")
      plotdata <<- data.frame(Date=as.numeric(as.Date(totalSales()$Date,format="%d/%m/%Y")),Sales=totalSales()$sales_excl_VAT)
      
      plotdata <- aggregate(Sales~Date, data=plotdata, sum, na.rm=TRUE)
      
      # to_jsdate <- function(date_){
      #   val = as.POSIXct(as.Date(date_),origin="1970-01-01")
      #   as.numeric(val)
      # }
      
      #plotdata <- transform(plotdata, date2 = to_jsdate(Date))
      
      # Create the chart
      totalSalesPlot <- nPlot(
        Sales ~ Date, 
        data = plotdata, 
        type = "lineChart")
      
      totalSalesPlot$xAxis(
        tickFormat =   "#!
        function(d) {return d3.time.format('%d/%m/%Y')(new Date(d*1000*60*60*24));}
        !#",
      #rotateLabels = -45,
        axisLabel="Date"
      )
      
      totalSalesPlot$yAxis(
        tickFormat = "#! function(d) {return d3.format(',')(d)} !#",
        axisLabel="Sales",
        width=60
      )
      totalSalesPlot$chart(forceY=c(0,max(plotdata$Sales)+mean(plotdata$Sales)/2))
      
      totalSalesPlot$set(width = session$clientData$output_plot1_width)
      
      totalSalesPlot
    }
  )
  
  output$infobox_totalSales <- renderInfoBox({
    infoBox(width=4,title = "Total Sales",color = "blue",value=paste("£",format(sum(totalSales()$sales_excl_VAT),big.mark = ","),sep = ""),icon=icon("shopping-cart"))
  })
  
  output$infobox_weekSales <- renderInfoBox({
    infoBox(width=4,title = "Week on Week % Change",color = "blue",value = weeklySalesChange())
  })
  
  output$infobox_yearSales <- renderInfoBox({
    infoBox(width=4,title = "Year on Year % Change",color = "blue")
  })
  
  ###### Footfall Box ######
  
  ## Variables ##
  #calculates current total sales based on the selected date range 
  totalFF <- reactive({
    currentSelectedFF <- subset(footfallData,as.Date(Date,"%d/%m/%Y")>=format(input$calandar_selectDate[1]) & as.Date(Date,"%d/%m/%Y")<=format(input$calandar_selectDate[2]))
    return((currentSelectedFF))
  })
  
  weeklyFFChange <- reactive({
    
    
    latestWeekFF <- totalFF()$Footfall[length(totalFF()$Footfall)]
    previousWeekFF <- totalFF()$Footfall[length(totalFF()$Footfall)-1]
    
    return(paste(round(latestWeekFF/previousWeekFF-1,digits=2)*100,"%",sep=""))
  })
  
  #### Ouputs ####
  output$plot_ff <- renderChart2(
    {
      #plot(totalSales()$sales_excl_VAT,type="l")
      ffplotdata <- data.frame(Date=as.numeric(as.Date(totalFF()$Date,format="%d/%m/%Y")),Footfall=totalFF()$Footfall)
      
      # Create the chart
      totalFFPlot <- nPlot(
        Footfall ~ Date, 
        data = ffplotdata, 
        type = "lineChart")
      
      totalFFPlot$xAxis(
        tickFormat =   "#!
        function(d) {return d3.time.format('%d/%m/%Y')(new Date(d*1000*60*60*24));}
        !#",
      #  rotateLabels = -45,
        axisLabel="Date"
      )
      
      totalFFPlot$yAxis(
        tickFormat = "#! function(d) {return d3.format(',')(d)} !#",
        axisLabel="Footfall",
        width=60
      )
      
      totalFFPlot$chart(forceY=c(0,max(ffplotdata$Footfall)++mean(ffplotdata$Footfall)/2))
      
      totalFFPlot$set(width = session$clientData$output_plot2_width)
      
      totalFFPlot
    }
  )
  
  output$infobox_totalFF <- renderInfoBox({
    infoBox(width=4,title = "Total Footfall",color = "blue",value=format(sum(totalFF()$Footfall),big.mark = ","),icon=icon("users"))
  })
  
  output$infobox_weekFF <- renderInfoBox({
    infoBox(width=4,title = "Week on Week % Change",color = "blue",value=weeklyFFChange())
  })
  
  output$infobox_yearFF <- renderInfoBox({
    infoBox(width=4,title = "Year on Year % Change",color = "blue")
  })
  
  ###### Web Vis Box ######
  
  ## Variables ##
  #calculates current total sales based on the selected date range 
  totalWV <- reactive({
    currentSelectedWV <- subset(webvisitData,as.Date(Date,"%d/%m/%Y")>=format(input$calandar_selectDate[1]) & as.Date(Date,"%d/%m/%Y")<=format(input$calandar_selectDate[2]))
    return((currentSelectedWV))
  })
  
  weeklyWVChange <- reactive({
    
    
    latestWeekWV <- totalWV()$WebVisits[length(totalWV()$WebVisits)]
    previousWeekWV <- totalWV()$WebVisits[length(totalWV()$WebVisits)-1]
    
    return(paste(round(latestWeekWV/previousWeekWV-1,digits=2)*100,"%",sep=""))
  })
  
  #### Ouputs ####
  output$plot_wv <- renderChart2(
    {
      #plot(totalSales()$sales_excl_VAT,type="l")
      wvplotdata <- data.frame(Date=as.numeric(as.Date(totalWV()$Date,format="%d/%m/%Y")),WebVisits=totalWV()$WebVisits)
      
      # Create the chart
      totalWVPlot <- nPlot(
        WebVisits ~ Date, 
        data = wvplotdata, 
        type = "lineChart")
      
      totalWVPlot$xAxis(
        tickFormat =   "#!
        function(d) {return d3.time.format('%d/%m/%Y')(new Date(d*1000*60*60*24));}
        !#",
      #  rotateLabels = -45,
        axisLabel="Date"
      )
      
      totalWVPlot$yAxis(
        tickFormat = "#! function(d) {return d3.format(',')(d)} !#",
        axisLabel="Web Visits",
        width=60
      )
      
      totalWVPlot$chart(forceY=c(0,max(wvplotdata$WebVisits)++mean(wvplotdata$WebVisits)/2))
      
      totalWVPlot$set(width = session$clientData$output_plot3_width)
      
      totalWVPlot
    }
  )
  
  output$infobox_totalWV <- renderInfoBox({
    infoBox(width=4,title = "Total Web Visits",color = "blue",value=format(sum(totalWV()$WebVisits),big.mark = ","),icon=icon("laptop"))
  })
  
  output$infobox_weekWV <- renderInfoBox({
    infoBox(width=4,title = "Week on Week % Change",color = "blue",value=weeklyWVChange())
  })
  
  output$infobox_yearWV <- renderInfoBox({
    infoBox(width=4,title = "Year on Year % Change",color = "blue")
  })
  
  ###### Brand Metrics Box ######
  
  ## Variables ##
  #calculates current total sales based on the selected date range 
  
  currentViewBrand <- reactive({
    currentSelectedBrand <- subset(brandMetricData,as.Date(Date,"%d/%m/%Y")>=format(input$calandar_selectDate[1]) & as.Date(Date,"%d/%m/%Y")<=format(input$calandar_selectDate[2]))
    return((currentSelectedBrand))
  })
  
  weeklyAwareChange <- reactive({
    
  })
  
  weeklyConsidChange <- reactive({
    
  })
  
  #### Ouputs ####
  output$plot_brand <- renderChart2(
    {
     # brandplotdata <- data.frame(Date=as.numeric(as.Date(totalAware()$Date,format="%d/%m/%Y")),Awareness=totalConsid()$Consideration)
      
      #test<<- totalAware()$Awareness
      
      #brandplotdata <<- data.frame(Date=as.numeric(as.Date(currentViewBrand()$Date,format="%d/%m/%Y")),Awareness=currentViewBrand()$Awareness,Consideration=currentViewBrand()$Consideration,PurchaseIntent=totalPI()$Purchase.Intent)
      brandplotdata <<- aggregate(Score~Date+Metric, data=currentViewBrand(), sum, na.rm=TRUE)
      
      #brandplotdata <- cbind(brandplotdata,Consideration=totalConsid()$Consideration)
      brandplotdata$Date <<- as.numeric(as.Date(currentViewBrand()$Date,format="%d/%m/%Y"))
      
      brandplot <- nPlot(
        Score ~ Date, 
        data = brandplotdata,
        group = "Metric",
        type = "lineChart")
      
      # Create the chart
    
      brandplot$xAxis(
        tickFormat =   "#!
        function(d) {return d3.time.format('%d/%m/%Y')(new Date(d*1000*60*60*24));}
        !#",
      #  rotateLabels = -45,
        axisLabel="Date"
      )
      
      brandplot$yAxis(
        tickFormat = "#! function(d) {return d3.format('.2f%')(d)} !#",
        axisLabel="Score",
        width=60
      )
     
      
      brandplot$chart(forceY=c(0,1))
      
      brandplot$set(width = session$clientData$output_plot4_width)
      
      brandplot
    }
  )
  
  output$infobox_awareness <- renderInfoBox({
    infoBox(width=6,title = "Awareness",color = "blue",value=paste(round(mean(subset(currentViewBrand(),Metric=="Awareness")$Score),digits=2)*100,"%",sep=""),icon=icon("eye"))
  })
  
  output$infobox_consideration <- renderInfoBox({
    infoBox(width=6,title = "Consideration",color = "blue",value=paste(round(mean(subset(currentViewBrand(),Metric=="Consideration")$Score),digits=2)*100,"%",sep=""),icon=icon("check-circle"))
  })
  
  output$infobox_purchaseIntent <- renderInfoBox({
    infoBox(width=6,title = "Purchase Intent",color = "blue",value=paste(round(mean(subset(currentViewBrand(),Metric=="Purchase Intent")$Score),digits=2)*100,"%",sep=""),icon=icon("shopping-basket"))
  })
  
  
  ########## Store Map ##########
  
  #### variables ####
  
  
  ## outputs ###
  
  #draw map
  output$map_stores <- renderLeaflet({
    
    # Create a palette that maps factor levels to colors
    pal <- colorFactor(c("red", "navy"), domain = c("Superstore", "Order and Collection Point"))
    
    #levels(storeInformationData$Store.Name))
    map <- leaflet() %>% addTiles() %>% setView(lng = -2, lat =54,  zoom = 6) %>%
      addCircleMarkers(data=subset(storeInformationData,Store_Type=="Superstore"),lng=~Lng,lat=~Lat,popup=~Store.Name,group="Superstores",color = ~pal(Store_Type),clusterOptions = markerClusterOptions()) %>%
      addCircleMarkers(data=subset(storeInformationData,Store_Type=="Order and Collection Point"),lng=~Lng,lat=~Lat,popup=~Store.Name,group="Order and Collection Points",color = ~pal(Store_Type),clusterOptions = markerClusterOptions() ) %>%
      addLayersControl(
        overlayGroups = c("Superstores", "Order and Collection Points"),
        options = layersControlOptions(collapsed = FALSE)
      )
    map
  })
  
  
  rankedStores <- reactive({
    topStoreValue <- data.frame(Sales=totalSales()$sales_excl_VAT,Store=totalSales()$Store.Name)
    topStoreValue <- aggregate(Sales~Store, data=topStoreValue, sum, na.rm=TRUE)
    
    finalRanks <- as.data.frame(topStoreValue$Store[order(topStoreValue$Sales,decreasing=TRUE)])
    colnames(finalRanks) <- c("Store")
    return(finalRanks)
  })
  
  #top store value box
  output$valueBox_TopStore <- renderValueBox({
   
    valueBox(value = rankedStores()$Store[1],"Top Store",icon = icon("building"),width=4)
  })
  
  #second store value box
  output$valueBox_2ndStore <- renderValueBox({
    valueBox(value = rankedStores()$Store[2],"2nd Store",icon = icon("building"),width=4)
  })
  
  #third store value box
  output$valueBox_3rdStore <- renderValueBox({
    
    
    valueBox(value = rankedStores()$Store[3],"3rd Store",icon = icon("building"),width=4)
    
  })
  
  
  observe({
    click<-input$map_stores_marker_click
    if(is.null(click))
      return()

    mapClickData <- click#reactive({})
    })
  
  
  
  
  #plot of stores weekly sales
  output$plot_store_weekly <- renderChart2(
    {
      
      #plot(totalSales()$sales_excl_VAT,type="l")
      store_week_plotdata <- data.frame(Date=as.numeric(as.Date(totalSales()$Date,format="%d/%m/%Y")),Sales=totalSales()$sales_excl_VAT,Store=totalSales()$Store.Name)

      store_week_plotdata <- aggregate(Sales~Date+Store, data=store_week_plotdata, sum, na.rm=TRUE)

      
      selectedStore <- "Birmingham"
      store_week_plotdata <- subset(store_week_plotdata,Store==input$slct_stores)
      
      # Create the chart
      store_week_Plot <- nPlot(
        Sales ~ Date,
        data = store_week_plotdata,
        type = "lineChart")

      store_week_Plot$xAxis(
        tickFormat =   "#!
        function(d) {return d3.time.format('%d/%m/%Y')(new Date(d*1000*60*60*24));}
        !#",
      #  rotateLabels = -45,
        axisLabel="Date"
      )

      store_week_Plot$yAxis(
        tickFormat = "#! function(d) {return d3.format(',')(d)} !#",
        axisLabel=paste("Sales"),
        width=60
      )

      store_week_Plot$chart(forceY=c(0,max(store_week_plotdata$Sales)+mean(store_week_plotdata$Sales)/2))

      store_week_Plot$set(width = session$clientData$output_plot7_width)

      store_week_Plot
    }
  )
  
  
  
  #plot of stores weekly sales
  output$plot_store_FY <- renderChart2(
    {
      
      #plot(totalSales()$sales_excl_VAT,type="l")
      store_FY_plotdata <- data.frame(Date=as.numeric(as.Date(totalSales()$Date,format="%d/%m/%Y")),Sales=totalSales()$sales_excl_VAT,Store=totalSales()$Store.Name,FY=totalSales()$FY)
      
      store_FY_plotdata <- aggregate(Sales~FY+Store, data=store_FY_plotdata, sum, na.rm=TRUE)
      
      
      selectedStore <- "Birmingham"
      store_FY_plotdata <- subset(store_FY_plotdata,Store==input$slct_stores)
      
      # Create the chart
      store_FY_Plot <- nPlot(
        Sales ~ FY,
        data = store_FY_plotdata,
        #group = "FY",
        type = "multiBarChart")
      
      store_FY_Plot$xAxis(
      #  tickFormat =   "#!
       # function(d) {return d3.time.format('%d/%m/%Y')(new Date(d*1000*60*60*24));}
        #!#",
        #rotateLabels = -45,
        axisLabel="FY"
      )
      
      store_FY_Plot$yAxis(
        tickFormat = "#! function(d) {return d3.format(',')(d)} !#",
        axisLabel=paste("Sales"),
        width=60
      )
      
      #store_FY_Plot$chart(forceY=c(0,max(store_week_plotdata$Sales)+mean(store_week_plotdata$Sales)/2))
      
      store_FY_Plot$set(width = session$clientData$output_plot7_width)
      
      store_FY_Plot
    }
  )
  
  output$slct_stores <- renderUI(
    {

      selectInput("slct_stores",label="Select a store: ",choices = as.list(levels(totalSales()$Store.Name)))
    }
  )
  
  output$tbl_weekSalesData <- renderDataTable({
    store_week_plotdata <- data.frame(Date=(as.Date(totalSales()$Date,format="%d/%m/%Y")),Sales=totalSales()$sales_excl_VAT,Store=totalSales()$Store.Name)
    
    store_week_plotdata <- aggregate(Sales~Date+Store, data=store_week_plotdata, sum, na.rm=TRUE)
    
    store_week_plotdata <- subset(store_week_plotdata,Store==input$slct_stores)
    
    store_week_plotdata$Sales <- paste("£",format(store_week_plotdata$Sales,big.mark = ","),sep="")
    
    store_week_plotdata
  })
  
  

  ####### Competitors #######
  
  
  #dynamic date slider
  output$slider_comp_week_dateRange <- renderUI({
    sliderInput("slider_comp_week_dateRange","Select a date range:",width="100%",step=7,min=as.Date(format(input$calandar_selectDate[1])),max=as.Date(format(input$calandar_selectDate[2])),value = c(as.Date(format(input$calandar_selectDate[1])),as.Date(format(input$calandar_selectDate[2]))))
  })
  
  #### Variables #####
  reactiveCompetitors <- reactive({
    currentSelectedCompetitors <- subset(competitorData,as.Date(Date,"%d/%m/%Y")>=format(input$slider_comp_week_dateRange[1]) & as.Date(Date,"%d/%m/%Y")<=format(input$slider_comp_week_dateRange[2]))
    return((currentSelectedCompetitors))
  })
  
  #### Ouputs ####
  
  
  ###****~~ Work out how to sum across all media channels for brand over time chart - causing weird overlay effect ~~~***###
  #Spend over time by competitor
  output$plot_comp_weekly <- renderChart2(
    {
      
      #plot(totalSales()$sales_excl_VAT,type="l")
      comp_week_plotdata <<- data.frame(Date=as.numeric(as.Date(reactiveCompetitors()$Date,format="%d/%m/%Y")),Spend=reactiveCompetitors()$Spend,Competitor=reactiveCompetitors()$Competitor)
      
      comp_week_plotdata <- aggregate(Spend~Date+Competitor, data=comp_week_plotdata, sum, na.rm=TRUE)
      
      # Create the chart
      comp_week_Plot <- nPlot(
        Spend ~ Date, 
        data = comp_week_plotdata, 
        group="Competitor",
        type = "stackedAreaChart")
      
      comp_week_Plot$xAxis(
        tickFormat =   "#!
        function(d) {return d3.time.format('%d/%m/%Y')(new Date(d*1000*60*60*24));}
        !#",
       # rotateLabels = -45,
        axisLabel="Date"
      )
      
      comp_week_Plot$yAxis(
        tickFormat = "#! function(d) {return d3.format(',')(d)} !#",
        axisLabel=paste("Competitor Spend"),
        width=60
      )
      
      comp_week_Plot$chart(forceY=c(0,max(comp_week_plotdata$Spend)+mean(comp_week_plotdata$Spend)/2))
      
      comp_week_Plot$set(width = session$clientData$output_plot5_width)
      
      comp_week_Plot
    }
  )
  
  # Comparison bar chart
  output$plot_comp_comparison <- renderChart2(
    {
      #plot(totalSales()$sales_excl_VAT,type="l")
      comp_comparison_plotdata <- data.frame(Spend=reactiveCompetitors()$Spend,Competitor=reactiveCompetitors()$Competitor,Channel=reactiveCompetitors()$Channel)
      
      comp_comparison_plotdata <- aggregate(Spend~Channel+Competitor, data=comp_comparison_plotdata, sum, na.rm=TRUE)
      
      # Create the chart
      comp_comparison_Plot <- nPlot(
        Spend ~ Competitor, 
        data = comp_comparison_plotdata, 
        group="Channel",
        type = "multiBarChart")
      
      comp_comparison_Plot$xAxis(
     #   tickFormat =   "#!
      #  function(d) {return d3.time.format('%d/%m/%Y')(new Date(d*1000*60*60*24));}
       # !#",
        #rotateLabels = -45,
        #axisLabel="Competitor",
       # align="Center"
      )
      comp_comparison_Plot$chart(reduceXTicks = FALSE)
      comp_comparison_Plot$xAxis(staggerLabels = TRUE)
      
      comp_comparison_Plot$yAxis(
        tickFormat = "#! function(d) {return d3.format(',')(d)} !#",
        axisLabel=paste("Competitor Spend"),
        width=60
      )
      
      comp_comparison_Plot$chart(forceY=c(0,sum(comp_comparison_plotdata$Spend)+mean(comp_comparison_plotdata$Spend)/2))
      
      comp_comparison_Plot$set(width = session$clientData$output_plot6_width)
      
      comp_comparison_Plot
    }
  )
  
  
  # Dynamic competitor data table 
  output$tbl_competitorData <- renderDataTable(
    {
      if(input$subtabbox_comp=="Weekly Spend")
      {
        comp_week_tabledata <- data.frame(Date=as.Date(reactiveCompetitors()$Date,format="%d/%m/%Y"),Spend=reactiveCompetitors()$Spend,Competitor=reactiveCompetitors()$Competitor)
        comp_week_tabledata <- aggregate(Spend~Date+Competitor, data=comp_week_tabledata, sum, na.rm=TRUE)
        comp_week_tabledata$Spend <- paste("£",format(comp_week_tabledata$Spend,big.mark = ","),sep="")
        return(comp_week_tabledata)
      }
      else if(input$subtabbox_comp=="Spend Comparison")
      {
        comp_comparison_tabledata <- data.frame(Spend=reactiveCompetitors()$Spend,Competitor=reactiveCompetitors()$Competitor,Channel=reactiveCompetitors()$Channel)
        comp_comparison_tabledata <- aggregate(Spend~Channel+Competitor, data=comp_comparison_tabledata, sum, na.rm=TRUE)
        return(comp_comparison_tabledata)
      }
      return(0)
    }
  )
  
  
  
  
})


