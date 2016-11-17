
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(rCharts)
library(DT)


shinyUI(dashboardPage(
  dashboardHeader(title = "Retailer Dashboard",#img(src="Ikea_logo.png",height=45,width=200),
                  # Dropdown menu for messages
                  dropdownMenu(type = "messages", badgeStatus = "success",
                               messageItem("Admin",
                                           "Data is currently updated to: ",
                                           time = Sys.time()
                               ))),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",tabName = "tab_dashboard",icon = icon("tachometer")),
      menuItem("Store Performance",tabName = "tab_storeMap",icon = icon("map")),
      menuItem("Competitors Media",tabName = "tab_competitors",icon = icon("industry")),
      br(),
      uiOutput("calandar_selectDate")
    )
  ),
  
  dashboardBody(
 
    
    tabItems(
      tabItem(tabName = "tab_dashboard",
              fluidRow(
                box(width = 12,title="Headlines",collapsible=TRUE,status="primary",background="blue",
                    valueBoxOutput("valueBox_ytdSales"))
              ),
              fluidRow(
                box(width=6,title = h1("Sales Revenue"),collapsible = TRUE,status = "primary",
                    plotOutput("plot1", height = "1px"), #used to resize rChart
                    chartOutput("plot_sales","nvd3"),
                    br(),
                    infoBoxOutput("infobox_totalSales"),
                    infoBoxOutput("infobox_weekSales"),
                    infoBoxOutput("infobox_yearSales"),
                    br(),
                    footer = "Source: NA"
                    ),
                
                box(width=6,title = h1("Footfall"),collapsible = TRUE,status = "primary",
                    plotOutput("plot2", height = "1px"), #used to resize rChart
                    chartOutput("plot_ff","nvd3"),
                    br(),
                    infoBoxOutput("infobox_totalFF"),
                    infoBoxOutput("infobox_weekFF"),
                    infoBoxOutput("infobox_yearFF"),
                    footer="Source: NA"
                    )
                
                
              ),
              fluidRow(
                box(width=6,title = h1("Web Visits"),collapsible = TRUE,status = "primary",
                    plotOutput("plot3", height = "1px"), #used to resize rChart
                    chartOutput("plot_wv","nvd3"),
                    br(),
                    infoBoxOutput("infobox_totalWV"),
                    infoBoxOutput("infobox_weekWV"),
                    infoBoxOutput("infobox_yearWV"),
                    br(),
                    footer="Source: NA"
                    ),
                
                box(width=6,title = h1("Brand Metrics"),collapsible = TRUE,status = "primary",
                    plotOutput("plot4", height = "1px"), #used to resize rChart
                    chartOutput("plot_brand","nvd3"),
                    br(),
                    infoBoxOutput("infobox_awareness"),
                    infoBoxOutput("infobox_consideration"),
                    infoBoxOutput("infobox_purchaseIntent"),
                    footer="Source: NA"
                    )
              )),
      tabItem(tabName = "tab_storeMap",
              fluidRow(
                box(width=12,background = "blue",
                    valueBoxOutput("valueBox_TopStore"),
                    valueBoxOutput("valueBox_2ndStore"),
                    valueBoxOutput("valueBox_3rdStore")
                )
              ),
              fluidRow(
                box(width=6,title=h1("Store Map"),
                    leafletOutput("map_stores",height = 800)),
                box(width=6,title=h1("Historic Store Performance"),
                    selectInput("slct_stores","Select a store: ",choices = c("Birmingham","Bristol")),
                    plotOutput("plot7", height = "1px"), #used to resize rChart
                    h2("Sales by FY"),
                    chartOutput("plot_store_FY","nvd3"),
                    h2("Sales by Week"),
                    chartOutput("plot_store_weekly","nvd3"))
              ),
              fluidRow(
                box(width=12,collapsible = TRUE,
                    h2("Data Table"),
                DT::dataTableOutput("tbl_weekSalesData",width="100%",height="auto")
                )
              )
              
                
                
              ),
      tabItem(tabName = "tab_competitors",
              fluidRow(
                tabBox(title = "Competitor Plots",width=12,id="subtabbox_comp",
                       tabPanel("Weekly Spend",height=600,id="subtab_comp_weekly",
                                plotOutput("plot5", height = "1px"), #used to resize rChart
                                chartOutput("plot_comp_weekly","nvd3"),
                                uiOutput("slider_comp_week_dateRange"),
                       br(),
                       "Source: Nielson Addynamix"),
                       tabPanel("Spend Comparison",height=600,id="subtab_comp_comparison",
                                plotOutput("plot6", height = "1px"), #used to resize rChart
                                chartOutput("plot_comp_comparison","nvd3"),
                                br(),
                                "Source: NA"))
                
              ),
              fluidRow(
                box(width=12,title="Data Table",collapsible = TRUE,status="primary",
                    DT::dataTableOutput("tbl_competitorData",width="100%",height="auto")
                    )
              ))
    )
  )

    )
)
