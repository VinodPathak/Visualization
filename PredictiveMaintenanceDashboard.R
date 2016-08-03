## app.R ##
library(shinydashboard)
library(AnomalyDetection)
library(dplyr)
library(zoo)
setwd("C:\\Users\\vinod.pathak\\Desktop\\Vinod Pathak\\Predictive Maintenance\\pre")
df <- read.csv("PdM_telemetry.csv")
df$datetime <- as.POSIXct(df$datetime,tz = "GMT")
df$month <- format(df$datetime,"%B")
machine_fails <- 0
failures <- read.csv("PdM_failures.csv")
failures$datetime <- as.POSIXct(failures$datetime,tz = "GMT")
failures$month <- format(failures$datetime,"%B")

ui <- dashboardPage(
  dashboardHeader(title = "Predictive Maintenance dashboard",titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Sensors",tabName = "Sensors")
              )
    ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "dashboard",
    fluidRow(
      column(width=2,
             selectInput("machine", "MachineID:",choices=as.integer(levels(as.factor(df[,2]))))),
      column(width=2,
            selectInput("month","Month",choices = c("January","February","March","April","May","June","July",
                                                    "August","September","October","November","December"))
      ),
      column(width=2,
             selectInput("sensor","Sensor:",choices = c("volt","rotate","pressure","vibration"))),
      column(width = 6,
             box(
               title = "Machine Information", width = 12, solidHeader = TRUE, status = "primary",
               textOutput("info1"),
               textOutput("info2")
             ))
          ),
      fluidRow(
        box(
          title = "Anomalies in Sensor data", solidHeader = TRUE,
          plotOutput("out")
        )
      )
    ),
    tabItem(tabName = "Sensors",
            fluidRow(
              column(6,
                     infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE))
            )
        )
    )
      
      )
)
    
  

server <- function(input, output) {
  inputdf <- reactive({
    dynamicdf <- df[df$machineID==input$machine & df$month==input$month,]
    dynamicdf$date_col <- substr(as.character(dynamicdf$datetime),1,10)
    dynamicdf$date_col <- as.POSIXct(dynamicdf$date_col,tz="GMT")
    
    dynamicdf <- as.data.frame(dynamicdf) %>%
      arrange(datetime)%>%
      mutate(voltmean = rollapply(volt, width = 24, FUN = mean, align = "right",fill=NA,by =24),
             rotatemean = rollapply(rotate, width = 24, FUN = mean, align = "right",fill=NA,by = 24),
             pressuremean = rollapply(pressure, width = 24, FUN = mean, align = "right",fill=NA, by = 24),
             vibrationmean = rollapply(vibration, width = 24, FUN = mean, align = "right",fill=NA,by = 24)) %>%
      filter(!is.na(voltmean))%>%
      select(date_col, voltmean, rotatemean, pressuremean, vibrationmean)%>%
      arrange(date_col)
    if(input$sensor=="volt")
    {
      newdf <- dynamicdf[,c(1,2)]
    }
    else if(input$sensor=="rotate")
    {
      newdf <- dynamicdf[,c(1,3)]
    }
    else if(input$sensor=="pressure")
    {
      newdf <- dynamicdf[,c(1,4)]
    }
    else if(input$sensor=="vibration")
    {
      newdf <- dynamicdf[,c(1,5)]
    }
    newdf
  })
 
 
  output$out <- renderPlot({
    AnomalyDetectionTs(inputdf(), max_anoms=0.02, direction='both', plot=TRUE)
  })
  
  output$info1 <- renderText({
    paste("Machine ID: ",input$machine,"\n")
    
  })
  
  
  machine_fails <- reactive({
    tryCatch(
      {
        Machine_failures <- failures[failures$machineID == input$machine & failures$month==input$month,]
        Machine_failures$failure <- 1
        sum(Machine_failures$failure)
      },
      error=function(cond) {
        0
      }
  )
})
  
  output$info2 <- renderText({
    paste("No of Failures this Month: ",machine_fails)
  })
  
}

shinyApp(ui, server)