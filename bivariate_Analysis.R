## app.R ##
#library(shinydashboard)
setwd("C:\\Users\\vinod.pathak\\Desktop\\Vinod Pathak\\Predictive Maintenance\\pre")
library(shiny)
library(ggplot2)


#df <- read.csv("PdM_telemetry.csv")
#failures <- read.csv("PdM_failures.csv")
ui <- fluidPage(    
  titlePanel("Predictive Maintenance"),
  fluidRow(
    column(6,
  # sidebarLayout(      
  #   sidebarPanel(
      selectInput("machine", "MachineID:",choices=as.integer(levels(as.factor(df[,2]))))),
    column(6,
      selectInput("duration", "Time:",choices=c("Daily","Weekly","Monthly"))
    )),
    mainPanel(width = 12,
              tabsetPanel(
                tabPanel("Sensor Output by MachineID", plotOutput("out"))
                         )
              )
              )

# Define a server for the Shiny app
server <- function(input, output) {
  inputdf <- reactive({
    dynamicdf <- df[df$machineID==input$machine,]
    dynamicdf$date_col <- substr(as.character(dynamicdf$datetime),1,10)
    dynamicdf$date_col <- as.Date(dynamicdf$date_col)
    if(input$duration=="Daily"){
    as.data.frame(dynamicdf) %>%
      arrange(datetime)%>%
      mutate(voltmean = rollapply(volt, width = 24, FUN = mean, align = "right",fill=NA,by =24),
             rotatemean = rollapply(rotate, width = 24, FUN = mean, align = "right",fill=NA,by = 24),
             pressuremean = rollapply(pressure, width = 24, FUN = mean, align = "right",fill=NA, by = 24),
             vibrationmean = rollapply(vibration, width = 24, FUN = mean, align = "right",fill=NA,by = 24)) %>%
      filter(!is.na(voltmean))%>%
      select(date_col, voltmean, rotatemean, pressuremean, vibrationmean)%>%
      arrange(date_col)
    }
    else if(input$duration=="Weekly")
    {
      as.data.frame(dynamicdf) %>%
        arrange(datetime)%>%
        mutate(voltmean = rollapply(volt, width = 168, FUN = mean, align = "right",fill=NA,by =168),
               rotatemean = rollapply(rotate, width = 168, FUN = mean, align = "right",fill=NA,by = 168),
               pressuremean = rollapply(pressure, width = 168, FUN = mean, align = "right",fill=NA, by = 168),
               vibrationmean = rollapply(vibration, width = 168, FUN = mean, align = "right",fill=NA,by = 168)) %>%
        filter(!is.na(voltmean))%>%
        select(date_col, voltmean, rotatemean, pressuremean, vibrationmean)%>%
        arrange(date_col)
    }
    else if(input$duration=="Monthly")
    {
      as.data.frame(dynamicdf) %>%
        arrange(datetime)%>%
        mutate(voltmean = rollapply(volt, width = 720, FUN = mean, align = "right",fill=NA,by =720),
               rotatemean = rollapply(rotate, width = 720, FUN = mean, align = "right",fill=NA,by = 720),
               pressuremean = rollapply(pressure, width = 720, FUN = mean, align = "right",fill=NA, by = 720),
               vibrationmean = rollapply(vibration, width = 720, FUN = mean, align = "right",fill=NA,by = 720)) %>%
        filter(!is.na(voltmean))%>%
        select(date_col, voltmean, rotatemean, pressuremean, vibrationmean)%>%
        arrange(date_col)
    }
    
    
  })
  
  machine_fails <- reactive({
  Machine_failures <- failures[failures$machineID == input$machine,]
  Machine_failures$date_col <- substr(as.character(Machine_failures$datetime),1,10)
  Machine_failures$date_col <- as.Date(Machine_failures$date_col)
  Machine_failures$failure <- 1
  data.frame(Machine_failures)
  })
  output$out <- renderPlot({
    df <- machine_fails()
    ggplot(inputdf(),aes(date_col))+
      geom_line(aes(y=voltmean,color="volt"))+
      geom_line(aes(y=rotatemean,color="rotate"))+
      geom_line(aes(y=pressuremean,color="pressure"))+
      geom_line(aes(y=vibrationmean,color="vibration"))+
      xlab("Date")+
      ylab("Sensor output")+
      theme(axis.text.x=element_text(angle=90, hjust=1))+
      geom_vline(xintercept=as.numeric(df[which(df$failure==1),]$date_col))
  })
}


shinyApp(ui = ui, server = server)