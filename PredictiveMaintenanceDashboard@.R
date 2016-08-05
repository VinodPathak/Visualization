## app.R ##
library(shinydashboard)
library(AnomalyDetection)
library(dplyr)
library(zoo)
library(ggplot2)


ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Predictive Maintenance For Aerospace Machines",titleWidth = 500),
                    dashboardSidebar(width = 200,
                                     sidebarMenu(
                                       menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
                                       selectInput("machine","MachineID",choices=as.integer(levels(as.factor(df[,2])))),
                                       selectInput("year","Year",choices=as.integer(levels(as.factor(df[,9])))),
                                       selectInput("month","Month",choices = c("January","February","March","April","May","June","July",
                                                                             "August","September","October","November","December")),
                                       selectInput("sensor","Sensor",choices = as.character(names(df[,c(3:6)])))
                                     ) 
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "dashboard",
                                #box(background = "aqua",width = 200,height = 800,
                                fluidRow(
                                  box(width = 7,title = "Sensor Output",background = "blue",status = "primary",
                                      plotOutput("out",width = 600,height = 200)
                                  ),box(width=3,title="Machine Details:",background = "olive",status = "primary",
                                        textOutput("machineid"),br(),
                                        textOutput("model"),br(),
                                        textOutput("age")
                                  )
                                ),
                                fluidRow(
                                  box(width = 5,title = "Error Count Per Month",background = "blue",status = "primary",
                                      plotOutput("errors",width = 400,height = 200)
                                  ),
                                  column(width=4,
                                         tags$head(tags$style(HTML('.info-box {min-height: 55px;} .info-box-icon {height: 55px; line-height: 55px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
                                         fluidRow(
                                           infoBoxOutput("error",width = 8)
                                         ),
                                         fluidRow(
                                           infoBoxOutput("failure",width = 8)
                                         )
                                  )
                                  
                                )
                        )
                        )
                      ))
    
  

server <- function(input, output) {
  
  
  inputdf <- reactive({
    dynamicdf <- df[df$machineID==input$machine & df$month==input$month & df$year==input$year,]
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
 
  
  fail_val <- reactive({
    tryCatch(
      {
        Machine_failures <- failures[failures$machineID == input$machine & failures$month==input$month,]
        Machine_failures$failure <- 1
        return(sum(Machine_failures$failure))
      },
      error=function(er){
        message("No success")
        return(0)
      }
    )
  })
  # machine_fails <- reactive({
  #   Machine_failures <- failures[failures$machineID == input$machine,]
  #   Machine_failures$date_col <- substr(as.character(Machine_failures$datetime),1,10)
  #   Machine_failures$date_col <- as.Date(Machine_failures$date_col)
  #   Machine_failures$failure <- 1
  #   data.frame(Machine_failures)
  # })
  
 
  output$out <- renderPlot({
    ggplot(inputdf(),aes(date_col,color=names(inputdf())[2]))+
      geom_line(aes(y=inputdf()[,2]))+
      xlab("Date")+
      ylab(names(inputdf())[2])+
      labs(colour="Sensor")
  })
  
  output$error <- renderInfoBox({
    infoBox(
      "Errors",sum(errorsdf()[,2]),icon = icon("exclamation-triangle",lib = "font-awesome"),color = "orange"
    )
  })
    
  output$failure <- renderInfoBox({
    infoBox(
      "Failures",fail_val(),icon = icon("thumbs-down",lib = "font-awesome"),color = "red"
    )
    
  })
  
  errorsdf <- reactive({
    tryCatch(
      {
        Machine_errors <- errors[errors$machineID == input$machine & errors$month==input$month,]
        Machine_errors <- data.frame(table(Machine_errors$errorID))
        names(Machine_errors) <- c("errorID","No_of_Errors")
        return(Machine_errors)
      },
      error=function(er){
        message("No success")
        return(0)
      }
    )
  })
  
 
  
  output$errors <- renderPlot({
    ggplot(errorsdf(),aes(errorID))+geom_bar(aes(y=No_of_Errors,fill=errorID),stat = "identity")+
      ylab("Error Count")+coord_flip()
  })
  
  
  purja <- reactive({
   machine[machine$machineID==input$machine,c(1,2,3)]

  })
  
  output$machineid <- renderText({
    paste("MachineID:",purja()[,1],sep = " ")
  })
  output$model <- renderText({
    paste("Model:",purja()[,2],sep = " ")
  })
  output$age <- renderText({
    paste("Age: ",purja()[,3],"years",sep = " ")
  })
}

shinyApp(ui, server)