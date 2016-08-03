library(shiny)
library(shinydashboard)
library(ggplot2)
df<-data.frame("errors"=c("error1","error2","error3","error4"),value=c(2,3,2,6))
num <- 5
x <- rnorm(100,mean = 3.7,sd=1)
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Predictive Maintenance",titleWidth = 500),
                    dashboardSidebar(width = 200,
                      sidebarMenu(
                        menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
                        selectInput("machine","MachineID",choices = c("M1","M2","M3")),
                        selectInput("month","Month",choices = c("January","February","March","April","May","June","July",
                                                                "August","September","October","November","December")),
                        selectInput("sensor","Sensor",choices = c("Volt","Pressure","Rotation"))
                      ) 
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "dashboard",
                                h2("Sensor Output"),
                                fluidRow(
                                         box(width = 7,title = "Histogram",background = "maroon",
                                             plotOutput("out",width = 600,height = 200)
                                         ),
                                         column(4,
                                         fluidRow(
                                           infoBoxOutput("error",width = 8)
                                         ),
                                         fluidRow(
                                           infoBoxOutput("failure",width = 8)
                                         ))
                                  ),
                                fluidRow(
                                  box(width = 7,title = "Barchart",background = "blue",
                                  plotOutput("bar",width = 600,height = 200)
                                  )
                                )
                                )
                        )))



server <- function(input,output){
  val <- reactive({
    tryCatch(
      {
        return(10/"num")
      },
      error=function(er){
        message("No success")
        return(0)
      }
    )
  })
  output$text1 <- renderText({
    paste("Machine ID: ",input$machine)
  })
  output$text2 <- renderText({
    paste("No of Failures: ")
  })
  output$out <- renderPlot({
    hist(x)
  })
  output$error <- renderInfoBox({
    infoBox(
      "Errors",13,icon = icon("exclamation-triangle",lib = "font-awesome"),color = "orange"
    )
  })
  
  output$failure <- renderInfoBox({
    infoBox(
      "Failures",6,icon = icon("thumbs-down",lib = "font-awesome"),color = "red"
    )
  })
  output$bar <- renderPlot({
    ggplot(df,aes(errors))+geom_bar(aes(y=value,fill=errors),stat="identity")
  })
}

shinyApp(ui,server)

