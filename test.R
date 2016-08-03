library(shiny)
library(shinydashboard)
num <- 5
x <- rnorm(100,mean = 3.7,sd=1)
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Predictive Maintenance",titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Machine Status"),
                fluidRow(
                  column(3,
                       selectInput("machine","MachineID",choices = c("M1","M2","M3"))),
                  column(3,
                       selectInput("month","Month",choices = c("January","February","March","April","May","June","July",
                                                               "August","September","October","November","December"))),
                  column(3,
                       selectInput("sensor","Sensor",choices = c("Volt","Pressure","Rotation")))
                
              ),
              fluidRow(
                column(9,
                          box(40,title = "Histogram",background = "maroon",
                                plotOutput("out")
                              )
                      ),
                column(3,
                  fluidRow(
                           infoBoxOutput("error",width = 12)
                           ),
                  fluidRow(
                           infoBoxOutput("failure",width = 12)
                           )
                )
              )
              ))))
            


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
      "error",13,icon = icon("exclamation-triangle",lib = "font-awesome"),color = "orange"
    )
  })
  
  output$failure <- renderInfoBox({
    infoBox(
      "failure",6,icon = icon("thumbs-down",lib = "font-awesome"),color = "red"
    )
  })
}

shinyApp(ui,server)

