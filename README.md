# Visualization
Here I have 2 different Shiny Visualization
1. Univariate Analysis
2. Bivariate Analysis

#Univariate Analysis
```
library(shinydashboard)
library(shiny)
library(ggplot2)


ui <- fluidPage(    
  titlePanel("Health"),
  sidebarLayout(      
    sidebarPanel(
      selectInput("Variables", "Variables:",
                  choices=colnames(df))#df is the dataframe
    ),
    mainPanel(width = 6,
              tabsetPanel(
                tabPanel("Univariate Analysis", plotOutput("healthPlot1"))
              )
    )
  
  )
)


########Legend setting
library(ggplot2)
ggplot(df[1:5000,],aes(x=datetime))+
  geom_line(aes(y=df[1:5000,3],color="volt"))+
  geom_line(aes(y=df[1:5000,4],color="rotate"))+
  geom_line(aes(y=df[1:5000,5],color="pressure"))+
  geom_line(aes(y=df[1:5000,6],color="vibration"))+
  xlab("Date")+
  scale_colour_manual("", 
                      breaks = c("volt","rotate", "pressure", "vibration"),
                      values = c("coral", "darkorchid", "blue","violet")) 

##### Define a server for the Shiny app
server <- function(input, output) {
output$healthPlot1 <- renderPlot({
      ggplot(df,aes(df[,input$Variables]))+
      geom_bar(aes(fill=as.factor(df[,input$Variables])))+
      ylab("Count")
  })
}


shinyApp(ui = ui, server = server)

#################################################################################################################################

#Bivariate Analysis

library(shinydashboard)
library(shiny)
library(ggplot2)

ui <- fluidPage(    
  
  titlePanel("Health"),
  sidebarLayout(      
    
    sidebarPanel(
      selectInput("Variables", "Variables:",
                  choices=colnames(df[,-c(46,47)]))
    ),
    
    mainPanel(width = 12,
              tabsetPanel(
                tabPanel("variable by health segment", plotOutput("healthPlot1")),
                tabPanel("variable by health subgroup", plotOutput("healthPlot2"))
              )
    )
    
  )
)


server <- function(input, output) {
  output$healthPlot1 <- renderPlot({
    ggplot(df,aes(df[,input$Variables]))+
      geom_bar(aes(fill=as.factor(df[,input$Variables])))+
      facet_wrap(~segment)+
      ylab("Count")
  })
  output$healthPlot2 <- renderPlot({
    ggplot(df,aes(df[,input$Variables]))+
      geom_bar(aes(fill=as.factor(df[,input$Variables])))+
      facet_wrap(~subgroup)+
      ylab("Count")
  })
  
}


shinyApp(ui = ui, server = server)
```
