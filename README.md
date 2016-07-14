# Visualization
Here I have 2 different Shiny Visualization
1. Univariate Analysis
2. Bivariate Analysis

###############################################Univariate Analysis########################################
## app.R ##
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

# Define a server for the Shiny app
server <- function(input, output) {
output$healthPlot1 <- renderPlot({
      ggplot(df,aes(df[,input$Variables]))+
      geom_bar(aes(fill=as.factor(df[,input$Variables])))+
      ylab("Count")
  })
}


shinyApp(ui = ui, server = server)

#################################################################################################################################

#######################################Bivariate Analysis#################################################################
## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)
#df is the dataframe for which bivariate Analysis is performed
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Health"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Variables", "Variables:",
                  choices=colnames(df[,-c(46,47)]))
    ),
    
    # Create a spot for the barplot
    mainPanel(width = 12,
              tabsetPanel(
                tabPanel("variable by health segment", plotOutput("healthPlot1")),
                tabPanel("variable by health subgroup", plotOutput("healthPlot2"))
              )
    )
    
  )
)




#Define a server for the Shiny app
server <- function(input, output) {
  # Fill in the spot we created for a plot
  output$healthPlot1 <- renderPlot({
    # Render a barplot
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
