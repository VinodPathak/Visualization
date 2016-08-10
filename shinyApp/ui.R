library(shiny)
library(shinydashboard)
dashboardPage(skin = "purple",
              dashboardHeader(title = "Predictive Maintenance",titleWidth = 500),
              dashboardSidebar(width = 200,
                               sidebarMenu(
                                 menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard"))
                                 ) 
              ),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "dashboard",
                                box(tags$embed(src = "engine.jpg", width = "100px", height = "100px",align="right"),
                                    title = "Components Status",width = 4,height = 380,status = "primary",
                                    tags$hr(),
                                    tags$b(htmlOutput("rul")),br(),
                                    htmlOutput("comp1"),br(),
                                    htmlOutput("comp2"),br(),
                                    htmlOutput("comp3"),br(),
                                    htmlOutput("comp4"),
                                    tags$hr(),
                                    textOutput("note")))
                          
                  
                )
              )
)