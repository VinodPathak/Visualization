library(shinydashboard)
library(shiny)
library(shinyjs)
library(dplyr)
library(zoo)
library(ggplot2)

dashboardPage(skin = "green",
              dashboardHeader(title = "Predictive Maintenance",titleWidth = 300),
              dashboardSidebar(width = 150,
                               sidebarMenu(
                                 menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
                                 selectInput("machine","MachineID",choices=as.integer(levels(as.factor(df[,2])))),
                                 selectInput("year","Year",choices=as.integer(levels(as.factor(df[,9])))),
                                 selectInput("month","Month",choices = c("January","February","March","April","May","June","July",
                                                                         "August","September","October","November","December")),
                                 menuItem("RUL Dashboard",tabName = "rul",icon = icon("dashboard"),badgeColor = "green"),
                                 selectInput("engineid","EngineID",choices=as.character(levels(as.factor(rul_Data[,1])))),
                                 selectInput("sensor","Sensor",choices=as.character(names(rul_Data[,c(6:26)]))))
                    
              ),
              dashboardBody(
                useShinyjs(),
                tabItems(
                  tabItem(tabName = "dashboard",
                          fluidRow(
                                  box(width = 7,title = "Sensor history",status = "primary",
                                      tags$hr(),
                                        plotOutput("volt",width = 600,height = 100),
                                        plotOutput("rotate",width = 600,height = 100),
                                        plotOutput("pressure",width = 600,height = 100),
                                        plotOutput("vibration",width = 600,height = 100)),
                                  box(width=4,title="Machine Details:",status = "primary",
                                      tags$hr(),
                                      htmlOutput("machineid"),br(),
                                      htmlOutput("model"),br(),
                                      htmlOutput("age"),br(),
                                      htmlOutput("lastservice")))),
                  tabItem(tabName = "rul",
                          fluidRow(
                            column(width=4,
                            box(width=60,height = 500,title = "Machine Status",status = "primary",tags$hr(),
                       
                                       # checkboxGroupInput("condition", label = "Risk Labels",
                                       #                    choices=c("Critical"="P1",
                                       #                              "Sub Optimal"="P2",
                                       #                              "Optimal"="P3"),inline = T),
                                column(width=12,
                                       fluidRow(
                                         htmlOutput("c"),
                                         box(width = 3,background = "red",id="box1",
                                            img(src='conveyer.png', align = "left",height=50,width=40),
                                            textOutput("c1")),
                                         box(width=3,background = "red",
                                            img(src='conveyer.png', align = "center",height=50,width=40),
                                            textOutput("c2")),
                                         box(width=3,background = "red",
                                            img(src='conveyer.png', align = "right",height=50,width=40),
                                            textOutput("c3"))),
                                       fluidRow(
                                         htmlOutput("so"),
                                         box(width = 3,background = "yellow",
                                             img(src='conveyer.png', align = "left",height=50,width=40),
                                             textOutput("so1")),
                                         box(width=3,background = "yellow",
                                             img(src='conveyer.png', align = "center",height=50,width=40),
                                             textOutput("so2")),
                                         box(width=3,background = "yellow",
                                             img(src='conveyer.png', align = "right",height=50,width=40),
                                             textOutput("so3"))),
                                       fluidRow(
                                         htmlOutput("o"),
                                         box(width = 3,background = "green",
                                             img(src='conveyer.png', align = "left",height=50,width=40),
                                             textOutput("o1")),
                                         box(width=3,background = "green",
                                             img(src='conveyer.png', align = "center",height=50,width=40),
                                             textOutput("o2")),
                                         box(width=3,background = "green",
                                             img(src='conveyer.png', align = "right",height=50,width=40),
                                             textOutput("o3")))
                                      ))),
                            column(width=4,
                                      box(#tags$embed(src = "conveyer.png", width = "100px", height = "100px",align="left"),
                                      title = "Remaining Useful Life",width = 30,height = 210,status = "primary",
                                      tags$style(HTML("
                                                        hr {
                                                           display: block;
                                                           margin-top: 0.5em;
                                                           margin-bottom: 0.5em;
                                                           margin-left: auto;
                                                           margin-right: auto;
                                                           border-style: inset;
                                                           border-width: 1px;
                                                           
                                                           }")),
                                       tags$hr(),br(),
                                       
                                       fluidRow(
                                        box(width = 7,height = 50,
                                           htmlOutput("comp1"))),
                                       tags$hr(),
                                       textOutput("note"))),
                            column(width=8,
                                   box(title = "Sensor history",width = 800,height = 350,status = "primary",
                                       tags$hr(),
                                   plotOutput("sensor1",width = 750,height = 250)))
                          
                                      )))
                            
                          )
                  )