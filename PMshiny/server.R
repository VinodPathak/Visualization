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
      newdf <- dynamicdf[,c(1:5)]
      newdf
  })
  
  
  # fail_val <- reactive({
  #   tryCatch(
  #     {
  #       Machine_failures <- failures[failures$machineID == input$machine & failures$month==input$month,]
  #       Machine_failures$failure <- 1
  #       return(sum(Machine_failures$failure))
  #     },
  #     error=function(er){
  #       message("No success")
  #       return(0)
  #     }
  #   )
  # })
  
  output$volt <- renderPlot({
    ggplot(inputdf(),aes(date_col))+
      geom_line(aes(y=inputdf()[,2],color="volt"))+
    xlab("Date")+
      ylab("Sensor output")+
      theme(legend.title=element_blank())
  })
  
  
  output$rotate <- renderPlot({
    ggplot(inputdf(),aes(date_col))+
      geom_line(aes(y=inputdf()[,3],color="rotate"))+
    xlab("Date")+
      ylab("Sensor output")+
      theme(legend.title=element_blank())
  })
      
  output$pressure <- renderPlot({
    ggplot(inputdf(),aes(date_col))+
      geom_line(aes(y=inputdf()[,4],color="pressure"))+
    xlab("Date")+
      ylab("Sensor output")+
      theme(legend.title=element_blank())
  })
        
  output$vibration <- renderPlot({
    ggplot(inputdf(),aes(date_col))+
      geom_line(aes(y=inputdf()[,5],color="vibration"))+
      xlab("Date")+
      ylab("Sensor output")+
      theme(legend.title=element_blank())
  })
 
  
  # errorsdf <- reactive({
  #   tryCatch(
  #     {
  #       Machine_errors <- errors[errors$machineID == input$machine & errors$month==input$month,]
  #       Machine_errors <- data.frame(table(Machine_errors$errorID))
  #       names(Machine_errors) <- c("errorID","No_of_Errors")
  #       return(Machine_errors)
  #     },
  #     error=function(er){
  #       message("No success")
  #       return(0)
  #     }
  #   )
  # })
  
  purja <- reactive({
     machine[machine$machineID==input$machine,c(1,2,3)]
  })
  
  output$machineid <- renderText({
    paste("<p align='justify'>MachineID:&emsp;",purja()[,1],"</p>")
  })
  output$model <- renderText({
    paste("<p align='justify'>Model:&emsp;",purja()[,2],"</p>")
  })
  output$age <- renderText({
    paste("<p align='justify'>Age:&emsp; ",purja()[,3],"years","</p>")
  })
  
  output$last_rep <- renderText({
    paste("<p align='justify'>Last Component Replacement time:&emsp;",15,"hours","</p>")
  })
  
  output$rul <- renderText({
    paste("&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;","Remaining Useful Life")
  })
  
  
  
  comp1_days <-  reactive({
   rul[rul$machineid==input$engineid,]
  })
  
  

  output$comp1 <- renderText({
    color_code <- "#008000"
    alert <- "Normal"
    if(comp1_days()[,3][1]=="Critical"){
      color_code <- "#FF0000"
      alert <- "Critical"
    }
    paste("Engine:&emsp;",input$engineid,"<font color=",color_code,"><b><br/>",comp1_days()[,2][1],"Days","</b></font>","<br/>","<br/>",
          "<br/>","Status:&emsp; ",alert)
  })
 

  
######################RUL######################################
  rul_dataset <- reactive({
    rul_Data[rul_Data$id==input$engineid,c("id","cycle",input$sensor)]
  })
  
###################For Critical
  critical <- reactive({
  rul[which(rul[,3]=="Critical"),1][1:3]
  #Optimal <- rul[which(rul$Status=="Optimal"),1]
  #Sub-Optimal <- rul[which(rul$Status=="Sub-Optimal"),1]
  })
  output$c <- renderText({
    paste("<font size='3'>Critical</font>")
  })
  output$c1 <- renderText({
    paste(critical()[1])
  })
  output$c2 <- renderText({
    paste(critical()[2])
  })
  output$c3 <- renderText({
    paste(critical()[3])
  })
  
  #################For sub optimal
  
  suboptimal <- reactive({
    rul[which(rul$Status=="Sub-Optimal"),1]
  })
  output$so <- renderText({
    paste("<font size='3'>Sub-Optimal</font>")
  })
  output$so1 <- renderText({
    paste(suboptimal()[1])
  })
  output$so2 <- renderText({
    paste(suboptimal()[2])
  })
  output$so3 <- renderText({
    paste(suboptimal()[3])
  })
  
  #################Optimal
  optimal <- reactive({
    rul[which(rul$Status=="Optimal"),1]
    
  })
  output$o <- renderText({
    paste("<font size='3'>Optimal</font>")
  })
  output$o1 <- renderText({
    paste(optimal()[1])
  })
  output$o2 <- renderText({
    paste(optimal()[2])
  })
  output$o3 <- renderText({
    paste(optimal()[3])
  })
  
  #################################################
  output$sensor1 <- renderPlot({
    ggplot(rul_dataset(),aes(cycle))+
      geom_line(aes(y=rul_dataset()[,3],color=names(rul_dataset()[3])))+
      xlab("Days")+
      ylab("Sensor Output")+
      theme(legend.title=element_blank())
  })
  

  
  
  last_serve<- reactive({
    recent_Data <- maint[maint$machineID==input$machine,]
    temp2 <- recent_Data %>%
      arrange(machineID,rev(datetime))
    temp2 <- temp2[!duplicated(temp2$comp),]
    temp2 <- temp2 %>%
      arrange(comp)
    tempdate <- as.POSIXct("2016-01-01 06:00:00",format="%Y-%m-%d %H:%M:%S",tz="UTC")
    temp2$days <- tempdate-temp2[,1]
    temp2
  })
  
  output$lastservice <- renderText({
    tryCatch(
      {
        paste("Recent Components Serviced:","<br/>","<br/>","&emsp;","&emsp;",
          last_serve()[,3][1],"&emsp;:",last_serve()[,4][1],"days ago","<br/>","<br/>","&emsp;","&emsp;",
        last_serve()[,3][2],"&emsp;:",last_serve()[,4][2],"days ago","<br/>","<br/>","&emsp;","&emsp;",
        last_serve()[,3][3],"&emsp;:",last_serve()[,4][3],"days ago","<br/>","<br/>","&emsp;","&emsp;",
        last_serve()[,3][4],"&emsp;:",last_serve()[,4][4],"days ago")
      },
      error=function(er){
        message("No success")
        return(0)
      }
    )
  })
  
  
  
  
}

