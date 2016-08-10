server <- function(input,output){
  output$rul <- renderText({
    paste("&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;","Remaining Useful Life")
  })
  output$comp1 <- renderText({
    color_code <- "#008000"
    comp1_days <- 5
    if(comp1_days<24){
      color_code <- "#FF0000"
    }
    paste("Component 1:&emsp;","<font color=",color_code,"><b>",comp1_days,"Hours","</b></font>",sep=" ")
  })
  output$comp2 <- renderText({
    color_code <- "#008000"
    comp2_days <- 15
    if(comp2_days<24){
      color_code <- "#FF0000"
    }
    paste("Component 2:&emsp;","<font color=",color_code,"><b>",comp2_days,"Hours","</b></font>",sep = " ")
  })
  output$comp3 <- renderText({
    color_code <- "#008000"
    comp3_days <- 25
    if(comp3_days<24){
      color_code <- "#FF0000"
    }
    paste("Component 3:&emsp;","<font color=",color_code,"><b>",comp3_days,"Hours","</b></font>",sep = " ")
  })
  output$comp4 <- renderText({
    color_code <- "#008000"
    comp4_days <- 30
    if(comp4_days<24){
      color_code <- "#FF0000"
    }
    paste("Component 4:&emsp;","<font color=",color_code,"><b>",comp4_days,"Hours","</b></font>",sep = " ")
  })
  output$note <- renderText({
    paste("Note: Time greater than 24 hours is crirtical")
  })
}
