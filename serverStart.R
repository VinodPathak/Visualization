library(plumber)
r <- plumb("C:\\Users\\vinod.pathak\\Desktop\\Vinod Pathak\\Cohort analysis\\postfile.R")  # Where 'myfile.R' is the location of the file shown above
r$run(port=8000)
