library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
setwd("C:\\Users\\vinod.pathak\\Desktop\\Vinod Pathak\\Cohort analysis")
cohortdata <- read.csv("Online Retail.csv")
cohortdata <- cohortdata[complete.cases(cohortdata),]

cohortdata <- cohortdata[cohortdata$UnitPrice>0,]
cohortdata <- cohortdata[cohortdata$Quantity>0,]
#Data Preparation
#Select Columns
cohortdata <- cohortdata[,c(4:7)]
#Create Expenditure columns
cohortdata$expenditure <- cohortdata[,1]*cohortdata[,3]
cohortdata$InvoiceDate <- as.Date(cohortdata$InvoiceDate,format = "%d-%m-%Y")


str(cohortdata)
cohortdata <- cohortdata[,c(2,4,5)]
cohortdata$InvoiceDate <- as.character(cohortdata$InvoiceDate)
cohortdata$InvoiceDate <- substr(cohortdata$InvoiceDate,1,10)

cohortdata <- cohortdata %>%
  filter(InvoiceDate>as.Date("2010-12-31"))

write.csv(cohortdata,"cohortdata.csv",row.names = F)

####################################################################

cohortdata <- read.csv("C:\\Users\\vinod.pathak\\Desktop\\Vinod Pathak\\Cohort analysis\\cohortdata.csv")
cohortdata$InvoiceDate <- as.Date(cohortdata$InvoiceDate,format = "%Y-%m-%d")
str(cohortdata)
#Filter out 2012 data


cohortdata1 <- cohortdata %>%
  group_by(InvoiceDate,CustomerID)%>%
  summarise(Total_expen=sum(expenditure))%>%
  ungroup()

cohortdata1$month <-  month(cohortdata1$InvoiceDate)
cohortdata1$quarter <- ""
cohortdata1$quarter <- ifelse(cohortdata1$month %in% c(1,2,3),"Quarter1",
                      ifelse(cohortdata1$month %in% c(4,5,6),"Quarter2",
                      ifelse(cohortdata1$month %in% c(7,8,9),"Quarter3","Quarter4")))
                      

####Create cohorts
temp <- cohortdata1 %>%
  group_by(CustomerID)%>%
  arrange(InvoiceDate)

temp <- temp[!duplicated(temp$CustomerID),]
temp$cohort <- paste("cohort",ifelse(month(temp$InvoiceDate)<10,
                                     paste("0",month(temp$InvoiceDate),sep = ""),
                                     month(temp$InvoiceDate)),sep = "")

temp <- temp[,c(2,6)]

cohort_dataset <- merge(cohortdata1,temp,by = intersect(names(cohortdata1),names(temp)))
str(cohort_dataset)

#Restructure Month
cohort_dataset$month <- paste("MON",ifelse(cohort_dataset$month<10,
                              paste("0",cohort_dataset$month,sep = ""),
                              cohort_dataset$month),sep = "")
write.csv(cohort_dataset[,c(6,4,5,3)],"cohortdatanew.csv",row.names = F)
##################################################################################
cohort_dataset <- read.csv("C:\\Users\\vinod.pathak\\Desktop\\Vinod Pathak\\Cohort analysis\\cohortdatanew.csv")

########################################################################################
########################################################################################
##########################Monthly Cohort Analysis ####################################

cohort2 <- cohort_dataset %>%
  group_by(cohort,month)%>%
  summarise(Avg_Expenditure=mean(Total_expen))%>%
  ungroup()

temp2 <- dcast(cohort2, cohort~ month,value.var = "Avg_Expenditure")
#convert all NA to zeros
temp2[is.na(temp2)] <- 0

temp2.r <- temp2 #create new data frame
totcols <- ncol(temp2.r) #count number of columns in data set
for (i in 1:nrow(temp2.r)) { #for loop for shifting each row
  df <- temp2.r[i,] #select row from data frame
  df <- df[ , !df[]==0] #remove columns with zeros
  partcols <- ncol(df) #count number of columns in row (w/o zeros)
  #fill columns after values by zeros
  if (partcols < totcols) df[, c((partcols+1):totcols)] <- 0
  temp2.r[i,] <- df #replace initial row by new one
}

cohort.chart1 <- melt(temp2.r, id.vars = 'cohort')
colnames(cohort.chart1) <- c('cohort', 'month', 'Avg_Expenditure')
write.csv(cohort.chart1,"cohortdata.csv",row.names = F)


cohort.chart1$cohort <- as.factor(cohort.chart1$cohort)
cohort.chart1 <- filter(cohort.chart1, Avg_Expenditure != 0)
linechart <- ggplot(cohort.chart1, aes(x=month, y=Avg_Expenditure, group=cohort, colour=cohort))
linechart <- linechart + geom_line(size=2, alpha=1/2) +
  geom_point(size=3, alpha=1) +
  #geom_smooth(aes(group=1), method = 'loess', size=2, colour='red', se=FALSE) +
  labs(title="Cohorts Average Expenditure  dynamics")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplotly(linechart)
# 


#cycle plot
cohort.chart2 <- cohort.chart1
cohort.chart2 <- mutate(cohort.chart2, month_cohort = paste(month,cohort,sep = ""))
cycleplot <- ggplot(cohort.chart2, aes(x=month_cohort, y=Avg_Expenditure, group=month, colour=month))
cycleplot <- cycleplot + geom_point(size=3) +
  geom_line(aes(group=month), size=2, alpha=1/2) +
  labs(title="Cohorts expenditure ratio cycle plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10))
ggplotly(cycleplot)

#we need to melt data
cohort.chart3 <- melt(temp2, id.vars = "cohort")
colnames(cohort.chart3) <- c('cohort', 'month', 'Avg_Expenditure')
#define palette
blues <- colorRampPalette(c('lightgreen', 'blue'))
#plot data
areachart <- ggplot(cohort.chart3, aes(x=month, y=Avg_Expenditure, group=cohort))
areachart <- areachart + geom_area(aes(fill = cohort)) +
  scale_fill_manual(values = blues(nrow(temp2))) +
  ggtitle('Average Expenditure by Cohort(Area Chart)')+
  theme(axis.text.x = element_text(angle=90, hjust=1))
ggplotly(areachart)


## plot data
library(plotly)
heatmap <- ggplot(cohort.chart1, aes(month,cohort )) +
  geom_tile(aes(fill = Avg_Expenditure)) + 
  geom_text(aes(label = round(Avg_Expenditure, 1))) +
  scale_fill_gradient(low = "white", high = "red") +
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits = rev(levels(cohort.chart1$cohort)))
  #theme(legend.position="bottom", 
  #      axis.text.x=element_text(angle=90, hjust=0.5,family="mono"))
ggplotly(heatmap)


########################################################################################
########################################################################################
##########################Quarterly Cohort Analysis ####################################


qcohort2 <- cohort_dataset %>%
  group_by(cohort,quarter)%>%
  summarise(Avg_Expenditure=mean(Total_expen))%>%
  ungroup()

qtemp2 <- dcast(qcohort2, cohort~ quarter,value.var = "Avg_Expenditure")
#convert all NA to zeros
qtemp2[is.na(qtemp2)] <- 0

qtemp2.r <- qtemp2 #create new data frame
totcols <- ncol(qtemp2.r) #count number of columns in data set
for (i in 1:nrow(qtemp2.r)) { #for loop for shifting each row
  df <- qtemp2.r[i,] #select row from data frame
  df <- df[ , !df[]==0] #remove columns with zeros
  partcols <- ncol(df) #count number of columns in row (w/o zeros)
  #fill columns after values by zeros
  if (partcols < totcols) df[, c((partcols+1):totcols)] <- 0
  qtemp2.r[i,] <- df #replace initial row by new one
}

qcohort.chart1 <- melt(qtemp2.r, id.vars = 'cohort')
colnames(qcohort.chart1) <- c('cohort', 'quarter', 'Avg_Expenditure')
#write.csv(qcohort.chart1,"cohortdata.csv",row.names = F)


qcohort.chart1$cohort <- as.factor(qcohort.chart1$cohort)
qcohort.chart1 <- filter(qcohort.chart1, Avg_Expenditure != 0)
qlinechart <- ggplot(qcohort.chart1, aes(x=quarter, y=Avg_Expenditure, group=cohort, colour=cohort))
qlinechart <- qlinechart + geom_line(size=2, alpha=1/2) +
  geom_point(size=3, alpha=1) +
  labs(title="Cohorts Average Expenditure  dynamics")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplotly(qlinechart)
# 


#cycle plot
qcohort.chart2 <- qcohort.chart1
qcohort.chart2 <- mutate(qcohort.chart2, quarter_cohort = paste(quarter,cohort,sep = ""))
qcycleplot <- ggplot(qcohort.chart2, aes(x=quarter_cohort, y=Avg_Expenditure, group=quarter, colour=quarter))
qcycleplot <- qcycleplot + geom_point(size=3) +
  geom_line(aes(group=quarter), size=2, alpha=1/2) +
  labs(title="Cohorts expenditure ratio cycle plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10))
ggplotly(qcycleplot)

#we need to melt data
qcohort.chart3 <- melt(qtemp2, id.vars = "cohort")
colnames(qcohort.chart3) <- c('cohort', 'quarter', 'Avg_Expenditure')
#define palette
blues <- colorRampPalette(c('lightgreen', 'blue'))
#plot data
qareachart <- ggplot(qcohort.chart3, aes(x=quarter, y=Avg_Expenditure, group=cohort))
qareachart <- qareachart + geom_area(aes(fill = cohort)) +
  scale_fill_manual(values = blues(nrow(qtemp2))) +
  ggtitle('Average Expenditure by Cohort(Area Chart)')+
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplotly(qareachart)


## plot data
library(plotly)
qheatmap <- ggplot(qcohort.chart1, aes(quarter,cohort )) +
  geom_tile(aes(fill = Avg_Expenditure)) + 
  geom_text(aes(label = round(Avg_Expenditure, 1))) +
  scale_fill_gradient(low = "white", high = "red") +
  scale_x_discrete(position = "top")+
  scale_y_discrete(limits = rev(levels(qcohort.chart1$cohort)))
#theme(legend.position="bottom", 
#      axis.text.x=element_text(angle=90, hjust=0.5,family="mono"))
ggplotly(qheatmap)