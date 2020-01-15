####--------Setting Up the Directory and Loading the Data-----####

getwd()

setwd("F:/altrSys/RFM UK Clustering")

library(xlsx)

Data <- readxl::read_xlsx("Online Retail.xlsx") 
colSums(is.na(Data))
####------Filtering Data only for U.K Customers------------####
library(plyr)
library(dplyr)
library(bindrcpp)
library(data.table)  ## Data table for segmenting the customers
library(ggplot2)     ## For Plots and Graphs
library(tidyr)       ## For Segmentaiton
library(knitr)       ## For pipelining during data mini
library(lubridate) 

##--------------------------------------------


DataUK <- dplyr::filter(Data, Country == "United Kingdom")

####------- DATA UNDERSTANDING AND DATA PREPERATION -----####
summary(DataUK)



DataUK$Quantity <-  ifelse(DataUK$Quantity <= 0,NA,DataUK$Quantity)
DataUK$UnitPrice <-  ifelse(DataUK$UnitPrice <= 0,NA,DataUK$UnitPrice)

##---Chechking for NA's -----###
colSums(is.na(DataUK))

##---Dropping All NA's--###
DataUK <- DataUK %>%
  drop_na()  

##----Amount Variable for Total price ----#####
DataUK$Amount <- DataUK$Quantity * DataUK$UnitPrice

##------SKU Variable-------####
DataUK$SKU <- substr(DataUK$StockCode,1,3)
##---Converting Into Relevent Classess------###
eRetail <- DataUK %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode),           
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))

DataUK$Time <-  hour(DataUK$InvoiceDate)
####---------Exploratory Data Analysis-------------------####
#install.packages("lubridate")
library(lubridate)

EDA <- DataUK

EDA$dayofWeek <- wday(DataUK$InvoiceDate, label = T)
EDA$Month <-  month(DataUK$InvoiceDate , label = T)
EDA$Year <-  year(DataUK$InvoiceDate)


# Checking for Revenue with respect to the Day
EDA %>%
  group_by(dayofWeek) %>%
  summarise(Revenue = sum(Amount)) %>%
  ggplot(aes(x = dayofWeek, y = Revenue)) + geom_col() + labs(x = 'Day of Week', y = 'Revenue', title = 'Revenue by Day of Week')

Dayofweek <-EDA %>%
  group_by(dayofWeek) %>%
  summarise(Revenue = sum(Amount))
write.csv(Dayofweek,file = "DayofweekRevenue.csv", row.names = F)


## Checking for Customers with respect to the day 
EDA %>%
  group_by(dayofWeek) %>%
  summarise(Customer = n()) %>%
  ggplot(aes(x = dayofWeek, y = Customer)) + geom_col() + labs(x = 'Day of Week', y = 'Customer', title = 'Customers by Day of Week')

Customers <-EDA %>%
  group_by(dayofWeek) %>%
  summarise(Customer = n())

write.csv(Customers,file = "DayofWeekCustomers.csv", row.names = F)

###-----Checking for Revenue with respect to the Month
EDA %>%
  group_by(Month) %>%
  summarise(Revenue = sum(Amount)) %>%
  ggplot(aes(x = Month, y = Revenue)) + geom_col() + labs(x = 'Day of Week', y = 'Revenue', title = 'Revenue by Day of Week')

MonthRevenue <-EDA %>%
  group_by(Month) %>%
  summarise(Revenue = sum(Amount))

write.csv(MonthRevenue,file = "MonthRevenue.csv", row.names = F)


## Checking for Customers with respect to the day 
EDA %>%
  group_by(Month) %>%
  summarise(Customer = n()) %>%
  ggplot(aes(x = Month, y = Customer)) + geom_col() + labs(x = 'Day of Week', y = 'Customer', title = 'Customers by Day of Week')

CustomersMonthRevenue <-EDA %>%
  group_by(Month) %>%
  summarise(Customer = n())

write.csv(CustomersMonthRevenue,file = "CustomersMonth.csv", row.names = F)

###-------Time of the day -----###
###-----Checking for Revenue with respect to the Month
EDA %>%
  group_by(Time) %>%
  summarise(Revenue = sum(Amount)) %>%
  ggplot(aes(x = Time, y = Revenue)) + geom_col() + labs(x = 'Day of Week', y = 'Revenue', title = 'Revenue by Day of Week')

TimeRevenue <-EDA %>%
  group_by(Time) %>%
  summarise(Revenue = sum(Amount))

write.csv(TimeRevenue,file = "TimeRevenue.csv", row.names = F)


## Checking for Customers with respect to the day 
EDA %>%
  group_by(Time) %>%
  summarise(Customer = n()) %>%
  ggplot(aes(x = Time, y = Customer)) + geom_col() + labs(x = 'Day of Week', y = 'Customer', title = 'Customers by Day of Week')

CustomersTimeRevenue <-EDA %>%
  group_by(Time) %>%
  summarise(Customer = n())

write.csv(CustomersTimeRevenue,file = "CustomersTimeRevenue .csv", row.names = F)


###-----Building Datasets for RFM Analysis -----###
getRFMdf<-function (RFM_raw){
  RFM_raw <- RFM_raw[!duplicated(RFM_raw$CustomerID),]
  RFM_raw <- cbind(RFM_raw, First_date = with(df,
                                              as.Date(as.integer(by(InvoiceDate, CustomerID, min)), "1970/01/01")))
  RFM_raw <- cbind(RFM_raw, Last_date = with(df,
                                             as.Date(as.integer(by(InvoiceDate, CustomerID, max)), "1970/01/01")))
  #Recency
  AsOfDate <- max(RFM_raw$Last_date)
  RFM_raw <- cbind(RFM_raw, Recency = with(df,
                                           as.numeric(difftime(AsOfDate,RFM_raw$Last_date,units="days")))/30)
  #First_purchase
  RFM_raw <- cbind(RFM_raw, First_purchase = with(df,
                                                  as.numeric(difftime(AsOfDate,RFM_raw$First_date,units="days")))/30)
  #Frequency
  RFM_raw <- cbind(RFM_raw, Frequency = with(df,
                                             as.numeric(by(InvoiceNo, CustomerID, function(x) length(unique(x))))))
  #Monetary & related
  RFM_raw <- cbind(RFM_raw, Monetary = with(df,
                                            as.numeric(by(Amount, CustomerID, sum))))
  RFM_raw <- cbind(RFM_raw, AvgM = with(df,
                                        as.numeric(by(Amount, CustomerID, mean))))
  RFM_raw <- cbind(RFM_raw, maxM = with(df,
                                        as.numeric(by(Amount, CustomerID, max))))
  #Breadth
  RFM_raw <- cbind(RFM_raw, Breadth = with(df,
                                           as.numeric(by(SKU, CustomerID, function(x) length(unique(x))))))
  #Tenure
  RFM_raw <- cbind(RFM_raw, Tenure = with(df, as.numeric(difftime(RFM_raw$Last_date,RFM_raw$First_date,units="days")))/30)
  #sum Quantity
  RFM_raw <- cbind(RFM_raw, sumQuant = with(df,
                                            as.numeric(by(Quantity, CustomerID, mean))))
}

#### Define for Getfmnor ----###

getRFMnor<-function (RFMn){
  RFMn<- as.data.frame(scale(df2[15:21], center= TRUE))
  RFMn<- cbind(df2[,c(1:14)],RFMn)
  }

##_------RFM Score -----###
#score 1 to 5
score15<-function(x){
  ceiling((rank(x))/(length(x))*5)
}
getRFMscore<-function (RFMs){
  RFMs <- as.data.frame(lapply(df3[,c(13:21)], score15))
  RFMs <- cbind(df3[,c(1:12)], R= ceiling((rank(-df3$R))/(length(df3$R))*5), RFMs)
  RFMs <- cbind(RFMs,RFMScore = 100*RFMs$R + 10*RFMs$Fq+RFMs$M)
}
###------RFM Score -----###
df<- eRetail
rawRFM<-as.data.frame(getRFMdf(df))

###---Excluding Outliers ----###
RFM<-subset(rawRFM,rawRFM$Recency<= 12 & rawRFM$Frequency<= 25 & rawRFM$Monetary>= 0 & rawRFM$Monetary<= 10000)
summary(rawRFM$Monetary)


####---------------Dta Normalization----###

df2<- RFM
nRFM<-as.data.frame(getRFMnor(df2))
RFMn<- dplyr::rename(nRFM, "R"="Recency" , "Fq"="Frequency"  ,"M"= "Monetary"  ,"B" = "Breadth" ,"Ten"= "Tenure"  , "Q"="sumQuant")
nRFM<- dplyr::rename(nRFM, "R"="Recency" , "Fq"="Frequency"  ,"M"= "Monetary"  ,"B" = "Breadth" ,"Ten"= "Tenure"  , "Q"="sumQuant")

#score
df3 <- nRFM

RFMs<-as.data.frame(getRFMscore(df3))
RFMs[,14] <- NULL

par(mfrow = c(1,3))
hist(RFMs$R)
hist(RFMs$Fq)
hist(RFMs$M)


#####K-means cluster analysis
####Visulize the clusters

## Decide Number of Clusters 
library(cluster) # Needed for silhouette function

RFM_cluster10 <- data.frame(nRFM$R,nRFM$Fq,nRFM$M)
# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()

minClust <- 3      # Hypothesized minimum number of segments
maxClust <- 10     # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11)            # For reproducibility
  km.out[i] <- list(kmeans(RFM_cluster10, centers = centr))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(RFM_cluster10)))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}

#Next, we plot the silhouette average widths for the choice of clusters. The best cluster is the one with the 
#largest silhouette average width, which turns out to be 3 clusters.

# Plot silhouette results to find best number of clusters; closer to 1 is better
library(ggplot2)
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

###-----K-Means Clustering for 3 Clusters -----#####
RFM_cluster <- data.frame(nRFM$R,nRFM$Fq,nRFM$M)
km <- kmeans(RFM_cluster,centers=3)
RFM_cluster$cluster <- as.factor(km$cluster)
RFM_cluster <- cbind(RFM_cluster,RFMs)



ggplot(RFM_cluster,aes(x=nRFM.R, y=nRFM.M, color= cluster,size= nRFM.Fq))+geom_point()+ scale_size_area(max_size=10)+labs(x="Recency", y="Monetary")

ggplot(RFM_cluster,aes(x=R, y= M, color= cluster,size= Fq))+geom_point()+ scale_size_area(max_size=20)+labs(x="Recency", y="Monetary")

RFM_cluster1<-RFM_cluster[which(RFM_cluster$cluster==1),]
RFM_cluster2<-RFM_cluster[which(RFM_cluster$cluster==2),]
RFM_cluster3<-RFM_cluster[which(RFM_cluster$cluster==3),]


cluster11 <- ddply(RFM_cluster1, .(StockCode,Description), summarize, sumAmount= sum(Amount), sumQuantity= sum(Quantity), nCustomer= length(unique(CustomerID)), nPurchase= length(unique(InvoiceNo)) )

cluster22 <- ddply(RFM_cluster2, .(StockCode,Description), summarize, sumAmount= sum(Amount), sumQuantity= sum(Quantity), nCustomer= length(unique(CustomerID)), nPurchase= length(unique(InvoiceNo)) )

cluster33 <- ddply(RFM_cluster3, .(StockCode,Description), summarize, sumAmount= sum(Amount), sumQuantity= sum(Quantity), nCustomer= length(unique(CustomerID)), nPurchase= length(unique(InvoiceNo)) )


### Visualization of Clusters 
ggplot(RFM_cluster,aes(x=nRFM.R, y=nRFM.M, color= cluster,size= nRFM.Fq))+geom_point()+ scale_size_area(max_size=10)+labs(x="Recency", y="Monetary")

# Cluster 1 
ggplot(RFM_cluster1,aes(x=RFMScore))+geom_histogram(bins=50)+ labs(x="RFMScore", y="Count")
ggplot(RFM_cluster1,aes(x=R, y=M, color= Fq, size = 10))+geom_point()+ labs(x="Recency", y="Monetary")

# Cluster 2 
ggplot(RFM_cluster2,aes(x=RFMScore))+geom_histogram(bins=50)+ labs(x="RFMScore", y="Count")
ggplot(RFM_cluster2,aes(x=R, y=M, color= Fq, size = 10))+geom_point()+ labs(x="Recency", y="Monetary")

# Cluster 3 
ggplot(RFM_cluster3,aes(x=RFMScore))+geom_histogram(bins=50)+ labs(x="RFMScore", y="Count")
ggplot(RFM_cluster3,aes(x=R, y=M, color= Fq, size = 10))+geom_point()+ labs(x="Recency", y="Monetary")


### Cluster 

cluster1 <- apply(RFM_cluster1[,c(17,19,20,26)],2,mean)
cluster2 <- apply(RFM_cluster2[,c(17,19,20,26)],2,mean)
cluster3 <- apply(RFM_cluster3[,c(17,19,20,26)],2,mean)

Cluster_New <-  data.frame(cluster1,cluster2,cluster3)

write.csv(Cluster_New, file = "Cluster_New.csv")
###------------Analysing the Clusters ---------###
RFM_cluster1$Month <- month(RFM_cluster1$InvoiceDate , label = T)
RFM_cluster1 %>%
  group_by(Month) %>%
  summarise(Revenue = sum(Amount)) %>%
  ggplot(aes(x = Month, y = Revenue)) + geom_col() + labs(x = 'Day of Week', y = 'Customer', title = 'Customers by Day of Week')
cluster1

RFM_cluster2$Month <- month(RFM_cluster2$InvoiceDate , label = T)
RFM_cluster2 %>%
  group_by(Month) %>%
  summarise(Revenue = sum(Amount)) %>%
  ggplot(aes(x = Month, y = Revenue)) + geom_col() + labs(x = 'Day of Week', y = 'Customer', title = 'Customers by Day of Week')

RFM_cluster3$Month <- month(RFM_cluster3$InvoiceDate, label = T)
RFM_cluster3 %>%
  group_by(Month) %>%
  summarise(Revenue = sum(Amount)) %>%
  ggplot(aes(x = Month, y = Revenue)) + geom_col() + labs(x = 'Day of Week', y = 'Customer', title = 'Customers by Day of Week')

######----------Time Series Analysis ---------#######
library(prophet)
TSA <- DataUK[,c(5,9)]
TSA$Date <- date(TSA$InvoiceDate)
TSA$InvoiceDate <- NULL
TSA <- TSA[c(2,1)]
TSA <- TSA%>% group_by(Date)%>% summarise(y = sum(Amount))


TSA <- dplyr::rename(TSA, "ds"="Date" )

Model <- prophet(TSA)
Future = make_future_dataframe(Model, periods = 180)

Forecasted <- predict(Model, Future)

##--------------------------Using Neural Network-------######
library(ggthemes)
library(forecast)
library(tidyverse)
library(tseries)
library(lubridate)
library(timetk)
library(readxl)
library(tidyquant)  #Not able to install Subrat
library(scales)
library(forecast)   #  forecasting pkg
library(sweep)   # Broom tidiers for forecast pkg
library(broom)
library(tibble)
library(stringr)
library(highcharter)
library(knitr)

dat_ts = ts(log(TSA$y), start=c(2010, 12), end=c(2011, 12),frequency = 12)

fitnn <- nnetar(dat_ts)
?nnetar
fcast <- forecast(fitnn, PI=TRUE, h=6)
plot(fcast)
summary(fcast)
accuracy(fcast)

m <- decompose(dat_ts)
plot(m)

####-----Time Series using Arima ----####

myts   <- ts(log(TSA$y), start=c(2010, 12), end=c(2011, 12), frequency=305) 

plot(myts)

myts1 <- ts(log(DataUK$Amount), start=c(2010, 12), end=c(2011, 12),frequency = 305)
plot(myts1)

