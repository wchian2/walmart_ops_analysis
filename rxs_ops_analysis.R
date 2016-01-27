#####################
##                 ##
##  Data Cleaning  ##
##                 ##
#####################

# Load Walmart dataset
data <- read.csv("TempOps-Analysis.csv", header=FALSE)

# Remove unnecessary rows
data <- data[-1:-6, ]
data <- data[-20:-132, ]

# Remove unnecessary columns
data <- data[ ,c(-1,-9,-21,-35)]

# Rename columns
colnames(data) <- c("StoreNumber", "CustomerSatisfaction", "CustomerGoal", "RxCount", "RxCountChange", "RollingRxChange", "ReturnToStockRate", "PercentInstore", "AverageInstorePerformance", "AverageInstoreResolution", "OnTimeInstore", "AverageInstoreOps", "PercentExpectationsMet", "PartialFillRate", "OutOfStockRate", "EasyPay", "InputRerouting", "InputQuality", "RxSales", "OTCSales", "OTCSalesIncrease", "OTCSalesIncreaseFiscal", "Outs", "OTCRetail", "OTCRetailInc", "RxCost", "RxCostInc", "InventoryTurnoverRate", "MTMRate", "DiscountCardUtilized", "DiscountCardLoss", "OverallRank")

# More cleaning of row data, e.g. Removal of unnecessary rows and naming rows in numerical order
data <- data[-1:-2, ]
row.names(data) <- c(1:17)
data <- data[-12:-17, ]
data <- data[ , -3]

# Convert data frame from factors to numeric values
indx <- sapply(data, is.factor)
data[indx] <- lapply(data[indx], function(x) as.numeric(as.character(x)))

# Create new columns containing data from Internet sources
# Average temperature and its range (difference of average high and low temps) were collected from www.wunderground.com by searching city by Week 49 data
# Texas city for each store was located using Google Maps
# Average household income and population data were acquired from Wikipedia search
data$AverageTemperature <- c(44,44,40,44,45,45,44,44,45,44,44)
data$AverageTemperatureRange <- c(53-36,54-34,45-35,54-33,55-35,55-35,54-34,53-36,55-35,54-34,54-34)
data$TexasTown <- c("Denison","Bonham","Gainesville","Bowie","Wichita Falls","Wichita Falls","Denton","Sherman","Wichita Falls","Denton","Cross Roads")
data$AverageHouseholdIncome <- c(31474,27277,30571,33846,32554,32554,44415,34211,32554,44415,77031)
data$Population <- c(22816,10127,16002,5219,104553,104553,113383,38521,104553,113383,1563)

# Identify index positions of unneeded columns for removal (After more thoughts, removed data is not needed for the analysis)
data <- data[ ,c(-which(colnames(data)=="RollingRxChange"))]
data <- data[ ,c(-which(colnames(data)=="OTCSalesIncreaseFiscal"))]
data <- data[ ,c(-which(colnames(data)=="OverallRank"))]
data <- data[ ,c(-which(colnames(data)=="Outs"):-which(colnames(data)=="RxCostInc"))]

# Reorder columns with Store Number being the first column, followed by location, weather, and demographic data, and then by Operations Data
# After line 40 is executed, data structure should still be "11 observations by 28 variables"
data <- data[c(1,24:28,2:23)]
data <- data[c(1,4,6,5,2,3,7:28)]

# Write cleaned data set to csv file for ease of loading into MS Excel or Apple's Numbers Application
write.table(data, file = "data_cleaned.csv", sep = ",", row.names = FALSE)


#################################
##                             ##
##  Exploratory Data Analysis  ##
##                             ##
#################################

# Load ggplot2 package
library(ggplot2)

# Draw bar graph illustrating customer satisfaction survey results by store number, with horizontal line showing mean score
ggplot(aes(x = ordered(as.factor(StoreNumber)), y = CustomerSatisfaction), data = data, fill=StoreNumber) +
  geom_bar(stat="identity", fill="orange", width=0.75) +
  coord_cartesian(ylim=c(8,10)) +
  geom_hline(yintercept=mean(data$CustomerSatisfaction)) +
  xlab("Store Number") +
  ylab("Customer Satisfaction Score") +
  ggtitle("Customer Satisfaction Survey Results by Store Number")

# Create correlation matrix listing correleations of the entire dataset
cor_test <- cor(data[,c(3:28)], use="pairwise", method="spearman")
cor_test


#####################
##                 ##
##  Data Analysis  ##
##                 ##
#####################

# Create linear regression model with the dependent variable as CustomerSatisfaction
# and the independent variables as the measures highly correlated with Customer Satisfaction scores
SatisfactionReg <- lm(CustomerSatisfaction ~ OutOfStockRate + EasyPay + DiscountCardUtilized + PartialFillRate + OTCSalesIncrease, data= data)
summary(SatisfactionReg)

# Create linear regression model with the dependent variable as RxSales
# and the independent variables as the measures highly correlated with RxSales
RxSalesReg <- lm(RxSales ~ AverageHouseholdIncome + AverageTemperature + RxCount + EasyPay + DiscountCardLoss, data= data)
summary(RxSalesReg)