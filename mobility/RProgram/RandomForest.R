##################
## R program 
## Author: Shajulin Benedict
## Email: shajulin@iiitkottayam.ac.in
## https://www.google.com/covid19/mobility/ (Dataset)
##################

# Phases: i) Collect the data in the field
data <- read.csv(file="/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/OmittedData.csv",sep=",",head=TRUE)
head(data)

# Change the lengthy column names
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(plotly)
library(lubridate)

names(data)[names(data) == "retail_and_recreation_percent_change_from_baseline"] <- "RetailRecreation"
names(data)[names(data) == "grocery_and_pharmacy_percent_change_from_baseline"] <- "GroceryPharmacy"
names(data)[names(data) == "parks_percent_change_from_baseline"] <- "Parks"
names(data)[names(data) == "transit_stations_percent_change_from_baseline"] <- "TransitStations"
names(data)[names(data) == "workplaces_percent_change_from_baseline"] <- "Workplaces"
names(data)[names(data) == "residential_percent_change_from_baseline"] <- "Recidential"


# Convert date to numeric number and create unique id
filterOmitData <- data
filterOmitData$posixDate <- as.POSIXlt(filterOmitData$date)
filterOmitData$nDate <- as.numeric(filterOmitData$posixDate)
filterUniqueDate <- transform(filterOmitData,dateID=as.numeric(factor(nDate)))


# Merge three columns and create unique monitoring site
filterUniqueDate$siteIDstr <- with(filterUniqueDate, paste0(country_region, sub_region_1, sub_region_2))
filterUnique <- transform(filterUniqueDate,siteID=as.numeric(factor(siteIDstr)))
filterUnique <- filterUnique[, -1]  # Removes X
filterUnique <- filterUnique[, -12] # Removes 

# Order data based on the date of occurance: (Increasing the date, covid cases increases)
OrderData <- filterUnique[order(filterUnique[,12],decreasing=TRUE),]

write.csv(OrderData, "/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/filterData.csv")

# #write.csv(filterOmitData, "/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/filterData.csv")
# # 5000 gives: 90655 entries; 1000 gives 293 entries
# filterSelectData <- filterUnique %>% 
#   select(country_region_code, country_region, sub_region_1, sub_region_2, date, RetailRecreation, GroceryPharmacy,Parks, TransitStations, Workplaces, Recidential, nDate, dateID, siteIDstr, siteID) %>%
#   filter(siteID <= "1000")
#   nrow(filterSelectData)

# write.csv(filterSelectData, "/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/filterData.csv")

# Plot values for different siteIDs. 
fig1 <- ggplot(OrderData, aes(x = siteID, y = Parks), color=siteID) +
    geom_col() 
ggplotly(fig1)

fig2 <- ggplot(OrderData, aes(x = nDate, y = Parks), color=siteID) +
    geom_col() 
ggplotly(fig2)

# Store it in filterData
nrow(OrderData)
filterData <- OrderData

# Convert minimal values to above zero
minRetailRecreation <- min(filterData$RetailRecreation)
filterData$ConstRetailRecreation <- filterData$RetailRecreation + abs(minRetailRecreation)
minGroceryPharmacy <- min(filterData$GroceryPharmacy)
filterData$ConstGroceryPharmacy <- filterData$GroceryPharmacy + abs(minGroceryPharmacy)
minParks <- min(filterData$Parks)
filterData$ConstParks <- filterData$Parks + abs(minParks)
minTransitStations <- min(filterData$TransitStations)
filterData$ConstTransitStations <- filterData$TransitStations + abs(minTransitStations)
minWorkplaces <- min(filterData$Workplaces)
filterData$ConstWorkplaces <- filterData$Workplaces + abs(minWorkplaces)
minRecidential <- min(filterData$Recidential)
filterData$ConstRecidential <- filterData$Recidential + abs(minRecidential)

#write.csv(filterData, "/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/filterData.csv")

################################ Real training and testing starts 

# splitdf function will return a list of training and testing sets
splitdf <- function(filterData, seed=NULL) {
if (!is.null(seed)) set.seed(seed)
index <- 1:nrow(filterData)
trainindex <- sample(index, trunc(length(index)/2))
trainset <- filterData[trainindex, ]
testset <- filterData[-trainindex, ]
list(trainset=trainset,testset=testset)
}
 
#apply the function
splits <- splitdf(filterData, seed=808)
 
#it returns a list - two data frames called trainset and testset
str(splits)
 
# there are 50-50 observations foreach data frame
lapply(splits,nrow)
 
#view the first few columns in each data frame
lapply(splits,head)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset
nrow(training)
nrow(testing)


###################model the training dataset using models
library(randomForest)

# ConstRetailRecreation + ConstGroceryPharmacy + ConstParks + ConstTransitStations + ConstWorkplaces + ConstRecidential 

################### RFM Prediction Approach for ConstParks
model_ConstParks <- randomForest(ConstParks ~ ConstGroceryPharmacy + ConstRetailRecreation + ConstTransitStations + ConstWorkplaces + ConstRecidential, data = training, importance=TRUE, keep.forest=TRUE, ntree=100, mtry=2)
#predict the outcome of the testing data
predicted_ConstParks_rfm <- predict(model_ConstParks, testing, type="response", predict.all=FALSE, proximity=FALSE, nodes=FALSE)

############################ Find Error for the prediction
actual_ConstParks <- testing$ConstParks
rsq_ConstParks <- 1-sum((actual_ConstParks-predicted_ConstParks_rfm)^2)/sum((actual_ConstParks-mean(actual_ConstParks))^2)
print(rsq_ConstParks)

# ################### Descale and print values
real_predicted_Parks_rfm <- predicted_ConstParks_rfm + minParks
testing$PredictedParks <- real_predicted_Parks_rfm
fig3 <- ggplot(testing, aes(x = siteID, y = PredictedParks), color=siteID) +
    geom_col() 
ggplotly(fig3)
fig4 <- ggplot(testing, aes(x = nDate, y = PredictedParks), color=siteID) +
    geom_col() 
ggplotly(fig4)


# model_RetailRecreation <- randomForest(ConstRetailRecreation ~ ConstGroceryPharmacy + ConstParks + ConstTransitStations + ConstWorkplaces + ConstRecidential, data = training, importance=TRUE, keep.forest=TRUE, ntree=100, mtry=2)
# #predict the outcome of the testing data
# predicted_RetailRecreation_rfm <- predict(model_RetailRecreation, testing, type="response", predict.all=FALSE, proximity=FALSE, nodes=FALSE)

# ############################ Find Error for the prediction
# actual_RetailRecreation <- testing$RetailRecreation
# rsq_RetailRecreation <- 1-sum((actual_RetailRecreation-predicted_RetailRecreation_rfm)^2)/sum((actual_RetailRecreation-mean(actual_RetailRecreation))^2)
# print(rsq_RetailRecreation)



# ################### LRM model the training dataset using models
# ################### LRM Prediction Approach for pH
# LRMmodel_RetailRecreation <- glm(ConstRetailRecreation ~ ConstGroceryPharmacy + ConstParks + ConstTransitStations + ConstWorkplaces + ConstRecidential, data = training)
# #predict the outcome of the testing data
# predicted_RetailRecreation_lrm <- predict(LRMmodel_RetailRecreation, testing, type="response")

# ############################ Find Error for the prediction
# lrm_actual_RetailRecreation <- testing$RetailRecreation
# lrm_rsq_RetailRecreation <- 1-sum((actual_RetailRecreation-predicted_RetailRecreation_lrm)^2)/sum((actual_RetailRecreation-mean(actual_RetailRecreation))^2)
# print(lrm_rsq_RetailRecreation)

############################ Descale Value after prediction in order to plot the values. 
real_predicted_RetailRecreation_rfm <- predicted_RetailRecreation_rfm + minRetailRecreation
testing$intRealPredictedValue <- as.integer(testing$real_predicted_RetailRecreation_rfm)

############################ plot the values for RFM
 plot(training$siteID, training$RetailRecreation, col="blue",xlab="Predictions ", ylab="Retail And Recreation")
 lines(testing$siteID, RetailRecreation, col="red")


testing$real_predicted_RetailRecreation_rfm <- as.factor(real_predicted_RetailRecreation_rfm)
fig1 <- ggplot(training, aes(x = siteID, y = RetailRecreation, fill = RetailRecreation), color=siteID) +
    geom_col() +
    coord_flip()
fig2 <- ggplot(testing, aes(x = siteID, y = intRealPredictedValue), color=siteID) + 
    geom_col() +
    coord_flip()

ggplotly(fig2)




# Reduced dataset
training <- data[1:300, , drop=FALSE]

training$siteID <- cumsum(!duplicated(training[14]))
max(training$siteID)

#training$siteID <- with(training, paste0(country_region, sub_region_1, sub_region_2))

fig3 <- ggplot(training, aes(x = country_region_code, y = intRealPredictedValue), color=type) + 
    geom_col() +
    coord_flip()
ggplotly(fig3)
write.csv(training, "/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/tempData.csv")

max(testing$intRealPredictedValue)



testing$intRealPredictedValue <- as.integer(testing$real_predicted_RetailRecreation_rfm)
fig3 <- ggplot(testing, aes(x = country_region_code, y = intRealPredictedValue), color=type) + 
    geom_col() +
    coord_flip()
ggplotly(fig3)



# Remove X from dataframe
data <- data[, -12]

# Remove NAs from data
#filterData[is.na(filterData)] <- 0
filterOmitData <- na.omit(data)