##################
## R program 
## Author: Shajulin Benedict
## Email: shajulin@iiitkottayam.ac.in
##################

# Phases: i) Collect the data in the field
data <- read.csv(file="/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/covid_19_data.csv",sep=",",head=TRUE, quote="")
head(data)

# Include libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(plotly)
library(lubridate)

# Shape data i) Remove X column
data <- data[, -9] 


# Change column names of the data
names(data)[names(data) == "X.SNo."] <- "S.No"
names(data)[names(data) == "X.ObservationDate."] <- "Date"
names(data)[names(data) == "X.Province.State."] <- "State"
names(data)[names(data) == "X.Country.Region."] <- "Country-Region"
names(data)[names(data) == "X.Last.Update."] <- "Update"
names(data)[names(data) == "X.Confirmed."] <- "Confirmed"
names(data)[names(data) == "X.Deaths."] <- "Deaths"
names(data)[names(data) == "X.Recovered."] <- "Recovered"

# Convert date to numeric number
data$date <- as.Date(data$Date, format = "%d/%m/%Y")
data$posixDate <- as.POSIXct(data$Date, format = "\"%d/%m/%Y\"")
data$nDate <- as.numeric(data$Date)
data$dateID <- cumsum(!duplicated(data$nDate))

# Merge three columns and create unique monitoring site
data$siteIDstr <- with(data, paste0(country_region, sub_region_1, sub_region_2))
data$siteID <- cumsum(!duplicated(data$siteIDstr))


# Filter data  
target <- c(1581705000, 1581791400, 1581877800)
filterData <- filter(data, nDate %in% target)
#filterData <- data 
nrow(filterData)
print(filterData$nDate)

# Replace NAs to 0 in data
filterData[is.na(filterData)] <- 0

# Convert minimal values to above zero
minRetailRecreation <- min(filterData$RetailRecreation)
filterData$ConstRetailRecreation <- filterData$RetailRecreation + abs(minRetailRecreation)
min(filterData$ConstRetailRecreation)
minGroceryPharmacy <- min(filterData$GroceryPharmacy)
filterData$ConstGroceryPharmacy <- filterData$GroceryPharmacy + abs(minGroceryPharmacy)
min(filterData$ConstGroceryPharmacy)
minParks <- min(filterData$Parks)
filterData$ConstParks <- filterData$Parks + abs(minParks)
min(filterData$ConstParks)
minTransitStations <- min(filterData$TransitStations)
filterData$ConstTransitStations <- filterData$TransitStations + abs(minTransitStations)
min(filterData$ConstTransitStations)
minWorkplaces <- min(filterData$Workplaces)
filterData$ConstWorkplaces <- filterData$Workplaces + abs(minWorkplaces)
min(filterData$ConstWorkplaces)
minRecidential <- min(filterData$Recidential)
filterData$ConstRecidential <- filterData$Recidential + abs(minRecidential)
min(filterData$ConstRecidential)

#write.csv(filterData, "/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/filterData.csv")

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

################### RFM Prediction Approach for RetailRecreation
model_RetailRecreation <- randomForest(ConstRetailRecreation ~ ConstGroceryPharmacy + ConstParks + ConstTransitStations + ConstWorkplaces + ConstRecidential, data = training, importance=TRUE, keep.forest=TRUE, ntree=100, mtry=2)
#predict the outcome of the testing data
predicted_RetailRecreation_rfm <- predict(model_RetailRecreation, testing, type="response", predict.all=FALSE, proximity=FALSE, nodes=FALSE)

############################ Find Error for the prediction
actual_RetailRecreation <- testing$RetailRecreation
rsq_RetailRecreation <- 1-sum((actual_RetailRecreation-predicted_RetailRecreation_rfm)^2)/sum((actual_RetailRecreation-mean(actual_RetailRecreation))^2)
print(rsq_RetailRecreation)



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
