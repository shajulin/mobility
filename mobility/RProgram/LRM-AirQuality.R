##############################################################################################
#
# R Code to validate the air quality values 
# copyright IoT Cloud Research Lab 
# Author: iotcloud Benedict Date: 1.6.2020
# LRM -- Algorithm
#
##############################################################################################

#Vary the application name
application <- "Appl"
version <- "version" 

# For predicting only three or four lat-long values
predict_busroute_airquality <- "TRUE"

# For calculating the entire time required for prediction or validation
sTime <- proc.time()

if(predict_busroute_airquality == "FALSE"){
	print("Processing Validation Step of SO2 and NO2")
	#load the consolidated.csv
	if( application == "Appl"){
	alldata <- read.csv(file="/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/latlongAirQuality-Kerala.csv",sep=",",head=TRUE)
	}
	alldata <- na.omit(alldata)
} else {
	## Processing for predicting values
	#load the consolidated.csv
	print("Processing Prediction of SO2 and NO2")
	if( application == "Appl"){
	alldata <- read.csv(file="/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/latlongAirQuality-Kerala-topredict.csv",sep=",",head=TRUE)
	}
	alldata <- na.omit(alldata)
	
}


	#convert date to numeric and add the stationcode
	alldata$LatLong <- as.numeric(alldata$Latitude + alldata$Longitude)
	alldata$NumericDate <- as.factor(alldata$Sampling.Date)
	alldata$NumericLocation <- as.factor(alldata$Location.of.Monitoring.Station)
	alldata$NumericNO2 <- as.numeric(alldata$NO2)
	alldata$NumericSO2 <- as.numeric(alldata$SO2)
	#alldata$NumericSPM <- as.factor(alldata$SPM)
	#alldata$NumericRSPM.PM10 <- as.numeric(alldata$RSPM.PM10)
	alldata$NumericCity.Town.Village.Area <- as.factor(alldata$City.Town.Village.Area)
	alldata$NumericLocation.of.Monitoring.Station <- as.factor(alldata$Location.of.Monitoring.Station)
	alldata$DateLocation <- as.numeric(alldata$NumericDate) + as.numeric(alldata$NumericLocation)

	naalldata <- na.omit(alldata)
	nrow(naalldata)

	#order based on SO2, NO2, and RSPM
	#OrderData <- naalldata[order(naalldata[,17],naalldata[,18],naalldata[,21],decreasing=TRUE),]
	#OrderDataSO2 <- naalldata[order(naalldata[,18],naalldata[,17],naalldata[,21],decreasing=TRUE),]

	OrderDataNO2 <- naalldata[order(naalldata$NumericNO2,decreasing=TRUE),]
	OrderDataSO2 <- naalldata[order(naalldata$NumericSO2,decreasing=TRUE),]

	#OrderDataSO2 <- OrderData
	maxNum <- nrow(OrderDataNO2)
	maxNum <- nrow(OrderDataSO2)

	OrderDataNO2$SeqNoNO2 <- seq(1,maxNum)
	OrderDataSO2$SeqNoSO2 <- seq(1,maxNum)

	#write.csv(OrderData, file = "/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/EdgeDS/ConsolidatedOrder.csv",row.names=TRUE, na="")

	# look at the first few
	head(OrderDataNO2)

	write.csv(OrderDataNO2, file = "/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/ConsolidatedOrderDataNO2.csv",row.names=TRUE, na="")

if(predict_busroute_airquality == "FALSE"){
	if(version == "version"){
		# splitdf function will return a list of training and testing sets
		splitdfNO2 <- function(OrderDataNO2, seed=NULL) {
		if (!is.null(seed)) set.seed(seed)
		indexNO2 <- 1:nrow(OrderDataNO2)
		trainindexNO2 <- sample(indexNO2, trunc(length(indexNO2)/2))
		trainsetNO2 <- OrderDataNO2[trainindexNO2, ]
		testsetNO2 <- OrderDataNO2[-trainindexNO2, ]
		list(trainsetNO2=trainsetNO2,testsetNO2=testsetNO2)
		}
		
		
		# splitdf function will return a list of training and testing sets
		splitdfSO2 <- function(OrderDataSO2, seed=NULL) {
		if (!is.null(seed)) set.seed(seed)
		indexSO2 <- 1:nrow(OrderDataSO2)
		trainindexSO2 <- sample(indexSO2, trunc(length(indexSO2)/2))
		trainsetSO2 <- OrderDataSO2[trainindexSO2, ]
		testsetSO2 <- OrderDataSO2[-trainindexSO2, ]
		list(trainsetSO2=trainsetSO2,testsetSO2=testsetSO2)
		}
	}


	#apply the function
	splitsNO2 <- splitdfNO2(OrderDataNO2, seed=100)
	splitsSO2 <- splitdfSO2(OrderDataSO2, seed=100)

	#it returns a list - two data frames called trainset and testset
	str(splitsNO2)
	str(splitsSO2)

	# there are 50-50 observations foreach data frame
	lapply(splitsNO2,nrow)
	lapply(splitsSO2,nrow)

	#view the first few columns in each data frame
	lapply(splitsNO2,head)
	lapply(splitsSO2,head)

	# save the training and testing sets as data frames
	trainingNO2 <- splitsNO2$trainsetNO2
	testingNO2 <- splitsNO2$testsetNO2
	trainingSO2 <- splitsSO2$trainsetSO2
	testingSO2 <- splitsSO2$testsetSO2
	nrow(trainingNO2)
	nrow(testingNO2)
	nrow(trainingSO2)
	nrow(testingSO2)
} else {
	# prediction only for the travel routes
	# save the training and testing sets based on zero values in SO2 and NO2	
	testingNO2 <- OrderDataNO2[OrderDataNO2$NumericNO2 < 0, ]
	trainingNO2 <- OrderDataNO2[OrderDataNO2$NumericNO2 >= 0, ]
	
	testingSO2 <- OrderDataSO2[OrderDataSO2$NumericSO2 < 0, ]
	trainingSO2 <- OrderDataSO2[OrderDataSO2$NumericSO2 >= 0, ]

	nrow(testingNO2)
	nrow(trainingNO2)
	
	nrow(testingSO2)
	nrow(trainingSO2)
}


############################ Model data
################### LRM - Linear regression using glm is used to avoid negative values while predicting the energy
stime_SO2 <- proc.time()
model_SO2 <- glm(NumericSO2 ~ SeqNoSO2 + NumericNO2 + LatLong, data = trainingSO2)
etime_SO2 <- proc.time() - stime_SO2
print(etime_SO2)

stime_NO2 <- proc.time()
model_NO2 <- glm(NumericNO2 ~ SeqNoNO2 + NumericSO2 + LatLong, data = trainingNO2)
etime_NO2 <- proc.time() - stime_NO2
print(etime_NO2)

summary(model_SO2)
summary(model_NO2)

#predict the outcome of the testing data 
spredicttimeSO2 <- proc.time()
predicted_SO2 <- predict(model_SO2, testingSO2, type="response", predict.all=FALSE)
epredicttimeSO2 <- proc.time() - spredicttimeSO2
print(epredicttimeSO2)
spredicttimeNO2 <- proc.time()
predicted_NO2 <- predict(model_NO2, testingNO2, type="response", predict.all=FALSE)
epredicttimeNO2 <- proc.time() - spredicttimeNO2
print(epredicttimeNO2)

if(predict_busroute_airquality == "FALSE"){
	############################ Find Error for the prediction
	actual_SO2 <- testingSO2$NumericSO2
	rsq_SO2 <- 1-sum((actual_SO2-predicted_SO2)^2)/sum((actual_SO2-mean(actual_SO2))^2)
	print(rsq_SO2)

	actual_NO2 <- testingNO2$NumericNO2
	rsq_NO2 <- 1-sum((actual_NO2 - predicted_NO2)^2)/sum((actual_NO2-mean(actual_NO2))^2)
	print(rsq_NO2)

	############################ PDF Files

	# pdf("/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/Results/QR_SO2Plot.pdf",width=8,height=5)
	#  x=rnorm(100)
	#  y=rnorm(100,5,1)
	# plot(trainingSO2$LatLong, trainingSO2$SO2,col="blue",xlab=" Latitude+Longitude ", ylab="SO2 Value ")
	# lines(testingSO2$LatLong, predicted_SO2, col="red")
	# dev.off()

	# pdf("/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/Results/QR_NO2Plot.pdf",width=8,height=5)
	#  x=rnorm(100)
	#  y=rnorm(100,5,1)
	# plot(trainingNO2$LatLong, trainingNO2$NO2,col="blue",xlab="Latitude+Longitude ", ylab="NO2 Value ")
	# lines(testingNO2$LatLong, predicted_NO2, col="red")
	# dev.off()


	# ggplot(trainingNO2, aes(x = `LatLong`, y = `NO2`, fill = ..x..)) +
	#   geom_density_ridges_gradient(scale = 1, rel_min_height = 0.1) +
	#   labs(title = 'NO2 Values of Training Dataset') +
	#   theme_ipsum() +
	#     theme(
	# 		axis.line = element_line(colour = "darkblue", 
	#                       size = 1, linetype = "solid"),
	# 		axis.text.y=element_text(size=0),
	#       	legend.position="none",
	#       	panel.spacing = unit(1, "lines")
	#     )


	########## Plot values  here 
	# library
	library(ggridges)
	library(ggplot2)
	library(viridis)
	library(hrbrthemes)
	maxTrainingNO2 <- max(trainingNO2$NO2)
	minTrainingNO2 <- min(trainingNO2$NO2)
	incrementNO2 <- (as.numeric(maxTrainingNO2) - as.numeric(minTrainingNO2)) / 4 
	plotNO2 <- ggplot() +
	geom_point(data = trainingNO2, aes(x = LatLong, y = NO2), color = "#993333") +
	geom_line(data = testingNO2, aes(x = LatLong, y = NO2), color = "steelblue") +
	xlab('Latitude+Longitude ') +
	ylab('NO2 Value ') + 
	# add more ticks on the y-axis
	scale_y_discrete(breaks=seq(minTrainingNO2,maxTrainingNO2,by=incrementNO2)) +
	theme(
			legend.text = element_text(face = "italic",colour="steelblue4",
								family = "Helvetica", size = rel(1)),
			axis.line = element_line(colour = "darkblue", 
						size = 1, linetype = "solid"),
			legend.position="right", 
			panel.grid.major = element_line(colour="grey", size = (1)),
			panel.grid.minor = element_line(size = (0.1), colour="red"), 
			legend.key = element_rect(fill = "whitesmoke"), 
					legend.title = element_text(colour = "steelblue",size = rel(1.5),
								family = "Helvetica") 
		)

	### Print pdf for NO2
	pdf("/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/Results/LRM_NO2Plot.pdf",width=8,height=5)
	print(plotNO2)
	dev.off()

	### 
	maxTrainingSO2 <- max(trainingSO2$SO2)
	minTrainingSO2 <- min(trainingSO2$SO2)
	incrementSO2 <- (as.numeric(maxTrainingSO2) - as.numeric(minTrainingSO2)) / 4 
	plotSO2 <- ggplot() +
	geom_point(data = trainingSO2, aes(x = LatLong, y = SO2), color = "#993333") +
	geom_line(data = testingSO2, aes(x = LatLong, y = SO2), color = "steelblue") +
	xlab('Latitude+Longitude ') +
	ylab('SO2 Value ') + 
	# add more ticks on the y-axis
	scale_y_discrete(breaks=seq(minTrainingSO2,maxTrainingSO2,by=incrementSO2)) +
	theme(
		legend.text = element_text(face = "italic",colour="steelblue4",
								family = "Helvetica", size = rel(1)),
			axis.line = element_line(colour = "darkblue", 
						size = 1, linetype = "solid"),
			legend.position="right", 
			panel.grid.major = element_line(colour="grey"),
			panel.grid.minor = element_line(colour="red"), 
			legend.key = element_rect(fill = "whitesmoke"), 
					legend.title = element_text(colour = "steelblue",size = rel(1.5),
								family = "Helvetica") 
		)

	### Print pdf for SO2
	pdf("/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/Results/LRM_SO2Plot.pdf",width=8,height=5)
	print(plotSO2)
	dev.off()

} else {
	# prediction only for the travel routes
	print("SO2 values ")
	print(predicted_SO2)
	print("NO2 values ")
	print(predicted_NO2)
	summary(model_SO2)
	summary(model_NO2)
}

eTime <- proc.time() - sTime
print(eTime)