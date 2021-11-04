##############################################################################################
#
# R Code to draw graphs in R 
# copyright IoT Cloud Research Lab 
# Author: iotcloud Benedict Date: 1.6.2020
# Graphs for certain values
#
##############################################################################################



library(dplyr)
library(caret)
library(tidyverse)
library(GGally)
library(corrplot)


### Prepare graphs for prediction accuracy R squared errors
### Step 1: Collect values in the dataset
### LR, LRM, QR, RR, LaR, ER, RF
RsquaredValues <- NULL
RsquaredValues$NO2 <- c(88.21, 88.21, 84.16, 92.66, 92.25, 88.25, 99.81)
RsquaredValues$SO2 <- c(51.66, 51.66, 36.31, 56.89, 55.8, 51.72, 99.48)

corrVal<-cor(mtcars)
corrplot(corrVal, method="circle")

cor(RsquaredValues, method = "pearson", use = "complete.obs")

ggcorr(RsquaredValues, method = c("everything", "pearson")) 



maxNO2 <- max(Rsquared_NO2)
minNO2 <- min(Rsquared_NO2)
incrementNO2 <- (as.numeric(maxNO2) - as.numeric(minNO2)) / 4 

plotR2 <- ggplot() +
	geom_line(data = Rsquared_NO2, aes(x = Rsquared_NO2, y = SO2), color = "#993333") +
	xlab('Latitude+Longitude ') +
	ylab('Prediction Accuracy -- RSquared NO2 ') + 
	# add more ticks on the y-axis
	scale_y_discrete(breaks=seq(maxNO2,minNO2,by=incrementSO2)) +
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
	pdf("/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/Results/PredAccuracy.pdf",width=8,height=5)
	print(plotR2)
	dev.off()