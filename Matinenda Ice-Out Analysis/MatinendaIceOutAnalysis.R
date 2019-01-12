# Applied Data Science, Final Project
# Peter Annable  4/14/2018
#
# Lake Matinenda Ice-Out Analysis
#
###################################################
#Artificial Neural Networks (ANN) package in R: nnet
###################################################
## (1) ANN on Iris data set: Classification

#install the package
#install.packages("nnet")
library(nnet)
library(data.table)
library(reshape2)
library(ggplot2)
library(xlsx)
mydata <- read.xlsx("AnnualData.xlsx",sheetName="Annual Data")
#
#
# For plotting, remove rows with null Ice Out date
#
mydata_ice <- subset(mydata,!is.na(mydata["Ice.Out.Julian"]))

mydata_ice <- within(mydata_ice, quantile <- as.integer(cut(Ice.Out.Julian, quantile(Ice.Out.Julian, probs=0:2/2, na.rm=TRUE), include.lowest=TRUE )))
mydata_ice$quantile <- as.factor(mydata_ice$quantile)

#qplot(x=Year, y=Ice.Out.Julian, data=mydata_ice, geom=c("point", "smooth", "quantile"), color=factor(quartile))
#qplot(x=Year, y=Ice.Out.Julian, data=mydata_ice, geom=c("point", "smooth", "quantile"), scale_x_discrete(breaks=c("1930", "1940", "1950", "2000")))

require(stats)
reg<-lm(Ice.Out.Julian ~ Year, data = mydata_ice)
reg
coeff=coefficients(reg)
# Equation of the line : 
eq = paste0("Ice-Out Julian Day = ",round(coeff[1],2)," - ", abs(round(coeff[2],4)), "*Year" )
print(eq)
#
# plot data with trend line
#
ggplot(mydata_ice, aes(x=Year, y=Ice.Out.Julian, color=Ice.Out.Julian)) + geom_point(size=2) +
  geom_smooth(method = "lm", se = FALSE,color="red", linetype="dashed", size=1.5) +
    scale_x_continuous(name="Year", breaks=seq(1930,2020,10)) +
    scale_y_continuous(name="Ice-Out Julian Day", breaks=seq(90,140,10)) +
  ggtitle(eq)
#
# linear line and smoothed line
#
ggplot(mydata_ice, aes(x=Year, y=Ice.Out.Julian)) + geom_point(shape=1, size=3) +
  theme(axis.text = element_text(size=12)) +
  scale_x_continuous(name="Year", breaks=seq(1930,2020,10)) +
  scale_y_continuous(name="Ice-Out Julian Day", breaks=seq(90,140,10)) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  geom_smooth(method = "lm", se = FALSE, color="red", linetype="dashed", size=1) +
  ggtitle(eq)
ggsave("01-IceOutTrend.png", width=6, height = 4, units ="in", dpi=300, scale=1.3)
#
# Plot using median split of data
#
ggplot(mydata_ice, aes(x=Year, y=Ice.Out.Julian, color=quantile)) + geom_point(shape=1, size=3) +
  theme(axis.text = element_text(size=12)) +
  scale_x_continuous(name="Year", breaks=seq(1930,2020,10)) +
  scale_y_continuous(name="Ice-Out Julian Day", breaks=seq(90,140,10)) +
  geom_smooth(method = "lm", se = FALSE,color="red", linetype="dashed", size=1) +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  ggtitle(eq)

ggplot(mydata_ice, aes(x=Year, y=Ice.Out.Julian, color=quantile)) + geom_point(shape=1,size=3) +
  theme(axis.text = element_text(size=12)) +
  scale_color_manual(values=c("darkgreen", "blue")) +
  scale_x_continuous(name="Year", breaks=seq(1930,2020,10)) +
  scale_y_continuous(name="Ice-Out Julian Day", breaks=seq(90,140,10)) +
  geom_smooth(method = "lm", se = FALSE,color="red", linetype="dashed", size=1) +
  geom_smooth(data = subset(mydata_ice, mydata_ice$quantile == 1), method="auto", se=TRUE, level=0.95) +
  geom_smooth(data = subset(mydata_ice, mydata_ice$quantile == 2), method="auto", se=TRUE, level=0.95) +
  ggtitle("Ice-Out Julian Day Trend 2-Quantile Split")
ggsave("02-IceOutTrendQuartile.png", width=6, height = 4, units ="in", dpi=300, scale=1.3)

#
# Local Temperature Analysis
#
ggplot(mydata, aes(x=Year, y=ColdFactor)) + geom_line() +
  theme(axis.text = element_text(size=12)) +
  scale_x_continuous(name="Year", breaks=seq(1930,2020,10)) +
  scale_y_continuous(name="JFM Average Temp") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  geom_smooth(method = "lm", se = FALSE, color="red", linetype="dashed", size=1.0) +
  ggtitle("Blind River JFM Avg Low")
ggsave("03-BRJFMTrend.png", width=6, height = 4, units ="in", dpi=300, scale=1.3)
#
ggplot(mydata, aes(x=Year, y=Blind.River.Avg.Low.Temp)) + geom_line() + 
   theme(axis.text = element_text(size=12)) +
  scale_x_continuous(name="Year", breaks=seq(1930,2020,10)) +
  scale_y_continuous(name="Blind River Avg Low Temp") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  geom_smooth(method = "lm", se = FALSE, color="red", linetype="dashed", size=1.0) +
  geom_hline(aes(yintercept = -0.3),show.legend = TRUE) +
  ggtitle("Blind River Low Temperature Avg")
ggsave("04-BRLowTempsTrend.png", width=6, height = 4, units ="in", dpi=300, scale=1.3)
#
ggplot(mydata, aes(x=Year, y=Blind.River.Avg.High.Temp)) + geom_line() +
  theme(axis.text = element_text(size=12)) +
  scale_x_continuous(name="Year", breaks=seq(1930,2020,10)) +
  scale_y_continuous(name="Blind River Avg High Temp") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  geom_smooth(method = "lm", se = FALSE, color="red", linetype="dashed", size=1.0) +
  geom_hline(aes(yintercept = 10.1),show.legend = TRUE) +
  ggtitle("Blind River High Temperature Avg")
ggsave("05-BRHighTempsTrend.png", width=6, height = 4, units ="in", dpi=300, scale=1.3)

# Great Lakes Analysis
#
# Create data subset for rows with great lakes data
#
mydata_lakes <- subset(mydata,!is.na(mydata["Lake.Superior.Avg.Degrees"]))
#
# Show three lakes on one graph
#
ggplot(mydata_lakes, aes(x=Year)) +
  scale_colour_manual("", 
                      breaks = c("Michigan", "Huron", "Superior"),
                      values = c("orange", "darkgreen", "blue")) +
  geom_line(aes(y=Lake.Superior.Avg.Degrees, colour="Superior")) +
  geom_line(aes(y=Lake.Huron.Avg.Degs, colour="Huron")) +
  geom_line(aes(y=Lake.Michigan.Avg.Degs, colour="Michigan")) +
  theme(axis.text = element_text(size=12)) +
  scale_x_continuous(name="Year", breaks=seq(1990,2020,5)) +
  scale_y_continuous(name="Average Surface Temperature Degrees C") +
  geom_smooth(aes(y=Lake.Superior.Avg.Degrees,colour="Superior"),method="auto", se=FALSE, fullrange=FALSE, level=0.95) +
  geom_smooth(aes(y=Lake.Huron.Avg.Degs, colour="Huron"),method="auto", se=FALSE, fullrange=FALSE, level=0.95) +
  geom_smooth(aes(y=Lake.Michigan.Avg.Degs,colour="Michigan"),method="auto", se=FALSE, fullrange=FALSE, level=0.95) +
  ggtitle("Great Lakes Avg Temp Trend")
ggsave("06-GreatLakesTrend.png", width=6, height = 4, units ="in", dpi=300, scale=1.3)

#
# Histogram if Ice-Out Dates
#
ggplot(mydata_ice,aes(x=mydata_ice$Ice.Out.Julian)) + geom_histogram(binwidth=5) + ggtitle("Histogram of Ice-Out Julian Dates 1932-2014")
ggsave("08-Ice-OutHistogram.png", width=6, height = 4, units ="in", dpi=300, scale=1.3)

mydata_ice$pivot <- seq.int(nrow(mydata_ice))
melteddata = melt(mydata_ice[,c("Year", "Ice.Out.Julian", "ColdFactor", "Blind.River.Avg.Low.Temp", "Blind.River.Avg.High.Temp", "Blind.River.Total.Precip..mm.")], value.name="value", id=1)

ggplot(data=melteddata, mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
ggsave("histograms.png",width = 8, height = 8)

#
# Plot with projected line out til 2030
#
ggplot(mydata, aes(x=Year, y=Ice.Out.Julian, color=Ice.Out.Julian)) + geom_point(size=2) +
  theme(axis.text = element_text(size=12)) +
  scale_colour_gradientn(colours=rainbow(4)) + xlim(1932, 2030) +
  stat_smooth(method="lm", fullrange=TRUE)

#
# Divide into training and test
#
set.seed(557)
# Creating training  and testing  data sets: Randomly picking 75% of data points for training
# and 25% for test
trsize = as.integer(.75*nrow(mydata_ice))
rndSample <- sample(1:nrow(mydata_ice), trsize)
tr <- mydata_ice[rndSample, ] #training data: randomly picked 
ts <- mydata_ice[-rndSample, ] # remaining will be test datac

###
# Logisic Regression
###
glm <- glm(Ice.Out.Vs.Trend ~ Global.Temp.Departure+ColdFactor+Blind.River.Total.Rain..mm.+
           Blind.River.Total.Snow..cm., data=tr, family="binomial" ) # Fit logsitic regressions to data
ps <- predict(glm, ts, type="response", interval="confidence")  #  classify test data
#ps
(cm <- table(ps, ts$Ice.Out.Vs.Trend)) # confusion matrix for evaluation
100*(1-sum(diag(cm))/sum(cm))  # determine error rate
cm
summary(glm)
###
# Linear model to predict Julian Day
###
lm <- lm(Ice.Out.Julian ~ Global.Temp.Departure+ColdFactor+Blind.River.Total.Rain..mm.+
            Blind.River.Total.Snow..cm., data=tr) 
ps <- predict(lm, ts)
#ps
#(cm <- table(ps, ts$Ice.Out.Julian)) # confusion matrix for evaluation
r2error = sqrt(avg((ts$Ice.Out.Julian - ps)^2))
ts$ps <- ps
View(ts[,c("Ice.Out.Julian","ps")])

##
# ANN to predict Early or Late Ice off Date
###
library(nnet)
n <- nnet(Ice.Out.Vs.Trend ~ Global.Temp.Departure+ColdFactor+Blind.River.Total.Rain..mm.+
            Blind.River.Total.Snow..cm., data=tr, size=20 ,trace=FALSE, maxit=10000, decay=.001) # train an ANN over ice out data
ps <- predict(n, ts, type="class")  #  classify test data
ps
(cm <- table(ps, ts$Ice.Out.Vs.Trend)) # confusion matrix for evaluation
100*(1-sum(diag(cm))/sum(cm))  # determine error rate
cm
#mydata_ice$ps <- ps

############################
# Visualization of ANN
###########################
library(NeuralNetTools)
## Feature importance (left graph)
garson(n) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
## Network diagram (rigth graph)
plotnet(n) 
#
#
# Naive Bayes Predictor
#
library (e1071)
#
# use tune function for 10fold cross validation of naiveBayes using same training set
#
#tr <- na.omit(tr)
#ts <- na.omit(ts)
#nb <- tune(naiveBayes , LiverPatient ~ AAP + SAlA + SAsA + Gender + TB + TP + Age + ALB + DB, data=tr)
nb <- tune(naiveBayes, Ice.Out.Vs.Trend ~ Global.Temp.Departure+ColdFactor+Blind.River.Total.Rain..mm.+
             Blind.River.Total.Snow..cm., data=tr)
summary(nb$best.model)
#
# predict using test set
#
nb.pred <- predict(nb$best.model, ts)
nb.matrix <- table(nb.pred, ts$Ice.Out.Vs.Trend)    #confusion matrix
nb.matrix
100*(1-sum(diag(nb.matrix))/sum(nb.matrix))  # determine error rate


#
# Divide into training and test
#
set.seed(557)
# Creating training  and testing  data sets: Randomly picking 75% of data points for training
# and 25% for test
trsize = as.integer(.75*nrow(mydata_ice))
rndSample <- sample(1:nrow(mydata_ice), trsize)
tr <- mydata_ice[rndSample, ] #training data: randomly picked 
ts <- mydata_ice[-rndSample, ] # remaining will be test datac

###
# Logisic Regression
###
glm <- glm(Ice.Out.Vs.Trend ~ Global.Temp.Departure+ColdFactor+Blind.River.Total.Rain..mm.+
             Blind.River.Total.Snow..cm., data=tr, family="binomial" ) # Fit logsitic regressions to data
ps <- predict(glm, ts, type="response", interval="confidence")  #  classify test data
#ps
(cm <- table(ps, ts$Ice.Out.Vs.Trend)) # confusion matrix for evaluation
100*(1-sum(diag(cm))/sum(cm))  # determine error rate
cm
summary(glm)


###
# Revised Model for 2015-2018 prediction on May 3 2018
###
library(date)
tr2 <- mydata[which(mydata$Year < 2015), ] # Train with all data thru 2014
ts2 <- mydata[which(mydata$Year > 2014 & mydata$Year < 2019), ] # predict 2014-2018
lm2018 <- lm(Ice.Out.Julian ~ Global.Temp.Departure+ColdFactor+Blind.River.JFM.Snow+Blind.River.Prior.Year.Rain, data=tr) 
ps2018 <- predict(lm2018, ts2)
ts2$ps2018<-ps2018
ts2$ps2018date<-date.ddmmmyy(ps2018)
  #(cm <- table(ps, ts$Ice.Out.Julian)) # confusion matrix for evaluation
View(ts2[,c("Year","ps2018","psdate")])

ps <- predict(lm, ts2)
ts2$ps<-ps
ts2$psdate<-date.ddmmmyy(ps2018)
#(cm <- table(ps, ts$Ice.Out.Julian)) # confusion matrix for evaluation
View(ts2[,c("Year","ps2018","psdate","ps","psdate")])





