library("TTR")
library("xts")
library("quantmod")
setDefaults("source",echo=FALSE)
#
# Read in Data, convert date for extended time series
#
branddata <- read.csv(file="../Multi-USFebrezeTrainData2.csv", head=TRUE, sep=",")
branddata$month_end_date <- strptime(branddata$month_end_date, format ="%m/%d/%Y")
goaldata <- read.csv(file="../Multi-USFebrezeGoalData.csv", head=TRUE, sep=",")
goaldata$month_end_date <- strptime(goaldata$month_end_date, format ="%m/%d/%Y")
#
# don't store date column since XTS will create own, and will allow other columns to be read as numeric
#
pen.traingraph <- xts(x=branddata[,-1], order.by = branddata[,"month_end_date"])
pen.train <- xts(x=branddata[,-1], order.by = branddata[,"month_end_date"])
pen.goal <- xts(x=goaldata[,-1], order.by = goaldata[,"month_end_date"])

pen.goal$Composite_Indicator = normalize(pen.goal$Composite_Indicator)
pen.goal$Penetration_Amt = normalize(pen.goal$Penetration_Amt)

pen.train$Composite_Indicator = normalize(pen.train$Composite_Indicator)
pen.train$Penetration_Amt = normalize(pen.train$Penetration_Amt)

print (pen.train)
print (pen.goal)
#
# Fit Model with training data using 3 states
#
library("RHmm")
hm_model <- HMMFit(obs=pen.train, nStates=3, control=list(iter=200))
print(hm_model)
#
# Determine most probable through training data
#
VitPath <- viterbi(hm_model, pen.train)
print(VitPath)
#
# Determine probability of future sequence projection
#
VitPathGoal <- viterbi(hm_model, pen.goal)
print(VitPathGoal)

#print(VitPathGoal$logViterbiScore)
print(paste("Probability of Goal Sequence:", exp(VitPath$logProbSeq)))
#
# Chart the known Penetration History, and forecast penetration, with marks to show state changes
#
pen.predict <- rbind(pen.train[,-2], pen.goal[,-2])
pen_amounts <- xts(branddata$Penetration_Amt, order.by = branddata[,"month_end_date"])
pen_amountsgoal <- xts(goaldata$Penetration_Amt, order.by = goaldata[,"month_end_date"])
pen_amounts <- rbind(pen_amounts, pen_amountsgoal)

pen.predict <- cbind(pen.predict, pen_amounts)
pen.predict <- cbind(pen.predict, 0,0,0)
colnames(pen.predict)[3] <- "state"
colnames(pen.predict)[4] <- "change"
colnames(pen.predict)[2] <- "Penetration_Amt"
colnames(pen.predict)[5] <- "PenetrationPredicted"
#chg_calc <- matrix(unlist(hm_model$HMM$distribution$mean),nrow=1,ncol=3)
chg_calc <- c(-.17, .03, .12)
#
# use loops to calculate Penetration values based on initial plus series of changes
#
for (i in 1:nrow(branddata)) {
  pen.predict$Penetration_Amt[i]=branddata$Penetration_Amt[i]
}
pen.predict$PenetrationPredicted[1] = pen.predict$Penetration_Amt[1]
VitPathStatesCombined <- c(VitPath$states, VitPathGoal$states)

for (i in 1:nrow(pen.predict)) {
  #print(paste("i=",i), quote=FALSE)
  pen.predict$state[i]= VitPathStatesCombined[i]
  pen.predict$change[i] = chg_calc[pen.predict$state[i]]
  if (i < nrow(pen.predict)) {pen.predict$PenetrationPredicted[i+1]=pen.predict$PenetrationPredicted[i] + pen.predict$change[i] }
  if (pen.predict$Penetration_Amt[i] == 0 ) {pen.predict$Penetration_Amt[i] = NA }
}

#
# For charting purposes, build a time series with just the forecast data, but calculated started from the last known actual value.
#
pen.forecast <- tail(pen.predict,nrow(pen.goal))
pen.forecast$PenetrationPredicted = branddata$Penetration_Amt[nrow(branddata)]

for (i in 1:nrow(pen.forecast)) {
  #print(paste("i=",i), quote=FALSE)
  pen.forecast$state[i]= VitPathStatesCombined[i]
  pen.forecast$change[i] = chg_calc[pen.predict$state[i]]
  if (i < nrow(pen.forecast)) {pen.forecast$PenetrationPredicted[i+1]=pen.forecast$PenetrationPredicted[i] + pen.forecast$change[i] }
}
#
# Chart the Result
#

chartSeries(pen.predict$PenetrationPredicted, type="line",theme="black", legend="Forecast",name="Penetration", TA="addTA(pen.traingraph$Penetration_Amt, on=1, col=4)" )
addTA(pen.forecast$PenetrationPredicted, on=1, col=8, type="l")
addTA(pen.predict[pen.predict[,"state"]==1,"Penetration_Amt"],on=1,type="p",col=5,pch=25)
addTA(pen.predict[pen.predict[,"state"]==2,"Penetration_Amt"],on=1,type="p",col=6,pch=23)
addTA(pen.predict[pen.predict[,"state"]==3,"Penetration_Amt"],on=1,type="p",col=7,pch=24)

normalize <- function(x) { 
  x <- sweep(x, 2, apply(x, 2, min)) 
  sweep(x, 2, apply(x, 2, max), "/") 
} 

