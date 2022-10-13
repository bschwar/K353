rm(list = ls())
load(url("https://www.dropbox.com/s/g852wf1akx3xljl/airbnb_project.rdata?dl=1"))
#############################################################
#Add Num Reserve Days
listing_2016Q2_booking <- listing_2016Q2[listing_2016Q2$Status=='R',]
agg_booking_Q2 <- aggregate(Status~PropertyID ,data=listing_2016Q2_booking,length)
colnames(agg_booking_Q2)<- c("PropertyID","BookingQ2")
property_info<- merge(x=property_info,y=agg_booking_Q2,by="PropertyID",all.x=TRUE)
property_info$BookingQ2[is.na(property_info$BookingQ2)]=0
##
listing_2016Q1_booking <- listing_2016Q1[listing_2016Q1$Status=='R',]
agg_booking_Q1 <- aggregate(Status~PropertyID ,data=listing_2016Q1_booking,length)
colnames(agg_booking_Q1)<- c("PropertyID","BookingQ1")
property_info<- merge(x=property_info,y=agg_booking_Q1,by="PropertyID",all.x=TRUE)
property_info$BookingQ1[is.na(property_info$BookingQ1)]=0

#Seperate test and train data
property_info_train<-property_info[property_info$PropertyID %in% reserve_2016Q3_train$PropertyID,]
property_info_test<-property_info[property_info$PropertyID %in% PropertyID_test,]

#Add Q3 Booking
property_info_train<-merge(x=property_info_train,y=reserve_2016Q3_train,by="PropertyID",all.x=TRUE)

#Test if Linear
#########################################################################
plot(property_info_train[,c("NumReserveDays2016Q3","NumberofReviews")])
abline(lm(property_info_train$NumReserveDays2016Q3 ~ property_info_train$NumberofReviews))
plot(property_info_train[,c("NumReserveDays2016Q3","MinimumStay")])
abline(lm(property_info_train$NumReserveDays2016Q3 ~ property_info_train$MinimumStay))
plot(property_info_train[,c("NumReserveDays2016Q3","BusinessReady")])
abline(lm(property_info_train$NumReserveDays2016Q3 ~ property_info_train$BusinessReady))
plot(property_info_train[,c("NumReserveDays2016Q3","BookingQ2")])
abline(lm(property_info_train$NumReserveDays2016Q3 ~ property_info_train$BookingQ2))
plot(property_info_train[,c("NumReserveDays2016Q3","BookingQ1")])
abline(lm(property_info_train$NumReserveDays2016Q3 ~ property_info_train$BookingQ1))

#Linear Regression Tests
###############################
lin_reg1 <- lm(formula = NumReserveDays2016Q3 ~ NumberofReviews
              +OverallRating + CleanRating + CheckinRating + ResponseTimemin 
              +MinimumStay + BusinessReady 
              +InstantbookEnabled + BookingQ2+BookingQ1, data = property_info_train)

#######Other Statistically Significant variables are Neighborhood, Property Type, and ################################

summary(lin_reg1)

#Test on Training Data
testOnTrain<-predict(object=lin_reg1,
        newdata = property_info_train)

MSE1<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - testOnTrain)^2, na.rm = TRUE))

#Test 2
lin_reg2 <- lm(formula = NumReserveDays2016Q3 ~ NumberofReviews
               +MinimumStay + BusinessReady+BookingQ2+BookingQ1, data = property_info_train)

summary(lin_reg2)

#Test on Training Data
testOnTrain<-predict(object=lin_reg2,
                     newdata = property_info_train)

MSE2<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - testOnTrain)^2, na.rm = TRUE))

#####################################################################
##Regression Tree
#####################################################################
install.packages('rpart')
library(rpart)
#############################################
####Tree with significant values from Lin1
Tree1<- rpart(formula = NumReserveDays2016Q3 ~ NumberofReviews
   +OverallRating + CleanRating + CheckinRating + ResponseTimemin 
   +MinimumStay + BusinessReady 
   +InstantbookEnabled + BookingQ2+BookingQ1, data = property_info_train, method="anova")

testOnTrain<-predict(object=Tree1,
                     newdata = property_info_train)

MSETree<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - testOnTrain)^2, na.rm = TRUE))

####Tree with Values from Lin 2
Tree2<- rpart(formula = NumReserveDays2016Q3 ~ NumberofReviews
              +MinimumStay + BusinessReady+BookingQ2+BookingQ1, data = property_info_train, method="anova", control = rpart.control(minsplit=10))

testOnTrain<-predict(object=Tree2,
                     newdata = property_info_train)

MSETree2<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - testOnTrain)^2, na.rm = TRUE))

plot(Tree2, 
     uniform=TRUE,
     ylim=c(0,1.5),xlim=c(0,13))
text(Tree2, cex=0.5, use.n=TRUE)
###############################################################################################
##Random Forest

#install.packages('randomForest')
library(randomForest)

##All significant
forestModel<-randomForest(formula=NumReserveDays2016Q3 ~ NumberofReviews+OverallRating + CleanRating + CheckinRating + ResponseTimemin+MinimumStay + BusinessReady+InstantbookEnabled + BookingQ2+BookingQ1,data=property_info_train,ntree=500,importance=TRUE)

forestModel
varImpPlot(forestModel)
pred_forestModel=predict(forestModel,property_info_train)
MSEForest<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - pred_forestModel)^2, na.rm = TRUE))

##All from LM2
forestModel2<-randomForest(formula=NumReserveDays2016Q3 ~ NumberofReviews +MinimumStay + BusinessReady+BookingQ2+BookingQ1,data=property_info_train,ntree=500,importance=TRUE)

forestModel2
varImpPlot(forestModel2)
pred_forestModel2=predict(forestModel2,property_info_train)
MSEForest2<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - pred_forestModel2)^2, na.rm = TRUE))

#################################################################################################
##Final Stuff

#Test on test Data Using Best MSE
Prediction<-predict(object=forestModel2,
                    newdata = property_info_test)


#################################################
#install.packages(c("httr","jsonlite"))
#library(httr)
#library(jsonlite)

#res<-GET("https://api.census.gov/data/2020/acs/acs5/variables/B19301_001E.json")
#res
#data<-fromJSON(rawToChar(res$content))


