rm(list = ls())
load(url("https://www.dropbox.com/s/g852wf1akx3xljl/airbnb_project.rdata?dl=1"))
###HouseKeeping#############################################################
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

#Add Average Price
avg_priceQ1_mean <- aggregate(Price~PropertyID, data=listing_2016Q1,mean)
colnames(avg_priceQ1_mean)<- c("PropertyID","meanPriceQ1")
property_info<- merge(x=property_info,y=avg_priceQ1_mean,by="PropertyID",all.x=TRUE)
##
avg_priceQ2_mean <- aggregate(Price~PropertyID, data=listing_2016Q2,mean)
colnames(avg_priceQ2_mean)<- c("PropertyID","meanPriceQ2")
property_info<- merge(x=property_info,y=avg_priceQ2_mean,by="PropertyID",all.x=TRUE)

#Fix Neighborgood
tb_neighborhood = table(property_info$Neighborhood)
rare_neighborhood = names(tb_neighborhood[tb_neighborhood<=20])
property_info$Neighborhood[property_info$Neighborhood %in% rare_neighborhood] = "rare neighborhood"

#Fix Zipcode
tb_zipcode = table(property_info$Zipcode)
rare_zipcode = names(tb_zipcode[tb_zipcode<=20])
property_info$Zipcode[property_info$Zipcode %in% rare_zipcode] = "rare zipcode"

#Seperate test and train data
property_info_train<-property_info[property_info$PropertyID %in% reserve_2016Q3_train$PropertyID,]
property_info_test<-property_info[property_info$PropertyID %in% PropertyID_test,]

#Add Q3 Booking
property_info_train<-merge(x=property_info_train,y=reserve_2016Q3_train,by="PropertyID",all.x=TRUE)

#####Packages#############

##install.packages('rpart')
library(rpart)

#install.packages('randomForest')
library(randomForest)

#install.packages('gbm')
library(gbm)

#install.packages('mlbench')
library(mlbench)

###Functions#########################################################
lmFunction<- function(fvars){
  set.seed(0)
  property_info_train$CVLabel = sample(x=1:5,size=nrow(property_info_train),replace=TRUE)
  cv_mse_lr <- rep(NA,3)
  v_label<-1
  
  f <- as.formula(paste("NumReserveDays2016Q3", paste(fvars, collapse = " + "), sep = " ~ "))
  print(f)
  while(v_label<6){
    lr_model <- lm(formula=f,data=property_info_train[property_info_train$CVLabel != v_label,])
    
    lr_pred <- predict(lr_model,newdata=property_info_train[property_info_train$CVLabel == v_label,])
    
    cv_mse_lr[v_label] <- mean((property_info_train$NumReserveDays2016Q3[property_info_train$CVLabel == v_label]-lr_pred)^2,na.rm=TRUE)
    
    v_label<-1+v_label
  }
  
  CV_MSE<-sqrt(mean(cv_mse_lr))
  print(CV_MSE)
  return(CV_MSE)
}

rfFunction<- function(fvars,NumTree){
  set.seed(0)
  property_info_train$CVLabel = sample(x=1:5,size=nrow(property_info_train),replace=TRUE)
  cv_mse_lr <- rep(NA,3)
  v_label<-1
  
  f <- as.formula(paste("NumReserveDays2016Q3", paste(fvars, collapse = " + "), sep = " ~ "))
  print(f)
  while(v_label<6){
    rf_model <- randomForest(formula=f,data=property_info_train[property_info_train$CVLabel != v_label,],ntree=NumTree,importance=TRUE)
    
    rf_pred <- predict(rf_model,newdata=property_info_train[property_info_train$CVLabel == v_label,])
    
    cv_mse_lr[v_label] <- mean((property_info_train$NumReserveDays2016Q3[property_info_train$CVLabel == v_label]-rf_pred)^2,na.rm=TRUE)
    
    v_label<-1+v_label
  }
  
  CV_MSE<-sqrt(mean(cv_mse_lr))
  print(CV_MSE)
  return(CV_MSE)
}

treeFunction<- function(fvars,MinSplit){
  set.seed(0)
  property_info_train$CVLabel = sample(x=1:5,size=nrow(property_info_train),replace=TRUE)
  cv_mse_lr <- rep(NA,3)
  v_label<-1
  
  f <- as.formula(paste("NumReserveDays2016Q3", paste(fvars, collapse = " + "), sep = " ~ "))
  print(f)
  while(v_label<6){
    tree_model <- rpart(formula=f,data=property_info_train[property_info_train$CVLabel != v_label,],method="anova", control = rpart.control(minsplit=MinSplit))
    
    tree_pred <- predict(tree_model,newdata=property_info_train[property_info_train$CVLabel == v_label,])
    
    cv_mse_lr[v_label] <- mean((property_info_train$NumReserveDays2016Q3[property_info_train$CVLabel == v_label]-tree_pred)^2,na.rm=TRUE)
    
    v_label<-1+v_label
  }
  
  CV_MSE<-sqrt(mean(cv_mse_lr))
  print(CV_MSE)
  return(CV_MSE)
}

boostedFunction<- function(fvars,nTrees,iDepth,Shrink){
  set.seed(0)
  property_info_train$CVLabel = sample(x=1:5,size=nrow(property_info_train),replace=TRUE)
  cv_mse_lr <- rep(NA,3)
  v_label<-1
  
  f <- as.formula(paste("NumReserveDays2016Q3", paste(fvars, collapse = " + "), sep = " ~ "))
  print(f)
  while(v_label<6){
    boosted_model <- gbm(formula=f,data=property_info_train[property_info_train$CVLabel != v_label,],n.trees = nTrees,interaction.depth = iDepth, shrinkage = Shrink)
    
    boosted_pred <- predict(boosted_model,newdata=property_info_train[property_info_train$CVLabel == v_label,])
    
    cv_mse_lr[v_label] <- mean((property_info_train$NumReserveDays2016Q3[property_info_train$CVLabel == v_label]-boosted_pred)^2,na.rm=TRUE)
    
    v_label<-1+v_label
  }
  
  CV_MSE<-sqrt(mean(cv_mse_lr))
  print(CV_MSE)
  return(CV_MSE)
}

###Linear Regression Tests#############################################
lin_reg1 <- lm(formula = NumReserveDays2016Q3 ~ meanPriceQ1+meanPriceQ2+NumberofReviews
              +OverallRating + CleanRating + CheckinRating + ResponseTimemin 
              +MinimumStay + BusinessReady 
              +InstantbookEnabled + BookingQ2+BookingQ1, data = property_info_train)

summary(lin_reg1)

testOnTrain<-predict(object=lin_reg1,newdata = property_info_train)

RMSE1<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - testOnTrain)^2, na.rm = TRUE))

#Test 2
lin_reg2 <- lm(formula = NumReserveDays2016Q3 ~  meanPriceQ1+meanPriceQ2+NumberofReviews
               +MinimumStay + BusinessReady+BookingQ2+BookingQ1, data = property_info_train)

summary(lin_reg2)

#Test on Training Data
testOnTrain<-predict(object=lin_reg2,newdata = property_info_train)

RMSE2<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - testOnTrain)^2, na.rm = TRUE))

###Regression Tree#######################################################################
####Tree with significant values from Lin1
Tree1<- rpart(formula = NumReserveDays2016Q3 ~ NumberofReviews
   +OverallRating + CleanRating + CheckinRating + ResponseTimemin 
   +MinimumStay + BusinessReady 
   +InstantbookEnabled + BookingQ2+BookingQ1, data = property_info_train, method="anova")

#testOnTrain<-predict(object=Tree1,newdata = property_info_train)

#RMSETree<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - testOnTrain)^2, na.rm = TRUE))

####Tree with Values from Lin 2
Tree2<- rpart(formula = NumReserveDays2016Q3 ~ NumberofReviews
              +MinimumStay + BusinessReady+BookingQ2+BookingQ1, data = property_info_train, method="anova", control = rpart.control(minsplit=10))

#testOnTrain<-predict(object=Tree2,newdata = property_info_train)

#RMSETree2<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - testOnTrain)^2, na.rm = TRUE))

plot(Tree2, 
     uniform=TRUE,
     ylim=c(0,1.5),xlim=c(0,13))
text(Tree2, cex=0.5, use.n=TRUE)
###Random Forest##############################################################
##All from LM2
forestModel2<-randomForest(formula=NumReserveDays2016Q3 ~ meanPriceQ1+meanPriceQ2+NumberofReviews +MinimumStay + BusinessReady+BookingQ2+BookingQ1,data=property_info_train,ntree=1000,importance=TRUE)

forestModel2
varImpPlot(forestModel2)
pred_forestModel2=predict(forestModel2,property_info_train)
RMSEForest2<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - pred_forestModel2)^2, na.rm = TRUE))

###Boosted Trees##########################################################################################
##All from LM2
BoostedTrees<-gbm(formula=NumReserveDays2016Q3 ~ NumberofReviews +MinimumStay+BookingQ2+BookingQ1,data=property_info_train,n.trees = 250,interaction.depth = 2, shrinkage = .01)

pred_BoostedTrees=predict(BoostedTrees,property_info_train)
RMSEBoosted2<-sqrt(mean((property_info_train$NumReserveDays2016Q3 - pred_BoostedTrees)^2, na.rm = TRUE))

####
###Final Stuff###############################################################################################


#Test on test Data Using Best MSE
pred<-predict(object=forestModel2,
                    newdata = property_info_test)
mean(is.na(pred))
##save(pred, file = "I:/2022-23/K353/Final Project/Hoosier_Analytics.rdata") 

###Test with median Income##############################################
#install.packages(c("httr","jsonlite"))
#library(httr)
#library(jsonlite)

#res<-GET("https://api.census.gov/data/2020/acs/acs5/variables/B19301_001E.json")
#res
#data<-fromJSON(rawToChar(res$content))

###Test Using Functions#########################################################
RMSE2t<-lmFunction(c("NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"))
RMSEForest2t<-rfFunction(c("NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),500)
RMSETree2t<-treeFunction(c("NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),10)
RMSEBoosted2t<-boostedFunction(c("NumberofReviews","MinimumStay","BookingQ2","BookingQ1"),250,2,.01)
##with both price
RMSE2tPs<-lmFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"))
RMSEForest2tPs<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),500)
RMSETree2tPs<-treeFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),10)
RMSEBoosted2tPs<-boostedFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BookingQ2","BookingQ1"),250,2,.01)
##with just Q2 price
RMSE2tP<-lmFunction(c("meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"))
RMSEForest2tP<-rfFunction(c("meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),500)
RMSETree2tP<-treeFunction(c("meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),10)
RMSEBoosted2tP<-boostedFunction(c("meanPriceQ2","NumberofReviews","MinimumStay","BookingQ2","BookingQ1"),250,2,.01)
## Forest with different numTrees
#RMSEForestNumTree100<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),100)
#RMSEForestNumTree200<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),200)
#RMSEForestNumTree300<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),300)
#RMSEForestNumTree400<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),400)
#RMSEForestNumTree500<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),500)
#RMSEForestNumTree600<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),600)
#RMSEForestNumTree700<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),700)
#RMSEForestNumTree800<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),800)
#RMSEForestNumTree900<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),900)
RMSEForestNumTree1000<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),1000)
#RMSEForestNumTree1100<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),1100)
#RMSEForestNumTree1200<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),1200)
#RMSEForestNumTree1300<-rfFunction(c("meanPriceQ1","meanPriceQ2","NumberofReviews","MinimumStay","BusinessReady","BookingQ2","BookingQ1"),1300)

##plot(c(RMSEForestNumTree100,RMSEForestNumTree200,RMSEForestNumTree300,RMSEForestNumTree400,RMSEForestNumTree500,RMSEForestNumTree600,RMSEForestNumTree700,RMSEForestNumTree800,RMSEForestNumTree900,RMSEForestNumTree1000,RMSEForestNumTree1100,RMSEForestNumTree1200,RMSEForestNumTree1300))

