Overview
========

Use data from accelerometers on the belt, forearm, arm, and dumbell of 6
participants to predict which activity they were performing. They were
asked to perform barbell lifts correctly and incorrectly in 5 different
ways.

Data
====

The training data for this project are available
[here](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)  
The test data are available
[here](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)  
The data for this project come from this source:
<http://groupware.les.inf.puc-rio.br/har>.  
More information is available from the website here: [Human Activity
Recognition](http://groupware.les.inf.puc-rio.br/har) (see the section
on the Weight Lifting Exercise Dataset).

Loading Data and Library
========================

Download traning data and testing data into working directory and load
data.

    library(dplyr)
    library(caret)
    library(rpart)
    training <- read.csv("pml-training.csv")
    testing <- read.csv("pml-testing.csv")
    dim(training)

    ## [1] 19622   160

    dim(testing)

    ## [1]  20 160

The training dataset should have 19622 rows and 160 columns.

Feature Selection
=================

Select most useful features.

    names<-names(training)
    features <- names[grepl("^roll|^pitch|^yaw|^gyros|^accel|^magnet.*",names)]
    training_feature <- training[,c(features,"classe")]

Exploratory Data Analysis & Base-line Accuracy
==============================================

    summary(training_feature$classe)

    ##    A    B    C    D    E 
    ## 5580 3797 3422 3216 3607

    baseline <- summary(training_feature$classe)[1]/nrow(training_feature)

Our traning data contains 5 classes. Our model accuracy should be larger
than 0.2843747, which is accuracy of predicting the majority class all
the time.

Train Models Using Cross Validation
===================================

Train gbm and rf model.

    set.seed(123)
    inTrain <- createDataPartition(training_feature$classe,p=0.8,list = FALSE)
    train <- training_feature[inTrain,]
    validation <- training_feature[-inTrain,]
    tc <- trainControl("cv",10)
    if(file.exists("gbmmodel.rd")){
      readRDS("gbmmodel.rd")
    }else{
      gbmmodel <- train(classe~.,method="gbm",trControl=tc,data=train)
    }
    if(file.exists("rfmodel.rd")){
      readRDS("rfmodel.rd")
    }else{
      rfmodel <- train(classe~.,method="rf",trControl=tc,data=train)
    }

Combine two models with rf model.

    gbm_est <- predict(gbmmodel,train)
    rf_est <- predict(rfmodel,train)
    if(file.exists("combinemodel.rd")){
      readRDS("combinemodel.rd")
    }else{
      ests <- data.frame(gbm_est=gbm_est,rf_est=rf_est,classe=train$classe)
      combine_model <- train(classe~.,method="rf",data=ests)
    }

Model Evaluation
================

Evaluation on validation data.

    gbm_est <- predict(gbmmodel,validation)
    rf_est <- predict(rfmodel,validation)
    ests <- data.frame(gbm_est=gbm_est,rf_est=rf_est)
    combine_est <- predict(combine_model,ests)
    confusionMatrix(gbm_est,validation$classe)$overall["Accuracy"]

    ##  Accuracy 
    ## 0.9638032

    confusionMatrix(rf_est,validation$classe)$overall["Accuracy"]

    ## Accuracy 
    ## 0.994392

    confusionMatrix(combine_est,validation$classe)$overall["Accuracy"]

    ## Accuracy 
    ## 0.994392

On validation data, combined model performs equally well as rf model.
Choose rf model for the convenience of data preprocessing.

Variable Importance
===================

    varImp(rfmodel$finalModel)

    ##                    Overall
    ## roll_belt         643.1432
    ## pitch_belt        420.7276
    ## yaw_belt          517.2509
    ## gyros_belt_x      147.5908
    ## gyros_belt_y      146.3473
    ## gyros_belt_z      255.8693
    ## accel_belt_x      169.8875
    ## accel_belt_y      160.0943
    ## accel_belt_z      326.7118
    ## magnet_belt_x     227.0831
    ## magnet_belt_y     317.5985
    ## magnet_belt_z     334.2471
    ## roll_arm          278.0508
    ## pitch_arm         198.0575
    ## yaw_arm           223.5487
    ## gyros_arm_x       184.8183
    ## gyros_arm_y       176.8710
    ## gyros_arm_z       103.8034
    ## accel_arm_x       250.3441
    ## accel_arm_y       192.4414
    ## accel_arm_z       179.9224
    ## magnet_arm_x      242.8204
    ## magnet_arm_y      249.7541
    ## magnet_arm_z      198.6414
    ## roll_dumbbell     322.3415
    ## pitch_dumbbell    212.0130
    ## yaw_dumbbell      252.6936
    ## gyros_dumbbell_x  157.7407
    ## gyros_dumbbell_y  251.7384
    ## gyros_dumbbell_z  120.5823
    ## accel_dumbbell_x  256.4273
    ## accel_dumbbell_y  350.9134
    ## accel_dumbbell_z  308.1369
    ## magnet_dumbbell_x 370.6638
    ## magnet_dumbbell_y 419.8749
    ## magnet_dumbbell_z 448.1362
    ## roll_forearm      364.8781
    ## pitch_forearm     443.4977
    ## yaw_forearm       188.9111
    ## gyros_forearm_x   124.6428
    ## gyros_forearm_y   164.3684
    ## gyros_forearm_z   122.4945
    ## accel_forearm_x   266.6877
    ## accel_forearm_y   183.4110
    ## accel_forearm_z   223.6186
    ## magnet_forearm_x  226.1433
    ## magnet_forearm_y  235.8194
    ## magnet_forearm_z  239.6934

Belt matters most!

Using rf Model to Predict
=========================

    test <- testing[,c(features)]
    est_rf_test <- predict(rfmodel,test)
    summary(est_rf_test)

    ## A B C D E 
    ## 7 8 1 1 3
