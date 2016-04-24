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
than 0.2843747, which is accuracy of predictiong the majority class all
the time.

Train Models Using Cross Validation
===================================

Train lda and rpart model.

    set.seed(123)
    tc <- trainControl("cv",10)
    ldamodel <- train(classe~.,method="lda",trControl=tc,data=training_feature)

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    rpartmodel <- train(classe~.,method="rpart",trControl=tc,data=training_feature)

Stack three models together.

    lda_est <- predict(ldamodel,training_feature)
    rpart_est <- predict(rpartmodel,training_feature)
    ests <- data.frame(lda_est=lda_est,rpart_est=rpart_est,classe=training_feature$classe)
    combine_model <- train(classe~.,method="rpart",data=ests)

Model Evaluation
================

Evaluation on training data. Since my laptop is rusty, I am not running
gbm or rf model.

    combine_est <- predict(combine_model,ests)
    confusionMatrix(lda_est,training_feature$classe)$overall["Accuracy"]

    ##  Accuracy 
    ## 0.6959535

    confusionMatrix(rpart_est,training_feature$classe)$overall["Accuracy"]

    ##  Accuracy 
    ## 0.4955662

    confusionMatrix(combine_est,training_feature$classe)$overall["Accuracy"]

    ##  Accuracy 
    ## 0.6027418

On training data, lda model is the best fit. Combining lda and rpart
model with rpart model reduce accuracy compared with lda only. Thus
we'll be using lda as the final model.

Plot lda\_est against classe
============================

    qplot(x=training_feature$classe,y=lda_est)+geom_jitter()

![](project_files/figure-markdown_strict/unnamed-chunk-7-1.png)<!-- -->
As shown in the plot, the model's performance is rather good. Density of
points around the diagnoal(classe==lda\_est) is higher.

Using lda Model to Predict
==========================

    testing_feature <- testing[,c(features)]
    lda_est_test <- predict(ldamodel,testing_feature)
    lda_est_test

    ##  [1] D A B C C C D D A A D A E A E A A B B B
    ## Levels: A B C D E

    summary(lda_est_test)

    ## A B C D E 
    ## 7 4 3 4 2