Daily Modeling Report
================

  - [Introduction](#introduction)
  - [Data, Tuesdays](#data-tuesdays)
  - [Training Data Summary](#training-data-summary)
  - [Model Fitting](#model-fitting)
      - [Decision Tree](#decision-tree)
      - [Gradient Boosted Tree](#gradient-boosted-tree)
      - [Test Fits, Tuesdays](#test-fits-tuesdays)

``` r
library(tidyverse)
library(caret)  
library(knitr)
# Day of week decoder
dfDOW<- data.frame(num<- 0:6, name <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
```

# Introduction

We are working with a bikeshare dataset. The number of bike renters
using a Washington, D.C. based rental service on a daily basis from
2011-2012 was aggregated by researchers and made
[available](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset).
There are 16 variables in the daily dataset.

  - 1 Meta column, indicating the index of the observation in the
    dataset.  
  - 7 date related columns – the date itself, as well as helper
    variables indicating if it was a workday / holiday / etc.
  - 5 weather related columns – a grade for precipitation, as well as
    fields for temperature, heat index, humidity, and windspeed  
  - 3 response variables – A count of casual users, registered users,
    and the total count

This report is focused on predicting the total user count on any Tuesday
given the date and weather information.

We will perform this modeling using both a simple decision tree with
leave-one-out cross validation, as well as a gradient boosted tree model
with 5-fold cross validation.

# Data, Tuesdays

For linear modeling we would typically convert most of the discrete
derived date variables as a factor to ensure that something like the
cyclical nature of the seasons are treated more appropriately. A
decision tree with a sufficient number of levels should be able to
reasonably account for nonlinear behavior, so we will not factorize
those variables for this modeling.

``` r
#Import the data
tibDaily<- read_csv('day.csv')

tibDOW<- tibDaily%>% filter(weekday == params$DOW)%>% select(-casual, -registered)

set.seed(1)
train <-sample(1:nrow(tibDOW), size =nrow(tibDOW)*0.7)
test <- dplyr::setdiff(1:nrow(tibDOW), train)
tibTrain <- tibDOW[train, ]
tibTest <- tibDOW[test, ]
```

# Training Data Summary

This is a quick overview of the training dataset for this Tuesday
report. It will be good to look at the number of holidays and working
days in this set. We will also plot the ridership trends vs time, and
the relationship with the weather variables. Note that the data has
already been standardized, so we will not rescale it further.

``` r
#Summarize holiday performance
tibSumm <- tibTrain %>% group_by(holiday)%>% 
  summarize(count = n(), avgRidership = mean(cnt, na.rm = TRUE))

#Plot out trends vs date
ggplot(data = tibTrain, mapping = aes(x = dteday, y = cnt, color = as.factor(season),
  shape = as.factor(workingday))) + geom_point()+
  ggtitle('Ridership vs Date') + xlab('Date') + ylab('Rider Count')
```

![](Tuesday_files/figure-gfm/Exploratory%20Plots-1.png)<!-- -->

``` r
#Gather all continuous weather variables for easy faceting
tibWeather <- gather(tibTrain, key = "Variable", value = "value", -cnt)%>% 
  filter(Variable %in% c('weathersit', 'temp', 'atemp', 'hum', 'windspeed'))

#Scatterplots for weather
ggplot (data = tibWeather, mapping = aes(x = value, y = cnt)) + geom_point() +
  geom_density2d() +  facet_wrap(~Variable, scales = 'free') + xlab('Standardized Weather Metric') + 
  ylab('Rider Count') + ggtitle('Ridership vs Weather Data')
```

![](Tuesday_files/figure-gfm/Exploratory%20Plots-2.png)<!-- -->

# Model Fitting

## Decision Tree

We will start by fitting a simple decision tree using the caret package.

We will omit the following variables from the predictors:

  - instant – the data index, doesn’t contain any real information.  
  - dteday – potentially confounds the other date variables.  
  - weekday – This is dependent on the day of week of the report, and
    will be a constant for every observation.
  - workingday – Since this report is based on a single day of the week,
    this field is the exact opposite of the holiday variable. Excluding
    to simplify runtime.

We will also add in the following interaction terms since they’d likely
influence someone’s desire to spend time outside of cover:

  - temp:holiday
  - windspeed:weathersit
  - hum:temp

No quadratic terms will be used, we will rely on the decision tree
algorithm to navigate non-linearities.

### Cross Validation

We are running leave one out cross validation

``` r
#Define caret cv control information
trctrl <- trainControl(method = "LOOCV")
```

### Fit the Decision Tree

We are running the `rpart` method in the `caret` package. We will use
the default tuning algorithm which should be more robust than simple
checks for absolute RMSE minima in an automated report.

``` r
#  Define a seed so each model starts at the same place 
set.seed(3333)
# Run the model with centered & scaled data, default tuning parameters
treeFit <- train(cnt ~ .-instant-dteday-weekday-workingday+
                 temp:holiday+windspeed:weathersit+hum:temp,
                 data = tibTrain, method = "rpart",
                 trControl=trctrl)

treeFit
```

    ## CART 
    ## 
    ## 72 samples
    ## 13 predictors
    ## 
    ## No pre-processing
    ## Resampling: Leave-One-Out Cross-Validation 
    ## Summary of sample sizes: 71, 71, 71, 71, 71, 71, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp          RMSE      Rsquared      MAE      
    ##   0.05621194  1291.538  0.4986061711   961.6789
    ##   0.25426425  1581.633  0.2736833271  1432.1560
    ##   0.43293590  2005.768  0.0001829279  1788.8942
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.05621194.

``` r
plot(treeFit)
```

![](Tuesday_files/figure-gfm/Classification%20Tree-1.png)<!-- -->

``` r
finalTree<- treeFit$finalModel
```

The final model had a training RMSE of 1291.5383789 on a tuning
parameter of cp=0.0562119. It has 5 branches and 3 termination points

## Gradient Boosted Tree

We will omit the following variables from the predictors:

  - `instant` – the data index, doesn’t contain any real information.  
  - `dteday` – potentially confounds the other date variables.  
  - `weekday` – This is dependent on the day of week of the report, and
    will be a constant for every observation.  
  - `workingday` – Since this report is based on a single day of the
    week, this field is redundant, as it is either going to be constant
    or potentially flip with `holiday`. Excluding to simplify.

We will not explicitly call out any interaction terms since the gradient
boosting algorithm includes a review of the interaction extent by
default – the interaction.depth parameter.

### Cross Validation

We will use 5-fold repeated cv for cross validating the boosted fit.
This should be faster than the leave-one-out fit we used on the previous
model.

``` r
cvInfo <- trainControl(method = 'repeatedcv', number = 5, repeats = 3)
```

### Fit the Gradient Boosted Tree

We are running the `gbm` method in the `caret` package. We will use the
default tuning algorithm as it should be more robust than a generic grid
search in an automated report.

``` r
set.seed(3333)
# Run the model with centered & scaled data, default tuning grid
boostFit <- train(cnt ~ .-instant-dteday-weekday-workingday,
                  data = tibTrain, method = "gbm", trControl=cvInfo)
```

``` r
#Review the numeric results of the fit
boostFit
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 72 samples
    ## 13 predictors
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 56, 58, 57, 60, 57, 58, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE      Rsquared   MAE     
    ##   1                   50      905.0575  0.7756279  706.4291
    ##   1                  100      835.8555  0.8045389  657.3436
    ##   1                  150      817.6501  0.8105302  651.6018
    ##   2                   50      906.6410  0.7763976  706.3101
    ##   2                  100      842.9497  0.8004690  660.5063
    ##   2                  150      826.1837  0.8063423  648.3256
    ##   3                   50      885.3465  0.7882141  697.6077
    ##   3                  100      814.0813  0.8117621  642.8956
    ##   3                  150      803.2385  0.8174814  633.9795
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.

``` r
#Graphic representation of the tuning parameters vs RMSE
plot(boostFit)
```

![](Tuesday_files/figure-gfm/Boosted%20Results-1.png)<!-- -->

The best tuning parameters were:

  - n.trees: 150  
  - interaction.depth: 3  
  - shrinkage: .1 (default)  
  - n.minobsinnode: 10 (default)

The resulted in a training RMSE of 803.2385495

The significant variables in the final model were: season, yr, mnth,
holiday, weathersit, temp, atemp, hum, windspeed

## Test Fits, Tuesdays

Now we compare these models against the test set.

``` r
#Generate Test Set predictions with the Decision Tree Final Model
treePrediction<- predict(treeFit, tibTest)
treeTest<- postResample(treePrediction, tibTest$cnt)

#Generate Test Set prediction with the Gradient Boosted Tree Final Model
boostedPrediction<- predict(boostFit, tibTest)
boostTest<- postResample(boostedPrediction, tibTest$cnt)
```

We find on the test set that the Boosted tree had a better test RMSE.
The boosted tree RMSE was 881.88 while the decision tree had an RMSE of
1408.8. The decision tree had 159.7% of the error of the boosted tree.
