Daily Modeling Report
================

  - [Introduction](#introduction)
  - [Data, Sundays](#data-sundays)
  - [Training Data Summary](#training-data-summary)
  - [Model Fitting](#model-fitting)
      - [Decision Tree](#decision-tree)
      - [Gradient Boosted Tree](#gradient-boosted-tree)
      - [Test Fits, Sundays](#test-fits-sundays)

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

This report is focused on predicting the total user count on any Sunday
given the date and weather information.

We will perform this modeling using both a simple decision tree with
leave-one-out cross validation, as well as a gradient boosted tree model
with 5-fold cross validation.

# Data, Sundays

For linear modeling we would typically convert most of the discrete
derived date variables as a factor to ensure that something like the
cyclical nature of the seasons are treated more appropriately. A
decision tree with a sufficient number of levels should be able to
reasonably account for nonlinear behavior, so we will not factorize
those variables for this modeling.

``` r
#Import the data
tibDaily<- read_csv('day.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   instant = col_double(),
    ##   dteday = col_date(format = ""),
    ##   season = col_double(),
    ##   yr = col_double(),
    ##   mnth = col_double(),
    ##   holiday = col_double(),
    ##   weekday = col_double(),
    ##   workingday = col_double(),
    ##   weathersit = col_double(),
    ##   temp = col_double(),
    ##   atemp = col_double(),
    ##   hum = col_double(),
    ##   windspeed = col_double(),
    ##   casual = col_double(),
    ##   registered = col_double(),
    ##   cnt = col_double()
    ## )

``` r
tibDOW<- tibDaily%>% filter(weekday == params$DOW)%>% select(-casual, -registered)

set.seed(1)
train <-sample(1:nrow(tibDOW), size =nrow(tibDOW)*0.7)
test <- dplyr::setdiff(1:nrow(tibDOW), train)
tibTrain <- tibDOW[train, ]
tibTest <- tibDOW[test, ]
```

# Training Data Summary

This is a quick overview of the training dataset for this Sunday report.
It will be good to look at the number of holidays and working days in
this set. We will also plot the ridership trends vs time, and the
relationship with the weather variables. Note that the data has already
been standardized, so we will not rescale it further.

``` r
#Summarize holiday performance
tibSumm <- tibTrain %>% group_by(holiday)%>% 
  summarize(count = n(), avgRidership = mean(cnt, na.rm = TRUE))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
#Plot out trends vs date
ggplot(data = tibTrain, mapping = aes(x = dteday, y = cnt, color = as.factor(season),
  shape = as.factor(workingday))) + geom_point()+
  ggtitle('Ridership vs Date') + xlab('Date') + ylab('Rider Count')
```

![](Sunday_files/figure-gfm/Exploratory%20Plots-1.png)<!-- -->

``` r
#Gather all continuous weather variables for easy faceting
tibWeather <- gather(tibTrain, key = "Variable", value = "value", -cnt)%>% 
  filter(Variable %in% c('weathersit', 'temp', 'atemp', 'hum', 'windspeed'))

#Scatterplots for weather
ggplot (data = tibWeather, mapping = aes(x = value, y = cnt)) + geom_point() +
  geom_density2d() +  facet_wrap(~Variable, scales = 'free') + xlab('Standardized Weather Metric') + 
  ylab('Rider Count') + ggtitle('Ridership vs Weather Data')
```

![](Sunday_files/figure-gfm/Exploratory%20Plots-2.png)<!-- -->

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
    ## 73 samples
    ## 13 predictors
    ## 
    ## No pre-processing
    ## Resampling: Leave-One-Out Cross-Validation 
    ## Summary of sample sizes: 72, 72, 72, 72, 72, 72, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp          RMSE      Rsquared    MAE      
    ##   0.05636109  1154.667  0.60902158   919.8139
    ##   0.15314767  1352.495  0.46747851  1181.0456
    ##   0.59850672  1915.526  0.01911437  1719.5141
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.05636109.

``` r
plot(treeFit)
```

![](Sunday_files/figure-gfm/Classification%20Tree-1.png)<!-- -->

``` r
finalTree<- treeFit$finalModel
```

The final model had a training RMSE of 1154.666638 on a tuning parameter
of cp=0.0563611. It has 5 branches and 3 termination points

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
    ## 73 samples
    ## 13 predictors
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 57, 59, 58, 60, 58, 59, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE      Rsquared   MAE     
    ##   1                   50      974.1172  0.7307504  810.3770
    ##   1                  100      950.9516  0.7437945  768.5700
    ##   1                  150      954.9313  0.7438702  769.7178
    ##   2                   50      971.7330  0.7304116  806.5684
    ##   2                  100      958.9084  0.7426080  780.1216
    ##   2                  150      948.2312  0.7447969  766.5504
    ##   3                   50      951.9226  0.7424777  793.3249
    ##   3                  100      938.0275  0.7504841  761.5493
    ##   3                  150      930.2446  0.7547405  749.7821
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.

``` r
#Graphic representation of the tuning parameters vs RMSE
plot(boostFit)
```

![](Sunday_files/figure-gfm/Boosted%20Results-1.png)<!-- -->

The best tuning parameters were:

  - n.trees: 150  
  - interaction.depth: 3  
  - shrinkage: .1 (default)  
  - n.minobsinnode: 10 (default)

The resulted in a training RMSE of 930.2445744

The significant variables in the final model were: season, yr, mnth,
holiday, weathersit, temp, atemp, hum, windspeed

## Test Fits, Sundays

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
The boosted tree RMSE was 891.44 while the decision tree had an RMSE of
1133.59. The decision tree had 127.2% of the error of the boosted tree.
