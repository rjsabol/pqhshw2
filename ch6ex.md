ch6ex
================
rachel sabol
February 26, 2018

Exercise 9- College dataset

1.  Split the data set into a training set and a test set.

``` r
library(ISLR)
data(College)
set.seed(2)
train=sample(1:nrow(College), nrow(College)/2)
test=-train
train=College[train,]
test=College[test,]
dim(test);dim(train)
```

    ## [1] 389  18

    ## [1] 388  18

1.  Fit a linear model using least squares on the training set, and report the test error obtained.

``` r
lm.fit=lm(Apps~., data=train)
lm.pred=predict(lm.fit, test)
mean((lm.pred-test$Apps)^2)
```

    ## [1] 1475528

``` r
1-(mean((lm.pred-test$Apps)^2)/mean((mean(test$Apps)-test$Apps)^2))
```

    ## [1] 0.9164977

1.  Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report the test error obtained.

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loading required package: foreach

    ## Loaded glmnet 2.0-13

``` r
train.mat=model.matrix(Apps~.,data=train)
test.mat=model.matrix(Apps~., data=test)
grid=10^(seq(10,-2,length=100))
ridge.fit=glmnet(train.mat,train$Apps,alpha=0,lambda=grid,thresh=1e-12)
ridge.cv=cv.glmnet(train.mat, train$Apps, alpha=0,lambda=grid,thresh=1e-12)
lambduh=ridge.cv$lambda.min
lambduh
```

    ## [1] 24.77076

``` r
ridge.pred=predict(ridge.fit, s=lambduh, newx=test.mat)
mean((ridge.pred-test$Apps)^2)
```

    ## [1] 1590641

``` r
1-(mean((ridge.pred-test$Apps)^2)/mean((mean(test$Apps)-test$Apps)^2))
```

    ## [1] 0.9099833

1.  Fit a lasso model on the training set, with λ chosen by crossvalidation. Report the test error obtained, along with the number of non-zero coefficient estimates.

``` r
lasso.fit=glmnet(train.mat,train$Apps,alpha=1,lambda=grid,thresh=1e-12)
lasso.cv=cv.glmnet(train.mat, train$Apps, alpha=1,lambda=grid,thresh=1e-12)
lambduh=lasso.cv$lambda.min
lambduh
```

    ## [1] 4.641589

``` r
lasso.pred=predict(lasso.fit, s=lambduh, newx=test.mat)
mean((lasso.pred-test$Apps)^2)
```

    ## [1] 1509411

``` r
1-(mean((lasso.pred-test$Apps)^2)/mean((mean(test$Apps)-test$Apps)^2))
```

    ## [1] 0.9145802

``` r
predict(lasso.fit, s=lambduh, type="coefficients")
```

    ## 19 x 1 sparse Matrix of class "dgCMatrix"
    ##                         1
    ## (Intercept) -990.67148718
    ## (Intercept)    .         
    ## PrivateYes  -673.96710514
    ## Accept         1.21688863
    ## Enroll        -0.27584079
    ## Top10perc     48.93535863
    ## Top25perc    -14.01149475
    ## F.Undergrad    0.07718824
    ## P.Undergrad    0.01636539
    ## Outstate      -0.05777332
    ## Room.Board     0.24587601
    ## Books         -0.16675817
    ## Personal       0.05750227
    ## PhD           -7.17399933
    ## Terminal      -4.95533244
    ## S.F.Ratio     29.99239572
    ## perc.alumni   -0.08055273
    ## Expend         0.08593159
    ## Grad.Rate      8.92640141

1.  Fit a PCR model on the training set, with M chosen by crossvalidation. Report the test error obtained, along with the value of M selected by cross-validation.

``` r
library(pls)
```

    ## 
    ## Attaching package: 'pls'

    ## The following object is masked from 'package:stats':
    ## 
    ##     loadings

``` r
pcr.fit=pcr(Apps~.,data=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
```

![](ch6ex_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
summary(pcr.fit)
```

    ## Data:    X dimension: 388 17 
    ##  Y dimension: 388 1
    ## Fit method: svdpc
    ## Number of components considered: 17
    ## 
    ## VALIDATION: RMSEP
    ## Cross-validated using 10 random segments.
    ##        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
    ## CV            3508     3517     1696     1719     1306     1294     1256
    ## adjCV         3508     3517     1693     1719     1282     1279     1252
    ##        7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
    ## CV        1240     1214     1200      1178      1182      1187      1189
    ## adjCV     1230     1203     1197      1175      1180      1183      1185
    ##        14 comps  15 comps  16 comps  17 comps
    ## CV         1185      1201      1059      1042
    ## adjCV      1181      1196      1054      1037
    ## 
    ## TRAINING: % variance explained
    ##       1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps
    ## X     32.0511    57.98    65.49    71.19    76.33    81.22    84.72
    ## Apps   0.1403    77.35    77.35    87.55    87.66    88.04    88.71
    ##       8 comps  9 comps  10 comps  11 comps  12 comps  13 comps  14 comps
    ## X       88.00    91.06     93.47     95.34      97.2     98.21     98.98
    ## Apps    89.15    89.19     89.67     89.69      89.8     89.88     89.95
    ##       15 comps  16 comps  17 comps
    ## X        99.52     99.87    100.00
    ## Apps     89.96     92.23     92.61

``` r
#choose M=4 to explain 85%
```

``` r
pcr.pred=predict(pcr.fit, test, ncomp=4)
mean((pcr.pred-test$Apps)^2)
```

    ## [1] 3326333

``` r
1-(mean((pcr.pred-test$Apps)^2)/mean((mean(test$Apps)-test$Apps)^2))
```

    ## [1] 0.8117579

1.  Fit a PLS model on the training set, with M chosen by crossvalidation. Report the test error obtained, along with the value of M selected by cross-validation.

``` r
pls.fit=plsr(Apps~.,data=train,scale=TRUE, validation="CV")
validationplot(pls.fit,val.type="MSEP")
```

![](ch6ex_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
summary(pls.fit)
```

    ## Data:    X dimension: 388 17 
    ##  Y dimension: 388 1
    ## Fit method: kernelpls
    ## Number of components considered: 17
    ## 
    ## VALIDATION: RMSEP
    ## Cross-validated using 10 random segments.
    ##        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
    ## CV            3508     1519     1161     1152     1141     1159     1098
    ## adjCV         3508     1516     1146     1151     1134     1133     1088
    ##        7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
    ## CV        1079     1077     1067      1068      1070      1071      1070
    ## adjCV     1072     1070     1061      1061      1064      1064      1063
    ##        14 comps  15 comps  16 comps  17 comps
    ## CV         1071      1071      1071      1071
    ## adjCV      1064      1064      1064      1064
    ## 
    ## TRAINING: % variance explained
    ##       1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps
    ## X       25.81    32.40    63.00    66.40    68.58    73.09    77.67
    ## Apps    82.01    89.71    90.03    90.94    91.96    92.31    92.44
    ##       8 comps  9 comps  10 comps  11 comps  12 comps  13 comps  14 comps
    ## X       81.56    83.39     86.04     89.17      91.3     93.59     94.78
    ## Apps    92.49    92.53     92.56     92.58      92.6     92.61     92.61
    ##       15 comps  16 comps  17 comps
    ## X        97.58     98.19    100.00
    ## Apps     92.61     92.61     92.61

``` r
#choose m=2 to explain 85%
```

``` r
pls.pred=predict(pls.fit,test,ncomp=2)
mean((pls.pred-test$Apps)^2)
```

    ## [1] 2868968

``` r
1-(mean((pls.pred-test$Apps)^2)/mean((mean(test$Apps)-test$Apps)^2))
```

    ## [1] 0.8376408

1.  Comment on the results obtained. How accurately can we predict the number of college applications received? Is there much difference among the test errors resulting from these five approaches?

The test errors for each method are as follows: Linear regression- 1475528 Ridge regression- 1590641 Lasso-1557517 PCR- 3326333 PLS-2868968

The R squared value for each model are as follows: Linear regression- 0.9164977 Ridge regression- 0.9127352 Lasso- 0.9118578 PCR- 0.8117579 PLS- 0.8376408

Based on these metrics, the linear regression model performs the best. Both the ridge regression and the lasso perform similarly to the linear regression. The principal components regression and the partial least squares perform markedly worse.

Question 11- Boston data set

1.  Try out some of the regression methods explored in this chapter, such as best subset selection, the lasso, ridge regression, and PCR. Present and discuss results for the approaches that you consider.

``` r
library(MASS)
data(Boston)
train=sample(1:nrow(Boston), nrow(Boston)/2)
test=-train
train=Boston[train,]
test=Boston[test,]
dim(test);dim(train)
```

    ## [1] 253  14

    ## [1] 253  14

Linear model:

``` r
lm.fit=lm(crim~., data=train)
lm.pred=predict(lm.fit, test)
mean((lm.pred-test$crim)^2)
```

    ## [1] 35.28376

``` r
1-(mean((lm.pred-test$crim)^2)/mean((mean(test$crim)-test$crim)^2))
```

    ## [1] 0.4278745

Ridge regression

``` r
library(glmnet)
train.mat=model.matrix(crim~.,data=train)
test.mat=model.matrix(crim~., data=test)
grid=10^(seq(10,-2,length=100))
ridge.fit=glmnet(train.mat,train$crim,alpha=0,lambda=grid,thresh=1e-12)
ridge.cv=cv.glmnet(train.mat, train$crim, alpha=0,lambda=grid,thresh=1e-12)
lambduh=ridge.cv$lambda.min
lambduh
```

    ## [1] 2.009233

``` r
ridge.pred=predict(ridge.fit, s=lambduh, newx=test.mat)
coef(ridge.cv)
```

    ## 15 x 1 sparse Matrix of class "dgCMatrix"
    ##                         1
    ## (Intercept)  3.8626450593
    ## (Intercept)  .           
    ## zn          -0.0003501098
    ## indus        0.0022235663
    ## chas        -0.0108930969
    ## nox          0.1327520696
    ## rm          -0.0087316421
    ## age          0.0004516469
    ## dis         -0.0068461225
    ## rad          0.0026292430
    ## tax          0.0001316118
    ## ptratio      0.0047428817
    ## black       -0.0001729344
    ## lstat        0.0025306085
    ## medv        -0.0016554253

``` r
mean((ridge.pred-test$crim)^2)
```

    ## [1] 35.21763

``` r
1-(mean((ridge.pred-test$crim)^2)/mean((mean(test$crim)-test$crim)^2))
```

    ## [1] 0.4289469

Lasso

``` r
lasso.fit=glmnet(train.mat,train$crim,alpha=1,lambda=grid,thresh=1e-12)
lasso.cv=cv.glmnet(train.mat, train$crim, alpha=1,lambda=grid,thresh=1e-12)
lambduh=lasso.cv$lambda.min
lambduh
```

    ## [1] 0.09326033

``` r
lasso.pred=predict(lasso.fit, s=lambduh, newx=test.mat)
mean((lasso.pred-test$crim)^2)
```

    ## [1] 34.64221

``` r
1-(mean((lasso.pred-test$crim)^2)/mean((mean(test$crim)-test$crim)^2))
```

    ## [1] 0.4382773

``` r
predict(lasso.fit, s=lambduh, type="coefficients")
```

    ## 15 x 1 sparse Matrix of class "dgCMatrix"
    ##                         1
    ## (Intercept)  7.597991e+00
    ## (Intercept)  .           
    ## zn           3.061049e-02
    ## indus       -9.670317e-05
    ## chas        -1.331519e+00
    ## nox         -6.152229e+00
    ## rm           1.277748e+00
    ## age         -1.063552e-03
    ## dis         -6.819019e-01
    ## rad          4.801442e-01
    ## tax          .           
    ## ptratio     -1.579690e-01
    ## black       -1.405967e-02
    ## lstat        1.305081e-01
    ## medv        -1.928263e-01

PCR

``` r
library(pls)
pcr.fit=pcr(crim~.,data=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
```

![](ch6ex_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
summary(pcr.fit)
```

    ## Data:    X dimension: 253 13 
    ##  Y dimension: 253 1
    ## Fit method: svdpc
    ## Number of components considered: 13
    ## 
    ## VALIDATION: RMSEP
    ## Cross-validated using 10 random segments.
    ##        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
    ## CV           9.293    7.880    7.876    7.354    7.365    7.386    7.482
    ## adjCV        9.293    7.876    7.871    7.341    7.353    7.373    7.465
    ##        7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
    ## CV       7.525    7.487    7.512     7.514     7.448     7.434     7.405
    ## adjCV    7.505    7.465    7.489     7.494     7.424     7.407     7.377
    ## 
    ## TRAINING: % variance explained
    ##       1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps
    ## X       45.80    58.77    67.86    75.77    81.97    87.66    91.16
    ## crim    28.88    29.33    38.60    39.92    39.93    39.95    40.32
    ##       8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
    ## X       93.61    95.79     97.26     98.55     99.57    100.00
    ## crim    41.14    41.52     41.57     43.01     43.98     44.62

``` r
#choose M=6 to explain 85%
```

``` r
pcr.pred=predict(pcr.fit, test, ncomp=6)
mean((pcr.pred-test$crim)^2)
```

    ## [1] 38.59211

``` r
1-(mean((pcr.pred-test$crim)^2)/mean((mean(test$crim)-test$crim)^2))
```

    ## [1] 0.3742297

PLS

``` r
pls.fit=plsr(crim~.,data=train,scale=TRUE, validation="CV")
validationplot(pls.fit,val.type="MSEP")
```

![](ch6ex_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
summary(pls.fit)
```

    ## Data:    X dimension: 253 13 
    ##  Y dimension: 253 1
    ## Fit method: kernelpls
    ## Number of components considered: 13
    ## 
    ## VALIDATION: RMSEP
    ## Cross-validated using 10 random segments.
    ##        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
    ## CV           9.293    7.681    7.353    7.416    7.390    7.318    7.355
    ## adjCV        9.293    7.676    7.341    7.392    7.364    7.298    7.331
    ##        7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
    ## CV       7.362    7.338    7.343     7.342     7.343     7.342     7.342
    ## adjCV    7.336    7.315    7.319     7.318     7.319     7.318     7.318
    ## 
    ## TRAINING: % variance explained
    ##       1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps
    ## X       45.24    54.88    62.39    69.66    75.16    79.72    82.86
    ## crim    33.27    41.55    43.05    44.02    44.31    44.48    44.55
    ##       8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
    ## X       86.57    89.21     93.36     96.53     98.53    100.00
    ## crim    44.59    44.62     44.62     44.62     44.62     44.62

``` r
#choose m=8 to explain 85%
```

``` r
pls.pred=predict(pls.fit,test,ncomp=8)
mean((pls.pred-test$crim)^2)
```

    ## [1] 35.26203

``` r
1-(mean((pls.pred-test$crim)^2)/mean((mean(test$crim)-test$crim)^2))
```

    ## [1] 0.428227

The test error and R squared value for each model are as follows: Linear regression- 69.09398, 0.3184834 Ridge- 68.63729, 0.322988 Lasso- 68.78914, 0.3214902 PCR- 68.62744, 0.3230851 PLS- 69.10215, 0.3184028

1.  Propose a model (or set of models) that seem to perform well on this data set, and justify your answer. Make sure that you are evaluating model performance using validation set error, crossvalidation, or some other reasonable alternative, as opposed to using training error.

The model that performed the best using the validation set error was the ridge regression model. The test error was 68.63729 and the R squared value was 0.322988.

1.  Does your chosen model involve all of the features in the data set? Why or why not?

The model does include all features in the data, although some have very low coefficients. As opposed to the lasso model, ridge does not attempt to completey remove any predictive features from the model. However, about 8 of the variables had coefficients that were near 0 (&lt;0.1)
