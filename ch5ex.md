Chapter 5 Exercises
================
rachel sabol
February 12, 2018

ISLR Chapter 5 Exercise 9

``` r
library(MASS)
library(boot)
```

1.  Provide an estimate of the population mean for medv.

``` r
mu=mean(Boston$medv)
mu
```

    ## [1] 22.53281

1.  Standard error of mu.

``` r
std_error=sd(Boston$medv)/sqrt(nrow(Boston))
std_error
```

    ## [1] 0.4088611

The error in the estimate of the mean of the median value of owner-occupied homes (medv) has a 95% change of being within 0.8177223 of the true measure of medv.

1.  Estimate std error using bootstrap.

``` r
set.seed(17)
bootstrap.fn=function(data, index){
  mu=mean(data[index])
  return (mu)
}
boot(Boston$medv, bootstrap.fn, 1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Boston$medv, statistic = bootstrap.fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##     original      bias    std. error
    ## t1* 22.53281 0.003069565   0.4025935

The standard error is very close to that calculated in part b, although slighly less.

1.  95% CI

``` r
interval=2*0.4025935
lower=mu-interval
upper=mu+interval
lower
```

    ## [1] 21.72762

``` r
upper
```

    ## [1] 23.33799

``` r
t.test(Boston$medv)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  Boston$medv
    ## t = 55.111, df = 505, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  21.72953 23.33608
    ## sample estimates:
    ## mean of x 
    ##  22.53281

The confidence intervals are approximately the same.

1.  

``` r
mumed=median(Boston$medv)
mumed
```

    ## [1] 21.2

1.  Standard error of the median

``` r
set.seed(17)
bootstrap.fn=function(data, index){
  mu=median(data[index])
  return (mu)
}
boot(Boston$medv, bootstrap.fn, 1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Boston$medv, statistic = bootstrap.fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##     original  bias    std. error
    ## t1*     21.2  0.0068   0.3769627

The median is the same as the computed value in e, and the standard error is relatively small.

g)Tenth percentila

``` r
mutenth=quantile(Boston$medv, c(0.1))
```

1.  Bootstrap mu tenth.

``` r
set.seed(17)
bootstrap.fn=function(data, index){
  mu=quantile(data[index], c(0.1))
  return (mu)
}
boot(Boston$medv, bootstrap.fn, 1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Boston$medv, statistic = bootstrap.fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##     original  bias    std. error
    ## t1*    12.75  0.0041   0.4870733

The quantile value is the same as that calculated before and the standard error is relatively small.
