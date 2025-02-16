Bank Marketing Case Study
================
Connor Allison
2025-02-16

Import libraries

``` r
knitr::opts_chunk$set(echo = TRUE)
library(caret)
```

    ## Loading required package: ggplot2

    ## Loading required package: lattice

``` r
library(lattice)
library(ggplot2)
library(logistf)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ purrr::lift()   masks caret::lift()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(olsrr)
```

    ## 
    ## Attaching package: 'olsrr'
    ## 
    ## The following object is masked from 'package:datasets':
    ## 
    ##     rivers

``` r
library(car)
```

    ## Loading required package: carData
    ## 
    ## Attaching package: 'car'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(ROCR)
library(MASS, include.only = c("lda"))
#or library(MASS, exclude = c("select"))
```

Read in the CSV

``` r
df = read.csv("bank-additional.csv", sep = ";")
str(df)
```

    ## 'data.frame':    4119 obs. of  21 variables:
    ##  $ age           : int  30 39 25 38 47 32 32 41 31 35 ...
    ##  $ job           : chr  "blue-collar" "services" "services" "services" ...
    ##  $ marital       : chr  "married" "single" "married" "married" ...
    ##  $ education     : chr  "basic.9y" "high.school" "high.school" "basic.9y" ...
    ##  $ default       : chr  "no" "no" "no" "no" ...
    ##  $ housing       : chr  "yes" "no" "yes" "unknown" ...
    ##  $ loan          : chr  "no" "no" "no" "unknown" ...
    ##  $ contact       : chr  "cellular" "telephone" "telephone" "telephone" ...
    ##  $ month         : chr  "may" "may" "jun" "jun" ...
    ##  $ day_of_week   : chr  "fri" "fri" "wed" "fri" ...
    ##  $ duration      : int  487 346 227 17 58 128 290 44 68 170 ...
    ##  $ campaign      : int  2 4 1 3 1 3 4 2 1 1 ...
    ##  $ pdays         : int  999 999 999 999 999 999 999 999 999 999 ...
    ##  $ previous      : int  0 0 0 0 0 2 0 0 1 0 ...
    ##  $ poutcome      : chr  "nonexistent" "nonexistent" "nonexistent" "nonexistent" ...
    ##  $ emp.var.rate  : num  -1.8 1.1 1.4 1.4 -0.1 -1.1 -1.1 -0.1 -0.1 1.1 ...
    ##  $ cons.price.idx: num  92.9 94 94.5 94.5 93.2 ...
    ##  $ cons.conf.idx : num  -46.2 -36.4 -41.8 -41.8 -42 -37.5 -37.5 -42 -42 -36.4 ...
    ##  $ euribor3m     : num  1.31 4.86 4.96 4.96 4.19 ...
    ##  $ nr.employed   : num  5099 5191 5228 5228 5196 ...
    ##  $ y             : chr  "no" "no" "no" "no" ...

Copy the dateset

``` r
df_train = df
```

Investigate and get rid of missing values

``` r
df_train = subset(df_train, !is.na(df_train$age))
df_train = subset(df_train, !is.na(df_train$campaign))
df_train = subset(df_train, !is.na(df_train$previous))

df_train = subset(df_train, !is.nan(df_train$job))
df_train = subset(df_train, !is.nan(df_train$marital))
df_train = subset(df_train, !is.nan(df_train$education))
df_train = subset(df_train, !is.nan(df_train$default))
df_train = subset(df_train, !is.nan(df_train$housing))
df_train = subset(df_train, !is.nan(df_train$loan))
df_train = subset(df_train, !is.nan(df_train$contact))
df_train = subset(df_train, !is.nan(df_train$month))
df_train = subset(df_train, !is.nan(df_train$day_of_week))
df_train = subset(df_train, !is.nan(df_train$poutcome))
```

Turns out there were no missing values

Investigate outcome variable to identify any missing values

``` r
levels(as.factor(df_train$y))
```

    ## [1] "no"  "yes"

Get rid of duration variable

``` r
df_train = df_train %>% 
  select(-duration)
```

Recode Pdays as three dummy variables

``` r
levels(as.factor(df_train$pdays))
```

    ##  [1] "0"   "1"   "2"   "3"   "4"   "5"   "6"   "7"   "9"   "10"  "11"  "12" 
    ## [13] "13"  "14"  "15"  "16"  "17"  "18"  "19"  "21"  "999"

``` r
df_train$daysdummy1 = ifelse(df_train$pdays %in% c(0:6), 1, 0)
df_train$daysdummy2 = ifelse(df_train$pdays %in% c(7:13), 1, 0)
df_train$daysdummy3 = ifelse(df_train$pdays %in% c(14:21), 1, 0)
```

Convert categorical data to factors

``` r
df_train$job = as.factor(df_train$job)
df_train$marital = as.factor(df_train$marital)
df_train$education = as.factor(df_train$education)
df_train$default = as.factor(df_train$default)
df_train$housing = as.factor(df_train$housing)
df_train$loan = as.factor(df_train$loan)
df_train$contact = as.factor(df_train$contact)
df_train$month = as.factor(df_train$month)
df_train$day_of_week = as.factor(df_train$day_of_week)
df_train$poutcome = as.factor(df_train$poutcome)
```

Recode the outcome variable as a new variable

``` r
df_train$y_train = ifelse(df_train$y == "yes", 1, 0)
```

Remove recoded variables from df_train

``` r
df_train = df_train %>% 
  select(-pdays, -y)
```

Now that we are ready to start creating models, we save our data as
df_sample. This will help prevent us from confusing our data frames
after spliting it into training and testing sets. Ideally, we would have
called it df_sample from the begining but sadly thats not how this one
played out. Lesson learned.

``` r
df_sample = df_train
```

Split the data into 70/30 training/testing split making sure to set a
seed.

``` r
set.seed(3)
index = sample(1:nrow(df_sample), 0.7 * nrow(df_sample))
df_train = df_sample[index, ]
df_test = df_sample[-index, ]
```

Create a glm model with all the predictors

``` r
m1 = glm(formula = y_train ~ ., data = df_train, family = binomial)
summary(m1)
```

    ## 
    ## Call:
    ## glm(formula = y_train ~ ., family = binomial, data = df_train)
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                                Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)                  -1.555e+02  1.327e+02  -1.172  0.24135   
    ## age                           1.058e-02  8.238e-03   1.285  0.19892   
    ## jobblue-collar               -1.749e-01  2.669e-01  -0.655  0.51235   
    ## jobentrepreneur              -6.520e-01  5.450e-01  -1.196  0.23157   
    ## jobhousemaid                  3.010e-02  4.410e-01   0.068  0.94558   
    ## jobmanagement                -1.788e-01  2.818e-01  -0.635  0.52564   
    ## jobretired                   -4.437e-01  3.815e-01  -1.163  0.24484   
    ## jobself-employed             -6.630e-01  4.599e-01  -1.442  0.14943   
    ## jobservices                  -1.946e-01  2.915e-01  -0.668  0.50442   
    ## jobstudent                   -5.799e-02  4.145e-01  -0.140  0.88872   
    ## jobtechnician                 8.936e-02  2.257e-01   0.396  0.69221   
    ## jobunemployed                 1.156e-01  4.106e-01   0.282  0.77832   
    ## jobunknown                   -7.506e-02  7.372e-01  -0.102  0.91891   
    ## maritalmarried               -3.397e-02  2.330e-01  -0.146  0.88409   
    ## maritalsingle                 9.281e-02  2.693e-01   0.345  0.73035   
    ## maritalunknown                5.821e-02  1.208e+00   0.048  0.96158   
    ## educationbasic.6y             3.827e-01  3.912e-01   0.978  0.32793   
    ## educationbasic.9y             1.500e-01  3.299e-01   0.455  0.64923   
    ## educationhigh.school          2.067e-01  3.152e-01   0.656  0.51196   
    ## educationilliterate          -1.206e+01  5.354e+02  -0.023  0.98203   
    ## educationprofessional.course  6.924e-02  3.462e-01   0.200  0.84147   
    ## educationuniversity.degree    3.289e-01  3.149e-01   1.044  0.29631   
    ## educationunknown             -3.069e-01  4.573e-01  -0.671  0.50213   
    ## defaultunknown                1.964e-01  2.038e-01   0.964  0.33530   
    ## defaultyes                   -1.023e+01  5.354e+02  -0.019  0.98476   
    ## housingunknown               -8.478e-01  6.415e-01  -1.322  0.18631   
    ## housingyes                    3.583e-02  1.405e-01   0.255  0.79867   
    ## loanunknown                          NA         NA      NA       NA   
    ## loanyes                      -2.178e-01  1.922e-01  -1.133  0.25708   
    ## contacttelephone             -8.176e-01  2.849e-01  -2.870  0.00410 **
    ## monthaug                     -5.257e-03  4.432e-01  -0.012  0.99054   
    ## monthdec                      4.777e-01  6.882e-01   0.694  0.48766   
    ## monthjul                     -5.256e-02  3.601e-01  -0.146  0.88395   
    ## monthjun                     -1.297e-02  4.691e-01  -0.028  0.97794   
    ## monthmar                      1.812e+00  5.538e-01   3.272  0.00107 **
    ## monthmay                     -4.138e-01  3.055e-01  -1.354  0.17564   
    ## monthnov                     -5.796e-01  4.244e-01  -1.365  0.17211   
    ## monthoct                     -4.199e-01  5.536e-01  -0.759  0.44811   
    ## monthsep                     -1.688e-01  6.364e-01  -0.265  0.79084   
    ## day_of_weekmon                7.232e-02  2.183e-01   0.331  0.74037   
    ## day_of_weekthu               -2.119e-02  2.250e-01  -0.094  0.92499   
    ## day_of_weektue               -7.110e-03  2.267e-01  -0.031  0.97498   
    ## day_of_weekwed                2.800e-01  2.247e-01   1.246  0.21274   
    ## campaign                     -8.711e-02  4.164e-02  -2.092  0.03645 * 
    ## previous                      1.095e-01  1.909e-01   0.574  0.56627   
    ## poutcomenonexistent           4.754e-01  3.171e-01   1.499  0.13381   
    ## poutcomesuccess               6.170e-02  8.424e-01   0.073  0.94161   
    ## emp.var.rate                 -8.972e-01  5.012e-01  -1.790  0.07345 . 
    ## cons.price.idx                1.511e+00  8.794e-01   1.718  0.08587 . 
    ## cons.conf.idx                 6.427e-02  2.828e-02   2.273  0.02301 * 
    ## euribor3m                    -9.217e-03  4.371e-01  -0.021  0.98317   
    ## nr.employed                   2.698e-03  1.062e-02   0.254  0.79946   
    ## daysdummy1                    1.683e+00  9.028e-01   1.864  0.06234 . 
    ## daysdummy2                    1.440e+00  8.233e-01   1.749  0.08034 . 
    ## daysdummy3                    5.155e-01  8.867e-01   0.581  0.56101   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2014.1  on 2882  degrees of freedom
    ## Residual deviance: 1556.8  on 2829  degrees of freedom
    ## AIC: 1664.8
    ## 
    ## Number of Fisher Scoring iterations: 12

Investigate the variables that could be an issue based on the results of
the model

``` r
length(which(df_train$loan == "unknown"))
```

    ## [1] 66

``` r
length(which(df_train$housing == "unknown"))
```

    ## [1] 66

``` r
length(which(df_train$default == "unknown"))
```

    ## [1] 543

Run a stepwise selection on the model

``` r
m2 = step(m1, direction = "both")
```

    ## Start:  AIC=1664.81
    ## y_train ~ age + job + marital + education + default + housing + 
    ##     loan + contact + month + day_of_week + campaign + previous + 
    ##     poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
    ##     euribor3m + nr.employed + daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - job            11   1563.3 1649.3
    ## - education       7   1561.4 1655.4
    ## - marital         3   1557.3 1659.3
    ## - day_of_week     4   1559.4 1659.4
    ## - default         2   1557.8 1661.8
    ## - euribor3m       1   1556.8 1662.8
    ## - nr.employed     1   1556.9 1662.9
    ## - housing         1   1556.9 1662.9
    ## - previous        1   1557.2 1663.2
    ## - daysdummy3      1   1557.2 1663.2
    ## - poutcome        2   1559.2 1663.2
    ## - loan            1   1558.1 1664.1
    ## - age             1   1558.5 1664.5
    ## <none>                1556.8 1664.8
    ## - cons.price.idx  1   1559.7 1665.7
    ## - emp.var.rate    1   1560.0 1666.0
    ## - daysdummy2      1   1560.0 1666.0
    ## - daysdummy1      1   1560.4 1666.4
    ## - campaign        1   1562.0 1668.0
    ## - cons.conf.idx   1   1562.1 1668.1
    ## - contact         1   1566.0 1672.0
    ## - month           9   1590.3 1680.3
    ## 
    ## Step:  AIC=1649.31
    ## y_train ~ age + marital + education + default + housing + loan + 
    ##     contact + month + day_of_week + campaign + previous + poutcome + 
    ##     emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + 
    ##     nr.employed + daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - education       7   1569.3 1641.3
    ## - day_of_week     4   1565.8 1643.8
    ## - marital         3   1563.9 1643.9
    ## - default         2   1564.3 1646.3
    ## - euribor3m       1   1563.3 1647.3
    ## - nr.employed     1   1563.4 1647.4
    ## - housing         1   1563.4 1647.4
    ## - previous        1   1563.6 1647.6
    ## - daysdummy3      1   1563.7 1647.7
    ## - poutcome        2   1565.8 1647.8
    ## - age             1   1564.2 1648.2
    ## - loan            1   1564.7 1648.7
    ## <none>                1563.3 1649.3
    ## - cons.price.idx  1   1566.1 1650.1
    ## - emp.var.rate    1   1566.3 1650.3
    ## - daysdummy2      1   1566.7 1650.7
    ## - daysdummy1      1   1567.0 1651.0
    ## - cons.conf.idx   1   1568.3 1652.3
    ## - campaign        1   1568.8 1652.8
    ## - contact         1   1571.7 1655.7
    ## + job            11   1556.8 1664.8
    ## - month           9   1597.9 1665.9
    ## 
    ## Step:  AIC=1641.28
    ## y_train ~ age + marital + default + housing + loan + contact + 
    ##     month + day_of_week + campaign + previous + poutcome + emp.var.rate + 
    ##     cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
    ##     daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - day_of_week     4   1571.6 1635.6
    ## - marital         3   1569.8 1635.8
    ## - default         2   1569.9 1637.9
    ## - nr.employed     1   1569.3 1639.3
    ## - euribor3m       1   1569.3 1639.3
    ## - housing         1   1569.4 1639.4
    ## - daysdummy3      1   1569.4 1639.4
    ## - previous        1   1569.6 1639.6
    ## - age             1   1569.8 1639.8
    ## - poutcome        2   1571.8 1639.8
    ## - loan            1   1570.7 1640.7
    ## <none>                1569.3 1641.3
    ## - cons.price.idx  1   1571.6 1641.6
    ## - emp.var.rate    1   1572.2 1642.2
    ## - daysdummy2      1   1572.6 1642.6
    ## - daysdummy1      1   1573.4 1643.4
    ## - cons.conf.idx   1   1573.5 1643.5
    ## - campaign        1   1574.5 1644.5
    ## - contact         1   1578.3 1648.3
    ## + education       7   1563.3 1649.3
    ## + job            11   1561.4 1655.4
    ## - month           9   1603.4 1657.4
    ## 
    ## Step:  AIC=1635.59
    ## y_train ~ age + marital + default + housing + loan + contact + 
    ##     month + campaign + previous + poutcome + emp.var.rate + cons.price.idx + 
    ##     cons.conf.idx + euribor3m + nr.employed + daysdummy1 + daysdummy2 + 
    ##     daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - marital         3   1572.2 1630.2
    ## - default         2   1572.1 1632.1
    ## - nr.employed     1   1571.6 1633.6
    ## - euribor3m       1   1571.7 1633.7
    ## - housing         1   1571.7 1633.7
    ## - daysdummy3      1   1571.7 1633.7
    ## - previous        1   1572.0 1634.0
    ## - age             1   1572.0 1634.0
    ## - poutcome        2   1574.2 1634.2
    ## - loan            1   1572.8 1634.8
    ## <none>                1571.6 1635.6
    ## - cons.price.idx  1   1573.7 1635.7
    ## - emp.var.rate    1   1574.3 1636.3
    ## - daysdummy2      1   1574.8 1636.8
    ## - daysdummy1      1   1575.7 1637.7
    ## - cons.conf.idx   1   1575.7 1637.7
    ## - campaign        1   1577.0 1639.0
    ## + day_of_week     4   1569.3 1641.3
    ## - contact         1   1580.3 1642.3
    ## + education       7   1565.8 1643.8
    ## + job            11   1563.9 1649.9
    ## - month           9   1605.0 1651.0
    ## 
    ## Step:  AIC=1630.18
    ## y_train ~ age + default + housing + loan + contact + month + 
    ##     campaign + previous + poutcome + emp.var.rate + cons.price.idx + 
    ##     cons.conf.idx + euribor3m + nr.employed + daysdummy1 + daysdummy2 + 
    ##     daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - default         2   1572.7 1626.7
    ## - nr.employed     1   1572.2 1628.2
    ## - euribor3m       1   1572.2 1628.2
    ## - housing         1   1572.3 1628.3
    ## - age             1   1572.3 1628.3
    ## - daysdummy3      1   1572.3 1628.3
    ## - previous        1   1572.7 1628.7
    ## - poutcome        2   1574.9 1628.9
    ## - loan            1   1573.4 1629.4
    ## <none>                1572.2 1630.2
    ## - cons.price.idx  1   1574.3 1630.3
    ## - emp.var.rate    1   1574.9 1630.9
    ## - daysdummy2      1   1575.3 1631.3
    ## - daysdummy1      1   1576.3 1632.3
    ## - cons.conf.idx   1   1576.3 1632.3
    ## - campaign        1   1577.6 1633.6
    ## + marital         3   1571.6 1635.6
    ## + day_of_week     4   1569.8 1635.8
    ## - contact         1   1580.8 1636.8
    ## + education       7   1566.5 1638.5
    ## + job            11   1564.5 1644.5
    ## - month           9   1606.9 1646.9
    ## 
    ## Step:  AIC=1626.7
    ## y_train ~ age + housing + loan + contact + month + campaign + 
    ##     previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
    ##     euribor3m + nr.employed + daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - nr.employed     1   1572.7 1624.7
    ## - euribor3m       1   1572.8 1624.8
    ## - housing         1   1572.8 1624.8
    ## - daysdummy3      1   1572.9 1624.9
    ## - age             1   1572.9 1624.9
    ## - previous        1   1573.2 1625.2
    ## - poutcome        2   1575.4 1625.4
    ## - loan            1   1574.0 1626.0
    ## <none>                1572.7 1626.7
    ## - cons.price.idx  1   1574.8 1626.8
    ## - emp.var.rate    1   1575.4 1627.4
    ## - daysdummy2      1   1575.9 1627.9
    ## - daysdummy1      1   1576.8 1628.8
    ## - cons.conf.idx   1   1576.8 1628.8
    ## - campaign        1   1578.1 1630.1
    ## + default         2   1572.2 1630.2
    ## + marital         3   1572.1 1632.1
    ## + day_of_week     4   1570.4 1632.4
    ## - contact         1   1581.3 1633.3
    ## + education       7   1567.4 1635.4
    ## + job            11   1565.2 1641.2
    ## - month           9   1607.1 1643.1
    ## 
    ## Step:  AIC=1624.71
    ## y_train ~ age + housing + loan + contact + month + campaign + 
    ##     previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
    ##     euribor3m + daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - euribor3m       1   1572.8 1622.8
    ## - housing         1   1572.8 1622.8
    ## - daysdummy3      1   1572.9 1622.9
    ## - age             1   1572.9 1622.9
    ## - previous        1   1573.2 1623.2
    ## - poutcome        2   1575.4 1623.4
    ## - loan            1   1574.0 1624.0
    ## <none>                1572.7 1624.7
    ## - daysdummy2      1   1575.9 1625.9
    ## + nr.employed     1   1572.7 1626.7
    ## - daysdummy1      1   1576.8 1626.8
    ## - emp.var.rate    1   1577.3 1627.3
    ## - campaign        1   1578.2 1628.2
    ## + default         2   1572.2 1628.2
    ## + marital         3   1572.1 1630.1
    ## + day_of_week     4   1570.4 1630.4
    ## - cons.conf.idx   1   1580.5 1630.5
    ## - contact         1   1582.2 1632.2
    ## + education       7   1567.4 1633.4
    ## - cons.price.idx  1   1585.9 1635.9
    ## + job            11   1565.2 1639.2
    ## - month           9   1609.2 1643.2
    ## 
    ## Step:  AIC=1622.8
    ## y_train ~ age + housing + loan + contact + month + campaign + 
    ##     previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
    ##     daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - housing         1   1572.9 1620.9
    ## - daysdummy3      1   1573.0 1621.0
    ## - age             1   1573.0 1621.0
    ## - previous        1   1573.3 1621.3
    ## - poutcome        2   1575.6 1621.6
    ## - loan            1   1574.0 1622.0
    ## <none>                1572.8 1622.8
    ## - daysdummy2      1   1576.0 1624.0
    ## + euribor3m       1   1572.7 1624.7
    ## + nr.employed     1   1572.8 1624.8
    ## - daysdummy1      1   1576.9 1624.9
    ## + default         2   1572.3 1626.3
    ## - campaign        1   1578.3 1626.3
    ## + marital         3   1572.2 1628.2
    ## + day_of_week     4   1570.5 1628.5
    ## - contact         1   1582.4 1630.4
    ## + education       7   1567.4 1631.4
    ## - cons.conf.idx   1   1583.4 1631.4
    ## + job            11   1565.3 1637.3
    ## - month           9   1609.4 1641.4
    ## - cons.price.idx  1   1607.1 1655.1
    ## - emp.var.rate    1   1653.2 1701.2
    ## 
    ## Step:  AIC=1620.91
    ## y_train ~ age + loan + contact + month + campaign + previous + 
    ##     poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
    ##     daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - daysdummy3      1   1573.1 1619.1
    ## - age             1   1573.2 1619.2
    ## - previous        1   1573.4 1619.4
    ## - poutcome        2   1575.8 1619.8
    ## - loan            2   1576.5 1620.5
    ## <none>                1572.9 1620.9
    ## - daysdummy2      1   1576.0 1622.0
    ## + housing         1   1572.8 1622.8
    ## + euribor3m       1   1572.8 1622.8
    ## + nr.employed     1   1572.9 1622.9
    ## - daysdummy1      1   1577.0 1623.0
    ## + default         2   1572.4 1624.4
    ## - campaign        1   1578.4 1624.4
    ## + marital         3   1572.4 1626.4
    ## + day_of_week     4   1570.7 1626.7
    ## - contact         1   1582.5 1628.5
    ## - cons.conf.idx   1   1583.5 1629.5
    ## + education       7   1567.5 1629.5
    ## + job            11   1565.3 1635.3
    ## - month           9   1609.6 1639.6
    ## - cons.price.idx  1   1607.1 1653.1
    ## - emp.var.rate    1   1653.3 1699.3
    ## 
    ## Step:  AIC=1619.08
    ## y_train ~ age + loan + contact + month + campaign + previous + 
    ##     poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
    ##     daysdummy1 + daysdummy2
    ## 
    ##                  Df Deviance    AIC
    ## - age             1   1573.3 1617.3
    ## - previous        1   1573.8 1617.8
    ## - poutcome        2   1576.0 1618.0
    ## - loan            2   1576.8 1618.8
    ## <none>                1573.1 1619.1
    ## - daysdummy2      1   1576.1 1620.1
    ## + daysdummy3      1   1572.9 1620.9
    ## + housing         1   1573.0 1621.0
    ## + euribor3m       1   1573.0 1621.0
    ## + nr.employed     1   1573.0 1621.0
    ## - daysdummy1      1   1577.3 1621.3
    ## + default         2   1572.5 1622.5
    ## - campaign        1   1578.6 1622.6
    ## + marital         3   1572.5 1624.5
    ## + day_of_week     4   1570.8 1624.8
    ## - contact         1   1582.6 1626.6
    ## - cons.conf.idx   1   1583.6 1627.6
    ## + education       7   1567.9 1627.9
    ## + job            11   1565.5 1633.5
    ## - month           9   1610.0 1638.0
    ## - cons.price.idx  1   1607.7 1651.7
    ## - emp.var.rate    1   1653.9 1697.9
    ## 
    ## Step:  AIC=1617.31
    ## y_train ~ loan + contact + month + campaign + previous + poutcome + 
    ##     emp.var.rate + cons.price.idx + cons.conf.idx + daysdummy1 + 
    ##     daysdummy2
    ## 
    ##                  Df Deviance    AIC
    ## - previous        1   1574.1 1616.1
    ## - poutcome        2   1576.3 1616.3
    ## - loan            2   1577.0 1617.0
    ## <none>                1573.3 1617.3
    ## - daysdummy2      1   1576.3 1618.3
    ## + age             1   1573.1 1619.1
    ## + daysdummy3      1   1573.2 1619.2
    ## + euribor3m       1   1573.2 1619.2
    ## + housing         1   1573.2 1619.2
    ## + nr.employed     1   1573.3 1619.3
    ## - daysdummy1      1   1577.5 1619.5
    ## + default         2   1572.7 1620.7
    ## - campaign        1   1578.8 1620.8
    ## + marital         3   1573.1 1623.1
    ## + day_of_week     4   1571.1 1623.1
    ## - contact         1   1582.8 1624.8
    ## - cons.conf.idx   1   1584.0 1626.0
    ## + education       7   1568.5 1626.5
    ## + job            11   1566.6 1632.6
    ## - month           9   1610.5 1636.5
    ## - cons.price.idx  1   1608.3 1650.3
    ## - emp.var.rate    1   1654.8 1696.8
    ## 
    ## Step:  AIC=1616.1
    ## y_train ~ loan + contact + month + campaign + poutcome + emp.var.rate + 
    ##     cons.price.idx + cons.conf.idx + daysdummy1 + daysdummy2
    ## 
    ##                  Df Deviance    AIC
    ## - poutcome        2   1576.5 1614.5
    ## - loan            2   1577.5 1615.5
    ## <none>                1574.1 1616.1
    ## + previous        1   1573.3 1617.3
    ## + daysdummy3      1   1573.7 1617.7
    ## + age             1   1573.8 1617.8
    ## + housing         1   1574.0 1618.0
    ## + euribor3m       1   1574.0 1618.0
    ## + nr.employed     1   1574.0 1618.0
    ## - daysdummy2      1   1578.3 1618.3
    ## + default         2   1573.4 1619.4
    ## - campaign        1   1579.6 1619.6
    ## - daysdummy1      1   1579.9 1619.9
    ## + day_of_week     4   1571.8 1621.8
    ## + marital         3   1573.8 1621.8
    ## - contact         1   1584.2 1624.2
    ## - cons.conf.idx   1   1584.8 1624.8
    ## + education       7   1569.3 1625.3
    ## + job            11   1567.4 1631.4
    ## - month           9   1611.2 1635.2
    ## - cons.price.idx  1   1617.3 1657.3
    ## - emp.var.rate    1   1664.1 1704.1
    ## 
    ## Step:  AIC=1614.55
    ## y_train ~ loan + contact + month + campaign + emp.var.rate + 
    ##     cons.price.idx + cons.conf.idx + daysdummy1 + daysdummy2
    ## 
    ##                  Df Deviance    AIC
    ## - loan            2   1580.3 1614.3
    ## <none>                1576.5 1614.5
    ## + poutcome        2   1574.1 1616.1
    ## + age             1   1576.2 1616.2
    ## + previous        1   1576.3 1616.3
    ## + euribor3m       1   1576.3 1616.3
    ## + daysdummy3      1   1576.4 1616.4
    ## + housing         1   1576.4 1616.4
    ## + nr.employed     1   1576.5 1616.5
    ## - campaign        1   1581.9 1617.9
    ## + default         2   1575.9 1617.9
    ## - daysdummy2      1   1582.5 1618.5
    ## + marital         3   1576.3 1620.3
    ## + day_of_week     4   1574.4 1620.4
    ## - contact         1   1585.3 1621.3
    ## - cons.conf.idx   1   1586.8 1622.8
    ## + education       7   1571.8 1623.8
    ## + job            11   1569.6 1629.6
    ## - month           9   1614.4 1634.4
    ## - daysdummy1      1   1609.6 1645.6
    ## - cons.price.idx  1   1617.6 1653.6
    ## - emp.var.rate    1   1666.1 1702.1
    ## 
    ## Step:  AIC=1614.32
    ## y_train ~ contact + month + campaign + emp.var.rate + cons.price.idx + 
    ##     cons.conf.idx + daysdummy1 + daysdummy2
    ## 
    ##                  Df Deviance    AIC
    ## <none>                1580.3 1614.3
    ## + loan            2   1576.5 1614.5
    ## + housing         2   1577.5 1615.5
    ## + poutcome        2   1577.5 1615.5
    ## + previous        1   1579.8 1615.8
    ## + age             1   1580.0 1616.0
    ## + euribor3m       1   1580.0 1616.0
    ## + daysdummy3      1   1580.2 1616.2
    ## + nr.employed     1   1580.2 1616.2
    ## - campaign        1   1585.5 1617.5
    ## + default         2   1579.7 1617.7
    ## - daysdummy2      1   1586.3 1618.3
    ## + marital         3   1580.0 1620.0
    ## + day_of_week     4   1578.2 1620.2
    ## - contact         1   1589.0 1621.0
    ## - cons.conf.idx   1   1590.2 1622.2
    ## + education       7   1575.8 1623.8
    ## + job            11   1572.9 1628.9
    ## - month           9   1617.8 1633.8
    ## - daysdummy1      1   1613.7 1645.7
    ## - cons.price.idx  1   1621.2 1653.2
    ## - emp.var.rate    1   1669.7 1701.7

``` r
summary(m2)
```

    ## 
    ## Call:
    ## glm(formula = y_train ~ contact + month + campaign + emp.var.rate + 
    ##     cons.price.idx + cons.conf.idx + daysdummy1 + daysdummy2, 
    ##     family = binomial, data = df_train)
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -110.11266   16.58613  -6.639 3.16e-11 ***
    ## contacttelephone   -0.70453    0.24874  -2.832  0.00462 ** 
    ## monthaug            0.10710    0.37154   0.288  0.77314    
    ## monthdec            0.48773    0.61471   0.793  0.42753    
    ## monthjul            0.08715    0.34269   0.254  0.79927    
    ## monthjun            0.21127    0.31765   0.665  0.50598    
    ## monthmar            1.77601    0.45090   3.939 8.19e-05 ***
    ## monthmay           -0.38729    0.27657  -1.400  0.16141    
    ## monthnov           -0.47886    0.33475  -1.431  0.15257    
    ## monthoct           -0.33539    0.45595  -0.736  0.46199    
    ## monthsep           -0.10843    0.45988  -0.236  0.81361    
    ## campaign           -0.08585    0.04127  -2.080  0.03751 *  
    ## emp.var.rate       -0.69292    0.07006  -9.890  < 2e-16 ***
    ## cons.price.idx      1.18070    0.17994   6.562 5.32e-11 ***
    ## cons.conf.idx       0.05644    0.01813   3.112  0.00186 ** 
    ## daysdummy1          1.58008    0.28081   5.627 1.84e-08 ***
    ## daysdummy2          1.22216    0.51040   2.394  0.01664 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2014.1  on 2882  degrees of freedom
    ## Residual deviance: 1580.3  on 2866  degrees of freedom
    ## AIC: 1614.3
    ## 
    ## Number of Fisher Scoring iterations: 6

Check for muticollinearity

``` r
vif(m2)
```

    ##                    GVIF Df GVIF^(1/(2*Df))
    ## contact        2.390730  1        1.546198
    ## month          5.560454  9        1.100006
    ## campaign       1.047248  1        1.023351
    ## emp.var.rate   3.289158  1        1.813604
    ## cons.price.idx 3.239028  1        1.799730
    ## cons.conf.idx  2.382295  1        1.543469
    ## daysdummy1     1.159846  1        1.076961
    ## daysdummy2     1.053397  1        1.026351

Create a confusion matrix to evaluate the predictive capabilities of our
models

``` r
df_test$PredProbA = predict.glm(m1, newdata = df_test, type = "response")
df_test$PredSurA = ifelse(df_test$PredProbA >= 0.09, 1, 0)
caret::confusionMatrix(as.factor(df_test$PredSurA), as.factor(df_test$y_train), positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 876  43
    ##          1 230  87
    ##                                          
    ##                Accuracy : 0.7791         
    ##                  95% CI : (0.7549, 0.802)
    ##     No Information Rate : 0.8948         
    ##     P-Value [Acc > NIR] : 1              
    ##                                          
    ##                   Kappa : 0.2822         
    ##                                          
    ##  Mcnemar's Test P-Value : <2e-16         
    ##                                          
    ##             Sensitivity : 0.66923        
    ##             Specificity : 0.79204        
    ##          Pos Pred Value : 0.27445        
    ##          Neg Pred Value : 0.95321        
    ##              Prevalence : 0.10518        
    ##          Detection Rate : 0.07039        
    ##    Detection Prevalence : 0.25647        
    ##       Balanced Accuracy : 0.73064        
    ##                                          
    ##        'Positive' Class : 1              
    ## 

Confusion matrix for stepwise version

``` r
df_test$PredProbB = predict(m2, newdata = df_test, type = "response")
df_test$PredSurB = ifelse(df_test$PredProbB >= 0.07684, 1, 0)
caret::confusionMatrix(as.factor(df_test$PredSurB), as.factor(df_test$y_train), positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 829  39
    ##          1 277  91
    ##                                          
    ##                Accuracy : 0.7443         
    ##                  95% CI : (0.719, 0.7685)
    ##     No Information Rate : 0.8948         
    ##     P-Value [Acc > NIR] : 1              
    ##                                          
    ##                   Kappa : 0.2487         
    ##                                          
    ##  Mcnemar's Test P-Value : <2e-16         
    ##                                          
    ##             Sensitivity : 0.70000        
    ##             Specificity : 0.74955        
    ##          Pos Pred Value : 0.24728        
    ##          Neg Pred Value : 0.95507        
    ##              Prevalence : 0.10518        
    ##          Detection Rate : 0.07362        
    ##    Detection Prevalence : 0.29773        
    ##       Balanced Accuracy : 0.72477        
    ##                                          
    ##        'Positive' Class : 1              
    ## 

Check optimal threshold to find the best cut-off

``` r
pred = prediction(df_test$PredProbB, df_test$y_train)
auc = round(as.numeric(performance(pred, measure = "auc")@y.values),5)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize = T, main = "ROC Curve")
text(0.5, 0.5, paste("AUC:", auc))
```

![](BankMarketingCaseStudy_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
plot(unlist(performance(pred, "sens")@x.values), unlist(performance(pred, "sens")@y.values), 
     type="l", lwd=2, 
     ylab="Sensitivity", xlab="Cutoff", main = paste("Maximized Cutoff\n","AUC: ",auc))

par(new=TRUE)

plot(unlist(performance(pred, "spec")@x.values), unlist(performance(pred, "spec")@y.values), 
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.2)) 

mtext("Specificity",side=4, col='red')
min.diff <-which.min(abs(unlist(performance(pred, "sens")@y.values) - unlist(performance(pred, "spec")@y.values)))
min.x<-unlist(performance(pred, "sens")@x.values)[min.diff]
min.y<-unlist(performance(pred, "spec")@y.values)[min.diff]
optimal <-min.x 

abline(h = min.y, lty = 3)
abline(v = min.x, lty = 3)
text(min.x,0,paste("optimal threshold=",round(optimal,5)), pos = 3)
```

![](BankMarketingCaseStudy_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->

Redo same processes for data set without loan = “unknown” observations

``` r
df_sample2 = subset(df_sample, !df_sample$loan == "unknown")
```

``` r
set.seed(3)
index = sample(1:nrow(df_sample2), 0.7 * nrow(df_sample2))
df_train2 = df_sample2[index, ]
df_test2 = df_sample2[-index, ]
```

``` r
m3 = glm(formula = y_train ~ ., data = df_train2, family = binomial)
summary(m3)
```

    ## 
    ## Call:
    ## glm(formula = y_train ~ ., family = binomial, data = df_train2)
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  -2.666e+02  1.325e+02  -2.012 0.044251 *  
    ## age                           2.941e-02  8.531e-03   3.448 0.000566 ***
    ## jobblue-collar               -1.146e-01  2.777e-01  -0.413 0.679757    
    ## jobentrepreneur              -1.032e+00  5.586e-01  -1.848 0.064653 .  
    ## jobhousemaid                 -1.664e-01  4.953e-01  -0.336 0.736873    
    ## jobmanagement                -5.572e-01  3.117e-01  -1.787 0.073861 .  
    ## jobretired                   -6.127e-01  3.698e-01  -1.657 0.097575 .  
    ## jobself-employed             -1.151e-01  3.907e-01  -0.295 0.768299    
    ## jobservices                  -3.032e-01  3.171e-01  -0.956 0.338963    
    ## jobstudent                    1.506e-01  4.208e-01   0.358 0.720439    
    ## jobtechnician                -1.733e-01  2.414e-01  -0.718 0.472904    
    ## jobunemployed                 5.730e-02  4.293e-01   0.133 0.893823    
    ## jobunknown                    1.119e-01  6.697e-01   0.167 0.867326    
    ## maritalmarried                2.948e-01  2.532e-01   1.164 0.244236    
    ## maritalsingle                 4.503e-01  2.888e-01   1.559 0.119021    
    ## maritalunknown                4.642e-01  1.263e+00   0.367 0.713280    
    ## educationbasic.6y             5.602e-01  4.357e-01   1.286 0.198540    
    ## educationbasic.9y             3.724e-01  3.411e-01   1.092 0.275021    
    ## educationhigh.school          2.279e-01  3.361e-01   0.678 0.497690    
    ## educationilliterate          -1.126e+01  5.354e+02  -0.021 0.983228    
    ## educationprofessional.course  5.552e-01  3.561e-01   1.559 0.118953    
    ## educationuniversity.degree    4.184e-01  3.339e-01   1.253 0.210269    
    ## educationunknown              7.054e-01  4.094e-01   1.723 0.084838 .  
    ## defaultunknown               -1.731e-01  2.290e-01  -0.756 0.449686    
    ## defaultyes                   -1.002e+01  5.354e+02  -0.019 0.985068    
    ## housingyes                   -2.042e-01  1.451e-01  -1.408 0.159272    
    ## loanyes                      -5.446e-02  1.939e-01  -0.281 0.778819    
    ## contacttelephone             -1.282e+00  3.010e-01  -4.260 2.05e-05 ***
    ## monthaug                      3.601e-01  4.420e-01   0.815 0.415213    
    ## monthdec                      8.271e-01  6.661e-01   1.242 0.214347    
    ## monthjul                     -1.270e-01  3.716e-01  -0.342 0.732465    
    ## monthjun                     -2.184e-01  4.602e-01  -0.475 0.635119    
    ## monthmar                      2.146e+00  5.566e-01   3.855 0.000116 ***
    ## monthmay                     -2.600e-01  3.116e-01  -0.834 0.404048    
    ## monthnov                     -6.953e-01  4.407e-01  -1.578 0.114643    
    ## monthoct                      1.761e-01  5.517e-01   0.319 0.749641    
    ## monthsep                      2.143e-02  6.422e-01   0.033 0.973377    
    ## day_of_weekmon               -4.347e-02  2.289e-01  -0.190 0.849397    
    ## day_of_weekthu                1.373e-01  2.264e-01   0.606 0.544316    
    ## day_of_weektue               -2.023e-01  2.407e-01  -0.841 0.400626    
    ## day_of_weekwed                8.165e-02  2.401e-01   0.340 0.733819    
    ## campaign                     -6.918e-02  4.223e-02  -1.638 0.101411    
    ## previous                     -5.523e-02  1.860e-01  -0.297 0.766587    
    ## poutcomenonexistent           1.906e-01  3.172e-01   0.601 0.547928    
    ## poutcomesuccess               1.150e+00  8.411e-01   1.368 0.171419    
    ## emp.var.rate                 -1.553e+00  4.958e-01  -3.132 0.001734 ** 
    ## cons.price.idx                2.389e+00  8.756e-01   2.728 0.006372 ** 
    ## cons.conf.idx                 3.748e-02  2.843e-02   1.318 0.187522    
    ## euribor3m                     2.318e-01  4.527e-01   0.512 0.608530    
    ## nr.employed                   7.797e-03  1.070e-02   0.728 0.466371    
    ## daysdummy1                    5.961e-01  8.854e-01   0.673 0.500771    
    ## daysdummy2                    2.282e-01  8.225e-01   0.277 0.781482    
    ## daysdummy3                   -1.171e+00  1.413e+00  -0.829 0.407226    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1917.3  on 2808  degrees of freedom
    ## Residual deviance: 1434.5  on 2756  degrees of freedom
    ## AIC: 1540.5
    ## 
    ## Number of Fisher Scoring iterations: 12

``` r
m4 = step(m3, direction = "both")
```

    ## Start:  AIC=1540.51
    ## y_train ~ age + job + marital + education + default + housing + 
    ##     loan + contact + month + day_of_week + campaign + previous + 
    ##     poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
    ##     euribor3m + nr.employed + daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - job            11   1444.1 1528.1
    ## - education       7   1439.9 1531.9
    ## - day_of_week     4   1437.2 1535.2
    ## - marital         3   1437.0 1537.0
    ## - default         2   1435.2 1537.2
    ## - daysdummy2      1   1434.6 1538.6
    ## - loan            1   1434.6 1538.6
    ## - previous        1   1434.6 1538.6
    ## - poutcome        2   1436.7 1538.7
    ## - euribor3m       1   1434.8 1538.8
    ## - daysdummy1      1   1435.0 1539.0
    ## - nr.employed     1   1435.0 1539.0
    ## - daysdummy3      1   1435.3 1539.3
    ## - cons.conf.idx   1   1436.3 1540.3
    ## - housing         1   1436.5 1540.5
    ## <none>                1434.5 1540.5
    ## - campaign        1   1437.7 1541.7
    ## - cons.price.idx  1   1442.0 1546.0
    ## - emp.var.rate    1   1444.2 1548.2
    ## - age             1   1446.4 1550.4
    ## - contact         1   1456.0 1560.0
    ## - month           9   1473.7 1561.7
    ## 
    ## Step:  AIC=1528.14
    ## y_train ~ age + marital + education + default + housing + loan + 
    ##     contact + month + day_of_week + campaign + previous + poutcome + 
    ##     emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + 
    ##     nr.employed + daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - education       7   1450.0 1520.0
    ## - day_of_week     4   1446.5 1522.5
    ## - default         2   1444.7 1524.7
    ## - marital         3   1447.6 1525.6
    ## - loan            1   1444.2 1526.2
    ## - daysdummy2      1   1444.3 1526.3
    ## - poutcome        2   1446.3 1526.3
    ## - previous        1   1444.3 1526.3
    ## - nr.employed     1   1444.5 1526.5
    ## - euribor3m       1   1444.6 1526.6
    ## - daysdummy1      1   1444.6 1526.6
    ## - daysdummy3      1   1444.8 1526.8
    ## - cons.conf.idx   1   1445.6 1527.6
    ## <none>                1444.1 1528.1
    ## - housing         1   1446.3 1528.3
    ## - campaign        1   1447.3 1529.3
    ## - cons.price.idx  1   1451.3 1533.3
    ## - emp.var.rate    1   1454.0 1536.0
    ## - age             1   1454.3 1536.3
    ## + job            11   1434.5 1540.5
    ## - contact         1   1463.7 1545.7
    ## - month           9   1484.1 1550.1
    ## 
    ## Step:  AIC=1520.05
    ## y_train ~ age + marital + default + housing + loan + contact + 
    ##     month + day_of_week + campaign + previous + poutcome + emp.var.rate + 
    ##     cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
    ##     daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - day_of_week     4   1452.5 1514.5
    ## - default         2   1450.9 1516.9
    ## - marital         3   1453.8 1517.8
    ## - loan            1   1450.1 1518.1
    ## - previous        1   1450.2 1518.2
    ## - daysdummy2      1   1450.2 1518.2
    ## - poutcome        2   1452.3 1518.3
    ## - nr.employed     1   1450.4 1518.4
    ## - daysdummy1      1   1450.5 1518.5
    ## - euribor3m       1   1450.6 1518.6
    ## - daysdummy3      1   1450.7 1518.7
    ## - cons.conf.idx   1   1451.6 1519.6
    ## - housing         1   1451.8 1519.8
    ## <none>                1450.0 1520.0
    ## - campaign        1   1453.2 1521.2
    ## - cons.price.idx  1   1457.6 1525.6
    ## - age             1   1459.5 1527.5
    ## + education       7   1444.1 1528.1
    ## - emp.var.rate    1   1460.5 1528.5
    ## + job            11   1439.9 1531.9
    ## - contact         1   1470.0 1538.0
    ## - month           9   1490.9 1542.9
    ## 
    ## Step:  AIC=1514.5
    ## y_train ~ age + marital + default + housing + loan + contact + 
    ##     month + campaign + previous + poutcome + emp.var.rate + cons.price.idx + 
    ##     cons.conf.idx + euribor3m + nr.employed + daysdummy1 + daysdummy2 + 
    ##     daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - default         2   1453.3 1511.3
    ## - marital         3   1456.4 1512.4
    ## - loan            1   1452.5 1512.5
    ## - daysdummy2      1   1452.6 1512.6
    ## - previous        1   1452.6 1512.6
    ## - poutcome        2   1454.8 1512.8
    ## - daysdummy1      1   1452.9 1512.9
    ## - euribor3m       1   1452.9 1512.9
    ## - nr.employed     1   1453.0 1513.0
    ## - daysdummy3      1   1453.0 1513.0
    ## - housing         1   1454.3 1514.3
    ## - cons.conf.idx   1   1454.3 1514.3
    ## <none>                1452.5 1514.5
    ## - campaign        1   1455.8 1515.8
    ## + day_of_week     4   1450.0 1520.0
    ## - cons.price.idx  1   1460.5 1520.5
    ## - age             1   1462.0 1522.0
    ## + education       7   1446.5 1522.5
    ## - emp.var.rate    1   1463.1 1523.1
    ## + job            11   1442.6 1526.6
    ## - contact         1   1473.1 1533.1
    ## - month           9   1492.7 1536.7
    ## 
    ## Step:  AIC=1511.35
    ## y_train ~ age + marital + housing + loan + contact + month + 
    ##     campaign + previous + poutcome + emp.var.rate + cons.price.idx + 
    ##     cons.conf.idx + euribor3m + nr.employed + daysdummy1 + daysdummy2 + 
    ##     daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - marital         3   1457.1 1509.1
    ## - loan            1   1453.4 1509.4
    ## - previous        1   1453.5 1509.5
    ## - daysdummy2      1   1453.5 1509.5
    ## - poutcome        2   1455.6 1509.6
    ## - daysdummy1      1   1453.7 1509.7
    ## - euribor3m       1   1453.7 1509.7
    ## - nr.employed     1   1453.8 1509.8
    ## - daysdummy3      1   1453.9 1509.9
    ## - cons.conf.idx   1   1455.1 1511.1
    ## - housing         1   1455.2 1511.2
    ## <none>                1453.3 1511.3
    ## - campaign        1   1456.6 1512.6
    ## + default         2   1452.5 1514.5
    ## + day_of_week     4   1450.9 1516.9
    ## - cons.price.idx  1   1461.3 1517.3
    ## - age             1   1462.2 1518.2
    ## + education       7   1447.1 1519.1
    ## - emp.var.rate    1   1464.0 1520.0
    ## + job            11   1443.4 1523.4
    ## - contact         1   1474.0 1530.0
    ## - month           9   1494.2 1534.2
    ## 
    ## Step:  AIC=1509.13
    ## y_train ~ age + housing + loan + contact + month + campaign + 
    ##     previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
    ##     euribor3m + nr.employed + daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - loan            1   1457.2 1507.2
    ## - daysdummy2      1   1457.2 1507.2
    ## - previous        1   1457.2 1507.2
    ## - daysdummy1      1   1457.5 1507.5
    ## - euribor3m       1   1457.5 1507.5
    ## - poutcome        2   1459.5 1507.5
    ## - nr.employed     1   1457.5 1507.5
    ## - daysdummy3      1   1457.6 1507.6
    ## - cons.conf.idx   1   1458.8 1508.8
    ## - housing         1   1459.1 1509.1
    ## <none>                1457.1 1509.1
    ## - campaign        1   1460.3 1510.3
    ## + marital         3   1453.3 1511.3
    ## + default         2   1456.4 1512.4
    ## - age             1   1463.4 1513.4
    ## + day_of_week     4   1454.5 1514.5
    ## - cons.price.idx  1   1464.8 1514.8
    ## + education       7   1450.6 1516.6
    ## - emp.var.rate    1   1467.4 1517.4
    ## + job            11   1446.3 1520.3
    ## - contact         1   1477.5 1527.5
    ## - month           9   1499.8 1533.8
    ## 
    ## Step:  AIC=1507.18
    ## y_train ~ age + housing + contact + month + campaign + previous + 
    ##     poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
    ##     euribor3m + nr.employed + daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - daysdummy2      1   1457.3 1505.3
    ## - previous        1   1457.3 1505.3
    ## - daysdummy1      1   1457.5 1505.5
    ## - euribor3m       1   1457.6 1505.6
    ## - poutcome        2   1459.6 1505.6
    ## - nr.employed     1   1457.6 1505.6
    ## - daysdummy3      1   1457.7 1505.7
    ## - cons.conf.idx   1   1458.9 1506.9
    ## - housing         1   1459.2 1507.2
    ## <none>                1457.2 1507.2
    ## - campaign        1   1460.3 1508.3
    ## + loan            1   1457.1 1509.1
    ## + marital         3   1453.4 1509.4
    ## + default         2   1456.4 1510.4
    ## - age             1   1463.5 1511.5
    ## + day_of_week     4   1454.6 1512.6
    ## - cons.price.idx  1   1464.9 1512.9
    ## + education       7   1450.6 1514.6
    ## - emp.var.rate    1   1467.5 1515.5
    ## + job            11   1446.3 1518.3
    ## - contact         1   1477.7 1525.7
    ## - month           9   1499.8 1531.8
    ## 
    ## Step:  AIC=1505.26
    ## y_train ~ age + housing + contact + month + campaign + previous + 
    ##     poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
    ##     euribor3m + nr.employed + daysdummy1 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - previous        1   1457.3 1503.3
    ## - daysdummy1      1   1457.6 1503.6
    ## - euribor3m       1   1457.7 1503.7
    ## - nr.employed     1   1457.7 1503.7
    ## - daysdummy3      1   1458.0 1504.0
    ## - cons.conf.idx   1   1458.9 1504.9
    ## <none>                1457.3 1505.3
    ## - housing         1   1459.3 1505.3
    ## - campaign        1   1460.4 1506.4
    ## + daysdummy2      1   1457.2 1507.2
    ## + loan            1   1457.2 1507.2
    ## - poutcome        2   1463.4 1507.4
    ## + marital         3   1453.5 1507.5
    ## + default         2   1456.5 1508.5
    ## - age             1   1463.5 1509.5
    ## + day_of_week     4   1454.7 1510.7
    ## - cons.price.idx  1   1464.9 1510.9
    ## + education       7   1450.7 1512.7
    ## - emp.var.rate    1   1467.5 1513.5
    ## + job            11   1446.3 1516.3
    ## - contact         1   1477.8 1523.8
    ## - month           9   1499.9 1529.9
    ## 
    ## Step:  AIC=1503.32
    ## y_train ~ age + housing + contact + month + campaign + poutcome + 
    ##     emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + 
    ##     nr.employed + daysdummy1 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - daysdummy1      1   1457.6 1501.6
    ## - euribor3m       1   1457.7 1501.7
    ## - nr.employed     1   1457.7 1501.7
    ## - daysdummy3      1   1458.1 1502.1
    ## - cons.conf.idx   1   1459.0 1503.0
    ## <none>                1457.3 1503.3
    ## - housing         1   1459.4 1503.4
    ## - campaign        1   1460.5 1504.5
    ## + previous        1   1457.3 1505.3
    ## + loan            1   1457.3 1505.3
    ## + daysdummy2      1   1457.3 1505.3
    ## + marital         3   1453.6 1505.6
    ## - poutcome        2   1463.8 1505.8
    ## + default         2   1456.5 1506.5
    ## - age             1   1463.5 1507.5
    ## + day_of_week     4   1454.8 1508.8
    ## - cons.price.idx  1   1464.9 1508.9
    ## + education       7   1450.8 1510.8
    ## - emp.var.rate    1   1467.5 1511.5
    ## + job            11   1446.4 1514.4
    ## - contact         1   1477.8 1521.8
    ## - month           9   1500.0 1528.0
    ## 
    ## Step:  AIC=1501.62
    ## y_train ~ age + housing + contact + month + campaign + poutcome + 
    ##     emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + 
    ##     nr.employed + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - nr.employed     1   1458.0 1500.0
    ## - euribor3m       1   1458.0 1500.0
    ## - daysdummy3      1   1458.7 1500.7
    ## - cons.conf.idx   1   1459.2 1501.2
    ## <none>                1457.6 1501.6
    ## - housing         1   1459.7 1501.7
    ## - campaign        1   1460.8 1502.8
    ## + daysdummy1      1   1457.3 1503.3
    ## + daysdummy2      1   1457.6 1503.6
    ## + loan            1   1457.6 1503.6
    ## + previous        1   1457.6 1503.6
    ## + marital         3   1453.9 1503.9
    ## + default         2   1456.8 1504.8
    ## - age             1   1463.9 1505.9
    ## + day_of_week     4   1455.0 1507.0
    ## - cons.price.idx  1   1465.3 1507.3
    ## + education       7   1451.2 1509.2
    ## - emp.var.rate    1   1468.1 1510.1
    ## + job            11   1446.8 1512.8
    ## - contact         1   1478.0 1520.0
    ## - month           9   1500.2 1526.2
    ## - poutcome        2   1488.3 1528.3
    ## 
    ## Step:  AIC=1500.03
    ## y_train ~ age + housing + contact + month + campaign + poutcome + 
    ##     emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + 
    ##     daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - daysdummy3      1   1459.1 1499.1
    ## - cons.conf.idx   1   1459.3 1499.3
    ## <none>                1458.0 1500.0
    ## - housing         1   1460.1 1500.1
    ## - euribor3m       1   1460.7 1500.7
    ## - campaign        1   1461.2 1501.2
    ## + nr.employed     1   1457.6 1501.6
    ## + daysdummy1      1   1457.7 1501.7
    ## + daysdummy2      1   1458.0 1502.0
    ## + loan            1   1458.0 1502.0
    ## + previous        1   1458.0 1502.0
    ## + marital         3   1454.3 1502.3
    ## + default         2   1457.2 1503.2
    ## - age             1   1464.3 1504.3
    ## + day_of_week     4   1455.3 1505.3
    ## + education       7   1451.7 1507.7
    ## - emp.var.rate    1   1470.6 1510.6
    ## + job            11   1447.5 1511.5
    ## - contact         1   1478.4 1518.4
    ## - cons.price.idx  1   1485.2 1525.2
    ## - month           9   1501.5 1525.5
    ## - poutcome        2   1489.0 1527.0
    ## 
    ## Step:  AIC=1499.12
    ## y_train ~ age + housing + contact + month + campaign + poutcome + 
    ##     emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m
    ## 
    ##                  Df Deviance    AIC
    ## - cons.conf.idx   1   1460.4 1498.4
    ## <none>                1459.1 1499.1
    ## - housing         1   1461.4 1499.4
    ## - euribor3m       1   1461.8 1499.8
    ## + daysdummy3      1   1458.0 1500.0
    ## - campaign        1   1462.2 1500.2
    ## + daysdummy1      1   1458.5 1500.5
    ## + nr.employed     1   1458.7 1500.7
    ## + loan            1   1459.1 1501.1
    ## + daysdummy2      1   1459.1 1501.1
    ## + previous        1   1459.1 1501.1
    ## + marital         3   1455.5 1501.5
    ## + default         2   1458.3 1502.3
    ## - age             1   1465.8 1503.8
    ## + day_of_week     4   1456.6 1504.6
    ## + education       7   1452.9 1506.9
    ## - emp.var.rate    1   1471.7 1509.7
    ## + job            11   1448.7 1510.7
    ## - contact         1   1479.3 1517.3
    ## - cons.price.idx  1   1485.9 1523.9
    ## - month           9   1503.2 1525.2
    ## - poutcome        2   1489.7 1525.7
    ## 
    ## Step:  AIC=1498.41
    ## y_train ~ age + housing + contact + month + campaign + poutcome + 
    ##     emp.var.rate + cons.price.idx + euribor3m
    ## 
    ##                  Df Deviance    AIC
    ## <none>                1460.4 1498.4
    ## - housing         1   1462.8 1498.8
    ## + cons.conf.idx   1   1459.1 1499.1
    ## + daysdummy3      1   1459.3 1499.3
    ## - campaign        1   1463.5 1499.5
    ## + daysdummy1      1   1459.9 1499.9
    ## + nr.employed     1   1460.3 1500.3
    ## + loan            1   1460.3 1500.3
    ## + daysdummy2      1   1460.4 1500.4
    ## + previous        1   1460.4 1500.4
    ## + marital         3   1456.8 1500.8
    ## + default         2   1459.6 1501.6
    ## - euribor3m       1   1465.8 1501.8
    ## - age             1   1467.3 1503.3
    ## + day_of_week     4   1457.8 1503.8
    ## + education       7   1454.0 1506.0
    ## + job            11   1450.0 1510.0
    ## - contact         1   1479.4 1515.4
    ## - emp.var.rate    1   1479.8 1515.8
    ## - cons.price.idx  1   1489.9 1525.9
    ## - poutcome        2   1493.0 1527.0
    ## - month           9   1513.3 1533.3

``` r
summary(m4)
```

    ## 
    ## Call:
    ## glm(formula = y_train ~ age + housing + contact + month + campaign + 
    ##     poutcome + emp.var.rate + cons.price.idx + euribor3m, family = binomial, 
    ##     data = df_train2)
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         -1.836e+02  3.343e+01  -5.492 3.98e-08 ***
    ## age                  1.546e-02  5.875e-03   2.631  0.00852 ** 
    ## housingyes          -2.192e-01  1.415e-01  -1.549  0.12145    
    ## contacttelephone    -1.060e+00  2.586e-01  -4.097 4.18e-05 ***
    ## monthaug             6.041e-01  3.131e-01   1.930  0.05365 .  
    ## monthdec             9.452e-01  5.738e-01   1.647  0.09952 .  
    ## monthjul             4.000e-02  3.471e-01   0.115  0.90824    
    ## monthjun            -7.333e-02  3.763e-01  -0.195  0.84552    
    ## monthmar             2.060e+00  4.483e-01   4.595 4.33e-06 ***
    ## monthmay            -2.888e-01  2.770e-01  -1.043  0.29710    
    ## monthnov            -7.727e-01  3.816e-01  -2.025  0.04288 *  
    ## monthoct             6.246e-02  4.249e-01   0.147  0.88315    
    ## monthsep             7.776e-03  4.131e-01   0.019  0.98498    
    ## campaign            -6.758e-02  4.176e-02  -1.618  0.10564    
    ## poutcomenonexistent  2.062e-01  2.121e-01   0.972  0.33090    
    ## poutcomesuccess      1.617e+00  3.037e-01   5.323 1.02e-07 ***
    ## emp.var.rate        -1.568e+00  3.517e-01  -4.458 8.28e-06 ***
    ## cons.price.idx       1.909e+00  3.492e-01   5.467 4.58e-08 ***
    ## euribor3m            6.352e-01  2.723e-01   2.332  0.01968 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1917.3  on 2808  degrees of freedom
    ## Residual deviance: 1460.4  on 2790  degrees of freedom
    ## AIC: 1498.4
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
df_test2$PredProbC = predict.glm(m3, newdata = df_test2, type = "response")
df_test2$PredSurC = ifelse(df_test2$PredProbC >= 0.092, 1, 0)
caret::confusionMatrix(as.factor(df_test2$PredSurC), as.factor(df_test2$y_train), positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 842  60
    ##          1 223  80
    ##                                           
    ##                Accuracy : 0.7651          
    ##                  95% CI : (0.7402, 0.7888)
    ##     No Information Rate : 0.8838          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.2405          
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.57143         
    ##             Specificity : 0.79061         
    ##          Pos Pred Value : 0.26403         
    ##          Neg Pred Value : 0.93348         
    ##              Prevalence : 0.11618         
    ##          Detection Rate : 0.06639         
    ##    Detection Prevalence : 0.25145         
    ##       Balanced Accuracy : 0.68102         
    ##                                           
    ##        'Positive' Class : 1               
    ## 

``` r
df_test2$PredProbD = predict(m4, newdata = df_test2, type = "response")
df_test2$PredSurD = ifelse(df_test2$PredProbD >= 0.078, 1, 0)
caret::confusionMatrix(as.factor(df_test2$PredSurD), as.factor(df_test2$y_train), positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 812  51
    ##          1 253  89
    ##                                          
    ##                Accuracy : 0.7477         
    ##                  95% CI : (0.7222, 0.772)
    ##     No Information Rate : 0.8838         
    ##     P-Value [Acc > NIR] : 1              
    ##                                          
    ##                   Kappa : 0.2448         
    ##                                          
    ##  Mcnemar's Test P-Value : <2e-16         
    ##                                          
    ##             Sensitivity : 0.63571        
    ##             Specificity : 0.76244        
    ##          Pos Pred Value : 0.26023        
    ##          Neg Pred Value : 0.94090        
    ##              Prevalence : 0.11618        
    ##          Detection Rate : 0.07386        
    ##    Detection Prevalence : 0.28382        
    ##       Balanced Accuracy : 0.69908        
    ##                                          
    ##        'Positive' Class : 1              
    ## 

``` r
vif(m4)
```

    ##                     GVIF Df GVIF^(1/(2*Df))
    ## age             1.024785  1        1.012317
    ## housing         1.022655  1        1.011264
    ## contact         2.231673  1        1.493878
    ## month           6.950908  9        1.113730
    ## campaign        1.052675  1        1.025999
    ## poutcome        1.422201  2        1.092044
    ## emp.var.rate   74.749260  1        8.645765
    ## cons.price.idx 11.485397  1        3.389011
    ## euribor3m      51.479337  1        7.174910

The results of VIF indicate that we should refit model without
emp.var.rate

``` r
m5 = glm(formula = y_train ~ .-emp.var.rate, data = df_train2, family = binomial)
m6 = step(m5, direction = "both")
```

    ## Start:  AIC=1548.25
    ## y_train ~ (age + job + marital + education + default + housing + 
    ##     loan + contact + month + day_of_week + campaign + previous + 
    ##     poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
    ##     euribor3m + nr.employed + daysdummy1 + daysdummy2 + daysdummy3) - 
    ##     emp.var.rate
    ## 
    ##                  Df Deviance    AIC
    ## - job            11   1454.0 1536.0
    ## - education       7   1450.2 1540.2
    ## - day_of_week     4   1447.0 1543.0
    ## - marital         3   1446.5 1544.5
    ## - default         2   1445.0 1545.0
    ## - daysdummy2      1   1444.2 1546.2
    ## - previous        1   1444.3 1546.3
    ## - euribor3m       1   1444.3 1546.3
    ## - cons.price.idx  1   1444.3 1546.3
    ## - loan            1   1444.4 1546.4
    ## - daysdummy1      1   1444.6 1546.6
    ## - poutcome        2   1446.8 1546.8
    ## - cons.conf.idx   1   1445.0 1547.0
    ## - daysdummy3      1   1445.1 1547.1
    ## - nr.employed     1   1446.2 1548.2
    ## <none>                1444.2 1548.2
    ## - housing         1   1446.6 1548.6
    ## - campaign        1   1447.9 1549.9
    ## - age             1   1456.6 1558.6
    ## - contact         1   1459.3 1561.3
    ## - month           9   1490.0 1576.0
    ## 
    ## Step:  AIC=1536.02
    ## y_train ~ age + marital + education + default + housing + loan + 
    ##     contact + month + day_of_week + campaign + previous + poutcome + 
    ##     cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
    ##     daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - education       7   1460.5 1528.5
    ## - day_of_week     4   1456.5 1530.5
    ## - default         2   1454.7 1532.7
    ## - marital         3   1457.2 1533.2
    ## - cons.price.idx  1   1454.0 1534.0
    ## - daysdummy2      1   1454.0 1534.0
    ## - previous        1   1454.1 1534.1
    ## - loan            1   1454.1 1534.1
    ## - euribor3m       1   1454.2 1534.2
    ## - daysdummy1      1   1454.4 1534.4
    ## - poutcome        2   1456.5 1534.5
    ## - cons.conf.idx   1   1454.5 1534.5
    ## - daysdummy3      1   1454.8 1534.8
    ## <none>                1454.0 1536.0
    ## - nr.employed     1   1456.5 1536.5
    ## - housing         1   1456.5 1536.5
    ## - campaign        1   1457.7 1537.7
    ## - age             1   1464.3 1544.3
    ## - contact         1   1467.3 1547.3
    ## + job            11   1444.2 1548.2
    ## - month           9   1500.5 1564.5
    ## 
    ## Step:  AIC=1528.46
    ## y_train ~ age + marital + default + housing + loan + contact + 
    ##     month + day_of_week + campaign + previous + poutcome + cons.price.idx + 
    ##     cons.conf.idx + euribor3m + nr.employed + daysdummy1 + daysdummy2 + 
    ##     daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - day_of_week     4   1463.1 1523.1
    ## - default         2   1461.3 1525.3
    ## - marital         3   1463.8 1525.8
    ## - cons.price.idx  1   1460.5 1526.5
    ## - daysdummy2      1   1460.5 1526.5
    ## - previous        1   1460.5 1526.5
    ## - loan            1   1460.6 1526.6
    ## - euribor3m       1   1460.6 1526.6
    ## - daysdummy1      1   1460.8 1526.8
    ## - poutcome        2   1463.0 1527.0
    ## - cons.conf.idx   1   1461.1 1527.1
    ## - daysdummy3      1   1461.2 1527.2
    ## <none>                1460.5 1528.5
    ## - housing         1   1462.6 1528.6
    ## - nr.employed     1   1463.1 1529.1
    ## - campaign        1   1464.0 1530.0
    ## - age             1   1470.0 1536.0
    ## + education       7   1454.0 1536.0
    ## - contact         1   1473.9 1539.9
    ## + job            11   1450.2 1540.2
    ## - month           9   1507.7 1557.7
    ## 
    ## Step:  AIC=1523.06
    ## y_train ~ age + marital + default + housing + loan + contact + 
    ##     month + campaign + previous + poutcome + cons.price.idx + 
    ##     cons.conf.idx + euribor3m + nr.employed + daysdummy1 + daysdummy2 + 
    ##     daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - default         2   1464.0 1520.0
    ## - marital         3   1466.6 1520.6
    ## - daysdummy2      1   1463.1 1521.1
    ## - cons.price.idx  1   1463.1 1521.1
    ## - previous        1   1463.1 1521.1
    ## - loan            1   1463.1 1521.1
    ## - euribor3m       1   1463.1 1521.1
    ## - daysdummy1      1   1463.4 1521.4
    ## - daysdummy3      1   1463.7 1521.7
    ## - poutcome        2   1465.7 1521.7
    ## - cons.conf.idx   1   1463.8 1521.8
    ## <none>                1463.1 1523.1
    ## - housing         1   1465.1 1523.1
    ## - nr.employed     1   1465.3 1523.3
    ## - campaign        1   1466.8 1524.8
    ## + day_of_week     4   1460.5 1528.5
    ## + education       7   1456.5 1530.5
    ## - age             1   1472.6 1530.6
    ## + job            11   1453.0 1535.0
    ## - contact         1   1477.1 1535.1
    ## - month           9   1509.4 1551.4
    ## 
    ## Step:  AIC=1519.95
    ## y_train ~ age + marital + housing + loan + contact + month + 
    ##     campaign + previous + poutcome + cons.price.idx + cons.conf.idx + 
    ##     euribor3m + nr.employed + daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - marital         3   1467.4 1517.4
    ## - daysdummy2      1   1464.0 1518.0
    ## - cons.price.idx  1   1464.0 1518.0
    ## - previous        1   1464.0 1518.0
    ## - loan            1   1464.0 1518.0
    ## - euribor3m       1   1464.0 1518.0
    ## - daysdummy1      1   1464.3 1518.3
    ## - daysdummy3      1   1464.6 1518.6
    ## - poutcome        2   1466.6 1518.6
    ## - cons.conf.idx   1   1464.7 1518.7
    ## <none>                1464.0 1520.0
    ## - housing         1   1466.1 1520.1
    ## - nr.employed     1   1466.3 1520.3
    ## - campaign        1   1467.7 1521.7
    ## + default         2   1463.1 1523.1
    ## + day_of_week     4   1461.3 1525.3
    ## - age             1   1472.9 1526.9
    ## + education       7   1457.1 1527.1
    ## + job            11   1454.0 1532.0
    ## - contact         1   1478.0 1532.0
    ## - month           9   1511.2 1549.2
    ## 
    ## Step:  AIC=1517.41
    ## y_train ~ age + housing + loan + contact + month + campaign + 
    ##     previous + poutcome + cons.price.idx + cons.conf.idx + euribor3m + 
    ##     nr.employed + daysdummy1 + daysdummy2 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - daysdummy2      1   1467.4 1515.4
    ## - cons.price.idx  1   1467.4 1515.4
    ## - previous        1   1467.4 1515.4
    ## - loan            1   1467.5 1515.5
    ## - euribor3m       1   1467.5 1515.5
    ## - daysdummy1      1   1467.7 1515.7
    ## - daysdummy3      1   1468.0 1516.0
    ## - cons.conf.idx   1   1468.1 1516.1
    ## - poutcome        2   1470.2 1516.2
    ## <none>                1467.4 1517.4
    ## - housing         1   1469.6 1517.6
    ## - nr.employed     1   1469.9 1517.9
    ## - campaign        1   1471.1 1519.1
    ## + marital         3   1464.0 1520.0
    ## + default         2   1466.6 1520.6
    ## - age             1   1473.8 1521.8
    ## + day_of_week     4   1464.7 1522.7
    ## + education       7   1460.3 1524.3
    ## + job            11   1456.4 1528.4
    ## - contact         1   1481.3 1529.3
    ## - month           9   1516.8 1548.8
    ## 
    ## Step:  AIC=1515.41
    ## y_train ~ age + housing + loan + contact + month + campaign + 
    ##     previous + poutcome + cons.price.idx + cons.conf.idx + euribor3m + 
    ##     nr.employed + daysdummy1 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - cons.price.idx  1   1467.4 1513.4
    ## - previous        1   1467.4 1513.4
    ## - loan            1   1467.5 1513.5
    ## - euribor3m       1   1467.5 1513.5
    ## - daysdummy1      1   1468.0 1514.0
    ## - daysdummy3      1   1468.1 1514.1
    ## - cons.conf.idx   1   1468.1 1514.1
    ## <none>                1467.4 1515.4
    ## - housing         1   1469.7 1515.7
    ## - nr.employed     1   1469.9 1515.9
    ## - poutcome        2   1473.1 1517.1
    ## - campaign        1   1471.1 1517.1
    ## + daysdummy2      1   1467.4 1517.4
    ## + marital         3   1464.0 1518.0
    ## + default         2   1466.6 1518.6
    ## - age             1   1473.8 1519.8
    ## + day_of_week     4   1464.7 1520.7
    ## + education       7   1460.3 1522.3
    ## + job            11   1456.4 1526.4
    ## - contact         1   1481.3 1527.3
    ## - month           9   1516.8 1546.8
    ## 
    ## Step:  AIC=1513.43
    ## y_train ~ age + housing + loan + contact + month + campaign + 
    ##     previous + poutcome + cons.conf.idx + euribor3m + nr.employed + 
    ##     daysdummy1 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - previous        1   1467.5 1511.5
    ## - loan            1   1467.5 1511.5
    ## - daysdummy1      1   1468.0 1512.0
    ## - daysdummy3      1   1468.1 1512.1
    ## - cons.conf.idx   1   1468.9 1512.9
    ## - euribor3m       1   1469.1 1513.1
    ## <none>                1467.4 1513.4
    ## - housing         1   1469.7 1513.7
    ## - campaign        1   1471.1 1515.1
    ## - poutcome        2   1473.2 1515.2
    ## + cons.price.idx  1   1467.4 1515.4
    ## + daysdummy2      1   1467.4 1515.4
    ## + marital         3   1464.0 1516.0
    ## + default         2   1466.6 1516.6
    ## - age             1   1473.8 1517.8
    ## + day_of_week     4   1464.7 1518.7
    ## + education       7   1460.4 1520.4
    ## + job            11   1456.5 1524.5
    ## - contact         1   1482.1 1526.1
    ## - nr.employed     1   1490.0 1534.0
    ## - month           9   1527.2 1555.2
    ## 
    ## Step:  AIC=1511.45
    ## y_train ~ age + housing + loan + contact + month + campaign + 
    ##     poutcome + cons.conf.idx + euribor3m + nr.employed + daysdummy1 + 
    ##     daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - loan            1   1467.5 1509.5
    ## - daysdummy1      1   1468.0 1510.0
    ## - daysdummy3      1   1468.2 1510.2
    ## - cons.conf.idx   1   1469.0 1511.0
    ## - euribor3m       1   1469.1 1511.1
    ## <none>                1467.5 1511.5
    ## - housing         1   1469.7 1511.7
    ## - campaign        1   1471.2 1513.2
    ## + previous        1   1467.4 1513.4
    ## + cons.price.idx  1   1467.4 1513.4
    ## + daysdummy2      1   1467.5 1513.5
    ## - poutcome        2   1473.5 1513.5
    ## + marital         3   1464.0 1514.0
    ## + default         2   1466.6 1514.6
    ## - age             1   1473.8 1515.8
    ## + day_of_week     4   1464.7 1516.7
    ## + education       7   1460.4 1518.4
    ## + job            11   1456.5 1522.5
    ## - contact         1   1482.1 1524.1
    ## - nr.employed     1   1491.0 1533.0
    ## - month           9   1527.3 1553.3
    ## 
    ## Step:  AIC=1509.54
    ## y_train ~ age + housing + contact + month + campaign + poutcome + 
    ##     cons.conf.idx + euribor3m + nr.employed + daysdummy1 + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - daysdummy1      1   1468.1 1508.1
    ## - daysdummy3      1   1468.2 1508.2
    ## - cons.conf.idx   1   1469.1 1509.1
    ## - euribor3m       1   1469.2 1509.2
    ## <none>                1467.5 1509.5
    ## - housing         1   1469.9 1509.9
    ## - campaign        1   1471.2 1511.2
    ## + loan            1   1467.5 1511.5
    ## + previous        1   1467.5 1511.5
    ## + cons.price.idx  1   1467.5 1511.5
    ## + daysdummy2      1   1467.5 1511.5
    ## - poutcome        2   1473.6 1511.6
    ## + marital         3   1464.1 1512.1
    ## + default         2   1466.7 1512.7
    ## - age             1   1473.9 1513.9
    ## + day_of_week     4   1464.8 1514.8
    ## + education       7   1460.5 1516.5
    ## + job            11   1456.7 1520.7
    ## - contact         1   1482.3 1522.3
    ## - nr.employed     1   1491.3 1531.3
    ## - month           9   1527.3 1551.3
    ## 
    ## Step:  AIC=1508.08
    ## y_train ~ age + housing + contact + month + campaign + poutcome + 
    ##     cons.conf.idx + euribor3m + nr.employed + daysdummy3
    ## 
    ##                  Df Deviance    AIC
    ## - daysdummy3      1   1469.2 1507.2
    ## - cons.conf.idx   1   1469.6 1507.6
    ## - euribor3m       1   1469.7 1507.7
    ## <none>                1468.1 1508.1
    ## - housing         1   1470.3 1508.3
    ## + daysdummy1      1   1467.5 1509.5
    ## - campaign        1   1471.8 1509.8
    ## + daysdummy2      1   1467.8 1509.8
    ## + loan            1   1468.0 1510.0
    ## + cons.price.idx  1   1468.1 1510.1
    ## + previous        1   1468.1 1510.1
    ## + marital         3   1464.6 1510.6
    ## + default         2   1467.2 1511.2
    ## - age             1   1474.5 1512.5
    ## + day_of_week     4   1465.4 1513.4
    ## + education       7   1461.2 1515.2
    ## + job            11   1457.4 1519.4
    ## - contact         1   1482.7 1520.7
    ## - nr.employed     1   1491.5 1529.5
    ## - poutcome        2   1501.4 1537.4
    ## - month           9   1527.7 1549.7
    ## 
    ## Step:  AIC=1507.21
    ## y_train ~ age + housing + contact + month + campaign + poutcome + 
    ##     cons.conf.idx + euribor3m + nr.employed
    ## 
    ##                  Df Deviance    AIC
    ## - euribor3m       1   1470.6 1506.6
    ## - cons.conf.idx   1   1470.9 1506.9
    ## <none>                1469.2 1507.2
    ## - housing         1   1471.7 1507.7
    ## + daysdummy3      1   1468.1 1508.1
    ## + daysdummy1      1   1468.2 1508.2
    ## - campaign        1   1472.9 1508.9
    ## + daysdummy2      1   1469.0 1509.0
    ## + loan            1   1469.1 1509.1
    ## + cons.price.idx  1   1469.2 1509.2
    ## + previous        1   1469.2 1509.2
    ## + marital         3   1465.8 1509.8
    ## + default         2   1468.3 1510.3
    ## - age             1   1476.1 1512.1
    ## + day_of_week     4   1466.7 1512.7
    ## + education       7   1462.5 1514.5
    ## + job            11   1458.6 1518.6
    ## - contact         1   1483.7 1519.7
    ## - nr.employed     1   1491.9 1527.9
    ## - poutcome        2   1502.2 1536.2
    ## - month           9   1529.2 1549.2
    ## 
    ## Step:  AIC=1506.6
    ## y_train ~ age + housing + contact + month + campaign + poutcome + 
    ##     cons.conf.idx + nr.employed
    ## 
    ##                  Df Deviance    AIC
    ## - cons.conf.idx   1   1471.8 1505.8
    ## <none>                1470.6 1506.6
    ## + euribor3m       1   1469.2 1507.2
    ## + cons.price.idx  1   1469.3 1507.3
    ## - housing         1   1473.4 1507.4
    ## + daysdummy3      1   1469.7 1507.7
    ## + daysdummy1      1   1469.8 1507.8
    ## - campaign        1   1474.1 1508.1
    ## + daysdummy2      1   1470.4 1508.4
    ## + loan            1   1470.5 1508.5
    ## + previous        1   1470.6 1508.6
    ## + marital         3   1467.3 1509.3
    ## + default         2   1469.8 1509.8
    ## - age             1   1477.6 1511.6
    ## + day_of_week     4   1468.0 1512.0
    ## + education       7   1463.7 1513.7
    ## + job            11   1459.8 1517.8
    ## - contact         1   1484.3 1518.3
    ## - poutcome        2   1505.9 1537.9
    ## - month           9   1530.1 1548.1
    ## - nr.employed     1   1558.6 1592.6
    ## 
    ## Step:  AIC=1505.83
    ## y_train ~ age + housing + contact + month + campaign + poutcome + 
    ##     nr.employed
    ## 
    ##                  Df Deviance    AIC
    ## <none>                1471.8 1505.8
    ## + cons.conf.idx   1   1470.6 1506.6
    ## - housing         1   1474.7 1506.7
    ## + daysdummy3      1   1470.8 1506.8
    ## + euribor3m       1   1470.9 1506.9
    ## + daysdummy1      1   1471.0 1507.0
    ## - campaign        1   1475.5 1507.5
    ## + daysdummy2      1   1471.7 1507.7
    ## + cons.price.idx  1   1471.7 1507.7
    ## + loan            1   1471.7 1507.7
    ## + previous        1   1471.8 1507.8
    ## + marital         3   1468.6 1508.6
    ## + default         2   1471.0 1509.0
    ## - age             1   1479.0 1511.0
    ## + day_of_week     4   1469.3 1511.3
    ## + education       7   1464.7 1512.7
    ## - contact         1   1484.3 1516.3
    ## + job            11   1461.0 1517.0
    ## - poutcome        2   1507.5 1537.5
    ## - month           9   1532.9 1548.9
    ## - nr.employed     1   1562.3 1594.3

``` r
summary(m6)
```

    ## 
    ## Call:
    ## glm(formula = y_train ~ age + housing + contact + month + campaign + 
    ##     poutcome + nr.employed, family = binomial, data = df_train2)
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         53.356048   5.608072   9.514  < 2e-16 ***
    ## age                  0.015647   0.005840   2.679 0.007380 ** 
    ## housingyes          -0.236835   0.140206  -1.689 0.091183 .  
    ## contacttelephone    -0.709184   0.207217  -3.422 0.000621 ***
    ## monthaug             0.143931   0.298442   0.482 0.629612    
    ## monthdec             0.859176   0.555907   1.546 0.122216    
    ## monthjul             0.337018   0.307003   1.098 0.272305    
    ## monthjun             0.883328   0.311759   2.833 0.004606 ** 
    ## monthmar             1.539638   0.453627   3.394 0.000689 ***
    ## monthmay            -0.523167   0.273453  -1.913 0.055724 .  
    ## monthnov            -0.397445   0.325639  -1.221 0.222273    
    ## monthoct             0.081393   0.407932   0.200 0.841851    
    ## monthsep            -0.501905   0.420512  -1.194 0.232652    
    ## campaign            -0.073911   0.041908  -1.764 0.077791 .  
    ## poutcomenonexistent  0.230040   0.209624   1.097 0.272470    
    ## poutcomesuccess      1.679658   0.301733   5.567  2.6e-08 ***
    ## nr.employed         -0.010896   0.001105  -9.864  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1917.3  on 2808  degrees of freedom
    ## Residual deviance: 1471.8  on 2792  degrees of freedom
    ## AIC: 1505.8
    ## 
    ## Number of Fisher Scoring iterations: 6

Check to see if it worked

``` r
vif(m6)
```

    ##                 GVIF Df GVIF^(1/(2*Df))
    ## age         1.025243  1        1.012543
    ## housing     1.017302  1        1.008614
    ## contact     1.390318  1        1.179117
    ## month       2.165531  9        1.043861
    ## campaign    1.045202  1        1.022351
    ## poutcome    1.405464  2        1.088817
    ## nr.employed 1.997349  1        1.413276

Rerun confusion matrix

``` r
df_test2$PredProbD = predict(m6, newdata = df_test2, type = "response")
df_test2$PredSurD = ifelse(df_test2$PredProbD >= 0.07189, 1, 0)
caret::confusionMatrix(as.factor(df_test2$PredSurD), as.factor(df_test2$y_train), positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 723  45
    ##          1 342  95
    ##                                           
    ##                Accuracy : 0.6788          
    ##                  95% CI : (0.6516, 0.7052)
    ##     No Information Rate : 0.8838          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.186           
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.67857         
    ##             Specificity : 0.67887         
    ##          Pos Pred Value : 0.21739         
    ##          Neg Pred Value : 0.94141         
    ##              Prevalence : 0.11618         
    ##          Detection Rate : 0.07884         
    ##    Detection Prevalence : 0.36266         
    ##       Balanced Accuracy : 0.67872         
    ##                                           
    ##        'Positive' Class : 1               
    ## 

Creat plot to identify optimal threshold

``` r
pred = prediction(df_test2$PredProbD, df_test2$y_train)
auc = round(as.numeric(performance(pred, measure = "auc")@y.values),5)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize = T, main = "ROC Curve")
text(0.5, 0.5, paste("AUC:", auc))
```

![](BankMarketingCaseStudy_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
plot(unlist(performance(pred, "sens")@x.values), unlist(performance(pred, "sens")@y.values), 
     type="l", lwd=2, 
     ylab="Sensitivity", xlab="Cutoff", main = paste("Maximized Cutoff\n","AUC: ",auc))

par(new=TRUE)

plot(unlist(performance(pred, "spec")@x.values), unlist(performance(pred, "spec")@y.values), 
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.2)) 

mtext("Specificity",side=4, col='red')
min.diff <-which.min(abs(unlist(performance(pred, "sens")@y.values) - unlist(performance(pred, "spec")@y.values)))
min.x<-unlist(performance(pred, "sens")@x.values)[min.diff]
min.y<-unlist(performance(pred, "spec")@y.values)[min.diff]
optimal <-min.x 

abline(h = min.y, lty = 3)
abline(v = min.x, lty = 3)
text(min.x,0,paste("optimal threshold=",round(optimal,5)), pos = 3)
```

![](BankMarketingCaseStudy_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->

LDA Model

``` r
set.seed(3)
index = sample(1:nrow(df_sample2), 0.7 * nrow(df_sample2))
df_train3 = df_sample2[index, ]
df_test3 = df_sample2[-index, ]
```

Create LDA model using the variables found to be significant in m4

``` r
lda_model = MASS::lda(y_train ~ age + contact + month + campaign + poutcome + nr.employed, data = df_train3)
lda_model
```

    ## Call:
    ## lda(y_train ~ age + contact + month + campaign + poutcome + nr.employed, 
    ##     data = df_train3)
    ## 
    ## Prior probabilities of groups:
    ##         0         1 
    ## 0.8924884 0.1075116 
    ## 
    ## Group means:
    ##        age contacttelephone  monthaug    monthdec  monthjul  monthjun
    ## 0 39.67132        0.3861189 0.1503789 0.003589948 0.1846829 0.1268448
    ## 1 42.80795        0.1589404 0.1523179 0.029801325 0.1390728 0.1456954
    ##      monthmar  monthmay   monthnov   monthoct   monthsep campaign
    ## 0 0.005185481 0.3498205 0.10929398 0.01116873 0.01196649 2.593139
    ## 1 0.059602649 0.1821192 0.09271523 0.05960265 0.05629139 1.993377
    ##   poutcomenonexistent poutcomesuccess nr.employed
    ## 0           0.8911049      0.01356203    5175.778
    ## 1           0.6456954      0.20529801    5088.819
    ## 
    ## Coefficients of linear discriminants:
    ##                             LD1
    ## age                  0.01365479
    ## contacttelephone    -0.13565201
    ## monthaug             0.49456848
    ## monthdec             1.67240490
    ## monthjul             0.62258222
    ## monthjun             0.83006568
    ## monthmar             2.47297259
    ## monthmay            -0.19478246
    ## monthnov             0.06910084
    ## monthoct             0.57739019
    ## monthsep            -0.31618890
    ## campaign            -0.02457223
    ## poutcomenonexistent  0.23896849
    ## poutcomesuccess      2.77697362
    ## nr.employed         -0.01065264

``` r
predict_lda = predict(lda_model, df_test3)
caret::confusionMatrix(as.factor(predict_lda$class), as.factor(df_test3$y_train), positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1026  100
    ##          1   39   40
    ##                                           
    ##                Accuracy : 0.8846          
    ##                  95% CI : (0.8653, 0.9021)
    ##     No Information Rate : 0.8838          
    ##     P-Value [Acc > NIR] : 0.4867          
    ##                                           
    ##                   Kappa : 0.3072          
    ##                                           
    ##  Mcnemar's Test P-Value : 3.597e-07       
    ##                                           
    ##             Sensitivity : 0.28571         
    ##             Specificity : 0.96338         
    ##          Pos Pred Value : 0.50633         
    ##          Neg Pred Value : 0.91119         
    ##              Prevalence : 0.11618         
    ##          Detection Rate : 0.03320         
    ##    Detection Prevalence : 0.06556         
    ##       Balanced Accuracy : 0.62455         
    ##                                           
    ##        'Positive' Class : 1               
    ## 

Create LDA model using the variables found to be significant in m2

``` r
lda_model2 = MASS::lda(y_train ~ contact + month + campaign + emp.var.rate + cons.price.idx + cons.conf.idx + daysdummy1 + daysdummy2 + daysdummy3, data = df_train)
lda_model2
```

    ## Call:
    ## lda(y_train ~ contact + month + campaign + emp.var.rate + cons.price.idx + 
    ##     cons.conf.idx + daysdummy1 + daysdummy2 + daysdummy3, data = df_train)
    ## 
    ## Prior probabilities of groups:
    ##         0         1 
    ## 0.8886576 0.1113424 
    ## 
    ## Group means:
    ##   contacttelephone  monthaug   monthdec  monthjul  monthjun    monthmar
    ## 0        0.3672912 0.1569087 0.00312256 0.1818891 0.1252927 0.005464481
    ## 1        0.1775701 0.1464174 0.02803738 0.1370717 0.1433022 0.065420561
    ##    monthmay   monthnov   monthoct    monthsep campaign emp.var.rate
    ## 0 0.3419204 0.11475410 0.01249024 0.008977361 2.601874    0.2440281
    ## 1 0.1931464 0.09657321 0.04984424 0.062305296 1.962617   -1.1429907
    ##   cons.price.idx cons.conf.idx daysdummy1 daysdummy2 daysdummy3
    ## 0       93.59620     -40.64235 0.01053864 0.00312256 0.00156128
    ## 1       93.42158     -39.54361 0.18068536 0.04049844 0.01246106
    ## 
    ## Coefficients of linear discriminants:
    ##                          LD1
    ## contacttelephone -0.55924015
    ## monthaug          0.24305501
    ## monthdec          0.77472903
    ## monthjul          0.26407430
    ## monthjun          0.09695895
    ## monthmar          2.51366252
    ## monthmay         -0.25592480
    ## monthnov         -0.14335650
    ## monthoct         -0.37781890
    ## monthsep         -0.08159841
    ## campaign         -0.02879093
    ## emp.var.rate     -0.72662258
    ## cons.price.idx    1.34532666
    ## cons.conf.idx     0.08097293
    ## daysdummy1        2.59567803
    ## daysdummy2        2.05198594
    ## daysdummy3        0.67910190

``` r
predict_lda2 = predict(lda_model2, df_test)
caret::confusionMatrix(as.factor(predict_lda2$class), as.factor(df_test$y_train), positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1063   84
    ##          1   43   46
    ##                                          
    ##                Accuracy : 0.8972         
    ##                  95% CI : (0.879, 0.9136)
    ##     No Information Rate : 0.8948         
    ##     P-Value [Acc > NIR] : 0.4128621      
    ##                                          
    ##                   Kappa : 0.3659         
    ##                                          
    ##  Mcnemar's Test P-Value : 0.0003861      
    ##                                          
    ##             Sensitivity : 0.35385        
    ##             Specificity : 0.96112        
    ##          Pos Pred Value : 0.51685        
    ##          Neg Pred Value : 0.92677        
    ##              Prevalence : 0.10518        
    ##          Detection Rate : 0.03722        
    ##    Detection Prevalence : 0.07201        
    ##       Balanced Accuracy : 0.65748        
    ##                                          
    ##        'Positive' Class : 1              
    ## 
