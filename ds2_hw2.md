ds2_hw2
================
Ruilian Zhang
3/6/2022

``` r
# data cleaning
df = read_csv("College.csv") %>% 
  janitor::clean_names() %>% 
  select(-college) %>% 
  select(outstate, everything()) %>% 
  na.omit()
```

    ## Rows: 565 Columns: 18

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): College
    ## dbl (17): Apps, Accept, Enroll, Top10perc, Top25perc, F.Undergrad, P.Undergr...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# data partition
indexTrain = createDataPartition(y = df$outstate,
                                 p = 0.8,
                                 list = FALSE)


train_df = df[indexTrain, ]
test_df = df[-indexTrain, ]

x_train = model.matrix(outstate ~ ., train_df)[ , -1]
y_train = train_df$outstate

x_test = model.matrix(outstate ~ ., test_df)[ , -1]
y_test = test_df$outstate
```

## Exploratory data analysis (using train_df)

``` r
# data dimension and summary
dim(train_df)
```

    ## [1] 453  17

``` r
summary(train_df)
```

    ##     outstate          apps           accept          enroll      
    ##  Min.   : 2340   Min.   :   81   Min.   :   72   Min.   :  35.0  
    ##  1st Qu.: 9100   1st Qu.:  632   1st Qu.:  503   1st Qu.: 209.0  
    ##  Median :11200   Median : 1132   Median :  859   Median : 326.0  
    ##  Mean   :11819   Mean   : 1957   Mean   : 1274   Mean   : 451.9  
    ##  3rd Qu.:13970   3rd Qu.: 2190   3rd Qu.: 1546   3rd Qu.: 514.0  
    ##  Max.   :21700   Max.   :20192   Max.   :13007   Max.   :4615.0  
    ##    top10perc      top25perc       f_undergrad     p_undergrad    
    ##  Min.   : 1.0   Min.   :  9.00   Min.   :  139   Min.   :   1.0  
    ##  1st Qu.:17.0   1st Qu.: 43.00   1st Qu.:  840   1st Qu.:  62.0  
    ##  Median :25.0   Median : 56.00   Median : 1235   Median : 208.0  
    ##  Mean   :29.7   Mean   : 57.58   Mean   : 1844   Mean   : 414.6  
    ##  3rd Qu.:37.0   3rd Qu.: 70.00   3rd Qu.: 1988   3rd Qu.: 513.0  
    ##  Max.   :96.0   Max.   :100.00   Max.   :27378   Max.   :5346.0  
    ##    room_board       books           personal         ph_d      
    ##  Min.   :2460   Min.   : 250.0   Min.   : 250   Min.   :  8.0  
    ##  1st Qu.:3750   1st Qu.: 450.0   1st Qu.: 800   1st Qu.: 59.0  
    ##  Median :4400   Median : 500.0   Median :1100   Median : 73.0  
    ##  Mean   :4575   Mean   : 546.7   Mean   :1226   Mean   : 71.1  
    ##  3rd Qu.:5370   3rd Qu.: 600.0   3rd Qu.:1500   3rd Qu.: 85.0  
    ##  Max.   :8124   Max.   :2340.0   Max.   :6800   Max.   :100.0  
    ##     terminal        s_f_ratio      perc_alumni        expend     
    ##  Min.   : 24.00   Min.   : 2.50   Min.   : 2.00   Min.   : 3186  
    ##  1st Qu.: 68.00   1st Qu.:11.10   1st Qu.:17.00   1st Qu.: 7438  
    ##  Median : 80.00   Median :12.80   Median :26.00   Median : 8946  
    ##  Mean   : 78.58   Mean   :12.89   Mean   :26.43   Mean   :10573  
    ##  3rd Qu.: 92.00   3rd Qu.:14.50   3rd Qu.:35.00   3rd Qu.:11751  
    ##  Max.   :100.00   Max.   :27.80   Max.   :64.00   Max.   :56233  
    ##    grad_rate     
    ##  Min.   : 15.00  
    ##  1st Qu.: 58.00  
    ##  Median : 69.00  
    ##  Mean   : 69.16  
    ##  3rd Qu.: 81.00  
    ##  Max.   :118.00

``` r
skimr::skim(train_df)
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | train_df |
| Number of rows                                   | 453      |
| Number of columns                                | 17       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| numeric                                          | 17       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |     mean |      sd |     p0 |    p25 |     p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|---------:|--------:|-------:|-------:|--------:|--------:|--------:|:------|
| outstate      |         0 |             1 | 11818.63 | 3756.05 | 2340.0 | 9100.0 | 11200.0 | 13970.0 | 21700.0 | ▁▆▇▃▂ |
| apps          |         0 |             1 |  1956.68 | 2402.27 |   81.0 |  632.0 |  1132.0 |  2190.0 | 20192.0 | ▇▁▁▁▁ |
| accept        |         0 |             1 |  1273.79 | 1324.95 |   72.0 |  503.0 |   859.0 |  1546.0 | 13007.0 | ▇▁▁▁▁ |
| enroll        |         0 |             1 |   451.94 |  461.90 |   35.0 |  209.0 |   326.0 |   514.0 |  4615.0 | ▇▁▁▁▁ |
| top10perc     |         0 |             1 |    29.70 |   18.18 |    1.0 |   17.0 |    25.0 |    37.0 |    96.0 | ▇▇▃▁▁ |
| top25perc     |         0 |             1 |    57.58 |   19.93 |    9.0 |   43.0 |    56.0 |    70.0 |   100.0 | ▁▆▇▅▃ |
| f_undergrad   |         0 |             1 |  1843.89 | 2140.92 |  139.0 |  840.0 |  1235.0 |  1988.0 | 27378.0 | ▇▁▁▁▁ |
| p_undergrad   |         0 |             1 |   414.58 |  610.62 |    1.0 |   62.0 |   208.0 |   513.0 |  5346.0 | ▇▁▁▁▁ |
| room_board    |         0 |             1 |  4574.78 | 1062.89 | 2460.0 | 3750.0 |  4400.0 |  5370.0 |  8124.0 | ▃▇▅▂▁ |
| books         |         0 |             1 |   546.66 |  183.44 |  250.0 |  450.0 |   500.0 |   600.0 |  2340.0 | ▇▁▁▁▁ |
| personal      |         0 |             1 |  1225.58 |  654.89 |  250.0 |  800.0 |  1100.0 |  1500.0 |  6800.0 | ▇▂▁▁▁ |
| ph_d          |         0 |             1 |    71.10 |   17.43 |    8.0 |   59.0 |    73.0 |    85.0 |   100.0 | ▁▂▆▇▇ |
| terminal      |         0 |             1 |    78.58 |   15.36 |   24.0 |   68.0 |    80.0 |    92.0 |   100.0 | ▁▁▃▆▇ |
| s_f\_ratio    |         0 |             1 |    12.89 |    3.27 |    2.5 |   11.1 |    12.8 |    14.5 |    27.8 | ▁▇▇▁▁ |
| perc_alumni   |         0 |             1 |    26.43 |   12.67 |    2.0 |   17.0 |    26.0 |    35.0 |    64.0 | ▅▇▇▃▁ |
| expend        |         0 |             1 | 10573.42 | 5882.74 | 3186.0 | 7438.0 |  8946.0 | 11751.0 | 56233.0 | ▇▂▁▁▁ |
| grad_rate     |         0 |             1 |    69.16 |   16.55 |   15.0 |   58.0 |    69.0 |    81.0 |   118.0 | ▁▃▇▆▁ |

There are 453 rows and 17 columns in training data, all the variables
are numeric.

``` r
# set plot theme
theme1 = trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .4, .2, .5)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(.8, .1, .1, 1)
theme1$plot.line$lwd = 2
theme1$strip.background$col = rgb(.0, .2, .6, .2)
trellis.par.set(theme1)

# scatter plot
# all predictors are included since they are all continuous
featurePlot(
  x_train, 
  y_train, 
  plot = "scatter", 
  labels = c("","Out-of-state Tuition"),
  layout = c(4, 4))
```

![](ds2_hw2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

From the scatter plot above, we can see that there might be some linear
trends between the outcome variable `outstate` and some of the
predictors, for example, `phd` and `terminal`.

## Soothing spline

``` r
set.seed(2570)

# fit smoothing spline models using terminal as the only predictor of outstate 
fit_ss = smooth.spline(x = train_df$terminal, y = train_df$outstate)

# optimal degree of freedom obtained by generalized cross-validation
fit_ss$df
```

    ## [1] 4.540695

``` r
# generate predictor grid
range(train_df$terminal)
```

    ## [1]  24 100

``` r
terminal_grid <- seq(from = 24, to = 100, by = 1)

# make prediction
pred_ss = predict(fit_ss,
                  x = terminal_grid)

pred_ss_df = data.frame(predicted = pred_ss$y,
                        terminal = terminal_grid)
```
