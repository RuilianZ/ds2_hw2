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
    ##  1st Qu.: 9100   1st Qu.:  626   1st Qu.:  501   1st Qu.: 203.0  
    ##  Median :11200   Median : 1109   Median :  837   Median : 321.0  
    ##  Mean   :11807   Mean   : 1951   Mean   : 1289   Mean   : 449.6  
    ##  3rd Qu.:13970   3rd Qu.: 2135   3rd Qu.: 1541   3rd Qu.: 509.0  
    ##  Max.   :21700   Max.   :20192   Max.   :13007   Max.   :4615.0  
    ##    top10perc       top25perc       f_undergrad     p_undergrad     
    ##  Min.   : 1.00   Min.   :  9.00   Min.   :  139   Min.   :    1.0  
    ##  1st Qu.:17.00   1st Qu.: 43.00   1st Qu.:  809   1st Qu.:   66.0  
    ##  Median :25.00   Median : 55.00   Median : 1270   Median :  211.0  
    ##  Mean   :29.37   Mean   : 57.02   Mean   : 1855   Mean   :  448.9  
    ##  3rd Qu.:36.00   3rd Qu.: 70.00   3rd Qu.: 1964   3rd Qu.:  542.0  
    ##  Max.   :96.00   Max.   :100.00   Max.   :27378   Max.   :10221.0  
    ##    room_board       books           personal         ph_d       
    ##  Min.   :2370   Min.   : 250.0   Min.   : 250   Min.   :  8.00  
    ##  1st Qu.:3700   1st Qu.: 450.0   1st Qu.: 800   1st Qu.: 59.00  
    ##  Median :4372   Median : 500.0   Median :1090   Median : 73.00  
    ##  Mean   :4588   Mean   : 546.1   Mean   :1211   Mean   : 70.74  
    ##  3rd Qu.:5400   3rd Qu.: 600.0   3rd Qu.:1500   3rd Qu.: 85.00  
    ##  Max.   :8124   Max.   :2340.0   Max.   :6800   Max.   :100.00  
    ##     terminal        s_f_ratio      perc_alumni       expend     
    ##  Min.   : 24.00   Min.   : 2.50   Min.   : 2.0   Min.   : 3186  
    ##  1st Qu.: 68.00   1st Qu.:11.10   1st Qu.:16.0   1st Qu.: 7495  
    ##  Median : 80.00   Median :12.60   Median :25.0   Median : 8990  
    ##  Mean   : 78.42   Mean   :12.86   Mean   :25.9   Mean   :10384  
    ##  3rd Qu.: 92.00   3rd Qu.:14.40   3rd Qu.:34.0   3rd Qu.:11525  
    ##  Max.   :100.00   Max.   :39.80   Max.   :64.0   Max.   :42926  
    ##    grad_rate     
    ##  Min.   : 15.00  
    ##  1st Qu.: 58.00  
    ##  Median : 69.00  
    ##  Mean   : 68.62  
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
| outstate      |         0 |             1 | 11807.10 | 3693.06 | 2340.0 | 9100.0 | 11200.0 | 13970.0 | 21700.0 | ▁▆▇▃▂ |
| apps          |         0 |             1 |  1951.45 | 2485.30 |   81.0 |  626.0 |  1109.0 |  2135.0 | 20192.0 | ▇▁▁▁▁ |
| accept        |         0 |             1 |  1289.30 | 1419.08 |   72.0 |  501.0 |   837.0 |  1541.0 | 13007.0 | ▇▁▁▁▁ |
| enroll        |         0 |             1 |   449.55 |  471.76 |   35.0 |  203.0 |   321.0 |   509.0 |  4615.0 | ▇▁▁▁▁ |
| top10perc     |         0 |             1 |    29.37 |   17.95 |    1.0 |   17.0 |    25.0 |    36.0 |    96.0 | ▇▇▃▁▁ |
| top25perc     |         0 |             1 |    57.02 |   19.64 |    9.0 |   43.0 |    55.0 |    70.0 |   100.0 | ▁▆▇▆▃ |
| f_undergrad   |         0 |             1 |  1854.64 | 2214.61 |  139.0 |  809.0 |  1270.0 |  1964.0 | 27378.0 | ▇▁▁▁▁ |
| p_undergrad   |         0 |             1 |   448.94 |  756.33 |    1.0 |   66.0 |   211.0 |   542.0 | 10221.0 | ▇▁▁▁▁ |
| room_board    |         0 |             1 |  4587.81 | 1102.11 | 2370.0 | 3700.0 |  4372.0 |  5400.0 |  8124.0 | ▃▇▅▃▁ |
| books         |         0 |             1 |   546.11 |  171.71 |  250.0 |  450.0 |   500.0 |   600.0 |  2340.0 | ▇▁▁▁▁ |
| personal      |         0 |             1 |  1211.32 |  639.95 |  250.0 |  800.0 |  1090.0 |  1500.0 |  6800.0 | ▇▂▁▁▁ |
| ph_d          |         0 |             1 |    70.74 |   17.78 |    8.0 |   59.0 |    73.0 |    85.0 |   100.0 | ▁▁▆▇▇ |
| terminal      |         0 |             1 |    78.42 |   15.66 |   24.0 |   68.0 |    80.0 |    92.0 |   100.0 | ▁▁▃▆▇ |
| s_f\_ratio    |         0 |             1 |    12.86 |    3.47 |    2.5 |   11.1 |    12.6 |    14.4 |    39.8 | ▁▇▁▁▁ |
| perc_alumni   |         0 |             1 |    25.90 |   12.53 |    2.0 |   16.0 |    25.0 |    34.0 |    64.0 | ▅▇▇▂▁ |
| expend        |         0 |             1 | 10383.65 | 5017.64 | 3186.0 | 7495.0 |  8990.0 | 11525.0 | 42926.0 | ▇▂▁▁▁ |
| grad_rate     |         0 |             1 |    68.62 |   16.91 |   15.0 |   58.0 |    69.0 |    81.0 |   118.0 | ▁▃▇▆▁ |

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

## Smoothing splines

``` r
set.seed(2570)

# fit smoothing spline models using terminal as the only predictor of outstate 
fit_ss = smooth.spline(x = train_df$terminal, y = train_df$outstate)

# optimal degree of freedom obtained by generalized cross-validation
fit_ss$df
```

    ## [1] 4.244528

The optimal degree of freedom obtained by default cross validation is
4.245.

Use this **optimal degree of freedom** to make following predictions:

``` r
# make prediction using a grid of terminal values
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

# plot test data
p = ggplot(data = test_df, aes(x = terminal, y = outstate)) +
     geom_point(color = rgb(.2, .4, .2, .5))

# plot predicted value
p + 
  geom_line(aes(x = terminal, y = predicted), 
            data = pred_ss_df,
            color = rgb(.8, .1, .1, 1)) + 
  theme_bw()
```

![](ds2_hw2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# make prediction using test data
pred_ss_test = predict(fit_ss,
                       x = test_df$terminal)

pred_ss_test_df = data.frame(predicted = pred_ss_test$y,
                             terminal = test_df$terminal)

# plot predicted value
p + 
  geom_line(aes(x = terminal, y = predicted), 
            data = pred_ss_test_df,
            color = rgb(.8, .1, .1, 1)) + 
  theme_bw()
```

![](ds2_hw2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Use **a range of degree of freedom** to make predictions:

``` r
# write a function using a range of df to fit models
ss_func = function(df) {
  
  fit_ss_fun = smooth.spline(x = train_df$terminal, 
                                   y = train_df$outstate,
                                   df = df)
  
  pred_ss_fun = predict(fit_ss_fun, x = terminal_grid)
  
  
  pred_ss_df_fun = data.frame(predicted = pred_ss_fun$y,
                         terminal = terminal_grid,
                         df = df)
  
}

# create a list of df
# 1 < df <= 16 - 1
df_list = seq(2, 15, 1)

# run the function using df_list
output_ss = list()

for (x in df_list) {
  output_ss[[x]] = ss_func(x)
}

# do.call() executes a function by its name and a list of corresponding arguments
# e.g. do.call("any_function", arguments_list) 
output_ss_df = do.call("rbind", output_ss) %>% 
  as.data.frame()
```
