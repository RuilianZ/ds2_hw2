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
    ##  1st Qu.: 9100   1st Qu.:  632   1st Qu.:  503   1st Qu.: 210.0  
    ##  Median :11200   Median : 1160   Median :  880   Median : 328.0  
    ##  Mean   :11769   Mean   : 1987   Mean   : 1316   Mean   : 461.6  
    ##  3rd Qu.:13970   3rd Qu.: 2161   3rd Qu.: 1546   3rd Qu.: 526.0  
    ##  Max.   :21700   Max.   :20192   Max.   :13007   Max.   :4615.0  
    ##    top10perc       top25perc       f_undergrad     p_undergrad   
    ##  Min.   : 2.00   Min.   :  9.00   Min.   :  139   Min.   :    1  
    ##  1st Qu.:16.00   1st Qu.: 42.00   1st Qu.:  836   1st Qu.:   61  
    ##  Median :25.00   Median : 55.00   Median : 1263   Median :  205  
    ##  Mean   :29.26   Mean   : 56.82   Mean   : 1890   Mean   :  440  
    ##  3rd Qu.:36.00   3rd Qu.: 70.00   3rd Qu.: 1988   3rd Qu.:  556  
    ##  Max.   :96.00   Max.   :100.00   Max.   :27378   Max.   :10221  
    ##    room_board       books           personal         ph_d       
    ##  Min.   :2370   Min.   : 250.0   Min.   : 250   Min.   : 10.00  
    ##  1st Qu.:3735   1st Qu.: 450.0   1st Qu.: 800   1st Qu.: 61.00  
    ##  Median :4400   Median : 500.0   Median :1100   Median : 74.00  
    ##  Mean   :4597   Mean   : 543.8   Mean   :1214   Mean   : 71.66  
    ##  3rd Qu.:5406   3rd Qu.: 600.0   3rd Qu.:1500   3rd Qu.: 85.00  
    ##  Max.   :7425   Max.   :2000.0   Max.   :6800   Max.   :100.00  
    ##     terminal        s_f_ratio      perc_alumni        expend     
    ##  Min.   : 24.00   Min.   : 2.50   Min.   : 2.00   Min.   : 3186  
    ##  1st Qu.: 68.00   1st Qu.:11.20   1st Qu.:17.00   1st Qu.: 7547  
    ##  Median : 81.00   Median :12.90   Median :25.00   Median : 8985  
    ##  Mean   : 78.72   Mean   :13.04   Mean   :26.02   Mean   :10618  
    ##  3rd Qu.: 92.00   3rd Qu.:14.60   3rd Qu.:34.00   3rd Qu.:11561  
    ##  Max.   :100.00   Max.   :39.80   Max.   :64.00   Max.   :56233  
    ##    grad_rate     
    ##  Min.   : 15.00  
    ##  1st Qu.: 58.00  
    ##  Median : 69.00  
    ##  Mean   : 68.95  
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
| outstate      |         0 |             1 | 11769.18 | 3710.18 | 2340.0 | 9100.0 | 11200.0 | 13970.0 | 21700.0 | ▁▆▇▃▂ |
| apps          |         0 |             1 |  1986.70 | 2451.86 |   81.0 |  632.0 |  1160.0 |  2161.0 | 20192.0 | ▇▁▁▁▁ |
| accept        |         0 |             1 |  1316.41 | 1393.87 |   72.0 |  503.0 |   880.0 |  1546.0 | 13007.0 | ▇▁▁▁▁ |
| enroll        |         0 |             1 |   461.64 |  470.84 |   35.0 |  210.0 |   328.0 |   526.0 |  4615.0 | ▇▁▁▁▁ |
| top10perc     |         0 |             1 |    29.26 |   17.90 |    2.0 |   16.0 |    25.0 |    36.0 |    96.0 | ▇▇▃▁▁ |
| top25perc     |         0 |             1 |    56.82 |   19.61 |    9.0 |   42.0 |    55.0 |    70.0 |   100.0 | ▁▆▇▆▃ |
| f_undergrad   |         0 |             1 |  1890.44 | 2187.77 |  139.0 |  836.0 |  1263.0 |  1988.0 | 27378.0 | ▇▁▁▁▁ |
| p_undergrad   |         0 |             1 |   440.02 |  750.99 |    1.0 |   61.0 |   205.0 |   556.0 | 10221.0 | ▇▁▁▁▁ |
| room_board    |         0 |             1 |  4596.78 | 1078.16 | 2370.0 | 3735.0 |  4400.0 |  5406.0 |  7425.0 | ▂▇▅▅▁ |
| books         |         0 |             1 |   543.75 |  159.13 |  250.0 |  450.0 |   500.0 |   600.0 |  2000.0 | ▇▂▁▁▁ |
| personal      |         0 |             1 |  1214.16 |  636.67 |  250.0 |  800.0 |  1100.0 |  1500.0 |  6800.0 | ▇▂▁▁▁ |
| ph_d          |         0 |             1 |    71.66 |   17.25 |   10.0 |   61.0 |    74.0 |    85.0 |   100.0 | ▁▂▅▇▆ |
| terminal      |         0 |             1 |    78.72 |   15.24 |   24.0 |   68.0 |    81.0 |    92.0 |   100.0 | ▁▁▃▆▇ |
| s_f\_ratio    |         0 |             1 |    13.04 |    3.58 |    2.5 |   11.2 |    12.9 |    14.6 |    39.8 | ▁▇▁▁▁ |
| perc_alumni   |         0 |             1 |    26.02 |   12.35 |    2.0 |   17.0 |    25.0 |    34.0 |    64.0 | ▅▇▇▂▁ |
| expend        |         0 |             1 | 10617.61 | 5960.09 | 3186.0 | 7547.0 |  8985.0 | 11561.0 | 56233.0 | ▇▂▁▁▁ |
| grad_rate     |         0 |             1 |    68.95 |   16.73 |   15.0 |   58.0 |    69.0 |    81.0 |   118.0 | ▁▃▇▆▁ |

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

    ## [1] 4.795306

The optimal degree of freedom obtained by default cross validation is
4.795.

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
  
  pred_ss_fun = predict(fit_ss_fun, x = test_df$terminal)
  
  
  pred_ss_df_fun = data.frame(predicted = pred_ss_fun$y,
                         terminal = test_df$terminal,
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

``` r
# plot results for a range of df
p + 
  geom_line(aes(x = terminal, y = predicted, group = df, color = df), data = output_ss_df) + 
  geom_line(aes(x = terminal, y = predicted), data = pred_ss_test_df, color = rgb(.8, .1, .1, 1))
```

![](ds2_hw2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

The above plot shows the fitted smoothing spline models using a range of
degree of freedoms. The lines wiggle around the red line, which is the
model using the optimum degree of freedom.  
As the degree of freedom approaching to 2, the line gets more linear; as
the degree of freedom approaching to 15, the line gets more wiggled.  
Among all the fitted lines within the (2, 15) degree of freedom range,
df = 4.795 should be the nearest to the red line.

## Generalized Additive Model (GAM)

``` r
set.seed(2570)

# set cross validation method
ctrl = trainControl(method = "cv", number = 10)

# fit a GAM model using all the predictors
# ngcv package not available for current R version, siwth to caret
gam_fit = train(x = x_train,
                y = y_train,
                method = "gam",
                #tuneGrid = data.frame(method = "GCV.Cp",select = c(TRUE, FALSE)),
                trControl = ctrl)
```

    ## Loading required package: mgcv

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-38. For overview type 'help("mgcv-package")'.

    ## Warning: model fit failed for Fold09: select= TRUE, method=GCV.Cp Error in magic(G$y, G$X, msp, G$S, G$off, L = G$L, lsp0 = G$lsp0, G$rank,  : 
    ##   magic, the gcv/ubre optimizer, failed to converge after 400 iterations.

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo, :
    ## There were missing values in resampled performance measures.

``` r
gam_fit$bestTune
```

    ##   select method
    ## 1  FALSE GCV.Cp

``` r
gam_fit$finalModel
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## .outcome ~ s(perc_alumni) + s(terminal) + s(books) + s(ph_d) + 
    ##     s(top10perc) + s(grad_rate) + s(top25perc) + s(s_f_ratio) + 
    ##     s(personal) + s(p_undergrad) + s(enroll) + s(room_board) + 
    ##     s(accept) + s(f_undergrad) + s(apps) + s(expend)
    ## 
    ## Estimated degrees of freedom:
    ## 1.84 1.00 1.88 5.68 4.85 4.08 1.00 
    ## 3.68 2.85 1.00 1.00 1.95 3.37 5.87 
    ## 4.46 6.69  total = 52.21 
    ## 
    ## GCV score: 2960824

``` r
summary(gam_fit)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## .outcome ~ s(perc_alumni) + s(terminal) + s(books) + s(ph_d) + 
    ##     s(top10perc) + s(grad_rate) + s(top25perc) + s(s_f_ratio) + 
    ##     s(personal) + s(p_undergrad) + s(enroll) + s(room_board) + 
    ##     s(accept) + s(f_undergrad) + s(apps) + s(expend)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11769.18      76.04   154.8   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                  edf Ref.df      F  p-value    
    ## s(perc_alumni) 1.840  2.332  5.398  0.00334 ** 
    ## s(terminal)    1.000  1.000  1.590  0.20812    
    ## s(books)       1.881  2.364  0.951  0.45853    
    ## s(ph_d)        5.675  6.770  1.651  0.11086    
    ## s(top10perc)   4.850  5.936  1.379  0.21796    
    ## s(grad_rate)   4.082  5.075  3.516  0.00385 ** 
    ## s(top25perc)   1.000  1.000  0.367  0.54504    
    ## s(s_f_ratio)   3.676  4.612  1.876  0.11596    
    ## s(personal)    2.851  3.564  1.784  0.14906    
    ## s(p_undergrad) 1.000  1.000  0.290  0.59030    
    ## s(enroll)      1.000  1.000 19.252 1.49e-05 ***
    ## s(room_board)  1.953  2.475 14.285 4.41e-07 ***
    ## s(accept)      3.374  4.180  4.081  0.00266 ** 
    ## s(f_undergrad) 5.874  6.882  3.013  0.00386 ** 
    ## s(apps)        4.461  5.426  1.485  0.17923    
    ## s(expend)      6.692  7.769 17.570  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =   0.81   Deviance explained = 83.1%
    ## GCV = 2.9608e+06  Scale est. = 2.6196e+06  n = 453

``` r
# plot the results
par(mar=c(1,1,1,1))
par(mfrow = c(4, 4))

plot(gam_fit$finalModel, 
     residuals = TRUE, 
     all.terms = TRUE, 
     shade = TRUE, 
     shade.col = 5)
```

![](ds2_hw2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# train RMSE of final model
gam_train_rmse = sqrt(mean((y_train - predict(gam_fit)) ^ 2))
gam_train_rmse
```

    ## [1] 1522.395

``` r
# make predictions
gam_pred = predict(gam_fit, x_test)

# test RMSE of final model
gam_test_rmse = sqrt(mean(y_test - gam_pred) ^ 2)
gam_test_rmse
```

    ## [1] 230.6434

The training RMSE is1522.3948472 and the test RMSE is 230.6433701.  
Coefficients are not printed for smooth terms because each smooth term
has several coefficients corresponding to different basis functions. The
degrees of freedom of each term represent the complexity of the smooth
function.  
In the final model, `perc_alumni`, `grad_rate`, `room_board`, `enroll`,
`accept`, `f_undergrad`, and `expend` are the most significant terms.

## Multivariate Adaptive Regression spline (MARS)

``` r
set.seed(2570)

# generate all possible combinations of degree and prune
mars_grid = expand.grid(degree = 1:3,
                        nprune = 2:15)

# fit MARS model using all predictors
mars_fit = train(x = x_train,
                 y = y_train,
                 method = "earth",
                 tuneGrid = mars_grid,
                 trControl = ctrl)
```

    ## Loading required package: earth

    ## Loading required package: Formula

    ## Loading required package: plotmo

    ## Loading required package: plotrix

    ## Loading required package: TeachingDemos

``` r
# plot results
plot(mars_fit)
```

![](ds2_hw2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
mars_fit$bestTune
```

    ##    nprune degree
    ## 14     15      1

``` r
summary(mars_fit$finalModel)
```

    ## Call: earth(x=matrix[453,16], y=c(12280,11250,1...), keepxy=TRUE, degree=1,
    ##             nprune=15)
    ## 
    ##                     coefficients
    ## (Intercept)           2181.94917
    ## h(apps-3712)             0.38773
    ## h(2314-accept)          -1.87906
    ## h(868-enroll)            4.70942
    ## h(1280-f_undergrad)     -1.59036
    ## h(f_undergrad-1280)     -0.36345
    ## h(4300-room_board)      -1.09650
    ## h(room_board-4300)       0.37171
    ## h(1300-personal)         0.79639
    ## h(ph_d-81)              49.78356
    ## h(31-perc_alumni)      -48.08623
    ## h(expend-5480)           1.44977
    ## h(15387-expend)          0.78374
    ## h(expend-15387)         -1.45751
    ## h(83-grad_rate)        -20.95025
    ## 
    ## Selected 15 of 22 terms, and 10 of 16 predictors (nprune=15)
    ## Termination condition: RSq changed by less than 0.001 at 22 terms
    ## Importance: expend, room_board, perc_alumni, accept, f_undergrad, apps, ...
    ## Number of terms at each degree of interaction: 1 14 (additive model)
    ## GCV 2923771    RSS 1160317664    GRSq 0.7880689    RSq 0.8135125

``` r
coef(mars_fit$finalModel)
```

    ##         (Intercept)     h(expend-15387)     h(15387-expend)     h(83-grad_rate) 
    ##        2181.9491708          -1.4575067           0.7837399         -20.9502461 
    ##  h(room_board-4300)  h(4300-room_board) h(f_undergrad-1280) h(1280-f_undergrad) 
    ##           0.3717076          -1.0965044          -0.3634485          -1.5903560 
    ##        h(apps-3712)   h(31-perc_alumni)    h(1300-personal)      h(expend-5480) 
    ##           0.3877255         -48.0862323           0.7963949           1.4497698 
    ##          h(ph_d-81)       h(868-enroll)      h(2314-accept) 
    ##          49.7835646           4.7094205          -1.8790578

``` r
# train RMSE of final model
mars_train_rmse = sqrt(mean((y_train - predict(mars_fit)) ^ 2))
mars_train_rmse
```

    ## [1] 1600.44

``` r
# make predictions
mars_pred = predict(mars_fit, x_test)

# test RMSE of final model
mars_test_rmse = sqrt(mean(y_test - gam_pred) ^ 2)
mars_test_rmse
```

    ## [1] 230.6434

The training RMSE is1600.4398292 and the test RMSE is 230.6433701.  
The final model’s maximum degree of interactions is 1, which means the
final model is an additive model. `nprune` is 13, which means there are
13 terms in the final model, including intercept.  
The most important terms in the final model are `expend`, `room_board`,
`perc_alumni`, `accept`, and `enroll`.

## 
