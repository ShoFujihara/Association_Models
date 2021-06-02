# 第2章





```r
library(tidyverse)
library(magrittr)
library(broom)
library(gnm)
```


## 表2.1 (p.8)

- クロス表に周辺度数を追加する場合は，`addmargins`を用いる．


```r
# default
options(contrasts = c(factor = "contr.treatment", ordered = "contr.poly"))

Freq <- c( 40, 250,
          160,3000)
tab_2.1 <- matrix(Freq, nrow = 2, ncol = 2, byrow = TRUE) %>% as.table()

# A. 度数（周辺度数の追加）
tab_2.1 %>% addmargins()
```

```
       A    B  Sum
A     40  250  290
B    160 3000 3160
Sum  200 3250 3450
```

```r
# B. Odds 
40/250
```

```
[1] 0.16
```

```r
160/3000
```

```
[1] 0.05333333
```

```r
# C. Odds ratio
(40 * 3000) / (250 * 160)
```

```
[1] 3
```

```r
# 度数のベクトル
Freq
```

```
[1]   40  250  160 3000
```

```r
# 行変数
COMM <- gl(n = 2, k = 2)
COMM
```

```
[1] 1 1 2 2
Levels: 1 2
```

```r
# 列変数
SUP  <- gl(n = 2, k = 1, length = 4)
SUP
```

```
[1] 1 2 1 2
Levels: 1 2
```

```r
# 度数，行変数，列変数からなるデータを作成
freq_tab_2.1 <- tibble(Freq, COMM, SUP)
# データの確認
freq_tab_2.1
```

```
# A tibble: 4 x 3
   Freq COMM  SUP  
  <dbl> <fct> <fct>
1    40 1     1    
2   250 1     2    
3   160 2     1    
4  3000 2     2    
```

```r
# gnmで推定
fit <- freq_tab_2.1 %>% 
  gnm(Freq ~ COMM + SUP + COMM*SUP, data = ., family = poisson)
summary(fit)
```

```

Call:
gnm(formula = Freq ~ COMM + SUP + COMM * SUP, family = poisson, 
    data = .)

Deviance Residuals: 
[1]  0  0  0  0

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)   3.6889     0.1581  23.331  < 2e-16 ***
COMM2         1.3863     0.1768   7.842  < 2e-16 ***
SUP2          1.8326     0.1703  10.761  < 2e-16 ***
COMM2:SUP2    1.0986     0.1886   5.824 5.75e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Residual deviance: 1.2257e-13 on 0 degrees of freedom
AIC: 37.649

Number of iterations: 2
```

```r
# Odds ratios
fit$coefficients[["COMM2:SUP2"]] %>% exp()
```

```
[1] 3
```


## 表2.3 (p.32)

```r
Freq <- c( 39, 50, 18,  4,
          140,178, 85, 23,
          108,195, 97, 23,
          238,598,363,111,
           78,250,150, 55,
           50,200,208, 74,
            8, 29, 46, 21)

# コントラスト
options('contrasts')
```

```
$contrasts
           factor           ordered 
"contr.treatment"      "contr.poly" 
```

```r
# default
options(contrasts = c(factor = "contr.treatment", ordered = "contr.poly"))

# データを表形式に変換
tab_2.3A <- matrix(Freq, nrow = 7, ncol = 4, byrow = TRUE) %>% as.table()
rownames(tab_2.3A) <- c("Strongly liberal",
                        "Liberal",
                        "Slightly liberal",
                        "Moderate",
                        "Slightly conservative", 
                        "Conservative",
                        "Strongly conservative")
colnames(tab_2.3A) <- c("Strongly Disagree",
                        "Disagree",
                        "Agree",
                        "Strongly agree")
tab_2.3A
```

```
                      Strongly Disagree Disagree Agree Strongly agree
Strongly liberal                     39       50    18              4
Liberal                             140      178    85             23
Slightly liberal                    108      195    97             23
Moderate                            238      598   363            111
Slightly conservative                78      250   150             55
Conservative                         50      200   208             74
Strongly conservative                 8       29    46             21
```

```r
# 度数，行変数，列変数からなる集計データを作成
polviews <- gl(n = 7, k = 4)
fefam <- gl(n = 4, k = 1, length = 28)
freq_tab_2.3A <- tibble(Freq, polviews, fefam)
freq_tab_2.3A
```

```
# A tibble: 28 x 3
    Freq polviews fefam
   <dbl> <fct>    <fct>
 1    39 1        1    
 2    50 1        2    
 3    18 1        3    
 4     4 1        4    
 5   140 2        1    
 6   178 2        2    
 7    85 2        3    
 8    23 2        4    
 9   108 3        1    
10   195 3        2    
# … with 18 more rows
```

```r
# 行変数と列変数の整数値を作成
freq_tab_2.3A %<>% 
  mutate(Rscore = as.numeric(polviews),
         Cscore = as.numeric(fefam))
freq_tab_2.3A
```

```
# A tibble: 28 x 5
    Freq polviews fefam Rscore Cscore
   <dbl> <fct>    <fct>  <dbl>  <dbl>
 1    39 1        1          1      1
 2    50 1        2          1      2
 3    18 1        3          1      3
 4     4 1        4          1      4
 5   140 2        1          2      1
 6   178 2        2          2      2
 7    85 2        3          2      3
 8    23 2        4          2      4
 9   108 3        1          3      1
10   195 3        2          3      2
# … with 18 more rows
```

- モデル適合度を表示するための関数


```r
model.summary <- function(obj){
  aic <- obj$deviance - obj$df * 2 # AIC(L2)
  bic <- obj$deviance - obj$df * log(sum(obj$y)) #BIC(L2)
  delta <- 100 * sum(abs(obj$y - obj$fitted.values)) / (2 * sum(obj$y))
  p <- 1- pchisq(obj$deviance, obj$df, lower.tail = F)     #p<-ifelse(p<0.001,"<0.001",p)
  result <- matrix(0, 1, 7)
  Model <- deparse(substitute(obj))
  result <- tibble("Model Description" = Model,
              "df" = obj$df, 
              "L2" = obj$deviance, 
              #"AIC(L2)" = aic, 
              "BIC" = bic, 
              "Delta" = delta, 
              "p" = p)
  return(result)
  }
```


- 独立モデル


```r
#  1. O: Independence/Null Association Model
O <- freq_tab_2.3A %>% gnm(Freq ~ polviews + fefam, 
                           family = poisson, 
                           data = ., 
                           tolerance = 1e-12)
summary(O)
```

```

Call:
gnm(formula = Freq ~ polviews + fefam, family = poisson, data = ., 
    tolerance = 1e-12)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-5.7415  -2.5344  -0.4283   1.5557   5.8265  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  3.06035    0.10115  30.256  < 2e-16 ***
polviews2    1.34491    0.10657  12.620  < 2e-16 ***
polviews3    1.33784    0.10664  12.545  < 2e-16 ***
polviews4    2.46825    0.09886  24.968  < 2e-16 ***
polviews5    1.56899    0.10433  15.038  < 2e-16 ***
polviews6    1.56711    0.10435  15.018  < 2e-16 ***
polviews7   -0.06514    0.13647  -0.477    0.633    
fefam2       0.81947    0.04669  17.553  < 2e-16 ***
fefam3       0.38044    0.05047   7.538 4.76e-14 ***
fefam4      -0.75396    0.06876 -10.965  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Residual deviance: 211.7 on 18 degrees of freedom
AIC: 403.05

Number of iterations: 5
```

```r
model.summary(O)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 O                      18  212.  65.1  8.09     1
```


- 一様連関モデル


```r
# 2. U: Uniform Association Model
U <- freq_tab_2.3A %>% gnm(Freq ~ polviews + fefam + Rscore:Cscore, 
                           family = poisson, 
                           data = ., 
                           tolerance = 1e-12)
summary(U)
```

```

Call:
gnm(formula = Freq ~ polviews + fefam + Rscore:Cscore, family = poisson, 
    data = ., tolerance = 1e-12)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.62563  -0.41050  -0.06989   0.53746   1.67998  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)    3.47979    0.10109  34.422  < 2e-16 ***
polviews2      0.95906    0.10934   8.771  < 2e-16 ***
polviews3      0.53993    0.11884   4.543 5.54e-06 ***
polviews4      1.23013    0.12993   9.468  < 2e-16 ***
polviews5     -0.13911    0.15936  -0.873 0.382733    
polviews6     -0.64184    0.19196  -3.344 0.000827 ***
polviews7     -2.80636    0.24740 -11.344  < 2e-16 ***
fefam2         0.05148    0.07139   0.721 0.470857    
fefam3        -1.23188    0.12922  -9.533  < 2e-16 ***
fefam4        -3.28436    0.20609 -15.936  < 2e-16 ***
Rscore:Cscore  0.20211    0.01520  13.299  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Residual deviance: 20.125 on 17 degrees of freedom
AIC: 213.48

Number of iterations: 4
```

```r
model.summary(U)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 U                      17  20.1 -118.  2.77 0.732
```


- 行効果モデル

```r
# 3. R: Row Effect Model
# contrast
options(contrasts = c(factor = "contr.sum", ordered = "contr.treatment"))
R <- freq_tab_2.3A %>%
  gnm(Freq ~ polviews + fefam + Cscore*polviews, 
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
summary(R)
```

```

Call:

gnm(formula = Freq ~ polviews + fefam + Cscore * polviews, family = poisson, 
    data = ., tolerance = 1e-12)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.5962  -0.4603   0.0477   0.3840   1.5700  

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)       4.262045   0.029618 143.902  < 2e-16 ***
polviews1         0.040378   0.222896   0.181   0.8562    
polviews2         1.088684   0.130977   8.312  < 2e-16 ***
polviews3         0.763635   0.132427   5.766 8.09e-09 ***
polviews4         1.335170   0.096565  13.827  < 2e-16 ***
polviews5         0.197587   0.127059   1.555   0.1199    
polviews6        -0.558409   0.134998  -4.136 3.53e-05 ***
fefam1           -0.112887   0.046238  -2.441   0.0146 *  
fefam2            0.758505   0.030086  25.211  < 2e-16 ***
fefam3            0.295555   0.032630   9.058  < 2e-16 ***
Cscore            0.000000         NA      NA       NA    
polviews1:Cscore -0.558727   0.107668  -5.189 2.11e-07 ***
polviews2:Cscore -0.405076   0.059149  -6.848 7.47e-12 ***
polviews3:Cscore -0.248459   0.057741  -4.303 1.69e-05 ***
polviews4:Cscore  0.008764   0.039709   0.221   0.8253    
polviews5:Cscore  0.112245   0.051284   2.189   0.0286 *  
polviews6:Cscore  0.419079   0.051434   8.148  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 15.906 on 12 degrees of freedom
AIC: 219.26

Number of iterations: 4
```

```r
model.summary(R)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 R                      12  15.9 -81.8  2.47 0.804
```



```r
# alternative (default)
options(contrasts = c(factor = "contr.treatment", ordered = "contr.poly"))
Ralt <- freq_tab_2.3A %>%
  gnm(Freq ~ polviews + fefam + Cscore*polviews, 
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
summary(Ralt)
```

```

Call:

gnm(formula = Freq ~ polviews + fefam + Cscore * polviews, family = poisson, 
    data = ., tolerance = 1e-12)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.5962  -0.4603   0.0477   0.3840   1.5700  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)        3.6308     0.1443  25.160  < 2e-16 ***
polviews2          1.0483     0.2801   3.742 0.000182 ***
polviews3          0.7233     0.2818   2.567 0.010258 *  
polviews4          1.2948     0.2617   4.947 7.52e-07 ***
polviews5          0.1572     0.2798   0.562 0.574154    
polviews6         -0.5988     0.2859  -2.094 0.036244 *  
polviews7         -2.9074     0.4145  -7.015 2.30e-12 ***
fefam2             0.3127     0.1265   2.471 0.013455 *  
fefam3            -0.7090     0.2447  -2.897 0.003763 ** 
fefam4            -2.5045     0.3703  -6.763 1.36e-11 ***
Cscore             0.0000         NA      NA       NA    
polviews2:Cscore   0.1537     0.1362   1.128 0.259246    
polviews3:Cscore   0.3103     0.1357   2.287 0.022191 *  
polviews4:Cscore   0.5675     0.1268   4.477 7.58e-06 ***
polviews5:Cscore   0.6710     0.1326   5.059 4.21e-07 ***
polviews6:Cscore   0.9778     0.1331   7.346 2.04e-13 ***
polviews7:Cscore   1.2309     0.1678   7.335 2.22e-13 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 15.906 on 12 degrees of freedom
AIC: 219.26

Number of iterations: 4
```

```r
model.summary(Ralt)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 Ralt                   12  15.9 -81.8  2.47 0.804
```


- 列効果モデル


```r
# 4. C: Column Effect Model
# contrast
options(contrasts = c(factor = "contr.sum", ordered = "contr.treatment"))
C <- freq_tab_2.3A %>% 
  gnm(Freq ~ polviews + fefam + Rscore*fefam, family = poisson, 
      data = ., 
      tolerance = 1e-12)
summary(C)
```

```

Call:

gnm(formula = Freq ~ polviews + fefam + Rscore * fefam, family = poisson, 
    data = ., tolerance = 1e-12)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.7572  -0.4920   0.1628   0.4922   1.4247  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)    4.26735    0.02842 150.161  < 2e-16 ***
polviews1     -1.34764    0.08885 -15.167  < 2e-16 ***
polviews2      0.10725    0.05058   2.120 0.033978 *  
polviews3      0.17817    0.04852   3.672 0.000241 ***
polviews4      1.35465    0.03449  39.280  < 2e-16 ***
polviews5      0.47060    0.04547  10.349  < 2e-16 ***
polviews6      0.45489    0.04714   9.650  < 2e-16 ***
fefam1         1.16938    0.10545  11.089  < 2e-16 ***
fefam2         1.01794    0.08835  11.521  < 2e-16 ***
fefam3        -0.27421    0.10365  -2.646 0.008155 ** 
Rscore         0.00000         NA      NA       NA    
fefam1:Rscore -0.32183    0.02601 -12.373  < 2e-16 ***
fefam2:Rscore -0.06518    0.02013  -3.238 0.001203 ** 
fefam3:Rscore  0.13740    0.02282   6.021 1.73e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 14.237 on 15 degrees of freedom
AIC: 211.59

Number of iterations: 4
```

```r
model.summary(C)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 C                      15  14.2 -108.  2.32 0.492
```



```r
# alternative (default)
options(contrasts = c(factor = "contr.treatment", ordered = "contr.poly"))
Calt <- freq_tab_2.3A %>% 
  gnm(Freq ~ polviews + fefam + Rscore*fefam, family = poisson, data = ., tolerance = 1e-12)
summary(Calt)
```

```

Call:

gnm(formula = Freq ~ polviews + fefam + Rscore * fefam, family = poisson, 
    data = ., tolerance = 1e-12)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.7572  -0.4920   0.1628   0.4922   1.4247  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)    3.76726    0.11222  33.571  < 2e-16 ***
polviews2      1.13306    0.10785  10.506  < 2e-16 ***
polviews3      0.88215    0.11310   7.800  < 2e-16 ***
polviews4      1.73680    0.11708  14.834  < 2e-16 ***
polviews5      0.53092    0.13848   3.834 0.000126 ***
polviews6      0.19337    0.16048   1.205 0.228239    
polviews7     -1.80128    0.20632  -8.730  < 2e-16 ***
fefam2        -0.15144    0.13618  -1.112 0.266095    
fefam3        -1.44359    0.15929  -9.063  < 2e-16 ***
fefam4        -3.08249    0.23741 -12.984  < 2e-16 ***
Rscore         0.00000         NA      NA       NA    
fefam2:Rscore  0.25665    0.03459   7.421 1.17e-13 ***
fefam3:Rscore  0.45924    0.03851  11.926  < 2e-16 ***
fefam4:Rscore  0.57144    0.05297  10.788  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 14.237 on 15 degrees of freedom
AIC: 211.59

Number of iterations: 4
```

```r
model.summary(Calt)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 Calt                   15  14.2 -108.  2.32 0.492
```

- 行・列効果モデル（$R+C$）

```r
# コントラスト
options(contrasts = c(factor = "contr.treatment", ordered = "contr.treatment"))

# 5. R+C: Row and Column Effect Model
# 収束しない
RplusCno <- freq_tab_2.3A %>% 
  gnm(Freq ~ polviews + fefam + Cscore:polviews + Rscore:fefam,
      family = poisson, 
      data = ., 
      tolerance = 1e-12)

# 変数と係数と係数の順番を表示
data.frame(var = names(RplusCno$coefficients),
           estimate = RplusCno$coefficients) %>% 
  mutate(estimate = estimate,
         number = row_number())
```

```
                              var      estimate number
(Intercept)           (Intercept) -1.080034e+13      1
polviews2               polviews2  1.440045e+13      2
polviews3               polviews3  2.880091e+13      3
polviews4               polviews4  4.320136e+13      4
polviews5               polviews5  5.760181e+13      5
polviews6               polviews6  7.200226e+13      6
polviews7               polviews7  8.640272e+13      7
fefam2                     fefam2 -2.520079e+13      8
fefam3                     fefam3 -5.040159e+13      9
fefam4                     fefam4 -7.560238e+13     10
polviews1:Cscore polviews1:Cscore  2.160068e+13     11
polviews2:Cscore polviews2:Cscore  1.800057e+13     12
polviews3:Cscore polviews3:Cscore  1.440045e+13     13
polviews4:Cscore polviews4:Cscore  1.080034e+13     14
polviews5:Cscore polviews5:Cscore  7.200226e+12     15
polviews6:Cscore polviews6:Cscore  3.600113e+12     16
polviews7:Cscore polviews7:Cscore            NA     17
fefam1:Rscore       fefam1:Rscore -1.080034e+13     18
fefam2:Rscore       fefam2:Rscore -7.200226e+12     19
fefam3:Rscore       fefam3:Rscore -3.600113e+12     20
fefam4:Rscore       fefam4:Rscore            NA     21
```

```r
# polviews1:Cscore(11) = polviews7:Cscore(17) = fefam1:Rscore(18) = 0
RplusC <- freq_tab_2.3A %>% 
  gnm(Freq ~ polviews + fefam + Cscore:polviews + Rscore:fefam,
      constrain = c(11,17,18), 
      constrainTo = c(0, 0, 0),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
summary(RplusC)
```

```

Call:
gnm(formula = Freq ~ polviews + fefam + Cscore:polviews + Rscore:fefam, 
    constrain = c(11, 17, 18), constrainTo = c(0, 0, 0), family = poisson, 
    data = ., tolerance = 1e-12)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.07536  -0.33353   0.01859   0.37548   1.05912  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)       3.69896    0.14301  25.866  < 2e-16 ***
polviews2         1.27521    0.25278   5.045 4.54e-07 ***
polviews3         1.15849    0.24328   4.762 1.92e-06 ***
polviews4         1.91613    0.21048   9.104  < 2e-16 ***
polviews5         0.92939    0.22891   4.060 4.91e-05 ***
polviews6         0.26063    0.24331   1.071   0.2841    
polviews7        -2.01888    0.30512  -6.617 3.67e-11 ***
fefam2           -0.11089    0.17635  -0.629   0.5295    
fefam3           -1.28476    0.28350  -4.532 5.85e-06 ***
fefam4           -2.75218    0.43313  -6.354 2.10e-10 ***
polviews1:Cscore  0.00000         NA      NA       NA    
polviews2:Cscore -0.08578    0.11437  -0.750   0.4532    
polviews3:Cscore -0.15923    0.10585  -1.504   0.1325    
polviews4:Cscore -0.12164    0.09246  -1.316   0.1883    
polviews5:Cscore -0.22243    0.10510  -2.116   0.0343 *  
polviews6:Cscore -0.09452    0.11454  -0.825   0.4092    
polviews7:Cscore  0.00000         NA      NA       NA    
fefam1:Rscore     0.00000         NA      NA       NA    
fefam2:Rscore     0.27936    0.04372   6.389 1.67e-10 ***
fefam3:Rscore     0.48417    0.06239   7.761  < 2e-16 ***
fefam4:Rscore     0.58586    0.08655   6.769 1.30e-11 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 7.6783 on 10 degrees of freedom
AIC: 215.03

Number of iterations: 4
```

```r
model.summary(RplusC)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 RplusC                 10  7.68 -73.8  1.77 0.340
```

```r
# 5. R+C: Row and Column Effect Model (Alternative)
RplusCalt <- freq_tab_2.3A %>% 
  gnm(Freq ~ polviews + fefam + Rscore:Cscore + Cscore:polviews + Rscore:fefam,
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
RplusCalt
```

```

Call:

gnm(formula = Freq ~ polviews + fefam + Rscore:Cscore + Cscore:polviews + 
    Rscore:fefam, family = poisson, data = ., tolerance = 1e-12)

Coefficients:
     (Intercept)         polviews2         polviews3         polviews4  
         3.50368           1.07992           0.76791           1.33027  
       polviews5         polviews6         polviews7            fefam2  
         0.14824          -0.71581          -3.19061          -0.11089  
          fefam3            fefam4     Rscore:Cscore  polviews2:Cscore  
        -1.28476          -2.75218           0.19529          -0.08578  
polviews3:Cscore  polviews4:Cscore  polviews5:Cscore  polviews6:Cscore  
        -0.15923          -0.12164          -0.22243          -0.09452  
polviews7:Cscore     fefam2:Rscore     fefam3:Rscore     fefam4:Rscore  
              NA           0.08407           0.09360                NA  

Deviance:            7.678265 
Pearson chi-squared: 7.790921 
Residual df:         10 
```

```r
summary(RplusCalt)
```

```

Call:

gnm(formula = Freq ~ polviews + fefam + Rscore:Cscore + Cscore:polviews + 
    Rscore:fefam, family = poisson, data = ., tolerance = 1e-12)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.07536  -0.33353   0.01859   0.37548   1.05912  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)       3.50368    0.13171  26.601  < 2e-16 ***
polviews2         1.07992    0.26750   4.037 5.41e-05 ***
polviews3         0.76791    0.27072   2.837  0.00456 ** 
polviews4         1.33027    0.25273   5.264 1.41e-07 ***
polviews5         0.14824    0.27598   0.537  0.59116    
polviews6        -0.71581    0.29336  -2.440  0.01469 *  
polviews7        -3.19061    0.44608  -7.153 8.52e-13 ***
fefam2           -0.11089    0.17635  -0.629  0.52949    
fefam3           -1.28476    0.28350  -4.532 5.85e-06 ***
fefam4           -2.75218    0.43313  -6.354 2.10e-10 ***
Rscore:Cscore     0.19529    0.02885   6.769 1.30e-11 ***
polviews2:Cscore -0.08578    0.11437  -0.750  0.45321    
polviews3:Cscore -0.15923    0.10585  -1.504  0.13250    
polviews4:Cscore -0.12164    0.09246  -1.316  0.18831    
polviews5:Cscore -0.22243    0.10510  -2.116  0.03431 *  
polviews6:Cscore -0.09452    0.11454  -0.825  0.40923    
polviews7:Cscore  0.00000         NA      NA       NA    
fefam2:Rscore     0.08407    0.03224   2.607  0.00912 ** 
fefam3:Rscore     0.09360    0.03861   2.424  0.01535 *  
fefam4:Rscore     0.00000         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 7.6783 on 10 degrees of freedom
AIC: 215.03

Number of iterations: 4
```

```r
model.summary(RplusCalt)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 RplusCalt              10  7.68 -73.8  1.77 0.340
```


- 行・列効果モデル（$RC(1)$）

```r
# 6. RC: RC(1) model
RC.un <- freq_tab_2.3A %>%
  gnm(Freq ~ polviews + fefam + Mult(1,polviews,fefam),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations............................
Done
```

```r
summary(RC.un)
```

```

Call:
gnm(formula = Freq ~ polviews + fefam + Mult(1, polviews, fefam), 
    family = poisson, data = ., tolerance = 1e-12)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.07428  -0.27239   0.03038   0.36792   1.03721  

Coefficients:
                             Estimate Std. Error z value Pr(>|z|)
(Intercept)                   2.90136         NA      NA       NA
polviews2                     1.39045         NA      NA       NA
polviews3                     1.42691         NA      NA       NA
polviews4                     2.58617         NA      NA       NA
polviews5                     1.68548         NA      NA       NA
polviews6                     1.62513         NA      NA       NA
polviews7                    -0.11517         NA      NA       NA
fefam2                        0.89083         NA      NA       NA
fefam3                        0.41717         NA      NA       NA
fefam4                       -0.76393         NA      NA       NA
Mult(., polviews, fefam).    -0.09443         NA      NA       NA
Mult(1, ., fefam).polviews1  16.96714         NA      NA       NA
Mult(1, ., fefam).polviews2  13.11793         NA      NA       NA
Mult(1, ., fefam).polviews3   7.97749         NA      NA       NA
Mult(1, ., fefam).polviews4   0.04632         NA      NA       NA
Mult(1, ., fefam).polviews5  -3.36461         NA      NA       NA
Mult(1, ., fefam).polviews6 -14.51905         NA      NA       NA
Mult(1, ., fefam).polviews7 -24.05376         NA      NA       NA
Mult(1, polviews, .).fefam1  -0.48312         NA      NA       NA
Mult(1, polviews, .).fefam2  -0.06346         NA      NA       NA
Mult(1, polviews, .).fefam3   0.26367         NA      NA       NA
Mult(1, polviews, .).fefam4   0.41886         NA      NA       NA

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 8.0718 on 10 degrees of freedom
AIC: 215.43

Number of iterations: 28
```

```r
model.summary(RC.un)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 RC.un                  10  8.07 -73.4  1.77 0.378
```

```r
# mu[i], i = 1 to 7
mu <- getContrasts(RC.un, pickCoef(RC.un, "[.]polviews"), 
                   ref = "mean",
                   scaleRef = "mean", 
                   scaleWeights = "unit")
# nu[j], j = 1 to 4
nu <- getContrasts(RC.un, pickCoef(RC.un, "[.]fefam"), 
                   ref = "mean",
                   scaleRef = "mean",
                   scaleWeights = "unit")

con <- c(mu$qvframe[,1][c(1,7)], nu$qvframe[,1][c(1,4)])

set.seed(1234)
RC <- freq_tab_2.3A %>% 
  gnm(Freq ~ polviews + fefam + Mult(1,polviews,fefam), 
      constrain = c(12,18,19,22),
      constrainTo = con, 
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...............................
Done
```

```r
summary(RC)
```

```

Call:
gnm(formula = Freq ~ polviews + fefam + Mult(1, polviews, fefam), 
    constrain = c(12, 18, 19, 22), constrainTo = con, family = poisson, 
    data = ., tolerance = 1e-12)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.07428  -0.27239   0.03038   0.36792   1.03721  

Coefficients:
                            Estimate Std. Error z value Pr(>|z|)    
(Intercept)                  2.82020    0.14578  19.346  < 2e-16 ***
polviews2                    1.40280    0.12442  11.275  < 2e-16 ***
polviews3                    1.45577    0.12746  11.421  < 2e-16 ***
polviews4                    2.64048    0.13143  20.090  < 2e-16 ***
polviews5                    1.75073    0.14175  12.351  < 2e-16 ***
polviews6                    1.72618    0.16727  10.320  < 2e-16 ***
polviews7                    0.01648    0.21938   0.075 0.940107    
fefam2                       0.91251    0.12222   7.466 8.25e-14 ***
fefam3                       0.45574    0.20388   2.235 0.025396 *  
fefam4                      -0.71735    0.24913  -2.879 0.003985 ** 
Mult(., polviews, fefam).   -2.37331    0.34813  -6.817 9.27e-12 ***
Mult(1, ., fefam).polviews1  0.48165         NA      NA       NA    
Mult(1, ., fefam).polviews2  0.37580    0.10049   3.740 0.000184 ***
Mult(1, ., fefam).polviews3  0.23443    0.09461   2.478 0.013216 *  
Mult(1, ., fefam).polviews4  0.01632    0.08366   0.195 0.845376    
Mult(1, ., fefam).polviews5 -0.07749    0.09263  -0.837 0.402872    
Mult(1, ., fefam).polviews6 -0.38425    0.10926  -3.517 0.000437 ***
Mult(1, ., fefam).polviews7 -0.64646         NA      NA       NA    
Mult(1, polviews, .).fefam1 -0.74812         NA      NA       NA    
Mult(1, polviews, .).fefam2 -0.14098    0.07469  -1.888 0.059090 .  
Mult(1, polviews, .).fefam3  0.33229    0.09162   3.627 0.000287 ***
Mult(1, polviews, .).fefam4  0.55680         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 8.0718 on 10 degrees of freedom
AIC: 215.43

Number of iterations: 31
```

```r
mu
```

```
                               Estimate Std. Error
Mult(1, ., fefam).polviews1  0.48165420 0.06776575
Mult(1, ., fefam).polviews2  0.37579722 0.05016783
Mult(1, ., fefam).polviews3  0.23443020 0.04842778
Mult(1, ., fefam).polviews4  0.01631519 0.03689088
Mult(1, ., fefam).polviews5 -0.07748893 0.04931050
Mult(1, ., fefam).polviews6 -0.38424697 0.06009815
Mult(1, ., fefam).polviews7 -0.64646091 0.06145462
```

```r
nu
```

```
                              Estimate Std. Error
Mult(1, polviews, .).fefam1 -0.7481178 0.02662158
Mult(1, polviews, .).fefam2 -0.1409760 0.04671343
Mult(1, polviews, .).fefam3  0.3322937 0.05164241
Mult(1, polviews, .).fefam4  0.5568002 0.04821776
```

```r
model.summary(RC)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 RC                     10  8.07 -73.4  1.77 0.378
```


## 表2.4A


```r
models <- list()
models[[1]] <- model.summary(O)
models[[2]] <- model.summary(U)
models[[3]] <- model.summary(R)
models[[4]] <- model.summary(C)
models[[5]] <- model.summary(RplusC)
models[[6]] <- model.summary(RC)
models %>% bind_rows()
```

```
# A tibble: 6 x 6
  `Model Description`    df     L2    BIC Delta     p
  <chr>               <int>  <dbl>  <dbl> <dbl> <dbl>
1 O                      18 212.     65.1  8.09 1    
2 U                      17  20.1  -118.   2.77 0.732
3 R                      12  15.9   -81.8  2.47 0.804
4 C                      15  14.2  -108.   2.32 0.492
5 RplusC                 10   7.68  -73.8  1.77 0.340
6 RC                     10   8.07  -73.4  1.77 0.378
```


## 表2.4B


```r
model_comparison <- function(x, y = 0) {
  models %>% bind_rows() %>%
    summarise(`Model Used` = 
                ifelse(y == 0,paste0(x),paste0(x,"-",y)),
              df = ifelse(y == 0, df[x], df[x] - df[y]),
              L2 = ifelse(y == 0, L2[x], L2[x] - L2[y]),
              p = pchisq(L2, df, lower.tail = FALSE))
}
```



```r
# Table 2.4 Panel B
bind_rows(model_comparison(1,2),
          model_comparison(2,6),
          model_comparison(6),
          model_comparison(1))
```

```
# A tibble: 4 x 4
  `Model Used`    df     L2        p
  <chr>        <int>  <dbl>    <dbl>
1 1-2              1 192.   1.44e-43
2 2-6              7  12.1  9.88e- 2
3 6               10   8.07 6.22e- 1
4 1               18 212.   4.54e-35
```



```r
# Table 2.4 Panel C
bind_rows(model_comparison(2,4),
          model_comparison(4,6),
          model_comparison(2,6))
```

```
# A tibble: 3 x 4
  `Model Used`    df    L2      p
  <chr>        <int> <dbl>  <dbl>
1 2-4              2  5.89 0.0527
2 4-6              5  6.17 0.290 
3 2-6              7 12.1  0.0988
```




## 表2.5A


```r
summary(U)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl("Rscore:Cscore", Variable))
```

```
       Variable  Estimate Std..Error  z.value     Pr...z..
1 Rscore:Cscore 0.2021125 0.01519706 13.29945 2.331718e-40
```

- 表2.5B

```r
# Table 2.5 Panel B Alternative
summary(R)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl(":Cscore", Variable))
```

```
          Variable     Estimate Std..Error    z.value     Pr...z..
1 polviews1:Cscore -0.558726563 0.10766840 -5.1893275 2.110549e-07
2 polviews2:Cscore -0.405075736 0.05914888 -6.8484091 7.467575e-12
3 polviews3:Cscore -0.248458921 0.05774110 -4.3029822 1.685144e-05
4 polviews4:Cscore  0.008763861 0.03970853  0.2207048 8.253223e-01
5 polviews5:Cscore  0.112245001 0.05128383  2.1887016 2.861853e-02
6 polviews6:Cscore  0.419078627 0.05143368  8.1479420 3.701697e-16
```

```r
# polviews7:Cscoreを求める
mycontrast <- numeric(length(coef(R)))
terms <- pickCoef(R,"[:]Cscore")
mycontrast[terms] <- rep(-1,6)
mycontrast <- cbind(mycontrast)
colnames(mycontrast) <- "polviews7:Cscore"
gnm::se(R, mycontrast)
```

```
                  Estimate Std. Error
polviews7:Cscore 0.6721737  0.1003211
```

- 表2.5Bの別の方法

```r
# Table 2.5 Panel B Alternative
summary(Ralt)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl(":Cscore", Variable))
```

```
          Variable  Estimate Std..Error  z.value     Pr...z..
1 polviews2:Cscore 0.1536508  0.1361941 1.128176 2.592458e-01
2 polviews3:Cscore 0.3102676  0.1356609 2.287082 2.219102e-02
3 polviews4:Cscore 0.5674904  0.1267654 4.476699 7.580590e-06
4 polviews5:Cscore 0.6709716  0.1326215 5.059297 4.208051e-07
5 polviews6:Cscore 0.9778052  0.1331060 7.346062 2.041319e-13
6 polviews7:Cscore 1.2309003  0.1678130 7.334954 2.217982e-13
```

- 表2.5C

```r
# Table 2.5 Panel C
summary(C)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl(":Rscore", Variable))
```

```
       Variable    Estimate Std..Error    z.value     Pr...z..
1 fefam1:Rscore -0.32183233 0.02601065 -12.373099 3.654449e-35
2 fefam2:Rscore -0.06518238 0.02012977  -3.238109 1.203247e-03
3 fefam3:Rscore  0.13740293 0.02281996   6.021174 1.731567e-09
```

```r
# fefam4:Rscoreを求める
mycontrast <- numeric(length(coef(C)))
terms <- pickCoef(C,"[:]Rscore")
mycontrast[terms] <- rep(-1,3)
mycontrast <- cbind(mycontrast)
colnames(mycontrast) <- "fefam4:Rscore"
gnm::se(C, mycontrast)
```

```
               Estimate Std. Error
fefam4:Rscore 0.2496118 0.03429697
```

- 表2.5Cの別の方法

```r
# Table 2.5 Panel C
summary(Calt)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl(":Rscore", Variable))
```

```
       Variable  Estimate Std..Error   z.value     Pr...z..
1 fefam2:Rscore 0.2566499 0.03458604  7.420624 1.165702e-13
2 fefam3:Rscore 0.4592353 0.03850703 11.926012 8.662502e-33
3 fefam4:Rscore 0.5714441 0.05297192 10.787680 3.935998e-27
```

- 表2.5R+C

```r
# Table 2.5 Panel D
summary(RplusC)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl("Rscore|Cscore", Variable))
```

```
           Variable    Estimate Std..Error    z.value     Pr...z..
1  polviews1:Cscore  0.00000000         NA         NA           NA
2  polviews2:Cscore -0.08578207 0.11436513 -0.7500719 4.532114e-01
3  polviews3:Cscore -0.15923097 0.10584890 -1.5043233 1.324981e-01
4  polviews4:Cscore -0.12164181 0.09246130 -1.3155970 1.883093e-01
5  polviews5:Cscore -0.22242867 0.10509647 -2.1164238 3.430878e-02
6  polviews6:Cscore -0.09452143 0.11453734 -0.8252455 4.092322e-01
7  polviews7:Cscore  0.00000000         NA         NA           NA
8     fefam1:Rscore  0.00000000         NA         NA           NA
9     fefam2:Rscore  0.27935717 0.04372418  6.3890770 1.668900e-10
10    fefam3:Rscore  0.48417258 0.06238681  7.7608170 8.438398e-15
11    fefam4:Rscore  0.58586364 0.08654948  6.7691179 1.295700e-11
```



- 表2.5R+Cの別の方法


```r
summary(RplusCalt)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl("Rscore|Cscore", Variable))
```

```
           Variable    Estimate Std..Error    z.value     Pr...z..
1     Rscore:Cscore  0.19528788 0.02884983  6.7691179 1.295700e-11
2  polviews2:Cscore -0.08578207 0.11436513 -0.7500719 4.532114e-01
3  polviews3:Cscore -0.15923097 0.10584890 -1.5043233 1.324981e-01
4  polviews4:Cscore -0.12164181 0.09246130 -1.3155970 1.883093e-01
5  polviews5:Cscore -0.22242867 0.10509647 -2.1164238 3.430878e-02
6  polviews6:Cscore -0.09452143 0.11453734 -0.8252455 4.092322e-01
7  polviews7:Cscore  0.00000000         NA         NA           NA
8     fefam2:Rscore  0.08406929 0.03224287  2.6073757 9.123918e-03
9     fefam3:Rscore  0.09359682 0.03861067  2.4241178 1.534563e-02
10    fefam4:Rscore  0.00000000         NA         NA           NA
```


- 表2.5RC


```r
summary(RC)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl("Mult", Variable))
```

```
                      Variable    Estimate Std..Error    z.value     Pr...z..
1    Mult(., polviews, fefam). -2.37330693 0.34812532 -6.8173924 9.270782e-12
2  Mult(1, ., fefam).polviews1  0.48165420         NA         NA           NA
3  Mult(1, ., fefam).polviews2  0.37579722 0.10049018  3.7396411 1.842832e-04
4  Mult(1, ., fefam).polviews3  0.23443020 0.09460850  2.4778980 1.321589e-02
5  Mult(1, ., fefam).polviews4  0.01631519 0.08365831  0.1950217 8.453760e-01
6  Mult(1, ., fefam).polviews5 -0.07748893 0.09263438 -0.8365030 4.028720e-01
7  Mult(1, ., fefam).polviews6 -0.38424697 0.10926427 -3.5166753 4.369880e-04
8  Mult(1, ., fefam).polviews7 -0.64646091         NA         NA           NA
9  Mult(1, polviews, .).fefam1 -0.74811781         NA         NA           NA
10 Mult(1, polviews, .).fefam2 -0.14097601 0.07468829 -1.8875250 5.908974e-02
11 Mult(1, polviews, .).fefam3  0.33229365 0.09161751  3.6269668 2.867702e-04
12 Mult(1, polviews, .).fefam4  0.55680017         NA         NA           NA
```

```r
mu
```

```
                               Estimate Std. Error
Mult(1, ., fefam).polviews1  0.48165420 0.06776575
Mult(1, ., fefam).polviews2  0.37579722 0.05016783
Mult(1, ., fefam).polviews3  0.23443020 0.04842778
Mult(1, ., fefam).polviews4  0.01631519 0.03689088
Mult(1, ., fefam).polviews5 -0.07748893 0.04931050
Mult(1, ., fefam).polviews6 -0.38424697 0.06009815
Mult(1, ., fefam).polviews7 -0.64646091 0.06145462
```

```r
nu
```

```
                              Estimate Std. Error
Mult(1, polviews, .).fefam1 -0.7481178 0.02662158
Mult(1, polviews, .).fefam2 -0.1409760 0.04671343
Mult(1, polviews, .).fefam3  0.3322937 0.05164241
Mult(1, polviews, .).fefam4  0.5568002 0.04821776
```

## logmultパッケージの利用
  

```r
library(logmult)
tab <- freq_tab_2.3A %>% xtabs(Freq ~ polviews + fefam, data = .)
anoas(tab)
```

```
Fitting independence model...
Fitting model with 1 dimension...
Initialising
Running start-up iterations..
Running main iterations............
Done
Fitting model with 2 dimensions...
Initialising
Running start-up iterations..
Running main iterations.............................
Done
Fitting model with 3 dimensions...
Initialising
Running start-up iterations..
Running main iterations...
Done
```

```
       Res. Df           Res. Dev    Dev./Indep. (%)   Dissim. (%)
Indep.      18 211.69508974175707 100.00000000000000 8.09171028965
RC(1)       10   8.07181892815001   3.81294575041711 1.76730792764
RC(2)        4   1.36909098846108   0.64672779615777 0.49639212425
RC(3)        0  -0.00000000000003  -0.00000000000001 0.00000000004
                      BIC                AIC Dev. Df
Indep.  65.12224155397629 175.69508974175707   NA NA
RC(1)  -73.35754117617265 -11.92818107184999 -204 -8
RC(2)  -31.20265305326799  -6.63090901153892   -7 -6
RC(3)   -0.00000000000003  -0.00000000000003   -1 -4
```

```r
rc_fit <- rc(tab, nd = 1, se = "bootstrap", weighting = "none",
             nreplicates = 100, ncpus = getOption("boot.ncpus")
             )
```

```
Initialising
Running start-up iterations..
Running main iterations.............
Done
Computing bootstrap standard errors...
.
```

```r
summary(rc_fit, weighting = "none")
```

```
Call:

rc(tab = tab, nd = 1, weighting = "none", se = "bootstrap", nreplicates = 100, 
    ncpus = getOption("boot.ncpus"))

Deviance Residuals:
    Min       1Q   Median       3Q      Max  
-1.0743  -0.2724   0.0304   0.3679   1.0372  

Association coefficients:
                Normalized   Adjusted  Std. error   Pr(>|z|)     
Dim1              2.373307         NA      0.2735  < 2.2e-16  ***
Dim1:polviews1    0.481654   0.742014      0.0732  4.834e-11  ***
Dim1:polviews2    0.375797   0.578936      0.0505  1.003e-13  ***
Dim1:polviews3    0.234430   0.361152      0.0544  1.609e-05  ***
Dim1:polviews4    0.016315   0.025134      0.0326   0.616651     
Dim1:polviews5   -0.077489  -0.119376      0.0492   0.114925     
Dim1:polviews6   -0.384247  -0.591953      0.0596  1.121e-10  ***
Dim1:polviews7   -0.646461  -0.995908      0.0621  < 2.2e-16  ***
Dim1:fefam1       0.748118   1.152516      0.0252  < 2.2e-16  ***
Dim1:fefam2       0.140976   0.217181      0.0470   0.002683  ** 
Dim1:fefam3      -0.332294  -0.511916      0.0583  1.214e-08  ***
Dim1:fefam4      -0.556800  -0.857781      0.0489  < 2.2e-16  ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Normalization weights: none
Deviance:              8.071819
Pearson chi-squared:   8.168769
Dissimilarity index:   1.767308%
Residual df:           10
BIC:                   -73.35754
AIC:                   -11.92818
```

```r
summary(rc_fit, weighting = "marginal")
```

```
Call:

rc(tab = tab, nd = 1, weighting = "none", se = "bootstrap", nreplicates = 100, 
    ncpus = getOption("boot.ncpus"))

Deviance Residuals:
    Min       1Q   Median       3Q      Max  
-1.0743  -0.2724   0.0304   0.3679   1.0372  

Association coefficients:
                Normalized  Adjusted
Dim1              0.256734        NA
Dim1:polviews1    1.820624    0.9225
Dim1:polviews2    1.415319    0.7171
Dim1:polviews3    0.874053    0.4429
Dim1:polviews4    0.038935    0.0197
Dim1:polviews5   -0.320222   -0.1623
Dim1:polviews6   -1.494737   -0.7574
Dim1:polviews7   -2.498701   -1.2661
Dim1:fefam1       1.657784    0.8400
Dim1:fefam2       0.191902    0.0972
Dim1:fefam3      -0.950760   -0.4817
Dim1:fefam4      -1.492808   -0.7564

No standard errors available
(jackknife and bootstrap disabled, or weighting changed since fitting).

Normalization weights: marginal
Deviance:              8.071819
Pearson chi-squared:   8.168769
Dissimilarity index:   1.767308%
Residual df:           10
BIC:                   -73.35754
AIC:                   -11.92818
```

```r
se(rc_fit)
```

```
$phi
          Dim1
[1,] 0.2735206

$row
, , 1

        Dim1
1 0.07324482
2 0.05050664
3 0.05435186
4 0.03259111
5 0.04915448
6 0.05957636
7 0.06206394


$col
, , 1

        Dim1
1 0.02523774
2 0.04696313
3 0.05832038
4 0.04885350
```



```r
# コントラスト
options('contrasts')
```

```
$contrasts
           factor           ordered 
"contr.treatment" "contr.treatment" 
```

```r
# default
options(contrasts = c(factor = "contr.treatment", ordered = "contr.poly"))

## 表2.3B
Freq <- c(518,  95, 6, 35, 5,
         　 81,  67, 4, 49, 2,
          452,1003,67,630, 5,
           71, 157,37,562,12)

# データを表形式に変換
tab_2.3B <- matrix(Freq, nrow = 4, ncol = 5, byrow = TRUE) %>% as.table()
rownames(tab_2.3B) <- c("College+",
                        "Junior college",
                        "High school",
                        "<High school")
colnames(tab_2.3B) <- c("Upper Nonmanual",
                        "Lower Nonmanual",
                        "Upper Manual",
                        "Lower Nonmanual",
                        "Farm")
tab_2.3B
```

```
               Upper Nonmanual Lower Nonmanual Upper Manual Lower Nonmanual
College+                   518              95            6              35
Junior college              81              67            4              49
High school                452            1003           67             630
<High school                71             157           37             562
               Farm
College+          5
Junior college    2
High school       5
<High school     12
```

```r
# 度数，行変数，列変数からなる集計データを作成
Educ <- gl(n = 4, k = 5)
Occ <- gl(n = 5, k = 1, length = 20)
freq_tab_2.3B <- tibble(Freq, Educ, Occ)
freq_tab_2.3B
```

```
# A tibble: 20 x 3
    Freq Educ  Occ  
   <dbl> <fct> <fct>
 1   518 1     1    
 2    95 1     2    
 3     6 1     3    
 4    35 1     4    
 5     5 1     5    
 6    81 2     1    
 7    67 2     2    
 8     4 2     3    
 9    49 2     4    
10     2 2     5    
11   452 3     1    
12  1003 3     2    
13    67 3     3    
14   630 3     4    
15     5 3     5    
16    71 4     1    
17   157 4     2    
18    37 4     3    
19   562 4     4    
20    12 4     5    
```

```r
# 行変数と列変数の整数値を作成
freq_tab_2.3B %<>% 
  mutate(Rscore = as.numeric(Educ),
         Cscore = as.numeric(Occ))
freq_tab_2.3B
```

```
# A tibble: 20 x 5
    Freq Educ  Occ   Rscore Cscore
   <dbl> <fct> <fct>  <dbl>  <dbl>
 1   518 1     1          1      1
 2    95 1     2          1      2
 3     6 1     3          1      3
 4    35 1     4          1      4
 5     5 1     5          1      5
 6    81 2     1          2      1
 7    67 2     2          2      2
 8     4 2     3          2      3
 9    49 2     4          2      4
10     2 2     5          2      5
11   452 3     1          3      1
12  1003 3     2          3      2
13    67 3     3          3      3
14   630 3     4          3      4
15     5 3     5          3      5
16    71 4     1          4      1
17   157 4     2          4      2
18    37 4     3          4      3
19   562 4     4          4      4
20    12 4     5          4      5
```



```r
O <- freq_tab_2.3B %>% 
  gnm(Freq ~ Educ + Occ, 
      family = poisson, 
      data = ., 
      tolerance = 1e-12)

U <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Rscore:Cscore, 
      family = poisson, 
      data = ., 
      tolerance = 1e-12)

R <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Rscore:Cscore + Educ:Cscore, 
      family = poisson, 
      data = ., 
      tolerance = 1e-12)

C <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Rscore:Cscore + Occ:Rscore, 
      family = poisson, 
      data = ., 
      tolerance = 1e-12)

RplusC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Rscore:Cscore + Educ:Cscore + Occ:Rscore,
      constrain = c(12,16), 
      constrainTo = c(0,0),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)

RC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Mult(1, Educ, Occ),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
..........................
Done
```

```r
UplusRC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Rscore:Cscore + Mult(1, Educ, Occ),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
............
Done
```

```r
RplusRC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Cscore:Educ + Mult(1, Educ, Occ),
      constrain = c(9,12),
      constrainTo = c(0,0),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.............................................
Done
```

```r
CplusRC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Rscore:Occ + Mult(1, Educ, Occ),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations....................................................
Done
```

```r
RplusCplusRC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Cscore:Educ + Rscore:Occ + Mult(1, Educ, Occ),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations..............
Done
```

```r
RC2 <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + instances(Mult(1, Educ, Occ),2),
      family = poisson, data = ., tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations........................
Done
```



## 表2.6A

```r
models <- list()
models[[1]] <- model.summary(O)
models[[2]] <- model.summary(U)
models[[3]] <- model.summary(R)
models[[4]] <- model.summary(C)
models[[5]] <- model.summary(RplusC)
models[[6]] <- model.summary(RC)
models[[7]] <- model.summary(UplusRC)
models[[8]] <- model.summary(RplusRC)
models[[9]] <- model.summary(CplusRC)
models[[10]] <- model.summary(RplusCplusRC)
models[[11]] <- model.summary(RC2)
models %>% bind_rows()
```

```
# A tibble: 11 x 6
   `Model Description`    df       L2    BIC   Delta     p
   <chr>               <int>    <dbl>  <dbl>   <dbl> <dbl>
 1 O                      12 1373.    1274.  23.9    1    
 2 U                      11  244.     153.   8.54   1    
 3 R                       9  206.     132.   7.38   1    
 4 C                       8  155.      89.3  7.47   1    
 5 RplusC                  6   91.6     42.1  4.63   1    
 6 RC                      6  125.      75.5  6.44   1    
 7 UplusRC                 5   17.6    -23.7  1.52   0.997
 8 RplusRC                 4    6.94   -26.1  0.832  0.861
 9 CplusRC                 3   11.4    -13.4  1.01   0.990
10 RplusCplusRC            2    0.278  -16.2  0.0538 0.130
11 RC2                     2    0.600  -15.9  0.0935 0.259
```

## 表2.6B


```r
# Table 2.4 Panel B
bind_rows(model_comparison(1,6),
          model_comparison(6,11),
          model_comparison(11),
          model_comparison(1))
```

```
# A tibble: 4 x 4
  `Model Used`    df       L2         p
  <chr>        <int>    <dbl>     <dbl>
1 1-6              6 1248.    1.84e-266
2 6-11             4  124.    5.95e- 26
3 11               2    0.600 7.41e-  1
4 1               12 1373.    8.44e-287
```


## 表2.6C


```r
# Table 2.4 Panel C
bind_rows(model_comparison(1,2),
          model_comparison(2,6),
          model_comparison(6,10),
          model_comparison(10),
          model_comparison(1))
```

```
# A tibble: 5 x 4
  `Model Used`    df       L2         p
  <chr>        <int>    <dbl>     <dbl>
1 1-2              1 1129.    1.52e-247
2 2-6              5  119.    5.22e- 24
3 6-10             4  125.    5.08e- 26
4 10               2    0.278 8.70e-  1
5 1               12 1373.    8.44e-287
```


- 表2.7A


```r
set.seed(1234)
UplusRC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Rscore:Cscore + Mult(1, Educ, Occ),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
......................
Done
```

```r
summary(UplusRC)
```

```

Call:
gnm(formula = Freq ~ Educ + Occ + Rscore:Cscore + Mult(1, Educ, 
    Occ), family = poisson, data = ., tolerance = 1e-12)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0893  -0.5631   0.1321   0.7372   1.9761  

Coefficients:
                       Estimate Std. Error z value Pr(>|z|)    
(Intercept)             5.49123         NA      NA       NA    
Educ2                  -2.07045         NA      NA       NA    
Educ3                  -1.00877         NA      NA       NA    
Educ4                  -3.55473         NA      NA       NA    
Occ2                   -1.06909         NA      NA       NA    
Occ3                   -5.17113         NA      NA       NA    
Occ4                   -4.53939         NA      NA       NA    
Occ5                  -10.91018         NA      NA       NA    
Rscore:Cscore           0.55212    0.03429    16.1   <2e-16 ***
Mult(., Educ, Occ).     0.05379         NA      NA       NA    
Mult(1, ., Occ).Educ1  57.38879         NA      NA       NA    
Mult(1, ., Occ).Educ2  28.47707         NA      NA       NA    
Mult(1, ., Occ).Educ3 -10.70009         NA      NA       NA    
Mult(1, ., Occ).Educ4   9.22301         NA      NA       NA    
Mult(1, Educ, .).Occ1   0.06067         NA      NA       NA    
Mult(1, Educ, .).Occ2  -0.31188         NA      NA       NA    
Mult(1, Educ, .).Occ3   0.01526         NA      NA       NA    
Mult(1, Educ, .).Occ4   0.19408         NA      NA       NA    
Mult(1, Educ, .).Occ5   1.34889         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 17.602 on 5 degrees of freedom
AIC: 163.98

Number of iterations: 79
```

```r
# 変数と係数と係数の順番を表示
data.frame(var = names(UplusRC$coefficients),
           estimate = UplusRC$coefficients) %>% 
  mutate(estimate = estimate,
         number = row_number())
```

```
                                        var     estimate number
(Intercept)                     (Intercept)   5.49123206      1
Educ2                                 Educ2  -2.07045035      2
Educ3                                 Educ3  -1.00877122      3
Educ4                                 Educ4  -3.55472593      4
Occ2                                   Occ2  -1.06909237      5
Occ3                                   Occ3  -5.17112951      6
Occ4                                   Occ4  -4.53939355      7
Occ5                                   Occ5 -10.91018443      8
Rscore:Cscore                 Rscore:Cscore   0.55211791      9
Mult(., Educ, Occ).     Mult(., Educ, Occ).   0.05378691     10
Mult(1, ., Occ).Educ1 Mult(1, ., Occ).Educ1  57.38878726     11
Mult(1, ., Occ).Educ2 Mult(1, ., Occ).Educ2  28.47707067     12
Mult(1, ., Occ).Educ3 Mult(1, ., Occ).Educ3 -10.70009243     13
Mult(1, ., Occ).Educ4 Mult(1, ., Occ).Educ4   9.22300612     14
Mult(1, Educ, .).Occ1 Mult(1, Educ, .).Occ1   0.06066883     15
Mult(1, Educ, .).Occ2 Mult(1, Educ, .).Occ2  -0.31187525     16
Mult(1, Educ, .).Occ3 Mult(1, Educ, .).Occ3   0.01525644     17
Mult(1, Educ, .).Occ4 Mult(1, Educ, .).Occ4   0.19407628     18
Mult(1, Educ, .).Occ5 Mult(1, Educ, .).Occ5   1.34889133     19
```

```r
mu <- getContrasts(UplusRC, pickCoef(UplusRC, "[.]Educ"), ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")
nu <- getContrasts(UplusRC, pickCoef(UplusRC, "[.]Occ"), ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")

summary(UplusRC)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl("Rscore|Cscore|Mult", Variable))
```

```
                Variable     Estimate Std..Error  z.value     Pr...z..
1          Rscore:Cscore   0.55211791 0.03429436 16.09938 2.576758e-58
2    Mult(., Educ, Occ).   0.05378691         NA       NA           NA
3  Mult(1, ., Occ).Educ1  57.38878726         NA       NA           NA
4  Mult(1, ., Occ).Educ2  28.47707067         NA       NA           NA
5  Mult(1, ., Occ).Educ3 -10.70009243         NA       NA           NA
6  Mult(1, ., Occ).Educ4   9.22300612         NA       NA           NA
7  Mult(1, Educ, .).Occ1   0.06066883         NA       NA           NA
8  Mult(1, Educ, .).Occ2  -0.31187525         NA       NA           NA
9  Mult(1, Educ, .).Occ3   0.01525644         NA       NA           NA
10 Mult(1, Educ, .).Occ4   0.19407628         NA       NA           NA
11 Mult(1, Educ, .).Occ5   1.34889133         NA       NA           NA
```

- 表2.7B


```r
RplusRC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Cscore:Educ + Mult(1, Educ, Occ),
      constrain = c(9,12),
      constrainTo = c(0,0),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations................................
Done
```

```r
summary(RplusRC)
```

```

Call:
gnm(formula = Freq ~ Educ + Occ + Cscore:Educ + Mult(1, Educ, 
    Occ), constrain = c(9, 12), constrainTo = c(0, 0), family = poisson, 
    data = ., tolerance = 1e-12)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-0.96462  -0.24301  -0.05231   0.31330   1.21311  

Coefficients:
                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)             5.1862         NA      NA       NA    
Educ2                  -0.5548         NA      NA       NA    
Educ3                   2.8279         NA      NA       NA    
Educ4                  -0.8642         NA      NA       NA    
Occ2                    0.6520         NA      NA       NA    
Occ3                   -1.1653         NA      NA       NA    
Occ4                    1.6136         NA      NA       NA    
Occ5                   -1.8048         NA      NA       NA    
Educ1:Cscore            0.0000         NA      NA       NA    
Educ2:Cscore           -0.4321     0.2210  -1.955   0.0506 .  
Educ3:Cscore           -1.2670     0.1931  -6.562 5.31e-11 ***
Educ4:Cscore            0.0000         NA      NA       NA    
Mult(., Educ, Occ).     5.4214         NA      NA       NA    
Mult(1, ., Occ).Educ1   1.6693         NA      NA       NA    
Mult(1, ., Occ).Educ2   0.3202         NA      NA       NA    
Mult(1, ., Occ).Educ3  -0.9820         NA      NA       NA    
Mult(1, ., Occ).Educ4  -0.2015         NA      NA       NA    
Mult(1, Educ, .).Occ1   0.1178         NA      NA       NA    
Mult(1, Educ, .).Occ2  -0.1448         NA      NA       NA    
Mult(1, Educ, .).Occ3  -0.2279         NA      NA       NA    
Mult(1, Educ, .).Occ4  -0.3551         NA      NA       NA    
Mult(1, Educ, .).Occ5  -0.2139         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 6.9359 on 4 degrees of freedom
AIC: 155.31

Number of iterations: 32
```

```r
summary(RplusRC)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl("Rscore|Cscore|Mult", Variable))
```

```
                Variable   Estimate Std..Error   z.value     Pr...z..
1           Educ1:Cscore  0.0000000         NA        NA           NA
2           Educ2:Cscore -0.4320760  0.2209849 -1.955228 5.055614e-02
3           Educ3:Cscore -1.2669500  0.1930731 -6.562024 5.308234e-11
4           Educ4:Cscore  0.0000000         NA        NA           NA
5    Mult(., Educ, Occ).  5.4213617         NA        NA           NA
6  Mult(1, ., Occ).Educ1  1.6693243         NA        NA           NA
7  Mult(1, ., Occ).Educ2  0.3202271         NA        NA           NA
8  Mult(1, ., Occ).Educ3 -0.9819762         NA        NA           NA
9  Mult(1, ., Occ).Educ4 -0.2015324         NA        NA           NA
10 Mult(1, Educ, .).Occ1  0.1177631         NA        NA           NA
11 Mult(1, Educ, .).Occ2 -0.1448198         NA        NA           NA
12 Mult(1, Educ, .).Occ3 -0.2278637         NA        NA           NA
13 Mult(1, Educ, .).Occ4 -0.3550915         NA        NA           NA
14 Mult(1, Educ, .).Occ5 -0.2138557         NA        NA           NA
```

```r
# 変数と係数と係数の順番を表示
data.frame(var = names(RplusRC$coefficients),
           estimate = RplusRC$coefficients) %>% 
  mutate(estimate = estimate,
         number = row_number())
```

```
                                        var   estimate number
(Intercept)                     (Intercept)  5.1861978      1
Educ2                                 Educ2 -0.5547571      2
Educ3                                 Educ3  2.8279123      3
Educ4                                 Educ4 -0.8642169      4
Occ2                                   Occ2  0.6520471      5
Occ3                                   Occ3 -1.1653253      6
Occ4                                   Occ4  1.6136412      7
Occ5                                   Occ5 -1.8047550      8
Educ1:Cscore                   Educ1:Cscore         NA      9
Educ2:Cscore                   Educ2:Cscore -0.4320760     10
Educ3:Cscore                   Educ3:Cscore -1.2669500     11
Educ4:Cscore                   Educ4:Cscore         NA     12
Mult(., Educ, Occ).     Mult(., Educ, Occ).  5.4213617     13
Mult(1, ., Occ).Educ1 Mult(1, ., Occ).Educ1  1.6693243     14
Mult(1, ., Occ).Educ2 Mult(1, ., Occ).Educ2  0.3202271     15
Mult(1, ., Occ).Educ3 Mult(1, ., Occ).Educ3 -0.9819762     16
Mult(1, ., Occ).Educ4 Mult(1, ., Occ).Educ4 -0.2015324     17
Mult(1, Educ, .).Occ1 Mult(1, Educ, .).Occ1  0.1177631     18
Mult(1, Educ, .).Occ2 Mult(1, Educ, .).Occ2 -0.1448198     19
Mult(1, Educ, .).Occ3 Mult(1, Educ, .).Occ3 -0.2278637     20
Mult(1, Educ, .).Occ4 Mult(1, Educ, .).Occ4 -0.3550915     21
Mult(1, Educ, .).Occ5 Mult(1, Educ, .).Occ5 -0.2138557     22
```

```r
mu <- getContrasts(RplusRC, pickCoef(RplusRC, "[.]Educ"), ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")
nu <- getContrasts(RplusRC, pickCoef(RplusRC, "[.]Occ"), ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")
con <- c(0,0,mu$qvframe[,1][c(1,4)],nu$qvframe[,1][c(1,5)])

RplusRC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Cscore:Educ + Mult(1, Educ, Occ),
      constrain = c(9,12,14,17,18,22),
      constrainTo = con,
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations............................................
Done
```

```r
summary(RplusRC)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl("Rscore|Cscore|Mult", Variable))
```

```
                Variable    Estimate Std..Error    z.value     Pr...z..
1           Educ1:Cscore  0.00000000         NA         NA           NA
2           Educ2:Cscore -0.43207600  0.2209849 -1.9552282 5.055614e-02
3           Educ3:Cscore -1.26695003  0.1930731 -6.5620238 5.308234e-11
4           Educ4:Cscore  0.00000000         NA         NA           NA
5    Mult(., Educ, Occ).  3.67046405  0.5474332  6.7048620 2.015968e-11
6  Mult(1, ., Occ).Educ1  0.75983648         NA         NA           NA
7  Mult(1, ., Occ).Educ2  0.06145542  0.1367987  0.4492399 6.532586e-01
8  Mult(1, ., Occ).Educ3 -0.61265036  0.1325427 -4.6222852 3.795357e-06
9  Mult(1, ., Occ).Educ4 -0.20864153         NA         NA           NA
10 Mult(1, Educ, .).Occ1  0.80614447         NA         NA           NA
11 Mult(1, Educ, .).Occ2  0.05693272  0.1225191  0.4646846 6.421573e-01
12 Mult(1, Educ, .).Occ3 -0.18001127  0.1630841 -1.1037941 2.696825e-01
13 Mult(1, Educ, .).Occ4 -0.54302264  0.2004941 -2.7084221 6.760398e-03
14 Mult(1, Educ, .).Occ5 -0.14004328         NA         NA           NA
```



- 表2.7C


```r
CplusRC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Rscore:Occ + Mult(1, Educ, Occ),
      constrain = c(9,13),
      constrainTo = c(0,0),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
.......
Done
```

```r
summary(CplusRC)
```

```

Call:
gnm(formula = Freq ~ Educ + Occ + Rscore:Occ + Mult(1, Educ, 
    Occ), constrain = c(9, 13), constrainTo = c(0, 0), family = poisson, 
    data = ., tolerance = 1e-12)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.98938  -0.14103  -0.01486   0.18280   2.20453  

Coefficients:
                      Estimate Std. Error z value Pr(>|z|)    
(Intercept)            6.20483         NA      NA       NA    
Educ2                 -1.63470         NA      NA       NA    
Educ3                 -0.14253         NA      NA       NA    
Educ4                 -2.15738         NA      NA       NA    
Occ2                  -2.72048         NA      NA       NA    
Occ3                  -5.73323         NA      NA       NA    
Occ4                  -3.94848         NA      NA       NA    
Occ5                  -4.95095         NA      NA       NA    
Occ1:Rscore            0.00000         NA      NA       NA    
Occ2:Rscore            1.26259    0.16970   7.440 1.00e-13 ***
Occ3:Rscore            1.28232    0.20937   6.125 9.08e-10 ***
Occ4:Rscore            1.41583    0.09054  15.638  < 2e-16 ***
Occ5:Rscore            0.00000         NA      NA       NA    
Mult(., Educ, Occ).   -0.23967         NA      NA       NA    
Mult(1, ., Occ).Educ1 -0.28499         NA      NA       NA    
Mult(1, ., Occ).Educ2 -0.42016         NA      NA       NA    
Mult(1, ., Occ).Educ3 -0.37097         NA      NA       NA    
Mult(1, ., Occ).Educ4 -2.25814         NA      NA       NA    
Mult(1, Educ, .).Occ1  0.37246         NA      NA       NA    
Mult(1, Educ, .).Occ2 -2.43809         NA      NA       NA    
Mult(1, Educ, .).Occ3  0.30887         NA      NA       NA    
Mult(1, Educ, .).Occ4  1.05400         NA      NA       NA    
Mult(1, Educ, .).Occ5  6.26622         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 11.405 on 3 degrees of freedom
AIC: 161.78

Number of iterations: 64
```

```r
summary(CplusRC)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl("Rscore|Cscore|Mult", Variable))
```

```
                Variable   Estimate Std..Error   z.value     Pr...z..
1            Occ1:Rscore  0.0000000         NA        NA           NA
2            Occ2:Rscore  1.2625901  0.1696974  7.440245 1.004990e-13
3            Occ3:Rscore  1.2823194  0.2093667  6.124752 9.082487e-10
4            Occ4:Rscore  1.4158283  0.0905404 15.637531 4.040916e-55
5            Occ5:Rscore  0.0000000         NA        NA           NA
6    Mult(., Educ, Occ). -0.2396739         NA        NA           NA
7  Mult(1, ., Occ).Educ1 -0.2849908         NA        NA           NA
8  Mult(1, ., Occ).Educ2 -0.4201625         NA        NA           NA
9  Mult(1, ., Occ).Educ3 -0.3709743         NA        NA           NA
10 Mult(1, ., Occ).Educ4 -2.2581369         NA        NA           NA
11 Mult(1, Educ, .).Occ1  0.3724646         NA        NA           NA
12 Mult(1, Educ, .).Occ2 -2.4380944         NA        NA           NA
13 Mult(1, Educ, .).Occ3  0.3088675         NA        NA           NA
14 Mult(1, Educ, .).Occ4  1.0540018         NA        NA           NA
15 Mult(1, Educ, .).Occ5  6.2662183         NA        NA           NA
```

```r
# 変数と係数と係数の順番を表示
data.frame(var = names(CplusRC$coefficients),
           estimate = CplusRC$coefficients) %>% 
  mutate(estimate = estimate,
         number = row_number())
```

```
                                        var   estimate number
(Intercept)                     (Intercept)  6.2048343      1
Educ2                                 Educ2 -1.6347033      2
Educ3                                 Educ3 -0.1425333      3
Educ4                                 Educ4 -2.1573812      4
Occ2                                   Occ2 -2.7204830      5
Occ3                                   Occ3 -5.7332333      6
Occ4                                   Occ4 -3.9484760      7
Occ5                                   Occ5 -4.9509503      8
Occ1:Rscore                     Occ1:Rscore         NA      9
Occ2:Rscore                     Occ2:Rscore  1.2625901     10
Occ3:Rscore                     Occ3:Rscore  1.2823194     11
Occ4:Rscore                     Occ4:Rscore  1.4158283     12
Occ5:Rscore                     Occ5:Rscore         NA     13
Mult(., Educ, Occ).     Mult(., Educ, Occ). -0.2396739     14
Mult(1, ., Occ).Educ1 Mult(1, ., Occ).Educ1 -0.2849908     15
Mult(1, ., Occ).Educ2 Mult(1, ., Occ).Educ2 -0.4201625     16
Mult(1, ., Occ).Educ3 Mult(1, ., Occ).Educ3 -0.3709743     17
Mult(1, ., Occ).Educ4 Mult(1, ., Occ).Educ4 -2.2581369     18
Mult(1, Educ, .).Occ1 Mult(1, Educ, .).Occ1  0.3724646     19
Mult(1, Educ, .).Occ2 Mult(1, Educ, .).Occ2 -2.4380944     20
Mult(1, Educ, .).Occ3 Mult(1, Educ, .).Occ3  0.3088675     21
Mult(1, Educ, .).Occ4 Mult(1, Educ, .).Occ4  1.0540018     22
Mult(1, Educ, .).Occ5 Mult(1, Educ, .).Occ5  6.2662183     23
```

```r
mu <- getContrasts(CplusRC, pickCoef(CplusRC, "[.]Educ"), ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")
nu <- getContrasts(CplusRC, pickCoef(CplusRC, "[.]Occ"), ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")
con <- c(0,0,mu$qvframe[,1][c(1,4)],nu$qvframe[,1][c(1,5)])

CplusRC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Rscore:Occ + Mult(1, Educ, Occ),
      constrain = c(9,13,15,18,19,23),
      constrainTo = con,
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
...............
Done
```

```r
summary(CplusRC)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl("Rscore|Cscore|Mult", Variable))
```

```
                Variable     Estimate Std..Error   z.value     Pr...z..
1            Occ1:Rscore  0.000000000         NA        NA           NA
2            Occ2:Rscore  1.262590058 0.16969738  7.440245 1.004990e-13
3            Occ3:Rscore  1.282319390 0.20936674  6.124752 9.082487e-10
4            Occ4:Rscore  1.415828283 0.09054040 15.637531 4.040916e-55
5            Occ5:Rscore  0.000000000         NA        NA           NA
6    Mult(., Educ, Occ). -2.509132039 0.47337666 -5.300498 1.154873e-07
7  Mult(1, ., Occ).Educ1  0.332914652         NA        NA           NA
8  Mult(1, ., Occ).Educ2  0.250882838 0.17043562  1.472009 1.410183e-01
9  Mult(1, ., Occ).Educ3  0.280733721 0.25875316  1.084948 2.779447e-01
10 Mult(1, ., Occ).Educ4 -0.864531211         NA        NA           NA
11 Mult(1, Educ, .).Occ1 -0.116510615         NA        NA           NA
12 Mult(1, Educ, .).Occ2 -0.558888360 0.15090381 -3.703607 2.125557e-04
13 Mult(1, Educ, .).Occ3 -0.126520705 0.12280113 -1.030289 3.028742e-01
14 Mult(1, Educ, .).Occ4 -0.009237675 0.06533009 -0.141400 8.875539e-01
15 Mult(1, Educ, .).Occ5  0.811157355         NA        NA           NA
```



- 表2.7D


```r
RplusCplusRC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Cscore:Educ + Rscore:Occ + Mult(1, Educ, Occ),
      constrain = c(9,12,13,14,17), constrainTo = c(0,0,0,0,0),
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................
Done
```

```r
# 変数と係数と係数の順番を表示
data.frame(var = names(CplusRC$coefficients),
           estimate = CplusRC$coefficients) %>% 
  mutate(estimate = estimate,
         number = row_number())
```

```
                                        var     estimate number
(Intercept)                     (Intercept)  6.132951036      1
Educ2                                 Educ2 -1.598655285      2
Educ3                                 Educ3 -0.119602932      3
Educ4                                 Educ4 -1.631176445      4
Occ2                                   Occ2 -3.281987830      5
Occ3                                   Occ3 -5.745938986      6
Occ4                                   Occ4 -3.812315688      7
Occ5                                   Occ5 -3.773472310      8
Occ1:Rscore                     Occ1:Rscore           NA      9
Occ2:Rscore                     Occ2:Rscore  1.262590058     10
Occ3:Rscore                     Occ3:Rscore  1.282319390     11
Occ4:Rscore                     Occ4:Rscore  1.415828283     12
Occ5:Rscore                     Occ5:Rscore           NA     13
Mult(., Educ, Occ).     Mult(., Educ, Occ). -2.509132039     14
Mult(1, ., Occ).Educ1 Mult(1, ., Occ).Educ1           NA     15
Mult(1, ., Occ).Educ2 Mult(1, ., Occ).Educ2  0.250882838     16
Mult(1, ., Occ).Educ3 Mult(1, ., Occ).Educ3  0.280733721     17
Mult(1, ., Occ).Educ4 Mult(1, ., Occ).Educ4           NA     18
Mult(1, Educ, .).Occ1 Mult(1, Educ, .).Occ1           NA     19
Mult(1, Educ, .).Occ2 Mult(1, Educ, .).Occ2 -0.558888360     20
Mult(1, Educ, .).Occ3 Mult(1, Educ, .).Occ3 -0.126520705     21
Mult(1, Educ, .).Occ4 Mult(1, Educ, .).Occ4 -0.009237675     22
Mult(1, Educ, .).Occ5 Mult(1, Educ, .).Occ5           NA     23
```

```r
mu <- getContrasts(RplusCplusRC, pickCoef(RplusCplusRC, "[.]Educ"), ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")
nu <- getContrasts(RplusCplusRC, pickCoef(RplusCplusRC, "[.]Occ"), ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")
con <- c(0,0,0,0,0,mu$qvframe[,1][c(1,4)],nu$qvframe[,1][c(1,5)])

RplusCplusRC <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + Cscore:Educ + Rscore:Occ + Mult(1, Educ, Occ),
      constrain = c(9,12,13,14,17,19,22,23,27), 
      constrainTo = con,
      family = poisson, 
      data = ., 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.............................
Done
```

```r
mu
```

```
                         Estimate Std. Error
Mult(1, ., Occ).Educ1 -0.76639941 0.03714637
Mult(1, ., Occ).Educ2 -0.05505913 0.08952794
Mult(1, ., Occ).Educ3  0.60073372 0.04744381
Mult(1, ., Occ).Educ4  0.22072481 0.06764890
```

```r
nu
```

```
                         Estimate Std. Error
Mult(1, Educ, .).Occ1  0.83832072 0.02535129
Mult(1, Educ, .).Occ2 -0.04397713 0.08855421
Mult(1, Educ, .).Occ3 -0.11946443 0.09158773
Mult(1, Educ, .).Occ4 -0.50065982 0.06550415
Mult(1, Educ, .).Occ5 -0.17421935 0.12622465
```

```r
summary(RplusCplusRC)$coefficients %>%
  data.frame() %>%
  rownames_to_column("Variable") %>%
  dplyr::filter(grepl("Rscore|Cscore|Mult", Variable))
```

```
                Variable    Estimate Std..Error    z.value     Pr...z..
1           Educ1:Cscore  0.00000000         NA         NA           NA
2           Educ2:Cscore -0.29016794  0.1835938 -1.5804884 1.139951e-01
3           Educ3:Cscore -0.95452353  0.2168230 -4.4023158 1.071015e-05
4           Educ4:Cscore  0.00000000         NA         NA           NA
5            Occ1:Rscore  0.00000000         NA         NA           NA
6            Occ2:Rscore  0.00000000         NA         NA           NA
7            Occ3:Rscore  0.39725281  0.1673266  2.3741161 1.759102e-02
8            Occ4:Rscore  0.32684292  0.1614939  2.0238715 4.298337e-02
9            Occ5:Rscore  0.00000000         NA         NA           NA
10   Mult(., Educ, Occ). -2.85715993  0.5343817 -5.3466654 8.958940e-08
11 Mult(1, ., Occ).Educ1 -0.76639941         NA         NA           NA
12 Mult(1, ., Occ).Educ2 -0.05505913  0.1291228 -0.4264091 6.698098e-01
13 Mult(1, ., Occ).Educ3  0.60073372  0.1297703  4.6292078 3.670672e-06
14 Mult(1, ., Occ).Educ4  0.22072481         NA         NA           NA
15 Mult(1, Educ, .).Occ1  0.83832072         NA         NA           NA
16 Mult(1, Educ, .).Occ2 -0.04397713  0.1677272 -0.2621944 7.931715e-01
17 Mult(1, Educ, .).Occ3 -0.11946443  0.1842187 -0.6484923 5.166666e-01
18 Mult(1, Educ, .).Occ4 -0.50065982  0.2149854 -2.3288081 1.986923e-02
19 Mult(1, Educ, .).Occ5 -0.17421935         NA         NA           NA
```


- 表2.7E


```r
RC2 <- freq_tab_2.3B %>%
  gnm(Freq ~ Educ + Occ + instances(Mult(Educ, Occ), instances = 2),
      family = poisson, data = ., tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations......................
Done
```

```r
summary(RC2)
```

```

Call:
gnm(formula = Freq ~ Educ + Occ + instances(Mult(Educ, Occ), 
    instances = 2), family = poisson, data = ., tolerance = 1e-12)

Deviance Residuals: 
      Min         1Q     Median         3Q        Max  
-0.631356  -0.030234  -0.004363   0.050323   0.320666  

Coefficients:
                             Estimate Std. Error z value Pr(>|z|)
(Intercept)                   5.03021         NA      NA       NA
Educ2                        -0.86903         NA      NA       NA
Educ3                         1.13486         NA      NA       NA
Educ4                         0.32082         NA      NA       NA
Occ2                          0.37850         NA      NA       NA
Occ3                         -2.16443         NA      NA       NA
Occ4                          0.09540         NA      NA       NA
Occ5                         -4.07248         NA      NA       NA
Mult(., Occ, inst = 1).Educ1 -2.78979         NA      NA       NA
Mult(., Occ, inst = 1).Educ2 -0.31931         NA      NA       NA
Mult(., Occ, inst = 1).Educ3 -0.28793         NA      NA       NA
Mult(., Occ, inst = 1).Educ4  4.24751         NA      NA       NA
Mult(Educ, ., inst = 1).Occ1 -0.28722         NA      NA       NA
Mult(Educ, ., inst = 1).Occ2 -0.07948         NA      NA       NA
Mult(Educ, ., inst = 1).Occ3  0.15279         NA      NA       NA
Mult(Educ, ., inst = 1).Occ4  0.26840         NA      NA       NA
Mult(Educ, ., inst = 1).Occ5  0.19547         NA      NA       NA
Mult(., Occ, inst = 2).Educ1  3.60546         NA      NA       NA
Mult(., Occ, inst = 2).Educ2  1.26355         NA      NA       NA
Mult(., Occ, inst = 2).Educ3 -1.15902         NA      NA       NA
Mult(., Occ, inst = 2).Educ4  1.12408         NA      NA       NA
Mult(Educ, ., inst = 2).Occ1  0.11598         NA      NA       NA
Mult(Educ, ., inst = 2).Occ2 -0.29739         NA      NA       NA
Mult(Educ, ., inst = 2).Occ3 -0.20716         NA      NA       NA
Mult(Educ, ., inst = 2).Occ4 -0.22669         NA      NA       NA
Mult(Educ, ., inst = 2).Occ5  0.34128         NA      NA       NA

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 0.6001 on 2 degrees of freedom
AIC: 152.98

Number of iterations: 22
```

```r
RCmodel_coef <- function(RCmodel, row, col, m, data){
  NI <- RCmodel$xlevels[[1]] %>% length()
  NJ <- RCmodel$xlevels[[2]] %>% length()
  iflag <- 0
  y <- length(RCmodel$y)
  X <- gl(NI,NJ,length = y)
  Y <- gl(NJ,1,length = y)
  IX <- pickCoef(RCmodel, paste0("[.]",deparse(substitute(row))))
  IY <- pickCoef(RCmodel, paste0("[.]",deparse(substitute(col))))
  X <- RCmodel$coefficients[IX]
  Y <- RCmodel$coefficients[IY]
  mu0 <- matrix(X,nrow = NI,ncol = m)
  nu0 <- matrix(Y,nrow = NJ,ncol = m)
  mu <- mu0
  nu <- nu0
  A <- mu %*% t(nu) # [NI, M] [M. NJ]
  R1 <- diag(1,NI)
  C1 <- diag(1,NJ)
  Rone <- matrix(rep(1,NI^2),nrow = NI)
  Cone <- matrix(rep(1,NJ^2),nrow = NJ)
  rowP <- c(rep(1,NI))
  colP <- c(rep(1,NJ));
  data <- RC2$model
  list(y,X,Y,IX,IY,data)
  if (iflag == 1) {
    rowP <- with(data, tapply(freq,row,sum)/sum(freq))
    colP <- with(data, tapply(freq,col,sum)/sum(freq))}
  DR <- diag(rowP^(-1/2),NI)
  DC <- diag(colP^(-1/2),NJ)
  RWsqr <- diag(rowP^(1/2),NI)
  CWsqr <- diag(colP^(1/2),NJ)
  RW <- RWsqr^2
  CW <- CWsqr^2
  L <- (R1 - (Rone %*% RW)/sum(RW)) %*% A %*% t(C1 - (Cone %*% CW)/sum(CW))
  phiv <- svd(RWsqr %*% L %*% CWsqr)$d[1:m]
  mu <- svd(RWsqr %*% L %*% CWsqr)$u[,1:m]
  nu <- svd(RWsqr %*% L %*% CWsqr)$v[,1:m]
  mu <- DR %*% mu
  nu <- DC %*% nu
  phi <- diag(phiv, m)
  L2 <- RCmodel$deviance
  df <- RCmodel$df.residual
  p.value <- 1 - pchisq(L2,df)
  fit.freq <- predict(RCmodel, type = "response", se.fit = TRUE)
  
  print(list(model = RCmodel, 
     L2 = L2, 
     df = df,
     p.value = p.value,
     obs_freq = summary(RC2)$y, 
     fit.freq = fit.freq,
     phi = phi, 
     mu = mu, 
     nu = nu,
     cor(mu) %>% round(3),
     cor(nu) %>% round(3)))
  }

RCmodel_coef(RC2, row = Educ, col = Occ, m =  2)
```

```
$model

Call:
gnm(formula = Freq ~ Educ + Occ + instances(Mult(Educ, Occ), 
    instances = 2), family = poisson, data = ., tolerance = 1e-12)

Coefficients:
                 (Intercept)                         Educ2  
                     5.03021                      -0.86903  
                       Educ3                         Educ4  
                     1.13486                       0.32082  
                        Occ2                          Occ3  
                     0.37850                      -2.16443  
                        Occ4                          Occ5  
                     0.09540                      -4.07248  
Mult(., Occ, inst = 1).Educ1  Mult(., Occ, inst = 1).Educ2  
                    -2.78979                      -0.31931  
Mult(., Occ, inst = 1).Educ3  Mult(., Occ, inst = 1).Educ4  
                    -0.28793                       4.24751  
Mult(Educ, ., inst = 1).Occ1  Mult(Educ, ., inst = 1).Occ2  
                    -0.28722                      -0.07948  
Mult(Educ, ., inst = 1).Occ3  Mult(Educ, ., inst = 1).Occ4  
                     0.15279                       0.26840  
Mult(Educ, ., inst = 1).Occ5  Mult(., Occ, inst = 2).Educ1  
                     0.19547                       3.60546  
Mult(., Occ, inst = 2).Educ2  Mult(., Occ, inst = 2).Educ3  
                     1.26355                      -1.15902  
Mult(., Occ, inst = 2).Educ4  Mult(Educ, ., inst = 2).Occ1  
                     1.12408                       0.11598  
Mult(Educ, ., inst = 2).Occ2  Mult(Educ, ., inst = 2).Occ3  
                    -0.29739                      -0.20716  
Mult(Educ, ., inst = 2).Occ4  Mult(Educ, ., inst = 2).Occ5  
                    -0.22669                       0.34128  

Deviance:            0.6001022 
Pearson chi-squared: 0.5746549 
Residual df:         2 

$L2
[1] 0.6001022

$df
[1] 2

$p.value
[1] 0.7407804

$obs_freq
NULL

$fit.freq
$fit.freq$fit
          1           2           3           4           5           6 
 517.836063   95.414391    5.433736   35.145848    5.169961   81.405000 
          7           8           9          10          11          12 
  65.976261    5.398934   48.639687    1.580117  451.846566 1003.387842 
         13          14          15          16          17          18 
  66.470016  630.136504    5.159072   70.912370  157.221505   36.697314 
         19          20 
 562.077961   12.090850 

$fit.freq$se.fit
        1         2         3         4         5         6         7         8 
22.748933  9.732333  2.174395  5.860792  2.230227  8.912865  7.856714  1.044826 
        9        10        11        12        13        14        15        16 
 6.616317  0.619348 21.250045 31.666672  8.114913 25.088611  2.233235  8.415482 
       17        18        19        20 
12.530867  6.041164 23.703381  3.469126 

$fit.freq$residual.scale
[1] 1


$phi
         [,1]     [,2]
[1,] 2.600861 0.000000
[2,] 0.000000 1.521971

$mu
            [,1]        [,2]
[1,]  0.74345129  0.27619989
[2,]  0.08846472 -0.06103682
[3,] -0.19984223 -0.77722344
[4,] -0.63207378  0.56206037

$nu
            [,1]         [,2]
[1,]  0.76509960 -0.137064850
[2,]  0.01966104 -0.548959995
[3,] -0.32245067 -0.119769392
[4,] -0.55005650 -0.009934123
[5,]  0.08774653  0.815728360

[[10]]
     [,1] [,2]
[1,]    1    0
[2,]    0    1

[[11]]
     [,1] [,2]
[1,]    1    0
[2,]    0    1
```



```r
library(logmult)

## 表2.3B
Freq <- c(518,  95, 6, 35, 5,
         　 81,  67, 4, 49, 2,
          452,1003,67,630, 5,
           71, 157,37,562,12)

# データを表形式に変換
tab_2.3B <- matrix(Freq, nrow = 4, ncol = 5, byrow = TRUE) %>% as.table()

anoas(tab_2.3B)
```

```
Fitting independence model...
Fitting model with 1 dimension...
Initialising
Running start-up iterations..
Running main iterations.........................................................
...
Done
Fitting model with 2 dimensions...
Initialising
Running start-up iterations..
Running main iterations........
Done
Fitting model with 3 dimensions...
Initialising
Running start-up iterations..
Running main iterations......
Done
```

```
       Res. Df           Res. Dev     Dev./Indep. (%)   Dissim. (%)
Indep.      12 1373.1757656522450 100.000000000000000 23.8615752953
RC(1)        6  125.0597485501469   9.107337289101130  6.4360390986
RC(2)        2    0.6001022352681   0.043701778772874  0.0935165801
RC(3)        0   -0.0000000000001  -0.000000000000007  0.0000000004
                      BIC                AIC    Dev. Df
Indep. 1274.0809153306568 1349.1757656522450      NA NA
RC(1)    75.5123233893528  113.0597485501469 -1248.1 -6
RC(2)   -15.9157061516633   -3.3998977647319  -124.5 -4
RC(3)    -0.0000000000001   -0.0000000000001    -0.6 -2
```

```r
RC2 <- NULL
RC2 <- rc(tab_2.3B, nd = 2, weighting = "none", se = "bootstrap",
             nreplicates = 500, ncpus = getOption("boot.ncpus"))
```

```
Initialising
Running start-up iterations..
Running main iterations.........
Done
Computing bootstrap standard errors...
.
```

```r
RC2 %>% summary() %$% coefficients %>%
  data.frame() %>%
  rownames_to_column(var = "Parameter") %>%
  arrange(Parameter)
```

```
       Parameter   Normalized    Adjusted Std..error      Pr...z..
1           Dim1  2.600861491          NA 0.25730754  5.091926e-24
2  Dim1:ColumnsA  0.765099599  1.23389040 0.04511215  1.624989e-64
3  Dim1:ColumnsB  0.019661041  0.03170773 0.04199402  6.396511e-01
4  Dim1:ColumnsC -0.322450671 -0.52002222 0.10146888  1.483826e-03
5  Dim1:ColumnsD -0.550056503 -0.88708639 0.07039000  5.521990e-15
6  Dim1:ColumnsE  0.087746534  0.14151047 0.10011903  3.808006e-01
7     Dim1:RowsA  0.743451285  1.19897776 0.01879415  0.000000e+00
8     Dim1:RowsB  0.088464723  0.14266871 0.04053668  2.908463e-02
9     Dim1:RowsC -0.199842229 -0.32228929 0.03031346  4.324105e-11
10    Dim1:RowsD -0.632073780 -1.01935718 0.02784236 4.292616e-114
11          Dim2  1.521970932          NA 4.04398500  7.066536e-01
12 Dim2:ColumnsA -0.137064849 -0.16909442 0.05016078  6.285368e-03
13 Dim2:ColumnsB -0.548959996 -0.67724199 0.08075311  1.060784e-11
14 Dim2:ColumnsC -0.119769391 -0.14775733 0.08922469  1.794875e-01
15 Dim2:ColumnsD -0.009934123 -0.01225555 0.07707267  8.974424e-01
16 Dim2:ColumnsE  0.815728360  1.00634928 0.05710818  2.756703e-46
17    Dim2:RowsA  0.276199888  0.34074279 0.09214127  2.721416e-03
18    Dim2:RowsB -0.061036818 -0.07530001 0.15124234  6.865292e-01
19    Dim2:RowsC -0.777223438 -0.95884646 0.05943556  4.472099e-39
20    Dim2:RowsD  0.562060367  0.69340368 0.06534657  7.887190e-18
```

```r
# getS3method("assoc","rc")
```
