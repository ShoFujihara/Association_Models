# 第3章


```
## [1] 1000
```



```r
library(tidyverse)
library(magrittr)
library(gnm)
```


## 表3.1A


```r
library(gnm)

# 表3.1
Freq <- c(  9, 5, 5, 1,    1, 6, 5, 1,    2, 2, 2, 1,
           17,13, 7, 4,   13,22, 9, 1,    7,13, 6, 2,
            8,14, 6, 0,   10,29,10, 0,    5,14, 6, 2,
           20,38,24, 8,   23,72,34,10,   17,67,36,12,
            4,21,12, 4,    7,30, 9, 1,    9,19,14, 2,
            2, 9, 8, 3,    1,16,19, 2,   11,28,28,11,
            0, 1, 5, 0,    2, 3, 3, 2,    2, 7, 6, 6)
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




```r
polviews <- gl(n = 7, k = 4*3, length = length(Freq))
fefam <- gl(n = 4, k = 1, length = length(Freq))
natfare <- gl(n = 3, k = 4, length = length(Freq))

tab_3.1 <- xtabs(Freq ~ polviews + fefam + natfare)
freq_tab_3.1 <- tibble(Freq, polviews, fefam, natfare)

# Model 1 - Independence Model
M1 <- freq_tab_3.1 %>%
  gnm(Freq ~ polviews + fefam + natfare, family = poisson, data = ., trace = F, tolerance = 1e-12)

# Model 2 - Full Two-way Interaction
M2 <- freq_tab_3.1 %>%
  gnm(Freq ~ polviews*fefam + fefam*natfare + polviews*natfare, family = poisson,
      data = ., trace = F, tolerance = 1e-12)

# Model 3 - Conditional Independence on polviews
M3 <- freq_tab_3.1 %>%
  gnm(Freq ~ polviews*fefam + polviews*natfare, family = poisson,
      data = ., trace = F,tolerance = 1e-12)

# Model 4 - Conditional Independence on fefam
M4 <- freq_tab_3.1 %>%
  gnm(Freq ~ polviews*fefam + fefam*natfare, family = poisson,
      data = ., trace = F, tolerance = 1e-12)

# Model 5 - Conditional Independence on natfare
M5 <- freq_tab_3.1 %>%
  gnm(Freq ~ fefam*natfare + polviews*natfare, family = poisson,
      data = ., trace = F, tolerance = 1e-12)

# Model 6 - RC(1)+RL(1) partial association
M6 <- freq_tab_3.1 %>%
  gnm(Freq ~ polviews + fefam + natfare + Mult(1, polviews, fefam) + Mult(1, polviews, natfare),
      data = ., 
      family = poisson, 
      tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
.............
Done
```

```r
# Model 7 - RC(1)+RL(1) partial association (consistent row scores - polviews)
M7 <- freq_tab_3.1 %>%
  gnm(Freq ~ polviews + fefam + natfare + Mult(1, polviews, fefam + natfare),
      data = ., family = poisson, tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations..................................................
Done
```

```r
# Model 8 - RC(1)+RL(1) partial association (consistent row scores - polviews)
# equality restrictions on row scores

freq_tab_3.1 %<>% 
  mutate(polviews.c = polviews)

levels(freq_tab_3.1$polviews.c) <- factor(c(1,1,2,3,3,4,5))

M8 <- freq_tab_3.1 %>%
  gnm(Freq ~ polviews + fefam + natfare + Mult(1, polviews.c, fefam + natfare),
      data = ., family = poisson, tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
....
Done
```

```r
## 表3.2
M <- list()
M[[1]] <- model.summary(M1) %>% 
  mutate(`Model Description` = "1. Complete independence")
M[[2]] <- model.summary(M2) %>% 
  mutate(`Model Description` = "2. Full two-way interaction")
M[[3]] <- model.summary(M3) %>% 
  mutate(`Model Description` = "3. Conditional independence on POLVIEWS")
M[[4]] <- model.summary(M4) %>% 
  mutate(`Model Description` = "4. Conditional independence on FEFAM")
M[[5]] <- model.summary(M5) %>% 
  mutate(`Model Description` = "5. Conditional independence on NATFARE")
M[[6]] <- model.summary(M6) %>% 
  mutate(`Model Description` = "6. RC(1) + RL(1) partial association")
M[[7]] <- model.summary(M7) %>% 
  mutate(`Model Description` = "7. Model 6 plus consisitent row (POLVIEWS) score restrictions")
M[[8]] <- model.summary(M8) %>% 
  mutate(`Model Description` = "8. Model 6 plus consisitent and equality restrictions on row (POLVIEWS) scores (mu_1 = mu_2, mu_4 = mu_5)")
M %>% bind_rows()
```

```
# A tibble: 8 x 6
  `Model Description`                                 df    L2   BIC Delta     p
  <chr>                                            <int> <dbl> <dbl> <dbl> <dbl>
1 1. Complete independence                            72 168.  -324. 14.1  1.00 
2 2. Full two-way interaction                         36  35.4 -211.  5.72 0.501
3 3. Conditional independence on POLVIEWS             42  47.3 -240.  7.30 0.733
4 4. Conditional independence on FEFAM                48  87.3 -241. 10.3  1.00 
5 5. Conditional independence on NATFARE              54  91.0 -278. 10.1  0.999
6 6. RC(1) + RL(1) partial association                57  68.6 -321.  8.83 0.860
7 7. Model 6 plus consisitent row (POLVIEWS) scor…    62  72.8 -351.  9.07 0.835
8 8. Model 6 plus consisitent and equality restri…    64  73.6 -364.  9.21 0.807
```



```r
# Estimating standard error

# Model 6
# mu1[i], i = 1 to 7
mu1 <- getContrasts(M6, pickCoef(M6, "[.]polviews")[1:7], ref = "mean",
                    scaleRef = "mean", scaleWeights = "unit")
# nu[j], j = 1 to 4
nu <- getContrasts(M6, pickCoef(M6, "[.]fefam"), ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")
# mu2[i], i = 1 to 7
mu2 <- getContrasts(M6, pickCoef(M6, "[.]polviews")[8:14], ref = "mean",
                    scaleRef = "mean", scaleWeights = "unit")
# eta[k], k = 1 to 3
eta <- getContrasts(M6, pickCoef(M6, "[.]natfare"), ref = "mean",
                    scaleRef = "mean", scaleWeights = "unit")

con <- c(mu1$qvframe[,1][c(1,7)], nu$qvframe[,1][c(1,4)],
         mu2$qvframe[,1][c(1,7)], eta$qvframe[,1][c(1,3)])

M6_SE <- freq_tab_3.1 %>%
  gnm(Freq ~ polviews + fefam + natfare + Mult(1, polviews, fefam) + Mult(1, polviews, natfare),
      constrain = c(14,20,21,24,26,32,33,35), constrainTo = con,
      data = ., family = poisson, tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
..........................
Done
```

```r
M6_SE_coef <- summary(M6_SE) %>%
  coefficients() %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  dplyr::filter(grepl("Mult",Variable))


# Model 7
# mu[i], i = 1 to 7
mu <- getContrasts(M7, pickCoef(M7, "[.]polviews")[1:7], ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")

con <- c(mu$qvframe[,1][c(1:7)], mu$qvframe[,1][c(1:7)])

M7_SE <- freq_tab_3.1 %>%
  gnm(Freq ~ polviews + fefam + natfare + Mult(1, polviews, fefam) + Mult(1, polviews, natfare),
      constrain = c(14:20, 26:32), constrainTo = con,
      data = ., family = poisson, tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...
Done
```

```r
# nu[j], j = 1 to 4
nu <- getContrasts(M7_SE, pickCoef(M7_SE, "[.]fefam"), ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")
# eta[k], k = 1 to 3
eta <- getContrasts(M7_SE, pickCoef(M7_SE, "[.]natfare"), ref = "mean",
                    scaleRef = "mean", scaleWeights = "unit")

con <- c(mu$qvframe[,1][c(1:7)], nu$qvframe[,1][c(1,4)],
         mu$qvframe[,1][c(1:7)], eta$qvframe[,1][c(1,3)])

M7_SE <- freq_tab_3.1 %>%
  gnm(Freq ~ polviews + fefam + natfare + Mult(1, polviews, fefam) + Mult(1, polviews, natfare),
      constrain = c(14:20,21,24,26:32,33,35), constrainTo = con,
      data = ., family = poisson, trace = T, tolerance = 1e-12)
```

```
Initialising
Initial Deviance = 168.761227
Running start-up iterations
Start-up iteration 1. Deviance = 74.557760
Start-up iteration 2. Deviance = 72.914158
Running main iterations
Iteration 1. Deviance = 72.773896
Iteration 2. Deviance = 72.773834
Iteration 3. Deviance = 72.773834
Done
```

```r
M7_SE_coef <- summary(M7_SE) %>%
  coefficients() %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  dplyr::filter(grepl("Mult",Variable))

# Model 8
# mu[i], i = 1 to 7
mu <- getContrasts(M8, pickCoef(M8, "[.]polviews.c"),
                   ref = c(2,1,2,1,1)/7, scaleRef =c(2,1,2,1,1)/7,
                   scaleWeights=c(2,1,2,1,1))

mu <- getContrasts(M8, pickCoef(M8, "[.]polviews.c"),
                   ref = c(2,1,2,1,1)/7, scaleRef =c(2,1,2,1,1)/7,
                   scaleWeights=c(2,1,2,1,1))

con <- c(mu$qvframe[,1][c(1,1,2,3,3,4,5)], mu$qvframe[,1][c(1,1,2,3,3,4,5)])

M8 <- freq_tab_3.1 %>% gnm(Freq ~ polviews + fefam + natfare + Mult(1, polviews, fefam) + Mult(1, polviews, natfare),
          constrain = c(14:20,26:32), constrainTo = con,
          data = ., family = poisson, tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...
Done
```

```r
summary(M8)
```

```

Call:
gnm(formula = Freq ~ polviews + fefam + natfare + Mult(1, polviews, 
    fefam) + Mult(1, polviews, natfare), constrain = c(14:20, 
    26:32), constrainTo = con, family = poisson, data = ., tolerance = 1e-12)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-2.09613  -0.69364  -0.06107   0.52149   2.13382  

Coefficients:
                               Estimate Std. Error z value Pr(>|z|)    
(Intercept)                    0.494059         NA      NA       NA    
polviews2                      1.047319   0.183771   5.699 1.20e-08 ***
polviews3                      1.015164         NA      NA       NA    
polviews4                      2.327750         NA      NA       NA    
polviews5                      1.321674         NA      NA       NA    
polviews6                      1.254444         NA      NA       NA    
polviews7                     -0.123528         NA      NA       NA    
fefam2                         0.993418   0.095568  10.395  < 2e-16 ***
fefam3                         0.451237   0.104313   4.326 1.52e-05 ***
fefam4                        -0.839900   0.148980  -5.638 1.72e-08 ***
natfare2                       0.345798   0.085610   4.039 5.36e-05 ***
natfare3                       0.302261   0.086847   3.480 0.000501 ***
Mult(., polviews, fefam).      0.163148         NA      NA       NA    
Mult(1, ., fefam).polviews1   -0.402725         NA      NA       NA    
Mult(1, ., fefam).polviews2   -0.402725         NA      NA       NA    
Mult(1, ., fefam).polviews3   -0.278981         NA      NA       NA    
Mult(1, ., fefam).polviews4   -0.001962         NA      NA       NA    
Mult(1, ., fefam).polviews5   -0.001962         NA      NA       NA    
Mult(1, ., fefam).polviews6    0.491603         NA      NA       NA    
Mult(1, ., fefam).polviews7    0.596753         NA      NA       NA    
Mult(1, polviews, .).fefam1   -7.746085         NA      NA       NA    
Mult(1, polviews, .).fefam2   -0.960123         NA      NA       NA    
Mult(1, polviews, .).fefam3    4.698011         NA      NA       NA    
Mult(1, polviews, .).fefam4    7.903679         NA      NA       NA    
Mult(., polviews, natfare).    4.627056         NA      NA       NA    
Mult(1, ., natfare).polviews1 -0.402725         NA      NA       NA    
Mult(1, ., natfare).polviews2 -0.402725         NA      NA       NA    
Mult(1, ., natfare).polviews3 -0.278981         NA      NA       NA    
Mult(1, ., natfare).polviews4 -0.001962         NA      NA       NA    
Mult(1, ., natfare).polviews5 -0.001962         NA      NA       NA    
Mult(1, ., natfare).polviews6  0.491603         NA      NA       NA    
Mult(1, ., natfare).polviews7  0.596753         NA      NA       NA    
Mult(1, polviews, .).natfare1 -0.208030         NA      NA       NA    
Mult(1, polviews, .).natfare2 -0.101990         NA      NA       NA    
Mult(1, polviews, .).natfare3  0.207269         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 73.594 on 67 degrees of freedom
AIC: 411.62

Number of iterations: 3
```

```r
# nu[j], j = 1 to 4
nu <- getContrasts(M8, pickCoef(M8, "[.]fefam"), ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")

# eta[k], k = 1 to 3
eta <- getContrasts(M8, pickCoef(M8, "[.]natfare"), ref = "mean",
                    scaleRef = "mean", scaleWeights = "unit")

con <- c(mu$qvframe[,1][c(1,1,2,3,3,4,5)], nu$qvframe[,1][c(1,4)],
         mu$qvframe[,1][c(1,1,2,3,3,4,5)], eta$qvframe[,1][c(1,3)])

M8_SE <- freq_tab_3.1 %>%  gnm(Freq ~ polviews + fefam + natfare + Mult(1, polviews, fefam) + Mult(1, polviews, natfare),
          constrain = c(14:20,21,24,26:32,33,35), constrainTo = con,
          data = ., family = poisson, tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations..................
Done
```

```r
M8_SE_coef <- summary(M8_SE) %>%
  coefficients() %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  dplyr::filter(grepl("Mult",Variable))


bind_cols(
M6_SE_coef %>% dplyr::select(Variable, Model6 = Estimate),
M7_SE_coef %>% dplyr::select(Model7 = Estimate),
M8_SE_coef %>% dplyr::select(Model8 = Estimate))
```

```
                        Variable      Model6      Model7       Model8
1      Mult(., polviews, fefam).  1.98292236 -1.94985651  1.941869008
2    Mult(1, ., fefam).polviews1 -0.18890704 -0.41258265 -0.402725149
3    Mult(1, ., fefam).polviews2 -0.45611586 -0.38551616 -0.402725149
4    Mult(1, ., fefam).polviews3 -0.36505971 -0.26877471 -0.278981096
5    Mult(1, ., fefam).polviews4 -0.01292237  0.02747595 -0.001961895
6    Mult(1, ., fefam).polviews5 -0.06262362 -0.05550221 -0.001961895
7    Mult(1, ., fefam).polviews6  0.42111705  0.49418258  0.491602614
8    Mult(1, ., fefam).polviews7  0.66451154  0.60071720  0.596752569
9    Mult(1, polviews, .).fefam1 -0.70268861  0.72624699 -0.732615689
10   Mult(1, polviews, .).fefam2 -0.20817046  0.17021171 -0.162486357
11   Mult(1, polviews, .).fefam3  0.30041075 -0.30370570  0.312887371
12   Mult(1, polviews, .).fefam4  0.61044832 -0.59275300  0.582214676
13   Mult(., polviews, natfare).  1.56691378 -1.43774015  1.411968172
14 Mult(1, ., natfare).polviews1 -0.60584367 -0.41258265 -0.402725149
15 Mult(1, ., natfare).polviews2 -0.27518966 -0.38551616 -0.402725149
16 Mult(1, ., natfare).polviews3 -0.15821154 -0.26877471 -0.278981096
17 Mult(1, ., natfare).polviews4  0.06460260  0.02747595 -0.001961895
18 Mult(1, ., natfare).polviews5 -0.05050796 -0.05550221 -0.001961895
19 Mult(1, ., natfare).polviews6  0.51153570  0.49418258  0.491602614
20 Mult(1, ., natfare).polviews7  0.51361453  0.60071720  0.596752569
21 Mult(1, polviews, .).natfare1 -0.60734824  0.58021189 -0.569479796
22 Mult(1, polviews, .).natfare2 -0.16892093  0.20740348 -0.221984457
23 Mult(1, polviews, .).natfare3  0.77626917 -0.78761536  0.791464253
```


## 表3.1B


```r
# 表3.1
Freq<-c(76, 14, 15,  4,
        32, 17,  7,  3,
        64, 23, 28, 15,
        41, 11, 27, 16,
        15,  2,  7,  4,
        27, 20,  9,  5,
        57, 31, 24, 15,
        27,  9, 22, 16,
        13,  6, 13,  5,
        12, 13, 10,  6,
        46, 32, 75, 20,
        54, 26, 58, 55,
        7,  6,  7,  6,
        7,  2,  3,  6,
        12, 11, 31, 15,
        52, 36, 80,101)

L<-gl(4,16,64)
R<-gl(4,4,64)
C<-gl(4,1,64)

freq_tab_3.1 <- tibble(Freq, L, R, C) %>% arrange(L, R, C)
freq_tab_3.1 %<>% 
  mutate(Rscore = as.numeric(R),
         Cscore = as.numeric(C),
         Lscore = as.numeric(L))
# Model 1 - Complete Independence
model1 <- freq_tab_3.1 %>% 
  gnm(Freq ~ R + C + L, 
                               data =., 
                               family = poisson, 
                               trace = F, tolerance = 1e-12)

# Model 2 - Unrestricted RC(1)+RL(1)+CL(1)
model2 <- freq_tab_3.1 %>% 
  gnm(Freq ~ R + C + L + Mult(1,R,C) + Mult(1,R,L) + Mult(1,C,L), 
                               data = .,
                               family = poisson,
                               trace = F, tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
...............................
Done
```

```r
# Model 3 - Restricted RC(1)+RL(1)+CL(1) with consistent score
# It may not be fitted with gnm package!
model3 <- freq_tab_3.1 %>% 
  gnm(Freq ~ R + C + L + instances(Mult(R,C),1) + instances(Mult(R,L),1) + MultHomog(C,L), family = poisson,
      data = .,
      trace = F, tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
...........................................
Done
```

```r
summary(model3)
```

```

Call:

gnm(formula = Freq ~ R + C + L + instances(Mult(R, C), 1) + instances(Mult(R, 
    L), 1) + MultHomog(C, L), family = poisson, data = ., tolerance = 1e-12, 
    trace = F)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.4356  -0.9453  -0.2028   0.6944   4.0080  

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)
(Intercept)              2.62859         NA      NA       NA
R2                      -0.07202         NA      NA       NA
R3                       1.07687         NA      NA       NA
R4                       1.24252         NA      NA       NA
C2                      -0.74242         NA      NA       NA
C3                      -0.26258         NA      NA       NA
C4                      -0.75368         NA      NA       NA
L2                      -0.14844         NA      NA       NA
L3                       0.33658         NA      NA       NA
L4                      -0.02280         NA      NA       NA
Mult(., C, inst = 1).R1 -1.15021         NA      NA       NA
Mult(., C, inst = 1).R2 -1.82067         NA      NA       NA
Mult(., C, inst = 1).R3 -0.44922         NA      NA       NA
Mult(., C, inst = 1).R4  0.95358         NA      NA       NA
Mult(R, ., inst = 1).C1 -0.09775         NA      NA       NA
Mult(R, ., inst = 1).C2 -0.19482         NA      NA       NA
Mult(R, ., inst = 1).C3  0.10249         NA      NA       NA
Mult(R, ., inst = 1).C4  0.38682         NA      NA       NA
Mult(., L, inst = 1).R1  0.60721         NA      NA       NA
Mult(., L, inst = 1).R2  0.48280         NA      NA       NA
Mult(., L, inst = 1).R3  0.20805         NA      NA       NA
Mult(., L, inst = 1).R4 -0.50448         NA      NA       NA
Mult(R, ., inst = 1).L1  1.04293         NA      NA       NA
Mult(R, ., inst = 1).L2  0.73141         NA      NA       NA
Mult(R, ., inst = 1).L3 -0.06269         NA      NA       NA
Mult(R, ., inst = 1).L4 -1.18855         NA      NA       NA
MultHomog(C, L)1         0.67719         NA      NA       NA
MultHomog(C, L)2         0.16076         NA      NA       NA
MultHomog(C, L)3        -0.32462         NA      NA       NA
MultHomog(C, L)4        -0.69521         NA      NA       NA

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 111.83 on 41 degrees of freedom
AIC: 452.14

Number of iterations: 100
```

```r
# Model 4 - Model 2 with consistent cells fitted exactly
freq_tab_3.1 %<>% mutate(consistent.cells = factor(ifelse((R==C) & (C==L),1:4,0)))
model4 <- freq_tab_3.1 %>% 
  gnm(Freq ~ R + C + L + consistent.cells + Mult(1,R,C) + Mult(1,R,L) + Mult(1,C,L),
      data = .,
              family = poisson, trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.............................................
Done
```

```r
# Model 5 - Model 3 with consistent cells fitted exactly
# It may not be fitted with gnm package!

# Model 6 - Full Two-way interaction
model6 <- freq_tab_3.1 %>% 
  gnm(Freq~R*C+R*L+C*L,
      data = .,
      ,family = poisson,trace = F,tolerance = 1e-12)
summary(model6)
```

```

Call:
gnm(formula = Freq ~ R * C + R * L + C * L, family = poisson, 
    data = ., tolerance = 1e-12, trace = F)

Deviance Residuals: 
      Min         1Q     Median         3Q        Max  
-1.685582  -0.504109  -0.008581   0.460359   1.299438  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  4.26029    0.11066  38.499  < 2e-16 ***
R2          -0.79606    0.18519  -4.299 1.72e-05 ***
R3          -0.09579    0.14996  -0.639  0.52299    
R4          -0.43476    0.16083  -2.703  0.00687 ** 
C2          -1.63953    0.22829  -7.182 6.89e-13 ***
C3          -1.39351    0.20261  -6.878 6.08e-12 ***
C4          -2.33707    0.28019  -8.341  < 2e-16 ***
R2:C2        0.90120    0.28232   3.192  0.00141 ** 
R3:C2        0.63274    0.25192   2.512  0.01202 *  
R4:C2        0.33707    0.26138   1.290  0.19719    
R2:C3       -0.06534    0.29098  -0.225  0.82232    
R3:C3        0.66917    0.22004   3.041  0.00236 ** 
R4:C3        0.60286    0.22215   2.714  0.00665 ** 
R2:C4        0.34284    0.36220   0.947  0.34388    
R3:C4        0.56412    0.29673   1.901  0.05729 .  
R4:C4        1.27643    0.28261   4.517 6.29e-06 ***
L2          -1.48502    0.22186  -6.694 2.18e-11 ***
L3          -1.52970    0.20833  -7.343 2.10e-13 ***
L4          -2.08728    0.24553  -8.501  < 2e-16 ***
R2:L2        1.34634    0.28170   4.779 1.76e-06 ***
R3:L2        1.28703    0.24739   5.203 1.97e-07 ***
R4:L2        1.03853    0.26517   3.916 8.99e-05 ***
R2:L3        0.67005    0.28444   2.356  0.01849 *  
R3:L3        1.21742    0.22757   5.350 8.81e-08 ***
R4:L3        1.59671    0.23321   6.847 7.56e-12 ***
R2:L4        0.17240    0.35528   0.485  0.62750    
R3:L4        0.61348    0.27149   2.260  0.02384 *  
R4:L4        2.16906    0.25643   8.459  < 2e-16 ***
C2:L2        0.35033    0.21463   1.632  0.10263    
C3:L2        0.22827    0.20862   1.094  0.27389    
C4:L2        0.49174    0.25839   1.903  0.05703 .  
C2:L3        0.65539    0.20819   3.148  0.00164 ** 
C3:L3        1.09826    0.18400   5.969 2.39e-09 ***
C4:L3        1.11150    0.23123   4.807 1.53e-06 ***
C2:L4        0.87747    0.23892   3.673  0.00024 ***
C3:L4        1.29956    0.20753   6.262 3.80e-10 ***
C4:L4        1.79692    0.23862   7.530 5.06e-14 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Residual deviance: 28.998 on 27 degrees of freedom
AIC: 397.32

Number of iterations: 5
```

```r
# Model 7 - Model 6 with consistent cells fitted exactly
model7<- freq_tab_3.1 %>% 
  gnm(Freq~R*C+R*L+C*L+consistent.cells,
      data = .,
      family = poisson,trace = F,tolerance = 1e-12)

# Model 8 - Unrestricted uniform and log-multiplicative association in all partial
# association

model8 <- freq_tab_3.1 %>% 
  gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,R,C)+
              Rscore:Lscore+Mult(1,R,L)+ Cscore:Lscore+Mult(1,C,L),
      data =.,
            family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
..........................
Done
```

```r
options(width=90,contrasts = c(factor="contr.treatment", ordered="contr.treatment"))
# Model 9 - Model 8 with consistent row scores
model9<- freq_tab_3.1 %>% gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,R,C+L)+
              Rscore:Lscore+Cscore:Lscore+Mult(1,C,L),
              data = .,
              family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................................................................

Done
```

```r
#summary(model9)

mu<-getContrasts(model9, pickCoef(model9, "[.]R"), ref = "mean",
                 scaleRef = "mean", scaleWeights = "unit")

nu2<-getContrasts(model9, pickCoef(model9, "[.]C")[5:8], ref = "mean",
                  scaleRef = "mean", scaleWeights = "unit")

eta2<-getContrasts(model9, pickCoef(model9, "[.]L")[4:7], ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")

model9.extended <- freq_tab_3.1 %>% gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,R,C)+
                       Rscore:Lscore+Mult(1,R,L)+Cscore:Lscore+Mult(1,C,L),
                     constrain=c(13:16,23:26),constrainTo=c(mu$qvframe[,1],mu$qvframe[,1]),
                     data = .,
                     family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................................................................
..........................
Done
```

```r
summary(model9.extended)
```

```

Call:
gnm(formula = Freq ~ R + C + L + Rscore:Cscore + Mult(1, R, C) + 
    Rscore:Lscore + Mult(1, R, L) + Cscore:Lscore + Mult(1, C, 
    L), constrain = c(13:16, 23:26), constrainTo = c(mu$qvframe[, 
    1], mu$qvframe[, 1]), family = poisson, data = ., tolerance = 1e-12,     trace = F)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.56526  -0.67024  -0.02438   0.54037   1.98333  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)       3.56160         NA      NA       NA    
R2               -0.94663         NA      NA       NA    
R3               -0.71817         NA      NA       NA    
R4               -1.55450         NA      NA       NA    
C2               -1.58861         NA      NA       NA    
C3               -2.05079         NA      NA       NA    
C4               -3.48065         NA      NA       NA    
L2               -1.38098         NA      NA       NA    
L3               -2.25593         NA      NA       NA    
L4               -3.81349         NA      NA       NA    
Rscore:Cscore     0.13353    0.02520   5.300 1.16e-07 ***
Mult(., R, C).   -0.10054         NA      NA       NA    
Mult(1, ., C).R1 -0.56136         NA      NA       NA    
Mult(1, ., C).R2  0.59333         NA      NA       NA    
Mult(1, ., C).R3  0.39165         NA      NA       NA    
Mult(1, ., C).R4 -0.42362         NA      NA       NA    
Mult(1, R, .).C1  1.41773         NA      NA       NA    
Mult(1, R, .).C2 -4.47477         NA      NA       NA    
Mult(1, R, .).C3 -0.13452         NA      NA       NA    
Mult(1, R, .).C4  4.66268         NA      NA       NA    
Rscore:Lscore     0.24848    0.02521   9.856  < 2e-16 ***
Mult(., R, L).    0.95021         NA      NA       NA    
Mult(1, ., L).R1 -0.56136         NA      NA       NA    
Mult(1, ., L).R2  0.59333         NA      NA       NA    
Mult(1, ., L).R3  0.39165         NA      NA       NA    
Mult(1, ., L).R4 -0.42362         NA      NA       NA    
Mult(1, R, .).L1 -0.04835         NA      NA       NA    
Mult(1, R, .).L2  0.80345         NA      NA       NA    
Mult(1, R, .).L3  0.16329         NA      NA       NA    
Mult(1, R, .).L4 -0.87000         NA      NA       NA    
Cscore:Lscore     0.20270    0.02560   7.919  < 2e-16 ***
Mult(., C, L).    1.40132         NA      NA       NA    
Mult(1, ., L).C1 -0.01979         NA      NA       NA    
Mult(1, ., L).C2  0.12720         NA      NA       NA    
Mult(1, ., L).C3  0.68032         NA      NA       NA    
Mult(1, ., L).C4 -0.06619         NA      NA       NA    
Mult(1, C, .).L1 -0.06182         NA      NA       NA    
Mult(1, C, .).L2 -0.24254         NA      NA       NA    
Mult(1, C, .).L3  0.23273         NA      NA       NA    
Mult(1, C, .).L4 -0.05328         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 47.134 on 40 degrees of freedom
AIC: 389.45

Number of iterations: 93
```

```r
nu1<-getContrasts(model9.extended,
                  pickCoef(model9.extended, "[.]C")[1:4], ref = "mean",
                  scaleRef = "mean", scaleWeights = "unit")

eta1<-getContrasts(model9.extended, pickCoef(model9.extended, "[.]L")[1:4], ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")

model9.phis<- freq_tab_3.1 %>%  gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,R,C)+
                   Rscore:Lscore+Mult(1,R,L)+Cscore:Lscore+Mult(1,C,L),
                 constrain=c(13:20,23:30,33:40),
                 constrainTo=c(mu$qvframe[,1],
                               nu1$qvframe[,1],
                               mu$qvframe[,1],
                               eta1$qvframe[,1],
                               nu2$qvframe[,1],
                               eta2$qvframe[,1]),
                 data = .,
                 family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...
Done
```

```r
summary(model9.phis);mu;nu1;eta1;nu2;eta2;model.summary(model9)
```

```

Call:
gnm(formula = Freq ~ R + C + L + Rscore:Cscore + Mult(1, R, C) + 
    Rscore:Lscore + Mult(1, R, L) + Cscore:Lscore + Mult(1, C, 
    L), constrain = c(13:20, 23:30, 33:40), constrainTo = c(mu$qvframe[, 
    1], nu1$qvframe[, 1], mu$qvframe[, 1], eta1$qvframe[, 1], 
    nu2$qvframe[, 1], eta2$qvframe[, 1]), family = poisson, data = ., 
    tolerance = 1e-12, trace = F)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.56526  -0.67024  -0.02438   0.54037   1.98333  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)       3.56903    0.08137  43.862  < 2e-16 ***
R2               -0.97605    0.11881  -8.215  < 2e-16 ***
R3               -0.74245    0.14810  -5.013 5.36e-07 ***
R4               -1.55801    0.22175  -7.026 2.13e-12 ***
C2               -1.59504    0.10577 -15.081  < 2e-16 ***
C3               -2.08142    0.17240 -12.073  < 2e-16 ***
C4               -3.47861    0.26841 -12.960  < 2e-16 ***
L2               -1.42666    0.10739 -13.285  < 2e-16 ***
L3               -2.18148    0.17791 -12.262  < 2e-16 ***
L4               -3.81133    0.27937 -13.643  < 2e-16 ***
Rscore:Cscore     0.13353    0.02486   5.371 7.83e-08 ***
Mult(., R, C).   -0.66121    0.14008  -4.720 2.36e-06 ***
Mult(1, ., C).R1 -0.56136         NA      NA       NA    
Mult(1, ., C).R2  0.59333         NA      NA       NA    
Mult(1, ., C).R3  0.39165         NA      NA       NA    
Mult(1, ., C).R4 -0.42362         NA      NA       NA    
Mult(1, R, .).C1  0.15965         NA      NA       NA    
Mult(1, R, .).C2 -0.73633         NA      NA       NA    
Mult(1, R, .).C3 -0.07638         NA      NA       NA    
Mult(1, R, .).C4  0.65306         NA      NA       NA    
Rscore:Lscore     0.24848    0.02515   9.879  < 2e-16 ***
Mult(., R, L).    1.13662    0.13964   8.140  < 2e-16 ***
Mult(1, ., L).R1 -0.56136         NA      NA       NA    
Mult(1, ., L).R2  0.59333         NA      NA       NA    
Mult(1, ., L).R3  0.39165         NA      NA       NA    
Mult(1, ., L).R4 -0.42362         NA      NA       NA    
Mult(1, R, .).L1 -0.05054         NA      NA       NA    
Mult(1, R, .).L2  0.66157         NA      NA       NA    
Mult(1, R, .).L3  0.12640         NA      NA       NA    
Mult(1, R, .).L4 -0.73743         NA      NA       NA    
Cscore:Lscore     0.20270    0.02315   8.756  < 2e-16 ***
Mult(., C, L).    0.28351    0.10068   2.816  0.00486 ** 
Mult(1, ., L).C1 -0.33661         NA      NA       NA    
Mult(1, ., L).C2 -0.08944         NA      NA       NA    
Mult(1, ., L).C3  0.84069         NA      NA       NA    
Mult(1, ., L).C4 -0.41464         NA      NA       NA    
Mult(1, C, .).L1 -0.08991         NA      NA       NA    
Mult(1, C, .).L2 -0.62111         NA      NA       NA    
Mult(1, C, .).L3  0.77584         NA      NA       NA    
Mult(1, C, .).L4 -0.06482         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 47.134 on 48 degrees of freedom
AIC: 373.45

Number of iterations: 3
```

```
                       Estimate Std. Error
Mult(1, ., C + L).R1 -0.5613577 0.07946201
Mult(1, ., C + L).R2  0.5933253 0.06976100
Mult(1, ., C + L).R3  0.3916506 0.08375491
Mult(1, ., C + L).R4 -0.4236182 0.08896023
```

```
                    Estimate Std. Error
Mult(1, R, .).C1  0.15964986 0.14637516
Mult(1, R, .).C2 -0.73633460 0.08871449
Mult(1, R, .).C3 -0.07637713 0.15684115
Mult(1, R, .).C4  0.65306187 0.10868144
```

```
                    Estimate Std. Error
Mult(1, R, .).L1 -0.05053742 0.08981862
Mult(1, R, .).L2  0.66156822 0.06038098
Mult(1, R, .).L3  0.12639555 0.08679365
Mult(1, R, .).L4 -0.73742635 0.04940126
```

```
                    Estimate Std. Error
Mult(1, ., L).C1 -0.33661004 0.30087703
Mult(1, ., L).C2 -0.08944163 0.36058729
Mult(1, ., L).C3  0.84069369 0.08936905
Mult(1, ., L).C4 -0.41464201 0.32252708
```

```
                    Estimate Std. Error
Mult(1, C, .).L1 -0.08990831  0.3610226
Mult(1, C, .).L2 -0.62111332  0.2399524
Mult(1, C, .).L3  0.77584334  0.1519881
Mult(1, C, .).L4 -0.06482171  0.3227342
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 model9                 38  47.1 -231.  6.25 0.853
```

```r
# Model 10 - Model 8 with consistent col scores
model10 <- freq_tab_3.1 %>% gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,C,R+L)+
               Rscore:Lscore+Mult(1,R,L)+ Cscore:Lscore,
               data = ., 
               family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................................................................
..........................................................................................
............................................................
Done
```

```r
model.summary(model10)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 model10                38  54.8 -223.  7.50 0.962
```

```r
# Model 11 - Model 8 with consistent layer scores
model11<- freq_tab_3.1 %>% gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,L,R+C)+
               Rscore:Lscore+Mult(1,R,C)+ Cscore:Lscore,
               data = .,
               family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................................................................
.............
Done
```

```r
model.summary(model11)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 model11                38  53.6 -225.  6.58 0.952
```

```r
# Model 12 - Modek 9 + U(RL)=U(CL)
# To impose equality constraint on U(RL) & U(CL),
# a covariate is created in following manner:


freq_tab_3.1 %<>% mutate(cov_rcl = Rscore*Lscore+Cscore*Lscore)
model12<- freq_tab_3.1 %>% gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,R,C+L)+
               cov_rcl+Mult(1,C,L),
               data =.,family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................................................................
................
Done
```

```r
#summary(model12)
model.summary(model12)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 model12                39  48.6 -237.  6.57 0.860
```

```r
mu<-getContrasts(model12, pickCoef(model12, "[.]R"), ref = "mean",
                 scaleRef = "mean", scaleWeights = "unit")

nu2<-getContrasts(model12, pickCoef(model12, "[.]C")[5:8], ref = "mean",
                  scaleRef = "mean", scaleWeights = "unit")

eta2<-getContrasts(model12, pickCoef(model12, "[.]L")[4:7], ref = "mean",
                   scaleRef = "mean", scaleWeights = "unit")

model12.extended <- freq_tab_3.1 %>% gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,R,C)+
                        cov_rcl+Mult(1,R,L)+Mult(1,C,L),
                      constrain=c(13:16,23:26),constrainTo=c(mu$qvframe[,1],mu$qvframe[,1]),
                      data = .,
                      family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................................................................
.............................
Done
```

```r
summary(model12.extended)
```

```

Call:
gnm(formula = Freq ~ R + C + L + Rscore:Cscore + Mult(1, R, C) + 
    cov_rcl + Mult(1, R, L) + Mult(1, C, L), constrain = c(13:16, 
    23:26), constrainTo = c(mu$qvframe[, 1], mu$qvframe[, 1]), 
    family = poisson, data = ., tolerance = 1e-12, trace = F)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.58844  -0.76809   0.02278   0.56330   1.92999  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)       3.58449         NA      NA       NA    
R2               -1.00051         NA      NA       NA    
R3               -0.70053         NA      NA       NA    
R4               -1.39883         NA      NA       NA    
C2               -1.63242         NA      NA       NA    
C3               -2.13996         NA      NA       NA    
C4               -3.65896         NA      NA       NA    
L2               -1.36583         NA      NA       NA    
L3               -2.16676         NA      NA       NA    
L4               -3.74210         NA      NA       NA    
Rscore:Cscore     0.13336    0.02542   5.246 1.55e-07 ***
Mult(., R, C).   -0.80657         NA      NA       NA    
Mult(1, ., C).R1  0.53827         NA      NA       NA    
Mult(1, ., C).R2 -0.59767         NA      NA       NA    
Mult(1, ., C).R3 -0.38941         NA      NA       NA    
Mult(1, ., C).R4  0.44880         NA      NA       NA    
Mult(1, R, .).C1 -0.12707         NA      NA       NA    
Mult(1, R, .).C2  0.60502         NA      NA       NA    
Mult(1, R, .).C3  0.08064         NA      NA       NA    
Mult(1, R, .).C4 -0.49208         NA      NA       NA    
cov_rcl           0.22423    0.01709  13.123  < 2e-16 ***
Mult(., R, L).    0.90628         NA      NA       NA    
Mult(1, ., L).R1  0.53827         NA      NA       NA    
Mult(1, ., L).R2 -0.59767         NA      NA       NA    
Mult(1, ., L).R3 -0.38941         NA      NA       NA    
Mult(1, ., L).R4  0.44880         NA      NA       NA    
Mult(1, R, .).L1  0.00258         NA      NA       NA    
Mult(1, R, .).L2 -0.88692         NA      NA       NA    
Mult(1, R, .).L3 -0.20816         NA      NA       NA    
Mult(1, R, .).L4  0.87418         NA      NA       NA    
Mult(., C, L).    0.80133         NA      NA       NA    
Mult(1, ., L).C1  0.06908         NA      NA       NA    
Mult(1, ., L).C2 -0.01996         NA      NA       NA    
Mult(1, ., L).C3 -0.65631         NA      NA       NA    
Mult(1, ., L).C4  0.16741         NA      NA       NA    
Mult(1, C, .).L1  0.06092         NA      NA       NA    
Mult(1, C, .).L2  0.41886         NA      NA       NA    
Mult(1, C, .).L3 -0.33023         NA      NA       NA    
Mult(1, C, .).L4  0.13138         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 48.569 on 41 degrees of freedom
AIC: 388.89

Number of iterations: 96
```

```r
nu1<-getContrasts(model12.extended,
                  pickCoef(model12.extended, "[.]C")[1:4], ref = "mean",
                  scaleRef = "mean", scaleWeights = "unit")

eta1<-getContrasts(model12.extended, pickCoef(model12.extended, "[.]L")[1:4],
                   ref = "mean",scaleRef = "mean", scaleWeights = "unit")

model12.phis<- freq_tab_3.1 %>% gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,R,C)+
                    cov_rcl+Mult(1,R,L)+Mult(1,C,L),
                  constrain=c(13:20,23:30,32:39),constrainTo=c(mu$qvframe[,1],nu1$qvframe[,1],
                                                               mu$qvframe[,1],eta1$qvframe[,1],nu2$qvframe[,1],eta2$qvframe[,1]),
                  data = .,
                  family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...
Done
```

```r
summary(model12.phis);mu;nu1;eta1;nu2;eta2;model.summary(model12)
```

```

Call:
gnm(formula = Freq ~ R + C + L + Rscore:Cscore + Mult(1, R, C) + 
    cov_rcl + Mult(1, R, L) + Mult(1, C, L), constrain = c(13:20, 
    23:30, 32:39), constrainTo = c(mu$qvframe[, 1], nu1$qvframe[, 
    1], mu$qvframe[, 1], eta1$qvframe[, 1], nu2$qvframe[, 1], 
    eta2$qvframe[, 1]), family = poisson, data = ., tolerance = 1e-12,     trace = F)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.58844  -0.76809   0.02278   0.56330   1.92999  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)       3.55535    0.08106  43.859  < 2e-16 ***
R2               -0.92909    0.11339  -8.194  < 2e-16 ***
R3               -0.64221    0.12609  -5.093 3.52e-07 ***
R4               -1.39320    0.17746  -7.851  < 2e-16 ***
C2               -1.63743    0.10043 -16.304  < 2e-16 ***
C3               -2.18079    0.15608 -13.973  < 2e-16 ***
C4               -3.65343    0.23846 -15.321  < 2e-16 ***
L2               -1.39736    0.10537 -13.261  < 2e-16 ***
L3               -2.13230    0.17235 -12.372  < 2e-16 ***
L4               -3.74831    0.27154 -13.804  < 2e-16 ***
Rscore:Cscore     0.13336    0.02490   5.357 8.48e-08 ***
Mult(., R, C).   -0.64007    0.13725  -4.663 3.11e-06 ***
Mult(1, ., C).R1  0.53827         NA      NA       NA    
Mult(1, ., C).R2 -0.59766         NA      NA       NA    
Mult(1, ., C).R3 -0.38941         NA      NA       NA    
Mult(1, ., C).R4  0.44880         NA      NA       NA    
Mult(1, R, .).C1 -0.18108         NA      NA       NA    
Mult(1, R, .).C2  0.74146         NA      NA       NA    
Mult(1, R, .).C3  0.08067         NA      NA       NA    
Mult(1, R, .).C4 -0.64105         NA      NA       NA    
cov_rcl           0.22423    0.01636  13.707  < 2e-16 ***
Mult(., R, L).    1.13998    0.13669   8.340  < 2e-16 ***
Mult(1, ., L).R1  0.53827         NA      NA       NA    
Mult(1, ., L).R2 -0.59766         NA      NA       NA    
Mult(1, ., L).R3 -0.38941         NA      NA       NA    
Mult(1, ., L).R4  0.44880         NA      NA       NA    
Mult(1, R, .).L1  0.04544         NA      NA       NA    
Mult(1, R, .).L2 -0.66170         NA      NA       NA    
Mult(1, R, .).L3 -0.12210         NA      NA       NA    
Mult(1, R, .).L4  0.73836         NA      NA       NA    
Mult(., C, L).   -0.27613    0.10158  -2.718  0.00656 ** 
Mult(1, ., L).C1 -0.27770         NA      NA       NA    
Mult(1, ., L).C2 -0.13959         NA      NA       NA    
Mult(1, ., L).C3  0.84752         NA      NA       NA    
Mult(1, ., L).C4 -0.43024         NA      NA       NA    
Mult(1, C, .).L1 -0.01743         NA      NA       NA    
Mult(1, C, .).L2  0.65220         NA      NA       NA    
Mult(1, C, .).L3 -0.74916         NA      NA       NA    
Mult(1, C, .).L4  0.11439         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 48.569 on 49 degrees of freedom
AIC: 372.89

Number of iterations: 3
```

```
                       Estimate Std. Error
Mult(1, ., C + L).R1  0.5382715 0.07940991
Mult(1, ., C + L).R2 -0.5976649 0.06869502
Mult(1, ., C + L).R3 -0.3894076 0.08390695
Mult(1, ., C + L).R4  0.4488010 0.08495489
```

```
                    Estimate Std. Error
Mult(1, R, .).C1 -0.18108336  0.1505749
Mult(1, R, .).C2  0.74145909  0.0888159
Mult(1, R, .).C3  0.08066998  0.1598862
Mult(1, R, .).C4 -0.64104571  0.1141041
```

```
                   Estimate Std. Error
Mult(1, R, .).L1  0.0454423 0.08895571
Mult(1, R, .).L2 -0.6617033 0.05943373
Mult(1, R, .).L3 -0.1220991 0.08507409
Mult(1, R, .).L4  0.7383601 0.04862532
```

```
                   Estimate Std. Error
Mult(1, ., L).C1 -0.2776974 0.30818568
Mult(1, ., L).C2 -0.1395889 0.36977790
Mult(1, ., L).C3  0.8475231 0.08001496
Mult(1, ., L).C4 -0.4302368 0.32326672
```

```
                    Estimate Std. Error
Mult(1, C, .).L1 -0.01742597  0.3681779
Mult(1, C, .).L2  0.65219849  0.2296758
Mult(1, C, .).L3 -0.74916478  0.1797929
Mult(1, C, .).L4  0.11439226  0.3321123
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 model12                39  48.6 -237.  6.57 0.860
```

```r
# Model 15

freq_tab_3.1 %<>% mutate(cov_rcl = Rscore*Lscore+Cscore*Lscore)
model15<- freq_tab_3.1 %>% 
  gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,R,C+L)+cov_rcl,
      data = .,
      ,family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.................................................
Done
```

```r
summary(model15)
```

```

Call:
gnm(formula = Freq ~ R + C + L + Rscore:Cscore + Mult(1, R, C + 
    L) + cov_rcl, family = poisson, data = ., tolerance = 1e-12,     trace = F)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.98677  -0.75501   0.01756   0.52206   2.97799  

Coefficients:
                     Estimate Std. Error z value Pr(>|z|)    
(Intercept)           3.57316         NA      NA       NA    
R2                   -0.96403         NA      NA       NA    
R3                   -0.68081         NA      NA       NA    
R4                   -1.42280         NA      NA       NA    
C2                   -1.67264         NA      NA       NA    
C3                   -2.17739         NA      NA       NA    
C4                   -3.68263         NA      NA       NA    
L2                   -1.44956         NA      NA       NA    
L3                   -2.16749         NA      NA       NA    
L4                   -3.78598         NA      NA       NA    
Rscore:Cscore         0.13307    0.02552   5.215 1.84e-07 ***
Mult(., R, C + L).    0.42235         NA      NA       NA    
Mult(1, ., C + L).R1  0.79131         NA      NA       NA    
Mult(1, ., C + L).R2 -1.01157         NA      NA       NA    
Mult(1, ., C + L).R3 -0.68058         NA      NA       NA    
Mult(1, ., C + L).R4  0.63184         NA      NA       NA    
Mult(1, R, . + L).C1  0.21278         NA      NA       NA    
Mult(1, R, . + L).C2 -0.66899         NA      NA       NA    
Mult(1, R, . + L).C3 -0.01786         NA      NA       NA    
Mult(1, R, . + L).C4  0.66458         NA      NA       NA    
Mult(1, R, C + .).L2 -1.20506         NA      NA       NA    
Mult(1, R, C + .).L3 -0.30408         NA      NA       NA    
Mult(1, R, C + .).L4  1.18104         NA      NA       NA    
cov_rcl               0.22862    0.01709  13.377  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 55.984 on 44 degrees of freedom
AIC: 390.3

Number of iterations: 49
```

```r
model.summary(model15)
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 model15                44  56.0 -266.  6.83 0.894
```

```r
mu<-getContrasts(model15, pickCoef(model15, "[.]R"), ref = "mean",
                 scaleRef = "mean", scaleWeights = "unit")

nu<-getContrasts(model15, pickCoef(model15, "[.]C"), ref = "mean",
                 scaleRef = "mean", scaleWeights = "unit")

model15.extended<- freq_tab_3.1 %>% gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,R,C)+
                        cov_rcl+Mult(1,R,L),constrain=c(13:16,23:26),
                      constrainTo=c(mu$qvframe[,1],mu$qvframe[,1]),
                      data = .,
                      family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations....
Done
```

```r
summary(model15.extended)
```

```

Call:
gnm(formula = Freq ~ R + C + L + Rscore:Cscore + Mult(1, R, C) + 
    cov_rcl + Mult(1, R, L), constrain = c(13:16, 23:26), constrainTo = c(mu$qvframe[, 
    1], mu$qvframe[, 1]), family = poisson, data = ., tolerance = 1e-12,     trace = F)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.98677  -0.75501   0.01756   0.52206   2.97799  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)       3.59119         NA      NA       NA    
R2               -1.01458         NA      NA       NA    
R3               -0.72208         NA      NA       NA    
R4               -1.42727         NA      NA       NA    
C2               -1.64759    0.10152  -16.23  < 2e-16 ***
C3               -2.17084    0.15843  -13.70  < 2e-16 ***
C4               -3.69546    0.24129  -15.32  < 2e-16 ***
L2               -1.41533    0.10612  -13.34  < 2e-16 ***
L3               -2.15885    0.17266  -12.50  < 2e-16 ***
L4               -3.81953    0.27268  -14.01  < 2e-16 ***
Rscore:Cscore     0.13307    0.02539    5.24  1.6e-07 ***
Mult(., R, C).   -1.01712         NA      NA       NA    
Mult(1, ., C).R1  0.54368         NA      NA       NA    
Mult(1, ., C).R2 -0.59799         NA      NA       NA    
Mult(1, ., C).R3 -0.38839         NA      NA       NA    
Mult(1, ., C).R4  0.44270         NA      NA       NA    
Mult(1, R, .).C1 -0.07188         NA      NA       NA    
Mult(1, R, .).C2  0.50633         NA      NA       NA    
Mult(1, R, .).C3  0.07936         NA      NA       NA    
Mult(1, R, .).C4 -0.36814         NA      NA       NA    
cov_rcl           0.22862    0.01643   13.91  < 2e-16 ***
Mult(., R, L).    0.42369         NA      NA       NA    
Mult(1, ., L).R1  0.54368         NA      NA       NA    
Mult(1, ., L).R2 -0.59799         NA      NA       NA    
Mult(1, ., L).R3 -0.38839         NA      NA       NA    
Mult(1, ., L).R4  0.44270         NA      NA       NA    
Mult(1, R, .).L1  0.05789         NA      NA       NA    
Mult(1, R, .).L2 -1.83911         NA      NA       NA    
Mult(1, R, .).L3 -0.42080         NA      NA       NA    
Mult(1, R, .).L4  1.91708         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 55.984 on 46 degrees of freedom
AIC: 386.3

Number of iterations: 4
```

```r
eta<-getContrasts(model15.extended, pickCoef(model15.extended, "[.]L"),
                  ref = "mean",scaleRef = "mean", scaleWeights = "unit")

model15.phis<- freq_tab_3.1 %>% gnm(Freq~R+C+L+Rscore:Cscore+Mult(1,R,C)+cov_rcl+Mult(1,R,L),
                  constrain=c(13:20,23:30),
                  constrainTo=c(mu$qvframe[,1],nu$qvframe[,1],mu$qvframe[,1],eta$qvframe[,1]),
                  data = .,
                  family = poisson,trace = F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...
Done
```

```r
summary(model15.phis);mu;nu;eta;model.summary(model15)
```

```

Call:
gnm(formula = Freq ~ R + C + L + Rscore:Cscore + Mult(1, R, C) + 
    cov_rcl + Mult(1, R, L), constrain = c(13:20, 23:30), constrainTo = c(mu$qvframe[, 
    1], nu$qvframe[, 1], mu$qvframe[, 1], eta$qvframe[, 1]), 
    family = poisson, data = ., tolerance = 1e-12, trace = F)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-1.98677  -0.75501   0.01756   0.52206   2.97799  

Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)       3.55464    0.08091  43.935  < 2e-16 ***
R2               -0.93784    0.11339  -8.271  < 2e-16 ***
R3               -0.65943    0.12613  -5.228 1.71e-07 ***
R4               -1.42048    0.17754  -8.001  < 2e-16 ***
C2               -1.64759    0.10044 -16.404  < 2e-16 ***
C3               -2.17084    0.15588 -13.927  < 2e-16 ***
C4               -3.69546    0.23861 -15.488  < 2e-16 ***
L2               -1.41533    0.10551 -13.414  < 2e-16 ***
L3               -2.15885    0.17245 -12.519  < 2e-16 ***
L4               -3.81953    0.27221 -14.032  < 2e-16 ***
Rscore:Cscore     0.13307    0.02492   5.341 9.24e-08 ***
Mult(., R, C).    0.64172    0.13829   4.641 3.48e-06 ***
Mult(1, ., C).R1  0.54368         NA      NA       NA    
Mult(1, ., C).R2 -0.59799         NA      NA       NA    
Mult(1, ., C).R3 -0.38839         NA      NA       NA    
Mult(1, ., C).R4  0.44270         NA      NA       NA    
Mult(1, R, .).C1  0.17165         NA      NA       NA    
Mult(1, R, .).C2 -0.74481         NA      NA       NA    
Mult(1, R, .).C3 -0.06806         NA      NA       NA    
Mult(1, R, .).C4  0.64122         NA      NA       NA    
cov_rcl           0.22862    0.01640  13.939  < 2e-16 ***
Mult(., R, L).    1.13826    0.13726   8.293  < 2e-16 ***
Mult(1, ., L).R1  0.54368         NA      NA       NA    
Mult(1, ., L).R2 -0.59799         NA      NA       NA    
Mult(1, ., L).R3 -0.38839         NA      NA       NA    
Mult(1, ., L).R4  0.44270         NA      NA       NA    
Mult(1, R, .).L1  0.04806         NA      NA       NA    
Mult(1, R, .).L2 -0.65804         NA      NA       NA    
Mult(1, R, .).L3 -0.13012         NA      NA       NA    
Mult(1, R, .).L4  0.74010         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 55.984 on 50 degrees of freedom
AIC: 378.3

Number of iterations: 3
```

```
                       Estimate Std. Error
Mult(1, ., C + L).R1  0.5436794 0.07818382
Mult(1, ., C + L).R2 -0.5979869 0.06871974
Mult(1, ., C + L).R3 -0.3883879 0.08391754
Mult(1, ., C + L).R4  0.4426954 0.08454001
```

```
                        Estimate Std. Error
Mult(1, R, . + L).C1  0.17164747 0.14998137
Mult(1, R, . + L).C2 -0.74480785 0.08583798
Mult(1, R, . + L).C3 -0.06806188 0.15421719
Mult(1, R, . + L).C4  0.64122226 0.11322236
```

```
                    Estimate Std. Error
Mult(1, R, .).L1  0.04806244 0.08932196
Mult(1, R, .).L2 -0.65804171 0.05969167
Mult(1, R, .).L3 -0.13011596 0.08427834
Mult(1, R, .).L4  0.74009523 0.04837401
```

```
# A tibble: 1 x 6
  `Model Description`    df    L2   BIC Delta     p
  <chr>               <int> <dbl> <dbl> <dbl> <dbl>
1 model15                44  56.0 -266.  6.83 0.894
```

