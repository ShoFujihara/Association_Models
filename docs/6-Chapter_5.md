
# 第5章






```r
library(tidyverse)
library(magrittr)
library(gnm)
```



```r
# Specify the constrasts
# Here, we use sum to zero contrast.
options(width=90,contrasts = c(factor="contr.sum", ordered="contr.treatment"))

# Define a function which calculation BIC based on L2 statistics
model.summary <- function(obj) {
  aic <- obj$deviance - obj$df * 2   # AIC(L2)
  bic <- obj$deviance - obj$df * log(sum(obj$y))   #BIC(L2)
  #Index of Dissimilarity (%)
  delta <- 100 * sum(abs(obj$y - obj$fitted.values)) / (2 * sum(obj$y))
  p <- pchisq(obj$deviance, obj$df, lower.tail = F)
  #p<-ifelse(p<0.001,"<0.001",p)
  result <- matrix(0, 1, 6)
  rownames(result) <- ""
  colnames(result) <- c("Df", "L2", "p", "AIC(L2)", "BIC(L2)", "Delta")
  #exp.freq=obj$fitted.values
  result[1, ] <- c(obj$df, obj$deviance, p, aic, bic, delta)
  print("Model Summary:")
  return(result)
}
```


## 表5.1

```r
# Table 5.1
Freq<-c(118, 28,  32,  6,  7,
        218, 28,  97, 12, 14,
         11,  2,   4,  1,  1,
        104, 22,  61,  8,  5,
        117, 24,  70,  9,  7,
         42,  6,  20,  2,  0,
         48, 16, 104, 14,  9,
        128, 52,  81, 14, 12)


Worries<-gl(8,5)
Situations<-gl(5,1,8*5)


# Model 1 - Independence
m1<-gnm(Freq~Worries+Situations,family=poisson,trace=F,tolerance = 1e-12)
model.summary(m1)
```

```
[1] "Model Summary:"
```

```
 Df       L2            p  AIC(L2)   BIC(L2)    Delta
 28 121.4661 1.307612e-13 65.46607 -84.29438 9.844607
```

```r
# Model 2 - RC(1)
m2<-gnm(Freq~Worries+Situations+Mult(1,Worries,Situations),family=poisson,
         trace=F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................................................................
..........................................................................................
..........................................................................................
..........................................................................................
..........................................................................................
.........................................................................
Done
```

```r
model.summary(m2)
```

```
[1] "Model Summary:"
```

```
 Df       L2            p  AIC(L2)   BIC(L2)    Delta
 27 121.4874 5.995511e-14 67.48739 -76.92448 9.838695
```

```r
# Model 3 - RC(1) with equality constraints on
# MIL=ECO=MTO, ENR=SAB=OTH and ASAF=IFAA
Worries.a<-Worries
levels(Worries.a)<-factor(c(1,2,2,3,3,2,4,3))
Worries.a
```

```
 [1] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 4 4 4 4 4 3 3 3 3 3
Levels: 1 2 3 4
```

```r
Situations.a<-Situations
levels(Situations.a)<-factor(c(1,2,3,3,4))
Situations.a
```

```
 [1] 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4
Levels: 1 2 3 4
```

```r
m3.un<-gnm(Freq~Worries+Situations+Mult(1,Worries.a,Situations.a),family=poisson,
         trace=F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................................................................
...............................................
Done
```

```r
model.summary(m3.un)
```

```
[1] "Model Summary:"
```

```
 Df       L2             p  AIC(L2)  BIC(L2)    Delta
 32 925.4714 8.207554e-174 861.4714 690.3166 30.30552
```

```r
mu<-getContrasts(m3.un, pickCoef(m3.un, "[.]Worries.a"),
                ref = c(1,3,3,1)/8, scaleRef = c(1,3,3,1)/8,
                scaleWeights = c(1,3,3,1))
nu<-getContrasts(m3.un, pickCoef(m3.un, "[.]Situations.a"),
                ref = c(1,1,2,1)/5, scaleRef = c(1,1,2,1)/5,
                scaleWeights = c(1,1,2,1))

con<-c(mu$qvframe[,1][c(1,4)],nu$qvframe[,1][c(1,4)])
m3<-gnm(Freq~Worries+Situations+Mult(1,Worries.a,Situations.a),family=poisson,
             constrain=c(14,17,18,21),constrainTo=con,
             trace=F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................................................................
...........................................................................
Done
```

```r
summary(m3);mu;nu;model.summary(m3.un)
```

```

Call:
gnm(formula = Freq ~ Worries + Situations + Mult(1, Worries.a, 
    Situations.a), constrain = c(14, 17, 18, 21), constrainTo = con, 
    family = poisson, tolerance = 1e-12, trace = F)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-11.364   -4.566   -1.177    1.247   11.623  

Coefficients:
                                    Estimate Std. Error z value Pr(>|z|)    
(Intercept)                          3.50501    0.04289  81.724  < 2e-16 ***
Worries1                             0.00000         NA      NA       NA    
Worries2                             0.00000         NA      NA       NA    
Worries3                             0.00000         NA      NA       NA    
Worries4                             0.00000         NA      NA       NA    
Worries5                             0.00000         NA      NA       NA    
Worries6                             0.00000         NA      NA       NA    
Worries7                             0.00000         NA      NA       NA    
Situations1                          0.00000         NA      NA       NA    
Situations2                          0.00000         NA      NA       NA    
Situations3                          0.00000         NA      NA       NA    
Situations4                          0.00000         NA      NA       NA    
Mult(., Worries.a, Situations.a).    0.45649    0.46489   0.982 0.326138    
Mult(1, ., Situations.a).Worries.a1  0.11313         NA      NA       NA    
Mult(1, ., Situations.a).Worries.a2 -3.63355    3.70632  -0.980 0.326906    
Mult(1, ., Situations.a).Worries.a3 -4.05192    4.13163  -0.981 0.326737    
Mult(1, ., Situations.a).Worries.a4 -0.35379         NA      NA       NA    
Mult(1, Worries.a, .).Situations.a1 -0.66229         NA      NA       NA    
Mult(1, Worries.a, .).Situations.a2  0.21821    0.05836   3.739 0.000185 ***
Mult(1, Worries.a, .).Situations.a3  0.01290    0.03812   0.338 0.734996    
Mult(1, Worries.a, .).Situations.a4  0.72845         NA      NA       NA    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for poisson family taken to be 1)

Std. Error is NA where coefficient has been constrained or is unidentified

Residual deviance: 1162.5 on 34 degrees of freedom
AIC: 1360.8

Number of iterations: 142
```

```
                                      Estimate Std. Error
Mult(1, ., Situations.a).Worries.a1  0.1131301 0.11361061
Mult(1, ., Situations.a).Worries.a2  0.4170232 0.02269737
Mult(1, ., Situations.a).Worries.a3 -0.3368043 0.03042467
Mult(1, ., Situations.a).Worries.a4 -0.3537867 0.08354914
```

```
                                       Estimate Std. Error
Mult(1, Worries.a, .).Situations.a1 -0.66229120 0.02512458
Mult(1, Worries.a, .).Situations.a2  0.11763705 0.03837698
Mult(1, Worries.a, .).Situations.a3 -0.09189854 0.01868966
Mult(1, Worries.a, .).Situations.a4  0.72845124 0.02397519
```

```
[1] "Model Summary:"
```

```
 Df       L2             p  AIC(L2)  BIC(L2)    Delta
 32 925.4714 8.207554e-174 861.4714 690.3166 30.30552
```

```r
# Model 4 - RC(1) with equality constraints on
# MIL=ECO=MTO=ENR=SAB=OTH and ASAF=IFAA
Worries.b<-Worries
levels(Worries.b)<-factor(c(1,2,2,2,2,2,3,2))
Worries.b
```

```
 [1] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 2 2 2 2 2
Levels: 1 2 3
```

```r
m4.un<-gnm(Freq~Worries+Situations+Mult(1,Worries.b,Situations.a),family=poisson,
         trace=F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations............................................................
Done
```

```r
model.summary(m4.un)
```

```
[1] "Model Summary:"
```

```
 Df       L2             p  AIC(L2)  BIC(L2)   Delta
 33 964.1288 3.382326e-181 898.1288 721.6254 30.4467
```

```r
# Model 5
m5.un<-gnm(Freq~Worries+Situations+Mult(1,Worries,Situations,inst=1)
         +Mult(1,Worries,Situations,inst=2),family=poisson,
         trace=F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................................................................
..........................................................................................
..........................................................................................
..........................................................................................
..........................................................................................
.........................................................................
Done
```

```r
model.summary(m5.un)
```

```
[1] "Model Summary:"
```

```
 Df       L2          p   AIC(L2)   BIC(L2)    Delta
 17 29.36242 0.03132697 -4.637583 -95.56357 3.927879
```

```r
# Model 6 - RC(2) with equality constraints on
# ECO=MTO=MIL=ENR=SAB and ASAF=IFAA in both dimensions
Worries.c<-Worries
levels(Worries.c)<-factor(c(1,2,2,2,2,2,3,4))
Worries.c
```

```
 [1] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4
Levels: 1 2 3 4
```

```r
set.seed(12345)
m6.un<-gnm(Freq~Worries+Situations+Mult(1,Worries.c,Situations.a,inst=1)
         +Mult(1,Worries.c,Situations.a,inst=2),family=poisson,
         trace=F,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations...................................................................
...
Done
```

```r
model.summary(m6.un)
```

```
[1] "Model Summary:"
```

```
 Df       L2             p  AIC(L2)  BIC(L2)    Delta
 27 853.0971 2.550771e-162 799.0971 654.6853 27.69847
```


## 表5.4


```r
# Table 5.4
Freq<-c(1096,  1847,  1255,  925,    3321,  6123,  6830,  5524,
        1541,  3134,  3145, 3300,    1915,  4429,  7035,  9421,
        4183,  5139,  1857, 1272,    8080,  8586,  4788,  4294,
        6033,  9211,  5046, 1058,   28130, 44589, 20074,  3408,
        4354, 13430, 18670, 9821,    2250,  9075, 18286, 14358,
       14587, 31470, 16390, 3751,    8242, 17532, 12825,  3956,
        1517,  5820,  6197, 2372,     721,  2909,  4141,  2070,
        3581,  9268,  5463, 1007,    1341,  3808,  3163,   815,
        1454,  3109,  1055,  888,     563,  1909,  1018,  1051,
        3237,  3851,   377,  102,     731,   858,   247,    84,
       14882, 22182,  5363, 1136,   11650, 15818,  5524,  2122,
        6033,  3475,    63,   18,    1603,  1005,    30,    16,
        
        5968,  8783,  7701, 6483,    8733, 14329, 19386, 28143,
        1011,  2162,  3536, 6649,     697,  1299,  2362, 10796,
        3214,  3621,  2485, 3177,     793,  1134,  1292,  3597,
       11532, 16837,  6975, 1839,    2563,  2995,  2060,  1600,
        1009,  2719,  3521, 3409,     296,   503,   626,  1273,
        1586,  3025,  1726,  668,     245,   415,   238,   218,
         387,   941,   564,  316,      86,   138,    79,    48,
         994,  1988,   542,  145,     158,   259,   101,    56,
         171,   409,   223,  245,      65,   172,    99,   174,
         293,   290,    67,   31,      32,    62,    18,    30,
        4288,  4916,  1452,  766,     616,   794,   347,   300,
         370,   186,     3,    4,      67,    37,     5,     2)

Income<-gl(4,1,192)
Occ<-gl(12,8,192)
E1<-gl(2,4,192)
E2<-gl(2,96,192)
Educ<-E2:E1
levels(Educ)<-1:4

#nEduc<-as.numeric(Educ)
#nAttitude<-as.numeric(Attitude)
#Attitude.c<-Attitude
#levels(Attitude.c)<-c("1","2","3","3")
Table<-data.frame(Freq,Income,Occ,Educ)
########################################################################################

# Table 5.5
# Model 1: Complete Independence
m1<-gnm(Freq~Educ+Occ+Income,data=Table,family=poisson,tolerance = 1e-12)
model.summary(m1) 
```

```
[1] "Model Summary:"
```

```
  Df       L2 p  AIC(L2)  BIC(L2)    Delta
 174 586906.2 0 586558.2 584536.9 33.84897
```

```r
# Model 2: Conditional Independence
m2<-gnm(Freq~Educ*Occ+Income*Occ,data=Table,family=poisson,tolerance = 1e-12)
model.summary(m2)
```

```
[1] "Model Summary:"
```

```
  Df      L2 p AIC(L2)  BIC(L2)    Delta
 108 27957.4 0 27741.4 26486.78 6.003195
```

```r
# Model 3: All two-way interaction
m3<-gnm(Freq~Educ*Occ+Income*Occ+Educ*Income,data=Table,family=poisson,tolerance = 1e-12)
model.summary(m3)
```

```
[1] "Model Summary:"
```

```
 Df       L2 p  AIC(L2)  BIC(L2)    Delta
 99 6540.396 0 6342.396 5192.331 2.635334
```

```r
# Model 4: RC(1)+RL(1) partial association
m4<-gnm(Freq~Educ+Occ+Income+Mult(1,Occ,Educ)+Mult(1,Occ,Income),
        data=Table,family=poisson,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
........
Done
```

```r
model.summary(m4)
```

```
[1] "Model Summary:"
```

```
  Df       L2 p  AIC(L2) BIC(L2)    Delta
 148 70860.99 0 70564.99 68845.7 11.16939
```

```r
# Model 5: Model 4 with consistent row (occupation) scores
m5<-gnm(Freq~Educ+Occ+Income+Mult(1,Occ,Educ+Income),
        data=Table,family=poisson,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
................................................................................
..........................................
Done
```

```r
model.summary(m5)
```

```
[1] "Model Summary:"
```

```
  Df       L2 p  AIC(L2)  BIC(L2)    Delta
 158 185515.3 0 185199.3 183363.8 18.26686
```

```r
mu<-getContrasts(m5, pickCoef(m5, "[.]Occ")[1:12],
       ref = "mean", scaleRef = "mean",scaleWeights = "unit")
mu
```

```
                                   Estimate  Std. Error
Mult(1, ., Educ + Income).Occ1  -0.54805987 0.001793134
Mult(1, ., Educ + Income).Occ2  -0.38746682 0.001271727
Mult(1, ., Educ + Income).Occ3  -0.21717857 0.001291161
Mult(1, ., Educ + Income).Occ4  -0.14398575 0.001002368
Mult(1, ., Educ + Income).Occ5  -0.08607017 0.001273939
Mult(1, ., Educ + Income).Occ6   0.15776750 0.001820890
Mult(1, ., Educ + Income).Occ7   0.05029005 0.002500252
Mult(1, ., Educ + Income).Occ8   0.14092026 0.002598170
Mult(1, ., Educ + Income).Occ9   0.02379483 0.003466324
Mult(1, ., Educ + Income).Occ10  0.39930723 0.004697125
Mult(1, ., Educ + Income).Occ11  0.10479535 0.001779509
Mult(1, ., Educ + Income).Occ12  0.50588597 0.004258340
```

```r
# Model 6: RC(1)+RL(1)+CL(1) partial association
m6 <- gnm(Freq~Educ+Occ+Income+Mult(1,Occ,Educ)+Mult(1,Occ,Income)+Mult(1,Educ,Income),
        data=Table,family=poisson,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
......................
Done
```

```r
model.summary(m6)
```

```
[1] "Model Summary:"
```

```
  Df       L2 p  AIC(L2)  BIC(L2)    Delta
 143 42101.44 0 41815.44 40154.24 8.274821
```

```r
# Model 7: Model 6 with consistent row (occupation) scores
m7 <- gnm(Freq~Educ+Occ+Income+Mult(1,Occ,Educ+Income)+Mult(1,Educ,Income),
        data=Table,family=poisson,tolerance = 1e-12)
```

```
Initialising
Running start-up iterations..
Running main iterations.........................................................
................................................................................
.........................................................................
Done
```

```r
model.summary(m7)
```

```
[1] "Model Summary:"
```

```
  Df       L2 p  AIC(L2)  BIC(L2)    Delta
 153 174073.1 0 173767.1 171989.8 17.80335
```

```r
mu <- getContrasts(m7, pickCoef(m7, "[.]Occ"),
                 ref = "mean", scaleRef = "mean",scaleWeights = "unit")
mu
```

```
                                   Estimate  Std. Error
Mult(1, ., Educ + Income).Occ1   0.56931429 0.001883457
Mult(1, ., Educ + Income).Occ2   0.37928161 0.001275097
Mult(1, ., Educ + Income).Occ3   0.22247516 0.001381547
Mult(1, ., Educ + Income).Occ4   0.17045561 0.001046926
Mult(1, ., Educ + Income).Occ5   0.05464251 0.001423421
Mult(1, ., Educ + Income).Occ6  -0.16380722 0.001871288
Mult(1, ., Educ + Income).Occ7  -0.07890623 0.002667870
Mult(1, ., Educ + Income).Occ8  -0.14504188 0.002680434
Mult(1, ., Educ + Income).Occ9  -0.04282708 0.003676179
Mult(1, ., Educ + Income).Occ10 -0.39013284 0.004810546
Mult(1, ., Educ + Income).Occ11 -0.09178943 0.001794307
Mult(1, ., Educ + Income).Occ12 -0.48366450 0.004397647
```

```r
# Model 8: model 6 with consistent row, column and layer scores
# currently, this might not be fitted with gnm!
```

