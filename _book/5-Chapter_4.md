# 第4章


```
## [1] 1000
```



```r
library(tidyverse)
library(magrittr)
library(gnm)
```

## 表4.3


```r
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

# Table 4.3
Freq <- c(201,  29,   8,  13,   5,     152,  29,   2,   8,   0,
        18,   6,   3,   6,   0,      17,  12,   0,   3,   0,
        109,  74, 164,  89,  16,     101, 336,   9, 134,   2,
        7,   6,  45,  30,   6,       7,  41,   7,  63,   0,
        247,  58,  20,  23,   2,     288,  51,   1,  17,   3,
        48,  11,  16,  13,   1,      47,  38,   2,  18,   0,
        157,  68, 178, 116,  27,     165, 321,  27, 168,   1,
        7,   7,  50,  42,   5,      12,  25,   5,  29,   6)

Educ <- gl(4, 10, 4 * 5 * 2 * 2)
Occ <- gl(5, 1, 4 * 5 * 2 * 2)
Sex <- gl(2, 5, 4 * 5 * 2 * 2)
Year <- gl(2, 40, 4 * 5 * 2 * 2)
L <- Year:Sex
levels(L) <- factor(1:4)


Rscore <- as.numeric(Educ)
Cscore <- as.numeric(Occ)



# Model 1 - RC(0)-L(homogeneous)
model1 <-
  gnm(
    Freq ~ Educ + Occ + L + Educ:L + Occ:L,
    family = poisson,
    trace = F,
    tolerance = 1e-12
  )
summary(model1)
```

```
FALSE 
FALSE Call:
FALSE gnm(formula = Freq ~ Educ + Occ + L + Educ:L + Occ:L, family = poisson, 
FALSE     tolerance = 1e-12, trace = F)
FALSE 
FALSE Deviance Residuals: 
FALSE     Min       1Q   Median       3Q      Max  
FALSE -9.2080  -2.2181  -0.4393   1.7534  10.3493  
FALSE 
FALSE Coefficients:
FALSE             Estimate Std. Error z value Pr(>|z|)    
FALSE (Intercept)  4.63188    0.07546  61.385  < 2e-16 ***
FALSE Educ2       -2.04867    0.18496 -11.076  < 2e-16 ***
FALSE Educ3        0.56850    0.07822   7.268 3.65e-13 ***
FALSE Educ4       -1.00188    0.12060  -8.307  < 2e-16 ***
FALSE Occ2        -1.06920    0.10808  -9.893  < 2e-16 ***
FALSE Occ3        -0.42050    0.08678  -4.846 1.26e-06 ***
FALSE Occ4        -0.88688    0.10115  -8.768  < 2e-16 ***
FALSE Occ5        -2.51829    0.20006 -12.588  < 2e-16 ***
FALSE L2          -0.58321    0.11600  -5.028 4.96e-07 ***
FALSE L3           0.35568    0.09908   3.590 0.000331 ***
FALSE L4           0.38267    0.09802   3.904 9.46e-05 ***
FALSE Educ2:L2     0.26213    0.26589   0.986 0.324189    
FALSE Educ3:L2     0.54569    0.11433   4.773 1.82e-06 ***
FALSE Educ4:L2     0.52029    0.16809   3.095 0.001966 ** 
FALSE Educ2:L3     0.67937    0.21978   3.091 0.001994 ** 
FALSE Educ3:L3    -0.12382    0.10396  -1.191 0.233635    
FALSE Educ4:L3    -0.14652    0.16251  -0.902 0.367276    
FALSE Educ2:L4     0.81653    0.21566   3.786 0.000153 ***
FALSE Educ3:L4     0.07042    0.10180   0.692 0.489078    
FALSE Educ4:L4    -0.54042    0.17410  -3.104 0.001908 ** 
FALSE Occ2:L2      1.48066    0.13298  11.135  < 2e-16 ***
FALSE Occ3:L2     -2.31314    0.25826  -8.957  < 2e-16 ***
FALSE Occ4:L2      0.60040    0.13656   4.397 1.10e-05 ***
FALSE Occ5:L2     -2.41258    0.73731  -3.272 0.001067 ** 
FALSE Occ2:L3     -0.09004    0.14424  -0.624 0.532465    
FALSE Occ3:L3     -0.13260    0.11618  -1.141 0.253725    
FALSE Occ4:L3      0.02568    0.13253   0.194 0.846332    
FALSE Occ5:L3     -0.05541    0.26603  -0.208 0.835011    
FALSE Occ2:L4      0.90622    0.12622   7.179 7.00e-13 ***
FALSE Occ3:L4     -2.26247    0.19508 -11.598  < 2e-16 ***
FALSE Occ4:L4      0.09529    0.12843   0.742 0.458122    
FALSE Occ5:L4     -1.41745    0.37680  -3.762 0.000169 ***
FALSE ---
FALSE Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
FALSE 
FALSE (Dispersion parameter for poisson family taken to be 1)
FALSE 
FALSE Residual deviance: 1370.9 on 48 degrees of freedom
FALSE AIC: 1796.7
FALSE 
FALSE Number of iterations: 6
```

```r
model.summary(model1)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2             p  AIC(L2)  BIC(L2)    Delta
FALSE  48 1370.934 1.366344e-255 1274.934 971.8922 23.75048
```

```r
# Model 2 - RC(1)-L(homogeneous)
model2 <- gnm(
  Freq ~ Educ + Occ + L + Educ:L + Occ:L + Mult(1, Educ, Occ)
  ,
  family = poisson,
  trace = F,
  tolerance = 1e-12
)
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations............................
FALSE Done
```

```r
model.summary(model2)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2            p  AIC(L2)   BIC(L2)    Delta
FALSE  42 143.8274 4.537907e-13 59.82743 -205.3338 6.388286
```

```r
# Model 3 - RC(1)-L(heterogeneous)
model3 <- gnm(
  Freq ~ Educ + Occ + L + Educ:L + Occ:L + Mult(L, L:Educ, L:Occ)
  ,
  family = poisson,
  trace = F,
  tolerance = 1e-12
)
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations.........................................................
FALSE .................................
FALSE Done
```

```r
model.summary(model3)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2           p  AIC(L2)   BIC(L2)    Delta
FALSE  24 69.06197 3.02988e-06 21.06197 -130.4587 3.038593
```

```r
# Model 4 - RC(2)-L(homogeneous)
model4 <-
  gnm(
    Freq ~ Educ + Occ + L + Educ:L + Occ:L + instances(Mult(1, Educ, Occ), 2)
    ,
    family = poisson,
    trace = F,
    tolerance = 1e-8
  )
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations.................
FALSE Done
```

```r
model.summary(model4)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2            p  AIC(L2)  BIC(L2)    Delta
FALSE  38 117.3838 4.933675e-10 41.38378 -198.524 5.236715
```

```r
# Model 4 - RC(2)-L(heterogeneous)
model5 <-
  gnm(
    Freq ~ Educ + Occ + L + Educ:L + Occ:L + instances(Mult(L, L:Educ, L:Occ), 2),
    family = poisson,
    trace = F,
    tolerance = 1e-10,
    iterStart = 5,
    iterMax = 1000000,
    verbose = F
  )
model.summary(model5)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2         p   AIC(L2)   BIC(L2)    Delta
FALSE   8 5.833864 0.6658353 -10.16614 -60.67303 0.436722
```

```r
# Model 6 - RC(3)-L(homogeneous)
model6 <-
  gnm(
    Freq ~ Educ + Occ + L + Educ:L + Occ:L + instances(Mult(1, Educ, Occ), 3)
    ,
    family = poisson,
    trace = F,
    tolerance = 1e-12
  )
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations.....
FALSE Done
```

```r
model.summary(model6)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2           p  AIC(L2)   BIC(L2)    Delta
FALSE  36 113.1797 6.60068e-10 41.17975 -186.1013 5.186927
```

```r
# Model 7 - RC(3)-L(heterogeneous)
model6 <-
  gnm(
    Freq ~ Educ + Occ + L + Educ:L + Occ:L + instances(Mult(L, L:Educ, L:Occ), 3),
    family = poisson,
    trace = F,
    tolerance = 1e-8
  )
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations.........................................................
FALSE ................................................................................
FALSE ................................................................................
FALSE ................................................................................
FALSE ................................................................................
FALSE ................................................................................
FALSE ....................................
FALSE Done
```

```r
model.summary(model6)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df           L2 p      AIC(L2)      BIC(L2)        Delta
FALSE   0 2.103745e-11 0 2.103745e-11 2.103745e-11 2.708683e-09
```

```r
# Model 98- U+RC(homogeneous)
model8 <-
  gnm(
    Freq ~ Educ + Occ + L + Educ:L + Occ:L + Rscore:Cscore + Mult(1, Educ, Occ),
    family = poisson,
    trace = F,
    tolerance = 1e-6,
    iterStart = 20,
    iterMax = 100000
  )
```

```
FALSE Initialising
FALSE Running start-up iterations....................
FALSE Running main iterations..............
FALSE Done
```

```r
model.summary(model8)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2            p  AIC(L2)   BIC(L2)    Delta
FALSE  41 124.7511 2.182856e-10 42.75111 -216.0967 5.580511
```

```r
model8$deviance - model8$df * log(4078)
```

```
FALSE [1] -216.0967
```

```r
# Model 8 - U+RC(heterogeneous)
Rscore <- as.numeric(Educ)
Cscore <- as.numeric(Occ)
model9 <-
  gnm(
    Freq ~ Educ + Occ + L + Educ:L + Occ:L + Rscore:Cscore:L + Mult(L, L:Educ, L:Occ),
    family = poisson,
    trace = F,
    tolerance = 1e-6,
    iterStart = 20,
    iterMax = 100000,
    verbose = F
  )
model.summary(model9)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2         p   AIC(L2)   BIC(L2)    Delta
FALSE  20 27.47202 0.1224988 -12.52798 -138.7952 1.496033
```

```r
model9$deviance - model9$df * log(4078)
```

```
FALSE [1] -138.7952
```

```r
model9$deviance - 20 * log(4078)
```

```
FALSE [1] -138.7952
```

```r
model9$deviance - 22 * log(4078)
```

```
FALSE [1] -155.4219
```

```r
pchisq(27.47, 20, lower.tail = FALSE)
```

```
FALSE [1] 0.1225512
```

```r
pchisq(27.47, 22, lower.tail = FALSE)
```

```
FALSE [1] 0.1939168
```


## 表4.6


```r
# Table 4.6
Freq <- c(201, 29, 8,13, 5, 152,29, 2, 8, 0,
          18, 6, 3, 6, 0,17,12, 0, 3, 0,
          109,74, 164,89,16, 101, 336, 9, 134, 2,
          7, 6,45,30, 6, 7,41, 7,63, 0,
          247,58,20,23, 2, 288,51, 1,17, 3,
          48,11,16,13, 1,47,38, 2,18, 0,
          157,68, 178, 116,27, 165, 321,27, 168, 1,
          7, 7,50,42, 5,12,25, 5,29, 6)

Educ <- gl(4, 10, 4 * 5 * 2 * 2)
Occ <- gl(5, 1, 4 * 5 * 2 * 2)
Sex <- gl(2, 5, 4 * 5 * 2 * 2)
Year <- gl(2, 40, 4 * 5 * 2 * 2)
L <- Year:Sex
levels(L) <- factor(1:4)
Rscore <- as.numeric(Educ)
Cscore <- as.numeric(Occ)

# Model 1
model1.un <- gnm(
  Freq ~ Educ + Occ + L + Educ:L + Occ:L +
    Mult(L, L:Educ, Occ, inst = 1) +
    Mult(L, L:Educ, Occ, inst = 2),
  family = poisson,
  trace = F,
  tolerance = 1e-8,
  iterStart = 20,
  iterMax = 100000,
  verbose = F)
model.summary(model1.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2          p   AIC(L2)   BIC(L2)    Delta
FALSE  20 29.24633 0.08302108 -10.75367 -137.0209 1.824495
```

```r
# Model 2
model2.un <- gnm(
  Freq ~ Educ + Occ + L + Educ:L + Occ:L + Mult(L, Educ, L:Occ, inst = 1)
    + Mult(L, Educ, L:Occ, inst = 2),
    family = poisson,
    trace = F,
    tolerance = 1e-8,
    iterStart = 20,
    iterMax = 100000,
    verbose = F
  )
model.summary(model2.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2         p   AIC(L2)   BIC(L2)     Delta
FALSE  14 7.969585 0.8909046 -20.03042 -108.4175 0.6869727
```

```r
# Model 3
# it might not be fitted with gnm!

# Model 4
# it might not be fitted with gnm!

# Model 5
# it might not be fitted with gnm!

# Model 6
# it might not be fitted with gnm!

# Model 7
# it might not be fitted with gnm!

# Model 8
# it might not be fitted with gnm!

# Model 9
model9.un <- gnm(
  Freq ~ Educ + Occ + L + Educ:L + Occ:L + Mult(L, Educ, Occ, inst = 1)
  + Mult(L, Educ, Occ, inst = 2),
  family = poisson,
  trace = F,
  tolerance = 1e-12,
  iterStart = 20,
  iterMax = 100000
)
```

```
FALSE Initialising
FALSE Running start-up iterations....................
FALSE Running main iterations.........................................................
FALSE .................................................
FALSE Done
```

```r
model.summary(model9.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2         p   AIC(L2)  BIC(L2)    Delta
FALSE  30 38.46182 0.1382491 -21.53818 -210.939 2.123425
```

```r
mu1 <- getContrasts(
  model9.un,
  pickCoef(model9.un, "[.]Educ")[1:4],
  ref = "mean",
  scaleRef = "mean",
  scaleWeights = "unit"
)

nu1 <- getContrasts(
  model9.un,
  pickCoef(model9.un, "[.]Occ")[1:5],
  ref = "mean",
  scaleRef = "mean",
  scaleWeights = "unit"
)

mu2 <- getContrasts(
  model9.un,
  pickCoef(model9.un, "[.]Educ")[5:8],
  ref = "mean",
  scaleRef = "mean",
  scaleWeights = "unit"
)

nu2 <- getContrasts(
  model9.un,
  pickCoef(model9.un, "[.]Occ")[6:10],
  ref = "mean",
  scaleRef = "mean",
  scaleWeights = "unit"
)

con <- c(mu1$qvframe[, 1][c(1, 4)], nu1$qvframe[, 1][c(1, 5)],
         mu2$qvframe[, 1][c(1, 4)], nu2$qvframe[, 1][c(1, 5)])


model9 <- gnm(
  Freq ~ Educ + Occ + L + Educ:L + Occ:L + Mult(L, Educ, Occ, inst = 1)
  + Mult(L, Educ, Occ, inst = 2),
  constrain = c(37, 40, 41, 45, 50, 53, 54, 58),
  constrainTo = con,
  family = poisson,
  tolerance = 1e-12,
  iterStart = 20,
  iterMax = 100000
)
```

```
FALSE Initialising
FALSE Running start-up iterations....................
FALSE Running main iterations.........................................................
FALSE ............................................
```

```r
summary(model9)
```

```
FALSE Length  Class   Mode 
FALSE      0   NULL   NULL
```

```r
mu1
```

```
FALSE                                   Estimate Std. Error
FALSE Mult(L, ., Occ, inst = 1).Educ1  0.6402222 0.02882211
FALSE Mult(L, ., Occ, inst = 1).Educ2  0.2386164 0.03949527
FALSE Mult(L, ., Occ, inst = 1).Educ3 -0.1683115 0.03139203
FALSE Mult(L, ., Occ, inst = 1).Educ4 -0.7105272 0.02071164
```

```r
nu1
```

```
FALSE                                   Estimate Std. Error
FALSE Mult(L, Educ, ., inst = 1).Occ1  0.7654028 0.02524700
FALSE Mult(L, Educ, ., inst = 1).Occ2  0.2501417 0.04589839
FALSE Mult(L, Educ, ., inst = 1).Occ3 -0.3979180 0.05170619
FALSE Mult(L, Educ, ., inst = 1).Occ4 -0.2733327 0.04831936
FALSE Mult(L, Educ, ., inst = 1).Occ5 -0.3442938 0.08637688
```

```r
mu2
```

```
FALSE                                   Estimate Std. Error
FALSE Mult(L, ., Occ, inst = 2).Educ1 -0.7309442 0.07862794
FALSE Mult(L, ., Occ, inst = 2).Educ2  0.2168538 0.14103742
FALSE Mult(L, ., Occ, inst = 2).Educ3  0.6355624 0.08287906
FALSE Mult(L, ., Occ, inst = 2).Educ4 -0.1214720 0.17120754
```

```r
nu2
```

```
FALSE                                    Estimate Std. Error
FALSE Mult(L, Educ, ., inst = 2).Occ1  0.07059438 0.10141736
FALSE Mult(L, Educ, ., inst = 2).Occ2 -0.48030206 0.07332532
FALSE Mult(L, Educ, ., inst = 2).Occ3 -0.19772154 0.10545108
FALSE Mult(L, Educ, ., inst = 2).Occ4 -0.21626011 0.06608227
FALSE Mult(L, Educ, ., inst = 2).Occ5  0.82368933 0.03866984
```

```r
model.summary(model9.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2         p   AIC(L2)  BIC(L2)    Delta
FALSE  30 38.46182 0.1382491 -21.53818 -210.939 2.123425
```

```r
# Model 10
Lc <- L
levels(Lc) <- c(1, 1, 2, 2)
model10.un <- gnm(
  Freq ~ Educ + Occ + L + Educ:L + Occ:L + Mult(Lc, Educ, Occ)
  + Mult(L, Educ, Occ),
  family = poisson,
  trace = F,
  tolerance = 1e-8,
  iterStart = 20,
  iterMax = 100000,
  verbose = F
)
model.summary(model10.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2         p   AIC(L2)   BIC(L2)    Delta
FALSE  32 40.43536 0.1455738 -23.56464 -225.5922 2.168624
```

```r
# Model 11
model11.un <- gnm(
  Freq ~ Educ + Occ + L + Educ:L + Occ:L + Mult(1, Educ, Occ)
  + Mult(L, Educ, Occ),
  family = poisson,
  trace = F,
  tolerance = 1e-8,
  iterStart = 20,
  iterMax = 100000,
  verbose = F
)
model.summary(model11.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2         p   AIC(L2)   BIC(L2)    Delta
FALSE  33 43.44178 0.1055553 -22.55822 -230.8992 2.307225
```

```r
# Model 12
L.c <- L
levels(L.c) <- factor(c(1, 2, 3, 2))

model12.un <-
  gnm(Freq ~ Educ + Occ + L + Educ:L + Occ:L + Mult(1, Educ, Occ) + Mult(L.c, Educ, Occ),
    family = poisson,
    trace = F,
    tolerance = 1e-12,
    iterStart = 20,
    iterMax = 100000
  )
```

```
FALSE Initialising
FALSE Running start-up iterations....................
FALSE Running main iterations.........................................................
FALSE ...............................................................................
FALSE Done
```

```r
model.summary(model12.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2         p   AIC(L2)   BIC(L2)    Delta
FALSE  34 44.64316 0.1046856 -23.35684 -238.0111 2.701997
```

```r
mu1 <- getContrasts(
  model12.un,
  pickCoef(model12.un, "[.]Educ")[1:4],
  ref = "mean",
  scaleRef = "mean",
  scaleWeights = "unit"
)
nu1 <- getContrasts(
  model12.un,
  pickCoef(model12.un, "[.]Occ")[1:5],
  ref = "mean",
  scaleRef = "mean",
  scaleWeights = "unit"
)

mu2 <- getContrasts(
  model12.un,
  pickCoef(model12.un, "[.]Educ")[5:8],
  ref = "mean",
  scaleRef = "mean",
  scaleWeights = "unit"
)
nu2 <- getContrasts(
  model12.un,
  pickCoef(model12.un, "[.]Occ")[6:10],
  ref = "mean",
  scaleRef = "mean",
  scaleWeights = "unit"
)

con <- c(mu1$qvframe[, 1][c(1, 4)], nu1$qvframe[, 1][c(1, 5)],
         mu2$qvframe[, 1][c(1, 4)], nu2$qvframe[, 1][c(1, 5)])

model12 <-
  gnm(Freq ~ Educ + Occ + L + Educ:L + Occ:L + Mult(1, Educ, Occ) + Mult(L.c, Educ, Occ),
    constrain = c(34, 37, 38, 42, 46, 49, 50, 54),
    constrainTo = con,
    family = poisson,
    trace = F,
    tolerance = 1e-12,
    iterStart = 20,
    iterMax = 100000
  )
```

```
FALSE Initialising
FALSE Running start-up iterations....................
FALSE Running main iterations.........................................................
FALSE ..............................................................
FALSE Done
```

```r
summary(model12)
```

```
FALSE 
FALSE Call:
FALSE gnm(formula = Freq ~ Educ + Occ + L + Educ:L + Occ:L + Mult(1, 
FALSE     Educ, Occ) + Mult(L.c, Educ, Occ), constrain = c(34, 37, 
FALSE     38, 42, 46, 49, 50, 54), constrainTo = con, family = poisson, 
FALSE     tolerance = 1e-12, iterStart = 20, iterMax = 1e+05, trace = F)
FALSE 
FALSE Deviance Residuals: 
FALSE     Min       1Q   Median       3Q      Max  
FALSE -1.5467  -0.5536  -0.0261   0.4806   1.6568  
FALSE 
FALSE Coefficients:
FALSE                          Estimate Std. Error z value Pr(>|z|)    
FALSE (Intercept)              3.831933   0.179456  21.353  < 2e-16 ***
FALSE Educ2                   -1.505076   0.229176  -6.567 5.12e-11 ***
FALSE Educ3                    1.275692   0.231604   5.508 3.63e-08 ***
FALSE Educ4                   -0.408987   0.350967  -1.165 0.243893    
FALSE Occ2                    -0.847529   0.138552  -6.117 9.53e-10 ***
FALSE Occ3                    -0.376661   0.150519  -2.502 0.012335 *  
FALSE Occ4                    -0.744515   0.145209  -5.127 2.94e-07 ***
FALSE Occ5                    -2.219419   0.258220  -8.595  < 2e-16 ***
FALSE L2                      -0.358933   0.102922  -3.487 0.000488 ***
FALSE L3                       0.262777   0.092634   2.837 0.004558 ** 
FALSE L4                       0.337839   0.088516   3.817 0.000135 ***
FALSE Educ2:L2                 0.103541   0.278147   0.372 0.709704    
FALSE Educ3:L2                 0.346651   0.146659   2.364 0.018096 *  
FALSE Educ4:L2                 0.984528   0.213599   4.609 4.04e-06 ***
FALSE Educ2:L3                 0.747897   0.225371   3.319 0.000905 ***
FALSE Educ3:L3                 0.038175   0.140601   0.272 0.785997    
FALSE Educ4:L3                -0.107116   0.192876  -0.555 0.578649    
FALSE Educ2:L4                 0.776042   0.225329   3.444 0.000573 ***
FALSE Educ3:L4                 0.051505   0.132609   0.388 0.697720    
FALSE Educ4:L4                 0.031554   0.210084   0.150 0.880610    
FALSE Occ2:L2                  1.107185   0.194363   5.696 1.22e-08 ***
FALSE Occ3:L2                 -2.749956   0.288313  -9.538  < 2e-16 ***
FALSE Occ4:L2                  0.225658   0.170627   1.323 0.185993    
FALSE Occ5:L2                 -2.649547   0.750607  -3.530 0.000416 ***
FALSE Occ2:L3                 -0.020823   0.219621  -0.095 0.924464    
FALSE Occ3:L3                  0.007572   0.167365   0.045 0.963915    
FALSE Occ4:L3                  0.121744   0.164521   0.740 0.459304    
FALSE Occ5:L3                 -0.457739   0.374537  -1.222 0.221652    
FALSE Occ2:L4                  0.707946   0.190827   3.710 0.000207 ***
FALSE Occ3:L4                 -2.361253   0.232910 -10.138  < 2e-16 ***
FALSE Occ4:L4                  0.025348   0.160516   0.158 0.874522    
FALSE Occ5:L4                 -1.295214   0.415023  -3.121 0.001803 ** 
FALSE Mult(., Educ, Occ).     -2.979351   0.336282  -8.860  < 2e-16 ***
FALSE Mult(1, ., Occ).Educ1   -0.649686         NA      NA       NA    
FALSE Mult(1, ., Occ).Educ2   -0.226686   0.060167  -3.768 0.000165 ***
FALSE Mult(1, ., Occ).Educ3    0.171250   0.042564   4.023 5.74e-05 ***
FALSE Mult(1, ., Occ).Educ4    0.705121         NA      NA       NA    
FALSE Mult(1, Educ, .).Occ1    0.747747         NA      NA       NA    
FALSE Mult(1, Educ, .).Occ2    0.279837   0.092207   3.035 0.002406 ** 
FALSE Mult(1, Educ, .).Occ3   -0.392678   0.134492  -2.920 0.003504 ** 
FALSE Mult(1, Educ, .).Occ4   -0.259076   0.118792  -2.181 0.029189 *  
FALSE Mult(1, Educ, .).Occ5   -0.375831         NA      NA       NA    
FALSE Mult(., Educ, Occ).L.c1 -0.731359   0.467211  -1.565 0.117496    
FALSE Mult(., Educ, Occ).L.c2 -1.972863   0.971962  -2.030 0.042380 *  
FALSE Mult(., Educ, Occ).L.c3  0.704917   0.420697   1.676 0.093817 .  
FALSE Mult(L.c, ., Occ).Educ1 -0.769751         NA      NA       NA    
FALSE Mult(L.c, ., Occ).Educ2  0.164999   0.321895   0.513 0.608241    
FALSE Mult(L.c, ., Occ).Educ3  0.616538   0.391128   1.576 0.114955    
FALSE Mult(L.c, ., Occ).Educ4 -0.011786         NA      NA       NA    
FALSE Mult(L.c, Educ, .).Occ1  0.043083         NA      NA       NA    
FALSE Mult(L.c, Educ, .).Occ2 -0.477126   0.188456  -2.532 0.011349 *  
FALSE Mult(L.c, Educ, .).Occ3 -0.227377   0.171559  -1.325 0.185053    
FALSE Mult(L.c, Educ, .).Occ4 -0.169317   0.115696  -1.463 0.143341    
FALSE Mult(L.c, Educ, .).Occ5  0.830738         NA      NA       NA    
FALSE ---
FALSE Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
FALSE 
FALSE (Dispersion parameter for poisson family taken to be 1)
FALSE 
FALSE Std. Error is NA where coefficient has been constrained or is unidentified
FALSE 
FALSE Residual deviance: 44.643 on 34 degrees of freedom
FALSE AIC: 498.46
FALSE 
FALSE Number of iterations: 119
```

```r
mu1
```

```
FALSE                         Estimate Std. Error
FALSE Mult(1, ., Occ).Educ1 -0.6496856 0.02689904
FALSE Mult(1, ., Occ).Educ2 -0.2266856 0.03916088
FALSE Mult(1, ., Occ).Educ3  0.1712502 0.02789996
FALSE Mult(1, ., Occ).Educ4  0.7051210 0.01869796
```

```r
nu1
```

```
FALSE                         Estimate Std. Error
FALSE Mult(1, Educ, .).Occ1  0.7477472 0.03046619
FALSE Mult(1, Educ, .).Occ2  0.2798375 0.04956910
FALSE Mult(1, Educ, .).Occ3 -0.3926778 0.05281437
FALSE Mult(1, Educ, .).Occ4 -0.2590761 0.04731457
FALSE Mult(1, Educ, .).Occ5 -0.3758307 0.08654401
```

```r
mu2
```

```
FALSE                           Estimate Std. Error
FALSE Mult(L.c, ., Occ).Educ1 -0.7697514 0.05982573
FALSE Mult(L.c, ., Occ).Educ2  0.1649990 0.14337525
FALSE Mult(L.c, ., Occ).Educ3  0.6165380 0.08295724
FALSE Mult(L.c, ., Occ).Educ4 -0.0117856 0.16680372
```

```r
nu2
```

```
FALSE                            Estimate Std. Error
FALSE Mult(L.c, Educ, .).Occ1  0.04308284 0.07694408
FALSE Mult(L.c, Educ, .).Occ2 -0.47712634 0.07295173
FALSE Mult(L.c, Educ, .).Occ3 -0.22737747 0.10213888
FALSE Mult(L.c, Educ, .).Occ4 -0.16931700 0.06314427
FALSE Mult(L.c, Educ, .).Occ5  0.83073797 0.03112819
```

```r
model.summary(model12.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2         p   AIC(L2)   BIC(L2)    Delta
FALSE  34 44.64316 0.1046856 -23.35684 -238.0111 2.701997
```

```r
# Model 13
model13.un <-
  gnm(Freq ~ Educ + Occ + L + Educ:L + Occ:L + Mult(1, Educ, Occ, inst = 1)
    + Mult(1, Educ, Occ, inst = 2),
    family = poisson,
    trace = F,
    tolerance = 1e-8,
    iterStart = 20,
    iterMax = 100000,
    verbose = F
  )
model.summary(model13.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2            p  AIC(L2)  BIC(L2)    Delta
FALSE  38 117.3838 4.933675e-10 41.38378 -198.524 5.236715
```

