
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
model.summary<-function(obj){
     aic<-obj$deviance-obj$df*2   # AIC(L2)
     bic<-obj$deviance-obj$df*log(sum(obj$y))   #BIC(L2)
     #Index of Dissimilarity (%)
     delta<-100*sum(abs(obj$y-obj$fitted.values))/(2*sum(obj$y))
     p<-pchisq(obj$deviance, obj$df, lower.tail=F)
     #p<-ifelse(p<0.001,"<0.001",p)
     result<-matrix(0,1,6)
     rownames(result)<-""
     colnames(result)<-c("Df","L2","p","AIC(L2)","BIC(L2)","Delta")
     #exp.freq=obj$fitted.values
     result[1,]<-c(obj$df,obj$deviance,p,aic,bic,delta)
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
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2            p  AIC(L2)   BIC(L2)    Delta
FALSE  28 121.4661 1.307612e-13 65.46607 -84.29438 9.844607
```

```r
# Model 2 - RC(1)
m2<-gnm(Freq~Worries+Situations+Mult(1,Worries,Situations),family=poisson,
         trace=F,tolerance = 1e-12)
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations...................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE .........................................................................
FALSE Done
```

```r
model.summary(m2)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2            p  AIC(L2)   BIC(L2)    Delta
FALSE  27 121.4954 5.976324e-14 67.49542 -76.91645 9.833756
```

```r
# Model 3 - RC(1) with equality constraints on
# MIL=ECO=MTO, ENR=SAB=OTH and ASAF=IFAA
Worries.a<-Worries
levels(Worries.a)<-factor(c(1,2,2,3,3,2,4,3))
Worries.a
```

```
FALSE  [1] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 4 4 4 4 4 3 3 3 3 3
FALSE Levels: 1 2 3 4
```

```r
Situations.a<-Situations
levels(Situations.a)<-factor(c(1,2,3,3,4))
Situations.a
```

```
FALSE  [1] 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4 1 2 3 3 4
FALSE Levels: 1 2 3 4
```

```r
m3.un<-gnm(Freq~Worries+Situations+Mult(1,Worries.a,Situations.a),family=poisson,
         trace=F,tolerance = 1e-12)
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations...................................................................
FALSE ...........................................................
FALSE Done
```

```r
model.summary(m3.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2             p  AIC(L2)  BIC(L2)    Delta
FALSE  32 925.4714 8.207554e-174 861.4714 690.3166 30.30552
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
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations...................................................................
FALSE ..........................................................................................
FALSE ..............
```

```r
summary(m3);mu;nu;model.summary(m3.un)
```

```
FALSE Length  Class   Mode 
FALSE      0   NULL   NULL
```

```
FALSE                                       Estimate Std. Error
FALSE Mult(1, ., Situations.a).Worries.a1 -0.1131301 0.11361061
FALSE Mult(1, ., Situations.a).Worries.a2 -0.4170232 0.02269737
FALSE Mult(1, ., Situations.a).Worries.a3  0.3368043 0.03042467
FALSE Mult(1, ., Situations.a).Worries.a4  0.3537867 0.08354914
```

```
FALSE                                        Estimate Std. Error
FALSE Mult(1, Worries.a, .).Situations.a1  0.66229120 0.02512458
FALSE Mult(1, Worries.a, .).Situations.a2 -0.11763705 0.03837698
FALSE Mult(1, Worries.a, .).Situations.a3  0.09189854 0.01868966
FALSE Mult(1, Worries.a, .).Situations.a4 -0.72845124 0.02397519
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2             p  AIC(L2)  BIC(L2)    Delta
FALSE  32 925.4714 8.207554e-174 861.4714 690.3166 30.30552
```

```r
# Model 4 - RC(1) with equality constraints on
# MIL=ECO=MTO=ENR=SAB=OTH and ASAF=IFAA
Worries.b<-Worries
levels(Worries.b)<-factor(c(1,2,2,2,2,2,3,2))
Worries.b
```

```
FALSE  [1] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 2 2 2 2 2
FALSE Levels: 1 2 3
```

```r
m4.un<-gnm(Freq~Worries+Situations+Mult(1,Worries.b,Situations.a),family=poisson,
         trace=F,tolerance = 1e-12)
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations...................................
FALSE Done
```

```r
model.summary(m4.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2             p  AIC(L2)  BIC(L2)   Delta
FALSE  33 964.1288 3.382326e-181 898.1288 721.6254 30.4467
```

```r
# Model 5
m5.un<-gnm(Freq~Worries+Situations+Mult(1,Worries,Situations,inst=1)
         +Mult(1,Worries,Situations,inst=2),family=poisson,
         trace=F,tolerance = 1e-12)
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations...................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE .........................................................................
FALSE Done
```

```r
model.summary(m5.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2           p  AIC(L2)   BIC(L2)    Delta
FALSE  17 35.97195 0.004626194 1.971955 -88.95403 5.089345
```

```r
# Model 6 - RC(2) with equality constraints on
# ECO=MTO=MIL=ENR=SAB and ASAF=IFAA in both dimensions
Worries.c<-Worries
levels(Worries.c)<-factor(c(1,2,2,2,2,2,3,4))
Worries.c
```

```
FALSE  [1] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4
FALSE Levels: 1 2 3 4
```

```r
set.seed(12345)
m6.un<-gnm(Freq~Worries+Situations+Mult(1,Worries.c,Situations.a,inst=1)
         +Mult(1,Worries.c,Situations.a,inst=2),family=poisson,
         trace=F,tolerance = 1e-12)
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations...................................................................
FALSE ...
FALSE Done
```

```r
model.summary(m6.un)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2             p  AIC(L2)  BIC(L2)    Delta
FALSE  27 853.0971 2.550771e-162 799.0971 654.6853 27.69847
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
FALSE [1] "Model Summary:"
```

```
FALSE   Df       L2 p  AIC(L2)  BIC(L2)    Delta
FALSE  174 586906.2 0 586558.2 584536.9 33.84897
```

```r
# Model 2: Conditional Independence
m2<-gnm(Freq~Educ*Occ+Income*Occ,data=Table,family=poisson,tolerance = 1e-12)
model.summary(m2)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE   Df      L2 p AIC(L2)  BIC(L2)    Delta
FALSE  108 27957.4 0 27741.4 26486.78 6.003195
```

```r
# Model 3: All two-way interaction
m3<-gnm(Freq~Educ*Occ+Income*Occ+Educ*Income,data=Table,family=poisson,tolerance = 1e-12)
model.summary(m3)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE  Df       L2 p  AIC(L2)  BIC(L2)    Delta
FALSE  99 6540.396 0 6342.396 5192.331 2.635334
```

```r
# Model 4: RC(1)+RL(1) partial association
m4<-gnm(Freq~Educ+Occ+Income+Mult(1,Occ,Educ)+Mult(1,Occ,Income),
        data=Table,family=poisson,tolerance = 1e-12)
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations...................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE .........................................................................
FALSE Done
```

```r
model.summary(m4)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE   Df       L2 p  AIC(L2)  BIC(L2)    Delta
FALSE  161 253618.9 0 253296.9 251426.6 21.93752
```

```r
# Model 5: Model 4 with consistent row (occupation) scores
m5<-gnm(Freq~Educ+Occ+Income+Mult(1,Occ,Educ+Income),
        data=Table,family=poisson,tolerance = 1e-12)
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations...................................................................
FALSE ..........................................................................................
FALSE ..............................................
FALSE Done
```

```r
model.summary(m5)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE   Df     L2 p AIC(L2)  BIC(L2)    Delta
FALSE  176 580146 0  579794 577749.4 34.02388
```

```r
mu<-getContrasts(m5, pickCoef(m5, "[.]Occ")[1:12],
       ref = "mean", scaleRef = "mean",scaleWeights = "unit")
mu
```

```
FALSE                                    Estimate   Std. Error
FALSE Mult(1, ., Educ + Income).Occ1   0.33839063 0.0008640428
FALSE Mult(1, ., Educ + Income).Occ2   0.21498414 0.0005596694
FALSE Mult(1, ., Educ + Income).Occ3   0.16684069 0.0007135237
FALSE Mult(1, ., Educ + Income).Occ4   0.27819124 0.0005931120
FALSE Mult(1, ., Educ + Income).Occ5   0.18522878 0.0006430725
FALSE Mult(1, ., Educ + Income).Occ6   0.17800946 0.0006694356
FALSE Mult(1, ., Educ + Income).Occ7  -0.12247946 0.0020470139
FALSE Mult(1, ., Educ + Income).Occ8  -0.05945973 0.0017761063
FALSE Mult(1, ., Educ + Income).Occ9  -0.36163327 0.0028438260
FALSE Mult(1, ., Educ + Income).Occ10 -0.52644741 0.0029566216
FALSE Mult(1, ., Educ + Income).Occ11  0.16967565 0.0007019976
FALSE Mult(1, ., Educ + Income).Occ12 -0.46130072 0.0029820415
```

```r
# Model 6: RC(1)+RL(1)+CL(1) partial association
m6<-gnm(Freq~Educ+Occ+Income+Mult(1,Occ,Educ)+Mult(1,Occ,Income)+Mult(1,Educ,Income),
        data=Table,family=poisson,tolerance = 1e-12)
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations...................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE .........................................................................
FALSE Done
```

```r
model.summary(m6)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE   Df       L2 p  AIC(L2)  BIC(L2)    Delta
FALSE  154 167714.2 0 167406.2 165617.2 16.84677
```

```r
# Model 7: Model 6 with consistent row (occupation) scores
m7<-gnm(Freq~Educ+Occ+Income+Mult(1,Occ,Educ+Income)+Mult(1,Educ,Income),
        data=Table,family=poisson,tolerance = 1e-12)
```

```
FALSE Initialising
FALSE Running start-up iterations..
FALSE Running main iterations...................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE ..........................................................................................
FALSE ...........
FALSE Done
```

```r
model.summary(m7)
```

```
FALSE [1] "Model Summary:"
```

```
FALSE   Df       L2 p  AIC(L2) BIC(L2)    Delta
FALSE  169 545514.3 0 545176.3  543213 31.30187
```

```r
mu<-getContrasts(m7, pickCoef(m7, "[.]Occ"),
                 ref = "mean", scaleRef = "mean",scaleWeights = "unit")
mu
```

```
FALSE                                    Estimate  Std. Error
FALSE Mult(1, ., Educ + Income).Occ1  -0.60162667 0.001624233
FALSE Mult(1, ., Educ + Income).Occ2  -0.17978630 0.001640694
FALSE Mult(1, ., Educ + Income).Occ3  -0.03148963 0.001714147
FALSE Mult(1, ., Educ + Income).Occ4   0.39959683 0.001127586
FALSE Mult(1, ., Educ + Income).Occ5   0.32643104 0.001187228
FALSE Mult(1, ., Educ + Income).Occ6   0.40809823 0.001123581
FALSE Mult(1, ., Educ + Income).Occ7  -0.05232457 0.001723636
FALSE Mult(1, ., Educ + Income).Occ8  -0.03112601 0.001713929
FALSE Mult(1, ., Educ + Income).Occ9  -0.17530724 0.001647652
FALSE Mult(1, ., Educ + Income).Occ10 -0.18239249 0.001636530
FALSE Mult(1, ., Educ + Income).Occ11  0.28025194 0.001245468
FALSE Mult(1, ., Educ + Income).Occ12 -0.16032512 0.001669009
```

```r
# Model 8: model 6 with consistent row, column and layer scores
# currently, this might not be fitted with gnm!
```

