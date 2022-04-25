# はじめに {-}



# シリーズ編者による序文 {-}

### 独立性の検定 {-}

- パッケージは`tidyverse`（データセットの処理のため），`magrittr`（パイプ演算子），`gnm`（連関分析の処理のため）を使用する．


```r
# パッケージの読み込み
library(tidyverse)
library(magrittr)
library(gnm)
```

- 使用するデータは精神衛生と親の社会経済的地位（SES）に関するミッドタウン・マンハッタンデータ（the Midtown Manhattan data）
- 元データについて，ここではクロス表（のようにみえる）形式で入力する．これを`Freq`とする．
- 実際は横に長い一行のベクトルなので，`matrix`関数を用いて，行列に変換する．これを`tab`する．`nrow`は行カテゴリ数，`ncol`は列カテゴリ数である．`byrow = TRUE`とすることを忘れないように注意．



```r
# 元データの入力
Freq <- c( 64,  94, 58, 46,
           57,  94, 54, 40,
           57, 105, 65, 60,
           72, 141, 77, 94,
           36,  97, 54, 78,
           21,  71, 54, 71)
# 確認
Freq
```

```
##  [1]  64  94  58  46  57  94  54  40  57 105  65  60  72 141  77  94  36  97  54
## [20]  78  21  71  54  71
```

```r
# データを表形式に変換
tab <- matrix(Freq, nrow = 6, ncol = 4, byrow = TRUE,
              dimnames = c(list(SES = LETTERS[1:6],
                                MHS = c("well", "mild", "modelrate", "impared"))))
# 確認
tab
```

```
##    MHS
## SES well mild modelrate impared
##   A   64   94        58      46
##   B   57   94        54      40
##   C   57  105        65      60
##   D   72  141        77      94
##   E   36   97        54      78
##   F   21   71        54      71
```

- 初歩的なクロス表の分析はここで作成された`tab`に対して行う．もう一度`tab`を確認してみよう．
- `as.table`によってクラスをmatrixからtableに変えることもできる．分析はどちらであっても問題ない．


```r
# 観察度数
tab
```

```
##    MHS
## SES well mild modelrate impared
##   A   64   94        58      46
##   B   57   94        54      40
##   C   57  105        65      60
##   D   72  141        77      94
##   E   36   97        54      78
##   F   21   71        54      71
```

```r
# クラスをmatrixからtableに変える
tab <- as.table(tab)
# 観察度数
tab
```

```
##    MHS
## SES well mild modelrate impared
##   A   64   94        58      46
##   B   57   94        54      40
##   C   57  105        65      60
##   D   72  141        77      94
##   E   36   97        54      78
##   F   21   71        54      71
```

- `chisq.test`関数を用いて，`tab`に対してカイ2乗検定を行う．カイ2乗値`X-squared`，自由度`df`，p値`p-value`が得られる．



```r
# 表に対してカイ2乗検定を行う
chisq.test(tab)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  tab
## X-squared = 45.985, df = 15, p-value = 5.346e-05
```

- `chisq.test(tab)`からは他にもいろいろな情報が得られる．
- ヘルプ`?chisq.test`か`names`関数で確認してみよう．



```r
# ヘルプの方法
?chisq.test
```

- `names`関数で確認．


```r
# chisq.test(tab)に含まれるオブジェクトの名前を確認
names(chisq.test(tab))
```

```
## [1] "statistic" "parameter" "p.value"   "method"    "data.name" "observed" 
## [7] "expected"  "residuals" "stdres"
```

- せっかくなので全部確認してみたい．


```r
# カイ2乗統計量
chisq.test(tab)$statistic
```

```
## X-squared 
##  45.98526
```

```r
# 自由度
chisq.test(tab)$parameter
```

```
## df 
## 15
```

```r
# p値
chisq.test(tab)$p.value
```

```
## [1] 5.345771e-05
```

```r
# 方法
chisq.test(tab)$method
```

```
## [1] "Pearson's Chi-squared test"
```

```r
# データ名
chisq.test(tab)$data.name
```

```
## [1] "tab"
```

```r
# 観測度数
chisq.test(tab)$observed
```

```
##    MHS
## SES well mild modelrate impared
##   A   64   94        58      46
##   B   57   94        54      40
##   C   57  105        65      60
##   D   72  141        77      94
##   E   36   97        54      78
##   F   21   71        54      71
```

```r
# 期待度数
chisq.test(tab)$expected
```

```
##    MHS
## SES     well      mild modelrate  impared
##   A 48.45422  95.01446  57.13494 61.39639
##   B 45.31024  88.84940  53.42771 57.41265
##   C 53.07771 104.08072  62.58675 67.25482
##   D 71.01687 139.25783  83.73976 89.98554
##   E 49.00904  96.10241  57.78916 62.09940
##   F 40.13193  78.69518  47.32169 50.85120
```

```r
# ピアソンの残差 (observed - expected) / sqrt(expected)
chisq.test(tab)$residuals
```

```
##    MHS
## SES        well        mild   modelrate     impared
##   A  2.23329871 -0.10407326  0.11444464 -1.96493132
##   B  1.73663030  0.54642528  0.07829468 -2.29805854
##   C  0.53837330  0.09010757  0.30504342 -0.88463658
##   D  0.11666251  0.14763196 -0.73650978  0.42319500
##   E -1.85826240  0.09156111 -0.49844745  2.01776175
##   F -3.02004713 -0.86745061  0.97081545  2.82552047
```

```r
# 標準化残差 (observed - expected) / sqrt(V)
chisq.test(tab)$stdres
```

```
##    MHS
## SES        well        mild   modelrate     impared
##   A  2.69558120 -0.14205313  0.14103036 -2.44697258
##   B  2.08347524  0.74134065  0.09590136 -2.84458018
##   C  0.65570340  0.12410557  0.37931249 -1.11164198
##   D  0.14738903  0.21092123 -0.95000069  0.55163334
##   E -2.24532455  0.12510918 -0.61489791  2.51546393
##   F -3.58789516 -1.16540491  1.17753617  3.46338368
```


- 期待度数は`chisq.test(tab)$expected`とすればよい．これを`tab_expected`というオブジェクトとする．

```r
# ixページの期待度数
tab_expected <- chisq.test(tab)$expected
tab_expected
```

```
##    MHS
## SES     well      mild modelrate  impared
##   A 48.45422  95.01446  57.13494 61.39639
##   B 45.31024  88.84940  53.42771 57.41265
##   C 53.07771 104.08072  62.58675 67.25482
##   D 71.01687 139.25783  83.73976 89.98554
##   E 49.00904  96.10241  57.78916 62.09940
##   F 40.13193  78.69518  47.32169 50.85120
```

- ixページの数式用いて，ピアソンの$\chi^2$統計量と尤度比統計量$L^2$を求める．
- 自由度についても`nrow`と`ncol`を用いて計算（`prod(dim(tab) -1)`でもよい）．
- `list`関数は様々なもの（値，ベクトル，データ，リスト等）を並べるときに用いる．


```r
# 適合度（X2とL2）
X2 <- ((tab - tab_expected)^2 / tab_expected) %>% sum()
L2 <- (tab * log(tab / tab_expected)) %>% sum() %>% "*"(2)
# 自由度
df <- (nrow(tab) - 1) * (ncol(tab) - 1)
# df <- prod(dim(tab) -1)
# 結果をリストで表示
list("自由度" = df,
     "ピアソンのカイ2乗統計量" = X2, 
     "尤度比統計量" = L2)
```

```
## $自由度
## [1] 15
## 
## $ピアソンのカイ2乗統計量
## [1] 45.98526
## 
## $尤度比統計量
## [1] 47.41785
```


### 一様連関モデル {-}
- ページixの一様連関モデルを再現する．

#### データの準備 {-}
- 多元表の分析はクロス表ではなく，度数，行変数，列変数からなる集計データを作成して行うことが多い．
- 先程のデータについてもクロス表ではなく，次のような集計データを作成する．





<table style="width:30%; margin-left: auto; margin-right: auto;" class="table">
<caption>(\#tab:unnamed-chunk-11)精神衛生と親の社会経済的地位（SES）に関するミッドタウン・マンハッタンデータ</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Freq </th>
   <th style="text-align:right;"> PSES </th>
   <th style="text-align:right;"> MHS </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 105 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 65 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 141 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 97 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>

- 度数については先程作成した`Freq`を使う．
- `gl`（Generate Factor Levels）によって度数に対応するカテゴリを作成する．数値で作成してもよいが，その場合は最後に`factor`に変換しておく．


```r
# 度数のベクトル
Freq
```

```
##  [1]  64  94  58  46  57  94  54  40  57 105  65  60  72 141  77  94  36  97  54
## [20]  78  21  71  54  71
```

```r
# 行変数
PSES <- gl(n = 6, k = 4)
PSES
```

```
##  [1] 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6
## Levels: 1 2 3 4 5 6
```

```r
# 列変数
MHS  <- gl(n = 4, k = 1, length = 24)
MHS
```

```
##  [1] 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4
## Levels: 1 2 3 4
```

- 以上で作成した度数（`Freq`），行変数（`PSES`），列変数（`MHS`）のベクトルを用いてデータを作成する．


```r
# 度数，行変数，列変数からなるデータを作成
d <- tibble(Freq, PSES, MHS)
# データの確認
d
```

```
## # A tibble: 24 × 3
##     Freq PSES  MHS  
##    <dbl> <fct> <fct>
##  1    64 1     1    
##  2    94 1     2    
##  3    58 1     3    
##  4    46 1     4    
##  5    57 2     1    
##  6    94 2     2    
##  7    54 2     3    
##  8    40 2     4    
##  9    57 3     1    
## 10   105 3     2    
## # … with 14 more rows
```

- このような形式のデータにすることで柔軟なモデリングを行うことができる．
- なお`gnm`パッケージには`mentalHealth`というデータがそもそも存在するのでそれを用いてもよい．
- クロス表からこのような集計データを作成する場合は`data.frame`を用いればよい．


```r
data.frame(tab)
```

```
##    SES       MHS Freq
## 1    A      well   64
## 2    B      well   57
## 3    C      well   57
## 4    D      well   72
## 5    E      well   36
## 6    F      well   21
## 7    A      mild   94
## 8    B      mild   94
## 9    C      mild  105
## 10   D      mild  141
## 11   E      mild   97
## 12   F      mild   71
## 13   A modelrate   58
## 14   B modelrate   54
## 15   C modelrate   65
## 16   D modelrate   77
## 17   E modelrate   54
## 18   F modelrate   54
## 19   A   impared   46
## 20   B   impared   40
## 21   C   impared   60
## 22   D   impared   94
## 23   E   impared   78
## 24   F   impared   71
```

- 逆に集計データからクロス表を作成するためには`xtabs`を用いる．


```r
xtabs(Freq ~ SES + MHS, data = data.frame(tab))
```

```
##    MHS
## SES well mild modelrate impared
##   A   64   94        58      46
##   B   57   94        54      40
##   C   57  105        65      60
##   D   72  141        77      94
##   E   36   97        54      78
##   F   21   71        54      71
```



#### 独立モデル {-}
- この形式のデータに対して，独立モデルによる分析を行う．これは先程の独立性の検定と同じ結果となる．
- `%>%`はパイプ演算子であり，パイプ演算子を使うと`function(x)`を`x %>% function()`とすることができ，処理の流れが分かりやすくなる．引数（argument）が複数ある場合（例えば`function(x, y)`）はパイプ演算子の左辺は1つめの引数として用いられる．つまり，`function(x, y)`は`x %>% function(y)`とすればよい．では`y %>%`とする場合は，`y %>% function(x, .)`とすることで`function(x, y)`と同じ結果を得ることができる．
- `d`というデータに対して，`gnm`を適用する．`度数 ~ 行変数 + 列変数`といった形で関連を指定する．
- モデルの分布族（family）は`poisson`（ポワソン分布）とする．これを忘れると`gaussian`（正規分布）が適用され，異なる結果が出力されるので注意する．
．`gnm`内で`data = .`となっているがこれは`data = d`とすることに等しい．
- こうして得られた分析の結果を`O`としている．


```r
# 独立モデル
O <- d %>%
  gnm(Freq ~ PSES + MHS, family = poisson, data = .)
```


- 結果は`O`（係数と最小限の適合度のみ）あるいは`summary(O)`（標準誤差やp値を含んだモデルの結果の要約）で確認できる．


```r
# 結果の表示
O
```

```
## 
## Call:
## gnm(formula = Freq ~ PSES + MHS, family = poisson, data = .)
## 
## Coefficients:
## (Intercept)        PSES2        PSES3        PSES4        PSES5        PSES6  
##     3.88062     -0.06709      0.09114      0.38230      0.01139     -0.18845  
##        MHS2         MHS3         MHS4  
##     0.67341      0.16480      0.23673  
## 
## Deviance:            47.41785 
## Pearson chi-squared: 45.98526 
## Residual df:         15
```

```r
summary(O)
```

```
## 
## Call:
## gnm(formula = Freq ~ PSES + MHS, family = poisson, data = .)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.3260  -0.7806   0.1028   0.5343   2.6643  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  3.88062    0.08045  48.238  < 2e-16 ***
## PSES2       -0.06709    0.08887  -0.755  0.45034    
## PSES3        0.09114    0.08545   1.067  0.28615    
## PSES4        0.38230    0.08013   4.771 1.83e-06 ***
## PSES5        0.01139    0.08712   0.131  0.89603    
## PSES6       -0.18845    0.09179  -2.053  0.04007 *  
## MHS2         0.67341    0.07013   9.602  < 2e-16 ***
## MHS3         0.16480    0.07759   2.124  0.03367 *  
## MHS4         0.23673    0.07634   3.101  0.00193 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
## Residual deviance: 47.418 on 15 degrees of freedom
## AIC: 209.59
## 
## Number of iterations: 4
```

- 他にもどのような情報があるのかをヘルプか`names`で確認する．


```r
names(O)
```

```
##  [1] "call"          "formula"       "terms"         "data"         
##  [5] "eliminate"     "ofInterest"    "na.action"     "xlevels"      
##  [9] "offset"        "tolerance"     "iterStart"     "iterMax"      
## [13] "coefficients"  "residuals"     "fitted.values" "rank"         
## [17] "family"        "predictors"    "deviance"      "aic"          
## [21] "iter"          "weights"       "prior.weights" "df.residual"  
## [25] "y"             "converged"     "constrain"     "constrainTo"  
## [29] "x"             "model"
```

```r
names(summary(O))
```

```
##  [1] "call"           "ofInterest"     "family"         "deviance"      
##  [5] "aic"            "df.residual"    "iter"           "deviance.resid"
##  [9] "coefficients"   "eliminated"     "dispersion"     "df"            
## [13] "cov.scaled"
```


- 結果には期待度数`fitted.values`があるので，これを用いて適合度を計算してみる．
- 観測度数については`Freq`の変わりに`O$y`を用いてもよい．これはモデル`O`で使用された従属変数`y`であり，`Freq`そのものである．


```r
# 期待度数をexpected_Oとする
observed <- O$y
expected_O <- O$fitted.values

# 行と列のカテゴリ数
I <- d$PSES %>% unique() %>% length()
J <- d$MHS %>% unique() %>% length()

# 自由度
df_O <- (I - 1) * (J - 1) 

# 適合度（X2とL2）
X2_O <- ((observed - expected_O)^2 / expected_O) %>% sum()
L2_O <- (observed * log(observed / expected_O)) %>% sum() * 2

# リストでまとめて表示
list("自由度" = df_O,
     "ピアソンのカイ2乗統計量" = X2_O,
     "尤度比統計量" = L2_O)
```

```
## $自由度
## [1] 15
## 
## $ピアソンのカイ2乗統計量
## [1] 45.98526
## 
## $尤度比統計量
## [1] 47.41785
```

- `summary(O)`では`Residual deviance: 47.418 on 15 degrees of freedom`となっており，先程の分析と適合度は一致する．


#### 一様連関モデル {-}

- 次に一様連関モデル（Uniform association model）による分析を行う．
- `PSES`を`as.integer`関数で整数にしたものを`Rscore`，`MHS`を`as.integer`関数で整数にしたものを`Cscore`として，`mutate`関数でデータに新たに変数を作成している．
- `freq_tab_intro %<>%`は`freq_tab_intro <- freq_tab_intro %>%`と同じであり，ここでは変数を追加して元のデータに上書きをしている．
- 変数の追加された`freq_tab_intro`データに対して，一様連関モデルによる分析を`gnm`パッケージで行う．
- 独立モデルとの違いは，作成した整数スコアの積`Rscore*Cscore`がモデルに追加されているだけである．


```r
# 行変数と列変数を連続した整数値とする
d %<>% 
  mutate(Rscore = as.integer(PSES),
         Cscore = as.integer(MHS))
# 一様連関モデル
U <- d %>% 
  gnm(Freq ~ PSES + MHS + Rscore*Cscore, family = poisson, data = .)
# 結果の表示
summary(U)
```

```
## 
## Call:
## gnm(formula = Freq ~ PSES + MHS + Rscore * Cscore, family = poisson, 
##     data = .)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.2663  -0.3285   0.2025   0.3912   1.0820  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)    4.08817    0.08299  49.259  < 2e-16 ***
## PSES2         -0.27661    0.09445  -2.929 0.003405 ** 
## PSES3         -0.33651    0.10841  -3.104 0.001909 ** 
## PSES4         -0.27228    0.13155  -2.070 0.038472 *  
## PSES5         -0.87902    0.16903  -5.200 1.99e-07 ***
## PSES6         -1.32360    0.20946  -6.319 2.63e-10 ***
## MHS2           0.37892    0.08360   4.533 5.82e-06 ***
## MHS3          -0.44530    0.12489  -3.566 0.000363 ***
## MHS4          -0.70991    0.17460  -4.066 4.79e-05 ***
## Rscore         0.00000         NA      NA       NA    
## Cscore         0.00000         NA      NA       NA    
## Rscore:Cscore  0.09069    0.01501   6.043 1.51e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
## Std. Error is NA where coefficient has been constrained or is unidentified
## 
## Residual deviance: 9.8951 on 14 degrees of freedom
## AIC: 174.07
## 
## Number of iterations: 3
```

```r
# 適合度（X2とL2）
observed <- U$y
expected_U <- U$fitted.values

df_U <- (I - 1)*(J - 1) - 1
X2_U <- ((observed - expected_U)^2 / expected_U) %>% sum()
L2_U <- (observed * log(observed / expected_U)) %>% sum() * 2
list("自由度" = df_U,
     "ピアソンのカイ2乗統計量" = X2_U, 
     "尤度比統計量" = L2_U)
```

```
## $自由度
## [1] 14
## 
## $ピアソンのカイ2乗統計量
## [1] 9.731848
## 
## $尤度比統計量
## [1] 9.895123
```

- 結果がixページと一致することを確認してほしい．




## 度数，行変数，列変数のデータからクロス表を作成 {-}
- 度数，行変数，列変数のデータからクロス表を作成するには`xtabs`関数を用いる．


```r
d %>% xtabs(Freq ~ PSES + MHS, data = .)
```

```
##     MHS
## PSES   1   2   3   4
##    1  64  94  58  46
##    2  57  94  54  40
##    3  57 105  65  60
##    4  72 141  77  94
##    5  36  97  54  78
##    6  21  71  54  71
```

- Rに初めから準備されている`Titanic`データは，多少特殊な集計がされているが，これに`data.frame`関数を適用すると，集計データになる．これに対して`xtabs`関数を用いればクロス表を簡単に作成できる．


```r
data.frame(Titanic) %>% xtabs(Freq ~ Class + Survived, data = .)
```

```
##       Survived
## Class   No Yes
##   1st  122 203
##   2nd  167 118
##   3rd  528 178
##   Crew 673 212
```

```r
data.frame(Titanic) %>% xtabs(Freq ~ Sex + Survived, data = .)
```

```
##         Survived
## Sex        No  Yes
##   Male   1364  367
##   Female  126  344
```

```r
data.frame(Titanic) %>% xtabs(Freq ~ Age + Survived, data = .)
```

```
##        Survived
## Age       No  Yes
##   Child   52   57
##   Adult 1438  654
```


## 個票データから集計データを作成

- 個票データから集計データを作成する方法はいくつか考えられるが，ここでは`count`関数を用いる．


```r
# スターウォーズデータ
starwars
```

```
## # A tibble: 87 × 14
##    name     height  mass hair_color skin_color eye_color birth_year sex   gender
##    <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
##  1 Luke Sk…    172    77 blond      fair       blue            19   male  mascu…
##  2 C-3PO       167    75 <NA>       gold       yellow         112   none  mascu…
##  3 R2-D2        96    32 <NA>       white, bl… red             33   none  mascu…
##  4 Darth V…    202   136 none       white      yellow          41.9 male  mascu…
##  5 Leia Or…    150    49 brown      light      brown           19   fema… femin…
##  6 Owen La…    178   120 brown, gr… light      blue            52   male  mascu…
##  7 Beru Wh…    165    75 brown      light      blue            47   fema… femin…
##  8 R5-D4        97    32 <NA>       white, red red             NA   none  mascu…
##  9 Biggs D…    183    84 black      light      brown           24   male  mascu…
## 10 Obi-Wan…    182    77 auburn, w… fair       blue-gray       57   male  mascu…
## # … with 77 more rows, and 5 more variables: homeworld <chr>, species <chr>,
## #   films <list>, vehicles <list>, starships <list>
```

```r
# 1つの変数にcount関数を適用
starwars %>% 
  count(sex)
```

```
## # A tibble: 5 × 2
##   sex                n
##   <chr>          <int>
## 1 female            16
## 2 hermaphroditic     1
## 3 male              60
## 4 none               6
## 5 <NA>               4
```

```r
# 2つの変数にcount関数を適用し，集計レベルのデータを作成
starwars %>% 
  count(sex, gender)
```

```
## # A tibble: 6 × 3
##   sex            gender        n
##   <chr>          <chr>     <int>
## 1 female         feminine     16
## 2 hermaphroditic masculine     1
## 3 male           masculine    60
## 4 none           feminine      1
## 5 none           masculine     5
## 6 <NA>           <NA>          4
```

```r
# 欠損値を処理し，nをFreqと名前を変更し，df_dstarwarsとしてデータを保存
df_dstarwars <- starwars %>% 
  count(sex, gender) %>% 
  drop_na() %>%
  rename(Freq = n)

# 確認
df_dstarwars
```

```
## # A tibble: 5 × 3
##   sex            gender     Freq
##   <chr>          <chr>     <int>
## 1 female         feminine     16
## 2 hermaphroditic masculine     1
## 3 male           masculine    60
## 4 none           feminine      1
## 5 none           masculine     5
```


## 練習問題 {-}
- せっかくデータを入力したので，このデータを使ってGoodman（1979）の表5A，表5B，表5Cの結果を再現しよう．
- 分析方法については第2章のプログラムを参考にしてほしい．

## 参考文献 {-}
- Goodman, Leo A. 1979. “Simple Models for the Analysis of Association in Cross-Classifications Having Ordered Categories.” *Journal of the American Statistical Association* 74(367):537–52.


