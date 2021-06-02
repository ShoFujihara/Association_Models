# はじめに {-}




## 本ページの目的 {-}

- ウォン, レイモンド, S.K. 藤原翔訳．2021．『カテゴリカルデータの連関分析』（共立出版）．のためのサポートページです．


## 正誤表 {-}
- 更新します．


## Rのインストール {-}

- CRAN（The Comprehensive R Archive Network）のページ（ https://cran.r-project.org ）
から最新版のRをダウンロードし，インストールてください．
- 必要であればRStudioもダウンロードし，インストールしてください．


## $l_{\rm EM}$のインストール {-}

- $l_{\rm EM}$はJeroen K. Vermunt氏のホームページ（ https://jeroenvermunt.nl/ ）からダウンロードできる．
- Software, User Manuals, and VideoのVermunt, J.K  (1997). LEM 1.0: A general program for the analysis of categorical data. Tilburg: Tilburg University. (download) の「 (download) 」をクリック．
- Windowsで動く．

## シリーズ編者による序文 {-}

### 独立性の検定 {-}


```r
# パッケージの読み込み
library(tidyverse)
```

```
## ─ Attaching packages ──────────────────── tidyverse 1.3.1 ─
```

```
## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
## ✓ tibble  3.1.0     ✓ dplyr   1.0.5
## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.1
```

```
## ─ Conflicts ───────────────────── tidyverse_conflicts() ─
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(magrittr)
```

```
## 
##  次のパッケージを付け加えます: 'magrittr'
```

```
##  以下のオブジェクトは 'package:purrr' からマスクされています: 
## 
##      set_names
```

```
##  以下のオブジェクトは 'package:tidyr' からマスクされています: 
## 
##      extract
```

```r
library(gnm)
```

- 精神衛生と親の社会経済的地位（SES）に関するミッドタウン・マンハッタンデータ（the Midtown Manhattan data）
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

# データを表形式に変換
tab <- matrix(Freq, nrow = 6, ncol = 4, byrow = TRUE)
```

- 初歩的なクロス表の分析はここで作成された`tab`に対して行う．`tab`を確認してみよう．
- `as.table`によってクラスをmatrixからtableに変えることもできる．分析はどちらであっても問題ない．


```r
# 観察度数
tab
```

```
##      [,1] [,2] [,3] [,4]
## [1,]   64   94   58   46
## [2,]   57   94   54   40
## [3,]   57  105   65   60
## [4,]   72  141   77   94
## [5,]   36   97   54   78
## [6,]   21   71   54   71
```

```r
# クラスをmatrixからtableに変える
tab <- as.table(tab)
tab
```

```
##     A   B   C   D
## A  64  94  58  46
## B  57  94  54  40
## C  57 105  65  60
## D  72 141  77  94
## E  36  97  54  78
## F  21  71  54  71
```

- `tab`に対してカイ2乗検定を行う．

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
names(chisq.test(tab))
```

```
## [1] "statistic" "parameter" "p.value"   "method"    "data.name" "observed" 
## [7] "expected"  "residuals" "stdres"
```

- 期待度数は`chisq.test(tab)$expected`とすればよい．これを`tab_expected`とする．

```r
# ixの期待度数
tab_expected <- chisq.test(tab)$expected
tab_expected
```

```
##          A         B        C        D
## A 48.45422  95.01446 57.13494 61.39639
## B 45.31024  88.84940 53.42771 57.41265
## C 53.07771 104.08072 62.58675 67.25482
## D 71.01687 139.25783 83.73976 89.98554
## E 49.00904  96.10241 57.78916 62.09940
## F 40.13193  78.69518 47.32169 50.85120
```

- ixページの数式用いて，ピアソンの$\chi^2$統計量と尤度比統計量$L^2$を求める．
- 自由度についても`nrow`と`ncol`を用いて計算（`prod(dim(tab) -1)`でもよい）．
- `list`関数は様々なもの（値，ベクトル，データ，リスト等）を並べるときに用いる．


```r
# 適合度（X2とL2）
X2 <- ((tab - tab_expected)^2 / tab_expected) %>% sum()
L2 <- (tab * log(tab / tab_expected)) %>% sum() * 2
df <- (nrow(tab) - 1) * (ncol(tab) - 1)
# df <- prod(dim(tab) -1)
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
- 多元表の分析はクロス表ではなく，度数，行変数，列変数からなるデータを作成して行う．度数については先程作成した`Freq`を使う．
- `gl`によって度数に対応するカテゴリを作成する．
- このような形にすることで柔軟なモデリングを行うことができる．
- なお`gnm`パッケージには`mentalHealth`というデータがあるのでそれを用いてもよい．


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

```r
# 度数，行変数，列変数からなるデータを作成
freq_tab_intro <- tibble(Freq, PSES, MHS)
# データの確認
freq_tab_intro
```

```
## # A tibble: 24 x 3
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


#### 独立モデル {-}
- この形式のデータに対して，独立モデルによる分析を行う．これは先程の独立性の検定と同じ結果となる．
- `%>%`はパイプ演算子である．`freq_tab_intro`というデータに対して，`gnm`を適用する．`gnm`内で`data = .`となっているがこれは`data = freq_tab_intro`とすることに等しい．
- こうして得られた分析の結果を`O`としている．


```r
# 独立モデル
O <- freq_tab_intro %>%
  gnm(Freq ~ PSES + MHS, family = poisson, data = .)
```


- 結果は`O`あるいは`summary(O)`で確認できる．


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



```r
# 適合度（X2とL2）
tab_expected_O <- O$fitted.values

# 行と列のカテゴリ数
I <- freq_tab_intro$PSES %>% unique() %>% length()
J <- freq_tab_intro$MHS %>% unique() %>% length()

df_0 <- (I - 1) * (J - 1) 
X2_O <- ((Freq - tab_expected_O)^2 / tab_expected_O) %>% sum()
L2_O <- (Freq * log(Freq / tab_expected_O)) %>% sum() * 2
list("自由度" = df_0,
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

- `Residual deviance: 47.418 on 15 degrees of freedom`となっており，先程の分析と適合度は一致する．


#### 一様連関モデル {-}

- 次に一様連関モデル（Uniform association model）による分析を行う．
- `mutate`関数で`PSES`を整数にしたものを`Rscore`，`MHS`を整数にしたものを`Cscore`として新たに変数を作成している．
- 変数の追加された`freq_tab_intro`データに対して，一様連関モデルによる分析を`gnm`パッケージで行う．
- 独立モデルとの違いは，作成した整数スコアの積`Rscore*Cscore`がモデルに追加されているだけである．


```r
# 行変数と列変数を連続した整数値とする
freq_tab_intro %<>% 
  mutate(Rscore = as.integer(PSES),
         Cscore = as.integer(MHS))
# 一様連関モデル
U <- freq_tab_intro %>% gnm(Freq ~ PSES + MHS + Rscore*Cscore, family = poisson, data = .)
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
tab_expected_U <- U$fitted.values
df_U <- (I - 1)*(J - 1) - 1
X2_U <- ((Freq - tab_expected_U)^2 / tab_expected_U) %>% sum()
L2_U <- (Freq * log(Freq / tab_expected_U)) %>% sum() * 2
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
freq_tab_intro %>% xtabs(Freq ~ PSES + MHS, data = .)
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


## 練習問題 {-}
- せっかくデータを入力したので，このデータを使ってGoodman（1979）の表5A，表5B，表5Cの結果を再現しよう．
- 分析方法については第2章のプログラムを参考にしてほしい．

## 参考文献 {-}
- Goodman, Leo A. 1979. “Simple Models for the Analysis of Association in Cross-Classifications Having Ordered Categories.” *Journal of the American Statistical Association* 74(367):537–52.


