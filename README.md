
## LGDtoolkit 0.0.9

`LGDtoolkit` provides collection of tools for loss given default (LGD)
rating model development and validation.</br> Keeping in mind the fact
that model development is highly iterative and repetitive process,
having standardized and automated tools for this purpose is of the
utmost importance for analysts. The main goal of this package is to
cover the most common steps of LGD model development. As additional
contribution we attempted to add some functionalities which at the
moment of the package development were not presented in `R` package
ecosystem in area of credit risk modeling. In the current package
version, available functionalities mainly refer to risk factor
engineering and multivariate analysis. With a new versions, package will
be extended with validation methods and survival analysis for LGD
modeling. </br>

The following case study shows the usage of `LGDtoolkit` package. The
study is based on synthetic data which consists of 1200 facilities with
closed or substantially closed recovery status. This data set is
distributed along with `LGDtoolkit` package under the data frame
`lgd.ds.c`. Presented examples are simplified, but yet realistic,
version of the LGD model development. So, let’s start with a concrete
case study.

First, we will import the libraries needed for the following examples.

``` r
library(monobin)
library(PDtoolkit)
library(LGDtoolkit)
```

If the packages are not already installed, run the following code before
the library import </br> `install.packages(c("monobin", "PDtoolkit"))`
in order to install these two packages from the CRAN, while
`devtools::install_github("andrija-djurovic/LGDtoolkit")` to install
development version of `LGDtoolkit` package.

> :warning: Be aware that some functions from `PDtoolkit` package will
> be masked after importing `LGDtoolkit` package. In order to avoid
> executing function from the wrong namespace, while running some of the
> commands we will explicitly add namespace while calling certain
> function (e.g. `PDtoolkit::` or `LGDtoolkit::`). <br/>

Then, let’s import and inspect the structure of the modeling data set -
`lgd.ds.c`.

``` r
data(lgd.ds.c)
str(lgd.ds.c)
```

    ## 'data.frame':    1200 obs. of  20 variables:
    ##  $ lgd  : num  0.541 0.857 0.829 0.91 0.857 ...
    ##  $ rf_01: num  6.06 12.68 22.62 11.21 12.68 ...
    ##  $ rf_02: num  111 120 116 111 120 ...
    ##  $ rf_03: num  19.4 45.5 28.9 29.3 45.5 ...
    ##  $ rf_04: num  268 311 343 370 311 ...
    ##  $ rf_05: num  15.88 4.84 64.84 67.05 4.84 ...
    ##  $ rf_06: num  NA NA NA NA NA ...
    ##  $ rf_07: num  822 1064 2243 1518 1064 ...
    ##  $ rf_08: num  43.1 86.5 85.4 102 86.5 ...
    ##  $ rf_09: num  6828 4374 3820 3378 4374 ...
    ##  $ rf_10: num  0.0184 0.011 0 0 0.011 ...
    ##  $ rf_11: num  NA NA 1.74 1 NA ...
    ##  $ rf_12: num  347 442 903 897 442 ...
    ##  $ rf_13: num  2.74 2.37 3.84 6.05 2.37 ...
    ##  $ rf_14: num  1857 3158 5330 4215 3158 ...
    ##  $ rf_15: num  1239 8651 2385 3530 8651 ...
    ##  $ rf_16: num  14.9 35.8 33.4 35.7 35.8 ...
    ##  $ rf_17: num  732 952 1874 2046 952 ...
    ##  $ rf_18: num  0.0384 0.0109 0.0257 0.0311 0.0109 ...
    ##  $ rf_19: num  NA NA NA NA NA ...

From the structure results, we conclude that there are in total 20
variables. All are of numeric type and variable `lgd` for the further
examples will be treated as target variable while all other will be
treated as risk factors that compete for the final model.

Usually the first step in model development is the univariate analysis.
For this purpose we can use `univariate` function from the `PDtoolkit`
package:

``` r
uni.res <- PDtoolkit::univariate(db = lgd.ds.c, sc.threshold = 0.3)
uni.res
```

    ##       rf rf.type       bin.type            bin  cnt         pct cnt.unique         min           p1            p5
    ## 1    lgd numeric complete cases complete cases 1200 1.000000000        615  0.00000000   0.00000000    0.00000000
    ## 2  rf_01 numeric complete cases complete cases 1200 1.000000000        775  0.00000000   1.00000000    3.99202376
    ## 3  rf_02 numeric  special cases  special cases  362 0.301666667          1         Inf           NA            NA
    ## 4  rf_02 numeric complete cases complete cases  838 0.698333333        388 17.65574122  23.00000000   23.00000000
    ## 5  rf_03 numeric  special cases  special cases  147 0.122500000          1         Inf           NA            NA
    ## 6  rf_03 numeric complete cases complete cases 1053 0.877500000        668  1.00444401   2.67928945    7.17500605
    ## 7  rf_04 numeric  special cases  special cases    4 0.003333333          1         Inf           NA            NA
    ## 8  rf_04 numeric complete cases complete cases 1196 0.996666667        808 22.97083233  28.91797075   48.00000000
    ## 9  rf_05 numeric  special cases  special cases    8 0.006666667          1         Inf           NA            NA
    ## 10 rf_05 numeric complete cases complete cases 1192 0.993333333        804  4.84052175  17.63929155   30.34240156
    ## 11 rf_06 numeric  special cases  special cases  529 0.440833333          1         Inf           NA            NA
    ## 12 rf_06 numeric complete cases complete cases  671 0.559166667        448 22.54000000  30.72439938  136.68058281
    ## 13 rf_07 numeric  special cases  special cases  593 0.494166667          1         Inf           NA            NA
    ## 14 rf_07 numeric complete cases complete cases  607 0.505833333        401 15.28000000  89.63092671  294.53491499
    ## 15 rf_08 numeric  special cases  special cases  489 0.407500000          1         Inf           NA            NA
    ## 16 rf_08 numeric complete cases complete cases  711 0.592500000        462 25.13532680  37.83857276   46.31208595
    ## 17 rf_09 numeric  special cases  special cases    8 0.006666667          1         Inf           NA            NA
    ## 18 rf_09 numeric complete cases complete cases 1192 0.993333333        825 28.03553689 331.89336495 1094.16481953
    ## 19 rf_10 numeric  special cases  special cases    4 0.003333333          1         Inf           NA            NA
    ## 20 rf_10 numeric complete cases complete cases 1196 0.996666667        366  0.00000000   0.00000000    0.00000000
    ## 21 rf_11 numeric  special cases  special cases  477 0.397500000          1         Inf           NA            NA
    ## 22 rf_11 numeric complete cases complete cases  723 0.602500000        319  1.00000000   1.00000000    1.00000000
    ## 23 rf_12 numeric  special cases  special cases  299 0.249166667          1         Inf           NA            NA
    ## 24 rf_12 numeric complete cases complete cases  901 0.750833333        610 40.79299196  80.66429745  199.08245061
    ## 25 rf_13 numeric  special cases  special cases   56 0.046666667          1         Inf           NA            NA
    ## 26 rf_13 numeric complete cases complete cases 1144 0.953333333        647  0.00000000   0.00000000    0.74992761
    ## 27 rf_14 numeric  special cases  special cases  174 0.145000000          1         Inf           NA            NA
    ## 28 rf_14 numeric complete cases complete cases 1026 0.855000000        691 18.00000000 124.64224900  733.27427502
    ## 29 rf_15 numeric  special cases  special cases  139 0.115833333          1         Inf           NA            NA
    ## 30 rf_15 numeric complete cases complete cases 1061 0.884166667        723 25.89103169 134.62457345  505.96257390
    ## 31 rf_16 numeric  special cases  special cases  339 0.282500000          1         Inf           NA            NA
    ## 32 rf_16 numeric complete cases complete cases  861 0.717500000        564  0.01000000   0.01000000    0.02861894
    ## 33 rf_17 numeric  special cases  special cases  580 0.483333333          1         Inf           NA            NA
    ## 34 rf_17 numeric complete cases complete cases  620 0.516666667        404  9.55715886 143.09429242  323.81487058
    ## 35 rf_18 numeric complete cases complete cases 1200 1.000000000        514 -0.09558666  -0.09558666   -0.09558666
    ## 36 rf_19 numeric  special cases  special cases  539 0.449166667          1         Inf           NA            NA
    ## 37 rf_19 numeric complete cases complete cases  661 0.550833333        458 -0.93826545  -0.76718787   -0.34052105
    ##               p25            p50             avg         avg.se            p75            p95             p99             max
    ## 1     0.004531067    0.244401597     0.368640404    0.010888824     0.69014569     1.02125333      1.22572992      1.47211318
    ## 2    13.508624746   22.759283183    33.179850507    0.900671298    39.54558626   108.69964615    139.73265872    163.80808151
    ## 3              NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 4    47.000000000   59.000000000    81.316485219    2.082000905   103.06613890   226.56058557    320.71023494    360.00000000
    ## 5              NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 6    19.640130269   36.111459262    42.562968628    0.992860052    52.00000000   108.78497380    156.93884055    199.48371485
    ## 7              NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 8   177.077454733  436.593547392   378.727003361    6.022114626   549.87086366   664.78662040    720.64942751    754.09840803
    ## 9              NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 10   66.108210086  114.212555009   121.242644759    1.904226326   172.97119442   233.18530648    283.95762761    317.67481818
    ## 11             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 12  625.820185630 1410.632064257  3250.742300492  181.967590186  3122.21772118 14658.67581971  19847.22461382  29308.79232210
    ## 13             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 14  974.654821003 2009.935649648  3974.710244391  195.792004653  4531.99322543 15939.16035492  19941.21610207  29308.79232210
    ## 15             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 16   95.575067313  154.273093829   153.191809152    2.694522227   206.04944612   280.20490058    308.72846918    329.36829346
    ## 17             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 18 3451.504760850 6387.968495929 13116.227712964  613.784710353 13209.48701866 48514.01533086 128780.86509465 150300.28736844
    ## 19             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 20    0.000000000    0.006545185    10.640635244    1.023022133     0.11415108    58.09044509    130.20986556    461.89746028
    ## 21             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 22    1.390595321    2.315279280     2.834502108    0.071709573     3.72683737     6.69769043      8.76458372     13.32276060
    ## 23             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 24  481.779871117  800.755212480  1005.618302342   28.532476294  1300.00390340  2393.06723243   4593.25540849   6766.97688547
    ## 25             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 26    2.000000000    3.765871170     5.275372901    0.151600737     6.68620933    16.45878534     24.30283094     40.86986102
    ## 27             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 28 3416.733392077 6663.596192870 12224.057202898  532.214934598 13175.30531341 42107.06794584  64345.04989650 182966.46247162
    ## 29             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 30 1858.723619660 4377.230297497 13880.543882895 1079.015094266 10636.51117503 57333.54421848 107806.99697343 388956.22094669
    ## 31             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 32    4.284460905   18.632123177    30.669779411    1.483646270    37.92775639   119.76771245    232.96024267    256.28195529
    ## 33             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 34 1382.595945211 2822.068110305  6015.401565964  375.345422508  5769.98378543 22575.55870222  47254.26489318  63442.87624218
    ## 35   -0.016089693    0.006024786     0.002922821    0.001223155     0.03216825     0.06317923      0.07584763      0.08204055
    ## 36             NA             NA             NaN             NA             NA             NA              NA            -Inf
    ## 37   -0.183700732   -0.084087004    -0.016573987    0.015850496    -0.01772558     0.65758039      1.76506460      3.29046060
    ##    neg  pos cnt.outliers sc.ind
    ## 1    0  957            0      0
    ## 2    0 1196          118      0
    ## 3   NA   NA            0      1
    ## 4    0  838           60      1
    ## 5   NA   NA            0      0
    ## 6    0 1053           61      0
    ## 7   NA   NA            0      0
    ## 8    0 1196            0      0
    ## 9   NA   NA            0      0
    ## 10   0 1192            0      0
    ## 11  NA   NA            0      1
    ## 12   0  671          103      1
    ## 13  NA   NA            0      1
    ## 14   0  607           72      1
    ## 15  NA   NA            0      1
    ## 16   0  711            0      1
    ## 17  NA   NA            0      0
    ## 18   0 1192          114      0
    ## 19  NA   NA            0      0
    ## 20   0  655          279      0
    ## 21  NA   NA            0      1
    ## 22   0  723           20      1
    ## 23  NA   NA            0      0
    ## 24   0  901           41      0
    ## 25  NA   NA            0      0
    ## 26   0 1126           86      0
    ## 27  NA   NA            0      0
    ## 28   0 1026          121      0
    ## 29  NA   NA            0      0
    ## 30   0 1061          141      0
    ## 31  NA   NA            0      0
    ## 32   0  861           67      0
    ## 33  NA   NA            0      1
    ## 34   0  620           81      1
    ## 35 532  668           87      0
    ## 36  NA   NA            0      1
    ## 37 514  147           95      1

Based on the results we can see that `univariate` treats differently
so-called special and complete cases. Additionally, we can conclude that
there are some risk factors with share of special cases higher than 30%
and they will be excluded from the further analysis.

> :warning: In the previous example threshold of 30% for the missing
> cases is selected for the sake of package demo, but in practice
> greater attention is paid on meaning and dealing with those values.
> <br/>

``` r
rf.excl <- unique(uni.res$rf[uni.res$sc.ind%in%1])
rf.excl
```

    ## [1] "rf_02" "rf_06" "rf_07" "rf_08" "rf_11" "rf_17" "rf_19"

``` r
lgd.ds.c <- lgd.ds.c[, !names(lgd.ds.c)%in%rf.excl]
```

For result details and additional arguments of `univariate` function
check help page `?PDtoolkit::univariate`.

Sometimes, when building LGD models numeric risk factors are
discretized, so we will proceed next with that step. For the purpose of
binning the numeric risk factors, we will use one of the functions
(`sts.bin`) from the `monobin` package. Details about this package can
be found [here](https://CRAN.R-project.org/package=monobin).
Usually before this step modelers split data set on training and testing and 
perform binning (and other model development steps) only on training data set.
For the sake of simplicity, for this case study we will skip this part but modelers
should be aware of importance of data set splitting step.

``` r
#define target variable and exclude it from the binning process
target <- "lgd"
num.rf <- names(lgd.ds.c)[!names(lgd.ds.c)%in%target]
lgd.ds.c[, num.rf] <- sapply(num.rf, function(x) 
                             monobin::ndr.bin(x = lgd.ds.c[, x], y = lgd.ds.c[, target])[[2]])
str(lgd.ds.c)
```

    ## 'data.frame':    1200 obs. of  13 variables:
    ##  $ lgd  : num  0.541 0.857 0.829 0.91 0.857 ...
    ##  $ rf_01: chr  "01 (-Inf,20.1001)" "01 (-Inf,20.1001)" "02 [20.1001,Inf)" "01 (-Inf,20.1001)" ...
    ##  $ rf_03: chr  "02 [14.4975,70.6008)" "02 [14.4975,70.6008)" "02 [14.4975,70.6008)" "02 [14.4975,70.6008)" ...
    ##  $ rf_04: chr  "03 [142.6312,531.6402)" "03 [142.6312,531.6402)" "03 [142.6312,531.6402)" "03 [142.6312,531.6402)" ...
    ##  $ rf_05: chr  "01 (-Inf,38.408)" "01 (-Inf,38.408)" "02 [38.408,177.9077)" "02 [38.408,177.9077)" ...
    ##  $ rf_09: chr  "01 (-Inf,25891.4584)" "01 (-Inf,25891.4584)" "01 (-Inf,25891.4584)" "01 (-Inf,25891.4584)" ...
    ##  $ rf_10: chr  "02 [0.0157,55.8871)" "01 (-Inf,0.0157)" "01 (-Inf,0.0157)" "01 (-Inf,0.0157)" ...
    ##  $ rf_12: chr  "01 (-Inf,1354.0616)" "01 (-Inf,1354.0616)" "01 (-Inf,1354.0616)" "01 (-Inf,1354.0616)" ...
    ##  $ rf_13: chr  "03 [2.6617,5.0394)" "02 [1,2.6617)" "03 [2.6617,5.0394)" "04 [5.0394,Inf)" ...
    ##  $ rf_14: chr  "02 [1698.4647,10839.1025)" "02 [1698.4647,10839.1025)" "02 [1698.4647,10839.1025)" "02 [1698.4647,10839.1025)" ...
    ##  $ rf_15: chr  "02 [825.8567,3825.6754)" "03 [3825.6754,Inf)" "02 [825.8567,3825.6754)" "02 [825.8567,3825.6754)" ...
    ##  $ rf_16: chr  "02 [0.0634,34.6935)" "03 [34.6935,43.6851)" "02 [0.0634,34.6935)" "03 [34.6935,43.6851)" ...
    ##  $ rf_18: chr  "03 [-0.0208,0.0506)" "03 [-0.0208,0.0506)" "03 [-0.0208,0.0506)" "03 [-0.0208,0.0506)" ...

After completing binning step, modelers usually perform bivariate
analysis and start examination of discriminatory power of risk factors
in relation with target variable. Probably the most common metric used
for this purpose is coefficient determination (R squared) and it usually
used for further risk factor filtering. In order to calculate R
squared we will use function `r.squared` from `LGDtoolkit` package.

``` r
LGDtoolkit::r.squared(db = lgd.ds.c, target = target)
```

    ##       rf   rf.type miss.inf miss.inf.pct  r.squared
    ## 1  rf_01 character        0            0 0.04520316
    ## 2  rf_03 character        0            0 0.04445570
    ## 3  rf_04 character        0            0 0.05649746
    ## 4  rf_05 character        0            0 0.08701528
    ## 5  rf_09 character        0            0 0.03057418
    ## 6  rf_10 character        0            0 0.06067633
    ## 7  rf_12 character        0            0 0.05918994
    ## 8  rf_13 character        0            0 0.05806014
    ## 9  rf_14 character        0            0 0.02408769
    ## 10 rf_15 character        0            0 0.06974936
    ## 11 rf_16 character        0            0 0.08610673
    ## 12 rf_18 character        0            0 0.04049297

Due to the fact that we do not have too many available risk factors, we
will not perform additional filtering based on the results of above
bivariate analysis. Thus we move on to multivariate analysis. For this
purpose `LGDtoolkit` package is equipped with two customized stepwise
algorithms `stepFWD` and `stepRPC`. Both functions support standard OLS
regression as well as fractional logistic regression. Note that latter
regression type requires target variable to be between 0 and 1. For the
following example we will use `stepFWD` with OLS estimation method.
Analysts are also encouraged to check the help page of both function as
well as other customized blockwise regression
(`?LGDtoolkit:: staged.blocks`, `?LGDtoolkit:: embedded.blocks`,
`?LGDtoolkit:: ensemble.blocks`)

``` r
mv.res <- LGDtoolkit::stepFWD(start.model = lgd ~ 1, 
                              p.value = 0.05, 
                              db = lgd.ds.c,
                              reg.type = "ols")
```

    ## [1] "Running iteration: 1"
    ## [1] "Running iteration: 2"
    ## [1] "Running iteration: 3"
    ## [1] "Running iteration: 4"
    ## [1] "Running iteration: 5"
    ## [1] "Running iteration: 6"
    ## [1] "Running iteration: 7"

``` r
names(mv.res)
```

    ## [1] "model"    "steps"    "warnings" "dev.db"

``` r
summary(mv.res$model)$coefficients
```

    ##                                 Estimate Std. Error    t value              Pr(>|t|)
    ## (Intercept)                   0.31472806 0.05800282  5.4260820 0.0000000698208174489
    ## rf_0502 [38.408,177.9077)    -0.15113159 0.03699533 -4.0851537 0.0000470138750068009
    ## rf_0503 [177.9077,Inf)       -0.31736141 0.04253544 -7.4611054 0.0000000000001656480
    ## rf_05SC                      -0.39272911 0.12412552 -3.1639676 0.0015959938925431542
    ## rf_0102 [20.1001,Inf)        -0.05813067 0.02256450 -2.5762008 0.0101096786738615555
    ## rf_1602 [0.0634,34.6935)      0.22012105 0.03886414  5.6638601 0.0000000185689455348
    ## rf_1603 [34.6935,43.6851)     0.31049170 0.05299670  5.8586987 0.0000000060405748407
    ## rf_1604 [43.6851,Inf)         0.34576836 0.04745510  7.2862216 0.0000000000005809151
    ## rf_16SC                       0.14966225 0.04049960  3.6954006 0.0002295802574621863
    ## rf_0302 [14.4975,70.6008)    -0.06459748 0.03365298 -1.9195176 0.0551591686509560614
    ## rf_0303 [70.6008,Inf)        -0.19066308 0.04244148 -4.4923757 0.0000077329442034241
    ## rf_03SC                      -0.11375612 0.04283670 -2.6555763 0.0080236864779808167
    ## rf_1502 [825.8567,3825.6754)  0.08273906 0.03688639  2.2430781 0.0250763047068701603
    ## rf_1503 [3825.6754,Inf)       0.12446684 0.03663231  3.3977339 0.0007020626257604785
    ## rf_15SC                       0.02916156 0.04336299  0.6724989 0.5013974803005729619
    ## rf_1202 [1354.0616,Inf)       0.10378840 0.03101615  3.3462693 0.0008447905530221169
    ## rf_12SC                       0.10780106 0.02603006  4.1414064 0.0000369702534436035

``` r
mv.res$steps
```

    ##      rf       aic                  p.val p.val.check trend.check
    ## 1 rf_05 -2442.196 0.00000000000000000000        TRUE        TRUE
    ## 2 rf_01 -2468.610 0.00000000000000000000        TRUE        TRUE
    ## 3 rf_16 -2568.960 0.00000000000000000000        TRUE        TRUE
    ## 4 rf_03 -2593.788 0.00000000000000000000        TRUE        TRUE
    ## 5 rf_15 -2612.353 0.00000000000000000000        TRUE        TRUE
    ## 6 rf_12 -2630.565 0.00000000000002375877        TRUE        TRUE

``` r
summary(mv.res$model)$r.squared
```

    ## [1] 0.236374

> :warning: Be aware that usually in practice greater attention is paid
> on dealing with special cases and missing values before running
> multivariate analysis. Sometimes they are merged with one of the
> modalities from so-called complete cases. In above example this
> procedure was not performed, but if analysts want to run this step it
> can be done using function `LGDtoolkit::sc.merge`. For details check
> the help page. <br/>

After selecting the final model set of validation procedures are
performed. For time being `LGDtoolkit` package has
`LGDtoolkit::kfold.vld` and `LGDtoolkit::kfold.idx` functions that can
be used for validating some of goodness-of-fit metrics, but with a new
versions of the package this area will be extended with additional
tests. In order to apply CV validation analysts can run the following
code:

``` r
LGDtoolkit::kfold.vld(model = mv.res$model, k = 10, seed = 1984)
```

    ## $iter
    ##     k  no       amse      rmse r.squared
    ## 1   1 120 0.11474930 0.3387467 0.1843044
    ## 2   2 120 0.08741986 0.2956685 0.2764421
    ## 3   3 120 0.12021084 0.3467143 0.3273682
    ## 4   4 120 0.11124528 0.3335345 0.2475501
    ## 5   5 120 0.12221417 0.3495914 0.1768010
    ## 6   6 120 0.11106522 0.3332645 0.2694624
    ## 7   7 120 0.11170662 0.3342254 0.1644091
    ## 8   8 120 0.11181607 0.3343891 0.1627783
    ## 9   9 120 0.11101191 0.3331845 0.2782350
    ## 10 10 120 0.12080593 0.3475715 0.1367292
    ## 
    ## $summary
    ##        amse     rmse r.squared
    ## 1 0.1122245 0.334689  0.222408
