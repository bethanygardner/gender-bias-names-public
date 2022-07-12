Experiment 4: Ratings Analyses
================
Bethany Gardner
2022-07-07

-   [Setup](#setup)
-   [Likeability](#likeability)
-   [Accomplishment](#accomplishment)
-   [Importance](#importance)

# Setup

-   Variable names:

    -   Experiment: exp4

    -   Type

        -   d = data
        -   m = model
        -   est = log odds estimate from model
        -   OR = odds ratio converted from est

    -   Analysis

        -   Lik = likability ratings
        -   Acc = accomplishment ratings
        -   Imp = importance ratings

``` r
exp4_d <- read.csv("../data/exp4_data.csv", 
                   stringsAsFactors=TRUE) %>%
  rename("Participant"="SubjID", "Item"="Name") %>%
  select(Participant, Condition, 
         GenderRating, Item, Male, Female, Other,
         Likeable, Accomplished, Important)
str(exp4_d)
```

    ## 'data.frame':    8771 obs. of  10 variables:
    ##  $ Participant : Factor w/ 1253 levels "R_00dmdQaotbTidXz",..: 1001 1001 1001 1001 1001 1001 1001 23 23 23 ...
    ##  $ Condition   : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ GenderRating: num  6.24 2.61 6.82 5.34 1.28 4.39 3.87 5.22 1.24 5.86 ...
    ##  $ Item        : Factor w/ 63 levels "Ashley Cook",..: 1 18 21 22 25 28 50 5 7 15 ...
    ##  $ Male        : int  0 1 0 0 1 1 1 1 1 0 ...
    ##  $ Female      : int  1 0 1 1 0 0 0 0 0 1 ...
    ##  $ Other       : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Likeable    : int  2 1 3 2 4 3 3 3 2 4 ...
    ##  $ Accomplished: int  1 2 1 4 2 2 2 5 3 3 ...
    ##  $ Important   : int  2 1 5 5 1 4 3 3 2 2 ...

Center gender rating for names: Original scale from 1 to 7, with 1 as
most masculine and 7 as most feminine. Mean-centered with higher still
as more feminine.

``` r
exp4_d %<>% mutate(GenderRatingCentered=
                     scale(GenderRating, scale=FALSE))
```

Set contrasts for name conditions, now weighted to account for uneven
sample sizes. This uses Scott Fraundorf’s function for weighted
contrasts. (The psycholing package version doesn’t support doing 2v1
comparisons, only 1v1.) Condition1 is Last vs First+Full. Condition2 is
First vs Full.

``` r
source("centerfactor.R")
contrasts(exp4_d$Condition) <- centerfactor(
  exp4_d$Condition, c("last","first"))
contrasts(exp4_d$Condition)
```

    ##             [,1]         [,2]
    ## first  0.3312051 -0.497605746
    ## full   0.3312051  0.502394254
    ## last  -0.6687949  0.002394254

Flip ratings from 1=most likeable/accomplished/important to 7=most
L/A/I, to make interpreting models easier, then mean-center.

``` r
exp4_d %<>% mutate(
  LikeableFlip = recode(Likeable, 
      '1'=7, '2'=6, '3'=5, '4'=4, '5'=3, '6'=2, '7'=1),
  AccomplishedFlip = recode(Accomplished,
      '1'=7, '2'=6, '3'=5, '4'=4, '5'=3, '6'=2, '7'=1),
  ImportantFlip = recode(Important,
      '1'=7, '2'=6, '3'=5, '4'=4, '5'=3, '6'=2, '7'=1),
  LikeableCentered = scale(
    LikeableFlip, scale=FALSE),
  AccomplishedCentered = scale(
    AccomplishedFlip, scale=FALSE),
  ImportantCentered = scale(
    ImportantFlip, scale=FALSE))

str(exp4_d)
```

    ## 'data.frame':    8771 obs. of  17 variables:
    ##  $ Participant         : Factor w/ 1253 levels "R_00dmdQaotbTidXz",..: 1001 1001 1001 1001 1001 1001 1001 23 23 23 ...
    ##  $ Condition           : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..- attr(*, "contrasts")= num [1:3, 1:2] 0.331 0.331 -0.669 -0.498 0.502 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:3] "first" "full" "last"
    ##   .. .. ..$ : NULL
    ##  $ GenderRating        : num  6.24 2.61 6.82 5.34 1.28 4.39 3.87 5.22 1.24 5.86 ...
    ##  $ Item                : Factor w/ 63 levels "Ashley Cook",..: 1 18 21 22 25 28 50 5 7 15 ...
    ##  $ Male                : int  0 1 0 0 1 1 1 1 1 0 ...
    ##  $ Female              : int  1 0 1 1 0 0 0 0 0 1 ...
    ##  $ Other               : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Likeable            : int  2 1 3 2 4 3 3 3 2 4 ...
    ##  $ Accomplished        : int  1 2 1 4 2 2 2 5 3 3 ...
    ##  $ Important           : int  2 1 5 5 1 4 3 3 2 2 ...
    ##  $ GenderRatingCentered: num [1:8771, 1] 2.03 -1.6 2.61 1.13 -2.93 ...
    ##   ..- attr(*, "scaled:center")= num 4.21
    ##  $ LikeableFlip        : num  6 7 5 6 4 5 5 5 6 4 ...
    ##  $ AccomplishedFlip    : num  7 6 7 4 6 6 6 3 5 5 ...
    ##  $ ImportantFlip       : num  6 7 3 3 7 4 5 5 6 6 ...
    ##  $ LikeableCentered    : num [1:8771, 1] 0.561 1.561 -0.439 0.561 -1.439 ...
    ##   ..- attr(*, "scaled:center")= num 5.44
    ##  $ AccomplishedCentered: num [1:8771, 1] 1.446 0.446 1.446 -1.554 0.446 ...
    ##   ..- attr(*, "scaled:center")= num 5.55
    ##  $ ImportantCentered   : num [1:8771, 1] 0.849 1.849 -2.151 -2.151 1.849 ...
    ##   ..- attr(*, "scaled:center")= num 5.15

# Likeability

Summary statistics:

``` r
summary(exp4_d$Likeable)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   2.000   2.561   3.000   7.000

``` r
summary(exp4_d$LikeableFlip)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   5.000   6.000   5.439   7.000   7.000

``` r
sd(exp4_d$Likeable)
```

    ## [1] 1.392179

Does the Likeability rating of the character predict how likely the
character is to be recalled as female, as opposed to male or other? The
maximal model includes random intercepts by item, but not by
participant.

``` r
exp4_m_lik <- buildmer(
  formula=(Female ~ Condition * GenderRatingCentered * 
           LikeableCentered + (1|Participant) + (1|Item)),
  data=exp4_d, family=binomial, 
  direction=c("order"), quiet=TRUE)

summary(exp4_m_lik)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## Female ~ 1 + GenderRatingCentered + LikeableCentered + GenderRatingCentered:LikeableCentered +  
    ##     Condition + GenderRatingCentered:Condition + LikeableCentered:Condition +  
    ##     GenderRatingCentered:LikeableCentered:Condition + (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9066.8   9158.8  -4520.4   9040.8     8758 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4522 -0.5946 -0.2609  0.5931  5.5758 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Item   (Intercept) 0.3372   0.5807  
    ## Number of obs: 8771, groups:  Item, 63
    ## 
    ## Fixed effects:
    ##                                                   Estimate Std. Error   z value
    ## (Intercept)                                      -0.244453   0.078424 -3.117059
    ## GenderRatingCentered                              0.751745   0.044492 16.896062
    ## LikeableCentered                                  0.086624   0.019205  4.510472
    ## Condition1                                        0.119371   0.055402  2.154647
    ## Condition2                                        0.066950   0.065302  1.025240
    ## GenderRatingCentered:LikeableCentered             0.103182   0.011834  8.719053
    ## GenderRatingCentered:Condition1                   0.124923   0.035196  3.549338
    ## GenderRatingCentered:Condition2                  -0.099175   0.042892 -2.312214
    ## LikeableCentered:Condition1                       0.044613   0.038171  1.168758
    ## LikeableCentered:Condition2                       0.091444   0.046297  1.975159
    ## GenderRatingCentered:LikeableCentered:Condition1  0.019182   0.023407  0.819486
    ## GenderRatingCentered:LikeableCentered:Condition2  0.001836   0.029003  0.063300
    ##                                                  Pr(>|z|) Pr(>|t|)    
    ## (Intercept)                                         0.002 0.001827 ** 
    ## GenderRatingCentered                                0.000  < 2e-16 ***
    ## LikeableCentered                                    0.000 6.47e-06 ***
    ## Condition1                                          0.031 0.031189 *  
    ## Condition2                                          0.305 0.305250    
    ## GenderRatingCentered:LikeableCentered               0.000  < 2e-16 ***
    ## GenderRatingCentered:Condition1                     0.000 0.000386 ***
    ## GenderRatingCentered:Condition2                     0.021 0.020766 *  
    ## LikeableCentered:Condition1                         0.243 0.242501    
    ## LikeableCentered:Condition2                         0.048 0.048250 *  
    ## GenderRatingCentered:LikeableCentered:Condition1    0.413 0.412509    
    ## GenderRatingCentered:LikeableCentered:Condition2    0.950 0.949528    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC LkblCn Cndtn1 Cndtn2 GnRC:LC GRC:C1 GRC:C2 LkC:C1
    ## GndrRtngCnt -0.028                                                         
    ## LikeblCntrd  0.035 -0.010                                                  
    ## Condition1   0.014  0.002 -0.026                                           
    ## Condition2  -0.013  0.012 -0.027 -0.019                                    
    ## GndrRtnC:LC -0.004  0.079 -0.149  0.020  0.024                             
    ## GndrRtnC:C1  0.004  0.034  0.019 -0.132  0.020  0.004                      
    ## GndrRtnC:C2  0.012 -0.029  0.024  0.020 -0.115 -0.030  -0.046              
    ## LkblCntr:C1 -0.008  0.006  0.078  0.091 -0.017 -0.011  -0.012  0.015       
    ## LkblCntr:C2 -0.009  0.010 -0.029 -0.015  0.046  0.004   0.015  0.013 -0.020
    ## GndRC:LC:C1  0.008  0.002 -0.010 -0.013  0.016  0.109   0.176 -0.021 -0.125
    ## GndRC:LC:C2  0.011 -0.014  0.006  0.016  0.012 -0.058  -0.019  0.155  0.005
    ##             LkC:C2 GRC:LC:C1
    ## GndrRtngCnt                 
    ## LikeblCntrd                 
    ## Condition1                  
    ## Condition2                  
    ## GndrRtnC:LC                 
    ## GndrRtnC:C1                 
    ## GndrRtnC:C2                 
    ## LkblCntr:C1                 
    ## LkblCntr:C2                 
    ## GndRC:LC:C1  0.005          
    ## GndRC:LC:C2 -0.127 -0.044

-   Characters who are rated more Likeable are more likely to be
    recalled as female across conditions

-   Interaction with Name Gender Rating: stronger effect of Likeability
    rating for more feminine names

-   Interaction with Condition (F vs F): n.s. after multiple comparisons

-   No other interactions significant

# Accomplishment

Summary statistics:

``` r
summary(exp4_d$Accomplished)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   2.000   2.446   3.000   7.000

``` r
summary(exp4_d$AccomplishedFlip)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   5.000   6.000   5.554   7.000   7.000

``` r
sd(exp4_d$Accomplished)
```

    ## [1] 1.371271

Does the Accomplishment rating of the character predict how likely the
character is to be recalled as female, as opposed to male or other? The
maximal model includes random intercepts by item, but not by
participant.

``` r
exp4_m_acc <- buildmer(
  formula=(Female ~ Condition * GenderRatingCentered * 
    AccomplishedCentered + (1|Participant) + (1|Item)),
  data=exp4_d, family=binomial, 
  direction=c("order"), quiet=TRUE)

summary(exp4_m_acc)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## Female ~ 1 + GenderRatingCentered + AccomplishedCentered + GenderRatingCentered:AccomplishedCentered +  
    ##     Condition + GenderRatingCentered:Condition + AccomplishedCentered:Condition +  
    ##     GenderRatingCentered:AccomplishedCentered:Condition + (1 |      Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9082.2   9174.2  -4528.1   9056.2     8758 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1200 -0.5912 -0.2519  0.5987  5.7663 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  Item   (Intercept) 0.3385   0.5818  
    ## Number of obs: 8771, groups:  Item, 63
    ## 
    ## Fixed effects:
    ##                                                       Estimate Std. Error
    ## (Intercept)                                          -0.243204   0.078552
    ## GenderRatingCentered                                  0.749198   0.044554
    ## AccomplishedCentered                                  0.047072   0.019043
    ## Condition1                                            0.113323   0.055390
    ## Condition2                                            0.071078   0.065376
    ## GenderRatingCentered:AccomplishedCentered             0.108068   0.011716
    ## GenderRatingCentered:Condition1                       0.126130   0.035188
    ## GenderRatingCentered:Condition2                      -0.100900   0.043082
    ## AccomplishedCentered:Condition1                       0.012340   0.038401
    ## AccomplishedCentered:Condition2                       0.079506   0.046748
    ## GenderRatingCentered:AccomplishedCentered:Condition1  0.056186   0.023330
    ## GenderRatingCentered:AccomplishedCentered:Condition2 -0.009309   0.029285
    ##                                                        z value Pr(>|z|)
    ## (Intercept)                                          -3.096102    0.002
    ## GenderRatingCentered                                 16.815545    0.000
    ## AccomplishedCentered                                  2.471823    0.013
    ## Condition1                                            2.045905    0.041
    ## Condition2                                            1.087224    0.277
    ## GenderRatingCentered:AccomplishedCentered             9.224136    0.000
    ## GenderRatingCentered:Condition1                       3.584509    0.000
    ## GenderRatingCentered:Condition2                      -2.342047    0.019
    ## AccomplishedCentered:Condition1                       0.321345    0.748
    ## AccomplishedCentered:Condition2                       1.700743    0.089
    ## GenderRatingCentered:AccomplishedCentered:Condition1  2.408265    0.016
    ## GenderRatingCentered:AccomplishedCentered:Condition2 -0.317879    0.751
    ##                                                      Pr(>|t|)    
    ## (Intercept)                                          0.001961 ** 
    ## GenderRatingCentered                                  < 2e-16 ***
    ## AccomplishedCentered                                 0.013443 *  
    ## Condition1                                           0.040766 *  
    ## Condition2                                           0.276938    
    ## GenderRatingCentered:AccomplishedCentered             < 2e-16 ***
    ## GenderRatingCentered:Condition1                      0.000338 ***
    ## GenderRatingCentered:Condition2                      0.019178 *  
    ## AccomplishedCentered:Condition1                      0.747949    
    ## AccomplishedCentered:Condition2                      0.088991 .  
    ## GenderRatingCentered:AccomplishedCentered:Condition1 0.016029 *  
    ## GenderRatingCentered:AccomplishedCentered:Condition2 0.750576    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC AccmpC Cndtn1 Cndtn2 GnRC:AC GRC:C1 GRC:C2 AcC:C1
    ## GndrRtngCnt -0.029                                                         
    ## AccmplshdCn  0.039 -0.008                                                  
    ## Condition1   0.015 -0.001 -0.009                                           
    ## Condition2  -0.013  0.013 -0.004 -0.020                                    
    ## GndrRtnC:AC -0.006  0.074 -0.124 -0.010  0.013                             
    ## GndrRtnC:C1  0.001  0.037 -0.009 -0.135  0.021  0.019                      
    ## GndrRtnC:C2  0.012 -0.029  0.015  0.021 -0.125 -0.027  -0.047              
    ## AccmplsC:C1 -0.001 -0.005  0.084  0.103 -0.002  0.003  -0.012  0.010       
    ## AccmplsC:C2 -0.004  0.009 -0.063  0.000  0.079  0.032   0.010 -0.024 -0.044
    ## GndRC:AC:C1 -0.005  0.012  0.003 -0.013  0.011  0.128   0.177 -0.019 -0.141
    ## GndRC:AC:C2  0.008 -0.015  0.032  0.009 -0.025 -0.090  -0.017  0.175  0.024
    ##             AcC:C2 GRC:AC:C1
    ## GndrRtngCnt                 
    ## AccmplshdCn                 
    ## Condition1                  
    ## Condition2                  
    ## GndrRtnC:AC                 
    ## GndrRtnC:C1                 
    ## GndrRtnC:C2                 
    ## AccmplsC:C1                 
    ## AccmplsC:C2                 
    ## GndRC:AC:C1  0.025          
    ## GndRC:AC:C2 -0.120 -0.065

-   Characters who were rated more Accomplished were more likely to be
    recalled as female, but this is n.s. after correction for multiple
    comparisons.

-   Interaction with Name Gender Rating: stronger effect of
    Accomplishment rating for more feminine names

-   Interaction between Condition (L vs F+F), Name Gender Rating, and
    Accomplishment: n.s. after correction for multiple comparisons

# Importance

Summary statistics:

``` r
summary(exp4_d$Important)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   2.849   4.000   7.000

``` r
summary(exp4_d$ImportantFlip)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   4.000   5.000   5.151   6.000   7.000

``` r
sd(exp4_d$Important)
```

    ## [1] 1.461433

Does the Importance rating of the character predict how likely the
character is to be recalled as female, as opposed to male or other? The
maximal model includes random intercepts by item, but not by
participant.

``` r
exp4_m_imp <- buildmer(
  formula=(Female ~ Condition * GenderRatingCentered * 
           ImportantCentered + (1|Participant) + (1|Item)),
  data=exp4_d, family=binomial, 
  direction=c("order"), quiet=TRUE)

summary(exp4_m_imp)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## Female ~ 1 + GenderRatingCentered + Condition + GenderRatingCentered:Condition +  
    ##     ImportantCentered + GenderRatingCentered:ImportantCentered +  
    ##     Condition:ImportantCentered + GenderRatingCentered:Condition:ImportantCentered +  
    ##     (1 | Item) + (1 | Participant)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9110.1   9209.2  -4541.1   9082.1     8757 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4351 -0.5792 -0.2520  0.5759  5.9605 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2045   0.4523  
    ##  Item        (Intercept) 0.3583   0.5986  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                                                    Estimate Std. Error
    ## (Intercept)                                       -0.248812   0.081532
    ## GenderRatingCentered                               0.772472   0.045914
    ## Condition1                                         0.125487   0.062102
    ## Condition2                                         0.079999   0.072919
    ## ImportantCentered                                  0.009610   0.019744
    ## GenderRatingCentered:Condition1                    0.133310   0.034992
    ## GenderRatingCentered:Condition2                   -0.098331   0.042671
    ## GenderRatingCentered:ImportantCentered             0.077713   0.011593
    ## Condition1:ImportantCentered                       0.009947   0.039429
    ## Condition2:ImportantCentered                       0.076869   0.046678
    ## GenderRatingCentered:Condition1:ImportantCentered  0.016048   0.023112
    ## GenderRatingCentered:Condition2:ImportantCentered -0.008267   0.028272
    ##                                                     z value Pr(>|z|) Pr(>|t|)
    ## (Intercept)                                       -3.051711    0.002 0.002275
    ## GenderRatingCentered                              16.824211    0.000  < 2e-16
    ## Condition1                                         2.020650    0.043 0.043316
    ## Condition2                                         1.097092    0.273 0.272601
    ## ImportantCentered                                  0.486734    0.626 0.626447
    ## GenderRatingCentered:Condition1                    3.809773    0.000 0.000139
    ## GenderRatingCentered:Condition2                   -2.304378    0.021 0.021201
    ## GenderRatingCentered:ImportantCentered             6.703149    0.000 2.04e-11
    ## Condition1:ImportantCentered                       0.252264    0.801 0.800837
    ## Condition2:ImportantCentered                       1.646771    0.100 0.099605
    ## GenderRatingCentered:Condition1:ImportantCentered  0.694352    0.487 0.487462
    ## GenderRatingCentered:Condition2:ImportantCentered -0.292412    0.770 0.769972
    ##                                                      
    ## (Intercept)                                       ** 
    ## GenderRatingCentered                              ***
    ## Condition1                                        *  
    ## Condition2                                           
    ## ImportantCentered                                    
    ## GenderRatingCentered:Condition1                   ***
    ## GenderRatingCentered:Condition2                   *  
    ## GenderRatingCentered:ImportantCentered            ***
    ## Condition1:ImportantCentered                         
    ## Condition2:ImportantCentered                      .  
    ## GenderRatingCentered:Condition1:ImportantCentered    
    ## GenderRatingCentered:Condition2:ImportantCentered    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC Cndtn1 Cndtn2 ImprtC GnRC:C1 GnRC:C2 GRC:IC Cn1:IC
    ## GndrRtngCnt -0.028                                                          
    ## Condition1   0.012  0.002                                                   
    ## Condition2  -0.011  0.013 -0.014                                            
    ## ImprtntCntr  0.025 -0.001 -0.006  0.013                                     
    ## GndrRtnC:C1  0.001  0.036 -0.118  0.019 -0.008                              
    ## GndrRtnC:C2  0.012 -0.029  0.019 -0.109  0.023 -0.044                       
    ## GndrRtnC:IC  0.002  0.060 -0.005  0.021 -0.103  0.023  -0.007               
    ## Cndtn1:ImpC -0.001 -0.003  0.069  0.010  0.043  0.005   0.015  -0.002       
    ## Cndtn2:ImpC  0.004  0.007  0.010  0.056 -0.070  0.015  -0.004   0.020 -0.051
    ## GndRC:C1:IC -0.002  0.010  0.002  0.015 -0.002  0.125  -0.005   0.087 -0.091
    ## GndRC:C2:IC  0.010 -0.002  0.015 -0.005  0.021 -0.002   0.135  -0.100  0.019
    ##             Cn2:IC GRC:C1:
    ## GndrRtngCnt               
    ## Condition1                
    ## Condition2                
    ## ImprtntCntr               
    ## GndrRtnC:C1               
    ## GndrRtnC:C2               
    ## GndrRtnC:IC               
    ## Cndtn1:ImpC               
    ## Cndtn2:ImpC               
    ## GndRC:C1:IC  0.019        
    ## GndRC:C2:IC -0.087 -0.074

-   No main effect of Importance like there was for other ratings

-   Interaction with Name Gender Rating: stronger effect of Importance
    rating for more feminine names
