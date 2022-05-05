Experiment 3: Main Analyses
================
Bethany Gardner
05/05/2022

-   [Setup](#setup)
-   [Data Summary](#data-summary)
-   [Model 1: With *Other* Responses](#model-1-with-other-responses)
    -   [Convert to Odds Ratios](#convert-to-odds-ratios)
-   [Model 2: Without *Other*
    Responses](#model-2-without-other-responses)
    -   [Convert to Odds Ratios](#convert-to-odds-ratios-1)

# Setup

Load data and select columns used in model. See data/exp3_data_about.txt
for more details.

``` r
d <- read.csv("../data/exp3_data.csv", stringsAsFactors=TRUE) %>%
  rename("Participant"="SubjID", "Item"="Name") %>%
  select(Participant, Condition, GenderRating, Item, He, She, Other)
str(d)
```

    ## 'data.frame':    8904 obs. of  7 variables:
    ##  $ Participant : Factor w/ 1272 levels "R_020UOb05Lb0EtX3",..: 216 216 216 216 216 216 216 41 41 41 ...
    ##  $ Condition   : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ GenderRating: num  5.22 1.24 5.86 3.75 6.78 4.34 2.41 6.24 2.61 6.82 ...
    ##  $ Item        : Factor w/ 63 levels "Ashley Cook",..: 6 9 13 43 47 52 62 2 16 20 ...
    ##  $ He          : int  0 1 0 0 0 0 1 0 1 0 ...
    ##  $ She         : int  0 0 1 0 1 1 0 0 0 1 ...
    ##  $ Other       : int  1 0 0 1 0 0 0 1 0 0 ...

Center gender rating for names: Original scale from 1 to 7, with 1 as
most masculine and 7 as most feminine. Mean-centered with higher still
as more feminine.

``` r
d %<>% mutate(GenderRatingCentered=scale(d$GenderRating, scale=FALSE))
```

Set contrasts for name conditions, now weighted to account for uneven
sample sizes. This uses Scott Fraundorf’s function for weighted
contrasts. (The psycholing package version doesn’t support doing 2v1
comparisons, only 1v1.) Condition1 is Last vs First+Full. Condition2 is
First vs Full.

``` r
source("centerfactor.R")
contrasts(d$Condition) <- centerfactor(d$Condition, c("last","first"))
contrasts(d$Condition)
```

    ##             [,1]        [,2]
    ## first  0.4009434 -0.48113208
    ## full   0.4009434  0.51886792
    ## last  -0.5990566  0.01886792

# Data Summary

Responses by condition.

``` r
d %<>% mutate(ResponseAll=case_when(
       He==1 ~ "He",
       She==1 ~ "She", 
       Other==1 ~ "Other"))

d.count_responses <- d %>% group_by(Condition, ResponseAll) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from=ResponseAll,
              values_from=n) %>%
  mutate(She_HeOther = She / (He+Other),
         She_He = She / He)

kable(d.count_responses, digits=3)
```

| Condition |   He | Other |  She | She_HeOther | She_He |
|:----------|-----:|------:|-----:|------------:|-------:|
| first     |  992 |   902 |  941 |       0.497 |  0.949 |
| full      |  899 |   752 |  848 |       0.514 |  0.943 |
| last      | 1378 |  1113 | 1079 |       0.433 |  0.783 |

# Model 1: With *Other* Responses

Effects of Condition (first name, last name, full name) and Gender
Rating on the likelihood of a *she* response, as opposed to a *he* or
*other* response. Participant and Item are included as random
intercepts, with items defined as the unique first, last and first +
last name combinations. Because the condition manipulations were fully
between-subject and between-item, fitting a random slope model was not
possible.

Because Experiment 3 always introduces the character with a full name,
then manipulates the name form in the subsequent 3 references, the main
analysis is one model, as opposed to the 2 for Experiment 1.

Condition1 is the contrast between last and first+full. Condition2 is
the contrast between first and full.

``` r
m.all <- glmer(She ~ Condition * GenderRatingCentered + 
                (1|Participant) + (1|Item), 
                data=d, family=binomial)
m.all_tidy <- tidy(m.all)
summary(m.all)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   7825.8   7882.5  -3904.9   7809.8     8896 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0250 -0.4836 -0.1394  0.5355  9.7282 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.7931   0.8905  
    ##  Item        (Intercept) 0.4209   0.6488  
    ## Number of obs: 8904, groups:  Participant, 1272; Item, 63
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -1.52419    0.10101 -15.090   <2e-16 ***
    ## Condition1                       0.15325    0.09155   1.674   0.0941 .  
    ## Condition2                       0.09120    0.11596   0.786   0.4316    
    ## GenderRatingCentered             1.14844    0.06039  19.017   <2e-16 ***
    ## Condition1:GenderRatingCentered  0.10499    0.04875   2.153   0.0313 *  
    ## Condition2:GenderRatingCentered -0.05627    0.06294  -0.894   0.3713    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtn1 Cndtn2 GndrRC C1:GRC
    ## Condition1   0.000                            
    ## Condition2  -0.015  0.023                     
    ## GndrRtngCnt -0.287 -0.004  0.016              
    ## Cndtn1:GnRC -0.009 -0.495  0.000  0.025       
    ## Cndtn2:GnRC  0.016  0.000 -0.488 -0.023  0.009

-   Fewer *she* responses overall

-   Last Name vs First+Full Names condition effect only trending

-   More *she* responses as first names become more feminine

-   Larger effect of first name gender in First+Full Name conditions
    than in Last Name conditions, which makes sense because there are 4
    repetitions of the gendered first name, as opposed to only 1.

## Convert to Odds Ratios

**Intercept**

``` r
m.all_intercept <- m.all_tidy %>% filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(m.all_intercept)
```

    ## [1] 0.2177981

``` r
exp(-m.all_intercept)
```

    ## [1] 4.591408

0.22x less likely to use *she* overall. Easier to interpret: 4.59x more
likely to use *he* and *other* overall.

**Condition: Last vs First+Full**

``` r
m.all_LFF <- m.all_tidy %>% 
  filter(term=="Condition1") %>%
  select(estimate) %>% as.numeric()
exp(m.all_LFF)
```

    ## [1] 1.165616

1.17x more likely to use *she* than *he* and *other* in First + Full
compared to Last. (n.s.)

**Condition: Last Only**

Dummy code with Last Name as 0, so that intercept is the Last Name
condition only.

``` r
d %<>% mutate(Condition_Last=case_when(
  Condition=="first" ~ 1,
  Condition=="full" ~ 1,
  Condition=="last" ~ 0))
d$Condition_Last %<>% as.factor()
```

``` r
m.all_last <- glmer(She ~ Condition_Last + (1|Participant) + (1|Item), 
          data=d, family=binomial)
m.all_last_tidy <- tidy(m.all_last)
```

``` r
m.all_lastonly <- m.all_last_tidy %>% 
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(m.all_lastonly)
```

    ## [1] 0.1747868

``` r
exp(-m.all_lastonly)
```

    ## [1] 5.721256

0.18x times less likely to use *she* than *he* and *other* in the Last
Name condition –> 5.72x more likely to use *he* and *other* in the Last
Name condition.

**Condition: First and Full Only**

Dummy code with First and Full Name as 0, so the intercept is the
combination of those two.

``` r
d %<>% mutate(Condition_FF=case_when(
  Condition=="first" ~ 0,
  Condition=="full" ~ 0,
  Condition=="last" ~ 1))
d$Condition_FF %<>% as.factor()
```

``` r
m.all_FF <- glmer(She ~ Condition_FF + (1|Participant) + (1|Item), 
          data=d, family=binomial)
m.all_FF_tidy <- tidy(m.all_FF)
```

``` r
m.all_FFonly <- m.all_FF_tidy %>% 
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(m.all_FFonly)
```

    ## [1] 0.2243583

``` r
exp(-m.all_FFonly)
```

    ## [1] 4.457156

0.22x times less likely to use *she* than *he* and *other* in the First
and Full Name conditions –> 4.46x more likely to use *he* and *other* in
the First and Full Name conditions.

# Model 2: Without *Other* Responses

The sentence completion prompt for Experiment 3 is more open-ended than
in Experiment 1. So, we get a much higher proportion of *other*
responses (31% vs 7%), which I didn’t anticipate.

``` r
o <- sum(d$Other) 
o
```

    ## [1] 2767

``` r
o/length(d$Other) 
```

    ## [1] 0.3107592

``` r
d.noOther <- d %>% filter(Other==0)
```

So, rerun the main model predicting the likelihood of *she* responses vs
*he* responses, with *other* responses excluded.

``` r
m.noOther <- glmer(She ~ Condition * GenderRatingCentered + 
                  (1|Participant) + (1|Item), 
             data=d.noOther, family=binomial)
m.noOther_tidy <- tidy(m.noOther)
summary(m.noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: She ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: d.noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4209.0   4262.8  -2096.5   4193.0     6129 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -9.0292 -0.3424 -0.0521  0.2952 12.5649 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.5394   0.7345  
    ##  Item        (Intercept) 0.6807   0.8251  
    ## Number of obs: 6137, groups:  Participant, 1223; Item, 63
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -0.42366    0.12377  -3.423 0.000619 ***
    ## Condition1                       0.25702    0.09784   2.627 0.008616 ** 
    ## Condition2                      -0.01455    0.12816  -0.114 0.909584    
    ## GenderRatingCentered             1.67709    0.08371  20.034  < 2e-16 ***
    ## Condition1:GenderRatingCentered  0.41953    0.07691   5.455  4.9e-08 ***
    ## Condition2:GenderRatingCentered -0.14907    0.11205  -1.330 0.183394    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtn1 Cndtn2 GndrRC C1:GRC
    ## Condition1   0.053                            
    ## Condition2  -0.020  0.005                     
    ## GndrRtngCnt -0.155 -0.005  0.005              
    ## Cndtn1:GnRC -0.007 -0.210  0.003  0.201       
    ## Cndtn2:GnRC  0.005  0.004 -0.182 -0.061 -0.053

These results are more similar to what we predicted from the previous
experiments:

-   Fewer *she* responses overall
-   Fewer *she* responses in the Last Name condition as compared to the
    First + Full Name conditions (although we wouldn’t predict as large
    as a difference as in Exp1, because here there is one instance of
    the first name in the Last Name condition)
-   More *she* responses as first names become more feminine
-   Larger effect of first name gender in First+Full Name conditions
    than in Last Name conditions (which makes sense because there are
    4repetitions of the gendered first name, as opposed to only 1.)

But, to keep the analyses consistent between experiments and avoid
post-hoc decision weirdness, both versions are reported.

## Convert to Odds Ratios

**Intercept**

``` r
m.noOther_intercept <- m.noOther_tidy %>% 
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(m.noOther_intercept)
```

    ## [1] 0.6546471

``` r
exp(-m.noOther_intercept)
```

    ## [1] 1.527541

0.65x less likely to use *she* than *he* overall. Easier to interpret:
1.53x more likely to use *he* than *she* overall.

**Condition: Last vs First+Full**

``` r
m.noOther_LFF <- m.noOther_tidy %>% 
  filter(term=="Condition1") %>%
  select(estimate) %>% as.numeric()
exp(m.noOther_LFF)
```

    ## [1] 1.293077

1.29x more likely to use *she* than *he* in First+Full than in Last –>
1.29x more likely to use *he* than *she* in Last than in First+Full.

**Condition: Last Only**

Dummy code with Last Name as 0, so that intercept is the Last Name
condition only.

``` r
d.noOther %<>% mutate(Condition_Last=case_when(
  Condition=="first" ~ 1,
  Condition=="full" ~ 1,
  Condition=="last" ~ 0))
d.noOther$Condition_Last %<>% as.factor()
```

``` r
m.noOther_last <- glmer(She ~ Condition_Last + (1|Participant) + (1|Item), 
          data=d.noOther, family=binomial)
m.noOther_last_tidy <- tidy(m.noOther_last)
```

``` r
m.noOther_lastonly <- m.noOther_last_tidy %>% 
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(m.noOther_lastonly)
```

    ## [1] 0.5080018

``` r
exp(-m.noOther_lastonly)
```

    ## [1] 1.968497

0.51x times less likely to use *she* than *he* in the Last Name
condition –> 1.97x more likely to use *he* than *she* in the Last Name
condition (n.s.)

**Condition: First and Full Only**

Dummy code with First and Full Name as 0, so the intercept is the
combination of those two.

``` r
d.noOther %<>% mutate(Condition_FF=case_when(
  Condition=="first" ~ 0,
  Condition=="full" ~ 0,
  Condition=="last" ~ 1))
d.noOther$Condition_FF %<>% as.factor()
```

``` r
m.noOther_FF <- glmer(She ~ Condition_FF + (1|Participant) + (1|Item), 
          data=d.noOther, family=binomial)
m.noOther_FF_tidy <- tidy(m.noOther_FF)
```

``` r
m.noOther_FFonly <- m.noOther_FF_tidy %>% 
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(m.noOther_FFonly)
```

    ## [1] 0.7385373

``` r
exp(-m.noOther_FFonly)
```

    ## [1] 1.354028

0.74x times less likely to use *she* than *he* and *other* in the First
and Full Name conditions –> 1.35x more likely to use *he* and *other* in
the First and Full Name conditions.
