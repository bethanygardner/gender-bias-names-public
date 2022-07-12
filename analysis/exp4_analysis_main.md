Experiment 4: Main Analyses
================
Bethany Gardner
2022-07-07

-   [Setup](#setup)
-   [Data Summary](#data-summary)
-   [Main Model](#main-model)
    -   [L v F+F Interaction](#l-v-ff-interaction)
    -   [F v F Interaction](#f-v-f-interaction)
    -   [Odds Ratios: Intercept](#odds-ratios-intercept)
    -   [Odds Ratios: Last vs
        First+Full](#odds-ratios-last-vs-firstfull)
    -   [Odds Ratios: Last Only](#odds-ratios-last-only)
    -   [Odds Ratios: First and Full
        Only](#odds-ratios-first-and-full-only)

# Setup

-   Variable names:

    -   Experiment: exp4

    -   Type

        -   d = data
        -   m = model
        -   est = log odds estimate from model
        -   OR = odds ratio converted from est

    -   Analysis

        -   count =sums of response types
        -   cond = effect of Condition (Last vs First+Full)
        -   nameGender = effects of Condition (First vs Full) and Name
            Gender Rating

    -   Subset

        -   all = including *other* responses

        -   noOther = excluding *other* responses

        -   FF = First and Full Name conditions only

        -   Last = Last Name condition only

Load data and select columns used in model. See data/exp4_data_about.txt
for more details.

``` r
exp4_d <- read.csv("../data/exp4_data.csv",
                   stringsAsFactors=TRUE) %>%
  rename("Participant"="SubjID", "Item"="Name") %>%
  select(Participant, Condition, GenderRating, 
         Item, Male, Female, Other)
str(exp4_d)
```

    ## 'data.frame':    8771 obs. of  7 variables:
    ##  $ Participant : Factor w/ 1253 levels "R_00dmdQaotbTidXz",..: 1001 1001 1001 1001 1001 1001 1001 23 23 23 ...
    ##  $ Condition   : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ GenderRating: num  6.24 2.61 6.82 5.34 1.28 4.39 3.87 5.22 1.24 5.86 ...
    ##  $ Item        : Factor w/ 63 levels "Ashley Cook",..: 1 18 21 22 25 28 50 5 7 15 ...
    ##  $ Male        : int  0 1 0 0 1 1 1 1 1 0 ...
    ##  $ Female      : int  1 0 1 1 0 0 0 0 0 1 ...
    ##  $ Other       : int  0 0 0 0 0 0 0 0 0 0 ...

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

# Data Summary

Responses by condition.

``` r
exp4_d %<>% mutate(ResponseAll=case_when(
  Male==1 ~ "Male",
  Female==1 ~ "Female", 
  Other==1 ~ "Other"))

exp4_d_count <- exp4_d %>% 
  group_by(Condition, ResponseAll) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from=ResponseAll,
              values_from=n) %>%
  mutate(Female_MaleOther = Female / (Male+Other),
         Female_Male = Female / Male)

kable(exp4_d_count)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Condition
</th>
<th style="text-align:right;">
Female
</th>
<th style="text-align:right;">
Male
</th>
<th style="text-align:right;">
Other
</th>
<th style="text-align:right;">
Female_MaleOther
</th>
<th style="text-align:right;">
Female_Male
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
first
</td>
<td style="text-align:right;">
1381
</td>
<td style="text-align:right;">
1511
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
0.8779402
</td>
<td style="text-align:right;">
0.9139643
</td>
</tr>
<tr>
<td style="text-align:left;">
full
</td>
<td style="text-align:right;">
1380
</td>
<td style="text-align:right;">
1416
</td>
<td style="text-align:right;">
116
</td>
<td style="text-align:right;">
0.9007833
</td>
<td style="text-align:right;">
0.9745763
</td>
</tr>
<tr>
<td style="text-align:left;">
last
</td>
<td style="text-align:right;">
1292
</td>
<td style="text-align:right;">
1529
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
0.8009919
</td>
<td style="text-align:right;">
0.8449967
</td>
</tr>
</tbody>
</table>

-   First name condition has second-most (slightly) *female* responses
-   Full name condition has most *female* responses
-   Last name condition has fewest *female* responses

# Main Model

Because Experiment 4 always introduces the character with a full name,
then manipulates the name form in the subsequent 3 references, the main
analysis is 1 model, as opposed to the 2 for Experiments 1 and 2.

Effects of Name Condition (first name, last name, full name) and first
name Gender Rating (centered, + fem, -masc) on the likelihood of
*female* responses, as opposed to *male* and *other* responses.
Participant and Item are included as random intercepts, with items
defined as the unique first, last and first + last name combinations.
Condition1 is the contrast between last and first+full. Condition2 is
the contrast between first and full.

``` r
exp4_m_all <- glmer(
  Female ~ Condition * GenderRatingCentered + 
    (1|Participant) + (1|Item), 
  data=exp4_d, family=binomial)
summary(exp4_m_all)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9145.4   9202.1  -4564.7   9129.4     8763 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4531 -0.5754 -0.2627  0.5724  5.4528 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2014   0.4488  
    ##  Item        (Intercept) 0.3598   0.5999  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -0.25605    0.08160  -3.138 0.001703 ** 
    ## Condition1                       0.12636    0.06170   2.048 0.040558 *  
    ## Condition2                       0.06836    0.07245   0.944 0.345422    
    ## GenderRatingCentered             0.76407    0.04590  16.648  < 2e-16 ***
    ## Condition1:GenderRatingCentered  0.13147    0.03451   3.809 0.000139 ***
    ## Condition2:GenderRatingCentered -0.10288    0.04204  -2.447 0.014403 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtn1 Cndtn2 GndrRC C1:GRC
    ## Condition1   0.012                            
    ## Condition2  -0.012 -0.016                     
    ## GndrRtngCnt -0.028  0.002  0.011              
    ## Cndtn1:GnRC  0.001 -0.121  0.016  0.035       
    ## Cndtn2:GnRC  0.011  0.016 -0.112 -0.030 -0.046

-   Less likely to recall character as female overall

-   More likely to recall character as female in the First and Full Name
    conditions than in the Last Name condition

-   More likely to recall character as female as first names become more
    feminine

**Double check the directions of the interactions:**

## L v F+F Interaction

Dummy code to get the gender rating effect for just the First and Full
Name conditions.

``` r
exp4_d$FFdummy = as.numeric(exp4_d$Condition) 
exp4_d$FFdummy[exp4_d$FFdummy == 1] <- 0
exp4_d$FFdummy[exp4_d$FFdummy == 2] <- 0
exp4_d$FFdummy[exp4_d$FFdummy == 3] <- 1
with(exp4_d, tapply(FFdummy, list(Condition), mean))
```

    ## first  full  last 
    ##     0     0     1

``` r
exp4_m_genderRatingFF <- glmer(
  Female ~ FFdummy * GenderRatingCentered + 
    (1|Participant) + (1|Item),
  data=exp4_d, family=binomial)
summary(exp4_m_genderRatingFF)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ FFdummy * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9147.7   9190.2  -4567.8   9135.7     8765 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2676 -0.5751 -0.2669  0.5734  4.9543 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2005   0.4477  
    ##  Item        (Intercept) 0.3603   0.6002  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  -0.21216    0.08437  -2.515 0.011918 *  
    ## FFdummy                      -0.12796    0.06164  -2.076 0.037901 *  
    ## GenderRatingCentered          0.80513    0.04766  16.893  < 2e-16 ***
    ## FFdummy:GenderRatingCentered -0.12944    0.03446  -3.756 0.000172 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) FFdmmy GndrRC
    ## FFdummy     -0.253              
    ## GndrRtngCnt -0.032  0.026       
    ## FFdmmy:GnRC  0.027 -0.120 -0.272

Then dummy code to get the gender rating effect just in the Last Name
condition.

``` r
exp4_d$Ldummy = as.numeric(exp4_d$Condition) 
exp4_d$Ldummy[exp4_d$Ldummy == 1] <- 1
exp4_d$Ldummy[exp4_d$Ldummy == 2] <- 1
exp4_d$Ldummy[exp4_d$Ldummy == 3] <- 0
with(exp4_d, tapply(Ldummy, list(Condition), mean))
```

    ## first  full  last 
    ##     1     1     0

``` r
exp4_m_genderRatingL <- glmer(
  Female ~ Ldummy * GenderRatingCentered + 
    (1|Participant) + (1|Item),
  data=exp4_d, family=binomial)
summary(exp4_m_genderRatingL)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Ldummy * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9147.7   9190.2  -4567.8   9135.7     8765 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2676 -0.5751 -0.2669  0.5734  4.9543 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2005   0.4477  
    ##  Item        (Intercept) 0.3603   0.6002  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 -0.34013    0.09101  -3.737 0.000186 ***
    ## Ldummy                       0.12797    0.06164   2.076 0.037901 *  
    ## GenderRatingCentered         0.67569    0.05066  13.337  < 2e-16 ***
    ## Ldummy:GenderRatingCentered  0.12944    0.03446   3.757 0.000172 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ldummy GndrRC
    ## Ldummy      -0.442              
    ## GndrRtngCnt -0.049  0.057       
    ## Ldmmy:GndRC  0.056 -0.120 -0.425

``` r
exp4_m_genderRatingFF %>% 
  tidy() %>%
  filter(term=="GenderRatingCentered") %>%
  select(estimate) %>% as.numeric()
```

    ## [1] 0.8051347

``` r
exp4_m_genderRatingL %>% 
  tidy() %>%
  filter(term=="GenderRatingCentered") %>%
  select(estimate) %>% as.numeric()
```

    ## [1] 0.6756906

Interaction indicates Gender Rating has a larger effect in the First and
Full Name conditions (0.81) than in the Last Name condition (0.67). This
makes sense because the gendered first name is repeated all 4x in the
First and Full name conditions, but only once in the Last Name
condition.

## F v F Interaction

Dummy code to get the gender rating effect for just the First Name
condition.

``` r
exp4_d$FirstDummy = as.numeric(exp4_d$Condition) 
exp4_d$FirstDummy[exp4_d$FirstDummy == 1] <- 0
exp4_d$FirstDummy[exp4_d$FirstDummy == 2] <- 1
exp4_d$FirstDummy[exp4_d$FirstDummy == 3] <- 1
with(exp4_d, tapply(FirstDummy, list(Condition), mean)) 
```

    ## first  full  last 
    ##     0     1     1

``` r
exp4_m_genderRatingFirst <- glmer(
  Female ~ FirstDummy * GenderRatingCentered + 
    (1|Participant) + (1|Item),
  data=exp4_d, family=binomial)
summary(exp4_m_genderRatingFirst)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ FirstDummy * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9151.6   9194.0  -4569.8   9139.6     8765 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4660 -0.5784 -0.2629  0.5803  5.4716 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2059   0.4538  
    ##  Item        (Intercept) 0.3592   0.5994  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -0.24885    0.09228  -2.697    0.007 ** 
    ## FirstDummy                      -0.01315    0.06304  -0.209    0.835    
    ## GenderRatingCentered             0.85944    0.05280  16.277  < 2e-16 ***
    ## FirstDummy:GenderRatingCentered -0.14454    0.03661  -3.948 7.88e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) FrstDm GndrRC
    ## FirstDummy  -0.468              
    ## GndrRtngCnt -0.055  0.065       
    ## FrstDmm:GRC  0.064 -0.125 -0.497

Dummy code to get the gender rating effect for just the Full Name
condition.

``` r
exp4_d$FullDummy = as.numeric(exp4_d$Condition) 
exp4_d$FullDummy[exp4_d$FullDummy == 1] <- 1
exp4_d$FullDummy[exp4_d$FullDummy == 2] <- 0
exp4_d$FullDummy[exp4_d$FullDummy == 3] <- 1
with(exp4_d, tapply(FullDummy, list(Condition), mean)) 
```

    ## first  full  last 
    ##     1     0     1

``` r
exp4_m_genderRatingFull <- glmer(
  Female ~ FullDummy * GenderRatingCentered + 
    (1|Participant) + (1|Item),
  data=exp4_d, family=binomial)
summary(exp4_m_genderRatingFull)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ FullDummy * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9164.2   9206.7  -4576.1   9152.2     8765 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0410 -0.5746 -0.2710  0.5694  4.7906 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2015   0.4489  
    ##  Item        (Intercept) 0.3602   0.6001  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                    -0.18022    0.09138  -1.972   0.0486 *  
    ## FullDummy                      -0.11477    0.06200  -1.851   0.0642 .  
    ## GenderRatingCentered            0.75588    0.05139  14.709   <2e-16 ***
    ## FullDummy:GenderRatingCentered  0.00606    0.03511   0.173   0.8630    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) FllDmm GndrRC
    ## FullDummy   -0.450              
    ## GndrRtngCnt -0.035  0.038       
    ## FllDmmy:GRC  0.038 -0.104 -0.451

``` r
exp4_m_genderRatingFirst %>%
  tidy() %>%
  filter(term=="GenderRatingCentered") %>%
  select(estimate) %>% as.numeric()
```

    ## [1] 0.8594438

``` r
exp4_m_genderRatingFull %>%
  tidy() %>%
  filter(term=="GenderRatingCentered") %>%
  select(estimate) %>% as.numeric()
```

    ## [1] 0.7558761

The effect of name gender rating is larger in the First Name condition
(0.86) than in the Full Name condition (0.76).

## Odds Ratios: Intercept

``` r
exp4_est_all_intercept <- exp4_m_all %>% 
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp4_est_all_intercept)
```

    ## [1] 0.774106

``` r
exp(-exp4_est_all_intercept)
```

    ## [1] 1.291813

``` r
#Save this for the table comparing all 4 experiments
exp4_OR_all_I <- exp(-exp4_est_all_intercept) %>% round(2)
```

0.77x less likely to recall as female overall. Easier to interpret:
1.29x more likely to recall as male overall, p\<.01

## Odds Ratios: Last vs First+Full

``` r
exp4_est_all_LFF <- exp4_m_all %>% 
  tidy() %>%
  filter(term=="Condition1") %>%
  select(estimate) %>% as.numeric()
exp(exp4_est_all_LFF)
```

    ## [1] 1.134694

``` r
#Save this for the table comparing all 4 experiments
exp4_OR_all_LFF <- exp(exp4_est_all_LFF) %>% 
  round(2)
```

1.13x more likely to recall as female in First + Full compared to Last,
p\<.05

## Odds Ratios: Last Only

Dummy code with Last Name as 0, so that intercept is the Last Name
condition only.

``` r
exp4_d %<>% mutate(Condition_Last=case_when(
  Condition=="first" ~ 1,
  Condition=="full" ~ 1,
  Condition=="last" ~ 0))
exp4_d$Condition_Last %<>% as.factor()
```

``` r
exp4_m_all_L <- glmer(
  Female ~ Condition_Last + (1|Participant) + (1|Item), 
  data=exp4_d, family=binomial)
summary(exp4_m_all_L)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_Last + (1 | Participant) + (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9265.9   9294.2  -4628.9   9257.9     8767 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0475 -0.5940 -0.2737  0.5750  4.4731 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.196    0.4428  
    ##  Item        (Intercept) 2.244    1.4981  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)     -0.36280    0.19562  -1.855   0.0636 .
    ## Condition_Last1  0.15844    0.06154   2.574   0.0100 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Condtn_Lst1 -0.211

``` r
exp4_est_all_L <- exp4_m_all_L %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp4_est_all_L)
```

    ## [1] 0.695725

``` r
exp(-exp4_est_all_L)
```

    ## [1] 1.437349

``` r
#Save this for the table comparing all 4 experiments
exp4_OR_all_L <- exp(-exp4_est_all_L) %>% 
  round(2)
```

0.17x times less likely to recall as female in the Last Name condition
–\> 5.72x more likely to recall as male in the Last Name condition,
p=0.06

## Odds Ratios: First and Full Only

Dummy code with First and Full Name as 0, so that intercept is average
for these two conditions.

``` r
exp4_d %<>% mutate(Condition_FF=case_when(
  Condition=="first" ~ 0,
  Condition=="full" ~ 0,
  Condition=="last" ~ 1))
exp4_d$Condition_FF %<>% as.factor()
```

``` r
exp4_m_all_FF <- glmer(
  Female ~ Condition_FF + (1|Participant) + (1|Item), 
  data=exp4_d, family=binomial)
summary(exp4_m_all_FF)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_FF + (1 | Participant) + (1 | Item)
    ##    Data: exp4_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9265.9   9294.2  -4628.9   9257.9     8767 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0475 -0.5940 -0.2737  0.5750  4.4732 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1961   0.4428  
    ##  Item        (Intercept) 2.2443   1.4981  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)   -0.20437    0.19228  -1.063    0.288  
    ## Condition_FF1 -0.15844    0.06154  -2.574    0.010 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Conditn_FF1 -0.105

``` r
exp4_est_all_FF <- exp4_m_all_FF %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp4_est_all_FF)
```

    ## [1] 0.8151591

``` r
exp(-exp4_est_all_FF)
```

    ## [1] 1.226754

``` r
#Save this for the table comparing all 4 experiments
exp4_OR_all_FF <- exp(-exp4_est_all_FF) %>% 
  round(2)
```

0.82x less likely to recall as female in First and Full Name conditions
–\> 1.23x more likely to recall as male in First and Full Name
conditions, p=.29
