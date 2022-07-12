Experiment 2: Main Analysis
================
Bethany Gardner
2022-07-07

-   [Setup](#setup)
-   [Data Summary](#data-summary)
-   [Model 1: Condition](#model-1-condition)
    -   [Odds Ratios: Intercept](#odds-ratios-intercept)
    -   [Odds Ratios: Last vs
        First+Full](#odds-ratios-last-vs-firstfull)
    -   [Odds Ratios: Last Only](#odds-ratios-last-only)
    -   [Odds Ratios: First and Full
        Only](#odds-ratios-first-and-full-only)
-   [Model 2: Condition \* Name Gender](#model-2-condition--name-gender)

# Setup

Variable names:

-   Experiment: exp2

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

Load data and select columns used in model. See data/exp2_data_about.txt
for more details.

``` r
exp2_d <- read.csv("../data/exp2_data.csv", 
                   stringsAsFactors=TRUE) %>%
  rename("Participant"="SubjID", "Item"="NameShown") %>%
  select(Participant, SubjGender, Condition, GenderRating, 
         Item, Male, Female, Other)
str(exp2_d)
```

    ## 'data.frame':    9457 obs. of  8 variables:
    ##  $ Participant : Factor w/ 1351 levels "R_06Tps0XX28Fe09j",..: 694 694 694 694 694 694 694 301 301 301 ...
    ##  $ SubjGender  : Factor w/ 5 levels "female","genderqueer",..: 3 3 3 3 3 3 3 1 1 1 ...
    ##  $ Condition   : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ GenderRating: num  5.59 4.22 2.12 6.73 3.61 4.73 1.21 6.24 4.39 2.61 ...
    ##  $ Item        : Factor w/ 105 levels "Ashley","Ashley Cook",..: 51 91 18 60 87 55 63 1 47 29 ...
    ##  $ Male        : int  1 1 0 1 1 0 1 0 0 1 ...
    ##  $ Female      : int  0 0 1 0 0 1 0 1 1 0 ...
    ##  $ Other       : int  0 0 0 0 0 0 0 0 0 0 ...

Center gender rating for names: Original scale from 1 to 7, with 1 as
most masculine and 7 as most feminine. Mean-centered with higher still
as more feminine.

``` r
exp2_d %<>% mutate(GenderRatingCentered=
            scale(GenderRating, scale=FALSE))
```

Set contrasts for name conditions.

``` r
contrasts(exp2_d$Condition) = cbind(
  "last vs first/full"=c(.33,.33,-0.66), 
  "first vs full"=c(-.5,.5,0))
contrasts(exp2_d$Condition)
```

    ##       last vs first/full first vs full
    ## first               0.33          -0.5
    ## full                0.33           0.5
    ## last               -0.66           0.0

Subset for gender rating effects (First and Full conditions only).

``` r
exp2_d_FF <- exp2_d %>% filter(Condition!="last") 
exp2_d_FF$Condition <- droplevels(exp2_d_FF$Condition)
contrasts(exp2_d_FF$Condition) = cbind(
  "first vs full"=c(-.5,.5)) #add contrast back
contrasts(exp2_d_FF$Condition)
```

    ##       first vs full
    ## first          -0.5
    ## full            0.5

# Data Summary

Responses by condition.

``` r
exp2_d %<>% mutate(ResponseAll=case_when(
  Male==1 ~ "Male",
  Female==1 ~ "Female", 
  Other==1 ~ "Other"))

exp2_d_count <- exp2_d %>% 
  group_by(Condition, ResponseAll) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from=ResponseAll,
              values_from=n) %>%
  mutate(Female_MaleOther = Female / (Male+Other),
         Female_Male = Female / Male)

kable(exp2_d_count, digits=3)
```

| Condition | Female | Male | Other | Female_MaleOther | Female_Male |
|:----------|-------:|-----:|------:|-----------------:|------------:|
| first     |   1566 | 1543 |    48 |            0.984 |       1.015 |
| full      |   1430 | 1633 |   101 |            0.825 |       0.876 |
| last      |    403 | 2490 |   243 |            0.147 |       0.162 |

-   First name condition has most *female* responses
-   Full name condition has second-most *female* responses
-   Last name condition has fewest *female* responses

# Model 1: Condition

Effect of Condition (first name, last name, full name) on likelihood of
a *female* response, as opposed to a *male* or *other* response.
Participant and Item are included as random intercepts, with items
defined as the unique first, last and first + last name combinations.
Because the condition manipulations were fully between-subject and
between-item, fitting a random slope model was not possible.

``` r
exp2_m_cond <- glmer(
  Female ~ Condition + (1|Participant) + (1|Item), 
  data=exp2_d, family=binomial)
summary(exp2_m_cond)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9177.5   9213.3  -4583.7   9167.5     9452 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9637 -0.4651 -0.2936  0.5472  4.8560 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2077   0.4557  
    ##  Item        (Intercept) 1.7857   1.3363  
    ## Number of obs: 9457, groups:  Participant, 1351; Item, 105
    ## 
    ## Fixed effects:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  -0.8800     0.1509  -5.831 5.52e-09 ***
    ## Conditionlast vs first/full   1.9863     0.3430   5.791 7.00e-09 ***
    ## Conditionfirst vs full       -0.2355     0.3452  -0.682    0.495    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cvfrs/
    ## Cndtnvfrst/ -0.170       
    ## Cndtnfrstvf -0.360 -0.241

-   Less likely overall to recall character as female.

-   Less likely to recall character as female in the Last Name condition
    as compared to the First and Full Name conditions.

## Odds Ratios: Intercept

``` r
exp2_est_cond_intercept <- exp2_m_cond %>% 
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp2_est_cond_intercept)
```

    ## [1] 0.4147892

``` r
exp(-exp2_est_cond_intercept)
```

    ## [1] 2.410863

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_all_I <- exp(-exp2_est_cond_intercept) %>% round(2)
```

0.41x less likely to recall as female overall. Easier to interpret:
2.41x more likely to recall as male/other overall, p\<.001

## Odds Ratios: Last vs First+Full

``` r
exp2_est_cond_LFF <- exp2_m_cond %>% 
  tidy() %>%
  filter(term=="Conditionlast vs first/full") %>%
  select(estimate) %>% as.numeric()
exp(exp2_est_cond_LFF)
```

    ## [1] 7.288822

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_all_LFF <- exp(exp2_est_cond_LFF) %>% round(2)
```

7.29x more likely to recall as female in First + Full compared to Last.
–\> 7.29 more likely to recall as male in Last than in First + Full,
p\<.001

## Odds Ratios: Last Only

Dummy code with Last Name as 0, so that intercept is the Last Name
condition only.

``` r
exp2_d %<>% mutate(Condition_Last=case_when(
  Condition=="first" ~ 1,
  Condition=="full" ~ 1,
  Condition=="last" ~ 0))
exp2_d$Condition_Last %<>% as.factor()
```

``` r
exp2_m_L <- glmer(
  Female ~ Condition_Last + (1|Participant) + (1|Item), 
  data=exp2_d, family=binomial)
summary(exp2_m_L)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_Last + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9175.9   9204.6  -4584.0   9167.9     9453 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9734 -0.4658 -0.2936  0.5462  4.8924 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2077   0.4557  
    ##  Item        (Intercept) 1.7929   1.3390  
    ## Number of obs: 9457, groups:  Participant, 1351; Item, 105
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -2.1910     0.2932  -7.473 7.85e-14 ***
    ## Condition_Last1   1.9107     0.3303   5.785 7.26e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Condtn_Lst1 -0.887

``` r
exp2_est_L <- exp2_m_L %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp2_est_L)
```

    ## [1] 0.1118068

``` r
exp(-exp2_est_L)
```

    ## [1] 8.943996

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_all_L <- exp(-exp2_est_L) %>% 
  round(2)
```

0.11x times less likely to recall as female in the Last Name condition
–\> 8.94x more likely to recall as male in the Last Name condition,
p\<.001

## Odds Ratios: First and Full Only

Dummy code with First and Full Name as 0, so that intercept is average
for these two conditions.

``` r
exp2_d %<>% mutate(Condition_FF=case_when(
  Condition=="first" ~ 0,
  Condition=="full" ~ 0,
  Condition=="last" ~ 1))
exp2_d$Condition_FF %<>% as.factor()
```

``` r
exp2_m_FF <- glmer(
  Female ~ Condition_FF + (1|Participant) + (1|Item), 
  data=exp2_d, family=binomial)
summary(exp2_m_FF)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_FF + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9175.9   9204.6  -4584.0   9167.9     9453 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9734 -0.4658 -0.2936  0.5462  4.8924 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2077   0.4557  
    ##  Item        (Intercept) 1.7929   1.3390  
    ## Number of obs: 9457, groups:  Participant, 1351; Item, 105
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -0.2803     0.1524  -1.839   0.0659 .  
    ## Condition_FF1  -1.9108     0.3303  -5.786 7.22e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Conditn_FF1 -0.460

``` r
exp2_est_FF <- exp2_m_FF %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp2_est_FF)
```

    ## [1] 0.7555928

``` r
exp(-exp2_est_FF)
```

    ## [1] 1.323464

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_all_FF <- exp(-exp2_est_FF) %>% 
  round(2)
```

0.75x times less likely to recall characters as female in the First and
Full Name conditions –\> 1.32x more likely to use recall characters as
male in the First and Full Name conditions, p=.07

# Model 2: Condition \* Name Gender

Effects of Condition (first name, full name) and the first name’s Gender
Rating (centered, positive=more feminine) on the likelihood of a
*female* response, as opposed to a *male* or *other* response. In
Experiment 2, the Last Name condition does not include any instances of
the gendered first name, so it is not included here. Participant and
Item are again included as random intercepts.

``` r
exp2_m_nameGender <- glmer(
  Female ~ Condition * GenderRatingCentered + 
    (1|Participant) + (1|Item), 
  data=exp2_d_FF, family=binomial)
summary(exp2_m_nameGender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp2_d_FF
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6780.8   6821.3  -3384.4   6768.8     6315 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9540 -0.6305 -0.2378  0.6395  4.4449 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1278   0.3575  
    ##  Item        (Intercept) 0.1506   0.3880  
    ## Number of obs: 6321, groups:  Participant, 903; Item, 83
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                 -0.19949    0.06001  -3.325
    ## Conditionfirst vs full                      -0.22603    0.11986  -1.886
    ## GenderRatingCentered                         0.78314    0.03574  21.909
    ## Conditionfirst vs full:GenderRatingCentered -0.07369    0.07000  -1.053
    ##                                             Pr(>|z|)    
    ## (Intercept)                                 0.000886 ***
    ## Conditionfirst vs full                      0.059331 .  
    ## GenderRatingCentered                         < 2e-16 ***
    ## Conditionfirst vs full:GenderRatingCentered 0.292442    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtvf GndrRC
    ## Cndtnfrstvf -0.342              
    ## GndrRtngCnt -0.065 -0.013       
    ## Cvfll:GndRC -0.010 -0.057 -0.304

-   Less likely overall to recall character as female in the First and
    Full Name conditions.

-   Somewhat more likely to recall the character as female in the First
    Name condition as compared to the Full Name condition (trending).

-   More likely to recall character as female as first name becomes more
    feminine.

-   No interaction between name condition and first name gender rating.
