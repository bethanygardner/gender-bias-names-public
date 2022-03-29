Experiment 2: Main Analysis
================
Bethany Gardner
3/29/2022

-   [Setup](#setup)
-   [Data summary](#data-summary)
-   [Model 1: Condition](#model-1-condition)
-   [Model 2: Condition \* Name Gender](#model-2-condition--name-gender)

# Setup

Load data and select columns used in model. See data/exp2_data_about.txt
for more details.

``` r
d <- read.csv("../data/exp2_data.csv", stringsAsFactors=TRUE) %>%
  rename("Participant"="SubjID", "Item"="NameShown") %>%
  select(Participant, Condition, GenderRating, Item, Male, Female, Other)
str(d)
```

    ## 'data.frame':    9457 obs. of  7 variables:
    ##  $ Participant : Factor w/ 1351 levels "R_06Tps0XX28Fe09j",..: 694 694 694 694 694 694 694 301 301 301 ...
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
d %<>% mutate(GenderRatingCentered=scale(d$GenderRating, scale=FALSE))
```

Set contrasts for name conditions.

``` r
contrasts(d$Condition) = cbind("last vs first/full"=c(.33,.33,-0.66), 
                               "first vs full"=c(-.5,.5,0))
contrasts(d$Condition)
```

    ##       last vs first/full first vs full
    ## first               0.33          -0.5
    ## full                0.33           0.5
    ## last               -0.66           0.0

Subset for gender rating effects (First and Full conditions only).

``` r
d.FF <- d %>% filter(Condition!="last") 
d.FF$Condition <- droplevels(d.FF$Condition)
contrasts(d.FF$Condition) = cbind("first vs full"=c(-.5,.5)) #add contrast back
contrasts(d.FF$Condition)
```

    ##       first vs full
    ## first          -0.5
    ## full            0.5

# Data summary

Responses by condition.

``` r
d <- d %>% mutate(ResponseAll=case_when(
           Male==1 ~ "Male",
           Female==1 ~ "Female", 
           Other==1 ~ "Other"))

d.count_responses <- d %>% group_by(Condition, ResponseAll) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from=ResponseAll,
              values_from=n) %>%
  mutate(Female_MaleOther = Female / (Male+Other),
         Female_Male = Female / Male)

print(d.count_responses)
```

    ## # A tibble: 3 x 6
    ## # Groups:   Condition [3]
    ##   Condition Female  Male Other Female_MaleOther Female_Male
    ##   <fct>      <int> <int> <int>            <dbl>       <dbl>
    ## 1 first       1566  1543    48            0.984       1.01 
    ## 2 full        1430  1633   101            0.825       0.876
    ## 3 last         403  2490   243            0.147       0.162

-   First name condition has most FEMALE responses
-   Full name condition has second-most FEMALE responses
-   Last name condition has fewest FEMALE responses

# Model 1: Condition

Effect of Name Condition (first name, last name, full name) on
likelihood of a FEMALE response, as opposed to a MALE or OTHER response.
Participant and Item are included as random intercepts, with items
defined as the unique first, last and first + last name combinations.
Because the condition manipulations were fully between-subject and
between-item, fitting a random slope model was not possible.

``` r
m.cond <- glmer(Female ~ Condition + (1|Participant) + (1|Item), 
            data=d, family=binomial)
summary(m.cond)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition + (1 | Participant) + (1 | Item)
    ##    Data: d
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

Less likely overall to recall character as FEMALE. Less likely to recall
character as FEMALE in the Last Name condition as compared to the First
and Full Name conditions.

# Model 2: Condition \* Name Gender

Effects of Name Condition (first name, full name) and the first name’s
Gender Rating (centered, positive=more feminine) on the likelihood of a
FEMALE response, as opposed to a MALE or OTHER response. In Experiment
2, the Last Name condition does not include any instances of the
gendered first name, so it is not included here. Participant and Item
are again included as random intercepts.

``` r
m.namegender <- glmer(Female ~ Condition * GenderRatingCentered + 
            (1|Participant) + (1|Item), 
            data=d.FF, family=binomial)
summary(m.namegender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: d.FF
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

Less likely overall to recall character as female in the First and Full
Name conditions. Somewhat more likely to recall the character as female
in the First Name condition as compared to the Full Name condition
(trending). More likely to recall character as female as first name
becomes more feminine. No interaction between name condition and first
name gender rating.
