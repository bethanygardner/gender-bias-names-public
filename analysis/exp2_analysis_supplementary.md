Experiment 2: Supplementary Analyses
================
Bethany Gardner
2022-07-07

-   [Setup](#setup)
-   [Without *Other* Responses](#without-other-responses)
    -   [Model 1: Condition w/o *Other*
        Responses](#model-1-condition-wo-other-responses)
        -   [Odds Ratios: Intercept](#odds-ratios-intercept)
        -   [Odds Ratios: Last vs
            First+Full](#odds-ratios-last-vs-firstfull)
        -   [Odds Ratios: Last Only](#odds-ratios-last-only)
        -   [Odds Ratios: First and Full
            Only](#odds-ratios-first-and-full-only)
    -   [Model 2: Condition \* Name Gender w/o *Other*
        Responses](#model-2-condition--name-gender-wo-other-responses)
-   [Quadratic Name Gender Rating](#quadratic-name-gender-rating)
    -   [Model 3: Quadratic](#model-3-quadratic)
-   [Participant Gender](#participant-gender)
    -   [Setup/Data Summary](#setupdata-summary)
    -   [Model 4: Condition \* Participant
        Gender](#model-4-condition--participant-gender)
        -   [Interaction](#interaction)
    -   [Model 5: Condition \* Name Gender \* Participant
        Gender](#model-5-condition--name-gender--participant-gender)

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

# Without *Other* Responses

The first supplementary analysis tests if excluding OTHER responses
(4.15% of total responses) affects the pattern of results.

``` r
sum(exp2_d$Other) 
```

    ## [1] 392

``` r
sum(exp2_d$Other)/length(exp2_d$Other) 
```

    ## [1] 0.04145078

Exclude *other* responses.

``` r
exp2_d_noOther <- exp2_d %>% filter(Other==0)
exp2_d_FF_noOther<- exp2_d_FF %>% filter(Other==0)
```

## Model 1: Condition w/o *Other* Responses

Effect of Name Condition (first name, last name, full name) on
likelihood of a *female* response, as opposed to a *male* response, with
*other* responses excluded. Participant and Item are again included as
random intercepts, with items defined as the unique first, last and
first + last name combinations.

``` r
exp2_m_cond_noOther <- glmer(
  Female ~ Condition + (1|Participant) + (1|Item), 
  exp2_d_noOther, family=binomial)
summary(exp2_m_cond_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8899.0   8934.6  -4444.5   8889.0     9060 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8742 -0.4709 -0.3084  0.5478  4.7464 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.100    0.3163  
    ##  Item        (Intercept) 1.789    1.3376  
    ## Number of obs: 9065, groups:  Participant, 1321; Item, 105
    ## 
    ## Fixed effects:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  -0.7936     0.1509  -5.261 1.43e-07 ***
    ## Conditionlast vs first/full   1.9098     0.3430   5.567 2.59e-08 ***
    ## Conditionfirst vs full       -0.2023     0.3451  -0.586    0.558    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cvfrs/
    ## Cndtnvfrst/ -0.170       
    ## Cndtnfrstvf -0.362 -0.241

No differences.

### Odds Ratios: Intercept

``` r
exp2_OR_noOther_I <- exp2_m_cond_noOther %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric() 
exp(exp2_OR_noOther_I)
```

    ## [1] 0.4521943

``` r
exp(-exp2_OR_noOther_I)
```

    ## [1] 2.211439

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_noOther_I <- exp(-exp2_OR_noOther_I) %>%
  round(2) 
```

0.45x less likely to recall as female overall. Easier to interpret:
2.21x more likely to recall as male/other overall, p\<.001

### Odds Ratios: Last vs First+Full

``` r
exp2_est_noOther_LFF <- exp2_m_cond_noOther %>% 
  tidy() %>%
  filter(term=="Conditionlast vs first/full") %>%
  select(estimate) %>% as.numeric() 
exp(exp2_est_noOther_LFF)
```

    ## [1] 6.75189

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_noOther_LFF <- exp(exp2_est_noOther_LFF) %>%
  round(2)
```

6.75x more likely to use *she* in First + Full compared to Last. –\>
6.75x times more likely to use *he* and *other* in Last than in First +
Full, p\<.001

### Odds Ratios: Last Only

Dummy code with Last Name as 0, so that intercept is the Last Name
condition only.

``` r
exp2_d_noOther %<>% mutate(Condition_Last=case_when(
  Condition=="first" ~ 1,
  Condition=="full" ~ 1,
  Condition=="last" ~ 0))
exp2_d_noOther$Condition_Last %<>% as.factor()
```

``` r
exp2_m_L_noOther <- glmer(
  Female ~ Condition_Last + (1|Participant) + (1|Item), 
  data=exp2_d_noOther, family=binomial)
summary(exp2_m_L_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_Last + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8897.3   8925.8  -4444.7   8889.3     9061 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8673 -0.4711 -0.3084  0.5471  4.7767 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.100    0.3163  
    ##  Item        (Intercept) 1.794    1.3396  
    ## Number of obs: 9065, groups:  Participant, 1321; Item, 105
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -2.054      0.293  -7.011 2.36e-12 ***
    ## Condition_Last1    1.843      0.330   5.584 2.35e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Condtn_Lst1 -0.887

``` r
exp2_est_L_noOther <- exp2_m_L_noOther %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp2_est_L_noOther)
```

    ## [1] 0.1281981

``` r
exp(-exp2_est_L_noOther)
```

    ## [1] 7.800429

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_noOther_L <- exp(-exp2_est_L_noOther) %>% 
  round(2) 
```

0.12x times less likely to recall as female in the Last Name condition
–\> 7.80x more likely to recall as male in the Last Name condition,
p\<.001

### Odds Ratios: First and Full Only

Dummy code with First and Full Name as 0, so that intercept is average
for these two conditions.

``` r
exp2_d_noOther %<>% mutate(Condition_FF=case_when(
  Condition=="first" ~ 0,
  Condition=="full" ~ 0,
  Condition=="last" ~ 1))
exp2_d_noOther$Condition_FF %<>% as.factor()
```

``` r
exp2_m_FF_noOther <- glmer(
  Female ~ Condition_FF + (1|Participant) + (1|Item), 
  data=exp2_d_noOther, family=binomial)
summary(exp2_m_FF_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition_FF + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8897.3   8925.8  -4444.7   8889.3     9061 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8673 -0.4711 -0.3084  0.5471  4.7766 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.100    0.3163  
    ##  Item        (Intercept) 1.794    1.3396  
    ## Number of obs: 9065, groups:  Participant, 1321; Item, 105
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -0.2114     0.1521  -1.390    0.165    
    ## Condition_FF1  -1.8428     0.3301  -5.582 2.38e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## Conditn_FF1 -0.460

``` r
exp2_est_FF_noOther <- exp2_m_FF_noOther %>%
  tidy() %>%
  filter(term=="(Intercept)") %>%
  select(estimate) %>% as.numeric()

exp(exp2_est_FF_noOther)
```

    ## [1] 0.8094392

``` r
exp(-exp2_est_FF_noOther)
```

    ## [1] 1.235423

``` r
#Save this for the table comparing all 4 experiments
exp2_OR_noOther_FF <- exp(-exp2_est_FF_noOther) %>% 
  round(2)
```

0.81x times less likely o recall as female in the First and Full Name
conditions –\> 1.24x more likely to use *he* in the n the First and Full
Name conditions, p=.17

## Model 2: Condition \* Name Gender w/o *Other* Responses

Effects of Name Condition (first name, full name) and the first name’s
Gender Rating (centered, positive=more feminine) on the likelihood of a
*female* response as opposed to a *male* response, with *other*
responses excluded. In Experiment 2, the Last Name condition does not
include any instances of the gendered first name, so it is not included
here. Participant and Item are again included as random intercepts.

``` r
exp2_m_nameGender_noOther <- glmer(
  Female ~ Condition * GenderRatingCentered + (1|Participant) + (1|Item), 
  exp2_d_FF_noOther, family=binomial)
summary(exp2_m_nameGender_noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: exp2_d_FF_noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6559.7   6600.1  -3273.9   6547.7     6166 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8717 -0.5959 -0.2364  0.6050  4.2845 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.0268   0.1637  
    ##  Item        (Intercept) 0.1459   0.3819  
    ## Number of obs: 6172, groups:  Participant, 897; Item, 83
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                 -0.13756    0.05860  -2.348
    ## Conditionfirst vs full                      -0.19127    0.11704  -1.634
    ## GenderRatingCentered                         0.78486    0.03552  22.098
    ## Conditionfirst vs full:GenderRatingCentered -0.06500    0.06969  -0.933
    ##                                             Pr(>|z|)    
    ## (Intercept)                                   0.0189 *  
    ## Conditionfirst vs full                        0.1022    
    ## GenderRatingCentered                          <2e-16 ***
    ## Conditionfirst vs full:GenderRatingCentered   0.3509    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtvf GndrRC
    ## Cndtnfrstvf -0.348              
    ## GndrRtngCnt -0.059 -0.012       
    ## Cvfll:GndRC -0.010 -0.053 -0.299

Compared to the main analysis including *other* responses, the intercept
has a larger p-value, the difference between the First and Full Name
conditions is no longer trending, and the Name Gender Rating is the
same.

# Quadratic Name Gender Rating

The second supplementary analysis tested the effect of squared name
gender rating, such that larger values meant names with stronger gender
associations (masc or fem), and smaller values meant names with weaker
gender associations.

``` r
exp2_d_FF %<>% mutate(GenderRatingSquared=GenderRatingCentered^2)
```

## Model 3: Quadratic

No quadratic effects.

``` r
exp2_m_nameGenderQuad <- glmer(
  Female ~ Condition*GenderRatingCentered + Condition*GenderRatingSquared +
    (1|Participant) + (1|Item), 
  exp2_d_FF, family="binomial")
summary(exp2_m_nameGenderQuad)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## Female ~ Condition * GenderRatingCentered + Condition * GenderRatingSquared +  
    ##     (1 | Participant) + (1 | Item)
    ##    Data: exp2_d_FF
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6784.5   6838.5  -3384.2   6768.5     6313 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9223 -0.6307 -0.2334  0.6387  4.5272 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1278   0.3575  
    ##  Item        (Intercept) 0.1503   0.3877  
    ## Number of obs: 6321, groups:  Participant, 903; Item, 83
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                 -0.16962    0.08275  -2.050
    ## Conditionfirst vs full                      -0.25725    0.16551  -1.554
    ## GenderRatingCentered                         0.77974    0.03630  21.481
    ## GenderRatingSquared                         -0.01045    0.02004  -0.521
    ## Conditionfirst vs full:GenderRatingCentered -0.06953    0.07121  -0.976
    ## Conditionfirst vs full:GenderRatingSquared   0.01019    0.04004   0.254
    ##                                             Pr(>|z|)    
    ## (Intercept)                                   0.0404 *  
    ## Conditionfirst vs full                        0.1201    
    ## GenderRatingCentered                          <2e-16 ***
    ## GenderRatingSquared                           0.6020    
    ## Conditionfirst vs full:GenderRatingCentered   0.3289    
    ## Conditionfirst vs full:GenderRatingSquared    0.7992    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtvf GndrRC GndrRS Cvf:GRC
    ## Cndtnfrstvf -0.380                             
    ## GndrRtngCnt -0.166  0.054                      
    ## GndrRtngSqr -0.688  0.262  0.170               
    ## Cvfll:GndRC  0.059 -0.166 -0.314 -0.088        
    ## Cvfll:GndRS  0.262 -0.689 -0.084 -0.339  0.177

# Participant Gender

## Setup/Data Summary

The third supplementary analysis looks at participant gender: if male
participants show a larger bias to recall the character as male than
non-male participants.

Participants entered their gender in a free-response box.

``` r
exp2_d %>% group_by(SubjGender) %>% 
  summarise(total=n_distinct(Participant)) %>%
  kable()
```

| SubjGender  | total |
|:------------|------:|
| female      |   566 |
| genderqueer |     1 |
| male        |   694 |
| N/A         |    88 |
| non-binary  |     2 |

For this analysis, we exclude participants who did not respond. Because
there are not enough participants to create 3 groups, we compare male to
non-male participants.

``` r
exp2_d_gender <- exp2_d %>% 
  filter(SubjGender != "N/A") %>%
  mutate(SubjGenderMale=(ifelse(SubjGender=="male", 1, 0)))

exp2_d_gender %>% group_by(SubjGenderMale) %>% 
  summarise(total=n_distinct(Participant)) %>%
  kable()
```

| SubjGenderMale | total |
|---------------:|------:|
|              0 |   569 |
|              1 |   694 |

Summary of responses by condition and participant gender.

``` r
exp2_d_gender %<>% mutate(ResponseAll=case_when(
  Male==1 ~ "Male",
  Female==1 ~ "Female", 
  Other==1 ~ "Other"))

exp2_d_gender <- exp2_d %>% 
  filter(SubjGender != "N/A") %>%
  mutate(SubjGenderMale=(ifelse(SubjGender=="male", 1, 0)))

exp2_d_gender %>% group_by(SubjGenderMale) %>% 
  summarise(total=n_distinct(Participant)) %>%
  kable()
```

| SubjGenderMale | total |
|---------------:|------:|
|              0 |   569 |
|              1 |   694 |

Participant gender is mean centered effects coded, comparing non-male
participants to male participants.

``` r
exp2_d_gender$SubjGenderMale %<>% as.factor()
contrasts(exp2_d_gender$SubjGenderMale)=cbind("NM_M"=c(-.5,.5)) 
contrasts(exp2_d_gender$SubjGenderMale)
```

    ##   NM_M
    ## 0 -0.5
    ## 1  0.5

Subset First and Full conditions.

``` r
exp2_d_FF_gender <- exp2_d_gender %>% filter(Condition!="last")
exp2_d_FF_gender$Condition <- droplevels(exp2_d_FF_gender$Condition)
contrasts(exp2_d_FF_gender$Condition) = 
  cbind("first vs full"=c(-.5,.5)) #add contrast back
contrasts(exp2_d_FF_gender$Condition)
```

    ##       first vs full
    ## first          -0.5
    ## full            0.5

## Model 4: Condition \* Participant Gender

Effect of Name Condition (first name, last name, full name) and
Participant Gender (non-male vs male) on likelihood of a *female*
response, as opposed to a *male* response or *other* response.
Participant and Item are again included as random intercepts.

``` r
exp2_m_cond_gender <- glmer(
  Female ~ Condition * SubjGenderMale + (1|Participant) + (1|Item), 
  exp2_d_gender, family=binomial)
summary(exp2_m_cond_gender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * SubjGenderMale + (1 | Participant) + (1 |  
    ##     Item)
    ##    Data: exp2_d_gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8545.3   8602.0  -4264.7   8529.3     8833 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6710 -0.4651 -0.2896  0.5581  4.7148 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.195    0.4416  
    ##  Item        (Intercept) 1.832    1.3535  
    ## Number of obs: 8841, groups:  Participant, 1263; Item, 105
    ## 
    ## Fixed effects:
    ##                                                Estimate Std. Error z value
    ## (Intercept)                                    -0.87078    0.15305  -5.689
    ## Conditionlast vs first/full                     2.00537    0.34799   5.763
    ## Conditionfirst vs full                         -0.20540    0.35009  -0.587
    ## SubjGenderMaleNM_M                             -0.12519    0.06240  -2.006
    ## Conditionlast vs first/full:SubjGenderMaleNM_M -0.39906    0.14326  -2.786
    ## Conditionfirst vs full:SubjGenderMaleNM_M       0.06204    0.14105   0.440
    ##                                                Pr(>|z|)    
    ## (Intercept)                                    1.28e-08 ***
    ## Conditionlast vs first/full                    8.28e-09 ***
    ## Conditionfirst vs full                          0.55741    
    ## SubjGenderMaleNM_M                              0.04482 *  
    ## Conditionlast vs first/full:SubjGenderMaleNM_M  0.00534 ** 
    ## Conditionfirst vs full:SubjGenderMaleNM_M       0.66002    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cvfrs/ Cndtvf SGMNM_ Cvf/:S
    ## Cndtnvfrst/ -0.169                            
    ## Cndtnfrstvf -0.359 -0.240                     
    ## SbjGndMNM_M -0.021  0.001 -0.001              
    ## Cvf/:SGMNM_  0.003 -0.022 -0.001 -0.194       
    ## Cvf:SGMNM_M -0.002 -0.001 -0.024 -0.002 -0.001

-   Male participants are less likely to recall the character as female
    overall, but this is not significant after correction for multiple
    comparisons.

-   The interaction between Condition (Last vs. First + Full) and
    Participant Gender is significant.

### Interaction

Dummy code to get the Participant Gender effect just for First and Full
Name conditions.

``` r
exp2_d_gender$FFdummy = as.numeric(exp2_d_gender$Condition) 
exp2_d_gender$FFdummy[exp2_d_gender$FFdummy == 1] <- 0
exp2_d_gender$FFdummy[exp2_d_gender$FFdummy == 2] <- 0
exp2_d_gender$FFdummy[exp2_d_gender$FFdummy == 3] <- 1
with(exp2_d_gender, tapply(FFdummy, list(Condition), mean)) 
```

    ## first  full  last 
    ##     0     0     1

``` r
exp2_m_cond_genderFF <- glmer(Female ~ 
    FFdummy*SubjGenderMale + (1|Participant) + (1|Item), 
    data=exp2_d_gender, family=binomial)
summary(exp2_m_cond_genderFF)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ FFdummy * SubjGenderMale + (1 | Participant) + (1 |  
    ##     Item)
    ##    Data: exp2_d_gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8541.9   8584.4  -4264.9   8529.9     8835 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7121 -0.4647 -0.2896  0.5561  4.7190 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1953   0.4419  
    ##  Item        (Intercept) 1.8379   1.3557  
    ## Number of obs: 8841, groups:  Participant, 1263; Item, 105
    ## 
    ## Fixed effects:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                -0.25645    0.15463  -1.658 0.097228 .  
    ## FFdummy                    -1.93803    0.33481  -5.788 7.11e-09 ***
    ## SubjGenderMaleNM_M         -0.25700    0.07058  -3.641 0.000271 ***
    ## FFdummy:SubjGenderMaleNM_M  0.39519    0.14182   2.787 0.005326 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) FFdmmy SGMNM_
    ## FFdummy     -0.461              
    ## SbjGndMNM_M -0.028  0.015       
    ## FFd:SGMNM_M  0.014 -0.023 -0.498

Then dummy code to get the participant gender effect just for Last Name
condition.

``` r
exp2_d_gender$Ldummy = as.numeric(exp2_d_gender$Condition) 
exp2_d_gender$Ldummy[exp2_d_gender$Ldummy == 1] <- 1
exp2_d_gender$Ldummy[exp2_d_gender$Ldummy == 2] <- 1
exp2_d_gender$Ldummy[exp2_d_gender$Ldummy == 3] <- 0
with(exp2_d_gender, tapply(Ldummy, list(Condition), mean)) 
```

    ## first  full  last 
    ##     1     1     0

``` r
exp2_m_cond_genderL <- glmer(Female ~ 
    Ldummy*SubjGenderMale + (1|Participant) + (1|Item), 
    data=exp2_d_gender, family=binomial)
summary(exp2_m_cond_genderL)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Ldummy * SubjGenderMale + (1 | Participant) + (1 | Item)
    ##    Data: exp2_d_gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8541.9   8584.4  -4264.9   8529.9     8835 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7121 -0.4647 -0.2896  0.5561  4.7190 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1953   0.4419  
    ##  Item        (Intercept) 1.8379   1.3557  
    ## Number of obs: 8841, groups:  Participant, 1263; Item, 105
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                -2.1944     0.2970  -7.388 1.49e-13 ***
    ## Ldummy                      1.9380     0.3347   5.791 7.02e-09 ***
    ## SubjGenderMaleNM_M          0.1382     0.1230   1.124  0.26120    
    ## Ldummy:SubjGenderMaleNM_M  -0.3952     0.1418  -2.787  0.00532 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Ldummy SGMNM_
    ## Ldummy      -0.887              
    ## SbjGndMNM_M -0.020  0.018       
    ## Ldm:SGMNM_M  0.019 -0.023 -0.867

-   Beta for subj gender in First + Full: -0.25700
-   Beta for subj gender in Last: 0.1382 NS

–\> Male participants were less likely to recall the referent as female
than non-male participants in the First and Full Name conditions. No
participant gender difference in the Last Name condition.

## Model 5: Condition \* Name Gender \* Participant Gender

Effects of Name Condition (first name, full name), the first name’s
Gender Rating (centered, positive=more feminine), and Participant Gender
(non-male vs. male) on the likelihood of a *female* response as opposed
to *male* or *other* responses. In Experiment 2, the Last Name condition
does not include any instances of the gendered first name, so it is not
included here.

``` r
exp2_m_nameGender_gender <- buildmer(formula=
            (Female ~ Condition * GenderRatingCentered * SubjGenderMale + 
            (1|Participant) + (1|Item)), 
            data=exp2_d_FF_gender, family=binomial, 
            direction=c("order"), quiet=TRUE)
summary(exp2_m_nameGender_gender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) (p-values based on Wald z-scores) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ 1 + GenderRatingCentered + SubjGenderMale + Condition +  
    ##     GenderRatingCentered:SubjGenderMale + GenderRatingCentered:Condition +  
    ##     SubjGenderMale:Condition + GenderRatingCentered:SubjGenderMale:Condition +  
    ##     (1 | Item) + (1 | Participant)
    ##    Data: exp2_d_FF_gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   6261.5   6328.3  -3120.7   6241.5     5870 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3535 -0.6348 -0.2316  0.6346  4.4956 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1147   0.3387  
    ##  Item        (Intercept) 0.1590   0.3987  
    ## Number of obs: 5880, groups:  Participant, 840; Item, 83
    ## 
    ## Fixed effects:
    ##                                                                Estimate
    ## (Intercept)                                                    -0.18616
    ## GenderRatingCentered                                            0.80851
    ## SubjGenderMaleNM_M                                             -0.21834
    ## Conditionfirst vs full                                         -0.20510
    ## GenderRatingCentered:SubjGenderMaleNM_M                        -0.15373
    ## GenderRatingCentered:Conditionfirst vs full                    -0.06485
    ## SubjGenderMaleNM_M:Conditionfirst vs full                       0.10276
    ## GenderRatingCentered:SubjGenderMaleNM_M:Conditionfirst vs full -0.12901
    ##                                                                Std. Error
    ## (Intercept)                                                       0.06211
    ## GenderRatingCentered                                              0.03749
    ## SubjGenderMaleNM_M                                                0.06894
    ## Conditionfirst vs full                                            0.12412
    ## GenderRatingCentered:SubjGenderMaleNM_M                           0.04501
    ## GenderRatingCentered:Conditionfirst vs full                       0.07337
    ## SubjGenderMaleNM_M:Conditionfirst vs full                         0.13781
    ## GenderRatingCentered:SubjGenderMaleNM_M:Conditionfirst vs full    0.08998
    ##                                                                 z value
    ## (Intercept)                                                    -2.99716
    ## GenderRatingCentered                                           21.56648
    ## SubjGenderMaleNM_M                                             -3.16697
    ## Conditionfirst vs full                                         -1.65246
    ## GenderRatingCentered:SubjGenderMaleNM_M                        -3.41552
    ## GenderRatingCentered:Conditionfirst vs full                    -0.88395
    ## SubjGenderMaleNM_M:Conditionfirst vs full                       0.74569
    ## GenderRatingCentered:SubjGenderMaleNM_M:Conditionfirst vs full -1.43370
    ##                                                                Pr(>|z|)
    ## (Intercept)                                                       0.003
    ## GenderRatingCentered                                              0.000
    ## SubjGenderMaleNM_M                                                0.002
    ## Conditionfirst vs full                                            0.098
    ## GenderRatingCentered:SubjGenderMaleNM_M                           0.001
    ## GenderRatingCentered:Conditionfirst vs full                       0.377
    ## SubjGenderMaleNM_M:Conditionfirst vs full                         0.456
    ## GenderRatingCentered:SubjGenderMaleNM_M:Conditionfirst vs full    0.152
    ##                                                                Pr(>|t|)    
    ## (Intercept)                                                    0.002725 ** 
    ## GenderRatingCentered                                            < 2e-16 ***
    ## SubjGenderMaleNM_M                                             0.001540 ** 
    ## Conditionfirst vs full                                         0.098441 .  
    ## GenderRatingCentered:SubjGenderMaleNM_M                        0.000637 ***
    ## GenderRatingCentered:Conditionfirst vs full                    0.376724    
    ## SubjGenderMaleNM_M:Conditionfirst vs full                      0.455854    
    ## GenderRatingCentered:SubjGenderMaleNM_M:Conditionfirst vs full 0.151657    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) GndrRC SGMNM_ Cndtvf GRC:SG GRC:Cvf SGMNvf
    ## GndrRtngCnt -0.064                                           
    ## SbjGndMNM_M -0.091 -0.006                                    
    ## Cndtnfrstvf -0.334 -0.013 -0.018                             
    ## GRC:SGMNM_M -0.007 -0.145 -0.133  0.023                      
    ## GndrRtC:Cvf -0.010 -0.283  0.020 -0.057 -0.035               
    ## SGMNM_M:Cvf -0.019  0.021  0.010 -0.092 -0.034  0.001        
    ## GRC:SGMNMvf  0.023 -0.036 -0.034 -0.008 -0.004 -0.142  -0.134

-   Male participants are less likely to recall the character as female
    overall. This matches the results of the interaction in the
    condition-only model.
-   The interaction between participant gender and first name gender
    rating is significant. Smaller effect of name gender rating in male
    participants.
-   Interaction with Condition, three-way interaction with Name Gender
    and Condition n.s.
