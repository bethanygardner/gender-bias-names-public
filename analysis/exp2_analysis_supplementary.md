Experiment 2: Supplementary Analyses
================
Bethany Gardner
3/29/2022

-   [Setup](#setup)
-   [Without OTHER responses](#without-other-responses)
    -   [Model 1: Condition w/o OTHER](#model-1-condition-wo-other)
    -   [Model 2: Condition \* Name Gender w/o
        OTHER](#model-2-condition--name-gender-wo-other)
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

Load data and select columns used in model. See data/exp2_data_about.txt
for more details.

``` r
d <- read.csv("../data/exp2_data.csv", stringsAsFactors=TRUE) %>%
  rename("Participant"="SubjID", "Item"="NameShown") %>%
  select(Participant, SubjGender, Condition, GenderRating, Item, 
         Male, Female, Other)
str(d)
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

# Without OTHER responses

The first supplementary analysis tests if excluding OTHER responses
(4.15% of total responses) affects the pattern of results.

``` r
o <- sum(d$Other) 
o
```

    ## [1] 392

``` r
o/length(d$Other) 
```

    ## [1] 0.04145078

Exclude OTHER responses.

``` r
d.noOther <- d %>% filter(Other==0)

d.FF.noOther <- d.FF %>% filter(Other==0)
```

## Model 1: Condition w/o OTHER

Effect of Name Condition (first name, last name, full name) on
likelihood of a FEMALE response, as opposed to a MALE response, with
OTHER responses excluded. Participant and Item are again included as
random intercepts, with items defined as the unique first, last and
first + last name combinations.

``` r
m.cond_other <- glmer(Female ~ Condition + (1|Participant) + (1|Item), 
                data=d.noOther, family=binomial)
summary(m.cond_other)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition + (1 | Participant) + (1 | Item)
    ##    Data: d.noOther
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

## Model 2: Condition \* Name Gender w/o OTHER

Effects of Name Condition (first name, full name) and the first name’s
Gender Rating (centered, positive=more feminine) on the likelihood of a
FEMALE response as opposed to a MALE response, with OTHER responses
excluded. In Experiment 2, the Last Name condition does not include any
instances of the gendered first name, so it is not included here.
Participant and Item are again included as random intercepts.

``` r
m.namegender_other <- glmer(Female ~ Condition * GenderRatingCentered + 
                            (1|Participant) + (1|Item), 
                            data=d.FF.noOther, family=binomial)
summary(m.namegender_other)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: d.FF.noOther
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

Compared to the main analysis including OTHER responses, the intercept
has a larger p-value, the difference between the First and Full Name
conditions is no longer trending, and the Name Gender Rating is the
same.

# Quadratic Name Gender Rating

The second supplementary analysis tested the effect of squared name
gender rating, such that larger values meant names with stronger gender
associations (masc or fem), and smaller values meant names with weaker
gender associations.

``` r
d.FF %<>% mutate(GenderRatingSquared=GenderRatingCentered^2)
```

## Model 3: Quadratic

No quadratic effects.

``` r
m.namegender_squared <- glmer(Female ~ Condition*GenderRatingCentered + 
                              Condition*GenderRatingSquared +
                              (1|Participant) + (1|Item), 
                              d.FF, family="binomial")
summary(m.namegender_squared)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## Female ~ Condition * GenderRatingCentered + Condition * GenderRatingSquared +  
    ##     (1 | Participant) + (1 | Item)
    ##    Data: d.FF
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
participants show a larger bias to recall the character as MALE than
non-male participants.

Participants entered their gender in a free-response box.

``` r
d %>% group_by(SubjGender) %>% 
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
d.gender <- d %>% filter(SubjGender != "N/A") %>%
            mutate(SubjGenderMale=(ifelse(SubjGender=="male", 1, 0)))

d.gender %>% group_by(SubjGenderMale) %>% 
  summarise(total=n_distinct(Participant)) %>%
  kable()
```

| SubjGenderMale | total |
|---------------:|------:|
|              0 |   569 |
|              1 |   694 |

Summary of responses by condition and participant gender.

``` r
d.gender %<>% mutate(ResponseAll=case_when(
              Male==1 ~ "Male",
              Female==1 ~ "Female", 
              Other==1 ~ "Other"))

d.gender.count_responses <- d.gender %>% 
  group_by(Condition, ResponseAll, SubjGenderMale) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from=c(ResponseAll),
              values_from=n) %>%
  mutate(Female_MaleOther = Female / (Male+Other),
         Female_Male = Female / Male) %>%
  rename("ParticipantGender"="SubjGenderMale") 
d.gender.count_responses$ParticipantGender %<>% recode("0"="Non-male", "1"="Male")

kable(d.gender.count_responses)
```

| Condition | ParticipantGender | Female | Male | Other | Female_MaleOther | Female_Male |
|:----------|:------------------|-------:|-----:|------:|-----------------:|------------:|
| first     | Non-male          |    684 |  609 |    30 |        1.0704225 |   1.1231527 |
| first     | Male              |    780 |  847 |    18 |        0.9017341 |   0.9208973 |
| full      | Non-male          |    595 |  609 |    49 |        0.9042553 |   0.9770115 |
| full      | Male              |    724 |  893 |    42 |        0.7743316 |   0.8107503 |
| last      | Non-male          |    170 | 1145 |    92 |        0.1374293 |   0.1484716 |
| last      | Male              |    210 | 1223 |   121 |        0.1562500 |   0.1717089 |

Participant gender is mean centered effects coded, comparing non-male
participants to male participants.

``` r
d.gender$SubjGenderMale %<>% as.factor()
contrasts(d.gender$SubjGenderMale)=cbind("NM_M"=c(-.5,.5)) 
contrasts(d.gender$SubjGenderMale)
```

    ##   NM_M
    ## 0 -0.5
    ## 1  0.5

Subset First and Full conditions.

``` r
d.FF.gender <- d.gender %>% filter(Condition!="last")
d.FF.gender$Condition <- droplevels(d.FF.gender$Condition)
contrasts(d.FF.gender$Condition) = 
  cbind("first vs full"=c(-.5,.5)) #add contrast back
contrasts(d.FF.gender$Condition)
```

    ##       first vs full
    ## first          -0.5
    ## full            0.5

## Model 4: Condition \* Participant Gender

Effect of Name Condition (first name, last name, full name) and
Participant Gender (non-male vs male) on likelihood of a FEMALE
response, as opposed to a MALE response or OTHER response. Participant
and Item are again included as random intercepts.

``` r
m.cond_subjgender <- glmer(Female ~ Condition * SubjGenderMale + 
            (1|Participant) + (1|Item), 
            data=d.gender, family=binomial)
summary(m.cond_subjgender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * SubjGenderMale + (1 | Participant) + (1 |  
    ##     Item)
    ##    Data: d.gender
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
d.gender$FFdummy = as.numeric(d.gender$Condition) 
d.gender$FFdummy[d.gender$FFdummy == 1] <- 0
d.gender$FFdummy[d.gender$FFdummy == 2] <- 0
d.gender$FFdummy[d.gender$FFdummy == 3] <- 1
with(d.gender, tapply(FFdummy, list(Condition), mean)) 
```

    ## first  full  last 
    ##     0     0     1

``` r
m.cond_subjgender_FF <- glmer(Female ~ 
    FFdummy*SubjGenderMale + (1|Participant) + (1|Item), 
    data=d.gender, family=binomial)
summary(m.cond_subjgender_FF)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ FFdummy * SubjGenderMale + (1 | Participant) + (1 |  
    ##     Item)
    ##    Data: d.gender
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
d.gender$Ldummy = as.numeric(d.gender$Condition) 
d.gender$Ldummy[d.gender$Ldummy == 1] <- 1
d.gender$Ldummy[d.gender$Ldummy == 2] <- 1
d.gender$Ldummy[d.gender$Ldummy == 3] <- 0
with(d.gender, tapply(Ldummy, list(Condition), mean)) 
```

    ## first  full  last 
    ##     1     1     0

``` r
m.cond_subjgender_L <- glmer(Female ~ 
    Ldummy*SubjGenderMale + (1|Participant) + (1|Item), 
    data=d.gender, family=binomial)
summary(m.cond_subjgender_L)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Ldummy * SubjGenderMale + (1 | Participant) + (1 | Item)
    ##    Data: d.gender
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

–> Male participants were less likely to recall the referent as female
than non-male participants in the First and Full Name conditions. No
participant gender difference in the Last Name condition.

## Model 5: Condition \* Name Gender \* Participant Gender

Effects of Name Condition (first name, full name), the first name’s
Gender Rating (centered, positive=more feminine), and Participant Gender
(non-male vs. male) on the likelihood of a FEMALE response as opposed to
MALE or OTHER responses. In Experiment 2, the Last Name condition does
not include any instances of the gendered first name, so it is not
included here.

``` r
m.cond_name_subjgender <- glmer(Female ~ 
      Condition * GenderRatingCentered * SubjGenderMale + 
      (1|Participant) + (1|Item), 
      data=d.FF.gender, family=binomial)
summary(m.cond_name_subjgender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered * SubjGenderMale +  
    ##     (1 | Participant) + (1 | Item)
    ##    Data: d.FF.gender
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
    ## (Intercept)                                                    -0.18617
    ## Conditionfirst vs full                                         -0.20509
    ## GenderRatingCentered                                            0.80851
    ## SubjGenderMaleNM_M                                             -0.21835
    ## Conditionfirst vs full:GenderRatingCentered                    -0.06485
    ## Conditionfirst vs full:SubjGenderMaleNM_M                       0.10276
    ## GenderRatingCentered:SubjGenderMaleNM_M                        -0.15373
    ## Conditionfirst vs full:GenderRatingCentered:SubjGenderMaleNM_M -0.12900
    ##                                                                Std. Error
    ## (Intercept)                                                       0.06211
    ## Conditionfirst vs full                                            0.12411
    ## GenderRatingCentered                                              0.03749
    ## SubjGenderMaleNM_M                                                0.06894
    ## Conditionfirst vs full:GenderRatingCentered                       0.07337
    ## Conditionfirst vs full:SubjGenderMaleNM_M                         0.13780
    ## GenderRatingCentered:SubjGenderMaleNM_M                           0.04501
    ## Conditionfirst vs full:GenderRatingCentered:SubjGenderMaleNM_M    0.08998
    ##                                                                z value Pr(>|z|)
    ## (Intercept)                                                     -2.997 0.002724
    ## Conditionfirst vs full                                          -1.652 0.098450
    ## GenderRatingCentered                                            21.566  < 2e-16
    ## SubjGenderMaleNM_M                                              -3.167 0.001539
    ## Conditionfirst vs full:GenderRatingCentered                     -0.884 0.376742
    ## Conditionfirst vs full:SubjGenderMaleNM_M                        0.746 0.455840
    ## GenderRatingCentered:SubjGenderMaleNM_M                         -3.415 0.000637
    ## Conditionfirst vs full:GenderRatingCentered:SubjGenderMaleNM_M  -1.434 0.151669
    ##                                                                   
    ## (Intercept)                                                    ** 
    ## Conditionfirst vs full                                         .  
    ## GenderRatingCentered                                           ***
    ## SubjGenderMaleNM_M                                             ** 
    ## Conditionfirst vs full:GenderRatingCentered                       
    ## Conditionfirst vs full:SubjGenderMaleNM_M                         
    ## GenderRatingCentered:SubjGenderMaleNM_M                        ***
    ## Conditionfirst vs full:GenderRatingCentered:SubjGenderMaleNM_M    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtvf GndrRC SGMNM_ Cvfl:GRC Cvf:SG GRC:SG
    ## Cndtnfrstvf -0.334                                            
    ## GndrRtngCnt -0.064 -0.013                                     
    ## SbjGndMNM_M -0.090 -0.018 -0.006                              
    ## Cvfll:GndRC -0.010 -0.057 -0.283  0.020                       
    ## Cvf:SGMNM_M -0.019 -0.092  0.021  0.010  0.001                
    ## GRC:SGMNM_M -0.007  0.023 -0.145 -0.133 -0.035   -0.034       
    ## Cvf:GRC:SGM  0.023 -0.008 -0.036 -0.034 -0.142   -0.134 -0.004

-   Male participants are less likely to recall the character as female
    overall. This matches the results of the interaction in the
    condition-only model.
-   The interaction between participant gender and first name gender
    rating is significant. Smaller effect of name gender rating in male
    participants.
-   Interaction with Condition, three-way interaction with Name Gender
    and Condition n.s.
