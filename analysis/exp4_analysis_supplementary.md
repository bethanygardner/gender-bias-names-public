Experiment 4: Supplementary Analyses
================
Bethany Gardner
4/07/2022

-   [Setup](#setup)
-   [Without *Other* Responses](#without-other-responses)
-   [Quadratic Name Gender Rating](#quadratic-name-gender-rating)
-   [Participant Gender](#participant-gender)
    -   [Setup/Data Summary](#setupdata-summary)
    -   [Model: Condition \* Name Gender \* Participant
        Gender](#model-condition--name-gender--participant-gender)

# Setup

Load data and select columns used in model. See data/exp4_data_about.txt
for more details.

``` r
d <- read.csv("../data/exp4_data.csv", stringsAsFactors=TRUE) %>%
  rename("Participant"="SubjID", "Item"="Name") %>%
  select(Participant, Condition, SubjGender, GenderRating, Item, Male, Female, Other)
str(d)
```

    ## 'data.frame':    8771 obs. of  8 variables:
    ##  $ Participant : Factor w/ 1253 levels "R_00dmdQaotbTidXz",..: 1001 1001 1001 1001 1001 1001 1001 23 23 23 ...
    ##  $ Condition   : Factor w/ 3 levels "first","full",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ SubjGender  : Factor w/ 5 levels "female","male",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ GenderRating: num  6.24 2.61 6.82 5.34 1.28 4.39 3.87 5.22 1.24 5.86 ...
    ##  $ Item        : Factor w/ 63 levels "Ashley Cook",..: 1 18 21 22 25 28 50 5 7 15 ...
    ##  $ Male        : int  0 1 0 0 1 1 1 1 1 0 ...
    ##  $ Female      : int  1 0 1 1 0 0 0 0 0 1 ...
    ##  $ Other       : int  0 0 0 0 0 0 0 0 0 0 ...

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

    ##             [,1]         [,2]
    ## first  0.3312051 -0.497605746
    ## full   0.3312051  0.502394254
    ## last  -0.6687949  0.002394254

# Without *Other* Responses

The first supplementary analysis tests if excluding *other* responses
(2.99% of total responses) affects the pattern of results.

``` r
o <- sum(d$Other) 
o
```

    ## [1] 262

``` r
o/length(d$Other) 
```

    ## [1] 0.02987117

Exclude *other* responses.

``` r
d.noOther <- d %>% filter(Other==0)
```

Effect of Name Condition (first name, last name, full name) and first
name Gender Rating on likelihood of a *female* response, as opposed to a
*male* response, with *other* responses excluded. Participant and Item
are again included as random intercepts, with items defined as the
unique first, last and first + last name combinations.

``` r
m.noOther <- glmer(Female ~ Condition * GenderRatingCentered + 
                   (1|Participant) + (1|Item), 
                   data=d.noOther, family=binomial)
summary(m.noOther)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered + (1 | Participant) +  
    ##     (1 | Item)
    ##    Data: d.noOther
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8737.3   8793.7  -4360.6   8721.3     8501 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4146 -0.5648 -0.2574  0.5646  4.7423 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.05031  0.2243  
    ##  Item        (Intercept) 0.36891  0.6074  
    ## Number of obs: 8509, groups:  Participant, 1232; Item, 63
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -0.16410    0.08196  -2.002   0.0453 *  
    ## Condition1                       0.13514    0.05783   2.337   0.0194 *  
    ## Condition2                       0.11301    0.06837   1.653   0.0983 .  
    ## GenderRatingCentered             0.76972    0.04650  16.554   <2e-16 ***
    ## Condition1:GenderRatingCentered  0.13700    0.03522   3.890   0.0001 ***
    ## Condition2:GenderRatingCentered -0.09189    0.04315  -2.130   0.0332 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtn1 Cndtn2 GndrRC C1:GRC
    ## Condition1   0.016                            
    ## Condition2  -0.010 -0.012                     
    ## GndrRtngCnt -0.022  0.005  0.013              
    ## Cndtn1:GnRC  0.004 -0.101  0.020  0.038       
    ## Cndtn2:GnRC  0.012  0.020 -0.085 -0.025 -0.038

Compared to the main model:

-   Intercept and Condition2:GenderRatingCentered (difference between
    Last Name and First+Full name conditions) potentially smaller
    differences

-   Condition2 now trending

# Quadratic Name Gender Rating

The second supplementary analysis tested the effect of squared name
gender rating, such that larger values meant names with stronger gender
associations (masc or fem), and smaller values meant names with weaker
gender associations.

``` r
d %<>% mutate(GenderRatingSquared=GenderRatingCentered^2)

m.quad <- glmer(Female ~ Condition*GenderRatingCentered + 
                Condition*GenderRatingSquared +
                (1|Participant) + (1|Item), 
          d, family="binomial")
summary(m.quad)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## Female ~ Condition * GenderRatingCentered + Condition * GenderRatingSquared +  
    ##     (1 | Participant) + (1 | Item)
    ##    Data: d
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9142.9   9220.8  -4560.4   9120.9     8760 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2148 -0.5741 -0.2557  0.5736  5.9912 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.2036   0.4512  
    ##  Item        (Intercept) 0.3482   0.5901  
    ## Number of obs: 8771, groups:  Participant, 1253; Item, 63
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -0.36854    0.11557  -3.189 0.001429 ** 
    ## Condition1                       0.16076    0.08016   2.006 0.044900 *  
    ## Condition2                      -0.07650    0.09273  -0.825 0.409405    
    ## GenderRatingCentered             0.77986    0.04638  16.814  < 2e-16 ***
    ## GenderRatingSquared              0.03431    0.02628   1.306 0.191653    
    ## Condition1:GenderRatingCentered  0.13226    0.03480   3.800 0.000145 ***
    ## Condition2:GenderRatingCentered -0.09191    0.04261  -2.157 0.031001 *  
    ## Condition1:GenderRatingSquared  -0.01425    0.01933  -0.737 0.461103    
    ## Condition2:GenderRatingSquared   0.05970    0.02351   2.539 0.011108 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtn1 Cndtn2 GndrRC GndrRS C1:GRC C2:GRC C1:GRS
    ## Condition1   0.002                                                 
    ## Condition2  -0.003 -0.002                                          
    ## GndrRtngCnt -0.173  0.003 -0.005                                   
    ## GndrRtngSqr -0.717 -0.005  0.005  0.212                            
    ## Cndtn1:GnRC  0.001 -0.165 -0.009  0.038 -0.003                     
    ## Cndtn2:GnRC -0.004 -0.008 -0.154 -0.027  0.029 -0.043              
    ## Cndtn1:GnRS -0.005 -0.636  0.007 -0.003  0.029  0.117  0.056       
    ## Cndtn2:GnRS  0.005  0.006 -0.620  0.034 -0.027  0.058  0.095 -0.045

-   Condition (F v F) \* Quadratic Gender Rating interaction, but n.s.
    after correction for multiple comparisons, so not making a big deal
    of it

# Participant Gender

## Setup/Data Summary

The third supplementary analysis looks at participant gender: if male
participants show a larger bias to recall the character as male than
non-male participants.

Participants entered their gender in a free-response box.

``` r
d %>% group_by(SubjGender) %>% 
  summarise(total=n_distinct(Participant)) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
SubjGender
</th>
<th style="text-align:right;">
total
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
555
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
602
</td>
</tr>
<tr>
<td style="text-align:left;">
non-binary
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
transgender female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
transgender male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
91
</td>
</tr>
</tbody>
</table>

For this analysis, we exclude participants who did not respond. Because
there are not enough participants to create 3 groups, we compare male
(male, transgender male) to non-male participants (female, non-binary,
transgender female).

``` r
d.gender <- d %>% filter(SubjGender != "N/A") %>%
            mutate(SubjGenderMale=(ifelse(
              SubjGender=="male"|SubjGender=="transgender male", 1, 0)))

d.gender %>% group_by(SubjGenderMale) %>% 
  summarise(total=n_distinct(Participant)) %>% kable()
```

<table>
<thead>
<tr>
<th style="text-align:right;">
SubjGenderMale
</th>
<th style="text-align:right;">
total
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
559
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
603
</td>
</tr>
</tbody>
</table>

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

<table>
<thead>
<tr>
<th style="text-align:left;">
Condition
</th>
<th style="text-align:left;">
ParticipantGender
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
<td style="text-align:left;">
Non-male
</td>
<td style="text-align:right;">
676
</td>
<td style="text-align:right;">
692
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
0.9159892
</td>
<td style="text-align:right;">
0.9768786
</td>
</tr>
<tr>
<td style="text-align:left;">
first
</td>
<td style="text-align:left;">
Male
</td>
<td style="text-align:right;">
619
</td>
<td style="text-align:right;">
718
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0.8456284
</td>
<td style="text-align:right;">
0.8621170
</td>
</tr>
<tr>
<td style="text-align:left;">
full
</td>
<td style="text-align:left;">
Non-male
</td>
<td style="text-align:right;">
624
</td>
<td style="text-align:right;">
584
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.9920509
</td>
<td style="text-align:right;">
1.0684932
</td>
</tr>
<tr>
<td style="text-align:left;">
full
</td>
<td style="text-align:left;">
Male
</td>
<td style="text-align:right;">
636
</td>
<td style="text-align:right;">
707
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
0.8324607
</td>
<td style="text-align:right;">
0.8995757
</td>
</tr>
<tr>
<td style="text-align:left;">
last
</td>
<td style="text-align:left;">
Non-male
</td>
<td style="text-align:right;">
568
</td>
<td style="text-align:right;">
639
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
0.8377581
</td>
<td style="text-align:right;">
0.8888889
</td>
</tr>
<tr>
<td style="text-align:left;">
last
</td>
<td style="text-align:left;">
Male
</td>
<td style="text-align:right;">
634
</td>
<td style="text-align:right;">
792
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
0.7583732
</td>
<td style="text-align:right;">
0.8005051
</td>
</tr>
</tbody>
</table>

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

## Model: Condition \* Name Gender \* Participant Gender

Effects of Name Condition (first name, full name), the first name’s
Gender Rating (centered, positive=more feminine), and Participant Gender
(non-male vs. male) on the likelihood of a *female* response as opposed
to *male* or *other* responses.

``` r
m.subjgender <- glmer(Female ~ 
      Condition * GenderRatingCentered * SubjGenderMale + 
      (1|Participant) + (1|Item), 
      data=d.gender, family=binomial)
summary(m.subjgender)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Female ~ Condition * GenderRatingCentered * SubjGenderMale +  
    ##     (1 | Participant) + (1 | Item)
    ##    Data: d.gender
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   8483.6   8581.6  -4227.8   8455.6     8120 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4061 -0.5731 -0.2626  0.5799  4.8537 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  Participant (Intercept) 0.1825   0.4272  
    ##  Item        (Intercept) 0.3673   0.6060  
    ## Number of obs: 8134, groups:  Participant, 1162; Item, 63
    ## 
    ## Fixed effects:
    ##                                                    Estimate Std. Error z value
    ## (Intercept)                                        -0.25022    0.08272  -3.025
    ## Condition1                                          0.15037    0.06379   2.357
    ## Condition2                                          0.07764    0.07464   1.040
    ## GenderRatingCentered                                0.76492    0.04662  16.409
    ## SubjGenderMaleNM_M                                 -0.19870    0.06077  -3.270
    ## Condition1:GenderRatingCentered                     0.09637    0.03620   2.662
    ## Condition2:GenderRatingCentered                    -0.09858    0.04345  -2.269
    ## Condition1:SubjGenderMaleNM_M                      -0.02392    0.12794  -0.187
    ## Condition2:SubjGenderMaleNM_M                      -0.14448    0.14940  -0.967
    ## GenderRatingCentered:SubjGenderMaleNM_M            -0.01992    0.03494  -0.570
    ## Condition1:GenderRatingCentered:SubjGenderMaleNM_M  0.04097    0.07261   0.564
    ## Condition2:GenderRatingCentered:SubjGenderMaleNM_M -0.05269    0.08694  -0.606
    ##                                                    Pr(>|z|)    
    ## (Intercept)                                         0.00249 ** 
    ## Condition1                                          0.01841 *  
    ## Condition2                                          0.29824    
    ## GenderRatingCentered                                < 2e-16 ***
    ## SubjGenderMaleNM_M                                  0.00108 ** 
    ## Condition1:GenderRatingCentered                     0.00777 ** 
    ## Condition2:GenderRatingCentered                     0.02329 *  
    ## Condition1:SubjGenderMaleNM_M                       0.85170    
    ## Condition2:SubjGenderMaleNM_M                       0.33350    
    ## GenderRatingCentered:SubjGenderMaleNM_M             0.56862    
    ## Condition1:GenderRatingCentered:SubjGenderMaleNM_M  0.57257    
    ## Condition2:GenderRatingCentered:SubjGenderMaleNM_M  0.54450    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Cndtn1 Cndtn2 GndrRC SGMNM_ Cn1:GRC Cn2:GRC C1:SGM C2:SGM
    ## Condition1   0.009                                                          
    ## Condition2  -0.007 -0.007                                                   
    ## GndrRtngCnt -0.029  0.007  0.012                                            
    ## SbjGndMNM_M -0.011  0.039 -0.042 -0.018                                     
    ## Cndtn1:GnRC  0.005 -0.122  0.017  0.025 -0.017                              
    ## Cndtn2:GnRC  0.012  0.017 -0.104 -0.024  0.000 -0.035                       
    ## Cn1:SGMNM_M  0.013 -0.063 -0.028 -0.003  0.020 -0.019   0.000               
    ## Cn2:SGMNM_M -0.014 -0.031 -0.005 -0.002 -0.011  0.000  -0.041  -0.007       
    ## GRC:SGMNM_M -0.015 -0.018  0.001 -0.012 -0.112  0.057  -0.052   0.009  0.024
    ## C1:GRC:SGMN -0.004 -0.020  0.000  0.016  0.010 -0.082  -0.036  -0.123  0.018
    ## C2:GRC:SGMN -0.001  0.000 -0.041 -0.018  0.024 -0.038   0.002   0.017 -0.103
    ##             GRC:SG C1:GRC:
    ## Condition1                
    ## Condition2                
    ## GndrRtngCnt               
    ## SbjGndMNM_M               
    ## Cndtn1:GnRC               
    ## Cndtn2:GnRC               
    ## Cn1:SGMNM_M               
    ## Cn2:SGMNM_M               
    ## GRC:SGMNM_M               
    ## C1:GRC:SGMN  0.057        
    ## C2:GRC:SGMN -0.050 -0.034

-   Male participants less likely to recall character as female than
    non-male participants overall.

-   No other interactions with participant gender significant.
