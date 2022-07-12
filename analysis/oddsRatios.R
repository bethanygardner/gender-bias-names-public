library(tidyverse)
library(magrittr)
library(htmlTable)

load("analysis/all-analysis.RData")

expCol <- rep("Experiment 1", 8) %>%
   append(rep("Experiment 2", 8))%>%
   append(rep("Experiment 3", 8))%>%
   append(rep("Experiment 4", 8))

respCol <- rep("Produce <i>He</i>", 8) %>%
    append(rep("Recall as Male",   8)) %>%
    append(rep("Produce <i>He</i>", 8)) %>%
    append(rep("Recall as Male",   8))

dataCol <- rep(c(
  "All Data", "All Data", "All Data","All Data",
  "<i>Other</i> Responses <br>Excluded", 
  "<i>Other</i> Responses <br>Excluded", 
  "<i>Other</i> Responses <br>Excluded", 
  "<i>Other</i> Responses <br>Excluded"), 4) %>%
  as.factor() %>% fct_relevel("All Data")

condCol <- rep(c(
  "All Conditions", "Last vs First + <br>Full Names",
  "Last Name", "First & Full Names"), 8)

resultsCol <- c(
  #Exp1: All
  exp1_OR_all_I,     exp1_OR_all_LFF, 
  exp1_OR_all_L,     exp1_OR_all_FF,
  #Exp1: No Other
  exp1_OR_noOther_I, exp1_OR_noOther_LFF,
  exp1_OR_noOther_L, exp1_OR_noOther_FF,
  #Exp2: All
  exp2_OR_all_I,     exp2_OR_all_LFF, 
  exp2_OR_all_L,     exp2_OR_all_FF,
  #Exp2: No Other
  exp2_OR_noOther_I, exp2_OR_noOther_LFF,
  exp2_OR_noOther_L, exp2_OR_noOther_FF,
  #Exp3: All
  exp3_OR_all_I,     exp3_OR_all_LFF, 
  exp3_OR_all_L,     exp3_OR_all_FF,
  #Exp3: No Other
  exp3_OR_noOther_I, exp3_OR_noOther_LFF,
  exp3_OR_noOther_L, exp3_OR_noOther_FF,
  #Exp4: All
  exp4_OR_all_I,     exp4_OR_all_LFF, 
  exp4_OR_all_L,     exp4_OR_all_FF,
  #Exp4: No Other
  exp4_OR_noOther_I, exp4_OR_noOther_LFF,
  exp4_OR_noOther_L, exp4_OR_noOther_FF) %>%
  
  format(nsmall=2, justify=c("left")) %>%
  formatC(flag="0") 
  
resultsFormat <- c(
  #Exp1: All
  str_c(resultsCol[1],"***"),
  str_c(resultsCol[2],"***"),
  str_c(resultsCol[3],"***"),
  str_c(resultsCol[4],"\U0020","\U0020","\U0020","\U0020"),
  #Exp1: No Other
  str_c(resultsCol[5],"***"),
  str_c(resultsCol[6],"***"),
  str_c(resultsCol[7],"***"),
  str_c(resultsCol[8],"\U0020","\U0020","\U0020","\U0020"),
  #Exp2: All
  str_c(resultsCol[9],"***"),
  str_c(resultsCol[10],"***"),
  str_c(resultsCol[11],"***"),
  str_c(resultsCol[12],"\U0020","\U0020","\U0020","\U0020"),
  #Exp2: No Other
  str_c(resultsCol[13],"***"),
  str_c(resultsCol[14],"***"),
  str_c(resultsCol[15],"***"),
  str_c(resultsCol[16],"\U0020","\U0020","\U0020","\U0020"),
  #Exp3: All
  str_c(resultsCol[17],"***"),
  str_c(resultsCol[18],"\U02D9","\U0020","\U0020","\U0020"), 
  str_c(resultsCol[19],"***"),
  str_c(resultsCol[20],"***"),
  #Exp3: No Other
  str_c(resultsCol[21],"***"),
  str_c(resultsCol[22],"**","\U0020"),
  str_c(resultsCol[23],"\U0020","\U0020","\U0020","\U0020"),
  str_c(resultsCol[24],"\U0020","\U0020","\U0020","\U0020"),
  #Exp4: All
  str_c(resultsCol[25],"**","\U0020","\U0020"),
  str_c(resultsCol[26],"*","\U0020","\U0020","\U0020"), 
  str_c(resultsCol[27],"\U02D9","\U0020","\U0020","\U0020"), 
  str_c(resultsCol[28],"\U0020","\U0020","\U0020","\U0020"),
  #Exp4: No Other
  str_c(resultsCol[29],"*","\U0020","\U0020","\U0020"),
  str_c(resultsCol[30],"*","\U0020","\U0020","\U0020"),
  str_c(resultsCol[31],"\U0020","\U0020","\U0020","\U0020"),
  str_c(resultsCol[32],"\U0020","\U0020","\U0020","\U0020"))
  

oddsRatios <- data.frame(
  "Experiment"=expCol,
  "Measure"   =respCol,
  "Data"      =dataCol,
  "Condition" =condCol,
  "OddsRatio" =resultsFormat) %>%
  arrange(Measure, Data, Condition)

oddsRatios$Condition %<>% as.factor() %>%
  fct_relevel("All Conditions", 
             "Last Name", 
             "First & Full Names",
             "Last vs First + <br>Full Names")


oddsTable_hor <- oddsRatios %>%
  addHtmlTableStyle(
    css.table   ="font-family: Arial", 
    css.header  ="font-family: Arial; font-weight: normal",
    css.tspanner="font-family: Arial",
    css.rgroup  ="font-family: Arial; font-weight: bold",
    css.cgroup  ="font-family: Arial; font-weight: bold",
    css.cell    ="white-space: pre") %>% 
  tidyHtmlTable(
    value  =OddsRatio,
    header =Data,
    cgroup =Condition, 
    rnames =Experiment,
    rgroup =Measure,
    caption="<b>Odds Ratios Across Experiments<b>")
oddsTable_hor
    
oddsRatios$Data %<>% recode(
  "<i>Other</i> Responses <br>Excluded"=
  "<i>Other</i> Excluded") 
oddsRatios$Condition %<>% recode(
  "Last vs First + <br>Full Names"=
  "Last vs First + Full Names")

oddsTable_ver <- oddsRatios %>%
  arrange(Condition, Data) %>%
  addHtmlTableStyle(
    css.table   ="font-family: Arial", 
    css.header  ="font-family: Arial; font-weight: bold",
    css.tspanner="font-family: Arial",
    css.rgroup  ="font-family: Arial; font-weight: bold",
    css.cgroup  ="font-family: Arial; font-weight: bold",
    css.cell    ="white-space: pre") %>% 
  tidyHtmlTable(
    value  =OddsRatio,
    header =Experiment,
    cgroup =Measure,
    rnames =Data,
    rgroup =Condition, 
    caption="<b>Odds Ratios Across Experiments<b>")
oddsTable_ver