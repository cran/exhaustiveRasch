## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----Define combination rules-------------------------------------------------
rules_object <- list()

rules_object[[1]] <- list("min", 1, 1:6)

rules_object[[2]] <- list("max", 3, 1:6)

rules_object[[3]] <- list("forbidden", c(8,10)) 

## ----table 1, echo=FALSE------------------------------------------------------
tab_tests <- c("all_rawscores",
               "no_test",
               "test_DIFtree",
               "test_itemfit",
               "test_LR",
               "test_mloef",
               "test_PSI",
               "test_personsItems",
               "test_respca",
               "test_waldtest",
               "threshold_order"
)

tab_desc <- c("checks, if all possible rawscores (sums of item scores) are empirically respresented in the data",
              "No test is performed, but the returned passed_exRa object contains fit models for the provided item combinations",
              "tests differential item functioning (DIF) related to the specified external variables by using raschtrees; checks, if no split is present is the resulting tree.",
              "checks, if the fit- indices (infit, outfit) are within the specified range",
              "performs Anderson’s likelihood ratio test with the specified split criterion",
              "performs the Martin-Löf test with the specified split criterion",
              "checks if the person separation index (PSI) - also known as person reliabilty exceeds the given value (between 0 and 1).",
              "checks, if there are item thresholds in the extreme low and high range of the latent dimension and/or checks, if the amount of item thresholds between neighboring person parameters is above the specified percentage",
              "performs a principal components analysis on the rasch residuals; checks if the eigenvalue of the highest loading contrast is below the specified value",
              "performs a Waldtest with the specified split criterion; checks, if all items have p-values below the specified alpha (or local alpha, if a Bonferroni correction is used)",
              "checks, if all threshold locations are ordered (not applicable for dichotomous rasch models)"
)

tab_param <- c("no arguments",
               "no arguments",
               "no arguments (but DIFvars must be provided)",
               "MSQ in- and outfits between 0.7 and 1.3 and no significant p-values (alpha=0.1, no Bonferroni correction)",
               "median rawscore as split criterion, no significant p-values (alpha=0.1, no Bonferroni correction)",
               "median rawscore as split criterion, no significant p-values (alpha=0.1)",
               "values above 0.8",
               "checks for thresholds in the extreme ranges, but not for the amount of thresholds between person parameters",
               "maximum eigenvalue of 1.5",
               "median rawscore as split criterion, no significant p-values (alpha=0.1, no Bonferroni correction)",
               "no arguments"
)

tab <- as.data.frame(cbind(tab_tests, tab_desc, tab_param))
colnames(tab) <- c("test", "description", "default setting")
knitr::kable(tab) 

## ----table 2, echo=FALSE------------------------------------------------------
tab_value <- c("> 2.0",
               "1.5 - 2.0",
               "0.5 - 1.5",
               "< 0.5"
)

tab_implic <- c("Distorts or degrades the measurement system. May be caused by only one or two observations.",
                "Unproductive for construction of measurement, but not degrading.",
                "Productive for measurement.",
                "Less productive for measurement, but not degrading. May produce misleadingly high reliability and separation coefficients."
)
tab <- as.data.frame(cbind(tab_value, tab_implic))
colnames(tab) <- c("MSQ", "implication for measurement")
knitr::kable(tab) 

## ----table 3, echo=FALSE------------------------------------------------------
tab_value <- c("≥ 3",
               "2.0 - 2.9",
               "-1.9- 1.9",
               "≤ -2"
)

tab_implic <- c("Data very unexpected if they fit the model (perfectly), so they probably do not. But, with large sample size, substantive misfit may be small.",
                "Data noticeably unpredictable.",
                "Data have reasonable predictability.",
                "Data are too predictable. Other 'dimensions' may be constraining the response patterns."
)
tab <- as.data.frame(cbind(tab_value, tab_implic))
colnames(tab) <- c("standardized value", "implication for measurement")
knitr::kable(tab) 

## ----table 4, echo=FALSE------------------------------------------------------
tab_value <- c("MCQ (high stakes)",
               "MCQ (run of the mill)",
               "rating scale (survey)",
               "clinical observation",
               "judged (agreement encouraged)"
)

tab_implic <- c("0.8 - 1.2",
                "0.7 - 1.3",
                "0.6 - 1.4",
                "0.5 - 1.7",
                "0.4 - 1.2"
)
tab <- as.data.frame(cbind(tab_value, tab_implic))
colnames(tab) <- c("type of test", "range")
knitr::kable(tab) 

## ----Define ombination rules (ADL example)------------------------------------
library(exhaustiveRasch)
data(ADL)
rules_object <- list()
rules_object[[1]] <- list("max", 2, 1:6) #mobility
rules_object[[2]] <- list("min", 1, 1:6) #mobility
rules_object[[3]] <- list("max", 2, 7:11) # personal hygiene/dressing
rules_object[[4]] <- list("min", 1, 7:11) # personal hygiene/dressing
rules_object[[5]] <- list("min", 1, 12:13) # eating/drinking
rules_object[[6]] <- list("min", 1, 14:15) # toileting
rules_object[[7]] <- list("forbidden", c(1,2)) # transfer from bed/ stand up from chair

## ----Apply combination rules (ADL example)------------------------------------
final_combos <- apply_combo_rules(combo_length= 4:10, full=1:15, rules= rules_object)

## ----Run tests (ADL example)--------------------------------------------------
passed_ADL <- exhaustive_tests(dset=ADL, combos=final_combos, modelType= "RM",
                               upperMSQ=1.5, lowerMSQ=0.5, use.pval=F, bonf=T,
                               na.rm=T, tests= c("test_mloef", "test_LR", "test_itemfit"),
                               estimation_param = estimation_control(
                                 est="psychotools"))

## ----Run additional test (ADL example)----------------------------------------
passed_ADL2 <- exhaustive_tests(
  dset=ADL, combos=passed_ADL, DIFvars=ADL[16:17], tests=c("test_DIFtree"),
                               estimation_param = estimation_control(
                                 est="psychotools"))

## ----Remove subsets (ADL example)---------------------------------------------
passed_rem <- remove_subsets(passed_ADL2, keep_longest=F)

