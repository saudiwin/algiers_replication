# produce tables of plots for appendix

require(dplyr)
require(modelsummary)
require(kableExtra)

out_mod_action <- readRDS("data/out_mod_action.rds")
out_action_int <- readRDS("data/out_action_int.rds")
out_mod_belief <- readRDS("data/out_mod_belief.rds")
out_belief_int <- readRDS("data/out_belief_int.rds")
out_action_int_prot <- readRDS("data/out_action_int_prot.rds")
out_belief_int_prot <- readRDS("data/out_belief_int_prot.rds")
out_action_int_corr <- readRDS("data/out_action_int_corr.rds")
out_belief_int_corr <- readRDS("data/out_belief_int_corr.rds")

names(out_mod_action) <- c("Protest",
                           "Complain",
                           "Funds Overseas",
                           "Switch Currencies")

modelsummary(out_mod_action, output="tables/action_ate.tex",
             statistic="conf.int",metrics=c("R2","LOOIC"),
             title="Model Coefficients for ATEs with Intent to Engage in Actions DVs (Figure 3)",
             label="ate_action",
             notes="Coefficients are posterior averages and intervals are the 5 percent to 95 percent posterior quantiles.",
             coef_map=c("treat_accounttreated_gas"="Gas/Fuel",
                        "treat_accounttreated_tax"="Tax",
                        "treat_accounttreated_within"="Oil Revenues"))

names(out_mod_belief) <- c("Employment",
                           "Health Care",
                           "Corruption",
                           "Reforming",
                           "Stability")

modelsummary(out_mod_belief, output="tables/belief_ate.tex",
             statistic="conf.int",metrics=c("R2","LOOIC"),
             title="Model Coefficients for ATEs with Attitudinal DVs (Figure 2)",
             notes="Coefficients are posterior averages and intervals are the 5 percent to 95 percent posterior quantiles.",
             label="ate_belief",
             coef_map=c("treat_accounttreated_gas"="Gas/Fuel",
                        "treat_accounttreated_tax"="Tax",
                        "treat_accounttreated_within"="Oil Revenues"))

# ses interaction

names(out_action_int) <- c("Protest",
                           "Complain",
                           "Funds Overseas",
                           "Switch Currencies")

modelsummary(out_action_int, output="tables/action_int_ate.tex",
             statistic="conf.int",metrics=c("R2","LOOIC"),
             title="Model Coefficients for Conditional ATEs of SES with Action DVs (Figure 5)",
             label="ate_int_action",
             notes="Coefficients are posterior averages and intervals are the 5 percent to 95 percent posterior quantiles.",
             coef_map=c("treat_accounttreated_gas"="Gas/Fuel",
                        "treat_accounttreated_tax"="Tax",
                        "treat_accounttreated_within"="Oil Revenues",
                        "treat_accounttreated_gas:median"="Gas/Fuel X SES",
                        "treat_accounttreated_tax:median"="Tax X SES",
                        "treat_accounttreated_within:median"="Oil Revenues X SES"))

names(out_belief_int) <- c("Employment",
                           "Health Care",
                           "Corruption",
                           "Reforming",
                           "Stability")

modelsummary(out_belief_int, output="tables/belief_int_ate.tex",
             statistic="conf.int",metrics=c("R2","LOOIC"),
             title="Model Coefficients for Conditional ATEs of SES with Attitudinal DVs (Figure 4)",
             label="ate_int_action",
             notes="Coefficients are posterior averages and intervals are the 5 percent to 95 percent posterior quantiles.",
             coef_map=c("treat_accounttreated_gas"="Gas/Fuel",
                        "treat_accounttreated_tax"="Tax",
                        "treat_accounttreated_within"="Oil Revenues",
                        "treat_accounttreated_gas:median"="Gas/Fuel X SES",
                        "treat_accounttreated_tax:median"="Tax X SES",
                        "treat_accounttreated_within:median"="Oil Revenues X SES"))


# protest interaction

names(out_action_int_prot) <- c("Protest",
                           "Complain",
                           "Funds Overseas",
                           "Switch Currencies")

modelsummary(out_action_int_prot, output="latex",
             statistic="conf.int",metrics=c("R2","LOOIC"),
             title="Model Coefficients for Conditional ATEs of Protest Attitudes with Action DVs (Figure 7)",
             label="ate_int_prot_action",
             notes="Coefficients are posterior averages and intervals are the 5 percent to 95 percent posterior quantiles.",
             coef_map=c("treat_accounttreated_gas"="Gas/Fuel",
                        "treat_accounttreated_tax"="Tax",
                        "treat_accounttreated_within"="Oil Revenues",
                        "protStrongly oppose"="Strongly Oppose",
                        "protSomewhat oppose"="Somewhat Oppose",
                        "protSomewhat support"="Somewhat Support",
                        "protStrongly support"="Strongly Support",
                        "treat_accounttreated_gas:protStrongly oppose"="Gas/Fuel X Strongly  Oppose",
                        "treat_accounttreated_tax:protStrongly oppose"="Tax X Strongly  Oppose",
                        "treat_accounttreated_within:protStrongly oppose"="Oil Revenues X Strongly Oppose",
                       "treat_accounttreated_gas:protSomewhat oppose"="Gas/Fuel X Somewhat Oppose",
                        "treat_accounttreated_tax:protSomewhat oppose"="Tax X Somewhat Oppose",
                        "treat_accounttreated_within:protSomewhat oppose"="Oil Revenues X Somewhat Oppose",
                        "treat_accounttreated_gas:protSomewhat support"="Gas/Fuel X Somewhat Support",
                        "treat_accounttreated_tax:protSomewhat support"="Tax X Somewhat Support",
                        "treat_accounttreated_within:protSomewhat support"="Oil Revenues X Somewhat Support",
                        "treat_accounttreated_gas:protStrongly support"="Gas/Fuel X Strongly Support",
                        "treat_accounttreated_tax:protStrongly support"="Tax X Strongly Support",
                        "treat_accounttreated_within:protStrongly support"="Oil Revenues X Strongly Support")) %>% 
  kable_styling(font_size=8) %>% 
  save_kable("tables/action_int_prot_ate.tex")

names(out_belief_int_prot) <- c("Employment",
                           "Health Care",
                           "Corruption",
                           "Reforming",
                           "Stability")

modelsummary(out_belief_int_prot, output="latex",
             statistic="conf.int",metrics=c("R2","LOOIC"),
             title="Model Coefficients for Conditional ATEs of Protest Attitudes with Attitudinal DVs (Figure 6)",
             label="ate_int_prot_belief",
             notes="Coefficients are posterior averages and intervals are the 5 percent to 95 percent posterior quantiles.",
             coef_map=c("treat_accounttreated_gas"="Gas/Fuel",
                                 "treat_accounttreated_tax"="Tax",
                                 "treat_accounttreated_within"="Oil Revenues",
                                 "protStrongly oppose"="Strongly Oppose",
                                 "protSomewhat oppose"="Somewhat Oppose",
                                 "protSomewhat support"="Somewhat Support",
                                 "protStrongly support"="Strongly Support",
                                 "treat_accounttreated_gas:protStrongly oppose"="Gas/Fuel X Strongly  Oppose",
                                 "treat_accounttreated_tax:protStrongly oppose"="Tax X Strongly  Oppose",
                                 "treat_accounttreated_within:protStrongly oppose"="Oil Revenues X Strongly Oppose",
                                 "treat_accounttreated_gas:protSomewhat oppose"="Gas/Fuel X Somewhat Oppose",
                                 "treat_accounttreated_tax:protSomewhat oppose"="Tax X Somewhat Oppose",
                                 "treat_accounttreated_within:protSomewhat oppose"="Oil Revenues X Somewhat Oppose",
                                 "treat_accounttreated_gas:protSomewhat support"="Gas/Fuel X Somewhat Support",
                                 "treat_accounttreated_tax:protSomewhat support"="Tax X Somewhat Support",
                                 "treat_accounttreated_within:protSomewhat support"="Oil Revenues X Somewhat Support",
                                 "treat_accounttreated_gas:protStrongly support"="Gas/Fuel X Strongly Support",
                                 "treat_accounttreated_tax:protStrongly support"="Tax X Strongly Support",
                                 "treat_accounttreated_within:protStrongly support"="Oil Revenues X Strongly Support")) %>% 
  kable_styling(font_size=8) %>% 
  save_kable("tables/belief_int_prot_ate.tex")


# corruption interaction

names(out_action_int_corr) <- c("Protest",
                                "Complain",
                                "Funds Overseas",
                                "Switch Currencies")

modelsummary(out_action_int_corr, output="latex",
             statistic="conf.int",metrics=c("R2","LOOIC"),
             title="Model Coefficients for Conditional ATEs of Corruption Attitudes with Action DVs (Figure 9)",
             label="ate_int_corr_action",
             notes="Coefficients are posterior averages and intervals are the 5 percent to 95 percent posterior quantiles.",
             coef_map=c("treat_accounttreated_gas"="Gas/Fuel",
                        "treat_accounttreated_tax"="Tax",
                        "treat_accounttreated_within"="Oil Revenues",
                        "corrLow"="Low Corruption",
                        "corrModerate"="Moderate Corruption",
                        "corrVery High"="Very High Corruption",
                        "corrVery Low"="Very Low Corruption",
                        "treat_accounttreated_gas:corrVery Low"="Gas/Fuel X Very Low Corruption",
                        "treat_accounttreated_tax:corrVery Low"="Tax X Very Low Corruption",
                        "treat_accounttreated_within:corrVery Low"="Oil Revenues X Very Low Corruption",
                        "treat_accounttreated_gas:corrLow"="Gas/Fuel X Low Corruption",
                        "treat_accounttreated_tax:corrLow"="Tax X Low Corruption",
                        "treat_accounttreated_within:corrLow"="Oil Revenues X Low Corruption",
                        "treat_accounttreated_gas:corrModerate"="Gas/Fuel X Moderate Corruption",
                        "treat_accounttreated_tax:corrModerate"="Tax X Moderate Corruption",
                        "treat_accounttreated_within:corrModerate"="Oil Revenues X Moderate Corruption",
                        "treat_accounttreated_gas:corrVery High"="Gas/Fuel X Very High Corruption",
                        "treat_accounttreated_tax:corrVery High"="Tax X Very High Corruption",
                        "treat_accounttreated_within:corrVery High"="Oil Revenues X Very High Corruption")) %>% 
  kable_styling(font_size=8) %>% 
  save_kable("tables/action_int_corr_ate.tex")

names(out_belief_int_corr) <- c("Employment",
                                "Health Care",
                                "Corruption",
                                "Reforming",
                                "Stability")

modelsummary(out_belief_int_corr, output="latex",
             statistic="conf.int",metrics=c("R2","LOOIC"),
             title="Model Coefficients for Conditional ATEs of Corruption Attitudes with Attitudinal DVs (Figure 8)",
             label="ate_int_corr_belief",
             notes="Coefficients are posterior averages and intervals are the 5 percent to 95 percent posterior quantiles.",
             coef_map=c("treat_accounttreated_gas"="Gas/Fuel",
                        "treat_accounttreated_tax"="Tax",
                        "treat_accounttreated_within"="Oil Revenues",
                        "corrLow"="Low Corruption",
                        "corrModerate"="Moderate Corruption",
                        "corrVery High"="Very High Corruption",
                        "corrVery Low"="Very Low Corruption",
                        "treat_accounttreated_gas:corrVery Low"="Gas/Fuel X Very Low Corruption",
                        "treat_accounttreated_tax:corrVery Low"="Tax X Very Low Corruption",
                        "treat_accounttreated_within:corrVery Low"="Oil Revenues X Very Low Corruption",
                        "treat_accounttreated_gas:corrLow"="Gas/Fuel X Low Corruption",
                        "treat_accounttreated_tax:corrLow"="Tax X Low Corruption",
                        "treat_accounttreated_within:corrLow"="Oil Revenues X Low Corruption",
                        "treat_accounttreated_gas:corrModerate"="Gas/Fuel X Moderate Corruption",
                        "treat_accounttreated_tax:corrModerate"="Tax X Moderate Corruption",
                        "treat_accounttreated_within:corrModerate"="Oil Revenues X Moderate Corruption",
                        "treat_accounttreated_gas:corrVery High"="Gas/Fuel X Very High Corruption",
                        "treat_accounttreated_tax:corrVery High"="Tax X Very High Corruption",
                        "treat_accounttreated_within:corrVery High"="Oil Revenues X Very High Corruption")) %>% 
  kable_styling(font_size=8) %>% 
  save_kable("tables/belief_int_corr_ate.tex")
