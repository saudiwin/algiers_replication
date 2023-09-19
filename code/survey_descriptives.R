# Script to reproduce survey sample statistics for Kubinec and Milner (2021)

require(readr)
require(dplyr)
require(ggplot2)
require(forcats)
require(tidyr)
require(lubridate)
require(survey)
require(stringr)
require(ggthemes)
require(kableExtra)

survey <- read_csv('data/survey_small.csv') %>% 
  filter(treat_account!="Default")

survey <- mutate(survey,bou_resign=as.numeric(grepl(x=goals,pattern="Bouteflika's immediate resignation")),
                 enlisted=as.numeric(rank %in% c("Private","Master Corporal")),
                 army=as.numeric(grepl(x=branch,pattern="People's National Army"))) %>% 
  filter(treat_account!="Default",Finished,
         !(age=="15-19" &  over18!="Yes"))

# post-stratification table

post_strat <- read_csv("data/alg_census_edited.csv") %>% 
  mutate(District=recode(District,`Ain Temouchent`="Aïn Témouchent",
                         Bechar="Béchar",
                         Bejaia="Béjaia",
                         `Bordj Bou Arreridj`="Bordj Bou Arréridj",
                         `El Bayadh`="El-Bayadh",
                         `El Oued`="El-Oued",
                         `El Tarf`="El-Taref",
                         `M'Sila`="M’Sila",
                         Medea="Médéa",
                         Naama="Naâma",
                         Boumerdes="Boumerdès",
                         `Oum El Bouaghi`="Oum el-Bouaghi",
                         Setif="Sétif",
                         `Sidi Bel Abbes`="Sidi Bel Abbès",
                         `Souk Ahras`="Souk-Ahras",
                         Tebessa="Tébessa",
                         `Tizi Ouzou`="Tizi-Ouzou")) %>% 
  gather(key="sex",value="pop_num",Men,Women) %>% 
  mutate(sex=recode(sex,Men="Male",Women="Female"),
         pop_num=if_else(Age=="15-19 ans",pop_num*(2/5),pop_num),
         Age=str_remove(Age," ?ans"),
         Age=str_replace(Age,".*70.*|.*75.*|.*80.*|.*85.*|.*65.*","65+"),
         Age=recode(Age,
                    `35-39`="35-44",
                    `40-44`="35-44",
                    `45-49`="45-54",
                    `50-54`="45-54",
                    `55-59`="55-64",
                    `60-64`="55-64")) %>% 
  filter(!(Age %in% c("0-4 Ans",
                      "10-14"))) %>% 
  group_by(Age,District,sex) %>% 
  summarize(pop_num=sum(pop_num)) %>% 
  mutate(pop_num=if_else(Age=="15-19",pop_num/5,pop_num)) %>% 
  ungroup %>% 
  mutate(prop_total=pop_num/sum(pop_num),
         pop_total=sum(pop_num)) %>% 
  rename(age="Age",
         gov="District")


out_tibble <- bind_rows(select(count(survey,sex),Variable="sex",n),
                        select(count(survey,age),Variable="age",n),
                        select(count(survey,urban),Variable="urban",n),
                        select(count(survey,status),Variable="status",n),.id="num") %>% 
  filter(!is.na(Variable)) %>% 
  group_by(num) %>% 
  mutate(`Sample Proportion`=round((n/sum(n))*100,1)) %>% 
  ungroup %>% 
  select(Variable,`Sample Proportion`)

# get some sample statistics from the census

census_stat <- bind_rows(select(summarize(group_by(post_strat,age),
                                  `Census Proportion`=round(sum(prop_total)*100,1)),
                               Variable="age",`Census Proportion`),
                        select(summarize(group_by(post_strat,sex),
                                         `Census Proportion`=round(sum(prop_total)*100,1)),
                               Variable="sex",`Census Proportion`))

# need to add data manually for labor force surveys

# calculate proportion rural/urban

tot_num_urbain <- sum(c(7320,936))
tot_num_rural <- sum(c(3525,336))


census_stat <- bind_rows(census_stat,
                         tibble(Variable=c("Rural","Suburban","Urban",
                                           "Employed","Housewife","Retired",
                                           "Student","Unemployed"),
                                `Census Proportion`=c(round((tot_num_rural/(tot_num_urbain + tot_num_rural))*100,1),
                                                      NA,round((tot_num_urbain/(tot_num_urbain + tot_num_rural))*100,1),
                                                      37.4,NA,NA,10,10.5)))

# left join

out_tibble <- left_join(out_tibble,census_stat)

out_tibble %>% 
  mutate(Variable=recode(Variable,
                    `15-19`="18-19")) %>% 
  kable(caption="Comparison of Algerian Sample Demographics to Census Proportions",
        format="latex",booktabs=T) %>% 
  kableExtra::kable_styling(latex_options = c("hold_position","striped")) %>% 
  save_kable(file="tables/descriptives.tex")


# Balance Table -----------------------------------------------------------

# create a balance table for covariates

library(cobalt)

bal_tab <- bal.tab(treat_account ~ age + edu + inc,disp=c("means","sds"),
                   data=survey)

# iterate over

over_vars <- c("sex", "age", "edu", "inc", "status", "urban", "prot", "partic")

all_compare <- lapply(over_vars, function(c) {
  
  d1 <- group_by(survey,treat_account, !!sym(c)) %>% 
    summarize(n=n()) %>% 
    group_by(treat_account) %>% 
    mutate(tot_treat=sum(n)) %>% 
    group_by(!!sym(c)) %>% 
    mutate(prop=n/tot_treat,
           tot=sum(n)) %>% 
    group_by(!!sym(c)) %>% 
    mutate(p_val=prop.test(prop, tot_treat)$p.value)
  
  names(d1)[2] <- "Value"
  
  d1$Variable <- c
  
  d1
  
}) %>% bind_rows %>% 
  filter(!is.na(Value))

library(knitr)
library(kableExtra)

all_compare %>% 
  select(Treatment="treat_account",
         Covariate="Variable",
         Level="Value",
         Count="n",
         Total="tot_treat",
         Proportion="prop",
         p="p_val") %>% 
  ungroup %>% 
  mutate(Treatment=dplyr::recode(Treatment,
                          control="Control",
                          treated_gas="Fuel Subsidy",
                          treated_within="Govt. Revenue",
                          treated_tax="VAT")) %>% 
  arrange(Covariate, Level, Treatment) %>% 
  kable(digits=3,caption="Balance Statistics for Treatment Groups",
        align=c('lllcccc'),format="latex",
        longtable = T, 
        booktabs  = T) %>% 
  kable_styling(latex_options=c("hold_position","striped"),
                font_size=9,
                repeat_header_continued = T) %>% 
    add_footnote(label=c("P-values calculated with Chi-square test with a null hypothesis of equal proportions."),
                 notation = "number") %>% 
  save_kable(file="tables/balance_table.tex")




