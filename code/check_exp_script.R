
require(readr)
require(dplyr)
require(rstanarm)
require(ggplot2)
require(forcats)
require(tidyr)
require(lubridate)
require(idealstan)
require(survey)
require(stringr)
require(ggthemes)
require(kableExtra)

# to install idealstan, use the following command:

# remotes::install_github("saudiwin/idealstan")

# Setup -------------------------------------------------------------------


options(na.action = "na.exclude")

# run_model will run all Bayesian model fits reported in the paper
# to avoid doing this, be sure to download the model objects from
# https://drive.google.com/drive/folders/1r_KBEBzUDtTT5APPC4RKYjNQks8BimFY?usp=drive_link
# and put in the data/ folder
# run_index will run the latent variable model used to produce the SES index

run_model <- T
run_index <- F


# Load Data ---------------------------------------------------------------


# non-imputed survey data
survey <- read_csv('data/survey_small.csv')

survey <- mutate(survey,bou_resign=as.numeric(grepl(x=goals,pattern="Bouteflika's immediate resignation")),
                 enlisted=as.numeric(rank %in% c("Private","Master Corporal")),
                 army=as.numeric(grepl(x=branch,pattern="People's National Army")),
                 car=grepl(x=own,pattern="Car"),
                 treat_account2=case_when(car & treat_account=="treated_gas"~"treated_car",
                                          !car & treat_account=="treated_gas"~"treated_taxi",
                                          TRUE~treat_account))

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

# save full post-strat table

saveRDS(post_strat,"data/algeria_strat.rds")

survey_strat <- left_join(survey,post_strat,by=c("gov","age","sex")) %>% 
  ungroup %>% 
  filter(!is.na(age),!is.na(gov),!is.na(sex),!is.na(pop_total)) %>% 
  mutate(fpc=n()/pop_total)

ps_data <- select(survey_strat,age,gov,sex,pop_num,pop_total) %>% 
  distinct %>% 
  mutate(rownum=1:n(),
         prop=pop_num/sum(pop_num))

# load survey data specific to this analysis 

pref_data <- survey %>% filter(treat_account %in% c("treated_gas",
                                                    "treated_tax",
                                                    "control")) %>% 
  gather(key=out_action,value=response,matches("direct")) %>% 
  mutate(out_action=factor(out_action,levels=c("direct_1",
                                               "direct_2",
                                               "direct_3"),
                           labels=c("I support democracy even\nif it means opposing the regime.",
                                    "I want Bouteflika to step down,\nbut I don't care if our political system stays\nas is or becomes a democracy.",
                                    "I want Bouteflika to step down\nbut I don't want our country\nto become a democracy.")),
         response=as.numeric(factor(response,levels=c("No","Yes")))-1) %>% 
  filter(!is.na(out_action))

#expanded version with separate categories for taxi users and car owners

pref_data_taxi <- survey %>% filter(treat_account2 %in% c("treated_car",
                                                          "treated_taxi",
                                                         "treated_tax",
                                                         "control")) %>% 
  gather(key=out_action,value=response,matches("direct")) %>% 
  mutate(out_action=factor(out_action,levels=c("direct_1",
                                               "direct_2",
                                               "direct_3"),
                           labels=c("I support democracy even\nif it means opposing the regime.",
                                    "I want Bouteflika to step down,\nbut I don't care if our political system stays\nas is or becomes a democracy.",
                                    "I want Bouteflika to step down\nbut I don't want our country\nto become a democracy.")),
         response=as.numeric(factor(response,levels=c("No","Yes")))-1) %>% 
  filter(!is.na(out_action))


# Idealstan index ---------------------------------------------------------

# only try to fit this if you have a working copy of idealstan installed (with cmdstanr)

if(run_index) {
  ses_index <- select(survey,ResponseId,gov,
                      status,urban,edu,inc,sex,
                      age,econ,own,gas,taxi,vat) %>% 
    mutate(small_biz=grepl(x=own,pattern="Small business"),
           mid_biz=grepl(x=own,pattern="Medium-sized"),
           large_biz=grepl(x=own,pattern="Large business"),
           bank_algeria=grepl(x=own,
                              pattern="Bank account at a bank in Algeria"),
           bank_foreign=grepl(x=own,
                              pattern="Bank account at a bank in a foreign country"),
           house=grepl(x=own,
                       pattern="House"),
           car=grepl(x=own,
                     pattern="Car"),
           farm=grepl(x=own,
                      pattern="Farm"),
           econ=factor(econ,levels=c("Very dissatisfied",
                                     "Somewhat dissatisfied",
                                     "Neutral",
                                     "Somewhat satisfied",
                                     "Very satisfied")),
           inc=factor(inc,levels=c("Less than DA 20,000",
                                   "DA 20,000-40,000",
                                   "DA 40,000-60,000",
                                   "DA 60,000-80,000",
                                   "DA 80,000-100,000",
                                   "DA 100,000-200,000",
                                   "DA 200,000-500,000",
                                   "DA 500,000-1M",
                                   "DA 1M-5M","More than DA 5M")),
           edu=factor(edu,levels=c("No Education",
                                   "Primary School",
                                   "Middle School",
                                   "High school graduate",
                                   "Bachelor's (BA, BS) / Engineering degree (3 to 5 years)",
                                   "Master's (MA, MS, MBA)",
                                   "Doctorate (PhD)")),
           age=factor(age,levels=unique(age)),
           sex=as.numeric(sex=="Female"),
           gov1s=1,
           status1s=1,
           urban1s=1) %>% 
    mutate_at(vars(small_biz:farm),
              ~ ifelse(is.na(own), NA, as.numeric(.))) %>%
    pivot_wider(names_from="gov",values_from="gov1s",values_fill=0) %>% 
    mutate_at(vars(Biskra:Tissemsilt), ~ ifelse(`NA`,NA,.)) %>% 
    select(-`NA`) %>% 
    pivot_wider(names_from="status",values_from="status1s",values_fill=0,
                names_repair="unique") %>% 
    mutate_at(vars(Student:Housewife), ~ ifelse(`NA`,NA,.)) %>% 
    select(-`NA`) %>% 
    pivot_wider(names_from="urban",values_from="urban1s",values_fill=0,
                names_repair="unique") %>% 
    mutate_at(vars(Rural:Suburban), ~ ifelse(`NA`,NA,.)) %>% 
    select(-`NA`) 
  
  # remove any that are all missing
  
  miss_sum <- naniar::miss_case_summary(select(ses_index,-ResponseId),
                                        order = F)
  
  ses_index <- filter(ses_index, miss_sum$pct_miss<95)
  
  ses_index_long <- ses_index %>% 
    select(-own) %>% 
    mutate_at(c("edu","inc","age","econ"),~as.numeric(.)) %>% 
    gather(key="item_id",value="outcome_disc",-ResponseId) %>% 
    mutate(model_id=case_when(item_id %in% c("edu","inc","age","econ")~4,
                              item_id %in% c("gas","taxi","vat")~9,
                              TRUE~2),
           ordered_id=case_when(item_id=="edu"~7,
                                item_id=="age"~8,
                                item_id=="inc"~10,
                                item_id=="econ"~5,
                                TRUE~0),
           outcome_cont=ifelse(item_id %in% c("gas","taxi","vat"),
                               outcome_disc,0),
           outcome_disc=ifelse(item_id %in% c("gas","taxi","vat"),
                               0,outcome_disc),
           outcome_cont=as.numeric(outcome_cont),
           outcome_disc=as.numeric(outcome_disc),
           outcome_cont=ifelse(((item_id  %in% c("gas","taxi","vat")) & outcome_cont>1e7),
                               NA,outcome_cont)) %>% 
    filter(!((item_id  %in% c("gas","taxi","vat")) & is.na(outcome_cont)),
           !((item_id=="mid_biz") & is.na(outcome_disc))) %>% 
    group_by(item_id) %>% 
    mutate(outcome_cont=ifelse(item_id  %in% c("gas","taxi","vat"),
                               scale(outcome_cont),outcome_cont))
  
  check_this <- id_sim_gen(num_person=100,num_bills=5,ordinal=F,inflate=F,
                           diff_sd=1,
                           reg_discrim_sd = 1,
                           absence_discrim_sd = 1)
  
  check_resp2 <- group_by(check_this@score_matrix,person_id) %>% 
    summarize(sum_ones=sum(outcome_disc==1,na.rm=T))
  
  check_survey <- filter(ses_index_long, ResponseId %in% ses_index$ResponseId[1:300],
                         item_id %in% c("Employed","Alger",
                                        "farm","car","house","small_biz")) %>% 
    select(-outcome_cont) %>% 
    ungroup %>% 
    mutate(ResponseId=as.numeric(factor(ResponseId)),
           item_id=as.numeric(factor(item_id)))
  
  check_resp <- group_by(check_survey,ResponseId) %>% 
    summarize(sum_ones=sum(outcome_disc,na.rm=T))
  
  print("Running index.")
  
  ses_mod <- ses_index_long %>% 
    #mutate(model_id=ifelse(model_id %in% c(2,4), model_id - 1, model_id)) %>%  
    ungroup %>% 
    #filter(ResponseId %in% sample(ResponseId,1000)) %>% 
    arrange(item_id,ResponseId) %>% 
    id_make(person_id="ResponseId") %>% 
    id_estimate(restrict_ind_high = "inc",save_files="/scratch/rmk7/wbsurvey",
                restrict_ind_low="edu",const_type="items",
                fixtype="prefix",het_var = F,max_treedepth=10,
                restrict_sd_high = .001,
                nchains = 2,ncores = parallel::detectCores(),fix_low=0,restrict_sd_low = 3,
                niters = 250,warmup = 250,map_over_id = "persons",
                id_refresh=100)
  
  saveRDS(ses_mod,"data/ses_mod.rds")
  
  # plot discrimination factors
  
  discrim_abs <- filter(ses_mod@summary,grepl(x=variable,pattern="sigma\\_abs\\_free")) %>% 
    mutate(varname=factor(as.numeric(str_extract(variable,"[0-9]+")),
                          labels=levels(ses_mod@score_data@score_matrix$item_id)))
  
  discrim_abs$varname_rec <- fct_recode(discrim_abs$varname,
                                    "Age" = "age",
                                    "Algerian Bank Account" = "bank_algeria",
                                    "Foreign Bank Account" = "bank_foreign",
                                    "Car" = "car",
                                    "Economic Perceptions" = "econ",
                                    "Education" = "edu",
                                    "Own Farm" = "farm",
                                    "Gas" = "gas",
                                    "Own House" = "house",
                                    "Homemaker" = "Housewife",
                                    "Income" = "inc",
                                    "Large Business" = "large_biz",
                                    "Medium Business" = "mid_biz",
                                    "Female" = "sex",
                                    "Small Business" = "small_biz",
                                    "Taxi" = "taxi",
                                    "VAT" = "vat")
  
  discrim_abs %>% 
    ggplot(aes(y=median,x=reorder(varname_rec,median))) +
    geom_pointrange(aes(ymin=upper,ymax=lower)) +
    theme_tufte() +
    coord_flip() +
    labs(x="",y="SES Scale",
         caption=str_wrap("Plot shows discrimination estimates from Bayesian latent variable model of socio-economic resources. Points are posterior medians and intervals as the 5% to 95% posterior quantile."))
  
  ggsave("plots/discrim_abs.png")
  
} else {
  
  ses_mod <- readRDS("data/ses_mod.rds")
  
}

discrim <- filter(ses_mod@summary,grepl(x=variable,pattern="sigma\\_reg\\_free")) %>% 
  mutate(varname=factor(as.numeric(str_extract(variable,"[0-9]+")),
                        labels=levels(ses_mod@score_data@score_matrix$item_id)))

discrim$varname_rec <- fct_recode(discrim$varname,
                                  "Age" = "age",
                                  "Algerian Bank Account" = "bank_algeria",
                                  "Foreign Bank Account" = "bank_foreign",
                                  "Car" = "car",
                                  "Economic Perceptions" = "econ",
                                  "Education" = "edu",
                                  "Own Farm" = "farm",
                                  "Gas" = "gas",
                                  "Own House" = "house",
                                  "Homemaker" = "Housewife",
                                  "Income" = "inc",
                                  "Large Business" = "large_biz",
                                  "Medium Business" = "mid_biz",
                                  "Female" = "sex",
                                  "Small Business" = "small_biz",
                                  "Taxi" = "taxi",
                                  "VAT" = "vat")

discrim %>% 
  ggplot(aes(y=median,x=reorder(varname_rec,median))) +
  geom_pointrange(aes(ymin=upper,ymax=lower)) +
  theme_tufte() +
  coord_flip() +
  labs(x="",y="SES Scale",
       caption=str_wrap("Plot shows discrimination estimates from Bayesian latent variable model of socio-economic resources. Points are posterior medians and intervals as the 5% to 95% posterior quantile."))

ggsave("plots/discrim.png",width=4.5,height=7,units='in')

discrim %>% 
  ggplot(aes(y=median,x=reorder(varname_rec,median))) +
  geom_pointrange(aes(ymin=upper,ymax=lower)) +
  theme_tufte() +
  scale_x_discrete(guide = guide_axis(check.overlap = T)) +
  coord_flip() +
  labs(x="",y="SES Scale",
       caption=str_wrap("Plot shows discrimination estimates from Bayesian latent variable model of socio-economic resources. Points are posterior medians and intervals as the 5% to 95% posterior quantile."))


ggsave("plots/discrim_wide.png",width=6,height=3,units='in')



# merge in index values

index_vars <- filter(ses_mod@summary,grepl(x=variable,pattern="L\\_full")) %>% 
  mutate(varname=factor(as.numeric(str_extract(variable,"[0-9]+")),
                        labels=levels(ses_mod@score_data@score_matrix$person_id)))

survey <- left_join(survey,index_vars,by=c("ResponseId"="varname"))

wide_belief <- gather(survey,key = "outcome",value="response",matches("attitude")) %>% 
  filter(treat_account!="Default") %>% 
  mutate(outcome=factor(recode(outcome,
                               out_attitude_1="Employment",
                               out_attitude_2="Health Care",
                               out_attitude_3="Corruption",
                               out_attitude_4="Reforming",
                               out_attitude_5="Stability")),
         response=as.numeric(response))

wide_action <- gather(survey,key = "outcome",value="response",matches("out\\_action")) %>% 
  mutate(outcome=factor(recode(outcome,
                               out_action_1="Protest",
                               out_action_2="Complain",
                               out_action_3="Funds Overseas",
                               out_action_4="Switch Currencies")),
         response=as.numeric(response))


# Run fewer and better models

if(run_model) {
  
  # estimate proportion of people who protested at least once
  
  pr_protest <- stan_glmer(formula = just_one ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account,
               family=binomial,
               data = mutate(survey,just_one=as.numeric(partic!="I have not participated")),
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  
  
  pr_pred <- try(posterior_linpred(pr_protest, transform=TRUE,
                                    newdata=as.data.frame(mutate(ps_data,treat_account="treated_gas")),
                                    draws=100))
  
  pr_est <- tibble(estimate=pr_pred %*% ps_data$prop)
  
  saveRDS(pr_protest,"data/pr_protest.rds")
  
  
  

# Regime preferences ------------------------------------------------------
  
  #   d <- gather(survey,key=out_action,value=response,matches("direct")) %>% 
  #     mutate(out_action=factor(out_action,levels=c("direct_1",
  #                                                  "direct_2",
  #                                                  "direct_3"),
  #                              labels=c("I support democracy even\nif it means opposing the regime.",
  #                                       "I want Bouteflika to step down,\nbut I don't care if our political system stays\nas is or becomes a democracy.",
  #                                       "I want Bouteflika to step down\nbut I don't want our country\nto become a democracy.")),
  #            response=as.numeric(factor(response,levels=c("No","Yes")))-1)
  #   
  #   out_d <- lapply(unique(d$out_action), function (o) {
  #     this_data <- filter(d,out_action==o,treat_account!="Default")
  #     stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account,
  #                family="binomial",
  #                data = this_data,
  #                QR=T,
  #                iter = 1000, chains = 1, cores = 1,seed = 8540) 
  #   })
  # 
  # 
  # saveRDS(out_d,"data/out_mod_direct.rds")
  # 
  # adjust_direct <- lapply(1:length(out_d), function (o) {
  #     
  #        out_d <- lapply(unique(out_d[[o]]$data$treat_account), function(r) {
  #       
  #       print(paste0("Now on iter ",r))
  #       ps_data$treat_account <- r
  #       
  #       post_num <- try(posterior_linpred(out_d[[o]], transform=TRUE,
  #                                     newdata=as.data.frame(ps_data),
  #                                     draws=100))
  #       
  #       if('try-error' %in% class(post_num)) {
  #         return(NULL)
  #       }
  #       
  #       as_tibble(post_num %*% ps_data$prop) %>% 
  #         mutate(treated=r,
  #                out_action=unique(d$out_action)[o]) 
  #     }) %>% bind_rows
  #     
  #   }) %>% bind_rows  
  # 
  # # now let's do a treatment interaction
  # 
  # out_d_int <- lapply(unique(d$out_action), function (o) {
  #   this_data <- filter(d,out_action==o,treat_account!="Default")
  #   stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account*median,
  #              family="binomial",
  #              data = this_data,
  #              QR=T,
  #              iter = 1000, chains = 1, cores = 1,seed = 8540) 
  # })
  # 
  # 
  # saveRDS(out_mod_direct,"data/out_mod_direct.rds")
  # saveRDS(out_d_int,"data/out_d_int.rds")
  
  

# Beliefs -----------------------------------------------------------------


  # preference outcomes
  
  out_mod_belief <- lapply(unique(wide_belief$outcome), function (o) {
    this_data <- filter(wide_belief,outcome==o,treat_account!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  adjust_belief <- lapply(1:length(out_mod_belief), function (o) {
    
    out_d <- lapply(unique(out_mod_belief[[o]]$data$treat_account), function(r) {
      
      print(paste0("Now on iter ",r))
      ps_data$treat_account <- r
      
      post_num <- try(posterior_linpred(out_mod_belief[[o]], transform=TRUE,
                                        newdata=as.data.frame(ps_data),
                                        draws=100))
      
      if('try-error' %in% class(post_num)) {
        return(NULL)
      }
      
      as_tibble(post_num %*% ps_data$prop) %>% 
        mutate(treated=r,
               outcome=unique(wide_belief$outcome)[o]) 
    }) %>% bind_rows
    
  }) %>% bind_rows  
  
  # now let's do a treatment interaction
  
  out_belief_int <- lapply(unique(wide_belief$outcome), function (o) {
    this_data <- filter(wide_belief,outcome==o,treat_account!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account*median,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  
  saveRDS(out_mod_belief,"data/out_mod_belief.rds")
  saveRDS(out_belief_int,"data/out_belief_int.rds")

# Actions -----------------------------------------------------------------
  
  out_mod_action <- lapply(unique(wide_action$outcome), function (o) {
    this_data <- filter(wide_action,outcome==o,treat_account!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  adjust_action <- lapply(1:length(out_mod_action), function (o) {
    
    out_d <- lapply(unique(out_mod_action[[o]]$data$treat_account), function(r) {
      
      print(paste0("Now on iter ",r))
      ps_data$treat_account <- r
      
      post_num <- try(posterior_linpred(out_mod_action[[o]], transform=TRUE,
                                        newdata=as.data.frame(ps_data),
                                        draws=100))
      
      if('try-error' %in% class(post_num)) {
        return(NULL)
      }
      
      as_tibble(post_num %*% ps_data$prop) %>% 
        mutate(treated=r,
               outcome=unique(wide_action$outcome)[o]) 
    }) %>% bind_rows
    
  }) %>% bind_rows  
  
  # now let's do a treatment interaction
  
  out_action_int <- lapply(unique(wide_action$outcome), function (o) {
    this_data <- filter(wide_action,outcome==o,treat_account!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account*median,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  
  saveRDS(out_mod_action,"data/out_mod_action.rds")
  saveRDS(out_action_int,"data/out_action_int.rds")
  


# Beliefs - Disag Fuel ----------------------------------------------------

  # preference outcomes
  
  out_mod_belief_disag <- lapply(unique(wide_belief$outcome), function (o) {
    this_data <- filter(wide_belief,outcome==o,treat_account2!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account2,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  adjust_belief_disag <- lapply(1:length(out_mod_belief_disag), function (o) {
    
    out_d <- lapply(unique(out_mod_belief[[o]]$data$treat_account2), function(r) {
      
      print(paste0("Now on iter ",r))
      ps_data$treat_account2 <- r
      
      post_num <- try(posterior_linpred(out_mod_belief_disag[[o]], transform=TRUE,
                                        newdata=as.data.frame(ps_data),
                                        draws=100))
      
      if('try-error' %in% class(post_num)) {
        return(NULL)
      }
      
      as_tibble(post_num %*% ps_data$prop) %>% 
        mutate(treated=r,
               outcome=unique(wide_belief$outcome)[o]) 
    }) %>% bind_rows
    
  }) %>% bind_rows  
  
  # now let's do a treatment interaction
  
  out_belief_int_disag <- lapply(unique(wide_belief$outcome), function (o) {
    this_data <- filter(wide_belief,outcome==o,treat_account2!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account2*median,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  
  saveRDS(out_mod_belief_disag,"data/out_mod_belief_disag.rds")
  saveRDS(out_belief_int_disag,"data/out_belief_int_disag.rds")
  
  
# Actions - Disag Fuel ----------------------------------------------------

  out_mod_action_disag <- lapply(unique(wide_action$outcome), function (o) {
    this_data <- filter(wide_action,outcome==o,treat_account2!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account2,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  adjust_action_disag <- lapply(1:length(out_mod_action_disag), function (o) {
    
    out_d <- lapply(unique(out_mod_action_disag[[o]]$data$treat_account2), function(r) {
      
      print(paste0("Now on iter ",r))
      ps_data$treat_account2 <- r
      
      post_num <- try(posterior_linpred(out_mod_action_disag[[o]], transform=TRUE,
                                        newdata=as.data.frame(ps_data),
                                        draws=100))
      
      if('try-error' %in% class(post_num)) {
        return(NULL)
      }
      
      as_tibble(post_num %*% ps_data$prop) %>% 
        mutate(treated=r,
               outcome=unique(wide_action$outcome)[o]) 
    }) %>% bind_rows
    
  }) %>% bind_rows  
  
  # now let's do a treatment interaction
  
  out_action_int_disag <- lapply(unique(wide_action$outcome), function (o) {
    this_data <- filter(wide_action,outcome==o,treat_account2!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account2*median,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  
  saveRDS(out_mod_action_disag,"data/out_mod_action_disag.rds")
  saveRDS(out_action_int_disag,"data/out_action_int_disag.rds")
  
  
  } else {
  
  out_mod_action <- readRDS("data/out_mod_action.rds")
  out_action_int <- readRDS("data/out_action_int.rds")
  out_mod_belief <- readRDS("data/out_mod_belief.rds")
  out_belief_int <- readRDS("data/out_belief_int.rds")
  
  out_mod_action_disag <- readRDS("data/out_mod_action_disag.rds")
  out_action_int_disag <- readRDS("data/out_action_int_disag.rds")
  out_mod_belief_disag <- readRDS("data/out_mod_belief_disag.rds")
  out_belief_int_disag <- readRDS("data/out_belief_int_disag.rds")
  
}


# Treatment modifiers -----------------------------------------------------

# preference outcomes

if(run_model) {
  
  out_mod_belief_modif <- lapply(unique(wide_belief$outcome), function (o) {
    this_data <- filter(wide_belief,outcome==o,treat_account!="Default")
    this_data$treatModifier <- as.numeric(this_data$treatModifier)
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account*treatModifier,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  
  saveRDS(out_mod_belief_modif,"data/out_mod_belief_modif.rds")
  
  out_mod_action_modif <- lapply(unique(wide_action$outcome), function (o) {
    this_data <- filter(wide_action,outcome==o,treat_account!="Default")
    this_data$treatModifier <- as.numeric(this_data$treatModifier)
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account*treatModifier,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  saveRDS(out_mod_action_modif,"data/out_mod_action_modif.rds")
  
  
} else {
  
  out_mod_action_modif <- readRDS("data/out_mod_action_modif.rds")
  out_mod_belief_modif <- readRDS("data/out_mod_belief_modif.rds")
  
}



saveRDS(out_mod_action_modif,"data/out_mod_action_modif.rds")

# get results

require(tidybayes)

treat_mod_belief <- lapply(1:length(unique(wide_belief$outcome)), function (o) {
  gather_draws(out_mod_belief_modif[[o]],`treat_accounttreated_gas:treatModifier`,
               `treat_accounttreated_tax:treatModifier`,
               `treat_accounttreated_within:treatModifier`) %>% 
    median_qi %>% 
    mutate(outcome=unique(wide_belief$outcome)[o],
           type="Beliefs")
}) %>% bind_rows

treat_mod_action <- lapply(1:length(unique(wide_action$outcome)), function (o) {
  gather_draws(out_mod_action_modif[[o]],`treat_accounttreated_gas:treatModifier`,
               `treat_accounttreated_tax:treatModifier`,
               `treat_accounttreated_within:treatModifier`) %>% 
    median_qi %>% 
    mutate(outcome=unique(wide_action$outcome)[o],
           type="Actions")
}) %>% bind_rows

all_eff <- bind_rows(treat_mod_belief,
                     treat_mod_action) %>% 
  mutate(.variable=recode(.variable,
                          `treat_accounttreated_gas:treatModifier`="Gas X Modifier",
                          `treat_accounttreated_tax:treatModifier`="Tax X Modifier",
                          `treat_accounttreated_within:treatModifier`="Oil Revenue X Modifier"))

all_eff %>% 
  ggplot(aes(y=.value,x=outcome)) +
  geom_pointinterval(aes(ymin=.lower,ymax=.upper,colour=.variable),
                     position=position_dodge(width=.5)) +
  scale_color_viridis_d(name="")  +
  facet_wrap(~type,scales="free_x") +
  geom_hline(yintercept=0, linetype=2) +
  theme_tufte(base_family = "") +
  labs(y="",x="") +
  coord_flip()

ggsave("plots/treat_mod_check.png",scale=0.7)

# Calculate predicted quantities that adjust for survey represntativeness


# ATEs --------------------------------------------------------------------

# beliefs

belief_ps <- lapply(1:length(out_mod_belief), function(i) {
  
  lapply(unique(wide_belief$treat_account), function(a) {
    
    ps_data$treat_account <- a
    post_num <- posterior_epred(out_mod_belief[[i]], 
                                newdata=as.data.frame(ps_data))
    
    as_tibble(post_num %*% ps_data$prop) %>% 
      mutate(treatment=a,
             outcome=unique(wide_belief$outcome)[i],
             type="Attitudes")
    
  }) %>% bind_rows
  
  
}) %>% bind_rows

belief_ps_sum <- group_by(belief_ps,treatment,outcome) %>% 
  summarize(med_est=median(V1),
            high_est=quantile(V1,.95),
            low_est=quantile(V1,.05))

# disaggregated treatments

belief_ps_disag <- lapply(1:length(out_mod_belief_disag), function(i) {
  
  lapply(unique(wide_belief$treat_account2), function(a) {
    
    ps_data$treat_account2 <- a
    post_num <- posterior_epred(out_mod_belief_disag[[i]], 
                                newdata=as.data.frame(ps_data))
    
    as_tibble(post_num %*% ps_data$prop) %>% 
      mutate(treatment=a,
             outcome=unique(wide_belief$outcome)[i],
             type="Attitudes")
    
  }) %>% bind_rows
  
  
}) %>% bind_rows

belief_ps_sum_disag <- group_by(belief_ps_disag,treatment,outcome) %>% 
  summarize(med_est=median(V1),
            high_est=quantile(V1,.95),
            low_est=quantile(V1,.05))

# actions

action_ps <- lapply(1:length(out_mod_action), function(i) {
  
  lapply(unique(wide_action$treat_account), function(a) {
    
    if(a=="Default") {
      return(NULL)
    }
    
    ps_data$treat_account <- a
    post_num <- posterior_epred(out_mod_action[[i]], 
                                newdata=as.data.frame(ps_data))
    
    as_tibble(post_num %*% ps_data$prop) %>% 
      mutate(treatment=a,
             outcome=unique(wide_action$outcome)[i],
             type="Attitudes")
    
  }) %>% bind_rows
  
  
}) %>% bind_rows

action_ps_sum <- group_by(action_ps,treatment,outcome) %>% 
  summarize(med_est=median(V1),
            high_est=quantile(V1,.95),
            low_est=quantile(V1,.05))

# disaggregated treatments

action_ps_disag <- lapply(1:length(out_mod_action_disag), function(i) {
  
  lapply(unique(wide_action$treat_account2), function(a) {
    
    if(a=="Default") {
      return(NULL)
    }
    
    ps_data$treat_account2 <- a
    post_num <- posterior_epred(out_mod_action_disag[[i]], 
                                newdata=as.data.frame(ps_data))
    
    as_tibble(post_num %*% ps_data$prop) %>% 
      mutate(treatment=a,
             outcome=unique(wide_action$outcome)[i],
             type="Attitudes")
    
  }) %>% bind_rows
  
  
}) %>% bind_rows

action_ps_sum_disag <- group_by(action_ps_disag,treatment,outcome) %>% 
  summarize(med_est=median(V1),
            high_est=quantile(V1,.95),
            low_est=quantile(V1,.05))

# Conditional ATEs --------------------------------------------------------

# iterate over SES scale

# beliefs

over_vals_belief <- parallel::mclapply(1:length(out_belief_int), function(m) {
  
  this_mod <- out_belief_int[[m]]
  
  # now predict max/min of original cost combined variable
  
  ses_scale <- seq(min(wide_belief$median,na.rm=T),
                  max(wide_belief$median,na.rm=T),
                  length.out = 200)
  
  
  over_vals <- expand.grid(list(treat_account=unique(wide_belief$treat_account),
                                    cond_var=ses_scale))
  
  lapply(1:nrow(over_vals), function(r) {
      
      print(paste0("Now on iter ",r))
      these_vals <- slice(over_vals,r)
      
      ps_data$median <- these_vals$cond_var
      ps_data$treat_account <- these_vals$treat_account
      
      post_num <- posterior_epred(this_mod, 
                                    newdata=as.data.frame(ps_data),
                                    draws=100)
      
      as_tibble(post_num %*% ps_data$prop) %>% 
        mutate(cond_var=these_vals$cond_var,
               type="Beliefs",
               treated=these_vals$treat_account,
               outcome=unique(wide_belief$outcome)[m]) 
    }) %>% bind_rows
    

},mc.cores=parallel::detectCores()) %>% bind_rows

# disaggregated

over_vals_belief_disag <- parallel::mclapply(1:length(out_belief_int_disag), function(m) {
  
  this_mod <- out_belief_int_disag[[m]]
  
  # now predict max/min of original cost combined variable
  
  ses_scale <- seq(min(wide_belief$median,na.rm=T),
                   max(wide_belief$median,na.rm=T),
                   length.out = 200)
  
  
  over_vals <- expand.grid(list(treat_account2=unique(wide_belief$treat_account2),
                                cond_var=ses_scale))
  
  lapply(1:nrow(over_vals), function(r) {
    
    print(paste0("Now on iter ",r))
    these_vals <- slice(over_vals,r)
    
    ps_data$median <- these_vals$cond_var
    ps_data$treat_account2 <- these_vals$treat_account2
    
    post_num <- posterior_epred(this_mod, 
                                newdata=as.data.frame(ps_data),
                                draws=100)
    
    as_tibble(post_num %*% ps_data$prop) %>% 
      mutate(cond_var=these_vals$cond_var,
             type="Beliefs",
             treated=these_vals$treat_account2,
             outcome=unique(wide_belief$outcome)[m]) 
  }) %>% bind_rows
  
  
},mc.cores=parallel::detectCores()) %>% bind_rows

# actions

over_vals_action <- parallel::mclapply(1:length(out_action_int), function(m) {
  
  this_mod <- out_action_int[[m]]
  
  # now predict max/min of original cost combined variable
  
  ses_scale <- seq(min(wide_belief$median,na.rm=T),
                   max(wide_belief$median,na.rm=T),
                   length.out = 200)
  
  
  over_vals <- expand.grid(list(treat_account=unique(wide_belief$treat_account),
                                cond_var=ses_scale))
  
  lapply(1:nrow(over_vals), function(r) {
    
    print(paste0("Now on iter ",r))
    these_vals <- slice(over_vals,r)
    
    ps_data$median <- these_vals$cond_var
    ps_data$treat_account <- these_vals$treat_account
    
    post_num <- posterior_epred(this_mod, 
                                  newdata=as.data.frame(ps_data),
                                  draws=100)
    
    as_tibble(post_num %*% ps_data$prop) %>% 
      mutate(cond_var=these_vals$cond_var,
             type="Actions",
             treated=these_vals$treat_account,
             outcome=unique(wide_action$outcome)[m]) 
  }) %>% bind_rows
  
  
},mc.cores=parallel::detectCores()) %>% bind_rows

# disaggregated

over_vals_action_disag <- parallel::mclapply(1:length(out_action_int_disag), function(m) {
  
  this_mod <- out_action_int_disag[[m]]
  
  # now predict max/min of original cost combined variable
  
  ses_scale <- seq(min(wide_belief$median,na.rm=T),
                   max(wide_belief$median,na.rm=T),
                   length.out = 200)
  
  
  over_vals <- expand.grid(list(treat_account2=unique(wide_belief$treat_account2),
                                cond_var=ses_scale))
  
  lapply(1:nrow(over_vals), function(r) {
    
    print(paste0("Now on iter ",r))
    these_vals <- slice(over_vals,r)
    
    ps_data$median <- these_vals$cond_var
    ps_data$treat_account2 <- these_vals$treat_account2
    
    post_num <- posterior_epred(this_mod, 
                                newdata=as.data.frame(ps_data),
                                draws=100)
    
    as_tibble(post_num %*% ps_data$prop) %>% 
      mutate(cond_var=these_vals$cond_var,
             type="Actions",
             treated=these_vals$treat_account2,
             outcome=unique(wide_action$outcome)[m]) 
  }) %>% bind_rows
  
  
},mc.cores=parallel::detectCores()) %>% bind_rows


# Create Tables -----------------------------------------------------------

# create these for the average treatment effects / raw results

belief_ps %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Average Treated`=median(estimate),
            treat_5=quantile(estimate,.05),
            treat_95=quantile(estimate,.95),
            `Average Control`=median(control),
            control_5=quantile(control,.05),
            control_95=quantile(control,.95)) %>% 
  mutate(treatment=dplyr::recode(treatment,
                                 control="Control",
                                 treated_gas="Fuel Subsidy",
                                 treated_tax="VAT",
                                 treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',
         `5% T`="treat_5",
         `Average Treated`,
         `95% T`="treat_95",
         `5% C`="control_5",
         `Average Control`,
         `95% C`="control_95") %>% 
  arrange(Outcome,Treatment) %>% 
  kable(format="latex",caption="Figure 3 Results as a Table",booktabs=T,
        digits=3) %>% 
  kable_styling(latex_options=c("striped","hold_position"),
                font_size=9) %>% 
  save_kable("tables/belief_table.tex")

belief_ps %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                        control="Control",
                        treated_gas="Fuel Subsidy",
                        treated_tax="VAT",
                        treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=Treatment)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`)) +
  geom_hline(yintercept=0,linetype=2) +
  theme_tufte() +
  coord_flip() +
  labs(y="0 to 10 Slider Scales",x="",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",ncol = 2)

ggsave("plots/belief_ate.png",height=7,width=5,units="in")  

belief_ps %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                          control="Control",
                          treated_gas="Fuel Subsidy",
                          treated_tax="VAT",
                          treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=Treatment)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`)) +
  geom_hline(yintercept=0,linetype=2) +
  theme_tufte() +
  coord_flip() +
  labs(y="0 to 10 Slider Scales",x="",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",nrow = 2)

ggsave("plots/belief_ate_wide.png",height=3,width=6,units="in")  

# disaggregate

belief_ps_disag %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_car,treated_taxi,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Average Treated`=median(estimate),
            treat_5=quantile(estimate,.05),
            treat_95=quantile(estimate,.95),
            `Average Control`=median(control),
            control_5=quantile(control,.05),
            control_95=quantile(control,.95)) %>% 
  mutate(treatment=dplyr::recode(treatment,
                                 control="Control",
                                 treated_car="Fuel Subsidy - Car",
                                 treated_taxi="Fuel Subsidy - Taxi",
                                 treated_tax="VAT",
                                 treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',
         `5% T`="treat_5",
         `Average Treated`,
         `95% T`="treat_95",
         `5% C`="control_5",
         `Average Control`,
         `95% C`="control_95") %>% 
  arrange(Outcome,Treatment) %>% 
  kable(format="latex",caption="Disaggregated Figure 3 Results as a Table",
        booktabs=T,digits=3
  ) %>% 
  kable_styling(latex_options=c("striped","hold_position"),
                font_size=9) %>% 
  save_kable("tables/belief_table_disag.tex")

belief_ps_disag %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_car,treated_taxi,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                          control="Control",
                          treated_car="Fuel Subsidy - Car",
                          treated_taxi="Fuel Subsidy - Taxi",
                          treated_tax="VAT",
                          treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=Treatment)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`)) +
  geom_hline(yintercept=0,linetype=2) +
  theme_tufte() +
  theme(text=element_text(family="")) +
  coord_flip() +
  labs(y="0 to 10 Slider Scales",x="",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",ncol = 2)

ggsave("plots/belief_ate_disag.png",height=7,width=5,units="in")  

belief_ps_disag %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_car,treated_taxi,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                          control="Control",
                          treated_car="Fuel Subsidy - Car",
                          treated_taxi="Fuel Subsidy - Taxi",
                          treated_tax="VAT",
                          treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=Treatment)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`)) +
  geom_hline(yintercept=0,linetype=2) +
  theme_tufte() +
  theme(text=element_text(family="")) +
  coord_flip() +
  labs(y="0 to 10 Slider Scales",x="",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",nrow = 2)

ggsave("plots/belief_ate_disag_wide.png",height=3,width=6,units="in")  

action_ps %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Average Treated`=median(estimate),
            treat_5=quantile(estimate,.05),
            treat_95=quantile(estimate,.95),
            `Average Control`=median(control),
            control_5=quantile(control,.05),
            control_95=quantile(control,.95)) %>% 
  mutate(treatment=dplyr::recode(treatment,
                        control="Control",
                        treated_gas="Fuel Subsidy",
                        treated_tax="VAT",
                        treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',
         `5% T`="treat_5",
         `Average Treated`,
         `95% T`="treat_95",
         `5% C`="control_5",
         `Average Control`,
         `95% C`="control_95") %>% 
  arrange(Outcome,Treatment) %>% 
  kable(format="latex",
        caption="Figure 4 Results as a Table",booktabs=T,
        digits=3) %>% 
  kable_styling(latex_options=c("striped","hold_position"),
                font_size = 9) %>% 
  save_kable("tables/action_table.tex")

action_ps %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                        control="Control",
                        treated_gas="Fuel Subsidy",
                        treated_tax="VAT",
                        treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=Treatment)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`)) +
  theme_tufte(base_family = "") +
  coord_flip() +
  geom_hline(yintercept=0,linetype=2) +
  labs(y="0 to 10 Slider Scales",x="",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",ncol = 2)

ggsave("plots/action_ate.png",height=7,width=5,units="in") 

action_ps %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                          control="Control",
                          treated_gas="Fuel Subsidy",
                          treated_tax="VAT",
                          treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=Treatment)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`)) +
  theme_tufte(base_family = "") +
  coord_flip() +
  geom_hline(yintercept=0,linetype=2) +
  labs(y="0 to 10 Slider Scales",x="",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",nrow = 2)

ggsave("plots/action_ate_wide.png",height=3,width=6,units="in") 

# disaggregated treatment

action_ps_disag %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_car,treated_taxi,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Average Treated`=median(estimate),
            treat_5=quantile(estimate,.05),
            treat_95=quantile(estimate,.95),
            `Average Control`=median(control),
            control_5=quantile(control,.05),
            control_95=quantile(control,.95)) %>% 
  mutate(treatment=dplyr::recode(treatment,
                          control="Control",
                          treated_car="Fuel Subsidy - Car",
                          treated_taxi="Fuel Subsidy - Taxi",
                          treated_tax="VAT",
                          treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',
         `5% T`="treat_5",
         `Average Treated`,
         `95% T`="treat_95",
         `5% C`="control_5",
         `Average Control`,
         `95% C`="control_95") %>% 
  arrange(Outcome,Treatment) %>% 
  kable(format="latex",
        caption="Disaggregated Figure 4 Results as a Table",
        booktabs=T,digits=3) %>% 
  kable_styling(latex_options=c("striped","hold_position"),
                font_size=9) %>% 
  save_kable("tables/action_table_disag.tex")

action_ps_disag %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_car,treated_taxi,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                          control="Control",
                          treated_car="Fuel Subsidy - Car",
                          treated_taxi="Fuel Subsidy - Taxi",
                          treated_tax="VAT",
                          treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=Treatment)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`)) +
  theme_tufte() +
  theme(text=element_text(family="")) +
  coord_flip() +
  geom_hline(yintercept=0,linetype=2) +
  labs(y="0 to 10 Slider Scales",x="",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",ncol = 2)

ggsave("plots/action_ate_disag.png",height=7,width=5,units="in") 

action_ps_disag %>% 
  group_by(outcome,treatment) %>% 
  mutate(iter=1:n()) %>% 
  spread(treatment,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_car,treated_taxi,treated_within) %>% 
  group_by(treatment,outcome) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                          control="Control",
                          treated_car="Fuel Subsidy - Car",
                          treated_taxi="Fuel Subsidy - Taxi",
                          treated_tax="VAT",
                          treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=Treatment)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`)) +
  theme_tufte() +
  theme(text=element_text(family="")) +
  coord_flip() +
  geom_hline(yintercept=0,linetype=2) +
  labs(y="0 to 10 Slider Scales",x="",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",nrow = 2)

ggsave("plots/action_ate_disag_wide.png",height=3,width=6,units="in") 


# now subset by SES

belief_ses <- over_vals_belief %>% 
  group_by(cond_var,type,outcome,treated) %>% 
  mutate(iter=1:n()) %>% 
  spread(treated,V1) %>% 
  gather(key="treated",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treated,outcome,cond_var) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treated=recode(treated,
                        treated_gas="Fuel Subsidy",
                        treated_tax="VAT",
                        treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treated',everything())
  

belief_ses %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=cond_var,ymin=`5% Low`,
                 ymax=`95% High`)) +
  geom_ribbon(alpha=0.5) +
  geom_line() +
  theme_tufte(base_family = "") +
  geom_hline(yintercept=0,linetype=2,colour="red") +
  labs(x="SES Scale",y="Average Treatment Effect",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_grid(rows=vars(Outcome),
             cols=vars(Treatment),scales="free_x") +
  ggtitle("Treatments") +
  theme(plot.title=element_text(hjust=0.5))

ggsave("plots/belief_ate_varying.png",height=7,width=5,units="in") 

belief_ses %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=cond_var,ymin=`5% Low`,
             ymax=`95% High`)) +
  geom_ribbon(alpha=0.5) +
  geom_line() +
  theme_tufte(base_family = "") +
  geom_hline(yintercept=0,linetype=2,colour="red") +
  labs(x="SES Scale",y="Average Treatment Effect",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_grid(cols=vars(Outcome),
             rows=vars(Treatment),scales="free_x") +
  ggtitle("Treatments") +
  theme(plot.title=element_text(hjust=0.5))

ggsave("plots/belief_ate_varying_wide.png",height=3,width=6,units="in") 

# disaggregated

belief_ses_disag <- over_vals_belief_disag %>% 
  group_by(cond_var,type,outcome,treated) %>% 
  mutate(iter=1:n()) %>% 
  spread(treated,V1) %>% 
  gather(key="treated",value="estimate",treated_tax,
         treated_taxi,
         treated_car,treated_within) %>% 
  group_by(treated,outcome,cond_var) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treated=recode(treated,
                        treated_car="Fuel Subsidy\nCar",
                        treated_taxi="Fuel Subsidy\nTaxi",
                        treated_tax="VAT",
                        treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treated',everything())


belief_ses_disag %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=cond_var,ymin=`5% Low`,
             ymax=`95% High`)) +
  geom_ribbon(alpha=0.5) +
  geom_line() +
  theme_tufte(base_family = "") +
  geom_hline(yintercept=0,linetype=2,colour="red") +
  labs(x="SES Scale",y="Average Treatment Effect",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_grid(rows=vars(Outcome),
             cols=vars(Treatment),scales="free_x") +
  ggtitle("Treatments") +
  theme(plot.title=element_text(hjust=0.5),
        text=element_text(family=""))

ggsave("plots/belief_ate_varying_disag.png",height=7,width=5,units="in") 

action_ses <- over_vals_action %>% 
  group_by(cond_var,type,outcome,treated) %>% 
  mutate(iter=1:n()) %>% 
  spread(treated,V1) %>% 
  gather(key="treated",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treated,outcome,cond_var) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treated=recode(treated,
                        control="Control",
                        treated_gas="Fuel Subsidy",
                        treated_tax="VAT",
                        treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treated',everything())


action_ses %>% 
  filter(Treatment!="Control") %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=cond_var,ymin=`5% Low`,
             ymax=`95% High`)) +
  geom_ribbon(alpha=0.5) +
  geom_line() +
  theme_tufte(base_family = "") +
  geom_hline(yintercept=0,linetype=2,colour="red") +
  labs(x="SES Scale",y="Average Treatment Effect",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_grid(rows=vars(Outcome),
             cols=vars(Treatment),scales="free_x") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Treatments")

ggsave("plots/action_ate_varying.png",height=7,width=5,units="in") 

action_ses %>% 
  filter(Treatment!="Control") %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=cond_var,ymin=`5% Low`,
             ymax=`95% High`)) +
  geom_ribbon(alpha=0.5) +
  geom_line() +
  theme_tufte(base_family = "") +
  geom_hline(yintercept=0,linetype=2,colour="red") +
  labs(x="SES Scale",y="Average Treatment Effect",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_grid(cols=vars(Outcome),
             rows=vars(Treatment),scales="free_x") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Treatments")

ggsave("plots/action_ate_varying_wide.png",height=3,width=6,units="in") 

# disaggregated

action_ses_disag <- over_vals_action_disag %>% 
  group_by(cond_var,type,outcome,treated) %>% 
  mutate(iter=1:n()) %>% 
  spread(treated,V1) %>% 
  gather(key="treated",value="estimate",treated_tax,
         treated_car,treated_taxi,treated_within) %>% 
  group_by(treated,outcome,cond_var) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treated=recode(treated,
                        treated_car="Fuel Subsidy\nCar",
                        treated_taxi="Fuel Subsidy\nTaxi",
                        treated_tax="VAT",
                        treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treated',everything())


action_ses_disag %>% 
  filter(Treatment!="Control") %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=cond_var,ymin=`5% Low`,
             ymax=`95% High`)) +
  geom_ribbon(alpha=0.5) +
  geom_line() +
  theme_tufte(base_family = "") +
  geom_hline(yintercept=0,linetype=2,colour="red") +
  labs(x="SES Scale",y="Average Treatment Effect",
       caption=stringr::str_wrap("Estimates are the difference of the predicted values of the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_grid(rows=vars(Outcome),
             cols=vars(Treatment),scales="free_x") +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="")) +
  ggtitle("Treatments")

ggsave("plots/action_ate_varying_disag.png",height=7,width=5,units="in") 

# make a histogram of the SES scale

wide_belief %>% 
  ggplot(aes(x=median)) +
  geom_histogram() +
  theme_tufte(base_family = "") +
  labs(x="Socio-economic Resources Scale",y="Count") 

ggsave("plots/ses_hist.png")

# Treatment by SES Table --------------------------------------------------------

# need a table showing value of treatment by different levels (deciles) of SES

belief_ses_deciles <- over_vals_belief %>% 
  mutate(ses_quant=ntile(cond_var, 10)) %>% 
  group_by(cond_var,type,outcome,treated) %>% 
  mutate(iter=1:n()) %>% 
  spread(treated,V1) %>% 
  gather(key="treated",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treated,outcome,ses_quant) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treated=dplyr::recode(treated,
                        treated_gas="Fuel Subsidy",
                        treated_tax="VAT",
                        treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treated',SES="ses_quant",
         everything())

belief_ses_deciles %>% 
  arrange(Outcome,Treatment,SES) %>% 
  rename(`SES Decile`="SES") %>% 
  kable(format="latex",
        caption="Table for Figure 6 Results: Accountability and SES Moderation",
        booktabs=T,digits=3,longtable=T
  ) %>% 
  kable_styling(latex_options=c("striped","hold_position")) %>% 
  save_kable("tables/belief_table_deciles.tex")

action_ses_deciles <- over_vals_action %>% 
  mutate(ses_quant=ntile(cond_var, 10)) %>% 
  group_by(cond_var,type,outcome,treated) %>% 
  mutate(iter=1:n()) %>% 
  spread(treated,V1) %>% 
  gather(key="treated",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treated,outcome,ses_quant) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treated=dplyr::recode(treated,
                               treated_gas="Fuel Subsidy",
                               treated_tax="VAT",
                               treated_within="Govt. Revenue")) %>% 
  select(Outcome="outcome",Treatment='treated',SES="ses_quant",
         everything())


action_ses_deciles %>% 
  arrange(Outcome,Treatment,SES) %>% 
  rename(`SES Decile`="SES") %>% 
  kable(format="latex",
        caption="Table for Figure 6 Results: Accountability and SES Moderation",
        booktabs=T,digits=3,longtable=T
  ) %>% 
  kable_styling(latex_options=c("striped","hold_position")) %>% 
  save_kable("tables/action_table_deciles.tex")

# By political opinion ----------------------------------------------------

# estimate model

if(run_model) {
  
  out_belief_int_prot <- lapply(unique(wide_belief$outcome), function (o) {
    this_data <- filter(wide_belief,outcome==o,treat_account!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account*prot,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  saveRDS(out_belief_int_prot,"data/out_belief_int_prot.rds")
  
} else {
  
  out_belief_int_prot <- readRDS("data/out_belief_int_prot.rds")
  
}

if(run_model) {
  
  out_action_int_prot <- lapply(unique(wide_action$outcome), function (o) {
    this_data <- filter(wide_action,outcome==o,treat_account!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account*prot,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  saveRDS(out_action_int_prot,"data/out_action_int_prot.rds")
  
} else {
  
  out_action_int_prot <- readRDS("data/out_action_int_prot.rds")
  
}



over_op_belief <- parallel::mclapply(1:length(out_belief_int_prot), function(m) {
  
  this_mod <- out_belief_int_prot[[m]]
  
  # now predict max/min of original cost combined variable
  
  opinions <- unique(wide_belief$prot)
  
  
  over_vals <- expand.grid(list(treat_account=unique(wide_belief$treat_account),
                                cond_var=opinions))
  
  lapply(1:nrow(over_vals), function(r) {
    
    print(paste0("Now on iter ",r))
    these_vals <- slice(over_vals,r)
    
    ps_data$prot <- these_vals$cond_var
    ps_data$treat_account <- these_vals$treat_account
    
    post_num <- posterior_epred(this_mod, 
                                newdata=as.data.frame(ps_data),
                                draws=100)
    
    as_tibble(post_num %*% ps_data$prop) %>% 
      mutate(cond_var=these_vals$cond_var,
             type="Beliefs",
             treated=these_vals$treat_account,
             outcome=unique(wide_belief$outcome)[m]) 
  }) %>% bind_rows
  
  
},mc.cores=parallel::detectCores()) %>% bind_rows

over_op_belief %>% 
  group_by(cond_var,type,outcome,treated) %>% 
  mutate(iter=1:n()) %>% 
  spread(treated,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treatment,outcome,cond_var) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                          treated_gas="Fuel Subsidy",
                          treated_tax="VAT",
                          treated_within="Govt. Revenue"),
         cond_var=ordered(cond_var,
                      levels=c("Strongly oppose",
                               "Somewhat oppose",
                               "Neutral",
                               "Somewhat support",
                               "Strongly support"))) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=cond_var)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`,
                      colour=Treatment,
                      shape=Treatment),position=position_dodge(width=0.7)) +
  theme_tufte(base_family = "") +
  coord_flip() +
  geom_hline(yintercept=0,linetype=2) +
  scale_color_viridis_d() +
  labs(y="0 to 10 Slider Scales",x="Support for Protests",
       caption=stringr::str_wrap("Estimates are differences in the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",ncol = 2) + 
  theme(legend.position = "bottom")

ggsave("plots/belief_prot_int.png",height=7,width=5,units="in")  

# actions

over_op_action <- parallel::mclapply(1:length(out_action_int_prot), function(m) {
  
  this_mod <- out_action_int_prot[[m]]
  
  # now predict max/min of original cost combined variable
  
  opinions <- unique(wide_action$prot)
  
  opinions <- opinions[!is.na(opinions)]
  
  
  over_vals <- expand.grid(list(treat_account=unique(wide_action$treat_account),
                                cond_var=opinions))
  
  over_vals <- filter(over_vals, treat_account!="Default")

  lapply(1:nrow(over_vals), function(r) {
    
    print(paste0("Now on iter ",r))
    these_vals <- slice(over_vals,r)
    
    ps_data$prot <- these_vals$cond_var
    ps_data$treat_account <- these_vals$treat_account
    
    post_num <- posterior_epred(this_mod, 
                                newdata=as.data.frame(ps_data),
                                draws=100)
    
    as_tibble(post_num %*% ps_data$prop) %>% 
      mutate(cond_var=these_vals$cond_var,
             type="Actions",
             treated=these_vals$treat_account,
             outcome=unique(wide_action$outcome)[m]) 
  }) %>% bind_rows
  
  
},mc.cores=parallel::detectCores()) %>% bind_rows

over_op_action %>% 
  group_by(cond_var,type,outcome,treated) %>% 
  mutate(iter=1:n()) %>% 
  spread(treated,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treatment,outcome,cond_var) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                          treated_gas="Fuel Subsidy",
                          treated_tax="VAT",
                          treated_within="Govt. Revenue"),
         cond_var=ordered(cond_var,
                          levels=c("Strongly oppose",
                                   "Somewhat oppose",
                                   "Neutral",
                                   "Somewhat support",
                                   "Strongly support"))) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=cond_var)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`,
                      colour=Treatment,
                      shape=Treatment),position=position_dodge(width=0.7)) +
  theme_tufte(base_family = "") +
  coord_flip() +
  geom_hline(yintercept=0,linetype=2) +
  scale_color_viridis_d() +
  labs(y="0 to 10 Slider Scales",x="Support for Protests",
       caption=stringr::str_wrap("Estimates are differences the 0-10 outcome of the likelihood of a given action between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",ncol = 2) + 
  theme(legend.position = "bottom")

ggsave("plots/action_prot_int.png",height=7,width=5,units="in") 


# By Corruption ---------------------------------------------------------


if(run_model) {
  
  out_belief_int_corr <- lapply(unique(wide_belief$outcome), function (o) {
    this_data <- filter(wide_belief,outcome==o,treat_account!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account*corr,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  saveRDS(out_belief_int_corr,"data/out_belief_int_corr.rds")
  
} else {
  
  out_belief_int_corr <- readRDS("data/out_belief_int_corr.rds")
  
}

if(run_model) {
  
  out_action_int_corr <- lapply(unique(wide_action$outcome), function (o) {
    this_data <- filter(wide_action,outcome==o,treat_account!="Default")
    stan_glmer(formula = response ~ (1 | age) + (1 | sex) + (1 | gov) + treat_account*corr,
               family="gaussian",
               data = this_data,
               QR=T,
               iter = 1000, chains = 1, cores = 1,seed = 8540) 
  })
  
  saveRDS(out_action_int_corr,"data/out_action_int_corr.rds")
  
} else {
  
  out_action_int_corr <- readRDS("data/out_action_int_corr.rds")
  
}



over_op_belief <- parallel::mclapply(1:length(out_belief_int_corr), function(m) {
  
  this_mod <- out_belief_int_corr[[m]]
  
  # now predict max/min of original cost combined variable
  
  opinions <- unique(wide_belief$corr)
  
  opinions <- opinions[!is.na(opinions)]
  
  
  over_vals <- expand.grid(list(treat_account=unique(wide_belief$treat_account),
                                cond_var=opinions))
  
  lapply(1:nrow(over_vals), function(r) {
    
    print(paste0("Now on iter ",r))
    these_vals <- slice(over_vals,r)
    
    ps_data$corr <- these_vals$cond_var
    ps_data$treat_account <- these_vals$treat_account
    
    post_num <- posterior_epred(this_mod, 
                                newdata=as.data.frame(ps_data),
                                draws=100)
    
    as_tibble(post_num %*% ps_data$prop) %>% 
      mutate(cond_var=these_vals$cond_var,
             type="Beliefs",
             treated=these_vals$treat_account,
             outcome=unique(wide_belief$outcome)[m]) 
  }) %>% bind_rows
  
  
},mc.cores=parallel::detectCores()) %>% bind_rows

corr_belief <- over_op_belief %>% 
  group_by(cond_var,type,outcome,treated) %>% 
  mutate(iter=1:n()) %>% 
  spread(treated,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treatment,outcome,cond_var) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                          treated_gas="Fuel Subsidy",
                          treated_tax="VAT",
                          treated_within="Govt. Revenue"),
         cond_var=ordered(cond_var,
                          levels=c("Very low",
                                   "Low",
                                   "Moderate",
                                   "High",
                                   "Very High"))) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=cond_var)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`,
                      colour=Treatment,
                      shape=Treatment),position=position_dodge(width=0.7)) +
  theme_tufte(base_family = "") +
  coord_flip() +
  geom_hline(yintercept=0,linetype=2) +
  scale_color_viridis_d() +
  labs(y="0 to 10 Slider Scales",x="Government Corruption",
       caption=stringr::str_wrap("Estimates are differneces in the 0-10 outcome assessing the government's performance between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",ncol = 2) + 
  theme(legend.position = "bottom")

corr_belief

ggsave("plots/belief_corr_int.png",height=7,width=5,units="in")  

# actions

over_op_action <- parallel::mclapply(1:length(out_action_int_corr), function(m) {
  
  this_mod <- out_action_int_corr[[m]]
  
  # now predict max/min of original cost combined variable
  
  opinions <- unique(wide_action$corr)
  
  opinions <- opinions[!is.na(opinions)]
  
  
  over_vals <- expand.grid(list(treat_account=unique(wide_action$treat_account),
                                cond_var=opinions))
  
  over_vals <- filter(over_vals, treat_account!="Default")
  
  lapply(1:nrow(over_vals), function(r) {
    
    print(paste0("Now on iter ",r))
    these_vals <- slice(over_vals,r)
    
    ps_data$corr <- these_vals$cond_var
    ps_data$treat_account <- these_vals$treat_account
    
    post_num <- posterior_epred(this_mod, 
                                newdata=as.data.frame(ps_data),
                                draws=100)
    
    as_tibble(post_num %*% ps_data$prop) %>% 
      mutate(cond_var=these_vals$cond_var,
             type="Actions",
             treated=these_vals$treat_account,
             outcome=unique(wide_action$outcome)[m]) 
  }) %>% bind_rows
  
  
},mc.cores=parallel::detectCores()) %>% bind_rows

corr_act <- over_op_action %>% 
  group_by(cond_var,type,outcome,treated) %>% 
  mutate(iter=1:n()) %>% 
  spread(treated,V1) %>% 
  gather(key="treatment",value="estimate",treated_tax,
         treated_gas,treated_within) %>% 
  group_by(treatment,outcome,cond_var) %>% 
  summarize(`Predicted ATE`=median(estimate-control),
            `5% Low`=quantile(estimate-control,.05),
            `95% High`=quantile(estimate-control,.95)) %>% 
  mutate(treatment=recode(treatment,
                          treated_gas="Fuel Subsidy",
                          treated_tax="VAT",
                          treated_within="Govt. Revenue"),
         cond_var=ordered(cond_var,
                          levels=c("Very low",
                                   "Low",
                                   "Moderate",
                                   "High",
                                   "Very High"))) %>% 
  select(Outcome="outcome",Treatment='treatment',everything()) %>% 
  ggplot(aes(y=`Predicted ATE`,
             x=cond_var)) +
  geom_pointrange(aes(ymin=`5% Low`,
                      ymax=`95% High`,
                      colour=Treatment,
                      shape=Treatment),position=position_dodge(width=0.7)) +
  theme_tufte(base_family = "") +
  coord_flip() +
  geom_hline(yintercept=0,linetype=2) +
  scale_color_viridis_d() +
  labs(y="0 to 10 Slider Scales",x="Government Corruption",
       caption=stringr::str_wrap("Estimates are differences in the 0-10 outcome assessing the likelihood of taking a particular between treatment and control. Estimates are adjusted with MRP to match Algerian census totals.")) +
  facet_wrap(~Outcome,scales="free_x",ncol = 2) + 
  theme(legend.position = "bottom")

corr_act

ggsave("plots/action_corr_int.png",height=7,width=5,units="in") 


# Probe the Wealth Result -------------------------------------------------

# see differences in wealth by car ownership

survey %>% filter(treat_account %in% c("treated_gas",
                                       "treated_tax",
                                       "control")) %>% 
  ggplot(aes(x=median)) +
  geom_density(aes(fill=car),alpha=0.5) +
  scale_fill_viridis_d(name="Car Ownership") +
  theme_tufte(base_family="") +
  labs(x="SES Index",y="",
       caption=str_wrap("Plot shows estimated SES index values by reported car ownership for all 8,724 observations.",
                        60))
 
ggsave("plots/car_ownership_ses.pdf",width=5,height=3)

# look at wealth differences by protest

survey$prot_future <- factor(survey$prot_future,
                             levels=c("Very unlikely",
                                      "Somewhat unlikely",
                                      "Somewhat likely",
                                      "Very likely"))

survey %>% filter(treat_account %in% c("treated_gas",
                                       "treated_tax",
                                       "control",
                                       "treated_within"),
                  !is.na(prot_future)) %>% 
  ggplot(aes(y=median,x=prot_future)) +
  stat_summary(fun.data="mean_cl_boot") +
  theme_tufte(base_family="") +
  labs(y="SES Index",x="",
       caption=str_wrap("Plot shows average SES index values with 5% to 95% bootstrapped CIs by reported protest intentions for all 8,724 observations.",
                        60))


ggsave("plots/prot_future_ses.pdf",width=5,height=3)

survey %>% filter(treat_account %in% c("treated_gas",
                                       "treated_tax",
                                       "control",
                                       "treated_within"),
                  !is.na(prot)) %>% 
  mutate(prot=factor(prot,
                     levels=c("Strongly oppose",
                              "Somewhat oppose",
                              "Neutral",
                              "Somewhat support",
                              "Strongly support"),
                     labels=c("Strongly\noppose",
                              "Somewhat\n oppose",
                              "Neutral",
                              "Somewhat\nsupport",
                              "Strongly\nsupport"))) %>% 
  ggplot(aes(y=median,x=prot)) +
  stat_summary(fun.data="mean_cl_boot") +
  theme_tufte(base_family="") +
  labs(y="SES Index",x="",
       caption=str_wrap("Plot shows average SES index values with 5% to 95% bootstrapped CIs by reported support for the protests for all 8,724 observations.",
                        60))


ggsave("plots/prot_support_ses.pdf",width=5,height=3)

# survey %>% filter(treat_account %in% c("treated_gas",
#                                        "treated_tax",
#                                        "control",
#                                        "treated_within"),
#                   !is.na(dembest)) %>% 
#   mutate(dembest=factor(dembest,
#                      levels=c("Strongly disagree",
#                               "Somewhat disagree",
#                               "Neutral",
#                               "Somewhat agree",
#                               "Strongly agree"))) %>% 
#   ggplot(aes(y=median,x=dembest)) +
#   stat_summary(fun.data="mean_cl_boot") +
#   theme_tufte(base_family="") +
#   labs(y="SES Index",x="",
#        caption=str_wrap("Plot shows average SES index values with 5% to 95% bootstrapped CIs by reported attitudes towards democracy for all 8,724 observations.",
#                         60))
# 
# 
# ggsave("plots/dembest_ses.pdf",width=5,height=3)

survey %>% filter(treat_account %in% c("treated_gas",
                                       "treated_tax",
                                       "control",
                                       "treated_within"),
                  !is.na(econ)) %>% 
  mutate(econ=factor(econ,
                        levels=c("Very dissatisfied",
                                 "Somewhat dissatisfied",
                                 "Neutral",
                                 "Somewhat satisfied",
                                 "Very satisfied"))) %>% 
  ggplot(aes(y=median,x=econ)) +
  stat_summary(fun.data="mean_cl_boot") +
  theme_tufte(base_family="") +
  labs(y="SES Index",x="",
       caption=str_wrap("Plot shows average SES index values with 5% to 95% bootstrapped CIs by reported attitudes towards democracy for all 8,724 observations.",
                        60))


ggsave("plots/econ_ses.pdf",width=5,height=3)

survey %>% filter(treat_account %in% c("treated_gas",
                                       "treated_tax",
                                       "control",
                                       "treated_within"),
                  !is.na(inst_2)) %>% 
  mutate(inst_2=factor(inst_2,
                       levels=c("Strongly oppose",
                                "Oppose",
                                "Neutral",
                                "Support",
                                "Strongly support"),
                       labels=c("Strongly\noppose",
                                "Oppose",
                                "Neutral",
                                "Support",
                                "Strongly\nsupport"))) %>% 
  ggplot(aes(y=median,x=inst_2)) +
  stat_summary(fun.data="mean_cl_boot") +
  theme_tufte(base_family="") +
  labs(y="SES Index",x="",
       caption=str_wrap("Plot shows average SES index values with 5% to 95% bootstrapped CIs by reported attitudes towards democracy for all 8,724 observations.",
                        60))


ggsave("plots/support_pol_system_ses.pdf",width=5,height=3)

survey %>% filter(treat_account %in% c("treated_gas",
                                       "treated_tax",
                                       "control",
                                       "treated_within"),
                  !is.na(inst_3)) %>% 
  mutate(inst_3=factor(inst_3,
                       levels=c("Strongly oppose",
                                "Oppose",
                                "Neutral",
                                "Support",
                                "Strongly support"),
                       labels=c("Strongly\noppose",
                                "Oppose",
                                "Neutral",
                                "Support",
                                "Strongly\nsupport"))) %>% 
  ggplot(aes(y=median,x=inst_3)) +
  stat_summary(fun.data="mean_cl_boot") +
  theme_tufte(base_family="") +
  labs(y="SES Index",x="",
       caption=str_wrap("Plot shows average SES index values with 5% to 95% bootstrapped CIs by reported attitudes towards democracy for all 8,724 observations.",
                        60))


ggsave("plots/military_ses.pdf",width=5,height=3)

survey %>% filter(treat_account %in% c("treated_gas",
                                       "treated_tax",
                                       "control",
                                       "treated_within"),
                  !is.na(inst_3)) %>% 
  mutate(inst_3=factor(inst_3,
                       levels=c("Strongly oppose",
                                "Oppose",
                                "Neutral",
                                "Support",
                                "Strongly support"),
                       labels=c("Strongly\noppose",
                                "Oppose",
                                "Neutral",
                                "Support",
                                "Strongly\nsupport"))) %>% 
  ggplot(aes(y=median,x=direct_3)) +
  stat_summary(fun.data="mean_cl_boot") +
  theme_tufte(base_family="") +
  labs(y="SES Index",x="",
       caption=str_wrap("Plot shows average SES index values with 5% to 95% bootstrapped CIs by reported attitudes towards democracy for all 8,724 observations.",
                        60))


ggsave("plots/oppose_regime_ses.pdf",width=5,height=3)

# look at regime goals

survey <- mutate(survey,
                 goal_unemp=grepl(x=goals,
                                  pattern="protest unemployment"),
                 goal_dem=grepl(x=goals,
                                pattern="call for democracy"),
                 goal_corr=grepl(x=goals,
                                  pattern="protest corruption"),
                 goal_fall=grepl(x=goals,
                                pattern="fall of the system"),
                 goal_destabilize=grepl(x=goals,
                                        pattern="destabilize"),
                 goal_wages=grepl(x=goals,
                                  pattern="low wages"),
                 goal_islamic=grepl(x=goals,
                                    pattern="Islamic system"))

survey %>% filter(treat_account %in% c("treated_gas",
                                       "treated_tax",
                                       "control",
                                       "treated_within"),
                  !is.na(goals)) %>%
  gather(key="Goal",value="indicator",matches("goal\\_")) %>% 
  mutate(Goal=recode(Goal,
                     goal_unemp="Unemployment",
                     goal_dem="Democracy",
                     goal_corr="Corruption",
                     goal_fall="Fall\nof System",
                     goal_destabilize="Destabilize\nSystem",
                     goal_wages="Low\nWages",
                     goal_islamic="Islamic System")) %>% 
  mutate(inst_3=factor(inst_3,
                       levels=c("Strongly oppose",
                                "Oppose",
                                "Neutral",
                                "Support",
                                "Strongly support"),
                       labels=c("Strongly\noppose",
                                "Oppose",
                                "Neutral",
                                "Support",
                                "Strongly\nsupport"))) %>% 
  ggplot(aes(y=median,x=Goal)) +
  stat_summary(fun.data="mean_cl_boot",
               aes(colour=indicator,
                   shape=indicator),
               position=position_dodge(.25)) +
  theme_tufte(base_family="") +
  scale_color_viridis_d(name="Support Goal") +
  scale_shape(name="Support Goal") +
  labs(y="SES Index",x="",
       caption=str_wrap("Plot shows average SES index values with 5% to 95% bootstrapped CIs by reported support for protest goals for all 8,724 observations.",
                        60)) +
  coord_flip()


ggsave("plots/prot_goals_ses.pdf",width=5,height=3)

survey %>% filter(treat_account %in% c("treated_gas",
                                       "treated_tax",
                                       "control",
                                       "treated_within"),
                  !is.na(goals)) %>%
  gather(key="Goal",value="indicator",matches("goal\\_")) %>% 
  mutate(Goal=recode(Goal,
                     goal_unemp="Unemployment",
                     goal_dem="Democracy",
                     goal_corr="Corruption",
                     goal_fall="Fall\nof System",
                     goal_destabilize="Destabilize\nSystem",
                     goal_wages="Low\nWages",
                     goal_islamic="Islamic System")) %>% 
  mutate(inst_3=factor(inst_3,
                       levels=c("Strongly oppose",
                                "Oppose",
                                "Neutral",
                                "Support",
                                "Strongly support"),
                       labels=c("Strongly\noppose",
                                "Oppose",
                                "Neutral",
                                "Support",
                                "Strongly\nsupport"))) %>% 
  ggplot(aes(y=median,x=Goal)) +
  stat_summary(fun.data="mean_cl_boot",
               aes(colour=indicator,
                   shape=indicator),
               position=position_dodge(.25)) +
  theme_tufte(base_family="") +
  scale_color_viridis_d(name="Support Goal") +
  scale_shape(name="Support Goal") +
  labs(y="SES Index",x="",
       caption=str_wrap("Plot shows average SES index values with 5% to 95% bootstrapped CIs by reported support for protest goals for all 8,724 observations.",
                        60)) +
  coord_flip()

ggsave("plots/prot_goals_ses_wide.png",width=6,height=3)


# Wealth Spline model -----------------------------------------------------

# use brms for this one

library(brms)

if(run_model) {
  
  this_data <- filter(wide_action,outcome=="Protest",
                      treat_account!="Default") %>% 
    filter(!is.na(median))
  
  wealth_spline_mod <- 
    brm(formula = response ~ (1 | age) + sex + (1 | gov) + median*treat_account +
          I(median^2)*treat_account,
               family="gaussian",
               data = this_data,
               iter = 1000, chains = 1, cores = 10,
        threads=10,
        seed = 8540) 
  
  saveRDS(wealth_spline_mod, "data/wealth_spline_mod.rds")
} else {
  
  wealth_spline_mod <- readRDS("data/wealth_spline_mod.rds")
  
} 

# other outcome variables
# oppose bouteflika?

if(run_model) {
  
  this_data <- filter(wide_action,outcome=="Protest",
                      treat_account!="Default") %>% 
    filter(!is.na(median)) %>% 
    mutate(direct_1=factor(direct_1, levels=c("No","Yes")))
  
  dem_regime <- 
    brm(formula = direct_1 ~ (1 | age) + sex + (1 | gov) + median*treat_account,
        family="bernoulli",
        data = this_data,
        iter = 1000, chains = 1, cores = 10,
        threads=10,
        seed = 8540) 
  
  saveRDS(dem_regime, "data/dem_regime.rds")
} else {
  
  dem_regime <- readRDS("data/dem_regime.rds")
  
} 

if(run_model) {
  
  this_data <- filter(wide_action,outcome=="Protest",
                      treat_account!="Default") %>% 
    filter(!is.na(median)) %>% 
    mutate(actions_2=ordered(actions_2,
                             levels=c("Strongly oppose",
                                      "Oppose",
                                      "Neutral",
                                      "Support",
                                      "Strongly support")))
  
  biz_peeps <- 
    brm(formula = actions_2 ~ (1 | age) + sex + (1 | gov) + median*treat_account,
        family="cumulative",
        data = this_data,
        iter = 1000, chains = 1, cores = 10,
        threads=10,
        seed = 8540) 
  
  saveRDS(biz_peeps, "data/biz_peeps.rds")
} else {
  
  biz_peeps <- readRDS("data/biz_peeps.rds")
  
} 

# export model coefficients
library(modelsummary)
library(gt)
modelsummary(list(Quadratic=wealth_spline_mod,
                  Democracy=dem_regime,
                  Corruption=biz_peeps),
             statistic="conf.int",
             output="gt",
             metrics=c("R2","RMSE",'LOOIC'),
             coef_map=c("b_median"="SES",
                        "b_ImedianE2"="SES$^2$",
                        "b_treat_accounttreated_gas"="Fuel Subsidy",
                        "b_median:treat_accounttreated_gas" = "Fuel Subsidy X SES",
                        "b_treat_accounttreated_gas:ImedianE2"="Fuel Subsidy X SES$^2$",
                        "b_treat_accounttreated_tax"="VAT",
                        "b_median:treat_accounttreated_tax"="VAT X SES",
                        "b_treat_accounttreated_tax:ImedianE2"="VAT X SES$^2$",
                        "b_treat_accounttreated_within"="Govt. Revenue",
                        "b_median:treat_accounttreated_within"="Govt. Revenue X SES",
                        "b_treat_accounttreated_within:ImedianE2"="Govt. Revenue X SES$^2$"),
             notes="Table shows coefficients from a Bayesian regression models (OLS for Quadratic, logit for Democracy and ordered logit specifications for Business models) with default uninformative priors. Confidence intervals in brackets are the 5% to 95% posterior quantiles.",
             title="Coefficients for Quadratic Interaction of SES and Experimental Treatments") %>% 
  tab_options(table.font.size = gt::px(6)) %>% 
  gtsave("tables/SES_quadratic.tex")

