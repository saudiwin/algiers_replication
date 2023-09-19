# Response to Reviewers
# World Politics
# Feb 27, 2023

library(dplyr)
library(ggplot2)
library(stringr)


set.seed(20230227)


# Setup -------------------------------------------------------------------

# what is true effect size? N(0.1,1) - N(0,1) per pre-reg
# assume baseline rate of 0.4, treatment then moves to 0.5 only in 
# subgroup of interest
# divide treatment condition into 1/2
# assume that noise = 1 as in original power calculation

# set up random partitions, see how many sig effects we get

over_sets_N <- parallel::mclapply(50:3000, function(i) {
  
  # get five estimates per N
  
    lapply(1:5, function(it) {
      
      N <- i
      
      # subgroup of interest
      assign <- runif(N)<.25
      treatment <- ifelse(assign, 0.4, 0.5)
      
      Y <- rnorm(N, treatment)
      
      # second group is random partition
      
      assign2 <- as.numeric(runif(N)<.25)
      
      check_sets <- summary(lm(Y ~ assign + assign2))
      
      tibble(coef1=check_sets$coefficients[2,'Estimate'],
             p_value1=check_sets$coefficients[2,'Pr(>|t|)'],
             coef2=check_sets$coefficients[3,'Estimate'],
             p_value2=check_sets$coefficients[3,'Pr(>|t|)'],
             iter=it,
             N=i)
      
      
    }) %>% bind_rows
  
  },mc.cores=parallel::detectCores()) %>% bind_rows

# plot convergence to true estimated effect

over_sets_N %>% 
  filter(p_value1<0.05) %>% 
  ggplot(aes(y=coef1,
             x=N)) +
  geom_point() +
  labs(y="Estimated Coefficient",
       x="Sample Size",
       caption=str_wrap("Plot shows simulation draws with values of statistically significant effects of true treatment sub-group. Horizontal line denotes the location of the true effect size (- 0.1).")) +
  guides(linetype="none") +
  geom_smooth() +
  annotate(x=500,y=-0.05,label="True Effect Size",geom="text") +
  geom_hline(aes(yintercept=c(-0.1)),linetype=2) +
  theme_minimal()

ggsave("plots/converge_true.pdf")

# how many effects are sig for random subgroup?

mean(over_sets_N$p_value2<0.05)

# plot as function of N
# keep only ones where random group is significant
over_sets_N %>% 
  filter(p_value2<0.05) %>% 
  mutate(negative=coef2<0) %>% 
  ggplot(aes(y=coef2,
           x=N)) +
  geom_point() +
  labs(y="Estimated Coefficient",
       x="Sample Size",
       caption=str_wrap("Plot shows simulation draws with values of statistically significant effects of random (spurious) sub-groups. Horizontal line denotes the location of the true effect size of 0.")) +
  guides(linetype="none") +
  geom_smooth(aes(linetype=negative)) +
  annotate(x=500,y=-0.05,label="True Effect Size",geom="text") +
  geom_hline(aes(yintercept=0),linetype=2) +
  theme_minimal()
  
ggsave("plots/effect_size_false_pos.pdf")

# now see what largest effect we can get from 100k simulation draws
# with an N of 3000 (subgroup of interest = 1/2 T or 750)
# this will take some time to run

over_sets_big <- parallel::mclapply(1:100000, function(i) {
  
  # assign some other units to a group that were not in the subgroup of interest
  
  N <- 3000
  assign <- runif(N)<.25
  treatment <- ifelse(assign, 0.4, 0.5)
  
  Y <- rnorm(N, treatment)
  
  # second group is random partition
  
  assign2 <- as.numeric(runif(N)<.25)
  
  check_sets <- summary(lm(Y ~ assign + assign2))
  
  tibble(coef1=check_sets$coefficients[2,'Estimate'],
         p_value1=check_sets$coefficients[2,'Pr(>|t|)'],
         coef2=check_sets$coefficients[3,'Estimate'],
         p_value2=check_sets$coefficients[3,'Pr(>|t|)'],
         iter=i)
  
  
},mc.cores=parallel::detectCores()) %>% bind_rows


ggplot(filter(over_sets_big,p_value2<0.05),
       aes(x=coef2)) +
  geom_histogram() +
  geom_vline(xintercept=0.1,linetype=2, colour="red") +
  geom_vline(xintercept=-0.1,linetype=2, colour="red") +
  theme_minimal() +
  labs(y="Estimated Coefficient",x="",
       caption=str_wrap("Plot shows 100,000 simulation draws of the effect of a random (spurious) sub-group. Red line denotes the location of the true treatment effect size (+/- 0.1).")) 

ggsave("plots/big_sim.pdf")



