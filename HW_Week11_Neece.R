## Week 11 Assignment ## 
## Jillian Neece ##

## The following simulation data represents the seed delivery (in seeds per meter-square) 
## at increasing distances (m) from a forest edge. Two conifer seed species were collected
## in seed traps (0 = lodgepole pine, 1 = Douglas-fir). 
## 
## Ecological Question: Does seed delivery by distance from a forest edge differ 
## between lodgepole pine and Douglas-fir seeds?

library(tidyverse)
library(car)
library(emmeans)

# simulating random linear-ish data
distance.to.seed.source.m <- rep(seq(0, 15, length.out = 50), 2)
set.seed(10)
error.1 <- rlnorm(100)
species <- c(rep(0, 50), rep(1, 50))

seed.delivery <- -1.5*distance.to.seed.source.m + 9.1*species + error.1 + 
  0.43*distance.to.seed.source.m*species + 22

## Is the interaction between x.num and x.cat significant?
mod <- lm(seed.delivery ~ distance.to.seed.source.m + species)
summary(mod)
Anova(mod, type = 3)
emmeans(mod, ~ distance.to.seed.source.m | species)


## putting simulated data into a dataframe to export and send to Rebekkah
seed.simulation <- data.frame(distance.to.seed.source.m, 
                              species, 
                              seed.delivery)
#write.csv(seed.simulation, "Data/Neece_seed.simulation.csv")

## Objective 2 ##

## Reading in Rebekkah's data
birdsong <- read.csv("LaBlue_birdsong_HW11.csv")
head(birdsong)

## Birds worldwide exhibit substantial diversity in their songs, with repertoire 
## size in particular varying widely across ecological and social contexts. 
## Species also differ in how they acquire song: either innately or as a learned 
## behavior from conspecifics. Because anthropogenic noise pollution can influence 
## bird vocal behavior, it may also influence the evolution and/or expression of 
## song complexity. (0 = innate, 1 = social)

## Ecological Question: Does the relationship between an individual's repertoire 
## size (total number of distinct phrases) and ambient noise level of its 
## breeding territory (average dB) differ between species that acquire their 
## songs innately versus socially?

## First, I'm checking if the interaction between avg_dB and acquisition_method 
## is significant. 
birdsong.interaction.mod <- lm(total_phrases ~ avg_db*acquisition_method,
                               data = birdsong)
Anova(birdsong.interaction.mod, type = 3)
summary(birdsong.interaction.mod)

## We have a significant interaction! Since the interaction is significant, I'll 
## leave all the other terms in the model as well (the other terms are also significant
## on their own, but even if they weren't I would include them since the interaction
## is significant). 

## The model shows that the effect of ambient noise level of the environment on 
## an individual bird's song repertoire is dependent on how the individual acquires
## their songs. When songs are acquired socially, an increase of 1 dB of ambient 
## noise level is associated with an average decrease of 0.3 phrases, whereas songs
## acquired innately would see a greater average decline of 0.7 phrases with the
## same increase of 1 dB. There is also a difference in intercept in the model, 
## implying that when there is no ambient noise in the breeding territory, we 
## would still expect to see differences between the number of phrases in birds 
## that acquire their songs socially (~5 phrases) and those who acquire songs 
## innately (~4.7 phrases on average). 

