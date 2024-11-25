## clear workspace
rm(list = ls())

## install packages
library(dplyr)
library(grid)
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('tidyverse',       
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',           
               'tidyr',           # tools for cleaning messy data
               'stringr',         # perform string substitutions easily
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'emmeans',         # contrast analysis for regression models
               'stats',           # use function to adjust for multiple comparisons
               'filesstrings',    # create and move files
               'simr',            # power analysis for mixed models
               'compute.es',      # effect size package
               'effsize',         # another effect size package
               'pwr',             # package for power calculation
               'Hmisc',            # get p values for mixed effect model
               'DescTools',        # get Cramer's V
               'rstatix',
               'effects',
)

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read_csv('data.csv')
d <- d[-c(1,2),]

d |>
  mutate_if(all.is.numeric, as.numeric) -> d

## Attention Check
d |>
  filter(att_1 == 2 & att_2 == 5) -> d

## Comp Check
d |>
  filter(comp_1 == 2 & comp_2 == 4) -> d

nrow(d)

## ================================================================================================================
##                                                 DEMOGRAPHICS                 
## ================================================================================================================

prop.table(table(d[d$gender == 1 | d$gender == 2,]$gender))[[1]]
mean(d$age)


## ================================================================================================================
##                                                 ANALYSIS                
## ================================================================================================================

## Correct Level of Automation
sum(d$automation == 2)
sum(d$automation == 2)/(length(d$automation))

## Higher than Level 2
sum(d$automation > 2)
sum(d$automation > 2)/(length(d$automation))

## t-test against NULL == 2
t.test(d$automation, mu = 2)
sd(d$automation)

## Aware of Autopilot
t.test(d[d$aware == 2, ]$automation,
       d[d$aware == 3, ]$automation)

## Used Autopilot
t.test(d[d$used == 2, ]$automation,
       d[d$used == 3, ]$automation)

## Made Purchase Themselves
t.test(d[d$purchase == 1, ]$automation,
       d[d$purchase == 2, ]$automation)

## Is the Primary Driver 
t.test(d[d$drive == 1, ]$automation,
       d[d$drive == 2, ]$automation)

## ================================================================================================================
##                                              DATA VIZUALIZATION              
## ================================================================================================================

d |>
  group_by(automation) |>
  dplyr::summarize(count = n()) |>
  mutate(`Level of Automation` = automation) -> d_plot

## Participant Responses
ggplot(data = d_plot, aes(x=`Level of Automation`, y = count)) +
  geom_bar(stat="identity", position="dodge", alpha = .75) +
  scale_fill_grey() +
  scale_color_grey() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10), legend.position = "top",
        axis.title = element_text(face="bold"), text = element_text(face="bold", size=10)) +
  scale_x_discrete(name ="Level of Automation", limits = 1:6)  +
  ylab("Number of Responses") -> p

p

ggsave("participants_responses.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")
