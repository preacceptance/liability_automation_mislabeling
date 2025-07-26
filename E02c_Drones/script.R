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
               'lavaan',
               'semTools'
)

# PROCESS Analysis (Set TRUE if you wish to run PROCESS code)
mediation <- F
if(mediation) {
  source("../process.R")
}

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

nrow(d)

## Comp Check
d |>
  filter(comp_1 == 1 & comp_2 == 1) -> d

nrow(d)

## ================================================================================================================
##                                                 DEMOGRAPHICS                 
## ================================================================================================================

prop.table(table(d[d$gender == 1 | d$gender == 2,]$gender))[[1]]
mean(d$age)

table(d$label)

## ================================================================================================================
##                                                 ANALYSIS                
## ================================================================================================================

t.test(d[d$label == "Autopilot",]$automation_1,
       d[d$label == "Copilot",]$automation_1)

sd(d[d$label == "Autopilot",]$automation_1)
sd(d[d$label == "Copilot",]$automation_1)

cohen.d(d[d$label == "Autopilot",]$automation_1,
        d[d$label == "Copilot",]$automation_1)

cronbach.alpha(d[,c("software_liable_1", "software_resp_1")])
cronbach.alpha(d[,c("human_liable_1", "human_resp_1" )])

## Discriminant Validity
## Reverse Coding Human
d |> mutate(
  hr = -(`human_resp_1` - 100),
  hl = -(`human_liable_1` - 100),
  fr = `software_resp_1`,
  fl = `software_liable_1`
) -> d

countf.model <- ' firm   =~ fr + fl
                  human  =~ hr + hl '

htmt(countf.model, d)

## Covariance Matrix
countf.cov <- cov(d[, c("fr", "fl", "hr", "hl")])

## HTMT using arithmetic mean
htmt(countf.model, sample.cov = countf.cov, htmt2 = FALSE)

d$human <- (d$human_liable_1 + d$human_resp_1) / 2
d$firm <-(d$software_liable_1 + d$software_resp_1) / 2

### Firm Liability

t.test(d[d$label == "Autopilot",]$firm,
       d[d$label == "Copilot",]$firm)

sd(d[d$label == "Autopilot",]$firm)
sd(d[d$label == "Copilot",]$firm)

cohen.d(d[d$label == "Autopilot",]$firm,
        d[d$label == "Copilot",]$firm)

### Human Liability 

t.test(d[d$label == "Autopilot",]$human,
       d[d$label == "Copilot",]$human)

sd(d[d$label == "Autopilot",]$human)
sd(d[d$label == "Copilot",]$human)

cohen.d(d[d$label == "Autopilot",]$human,
        d[d$label == "Copilot",]$human)

## ================================================================================================================
##                                              PROCESS             
## ================================================================================================================


d$cond <- as.numeric(as.factor(d$label))

if(mediation) {
  process(data = d, y = "firm", x = "cond", 
          m =c("automation_1"), model = 4, effsize = 1, total = 1, stand = 1, 
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
  
  process(data = d, y = "human", x = "cond", 
          m =c("automation_1"), model = 4, effsize = 1, total = 1, stand = 1, 
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
}

## ================================================================================================================
##                                              DATA VIZUALIZATION              
## ================================================================================================================

# Renaming and labeling for plots
d |>
  select(cond,label, automation_1, firm, human) |>
  mutate(
    `Label` = ifelse(cond == 1, "Autopilot", "Copilot"),
    `Firm Liability` = firm,
    `Human Liability` = human
  ) |>
  select(`Label`, `Firm Liability`, `Human Liability`)  -> d_plot

# Obtain mean and standard errors for condition and measure
d_plot |>
  gather(key = "DV", value = "Value", `Firm Liability`, `Human Liability`) |>
  dplyr::group_by(`Label`, DV) |>
  dplyr::summarize(
    avg_value = mean(Value),
    se_value = sd(Value)/sqrt(n())
  ) -> d_plot2

se_width <- 1.96

# Plot Firm Liability
ggplot(data = d_plot2, aes(fill=`Label`, y=avg_value, x = DV)) +
  geom_bar(stat="identity", position="dodge", alpha=.75, width=.6) +
  geom_point(position=position_dodge(width = .6), size=.5, color="black") +
  geom_errorbar(aes(ymin=avg_value-(se_value*se_width), ymax=avg_value+(se_value*se_width)), position = position_dodge(width=.6), 
                size=.25, color="black", width=.25) +
  geom_signif(
    y_position = c(90), xmin = c(0.85, 1.85), xmax = c(1.15, 2.15),
    annotation = c("***","***"), tip_length = 0.1, color='black', size = .25, textsize = 3.5 
  ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12), 
        axis.title=element_text(size=10,face="bold"), legend.position = "top") +
  ylab("Mean Ratings") +
  xlab("") +
  ggtitle("") +
  scale_fill_grey() +
  scale_color_grey() +
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100))-> p1

p1

ggsave("liability.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")
