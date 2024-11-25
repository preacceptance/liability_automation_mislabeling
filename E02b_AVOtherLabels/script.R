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
mediation <- FALSE
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

## Comp Check
d |>
  filter(comp_1 == 2 & comp_2 == 4 & comp_3 == 1) -> d

nrow(d)

## ================================================================================================================
##                                                 DEMOGRAPHICS                 
## ================================================================================================================

prop.table(table(d[d$gender == 1 | d$gender == 2,]$gender))[[1]]
mean(as.numeric(d$age), na.rm = T)

table(d$label)

## ================================================================================================================
##                                                 ANALYSIS                
## ================================================================================================================

## Capability 
t.test(d[d$label == "Full Self-Driving",]$automation,
       d[d$label == "Lane Sense",]$automation)

sd(d[d$label == "Full Self-Driving",]$automation)
sd(d[d$label == "Lane Sense",]$automation)

cohen.d(d[d$label == "Full Self-Driving",]$automation,
        d[d$label == "Lane Sense",]$automation)

cronbach.alpha(d[,c("firm_resp_1", "firm_liable_1")])
cronbach.alpha(d[,c("human_liable_1", "human_resp_1" )])

## Discriminant Validity
## Reverse Coding Human
d |> mutate(
  hr = -(`human_resp_1` - 100),
  hl = -(`human_liable_1` - 100),
  fr = `firm_resp_1`,
  fl = `firm_liable_1`
) -> d

countf.model <- ' firm   =~ fr + fl
                  human  =~ hr + hl '

htmt(countf.model, d)

## Covariance Matrix
countf.cov <- cov(d[, c("fr", "fl", "hr", "hl")])

## HTMT using arithmetic mean
htmt(countf.model, sample.cov = countf.cov, htmt2 = FALSE)

d$human <- (d$human_liable_1 + d$human_resp_1) / 2
d$firm <-(d$firm_liable_1 + d$firm_resp_1) / 2

### Firm Liability

t.test(d[d$label == "Full Self-Driving",]$firm,
       d[d$label == "Lane Sense",]$firm)

sd(d[d$label == "Full Self-Driving",]$firm)
sd(d[d$label == "Lane Sense",]$firm)

cohen.d(d[d$label == "Full Self-Driving",]$firm,
        d[d$label == "Lane Sense",]$firm)

### Human Liability 

t.test(d[d$label == "Full Self-Driving",]$human,
       d[d$label == "Lane Sense",]$human)

sd(d[d$label == "Full Self-Driving",]$human)
sd(d[d$label == "Lane Sense",]$human)

cohen.d(d[d$label == "Full Self-Driving",]$human,
        d[d$label == "Lane Sense",]$human)

## ================================================================================================================
##                                              PROCESS             
## ================================================================================================================

d$cond <- as.numeric(as.factor(d$label))

if(mediation) {
  process(data = d, y = "firm", x = "cond", 
          m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321, conf = 97.06)
  
  process(data = d, y = "human", x = "cond", 
          m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321, conf = 97.06)
}

## ================================================================================================================
##                                              DATA VIZUALIZATION              
## ================================================================================================================

# Renaming and labeling for plots
d |>
  select(cond,label, automation, firm, human) |>
  mutate(
    `Label` = ifelse(cond == 1, "Full Self-Driving", "Lane Sense"),
    `Firm Liability` = firm,
    `Human Liability` = human
  ) |>
  select(`Label`, `Firm Liability`, `Human Liability`) |>
  gather(key = "DV", value = "Value", `Firm Liability`, `Human Liability`) -> d_plot

# Obtain mean and standard errors for condition and measure
d_plot |>
  dplyr::group_by(`Label`, DV) |>
  dplyr::summarize(
    avg_value = mean(Value),
    se_value = sd(Value)/sqrt(n())
  ) -> d_plot

se_width <- 1.96

# Plot Firm Liability
ggplot(data = d_plot, aes(fill=`Label`, y=avg_value, x = DV)) +
  geom_bar(stat="identity", position="dodge", alpha=.75, width=.6) +
  geom_point(position=position_dodge(width = .6), size=.5, color="black") +
  geom_errorbar(aes(ymin=avg_value-(se_value*se_width), ymax=avg_value+(se_value*se_width)), position = position_dodge(width=.6), 
                size=.25, color="black", width=.25) +
  geom_signif(
    y_position = c(95), xmin = c(0.85, 1.85), xmax = c(1.15, 2.15),
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
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) -> p1

p1

ggsave("liability.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")
