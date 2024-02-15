## ================================================================================================================
##                                DATA ANALYSIS | AV Label STUDY | EXPERIMENT 2               
## ================================================================================================================
## clear workspace
rm(list = ls()) 

## install packages
library(dplyr)
library(grid)
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',         # plotting
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
               'nlme',            # get p values for mixed effect model
               'DescTools',        # get Cramer's V
               'rstatix',
               'effects'
)

# PROCESS Analysis (Set TRUE if you wish to run PROCESS code)
mediation <- FALSE

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.csv('data.csv') 
if(mediation) {
  source("../process.R")
}

## rename colnames
## and change condition entries
d |>
  rename(
    cond = FL_12_DO,
    auto_resp_human = resp_human1_10,
    auto_resp_software = resp_software1_10,
    co_resp_human = resp_human2_10,
    co_resp_software = resp_software2_10,
    auto_liab_human = liable_human1_1,
    auto_liab_firm = liable_firm1_1,
    co_liab_human = liab_human2_1,
    co_liab_firm = liab_firm2_1,
  ) |>
  mutate(cond = ifelse(cond == "FL_35", "auto", "co")) -> d

## subjects randomized:
table(d$cond)

## ================================================================================================================
##                                                   EXCLUSIONS                
## ================================================================================================================

## incomplete responses
d <- subset(d, (d$Finished == 1))
## attention exclusions: 
# remove responses from data frame that failed attention checks
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))
n_original <- dim(d)[1]
n_original

## comprehension exclusions: 
# remove responses from data frame that failed comprehension checks
d <- subset(d, (d$comp_1 == 2 & d$comp_2 == 4))
d <- subset(d, (d$comp_3 == 2 | d$comp_4 == 1))
## number of participants AFTER exclusions: 
n_final <- dim(d)[[1]] # extracting number of rows only, not columns
n_final 

# n_excluded
n_excluded <- n_original - n_final; n_excluded

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

d <- d %>% relocate(co_1, .after = auto_1)
d <- d %>% relocate(co_resp_software, .after = auto_resp_software)
d <- d %>% relocate(co_resp_human, .after = auto_resp_human)
d <- d %>% relocate(co_liab_firm, .after = auto_liab_firm)
d <- d %>% relocate(co_liab_human, .after = auto_liab_human)

## new data frame to extract pre-processed data into:
d_subset <- array(dim=c(dim(d)[1], 6))
colnames(d_subset) <- c('cond','automation','software responsibility','human responsibility','firm liability','human liability')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE) 

## extract data of interest from middle part of raw data:
for(i in 1:dim(d)[1]) {
  pref1 <- d[i,23:24][!is.na(d[i,23:24])] # for a given row, get only the non-NA values
  d_subset[i,2] <- as.numeric(pref1[pref1!= ""]) # and only the non-empty values
  resp1 <- d[i,25:26][!is.na(d[i,25:26])]
  d_subset[i,3] <- as.numeric(resp1[resp1!= ""])
  resp2 <- d[i,27:28][!is.na(d[i,27:28])]
  d_subset[i,4] <- as.numeric(resp2[resp2!= ""])
  liab1 <- d[i,29:30][!is.na(d[i,29:30])]
  d_subset[i,5] <- as.numeric(liab1[liab1!= ""])
  liab2 <- d[i,31:32][!is.na(d[i,31:32])]
  d_subset[i,6] <- as.numeric(liab2[liab2!= ""])
  d_subset[i,1] <- d[i,49][!is.na(d[i,49])] 
}

## merge data of interest back with raw data:
# new data frame to work with
d_merged <- cbind(d_subset, d[,38:46])
d_merged$ss <- 1:dim(d_merged)[1]

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
mean(d_merged$age, trim = 0, na.rm = TRUE) ## mean age 

## gender
gender <- ifelse(d$gender == 1, "Male", "Female")
prop.table(table(gender))["Male"]

rm(d, d_subset)

## ================================================================================================================
##                                              DATA ANALYSIS               
## ================================================================================================================

# Cronbach Alpha of Human and Firm Measure
cronbach.alpha(d_merged[,c("software responsibility", "firm liability")])
cronbach.alpha(d_merged[,c("human responsibility", "human liability")])

# Create composite measure and make condition as factor
d_merged |>
  mutate(firm = (`software responsibility` + `firm liability`) / 2,
         human = (`human responsibility` + `human liability`) / 2, 
         cond = as.factor(cond)) -> d_merged

# t-tests
## Perceived Level of Automation
t.test(automation ~ cond, d_merged)

sd(d_merged[d_merged$cond == "auto",]$automation)
sd(d_merged[d_merged$cond == "co",]$automation)

cohen.d(d_merged[d_merged$cond == "auto",]$automation,
        d_merged[d_merged$cond == "co",]$automation)

## Firm Liability
t.test(firm ~ cond, d_merged)

sd(d_merged[d_merged$cond == "auto",]$firm)
sd(d_merged[d_merged$cond == "co",]$firm)

cohen.d(d_merged[d_merged$cond == "auto",]$firm,
        d_merged[d_merged$cond == "co",]$firm)

## Human Liability
t.test(human ~ cond, d_merged)

sd(d_merged[d_merged$cond == "auto",]$human)
sd(d_merged[d_merged$cond == "co",]$human)

cohen.d(d_merged[d_merged$cond == "auto",]$human,
        d_merged[d_merged$cond == "co",]$human)

## ================================================================================================================
##                                          MEDIATION ANALYSIS               
## ================================================================================================================
d_merged$cond <- as.numeric(d_merged$cond)

## Simple Mediation
if(mediation) {
process(data = d_merged, y = "firm", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

process(data = d_merged, y = "human", x = "cond", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
}

## ================================================================================================================
##                                              DATA VIZUALIZATION              
## ================================================================================================================

# Renaming and labeling for plots
d_merged |>
  select(cond, automation, firm, human) |>
  mutate(
    `Marketing Label` = ifelse(cond == 1, "Autopilot", "Copilot"),
    Firm = firm,
    Human = human
  ) |>
  select(`Marketing Label`, Firm, Human) -> d_plot

# Obtain mean and standard errors for condition and measure
d_plot |>
  group_by(`Marketing Label`) |>
  summarize(
    avg_F = mean(Firm),
    avg_H = mean(Human),
    se_F = sd(Firm)/sqrt(n()),
    se_H = sd(Human)/sqrt(n())
  ) -> d_plot

se_width <- 1.96

# Plot Firm Liability
ggplot(data = d_plot, aes(x=factor(`Marketing Label`, level = c("Autopilot", "Copilot")), y=avg_F)) +
  geom_bar(stat="identity", alpha=.75) +
  geom_point(size=.75, color="black") +
  geom_errorbar(aes(ymin=avg_F-(se_F*se_width), ymax=avg_F+(se_F*se_width)), position = "dodge", 
                size=.25, color="black", width=.75) +
  geom_signif(
    y_position = c(100), xmin = c("Autopilot"), xmax = c( "Copilot"),
    annotation = c("***"), tip_length = 0.1, color='black', size = .5, textsize = 3.5
  ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12), 
        axis.title=element_text(size=10,face="bold")) +
  ylab("Mean Ratings") +
  xlab("") +
  ggtitle("Firm Liability") -> p1

p1

# Plot Human Liability
ggplot(data = d_plot, aes(x=factor(`Marketing Label`, level = c("Autopilot", "Copilot")), y=avg_H)) +
  geom_bar(stat="identity", alpha=.75) +
  geom_point(size=.75, color="black") +
  geom_errorbar(aes(ymin=avg_H-(se_H*se_width), ymax=avg_H+(se_H*se_width)), position = "dodge", 
                size=.25, color="black", width=.75) +
  geom_signif(
    y_position = c(100), xmin = c("Autopilot"), xmax = c( "Copilot"),
    annotation = c("*"), tip_length = 0.1, color='black', size = .5, textsize = 3.5
  ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12),
        axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ylab("") +
  xlab("") +
  ggtitle("Human Liability") -> p2

p2

# Combine both figures
ggarrange(p1,p2) |>
  annotate_figure(bottom = textGrob("Marketing Label", gp = gpar(cex = 1, fontsize=10, fontface="bold")))

ggsave("liability_ascriptions.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================

