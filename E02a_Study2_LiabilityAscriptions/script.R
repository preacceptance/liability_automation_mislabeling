# clear workspace
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
               'effects',
               'lavaan',
               'semTools'
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

# Discriminant Validity
# Reverse Coding Human Responsibility/Liability

d <- d_merged
d |> mutate(
  hr = -(100 - `human responsibility`),
  hl = -(100 - `human liability`),
  fr = `software responsibility`,
  fl = `firm liability`
) -> d

countf.model <- ' firm   =~ fr + fl
                  human  =~ hr + hl '

htmt(countf.model, d)

## Covariance Matrix
countf.cov <- cov(d[, c("fr", "fl", "hr", "hl")])

## HTMT using arithmetic mean
htmt(countf.model, sample.cov = countf.cov, htmt2 = FALSE)

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

d_merged |>
  mutate(
    Label = ifelse( cond == 1, "Autopilot", "Copilot"),
    `Firm Liability` = firm,
    Capability = automation
  ) -> d_plot

ggplot(d_plot, aes(x = Label, y = `Firm Liability`)) +
  stat_summary(fun = mean, geom = "bar",, alpha = 0.5) +  # Bar plot
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5) +   # Error bars
  geom_jitter(width = 0.2, alpha = 0.15, size = .1, color = "#00003B") +  # Individual data points
  scale_fill_grey() +
  scale_color_grey() +
  theme_classic() + 
  geom_signif(comparisons = list(c("Copilot", "Autopilot")), map_signif_level = TRUE, , test = "t.test") +
  theme(text = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, size=10)) +
  xlab("") +
  ylab("") +
  ggtitle("Firm Liability") -> p1

p1

ggplot(d_plot, aes(x = Label, y = `Capability`)) +
  stat_summary(fun = mean, geom = "bar",, alpha = 0.5) +  # Bar plot
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5) +   # Error bars
  geom_jitter(width = 0.2,  alpha = 0.15, size = .1, color = "#00003B" , height = 0.1) +  # Individual data points
  scale_fill_grey() +
  scale_color_grey() +
  theme_classic() + 
  geom_signif(comparisons = list(c("Copilot", "Autopilot")), map_signif_level = TRUE, test = "t.test", y_position = 6.5) +
  theme(text = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, size=10)) +
  xlab("") +
  ylab("") +
  ggtitle("Perceived Level of Capability") -> p2

p2

p <- ggarrange(p2, p1, nrow = 1, ncol = 2)

annotate_figure(p,
                left = text_grob("Ratings", color = "black", face = "bold", size = 10, rot = 90, vjust = 2.2, hjust = .2),
                bottom = text_grob("Label", size = 10, vjust = -1.8, face = "bold"))

ggsave("LiabilityAscriptions.png", device = "png",width = 8.3, height = 4.3, units = "in")

## ================================================================================================================
##                                              DATA VIZUALIZATION (OLD)             
## ================================================================================================================

# Renaming and labeling for plots
d_merged |>
  select(cond, automation, firm, human) |>
  mutate(
    `Label` = ifelse(cond == 1, "Autopilot", "Copilot"),
    `Firm Liability` = firm,
    `Human Liability` = human
  ) |>
  select(`Label`, `Firm Liability`, `Human Liability`) |>
  gather(key = "DV", value = "Value", `Firm Liability`, `Human Liability`) -> d_plot

# Obtain mean and standard errors for condition and measure
d_plot |>
  group_by(`Label`, DV) |>
  summarize(
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

ggsave("liability_ascriptions.png", device = "png",width = 5.3, height = 3.7, units = "in")

## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================

