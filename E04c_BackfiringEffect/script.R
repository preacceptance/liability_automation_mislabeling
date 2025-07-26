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
               'effects',
               'lavaan',
               'semTools',
               "Hmisc",
               'sjstats'
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
d <- read.csv('data.csv') 
d <- d[-c(1:2),]

## ================================================================================================================
##                                                   EXCLUSIONS                
## ================================================================================================================

d |>
  mutate_if(all.is.numeric, as.numeric) -> d

## Attention Check
d |>
  filter(att_1 == 2 & att_2 == 5) -> d

## Comp Check
d |>
  filter(comp_1 == 2 & comp_2 == 4 & comp_3 == 1) -> d

d |>
  filter(comp_4 == 1 | is.na(comp_4)) -> d

nrow(d) # Final Sample

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
mean(d$age, trim = 0, na.rm = TRUE) ## mean age 

## gender
prop.table(table(d[d$gender == 1 | d$gender == 2,]$gender))[[1]]

table(d$ad, d$label)

## ================================================================================================================
##                                              DATA ANALYSIS               
## ================================================================================================================

# Cronbach Alpha of Human and Firm Measure
cronbach.alpha(d[,c("firm_resp_1", "firm_liable_1")])
cronbach.alpha(d[,c("human_resp_1", "human_liable_1")])

# Discriminant Validity
# Reverse Coding Human Responsibility/Liability
d_merged <- d
d_merged |> mutate(
  hr = -(`human_resp_1`-100),
  hl = -(`human_liable_1` - 100),
  fl = `firm_resp_1`,
  fr = `firm_liable_1`
) -> d_merged

countf.model <- ' firm   =~ fr + fl
                  human  =~ hr + hl '

htmt(countf.model, d_merged)

## Covariance Matrix
countf.cov <- cov(d_merged[, c("fr", "fl", "hr", "hl")])

## HTMT using arithmetic mean
htmt(countf.model, sample.cov = countf.cov, htmt2 = FALSE)

## Composite measure
d |> 
  mutate(
    human = (human_resp_1 + human_liable_1)/2,
    firm = (firm_liable_1 + firm_resp_1)/2
  ) -> d

# CAPABILITY
a <- aov(automation ~ as.factor(label) * as.factor(ad), data = d)
summary(a)
anova_stats(a); anova_stats(a)$partial.etasq

## Label Effect
t.test(d[d$label == 'Autopilot',]$automation,
       d[d$label == 'Copilot',]$automation)

sd(d[d$label == 'Autopilot',]$automation)
sd(d[d$label == 'Copilot',]$automation)

cohen.d(d[d$label == 'Autopilot',]$automation,
       d[d$label == 'Copilot',]$automation)

## Safety Benefits Effect
t.test(d[d$ad == 'yes',]$automation,
       d[d$ad == 'no',]$automation)

sd(d[d$ad == 'yes',]$automation)
sd(d[d$ad == 'no',]$automation)

cohen.d(d[d$ad == 'yes',]$automation,
       d[d$ad == 'no',]$automation)

## Absent Condition
t1 <- t.test(d[d$ad == 'no' & d$label == 'Autopilot',]$automation,
             d[d$ad == 'no' & d$label == 'Copilot',]$automation, paired = FALSE)
t1

## Present Condition
t2 <- t.test(d[d$ad == 'yes' & d$label == 'Autopilot',]$automation,
             d[d$ad == 'yes' & d$label == 'Copilot',]$automation, paired = FALSE)
t2

# FIRM
a <- aov(firm ~ as.factor(label) * as.factor(ad), data = d)
summary(a)
anova_stats(a); anova_stats(a)$partial.etasq

## Label Effect
t.test(d[d$label == 'Autopilot',]$firm,
       d[d$label == 'Copilot',]$firm)

sd(d[d$label == 'Autopilot',]$firm)
sd(d[d$label == 'Copilot',]$firm)

cohen.d(d[d$label == 'Autopilot',]$firm,
       d[d$label == 'Copilot',]$firm)

## Safety Benefits Effect
t.test(d[d$ad == 'yes',]$firm,
       d[d$ad == 'no',]$firm)

sd(d[d$ad == 'yes',]$firm)
sd(d[d$ad == 'no',]$firm)

cohen.d(d[d$ad == 'yes',]$firm,
       d[d$ad == 'no',]$firm)

## Absent Condition
t1 <- t.test(d[d$ad == 'no' & d$label == 'Autopilot',]$firm,
             d[d$ad == 'no' & d$label == 'Copilot',]$firm, paired = FALSE)
t1

sd(d[d$ad == 'no' & d$label == 'Autopilot',]$firm)
sd(d[d$ad == 'no' & d$label == 'Copilot',]$firm)

cohen.d(d[d$ad == 'no' & d$label == 'Autopilot',]$firm,
       d[d$ad == 'no' & d$label == 'Copilot',]$firm)

## Present Condition
t2 <- t.test(d[d$ad == 'yes' & d$label == 'Autopilot',]$firm,
             d[d$ad == 'yes' & d$label == 'Copilot',]$firm, paired = FALSE)
t2

sd(d[d$ad == 'yes' & d$label == 'Autopilot',]$firm)
sd(d[d$ad == 'yes' & d$label == 'Copilot',]$firm)

cohen.d(d[d$ad == 'yes' & d$label == 'Autopilot',]$firm,
        d[d$ad == 'yes' & d$label == 'Copilot',]$firm)

# Human
a <- aov(human ~ as.factor(label) * as.factor(ad), data = d)
summary(a)
anova_stats(a); anova_stats(a)$partial.etasq

## Label Effect
t.test(d[d$label == 'Autopilot',]$human,
       d[d$label == 'Copilot',]$human)

## Safety Benefits Effect
t.test(d[d$ad == 'yes',]$human,
       d[d$ad == 'no',]$human)

sd(d[d$ad == 'yes',]$human)
sd(d[d$ad == 'no',]$human)

cohen.d(d[d$ad == 'yes',]$human,
       d[d$ad == 'no',]$human)

## Absent Condition
t1 <- t.test(d[d$ad == 'no' & d$label == 'Autopilot',]$human,
             d[d$ad == 'no' & d$label == 'Copilot',]$human, paired = FALSE)
t1

sd(d[d$ad == 'no' & d$label == 'Autopilot',]$human)
sd(d[d$ad == 'no' & d$label == 'Copilot',]$human)
       
cohen.d(d[d$ad == 'no' & d$label == 'Autopilot',]$human,
       d[d$ad == 'no' & d$label == 'Copilot',]$human)

## Present Condition
t2 <- t.test(d[d$ad == 'yes' & d$label == 'Autopilot',]$human,
             d[d$ad == 'yes' & d$label == 'Copilot',]$human, paired = FALSE)
t2

sd(d[d$ad == 'yes' & d$label == 'Autopilot',]$human)
sd(d[d$ad == 'yes' & d$label == 'Copilot',]$human)

cohen.d(d[d$ad == 'yes' & d$label == 'Autopilot',]$human,
        d[d$ad == 'yes' & d$label == 'Copilot',]$human)

## ================================================================================================================
##                                          MEDIATION ANALYSIS               
## ================================================================================================================

d_merged <- d
d_merged$label <- as.numeric(as.factor(d_merged$label))
d_merged$ad <- as.numeric(as.factor(d_merged$ad))

## Simple Mediation
if(mediation) {
process(data = d_merged, y = "firm", x = "label", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321, conf = 97.01)

process(data = d_merged, y = "human", x = "label", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321, conf = 97.01)
}

## Moderated Mediation
if(mediation) {
  process(data = d_merged, y = "firm", x = "label", 
          m =c("automation"), w = c("ad"), model = 7, effsize = 1, total = 1, stand = 1, 
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
  
  process(data = d_merged, y = "human", x = "label", 
          m =c("automation"), w = c("ad"), model = 7, effsize = 1, total = 1, stand = 1, 
          contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
}

## ================================================================================================================
##                                              DATA VIZUALIZATION              
## ================================================================================================================

d |>
  mutate(
    Advertisement = ifelse( ad == "no", "Absent", "Present"),
    `Firm Liability` = firm
  ) -> d_plot

ggplot(d_plot, aes(x = Advertisement, y = `Firm Liability`)) +
  stat_summary(fun = mean, geom = "bar",, alpha = 0.5) +  # Bar plot
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5) +   # Error bars
  geom_jitter(width = 0.2, alpha = 0.15, size = .1, color = "#00003B") +  # Individual data points
  scale_fill_grey() +
  scale_color_grey() +
  theme_classic() + 
  geom_signif(comparisons = list(c("Absent", "Present")), map_signif_level = TRUE, , test = "t.test") +
  theme(text = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, size=10)) +
  xlab("") +
  ylab("") +
  ggtitle("Firm Liability") -> p1

p1

ggplot(d_plot, aes(x = Advertisement, y = `automation`)) +
  stat_summary(fun = mean, geom = "bar",, alpha = 0.5) +  # Bar plot
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5) +   # Error bars
  geom_jitter(width = 0.2, alpha = 0.15, size = .1, height = 0.1, , color = "#00003B") +  # Individual data points
  scale_fill_grey() +
  scale_color_grey() +
  theme_classic() + 
  geom_signif(comparisons = list(c("Absent", "Present")), map_signif_level = TRUE, test = "t.test", y_position = 6.5) +
  theme(text = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, size=10)) +
  xlab("") +
  ylab("") +
  ggtitle("Perceived Level of Capability") -> p2

p2

p <- ggarrange(p2, p1, nrow = 1, ncol = 2)

annotate_figure(p,
                left = text_grob("Ratings", color = "black", face = "bold", size = 10, rot = 90, vjust = 2.2, hjust = .2),
                bottom = text_grob("Advertisement", size = 10, vjust = -1.8, face = "bold"))

ggsave("LiabilityAscriptions.png", device = "png",width = 8.3, height = 4.3, units = "in")

## ================================================================================================================
##                                              DATA VIZUALIZATION  (OLD)            
## ================================================================================================================

std.error <- function(x) sd(x)/sqrt(length(x))

d |>
  gather(key = "DV", value = "Value", firm, human, automation) |>
  group_by(ad, DV) |>
  dplyr::summarize(
    avg_value = mean(Value),
    se_value = std.error(Value) 
  ) |>
  mutate(
    `AV Safety Benefits` = ifelse( ad == "yes", "Present", "Absent"),
    DV = case_when(
      DV == "firm" ~ "Firm Liability",
      DV == "human" ~ "Human Liability",
      DV == 'automation' ~ "Capability"
    )
  ) -> d_plot2

se_width <- 1.96 

ggplot(data = d_plot2[d_plot2$DV != "Capability",], aes(fill=`AV Safety Benefits`, y=avg_value, x = DV)) +
  geom_bar(stat="identity", position="dodge", alpha=.75, width=.6) +
  geom_point(position=position_dodge(width = .6), size=.5, color="black") +
  geom_errorbar(aes(ymin=avg_value-(se_value*se_width), ymax=avg_value+(se_value*se_width)), position = position_dodge(width=.6), 
                size=.25, color="black", width=.25) +
  geom_signif(
    y_position = c(90), xmin = c(0.85, 1.85), xmax = c(1.15, 2.15),
    annotation = c("***","*"), tip_length = 0.1, color='black', size = .25, textsize = 3.5 
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
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) -> p

p

ggsave("backfiring.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

# Plot Level of Automation
ggplot(data = d_plot2[d_plot2$DV == "Capability",], aes(x=`AV Safety Benefits`, y=avg_value)) +
  geom_bar(stat="identity", alpha=.75, width = .5) +
  geom_point(size=.75, color="black") +
  geom_errorbar(aes(ymin=avg_value-(se_value*se_width), ymax=avg_value+(se_value*se_width)), position = "dodge", 
                size=.25, color="black", width=.25) +
  geom_signif(
    y_position = c(6), xmin = c("Absent"), xmax = c("Present"),
    annotation = c("***"), tip_length = 0.1, color='black', size = .5, textsize = 3.5
  ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12), 
        axis.title=element_text(size=10,face="bold")) +
  ylab("Mean Ratings") +
  xlab("") +
  ggtitle("Level of Automation") +
  scale_y_continuous(limits = c(0,6.5)) -> p2

p2

ggsave("level_of_automation.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

##==========================================================
##                        Covariates
##==========================================================

## DV: Firm
summary(lm(firm ~ label + age, d))
summary(lm(firm ~ label + gender, d))

summary(lm(firm ~ ad + age, d))
summary(lm(firm ~ ad + gender, d))

## DV: Human
summary(lm(human ~ label + age, d))
summary(lm(human ~ label + gender, d))

summary(lm(human ~ ad + age, d))
summary(lm(human ~ ad + gender, d))
