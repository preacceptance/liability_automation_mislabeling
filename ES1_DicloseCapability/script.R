## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
library(dplyr)
library(grid)
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',         # plotting
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',             # probably not using..
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
               'sjstats'
)

# PROCESS Analysis (Set TRUE if you wish to run PROCESS code)
mediation <- FALSE

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data: 
# set working directory to current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.csv('data.csv') 

## rename variables:
names(d)[names(d) == 'FL_12_DO'] <- 'cond'
names(d)[names(d) == 'resp_human1_10'] <- 'auto_nt_human_r'
names(d)[names(d) == 'resp_software1_10'] <- 'auto_nt_firm_r'
names(d)[names(d) == 'resp_human2_10'] <- 'co_nt_human_r'
names(d)[names(d) == 'resp_software2_10'] <- 'co_nt_firm_r'
names(d)[names(d) == 'liable_human1_1'] <- 'auto_nt_human_l'
names(d)[names(d) == 'liable_firm1_1'] <- 'auto_nt_firm_l'
names(d)[names(d) == 'liab_human2_1'] <- 'co_nt_human_l'
names(d)[names(d) == 'liab_firm2_1'] <- 'co_nt_firm_l'
names(d)[names(d) == 'resp_human3_10'] <- 'auto_ft_human_r'
names(d)[names(d) == 'resp_software3_10'] <- 'auto_ft_firm_r'
names(d)[names(d) == 'resp_human4_10'] <- 'co_ft_human_r'
names(d)[names(d) == 'resp_software4_10'] <- 'co_ft_firm_r'
names(d)[names(d) == 'liab_human3_1'] <- 'auto_ft_human_l'
names(d)[names(d) == 'liab_soft3_1'] <- 'auto_ft_firm_l'
names(d)[names(d) == 'liab_human4_1'] <- 'co_ft_human_l'
names(d)[names(d) == 'liab_software4_1'] <- 'co_ft_firm_l'

## change condition entries
d$cond[d$cond == "FL_35"] <- "auto_nt"
d$cond[d$cond == "FL_36"] <- "co_nt"
d$cond[d$cond == "FL_50"] <- "auto_ft"
d$cond[d$cond == "FL_54"] <- "co_ft"

## ================================================================================================================
##                                                   EXCLUSIONS                
## ================================================================================================================


## attention exclusions: 
# remove responses from data frame that failed attention checks
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))
## incomplete responses
d <- subset(d, (d$Finished == 1))

n_original <- dim(d)[1]; n_original

## comprehension exclusions: 
# remove responses from data frame that failed comprehension checks
d <- subset(d, (d$comp_1 == 2))
n_original - nrow(d)
d <- subset(d, (d$comp_2 == 4))
n_original - nrow(d)
d <- subset(d, (d$comp_3 == 2 | d$comp_4 == 1 | d$comp_5 == 2 | d$comp_6 == 1 | d$comp_7 == 1 | d$comp_8 == 1))
n_original - nrow(d)

## number of participants AFTER exclusions: 
n_final <- dim(d)[1] # extracting number of rows only, not columns
n_final

## Number excluded
n_original - n_final

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

d <- d %>% relocate(co_1, .after = auto_1)
d <- d %>% relocate(auto_2, .after = co_1)
d <- d %>% relocate(co_2, .after = auto_2)
d <- d %>% relocate(co_nt_firm_r, .after = auto_nt_firm_r)
d <- d %>% relocate(auto_ft_firm_r, .after = co_nt_firm_r)
d <- d %>% relocate(co_ft_firm_r, .after = auto_ft_firm_r)

d <- d %>% relocate(co_nt_human_r, .after = auto_nt_human_r)
d <- d %>% relocate(auto_ft_human_r, .after = co_nt_human_r)
d <- d %>% relocate(co_ft_human_r, .after = auto_ft_human_r)

d <- d %>% relocate(co_nt_firm_l, .after = auto_nt_firm_l)
d <- d %>% relocate(auto_ft_firm_l, .after = co_nt_firm_l)
d <- d %>% relocate(co_ft_firm_l, .after = auto_ft_firm_l)

d <- d %>% relocate(co_nt_human_l, .after = auto_nt_human_l)
d <- d %>% relocate(auto_ft_human_l, .after = co_nt_human_l)
d <- d %>% relocate(co_ft_human_l, .after = auto_ft_human_l)

## new data frame to extract pre-processed data into:
d_subset <- array(dim=c(dim(d)[1], 6))
colnames(d_subset) <- c('cond','automation','firm_responsibility','human_responsibility','firm_liability','human_liability')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE) 

## extract data of interest from middle part of raw data:
for(i in 1:dim(d)[1]) {
  pref1 <- d[i,24:27][!is.na(d[i,24:27])] # for a given row, get only the non-NA values
  d_subset[i,2] <- as.numeric(pref1[pref1!= ""]) # and only the non-empty values
  resp1 <- d[i,28:31][!is.na(d[i,28:31])]
  d_subset[i,3] <- as.numeric(resp1[resp1!= ""])
  resp2 <- d[i,32:35][!is.na(d[i,32:35])]
  d_subset[i,4] <- as.numeric(resp2[resp2!= ""])
  liab1 <- d[i,36:39][!is.na(d[i,36:39])]
  d_subset[i,5] <- as.numeric(liab1[liab1!= ""])
  liab2 <- d[i,40:43][!is.na(d[i,40:43])]
  d_subset[i,6] <- as.numeric(liab2[liab2!= ""])
  d_subset[i,1] <- d[i,69][!is.na(d[i,69])] 
}

## merge data of interest back with raw data:
# new data frame to work with
d_merged <- cbind(d_subset, d[,52:68])
d_merged$ss <- 1:dim(d_merged)[1]

## add columns for label and transparency (now renamed as disclosure) condition entries
d_merged$label <- ""
d_merged$transparency <- ""
d_merged$label[d_merged$cond == 'auto_nt'] <- 'auto'
d_merged$label[d_merged$cond == 'auto_ft'] <- 'auto'
d_merged$label[d_merged$cond == 'co_nt'] <- 'co'
d_merged$label[d_merged$cond == 'co_ft'] <- 'co'
d_merged$transparency[d_merged$cond == 'auto_nt'] <- 'no'
d_merged$transparency[d_merged$cond == 'auto_ft'] <- 'yes'
d_merged$transparency[d_merged$cond == 'co_nt'] <- 'no'
d_merged$transparency[d_merged$cond == 'co_ft'] <- 'yes'

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
mean(d_merged$age, trim = 0, na.rm = TRUE) ## mean age 

## gender
prop.table(table(d_merged$gender))[1] ## percentage of males

## ================================================================================================================
##                                                    Analysis                 
## ================================================================================================================

cronbach.alpha(d_subset[, c("firm_responsibility","firm_liability")])
cronbach.alpha(d_subset[, c("human_responsibility","human_liability")])

d_merged |>
  mutate(
    firm = (`firm_responsibility` + `firm_liability`) / 2,
    human = (`human_responsibility` + `human_liability`) / 2,
    disclosure = transparency
  ) -> d_merged


# FIRM LIABILITY
## ANOVA
firmliab_mod <- aov(firm ~ as.factor(label) * as.factor(disclosure), data = d_merged)
summary(firmliab_mod)
anova_stats(firmliab_mod); anova_stats(firmliab_mod)$partial.etasq

## t-tests
### Disclosure Absent
t1 <- t.test(d_merged$firm[d_merged$disclosure == 'no'& d_merged$label == 'auto'],
             d_merged$firm[d_merged$disclosure == 'no'& d_merged$label == 'co'], paired = FALSE)
t1

sd(d_merged$firm[d_merged$disclosure == 'no'& d_merged$label == 'auto'])
sd(d_merged$firm[d_merged$disclosure == 'no'& d_merged$label == 'co'])

cohen.d(d_merged$firm[d_merged$disclosure == 'no'& d_merged$label == 'auto'],
        d_merged$firm[d_merged$disclosure == 'no'& d_merged$label == 'co'])

### Disclosure Present
t2 <- t.test(d_merged$firm[d_merged$disclosure == 'yes'& d_merged$label == 'auto'],
              d_merged$firm[d_merged$disclosure == 'yes'& d_merged$label == 'co'], paired = FALSE)
t2

sd(d_merged$firm[d_merged$disclosure == 'yes'& d_merged$label == 'auto'])
sd(d_merged$firm[d_merged$disclosure == 'yes'& d_merged$label == 'co'])

cohen.d(d_merged$firm[d_merged$disclosure == 'yes'& d_merged$label == 'auto'],
        d_merged$firm[d_merged$disclosure == 'yes'& d_merged$label == 'co'])


# HUMAN LIABILITY
## ANOVA
humaliab_mod <- aov(human ~ as.factor(label) * as.factor(disclosure), data = d_merged)
summary(humaliab_mod)
anova_stats(humaliab_mod); anova_stats(humaliab_mod)$partial.etasq

## t-tests
### Disclosure Absent
t1 <- t.test(d_merged$human[d_merged$disclosure == 'no'& d_merged$label == 'auto'],
             d_merged$human[d_merged$disclosure == 'no'& d_merged$label == 'co'], paired = FALSE)
t1

sd(d_merged$human[d_merged$disclosure == 'no'& d_merged$label == 'auto'])
sd(d_merged$human[d_merged$disclosure == 'no'& d_merged$label == 'co'])

cohen.d(d_merged$human[d_merged$disclosure == 'no'& d_merged$label == 'auto'],
        d_merged$human[d_merged$disclosure == 'no'& d_merged$label == 'co'])

### Disclosure Present
t2 <- t.test(d_merged$human[d_merged$disclosure == 'yes'& d_merged$label == 'auto'],
             d_merged$human[d_merged$disclosure == 'yes'& d_merged$label == 'co'], paired = FALSE)
t2

sd(d_merged$human[d_merged$disclosure == 'yes'& d_merged$label == 'auto'])
sd(d_merged$human[d_merged$disclosure == 'yes'& d_merged$label == 'co'])

cohen.d(d_merged$human[d_merged$disclosure == 'yes'& d_merged$label == 'auto'],
       d_merged$human[d_merged$disclosure == 'yes'& d_merged$label == 'co'])


## ================================================================================================================
##                                              DATA ANALYSIS - MEDIATION / MODERATED MEDIATION               
## ================================================================================================================
d_merged$cond_text = d_merged$cond
d_merged$cond = as.factor(d_merged$cond)
d_merged$cond = as.numeric(d_merged$cond)
d_merged$disclosure = as.numeric(as.factor(d_merged$disclosure))
d_merged$label = as.numeric(as.factor(d_merged$label))

if (mediation) {
  
source('../process.R')

## Model 14 'b' path

### FIRM LIABILITY
process(data = d_merged, y = "firm", x = "label", 
        m =c("automation"), w = "disclosure", model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

### HUMAN LIABILITY
process(data = d_merged, y = "human", x = "label", 
        m =c("automation"), w = "disclosure",model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Model 4 Simple Mediation

### FIRM LIABILITY
process(data = d_merged, y = "firm", x = "label", 
        m =c("automation"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

### HUMAN LIABILITY
process(data = d_merged, y = "human", x = "label", 
        m =c("automation"), model = 4, effsize  = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)
}

## ================================================================================================================
##                                              VISUALIZATION                
## ================================================================================================================

std.error <- function(x) sd(x)/sqrt(length(x))

d_merged |>
  gather(key = "DV", value = "Value", firm, human) |>
  mutate(
    DV = ifelse( DV == "human", "Human Liability", "Firm Liability"),
    `Marketing Label` = ifelse(grepl("auto", cond_text), "Autopilot", "Copilot"),
    Disclosure = ifelse(grepl("ft", cond_text), "Disclosed", "Not Disclosed")
  ) |>
  group_by(`Marketing Label`, Disclosure, DV) |>
  summarize( 
    mean = mean(Value),
    se = std.error(Value) 
  ) -> d_plot

plot_did <- function(df=d_plot, dv, signif=c("*","*","*"), yaxis=TRUE, ypos=c(100, 100, 114)) {
  
  d_plot <- df |>
    filter(DV == dv)
  
  se_width <- 1.96
  
  ggplot(data = d_plot, aes(x=Disclosure, y=mean, fill=`Marketing Label`)) +
    geom_bar(stat="identity", position="dodge", alpha=.75) +
    geom_errorbar(aes(ymin=mean-(se*se_width), ymax=mean+(se*se_width)), position = position_dodge(width=.9), 
                  size=.25, color="black", width=.5) +
    geom_point(aes(y=mean),position=position_dodge(width = .9), size=.5, color="black") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold",size=10)
    ) +
    geom_signif(
      y_position = ypos, xmin = c(0.8, 1.8, 1.0), xmax = c(1.2, 2.2, 2.0),
      annotation = signif, tip_length = 0.1, color='black', size = .25, textsize = 3.5 
    ) +
    scale_fill_grey() +
    scale_color_grey() +
    ggtitle(dv) +
    xlab("Disclosure") +
    ylab("Response") +
    scale_y_continuous(limits = c(0,110), breaks = c(0,20,40,60,80,100)) -> p
  
  if(!yaxis) {
    p <- p +
      theme( axis.line.y = element_line(color = "white"),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank()) +
      ylab("")
  }
  
  return(p)
}

plot_did(dv = "Firm Liability", signif = c("*", "**", "ns"), ypos = c(80,80,95), yaxis=T) -> p4

plot_did(dv = "Human Liability", signif = c("ns", "+", "ns"), ypos = c(95,95,108), yaxis=F) -> p3


ggarrange(p4 + rremove("ylab") + rremove("xlab"),
          p3 + rremove("ylab") + rremove("xlab"), common.legend = T) |>
  annotate_figure(left = textGrob("Mean Ratings", rot = 90, vjust = 1, gp = gpar(cex = .8, fontface="bold")),
    bottom = textGrob("Disclosure Condition", gp = gpar(cex = .8, fontface="bold")))

ggsave("disclosure_condition.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")
