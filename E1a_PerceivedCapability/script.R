## ================================================================================================================
##                                DATA ANALYSIS | AV LABEL STUDY | EXPERIMENT 1a               
## ================================================================================================================
## clear workspace
rm(list = ls()) 

## import packages
pacman::p_load('BayesFactor')
library(dplyr)
library(ltm)
library(ggplot2)
library(ggsignif)
library(ggpubr)
library(tidyr)
library(lsr)
library(ltm)
library(effsize)


## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

## read in data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.csv('data.csv') 

## rename variables:
d |>
  rename( cond = FL_12_DO ) |>
  mutate( cond = case_when(
    cond == "FL_35" ~ "auto",  # Autopilot
    cond == "FL_36" ~ "co",    # Copilot
    cond == "FL_37" ~ "dless", # Driverless
  )) -> d

## subjects randomized:
table(d$cond)
## ================================================================================================================
##                                                   EXCLUSIONS                
## ================================================================================================================


## attention exclusions: 
# remove responses from data frame that failed attention checks
d <- subset(d, (d$att_1 == 2 & d$att_2 == 2))

dim(d) # number of participants should decrease after attention exclusions
n_original <- dim(d)[1] # extracting number of rows only, not columns
n_original

## comprehension exclusions: 
# remove responses from data frame that failed comprehension checks
d <- subset(d, (d$comp_1 == 2 & d$comp_2 == 4))

## number of participants AFTER exclusions: 
n_final <- dim(d)[1] # extracting number of rows only, not columns
n_final 

## Num excluded
n_excluded <- n_original - n_final; n_excluded

## ================================================================================================================
##                                                    SUBSETTING                 
## ================================================================================================================

##re-arrange data
d <- d %>% relocate(co_1, .after = auto_1)
d <- d %>% relocate(dr_1, .after = co_1)
d <- d %>% relocate(use_2_1_1, .after = use_1_1_1)
d <- d %>% relocate(use_3_1_1, .after = use_2_1_1)
d <- d %>% relocate(use_2_2_1, .after = use_1_2_1)
d <- d %>% relocate(use_3_2_1, .after = use_2_2_1)
d <- d %>% relocate(use_2_3_1, .after = use_1_3_1)
d <- d %>% relocate(use_3_3_1, .after = use_2_3_1)
d <- d %>% relocate(use_2_4_1, .after = use_1_4_1)
d <- d %>% relocate(use_3_4_1, .after = use_2_4_1)
d <- d %>% relocate(value_2_1_1, .after = value_1_1_1)
d <- d %>% relocate(value_3_1_1, .after = value_2_1_1)
d <- d %>% relocate(value_2_2_1, .after = value_1_2_1)
d <- d %>% relocate(value_3_2_1, .after = value_2_2_1)
d <- d %>% relocate(value_2_3_1, .after = value_1_3_1)
d <- d %>% relocate(value_3_3_1, .after = value_2_3_1)
d <- d %>% relocate(value_2_4, .after = value_1_4)
d <- d %>% relocate(value_3_4, .after = value_2_4)

## new data frame to extract pre-processed data into:
d_subset <- array(dim=c(dim(d)[1], 10))
colnames(d_subset) <- c('cond','auto','use1','use2','use3','use4','value1','value2','value3','value4')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE) 

## extract data of interest from middle part of raw data:
for(i in 1:dim(d)[1]) {
  curr <- d[i,24:26][!is.na(d[i,24:26])] # for a given row, get only the non-NA values
  d_subset[i,2] <- as.numeric(curr[curr!= ""]) # and only the non-empty values
  u1 <- d[i,27:29][!is.na(d[i,27:29])]
  d_subset[i,3] <- as.numeric(u1[u1!= ""])
  u2 <- d[i,30:32][!is.na(d[i,30:32])]
  d_subset[i,4] <- as.numeric(u2[u2!= ""])
  u3 <- d[i,33:35][!is.na(d[i,33:35])]
  d_subset[i,5] <- as.numeric(u3[u3!= ""])
  u4 <- d[i,36:38][!is.na(d[i,36:38])]
  d_subset[i,6] <- as.numeric(u4[u4!= ""])
  v1 <- d[i,39:41][!is.na(d[i,39:41])]
  d_subset[i,7] <- as.numeric(v1[v1!= ""])
  v2 <- d[i,42:44][!is.na(d[i,42:44])]
  d_subset[i,8] <- as.numeric(v2[v2!= ""])
  v3 <- d[i,45:47][!is.na(d[i,45:47])]
  d_subset[i,9] <- as.numeric(v3[v3!= ""])
  v4 <- d[i,48:50][!is.na(d[i,48:50])]
  d_subset[i,10] <- as.numeric(v4[v4!= ""])
  d_subset[i,1] <- d[i,61][!is.na(d[i,61])] 
}

## merge data of interest back with raw data:
# new data frame to work with
d_merged <- cbind(d_subset, d[,51:60])
d_merged$ss <- 1:dim(d_merged)[1]

## cleaning up extreme prices of $30, $50 and $480,000
d_merged <- d_merged[-98,] #50
d_merged <- d_merged[-102,] #30
d_merged <- d_merged[-60,] #480k

rm(d, d_subset, i, curr)

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

## age
mean_age <- mean(d_merged$age, trim = 0, na.rm = TRUE) ## mean age 
mean_age

## gender
prop_male <- table(d_merged$gender)[[1]]/sum(table(d_merged$gender)) ## percentage of males
prop_male

## ================================================================================================================
##                                              DATA ANALYSIS             
## ================================================================================================================

# LEVEL OF AUTOMATION
## Autopilot vs Copilot
d1 <- subset(d_merged, d_merged$cond != 'dless')
t.test(auto ~ cond, data = d1)

sd(d_merged[d_merged$cond == "auto",]$auto)
sd(d_merged[d_merged$cond == "co",]$auto)

cohen.d(d_merged[d_merged$cond == "co",]$auto, d_merged[d_merged$cond == "auto",]$auto)

## Autopilot vs Driverless
d2 <- subset(d_merged, d_merged$cond != 'co')
t.test(auto ~ cond, data = d2)

sd(d_merged[d_merged$cond == "dless",]$auto)

cohen.d(d_merged[d_merged$cond == "dless",]$auto, d_merged[d_merged$cond == "auto",]$auto)

## Copilot vs. Driverless
d3 <- subset(d_merged, d_merged$cond != 'auto')
t.test(auto ~ cond, data = d3)

cohen.d(d_merged[d_merged$cond == "co",]$auto, d_merged[d_merged$cond == "dless",]$auto)

## Bayes Factor t-test
### Autopilot and Copilot
result <- ttestBF(x = d_merged[d_merged$cond == "auto",]$auto, 
                  y = d_merged[d_merged$cond == "co",]$auto)
1 / result

### Autopilot and Driverless
result <- ttestBF(x = d_merged[d_merged$cond == "auto",]$auto, 
                  y = d_merged[d_merged$cond == "dless",]$auto)
1 / result

## Copilot and Driverless
result <- ttestBF(x = d_merged[d_merged$cond == "co",]$auto, 
                  y = d_merged[d_merged$cond == "dless",]$auto)
1 / result

## ================================================================================================================
##                                              Cronbach Alpha           
## ================================================================================================================

d_merged |>
  mutate(
    use1 = (use1 - mean(use1))/sd(use1),
    use2 = (use2 - mean(use2))/sd(use2),
    use3 = (use3 - mean(use3))/sd(use3),
    use4 = (use4 - mean(use4))/sd(use4),
    value1 = (value1 - mean(value1))/sd(value1),
    value2 = (value2 - mean(value2))/sd(value2),
    value3 = (value3 - mean(value3))/sd(value3),
    value4 = (value4 - mean(value4))/sd(value4)
  ) -> d_merged


## Cronbach Alpha
### Perceived value
d_val <- d_merged[,c("value1", "value2", "value3", "value4")]
cronbach.alpha(d_val)

### Ease of Use
d_use <- d_merged[,c("use1", "use2", "use3", "use4")]
d_use$use4 <- -d_use$use4
cronbach.alpha(d_use)

## ================================================================================================================
##                                       Data Visualization                
## ================================================================================================================

d_merged[,c("cond", "auto", "use1", "use2", "use3", "use4", "value1", "value2", "value3", "value4")] -> d_plot

colnames(d_plot) <- c("Condition", "Perceived Automation", "Learn", "Control", "Understand", "Misinterpret", "Quality", "Enjoyment", "Social Standing", "Price")

d <- d_plot

d_plot |>
  gather(key = "Measure", value = "Response" , 2:10) |>
  mutate( 
    Condition = case_when(
      Condition == "co" ~ "Copilot",
      Condition == "auto" ~ "Autopilot",
      Condition == "dless" ~ "Driverless",
    )
  ) |>
  group_by(Condition, Measure) |>
  dplyr::summarize(
    Mean = mean(Response),
    SE = sd(Response)/sqrt(n()),
    SD = sd(Response)
  ) -> d_plot


plot_bar <- function(df=d_plot, dv, y_pos, signif=c("*","*","*"), titulo) {
  
  d_plot <- d_plot |>
    filter(Measure == dv)
  
  se_width <- 1.96
  
  ggplot(data = d_plot, aes(x=factor(Condition, level = c("Copilot", "Autopilot", "Driverless")), y=Mean)) +
    geom_bar(stat="identity", alpha=.75) +
    geom_point(size=.75, color="black") +
    geom_errorbar(aes(ymin=Mean-(SE*se_width), ymax=Mean+(SE*se_width)), position = "dodge", 
                  size=.25, color="black", width=.75) +
    geom_signif(
      y_position = y_pos, xmin = c("Copilot", "Autopilot", "Driverless"), xmax = c("Autopilot", "Driverless", "Copilot"),
      annotation = signif, tip_length = 0.1, color='black', size = .5, textsize = 3.5
    ) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold"),,
          axis.text.x = element_text(size=10, face="bold", color = "black"),
          axis.title = element_text(size=12, face="bold")) +
    ylab("Perceived Level of Automation") +
    xlab("Marketing Label") -> p
  
  return(p)
}

plot_bar(dv = "Perceived Automation", y_pos = c(6, 6.75, 7.5),
         signif = c("***", "ns", "***"), 
         titulo = "Perceived Level of Automation") -> a1

a1 

ggsave("perceived_automation.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

## ================================================================================================================
##                                      Ease of Use and Perceived Value                
## ================================================================================================================

d_merged |>
  mutate(use = (use1 + use2 + use3 + use4)/4,
         value = (value1 + value2 + value3 + value4) / 4) -> d_merged

# USE
# t-tests
## Autopilot vs Copilot
d1 <- subset(d_merged, d_merged$cond != 'dless')
t.test(use ~ cond, data = d1)
## Autopilot vs Driverless
d2 <- subset(d_merged, d_merged$cond != 'co')
t.test(use ~ cond, data = d2)
## Copilot vs. Driverless
d3 <- subset(d_merged, d_merged$cond != 'auto')
t.test(use ~ cond, data = d3)

# Standard Deviation
## Driverless
sd(d_merged[d_merged$cond == "dless",]$use)
## Autopilot
sd(d_merged[d_merged$cond == "auto",]$use)
## Copilot
sd(d_merged[d_merged$cond == "co",]$use)

# VALUE
# t-tests
## Autopilot vs Copilot
d1 <- subset(d_merged, d_merged$cond != 'dless')
t.test(value ~ cond, data = d1)
## Autopilot vs Driverless
d2 <- subset(d_merged, d_merged$cond != 'co')
t.test(value ~ cond, data = d2)
## Copilot vs. Driverless
d3 <- subset(d_merged, d_merged$cond != 'auto')
t.test(value ~ cond, data = d3)

# Standard Deviation
## Driverless
sd(d_merged[d_merged$cond == "dless",]$value)
## Autopilot
sd(d_merged[d_merged$cond == "auto",]$value)
## Copilot
sd(d_merged[d_merged$cond == "co",]$value)


# Cohen's D
## Driverless vs. Autopilot
cohen.d(d_merged[d_merged$cond == "dless",]$use, d_merged[d_merged$cond == "auto",]$use)
## Driverless vs Copilot
cohen.d(d_merged[d_merged$cond == "dless",]$use, d_merged[d_merged$cond == "co",]$use)
## Copilot vs Autopilot
cohen.d(d_merged[d_merged$cond == "co",]$use, d_merged[d_merged$cond == "auto",]$use)



## ================================================================================================================
##                                                  END OF ANALYSIS                 
## ================================================================================================================
