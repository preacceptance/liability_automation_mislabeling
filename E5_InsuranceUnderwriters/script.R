## clear workspace
rm(list = ls()) 

# options(download.file.method="libcurl")

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('tidyverse',       # most stuff
               'ggsignif',
               'grid', # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',             # Cronbach Alpha
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'emmeans',         # contrast analysis for regression models
               'stats',           # use function to adjust for multiple comparisons
               'filesstrings',    # create and move files
               'rstatix',
               'effects',
               "Hmisc", 
               "sjstats"
)

## ================================================================================================================
##                                Pre-Processing              
## ================================================================================================================

# Read full dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read_csv("data.csv")
# Remove first two rows that were headers
df <- df[-c(1,2),]

df |>
  mutate_if(all.is.numeric, as.numeric) -> df

## Number of participants
n_participants <- nrow(df); n_participants

df$`Consider Label` <- ifelse(df$risk_1 == 1, "Yes", "No")

## ================================================================================================================
##                                ANALYSIS             
## ================================================================================================================


# Proportion Responded
sum(!is.na(df$risk_1))/length(df$risk_1) # Consider labels for risk
sum(!is.na(df$adjust_4))/length(df$adjust_4) # Adjust Risk Premiums
sum(!is.na(df$risk_4))/length(df$risk_4) # Increase Risk Premiums
sum(!is.na(df$advise_4))/length(df$advise_4) # Advise
(sum(!is.na(df$reasoning)) - 2)/length(df$reasoning) # Reasoning (minus 2 for n/a and - in response)


## Consider Label
prop.table(table(df$`Consider Label`))
table(df$`Consider Label`)

chisq.test(table(df$`Consider Label`), p = c(.5,.5))

# Adjust Risk Estimates
t.test(df$adjust_4, mu = 50)
sd(df$adjust_4, na.rm = T)

# Increase/Decrease Risk Estimates
t.test(df$risk_4, mu = 50)
sd(df$risk_4, na.rm = T)

## ================================================================================================================
##                                CODED DATA              
## ================================================================================================================

coded <- read_csv('coded_responses.csv')

coded <- coded[!is.na(coded$code),]

c <- paste(coded$code, collapse = ",")

c <- strsplit(c, ",")

table(c)
