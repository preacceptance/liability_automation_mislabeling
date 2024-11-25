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
               'simr',            # power analysis for mixed models
               'compute.es',      # effect size package
               'effsize',         # another effect size package
               'pwr',             # package for power calculation
               'nlme',            # get p values for mixed effect model
               'DescTools',        # get Cramer's V
               'rstatix',
               'effects',
               "Hmisc", 
               "sjstats"
)

## ================================================================================================================
##                                Exclusions               
## ================================================================================================================

# Read full dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read_csv("data.csv")
# Remove first two rows that were headers
df <- df[-c(1,2),]

# Numericize and only include those who finished
df |>
  mutate_if(all.is.numeric, as.numeric) |>
  filter( Finished == 1) -> df

# ATTENTION CHECKS
n_initial <- nrow(df)
df |>
  filter(att_1 == 2, att_2 == 2) -> df

## Number of participants
n_attention <- nrow(df); n_attention

# COMPREHENSION CHECKS
df |>
  filter(comp_1 == 2 & comp_2 == 4 ) |>
  filter(`comp_3...33` == 3 | `comp_3...28` == 3) -> df

## Final Sample
n_comprehension <- nrow(df); n_comprehension

## ================================================================================================================
##                                Preprocessing               
## ================================================================================================================

auto <- df[,c("auto_verb", "auto_explanation", "auto_magnitude_5", "perceived_automation", "age", "gender")]
co <- df[, c("co_verb", "co_explanation", "co_magnitude_5", "capability_copilot", "age", "gender")]

auto$condition <- "auto"
co$condition <- "co"

column_names <- c("verb", "explanation", "magnitude", "capability", "age", "gender", "condition")
colnames(auto) <- column_names
colnames(co) <- column_names

df <- rbind(auto, co)

df |>
  drop_na() -> d

rm(auto,co)

## ================================================================================================================
##                                Participants Characteristics               
## ================================================================================================================

# AGE
mean(as.numeric(d$age), na.rm=T) # filtering the ones who put year

# GENDER
prop_male <- prop.table(table(d$gender))[[1]]; prop_male

## ================================================================================================================
##                                Analysis               
## ================================================================================================================

## Capability
t.test(d[d$condition == "auto", ]$capability, d[d$condition == "co", ]$capability)

sd(d[d$condition == "auto", ]$capability)
sd(d[d$condition == "co", ]$capability)

cohen.d(d[d$condition == "auto", ]$capability, d[d$condition == "co", ]$capability)

## Choice of verbs
d$cause <- ifelse(d$verb == 1, 1, 0)
d$help <- ifelse(d$verb == 2, 1, 0)
d$prevent <- ifelse(d$verb == 3, 1, 0)

## Cause
summary(glm(cause ~ condition, d, family = "binomial"))
prop.table(table(d$cause, d$condition))

## Help
summary(glm(help ~ condition, d, family = "binomial"))
prop.table(table(d$help, d$condition))

## Prevent
summary(glm(prevent ~ condition, d, family = "binomial"))

## Causal Magnitude
t.test(d[d$condition == "auto",]$magnitude, d[d$condition == "co",]$magnitude)

sd(d[d$condition == "auto", ]$magnitude)
sd(d[d$condition == "co", ]$magnitude)

cohen.d(d[d$condition == "auto", ]$magnitude, d[d$condition == "co", ]$magnitude)

## correlation
cor.test(d$capability, d$magnitude)

## ================================================================================================================
##                                      End of Analysis               
## ================================================================================================================
