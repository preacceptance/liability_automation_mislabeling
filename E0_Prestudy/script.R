
## clear workspace
rm(list = ls()) 

# Import libraries required
library(tidyverse)
library(ltm)
library(readxl)

# =====================================================================================
#                                 Read XLSX files
#             This does not run since copyrighted data is included
# =====================================================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

nyt <- read_xlsx("./data/ArticleCoding - New York Times.xlsx", sheet = "Articles", skip = 2)
usa <- read_xlsx("./data/ArticleCoding - USA Today.xlsx", sheet = 2, skip = 3)
wsj <- read_xlsx("./data/ArticleCoding - Wall Street Journal.xlsx", sheet = 2, skip = 2)
wapo <- read_xlsx("./data/ArticleCoding - Washington Post.xlsx", sheet = 1, skip = 2)
nyp <- read_xlsx("./data/ArticleCoding - New York Post.xlsx", sheet = 1, skip = 2)

# =====================================================================================
#                                 PREPROCESSING 
#                 This does not run since copyrighted data is included
# =====================================================================================
relevant_col <- c("Journal", "Categorization for Coder 1", "Categorization for Coder 2")

## Read indvidual newspaper sources and standardize columns
nyt <- nyt[,relevant_col]
usa <- usa[, relevant_col]
wapo <- wapo[, relevant_col]
wsj <- wsj[, c(4,7,8)]
nyp <- nyp[, c(5,8,9)]
colnames(wsj) <- relevant_col 
colnames(nyp) <- relevant_col

## Bind the dataframe
df <- rbind(nyt, usa, wapo, wsj, nyp)

write.csv(df, "data.csv", row.names = F)

# =====================================================================================
#                                 ANALYSIS
# =====================================================================================
## Cleaned data
df <- read_csv("data.csv")

## Reliability of Coding
cronbach.alpha(df[df$Journal == "New York Times", c(2,3)])
cronbach.alpha(df[df$Journal == "Wall Street Journal", c(2,3)])
cronbach.alpha(df[df$Journal == "Washington Post", c(2,3)])
cronbach.alpha(df[df$Journal == "New York Post", c(2,3)])
cronbach.alpha(df[df$Journal == "USA Today", c(2,3)])

## Only keep those with agreements
## and make dummy variables for mismarketing
df |>
  filter(`Categorization for Coder 1` == `Categorization for Coder 2`) |>
  mutate(
    mismarketing = ifelse(`Categorization for Coder 1` == 1, TRUE, FALSE)
  ) -> df

## Overall proportion of mismarketing
p <- mean(df$mismarketing); p

df |>
  group_by(Journal) |>
  summarize( count = sum(mismarketing),
             num_documents = n()) |>
  mutate( prop = count/num_documents )-> prop

prop
