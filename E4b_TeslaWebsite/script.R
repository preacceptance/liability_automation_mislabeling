
## ================================================================================================================
##                                DATA ANALYSIS | AV LABEL STUDY | EXPERIMENT 4b               
## ================================================================================================================
## clear workspace
rm(list = ls())

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('tidyverse',   
               'ggpubr',         
               'ggsignif', 
               'knitr',       
               'wordcloud2',
               'tidytext',
               'tm'
)

# Read full dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read_csv("data.csv")
# Remove first two rows that were headers
df <- df[-c(1,2),]

## ================================================================================================================
##                                Exclusions              
## ================================================================================================================
## Failed Attention Checks
df |>
  filter(
    att_1 == 2,
    att_2 == 2) -> df


## Number of recruited
n_original <- nrow(df); n_original

## Comprehension Checks
df |>
  filter(
    comp_1 == 2,
    comp_2 == 4
  ) -> df

## Check the percentage that clicked
percentage_clicked <- mean(as.numeric(df$clicked))
percentage_clicked

## Remove those who did not
df <- df[df$clicked == 1,]

## Final number of participants
n_final <- nrow(df); n_final
## Number of excluded
n_excluded <- n_original - n_final; n_excluded

## ================================================================================================================
##                                Participant Characteristics               
## ================================================================================================================
## Age
mean(as.numeric(df$age), na.rm = T)

## Gender
n_male <- length(df[df$gender == 1,]$gender)
n_female <- length(df[df$gender == 2,]$gender)
prop_female <- n_female / (n_male + n_female); prop_female

rm(n_male, n_female, prop_female)

## ================================================================================================================
##                                Pre-processing               
## ================================================================================================================

## Write the file out for wordcloud generation
text_responses <- df$words_1
word_cloud <- file("wordCloud.txt", "wb")
writeBin( paste(text_responses, collapse="\n"), word_cloud ) 
close(word_cloud)


# Relevant Columns and Elongate Data
rel_col <- c("level_1", "level_2", "hard_1_1", "info_text", "level_3", "gender", "age", "license", "ai_knowledge_1")
df <- df[,rel_col]

df |>
  filter(level_1 == 1) |>
  dplyr::select(level_1, level_2, hard_1_1, gender, age, license, ai_knowledge_1) -> found

colnames(found) <- c("found", "auto_level", "difficulty", "gender", "age", "license", "ai_knowledge")

df |>
  filter(level_1 == 2) |>
  dplyr::select(level_1, level_3, hard_1_1, gender, age, license, ai_knowledge_1) -> not_found

colnames(not_found) <- colnames(found)

df <- rbind(found, not_found)

rm(found, not_found)

df |>
  dplyr::summarize_all(as.numeric) |>
  mutate( found = ifelse(found == 1, TRUE, FALSE),
          is_correct = ifelse(auto_level == 2, TRUE, FALSE),
          found_label = ifelse(found, "Found", "Not Found"), 
          is_correct_label = ifelse(is_correct, "Correct", "Wrong")) -> df


## ================================================================================================================
##                                      Analysis               
## ================================================================================================================

## Proportion that claimed to have found the level of automation
prop_found <- mean(df$found); prop_found

## Proportion that is correct
prop_correct <- mean(df$is_correct); prop_correct

## Proportion reporting Level 3 or higher
mean(df$auto_level >= 3)

## t-test for those who found vs not found
t.test(df[df$found,]$auto_level, df[!df$found,]$auto_level)

sd(df[df$found,]$auto_level)
sd(df[!df$found,]$auto_level)

## Difficulty == 50? 
t.test(df[!is.na(df$difficulty),]$difficulty , mu = 50)
sd(df[!is.na(df$difficulty),]$difficulty)

## Difficulty correlation with automation level
cor.test(df[!is.na(df$difficulty),]$difficulty, df[!is.na(df$difficulty),]$auto_level)

## ================================================================================================================
##                                VISUALIZATION               
## ================================================================================================================

df |>
  group_by(auto_level, found) |>
  summarize( count = n()) -> df_plot

df_plot <- as.data.frame(df_plot)
df_plot[nrow(df_plot) + 1,] <- c(3, TRUE, 0)
df_plot[nrow(df_plot) + 1,] <- c(1, TRUE, 0)
df_plot[nrow(df_plot) + 1,] <- c(1, FALSE, 0)

df |>
  mutate(high = ifelse(auto_level > 3, T, F)) |>
  group_by(high) |>
  summarize(count = n())

df_plot |>
  mutate( Found = ifelse(found == 1, "TRUE", FALSE),
          `Level of Automation` = auto_level) -> df_plot

## Participant Responses
ggplot(data = df_plot, aes(x=`Level of Automation`, y = count, fill = Found)) +
  geom_bar(stat="identity", position="dodge", alpha = .75) +
  scale_fill_grey() +
  scale_color_grey() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10), legend.position = "top",
        axis.title = element_text(face="bold"), text = element_text(face="bold", size=10)) +
  scale_x_discrete(name ="Level of Automation", 
                   limits = 1:6) +
  scale_y_discrete(
    name = "Number of Responses",
    limits = 0:10
  ) -> p

p

ggsave("participants_responses.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

## Word Count
wc <- read_csv("word_count.csv")
wc |>
  arrange(desc(count)) -> wc

ggplot(wc, aes( x=count, y = reorder(word, (count)))) +
  geom_bar(stat="identity") +
  theme_classic() +
  xlab("Count") +
  ylab("Keywords") + 
  theme(axis.title = element_text(face="bold", size=10), axis.text = element_text(face="bold", size=10))

ggsave("word_count.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")
