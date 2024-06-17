
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

# PROCESS Analysis (Set TRUE if you wish to run PROCESS code)
mediation <- FALSE

if(mediation) source('../process.R')


## ================================================================================================================
##                                    Exclusions            
## ================================================================================================================

# Read full dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read_csv("data.csv")
# Remove first two rows that were headers
df <- df[-c(1,2),]

df |>
  mutate_if(all.is.numeric, as.numeric) -> df

# ATTENTION CHECKS
n_initial <- nrow(df)
df |>
  filter(att_1 == 2, att_2 == 2) -> df

n_attention <- nrow(df); n_attention

# COMPREHENSION CHECKS
df |>
  filter(comp_1 == 2 & comp_2 == 4) -> df

# Rearranging
df |>
  mutate(auto_1 = ifelse(is.na(auto_1), auto_1_ad, auto_1),
         co_1 = ifelse(is.na(co_1), co_1_ad, co_1)) -> df

auto <- df[,c("auto_1","ad_1","resp_software1_10","resp_human1_10",
              "liable_firm1_1", "liable_human1_1", "comp_3", "age", "gender")]
co <- df[,c("co_1","ad_2","resp_software2_10","resp_human2_10",
            "liab_firm2_1", "liab_human2_1", "comp_4", "age", "gender")]

new_colnames <- c("capability", "comp_4", "r_soft", "r_human", "l_firm", "l_human", "comp_3", "age", "gender")
colnames(auto) <- new_colnames
colnames(co) <- new_colnames

auto$label <- "auto"
auto$benefits <- ifelse(is.na(auto$comp_4), "Absent", "Present")

co$label <- "co"
co$benefits <- ifelse(is.na(co$comp_4), "Absent", "Present")

d <- rbind(auto,co)
d <- d[!is.na(d$capability),]


# COMPREHENSION CHECKS 3 & 4
d |>
  filter((comp_3 == 2 & label == "auto") | (comp_3 == 1 & label == "co")) -> d
d |>
  filter((comp_4 == 1 | is.na(comp_4))) -> d

## For PROCESS
d$firm <- rowMeans(d[,c("r_soft", "l_firm")])
d$human <- rowMeans(d[,c("r_human", "l_human")])

d_process <- d
d_process$label <- as.numeric(as.factor(d_process$label))
d_process$benefits <- as.numeric(as.factor(d_process$benefits))

## Number of final participants
n_comprehension <- nrow(d); n_comprehension

## Number excluded
n_attention - n_comprehension
## ================================================================================================================
##                                Participant Characteristics              
## ================================================================================================================
# AGE
mean(d$age) 

# GENDER
prop_male <- prop.table(table(d$gender))[[1]]; prop_male

## ================================================================================================================
##                                      ANALYSIS              
## ================================================================================================================

cronbach.alpha(d[,c("r_soft", "l_firm")])
cronbach.alpha(d[,c("r_human", "l_human")])

# FIRM
a <- aov(firm ~ as.factor(label) * as.factor(benefits), data = d)
summary(a)
anova_stats(a); anova_stats(a)$partial.etasq

## t-tests
### Absent Condition
t1 <- t.test(d[d$benefits == 'Absent' & d$label == 'auto',]$firm,
             d[d$benefits == 'Absent' & d$label == 'co',]$firm, paired = FALSE)
t1

sd(d[d$benefits == 'Absent' & d$label == 'auto',]$firm)
sd(d[d$benefits == 'Absent' & d$label == 'co',]$firm)

cohen.d(d[d$benefits == 'Absent' & d$label == 'auto',]$firm,
        d[d$benefits == 'Absent' & d$label == 'co',]$firm)

### Present Condition
t2 <- t.test(d[d$benefits == 'Present' & d$label == 'auto',]$firm,
              d[d$benefits == 'Present' & d$label == 'co',]$firm, paired = FALSE)
t2

sd(d[d$benefits == 'Present' & d$label == 'auto',]$firm)
sd(d[d$benefits == 'Present' & d$label == 'co',]$firm)

cohen.d(d[d$benefits == 'Present' & d$label == 'auto',]$firm,
       d[d$benefits == 'Present' & d$label == 'co',]$firm)

## Simple Mediation
if(mediation) process(data = d_process, y = "firm", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)


# HUMAN
a <- aov(human ~ as.factor(label) * as.factor(benefits), data = d)
summary(a)
anova_stats(a); anova_stats(a)$partial.etasq

## Absent Condition
t1 <- t.test(d[d$benefits == 'Absent' & d$label == 'auto',]$human,
             d[d$benefits == 'Absent' & d$label == 'co',]$human, paired = FALSE)
t1

sd(d[d$benefits == 'Absent' & d$label == 'auto',]$human)
sd(d[d$benefits == 'Absent' & d$label == 'co',]$human)

cohen.d(d[d$benefits == 'Absent' & d$label == 'auto',]$human,
        d[d$benefits == 'Absent' & d$label == 'co',]$human)

## Present Condition
t2 <- t.test(d[d$benefits == 'Present' & d$label == 'auto',]$human,
             d[d$benefits == 'Present' & d$label == 'co',]$human, paired = FALSE)
t2

sd(d[d$benefits == 'Present' & d$label == 'auto',]$human)
sd(d[d$benefits == 'Present' & d$label == 'co',]$human)

cohen.d(d[d$benefits == 'Present' & d$label == 'auto',]$human,
       d[d$benefits == 'Present' & d$label == 'co',]$human)

## Simple Mediation
if(mediation) process(data = d_process, y = "human", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## ================================================================================================================
##                                VISUALIZATION              
## ================================================================================================================

std.error <- function(x) sd(x)/sqrt(length(x))

d |>
  gather(key = "DV", value = "Value", 
         firm, human) |>
  mutate(
    DV = ifelse( DV == "firm", "Firm Liability", "Human Liability"),
    `Marketing Label` = case_when(
      label == "auto" ~ "Autopilot",
      label == "co" ~ "Copilot"
    ),
    Benefits = benefits
  ) |>
  group_by(`Marketing Label`, Benefits, DV) |>
  summarise( 
    mean = mean(Value),
    se = std.error(Value) 
  ) -> d_plot

plot_did <- function(df=d_plot, dv, signif=c("*","*","*"), yaxis=TRUE, ypos=c(100, 100, 114)) {
  
  d_plot <- df |>
    filter(DV == dv)
  
  se_width <- 1.96
  
  ggplot(data = d_plot, aes(x=Benefits, y=mean, fill=`Marketing Label`)) +
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
    xlab("Benefits") +
    ylab("Response") +
    scale_y_continuous(limits = c(0,118), breaks = c(0,20,40,60,80,100)) -> p
  
  if(!yaxis) {
    p <- p +
      theme( axis.line.y = element_line(color = "white"),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank())
  }
  
  return(p)
}
plot_did(dv = "Human Liability", signif = c("***", "***", "ns"), yaxis=F) -> p1
plot_did(dv = "Firm Liability", signif = c("***", "***", "ns"))  -> p2

ggarrange(p2 + rremove("ylab") + rremove("xlab"),
          p1 + rremove("ylab") + rremove("xlab"),
          ncol = 2, common.legend = TRUE) |>
  annotate_figure( left = textGrob("Mean Ratings", rot = 90, vjust = 1, gp = gpar(cex = .8, fontface = "bold")),
                   bottom = textGrob("Environmental Benefits Condition", gp = gpar(cex = .8, fontface = "bold"))) -> p

p

ggsave("env_benefits.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")
