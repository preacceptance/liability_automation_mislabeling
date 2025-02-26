## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
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
               'emmeans',         # cooprast analysis for regression models
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
               "tidyverse",
               "grid",
               'sjstats',
               'lavaan',
               'semTools'
)

# PROCESS Analysis (Set TRUE if you wish to run PROCESS code)
mediation <- FALSE

## ================================================================================================================
##                                                  EXCLUSIONS                
## ================================================================================================================

# Read full dataset
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read_csv("data.csv")
# Remove first two rows that were headers
df <- df[-c(1,2),]

# Rename column and labels
df$cond <- df$FL_12_DO
df$cond[df$cond == "FL_35"] <- "auto_op"
df$cond[df$cond == "FL_36"] <- "co_op"
df$cond[df$cond == "FL_50"] <- "auto_ft"
df$cond[df$cond == "FL_54"] <- "co_ft"

# exclusions
df |>
  filter(
    att_1 == 2,
    att_2 == 2 ) -> df

## Recruited Participants
recruited_participants <- dim(df)[1]; recruited_participants


## Comprehension Checks
df |>
  filter(
    comp_1 == 2,
    comp_2 == 4,
    comp_3 == 1
  ) -> df

df |>
  filter((cond == 'auto_op' & comp_4 == 2 |
            cond == 'auto_ft' & comp_4 == 3 |
            cond == 'co_op' & comp_4 == 2   | 
            cond == 'co_ft' & comp_4 == 3     )) -> df

## Number of final participants
final_n <- dim(df)[1]; final_n
## Number excluded
excluded <- recruited_participants - final_n; excluded

## ================================================================================================================
##                                                  PRE-PROCESSING                 
## ================================================================================================================

# Identify transparency and labels
df |>
  mutate(
    transparency = ifelse(grepl("op", cond), "no", "yes"),
    label = ifelse(grepl("auto", cond), "auto", "co"),
  ) -> df

# Get data for different groups
# Elongate dataset for visualization and regression purposes

std_colnames <- c("id", "cond", "transparency", "label", "resp_soft", "resp_human", 
                  "liable_firm", "liable_human", "capability","age", "gender", "ai_knowledge", "license")

auto_op <- df |>
  filter(cond == "auto_op") |>
  dplyr::select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}1_[0-9]{1,}"), auto_1, age, gender, ai_knowledge_1, license)
colnames(auto_op) <- std_colnames

auto_ft <- df |>
  filter(cond == "auto_ft") |>
  dplyr::select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}3_[0-9]{1,}"), auto_2, age, gender, ai_knowledge_1, license)

colnames(auto_ft) <- std_colnames

co_op <- df |>
  filter(cond == "co_op") |>
  dplyr::select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}2_[0-9]{1,}"), co_1, age, gender, ai_knowledge_1, license)
colnames(co_op) <- std_colnames

co_ft <- df |>
  filter(cond == "co_ft") |>
  dplyr::select(id, cond, transparency, label, matches("[a-z]{1,}_[a-z]{1,}4_[0-9]{1,}"), co_2, age, gender, ai_knowledge_1, license)
colnames(co_ft) <- std_colnames

d <- rbind(auto_ft, co_ft, co_op, auto_op)
rm(auto_ft, co_ft, co_op, auto_op)

d |>
  mutate_at(
    c("resp_soft", "resp_human", "liable_firm", "liable_human", "capability",
      "age", "gender", "license", "ai_knowledge"),
    as.numeric
  ) -> d


## ================================================================================================================
##                                              PARTICIPANT CHARACTERISTICS              
## ================================================================================================================
# AGE
mean(d[d$age < 150,]$age) # filtering the ones who put year

# GENDER
prop.table(table(d[d$gender < 3,]$gender))[1]

## ================================================================================================================
##                                              PARTICIPANT CHARACTERISTICS              
## ===============================================================================================================

cronbach.alpha(d[,c("resp_soft", "liable_firm")])
cronbach.alpha(d[,c("resp_human", "liable_human")])

## Discriminant Validity
## Reverse Coding Human
d |> mutate(
  hr = -(`resp_human` - 100),
  hl = -(`liable_human` - 100),
  fr = `resp_soft`,
  fl = `liable_firm`
) -> d

countf.model <- ' firm   =~ fr + fl
                  human  =~ hr + hl '

htmt(countf.model, d)

## Covariance Matrix
countf.cov <- cov(d[, c("fr", "fl", "hr", "hl")])

## HTMT using arithmetic mean
htmt(countf.model, sample.cov = countf.cov, htmt2 = FALSE)

d |>
  mutate(
    human = (resp_human + liable_human)/2,
    firm = (resp_soft + liable_firm)/2
  ) -> d

# FIRM LIABILITY
## ANOVA
f_anova <- aov(firm ~ as.factor(label) * as.factor(transparency), data = d)
summary(f_anova)
anova_stats(f_anova); anova_stats(f_anova)$partial.etasq

## t-tests

### Opaque Condition
t1 <- t.test(d[d$transparency == 'no' & d$label == 'auto',]$firm,
             d[d$transparency == 'no' & d$label == 'co',]$firm, paired = FALSE)
t1

sd(d[d$transparency == 'no'& d$label == 'auto',]$firm)
sd(d[d$transparency == 'no'& d$label == 'co',]$firm)

cohen.d(d[d$transparency == 'no'& d$label == 'auto',]$firm,
        d[d$transparency == 'no'& d$label == 'co',]$firm)

### Transparent Condition
t2 <- t.test(d[d$transparency == 'yes'& d$label == 'auto',]$firm,
             d[d$transparency == 'no'& d$label == 'auto',]$firm, paired = FALSE)
t2

sd(d[d$transparency == 'yes'& d$label == 'auto',]$firm)
sd(d[d$transparency == 'yes'& d$label == 'co',]$firm)

cohen.d(d[d$transparency == 'yes'& d$label == 'auto',]$firm,
        d[d$transparency == 'yes'& d$label == 'co',]$firm)


# HUMAN LIABILITY
## ANOVA
h_anova <- aov(human ~ as.factor(label) * as.factor(transparency), data = d)
summary(h_anova)
anova_stats(h_anova); anova_stats(h_anova)$partial.etasq

## t-tests

### Opaque Condition
t1 <- t.test(d[d$transparency == 'no' & d$label == 'auto',]$human,
             d[d$transparency == 'no' & d$label == 'co',]$human, paired = FALSE)
t1

sd(d[d$transparency == 'no' & d$label == 'auto',]$human)
sd(d[d$transparency == 'no' & d$label == 'co',]$human)

cohen.d(d[d$transparency == 'no'& d$label == 'auto',]$human,
        d[d$transparency == 'no'& d$label == 'co',]$human)

### Transparent Condition
t2 <- t.test(d[d$transparency == 'yes'& d$label == 'auto',]$human,
             d[d$transparency == 'yes'& d$label == 'co',]$human, paired = FALSE)
t2

sd(d[d$transparency == 'yes'& d$label == 'auto',]$human)
sd(d[d$transparency == 'yes'& d$label == 'co',]$human)

cohen.d(d[d$transparency == 'yes'& d$label == 'auto',]$human,
       d[d$transparency == 'yes'& d$label == 'co',]$human)


## ================================================================================================================
##                                                 PROCESS ANALYSIS              
## ================================================================================================================
d |>
  mutate_at( c("transparency", "label"), as.factor) |>
  mutate_at( c("transparency", "label"), as.numeric) -> d_process

if(mediation) {
  
source('../process.R')

## NOTE
### - transparency no ~ 1, yes ~ 2
### - label auto ~ 1, co ~ 2

# MODERATED MEDIATION
## FIRM LIABILITY
process(data = d_process, y = "firm", x = "label", 
        m =c("capability"), w="transparency", model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## HUMAN LIABILITY
process(data = d_process, y = "human", x = "label", 
        m =c("capability"), w="transparency", model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

}

## ================================================================================================================
##                                                 VISUALIZATION              
## ================================================================================================================
d |>
  mutate(
    Label = ifelse( label == "co", "Copilot", "Autopilot"),
    `Firm Liability` = firm,
    Transparency = ifelse(transparency == "no", "Opaque", "Transparent")
  ) -> d_plot

ggplot(d_plot, aes(x = Label, y = `Firm Liability`, fill = Transparency)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.5,  position="dodge") +  # Bar plot
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5, position = position_dodge(width=.9)) +   # Error bars
  geom_jitter(alpha = 0.2, size = .1, color = "#00003B", position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9)) +  # Individual data points
  scale_fill_grey() +
  scale_color_grey() +
  theme_classic() + 
  theme(text = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, size=10), legend.position = "top", legend.title = element_blank(), legend.text = element_text(hjust = 0.5, size=8)) +
  xlab("Label") +
  ylab("Ratings") +
  ggtitle("Firm Liability") +
  geom_signif(
    y_position = c(107,107,120), xmin = c(0.8, 1.8, 1.0), xmax = c(1.2, 2.2, 2.0),
    annotation = c("***", "***", "."), tip_length = 0.03, color='black', size = .25, textsize = 3.5 
  )  +
  scale_y_continuous(limits = c(0,125), breaks = c(0,20,40,60,80,100)) -> p1

p1

ggsave("FirmTransparency.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

## ================================================================================================================
##                                                 VISUALIZATION (OLD)          
## ================================================================================================================
# dev.off()
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
    Transparency = case_when(
                    transparency == "no" ~ "Opaque",
                    transparency == "yes" ~ "Transparent"
    )
  ) |>
  group_by(`Marketing Label`, Transparency, DV) |>
  summarise( 
    mean = mean(Value),
    se = std.error(Value) 
    ) -> d_plot

plot_did <- function(df=d_plot, dv, signif=c("*","*","*"), yaxis=TRUE, ypos=c(100, 100, 114)) {
  
  d_plot <- df |>
    filter(DV == dv)
  
  se_width <- 1.96
  
  ggplot(data = d_plot, aes(x=Transparency, y=mean, fill=`Marketing Label`)) +
    geom_bar(stat="identity", position="dodge", alpha=.75) +
    geom_errorbar(aes(ymin=mean-(se*se_width), ymax=mean+(se*se_width)), position = position_dodge(width=.9), 
                  size=.25, color="black", width=.5) +
    geom_point(aes(y=mean),position=position_dodge(width = .9), size=.5, color="black") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold", size=10)
          ) +
    geom_signif(
      y_position = ypos, xmin = c(0.8, 1.8, 1.0), xmax = c(1.2, 2.2, 2.0),
      annotation = signif, tip_length = 0.1, color='black', size = .25, textsize = 3.5 
    ) +
    scale_fill_grey() +
    scale_color_grey() +
    ggtitle(dv) +
    xlab("Transparency") +
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

plot_did(dv = "Human Liability", signif = c("*", "ns", "*"), yaxis=F) -> p1
plot_did(dv = "Firm Liability", signif = c("+", "ns", "+"), ypos = c(80,80,94))  -> p2

ggarrange(p2 + rremove("ylab") + rremove("xlab"),
          p1 + rremove("ylab") + rremove("xlab"),
          ncol = 2, common.legend = TRUE) |>
          annotate_figure( left = textGrob("Mean Ratings", rot = 90, vjust = 1, gp = gpar(cex = .8, fontface = "bold")),
                           bottom = textGrob("Firm Transparency Condition", gp = gpar(cex = .8, fontface = "bold"))) -> p
p

ggsave("firm_transparency.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")


## ================================================================================================================
##                                                 END OF ANALYSIS              
## ================================================================================================================