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

mediation <- FALSE
if(mediation) source('../process.r')


## ================================================================================================================
##                                Exclusions and Pre-processing               
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
df |>
  filter(att_1 == 2, att_2 == 2) -> df

## Number Recruited
n_attention <- nrow(df); n_attention

# COMPREHENSION CHECKS 1 and 2
df |>
  filter(comp_1 == 2 & comp_2 == 4) -> df

auto <- df[,c("auto_1", "control_auto_10", "hands_off_auto_10", "watch_auto_10",
              "nap_auto_10", "comp_3...29", "unsafe_self_10",
              "worried_self_10", "unsafe_others_10", "worried_others_10", 
              "likely_others_10", "likely_self_10", "concern_others_10", 
              "concern_self_10", "TimeVehicleStopped", "ai_knowledge_1" ,"age", "gender")]

# Correcting for max Time Vehicle Stopped
auto |>
  drop_na(auto_1) |>
  mutate(TimeVehicleStopped = ifelse(is.na(TimeVehicleStopped), 19.36, TimeVehicleStopped),
         label = "auto") -> auto

co <- df[,c("co_1","control_co_10", "hands_off_co_10", "watch_co_10",
            "nap_co_10", "comp_3...35", "unsafe_self_10",
            "worried_self_10", "unsafe_others_10", "worried_others_10", 
            "likely_others_10", "likely_self_10", "concern_others_10",
            "concern_self_10", "TimeVehicleStopped", "ai_knowledge_1" ,"age", "gender")]

# Correcting for max Time Vehicle Stopped
co |>
  drop_na(co_1) |>
  mutate(TimeVehicleStopped = ifelse(is.na(TimeVehicleStopped), 19.36, TimeVehicleStopped),
         label = "co") -> co

column_names <- c("capability","control", "hands_off", "watch",
                  "nap", "comp_3", "unsafe_self", "worried_self", "unsafe_others",
                  "worried_others", "likely_others", "likely_self", "concern_others",
                  "concern_self", "time_control", "ai_knowledge" ,"age", "gender", "label")

colnames(auto) <- column_names
colnames(co) <- column_names

df <- rbind(auto, co)
rm(auto,co)

# COMPREHENSION CHECKS 3 & 4
df |>
  filter(comp_3 == 1) -> d

d$behavior <- rowMeans(d[, c("control", "hands_off", "watch", "nap")])
d$risk_aversion <- rowMeans(d[, c("unsafe_self", "worried_self", "unsafe_others", "worried_others", 
                                  "likely_others", "likely_self", "concern_others", "concern_self")])

# FOR PROCESS
d_process <- d
d_process$label <- as.numeric(as.factor(d_process$label))

## Number of final participants
n_comprehension <- nrow(d); n_comprehension

## Number Excluded
n_attention - n_comprehension

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
cronbach.alpha(d[, c("control", "hands_off", "watch", "nap")])
cronbach.alpha(d[, c("unsafe_self", "worried_self", "unsafe_others", "worried_others", 
                     "likely_others", "likely_self", "concern_others", "concern_self")])


# Distracted Intentions
t1 <- t.test(d[d$label == "auto",]$behavior, d[d$label == "co",]$behavior, paired = FALSE)
t1

sd(d[d$label == "auto",]$behavior)
sd(d[d$label == "co",]$behavior)

cohen.d(d[d$label == "auto",]$behavior, d[d$label == "co",]$behavior)

## Simple Mediation
if(mediation) process(data = d_process, y = "behavior", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Moderated Mediation (Risk Aversion) 
if(mediation) process(data = d_process, y = "behavior", x = "label", w = c("risk_aversion"),
        m =c("capability"), model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Moderated Mediation (AI Knowledge) 
if(mediation) process(data = d_process, y = "behavior", x = "label", w = "ai_knowledge",
        m =c("capability"), model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)


# Time to Take Control
t2 <- t.test(d[d$label == "auto",]$time_control, d[d$label == "co",]$time_control, paired = FALSE)
t2

sd(d[d$label == "auto",]$time_control)
sd(d[d$label == "co",]$time_control)

cohen.d(d[d$label == "auto",]$time_control, d[d$label == "co",]$time_control)

## Simple Mediation
if(mediation) process(data = d_process, y = "time_control", x = "label", 
        m =c("capability"), model = 4, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Moderated Mediation (Risk Aversion) 
if(mediation) process(data = d_process, y = "time_control", x = "label", w = c("risk_aversion"),
        m =c("capability"), model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Moderated Mediation (AI Knowledge) 
if(mediation) process(data = d_process, y = "time_control", x = "label", w = "ai_knowledge",
        m =c("capability"), model = 14, effsize = 1, total = 1, stand = 1, 
        contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

## Distribution of Time to Take Control
## Kolmogorov-Smirnov 
ks.test(d[d$label == "auto",]$time_control, d[d$label == "co",]$time_control)

## ================================================================================================================
##                                VISUALIZATION               
## ================================================================================================================

std.error <- function(x) sd(x)/sqrt(length(x))

d |>
  gather(key = "DV", value = "Value", 
         time_control, behavior) |>
  mutate(
    DV = ifelse( DV == "behavior", "Distraction Intention", "Time to Take Control"),
    `Marketing Label` = case_when(
      label == "auto" ~ "Autopilot",
      label == "co" ~ "Copilot"
    )
  ) |>
  group_by(`Marketing Label`, DV) |>
  summarise( 
    mean = mean(Value),
    se = std.error(Value) 
  ) -> d_plot


plot_did <- function(df=d_plot, dv, signif=c("*","*","*"), yaxis=TRUE, ypos=c(40)) {
  
  d_plot <- df |>
    filter(DV == dv)
  
  se_width <- 1.96
  
  ggplot(data = d_plot, aes(x=`Marketing Label`, y=mean)) +
    geom_bar(stat="summary", position="dodge", alpha=.75) +
    geom_errorbar(aes(ymin=mean-(se*se_width), ymax=mean+(se*se_width)), position = position_dodge(width=.9), 
                  size=.25, color="black", width=.5) +
    geom_point(aes(y=mean),position=position_dodge(width = .9), size=.5, color="black") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold", size=10)
    ) +
    geom_signif(
      y_position = ypos, xmin = c(1.0), xmax = c(2.0),
      annotation = signif, tip_length = 0.1, color='black', size = .25, textsize = 3.5 
    ) +
    scale_fill_grey() +
    scale_color_grey() +
    ggtitle(dv) +
    xlab("Marketing Label") +
    ylab("Response") -> p
  
   #+ scale_y_continuous(limits = c(0,118), breaks = c(0,20,40,60,80,100)) -> p
  
  if(!yaxis) {
    p <- p +
      theme( axis.line.y = element_line(color = "white"),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank())
  }
  
  return(p)
}

plot_did(dv = "Distraction Intention", signif = c("*"), yaxis=T) -> p1
p1 + theme(text = element_text(face = "bold")) 

ggsave("behavioral.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

plot_did(dv = "Time to Take Control", signif = c("**"), yaxis=T, ypos = 17)  +
  ylab("Response Time (s)") -> p2
p2 + theme(text = element_text(face = "bold"))

ggsave("control_time.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

ggarrange(p1 + ylab("Mean Rating") + rremove("xlab") + theme(text = element_text(face = "bold")),
          p2+ rremove("xlab") + theme(text = element_text(face = "bold")),
          ncol = 2, common.legend = TRUE)  |>
  annotate_figure(bottom = textGrob("Marketing Label", gp = gpar(cex = .8, fontface = "bold")))

ggsave("behavior_time.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

d  |>
  mutate(
    `Marketing Label` = case_when(
      label == "auto" ~ "Autopilot",
      label == "co" ~ "Copilot"
    )
  ) -> d_density

ggplot(data = d_density, aes(color =`Marketing Label`, x=time_control )) +
  stat_density(geom="line", position="identity", alpha=.75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=10), legend.position = 'top' ) + 
  scale_color_grey() +
  ylab("Density") +
  xlab("Time to Take Control (s)") + 
  ggplot2::annotate("rect", xmin = 5, xmax = 10, ymin = 0, ymax = .2,alpha = .1) +
  ggplot2::annotate("text", x = 7.5, y = .08, label = "Vehicle approaches intersection", size = 2) +
  ggplot2::annotate("rect", xmin = 15, xmax = 20, ymin = 0, ymax = .2,alpha = .1) +
  ggplot2::annotate("text", x = 17.5, y = .19, label = "Vehicle approaches jaywalkers", size = 2) +
  theme(legend.key = element_rect(fill = NA))

ggsave("time_density.pdf", device = "pdf",width = 5.3, height = 3.7, units = "in")


