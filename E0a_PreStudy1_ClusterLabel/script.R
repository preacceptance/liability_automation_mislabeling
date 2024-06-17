rm(list = ls())

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('tidyverse')   
                
##=============================================================================
#                             Data Cleaning and Exclusions
##=============================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read_csv("data.csv")
marketing_labels <- substr(df[1,24:48], 193,2000)

## Attention Checks
df |>
  filter(att_1 == 2, att_2 == 5) -> df

nrow(df)

## Comp Checks
df |>
  filter(comp_1 == 2, comp_2 == 4, comp_3 == 3) |>
  mutate(age = as.numeric(age)) -> df

nrow(df)

##=============================================================================
#                             Demographics
##=============================================================================
# Prop male
prop.table(table(df$gender))[1]
# Mean Age
mean(df$age[df$age < 1000])

##=============================================================================
#                             Plot Dendograms
##=============================================================================
d <- df[,24:48]

colnames(d) <- marketing_labels

d |>
  gather(key = "Label", value = "Capability") |>
  mutate(
    Capability = as.numeric(Capability),
    Label = ifelse(Label == "Full-Self Driving", "Full Self-Driving", Label),
    Label = ifelse(Label == "Conditional Automation", "   Conditional Automation", Label)) |>
  group_by(Label) |>
  summarize(
    Mean = mean(Capability),
    Median = median(Capability),
    SE = sd(Capability)/n()) -> d_plot

marketing_labels <- d_plot$Label

# Remove Copilot and Autopilot for plotting purposes as labels
marketing_labels[6] <- ""
marketing_labels[5] <- ""

## Median Dendrogram
dist_mat <- dist(d_plot$Median, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average') 
hclust_avg$labels <- marketing_labels

pdf('MedianDendogram.pdf', width = 11, height = 5)
plot(hclust_avg, main = "Clusters of Marketing Labels",
          ylab = "", xlab = "Marketing Label", sub = "")
rect(xleft = 0.6, ybottom = -4.45, xright = 11.35, ytop = .4, lty = "dashed")
rect(xleft = 11.65, ybottom = -4.45, xright = 19.35, ytop = .4, lty = "dashed")
rect(xleft = 19.65, ybottom = -4.45, xright = 24.5, ytop = .4, lty = "dashed")
text(x = 8, y = -1.2, substitute(paste(bold("Copilot"))), srt = 90,)
text(x = 20, y = -1.4, substitute(paste(bold("Autopilot"))), srt = 90,)
dev.off()

## Mean Dendrogram
dist_mat <- dist(d_plot$Mean, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average') 
hclust_avg$labels <- marketing_labels

pdf('MeanDendogram.pdf', width = 11, height = 5)
hclust_avg$order <- rev(hclust_avg$order)
plot(hclust_avg, main = "Clusters of Marketing Labels",
     ylab = "", xlab = "Marketing Label", sub = "")
rect(xleft = 0.6, ybottom = -3.2, xright = 14.35, ytop = 1, lty = "dashed")
rect(xleft = 14.65, ybottom = -3.2, xright = 19.35, ytop = 1, lty = "dashed")
rect(xleft = 19.65, ybottom = -3.2, xright = 24.5, ytop = 1, lty = "dashed")
text(x = 9, y = -.8, substitute(paste(bold("Copilot"))), srt = 90,)
text(x = 23, y = -.8, substitute(paste(bold("Autopilot"))), srt = 90,)
dev.off()


plot(hclust_avg, main = "Clusters of Marketing Labels",
     ylab = "", xlab = "Marketing Label", sub = "")

##=============================================================================
#                             Analysis by Clusters
##=============================================================================

first_cluster <- c("Conditional Automation", "AcuraWatch",               
                  "Apollo", "Copilot", "Drive Wise", "Eyesight Driver",          
                   "Lane Sense", "Mobile Teammate",
                   "Partial Automation", "Safety System+","Super Cruise")

second_cluster <- c("Active Driving Assist", "Drive Pilot", "Highway Pilot",
                    "ProPilot", "Ride Pilot", "Sensing Elite", "Smart Sense", "XPilot")

third_cluster <- c("Autopilot", "Full Automation", "Full Self-Driving",
                    "High Automation", "Self-Driving")

d |>
  gather(key = "Label", value = "Capability") |>
  mutate(
    Capability = as.numeric(Capability),
    Label = ifelse(Label == "Full-Self Driving", "Full Self-Driving", Label),
    Cluster = case_when(
      Label %in% first_cluster ~ 1,
      Label %in% second_cluster ~ 2,
      Label %in% third_cluster ~ 3
    )) -> d

# First Cluster and Second
t.test(d[d$Cluster == 1,]$Capability, d[d$Cluster == 2,]$Capability)
sd(d[d$Cluster == 1,]$Capability)
sd(d[d$Cluster == 2,]$Capability)


# Second Cluster and Third
t.test(d[d$Cluster == 2,]$Capability, d[d$Cluster == 3,]$Capability)
sd(d[d$Cluster == 2,]$Capability)
sd(d[d$Cluster == 3,]$Capability)

# First Cluster and Third
t.test(d[d$Cluster == 1,]$Capability, d[d$Cluster == 3,]$Capability)
sd(d[d$Cluster == 1,]$Capability)
sd(d[d$Cluster == 3,]$Capability)

## t-test
t.test(d[d$Label == "Autopilot",]$Capability, d[d$Label == "Copilot",]$Capability)

## Autopilot
mean(d[d$Label == "Autopilot",]$Capability)
sd(d[d$Label == "Autopilot",]$Capability)

## Copilot
mean(d[d$Label == "Copilot",]$Capability)
sd(d[d$Label == "Copilot",]$Capability)


