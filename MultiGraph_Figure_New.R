### Code for arranging multiple graphs into one figure #####


# library
library(multcompView)
library(ggplot2)
library(olsrr)
library(agricolae)
library(ggpubr)
library(ggdist)
library(gghalves)
library(labeling)
library(lemon)
library(ggalignment)
library(ggimage)
library(gridExtra)
library(tibble)
library(flextable)
library(cowplot)
library(dplyr)




## Import Data ##
library(readxl)
AllData_R <- read_excel("Projects/Grape Seed/Data/AllData_R.xlsx")
View(AllData_R)

Data <- AllData_R
print(colnames(Data))





######## Following code is for volume data #######################

## Integument Volume ##

factor1 <- (Data$Fermentation_Stage)
A <- (Data$integument_volume)
perimeter_pixels1 <- (Data$integument_volume)

model <- lm(A ~ factor1)
ANOVA <- aov(model)
print(ANOVA)
summary(ANOVA)


TUKEY <- TukeyHSD(x=ANOVA, "factor1", conf.level=0.95)

# # # # # Tukey test representation :
plot(TUKEY)
print(TUKEY)


# # # # # Tukey test representation :
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labelsfactor1=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labelsfactor1) , ]
  return(Tukey.labels)
}

summary(ANOVA)
tukey.test <- TukeyHSD(ANOVA)
tukey.test
tukey.test2 <- HSD.test(ANOVA, trt='factor1')
print(tukey.test2)

# Apply the significant difference stat function (a,b,c) on my data set
## LABELS is defined as these sig fig letters
LABELS <- generate_label_df(TUKEY , "factor1")
print(LABELS)


treatment <- as.factor(factor1)

# Draw the basic boxplot with no axis titles
## Fill in box plot colors
a1 <- ggplot(Data, aes(x = treatment, y = perimeter_pixels1, fill = treatment)) + 
  
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  
  coord_cartesian(ylim=c(0,7), xlim = c(1.2, NA), clip = "off") +
  annotate("text", x = c('Early Fermentation', 'Late Fermentation', 'Pre Fermentation'), y=7, label = c(LABELS)) +
  labs(title = "A. Integument Volume", size = 10, x = NULL, y = NULL) +  
  geom_boxplot(width=0.5, lwd=1.0) +
  #geom_text(x = 40, y = 20, label = "Total Pore Space", size = 20/.pt, inherit.aes = FALSE) +
  ###  geom_dotplot(binaxis = 'y', stackdir = 'center') if you want the dots, geom_jitter if you want random variation and no overlap ##
  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(size = 10), legend.position = "none")

##theme(plot.title=element_text(hjust = 1, vjust = 0.5, margin = margin(t=40, b=-18)), aspect.ratio = 4/3,
##  axis.text.x = element_blank(),
## axis.ticks.x = element_blank())

a1

b1 <- a1+scale_x_discrete(limits = c('Pre Fermentation', 'Early Fermentation', 'Late Fermentation'))
b1

c1 <- b1 + scale_fill_manual(name = "Fermentation Stage", 
                        limits = c("Pre Fermentation", "Early Fermentation", "Late Fermentation"),
                        values = c("#00AFBB", "#E7B800", "#FC4E07"))
c1




                              ## Endosperm Volume ##

factor1 <- (Data$Fermentation_Stage)
B <- (Data$endosperm_volume)
perimeter_pixels2 <- (Data$endosperm_volume)

model <- lm(B ~ factor1)
ANOVA <- aov(model)
print(ANOVA)
summary(ANOVA)

# # # Tukey test to study each pair of treatment :

TUKEY <- TukeyHSD(x=ANOVA, "factor1", conf.level=0.95)

# # # # # Tukey test representation :
plot(TUKEY)
print(TUKEY)
tuk_plot(TUKEY, "Differences in Mean Levels", "Fermentation Stage Comparison", c("PF-LF", "PF-EF", "LF-EF"))
print(TUKEY)

# # # # # Tukey test representation :
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labelsfactor1=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labelsfactor1) , ]
  return(Tukey.labels)
}

summary(ANOVA)
tukey.test <- TukeyHSD(ANOVA)
tukey.test
tukey.test2 <- HSD.test(ANOVA, trt='factor1')
print(tukey.test2)

# Apply the significant difference stat function (a,b,c) on my data set
## LABELS is defined as these sig fig letters
LABELS <- generate_label_df(TUKEY , "factor1")
print(LABELS)


treatment <- as.factor(factor1)

# Draw the basic boxplot with no axis titles
## Fill in box plot colors
a2 <- ggplot(Data, aes(x = treatment, y = perimeter_pixels2, fill = treatment)) + 
  
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  
  
  coord_cartesian(ylim=c(0,7), xlim = c(1.2, NA), clip = "off") + 
  annotate("text", x = c('Early Fermentation', 'Late Fermentation', 'Pre Fermentation'), y=7, label = c(LABELS)) +
  labs(title = "C. Endosperm Volume", size = 10, x = NULL, y = NULL) +  
  geom_boxplot(width=0.5, lwd=1.0) +
  #geom_text(x = 40, y = 20, label = "Total Air Space", size = 20/.pt, inherit.aes = FALSE) +
  ###  geom_dotplot(binaxis = 'y', stackdir = 'center') if you want the dots, geom_jitter if you want random variation and no overlap ##
  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(size = 10), legend.position = "none")

##theme(plot.title=element_text(hjust = 1, vjust = 0.5, margin = margin(t=40, b=-18)), aspect.ratio = 4/3,
##  axis.text.x = element_blank(),
## axis.ticks.x = element_blank())

a2

b2 <- a2+scale_x_discrete(limits = c('Pre Fermentation', 'Early Fermentation', 'Late Fermentation')) 
b2

c2 <- b2 + scale_fill_manual(name = "Fermentation Stage", 
                             limits = c("Pre Fermentation", "Early Fermentation", "Late Fermentation"),
                             values = c("#00AFBB", "#E7B800", "#FC4E07"))
c2





                              ## Endosperm Pore Space Volume ##

factor1 <- (Data$Fermentation_Stage)
C <- (Data$endosperm_pore_space_volume)
perimeter_pixels3 <- (Data$endosperm_pore_space_volume)

model <- lm(C ~ factor1)
ANOVA <- aov(model)
print(ANOVA)
summary(ANOVA)

# # # Tukey test to study each pair of treatment :

TUKEY <- TukeyHSD(x=ANOVA, "factor1", conf.level=0.95)

# # # # # Tukey test representation :
plot(TUKEY)
print(TUKEY)
tuk_plot(TUKEY, "Differences in Mean Levels", "Fermentation Stage Comparison", c("PF-LF", "PF-EF", "LF-EF"))
print(TUKEY)

# # # # # Tukey test representation :
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labelsfactor1=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labelsfactor1) , ]
  return(Tukey.labels)
}

summary(ANOVA)
tukey.test <- TukeyHSD(ANOVA)
tukey.test
tukey.test2 <- HSD.test(ANOVA, trt='factor1')
print(tukey.test2)

# Apply the significant difference stat function (a,b,c) on my data set
## LABELS is defined as these sig fig letters
LABELS <- generate_label_df(TUKEY , "factor1")
print(LABELS)


treatment <- as.factor(factor1)

# Draw the basic boxplot with no axis titles
## Fill in box plot colors
a3 <- ggplot(Data, aes(x = treatment, y = perimeter_pixels3, fill = treatment)) + 
  
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  
  
  coord_cartesian(ylim=c(0,7), xlim = c(1.2, NA), clip = "off") + 
  annotate("text", x = c('Early Fermentation', 'Late Fermentation', 'Pre Fermentation'), y=7, label = c(LABELS)) +
  labs(title = "E. Endosperm Air Space Volume", size = 10, x = NULL, y = NULL) +  
  geom_boxplot(width=0.5, lwd=1.0) +
  #geom_text(x = 40, y = 20, label = "Total Air Space", size = 20/.pt, inherit.aes = FALSE) +
  ###  geom_dotplot(binaxis = 'y', stackdir = 'center') if you want the dots, geom_jitter if you want random variation and no overlap ##
  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(size = 10), legend.position = "none")

##theme(plot.title=element_text(hjust = 1, vjust = 0.5, margin = margin(t=40, b=-18)), aspect.ratio = 4/3,
##  axis.text.x = element_blank(),
## axis.ticks.x = element_blank())

a3

b3 <- a3+scale_x_discrete(limits = c('Pre Fermentation', 'Early Fermentation', 'Late Fermentation')) 
b3

c3 <- b3 + scale_fill_manual(name = "Fermentation Stage", 
                             limits = c("Pre Fermentation", "Early Fermentation", "Late Fermentation"),
                             values = c("#00AFBB", "#E7B800", "#FC4E07"))
c3






                    ## Total Pore Space Volume ##

factor1 <- (Data$Fermentation_Stage)
D <- (Data$total_pore_space_volume)
perimeter_pixels4 <- (Data$total_pore_space_volume)

model <- lm(D ~ factor1)
ANOVA <- aov(model)
print(ANOVA)
summary(ANOVA)

# # # Tukey test to study each pair of treatment :

TUKEY <- TukeyHSD(x=ANOVA, "factor1", conf.level=0.95)

# # # # # Tukey test representation :
plot(TUKEY)
print(TUKEY)
tuk_plot(TUKEY, "Differences in Mean Levels", "Fermentation Stage Comparison", c("PF-LF", "PF-EF", "LF-EF"))
print(TUKEY)

# # # # # Tukey test representation :
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labelsfactor1=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labelsfactor1) , ]
  return(Tukey.labels)
}

summary(ANOVA)
tukey.test <- TukeyHSD(ANOVA)
tukey.test
tukey.test2 <- HSD.test(ANOVA, trt='factor1')
print(tukey.test2)

# Apply the significant difference stat function (a,b,c) on my data set
## LABELS is defined as these sig fig letters
LABELS <- generate_label_df(TUKEY , "factor1")
print(LABELS)


treatment <- as.factor(factor1)

# Draw the basic boxplot with no axis titles
## Fill in box plot colors
a4 <- ggplot(Data, aes(x = treatment, y = perimeter_pixels4, fill = treatment)) + 
  
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  
  
  coord_cartesian(ylim=c(0,7), xlim = c(1.2, NA), clip = "off") + 
  annotate("text", x = c('Early Fermentation', 'Late Fermentation', 'Pre Fermentation'), y=7, label = c(LABELS)) +
  labs(title = "G. Total Air Space Volume", size = 10, x = NULL, y = NULL) +  
  geom_boxplot(width=0.5, lwd=1.0) +
  #geom_text(x = 40, y = 20, label = "Total Air Space", size = 20/.pt, inherit.aes = FALSE) 
  ###  geom_dotplot(binaxis = 'y', stackdir = 'center') if you want the dots, geom_jitter if you want random variation and no overlap ##
  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(size = 10), legend.position = "none")
  
  ##theme(plot.title=element_text(hjust = 1, vjust = 0.5, margin = margin(t=40, b=-18)), aspect.ratio = 4/3,
      ##  axis.text.x = element_blank(),
        ## axis.ticks.x = element_blank())

a4

b4 <- a4+scale_x_discrete(limits = c('Pre Fermentation', 'Early Fermentation', 'Late Fermentation')) 
b4

c4 <- b4 + scale_fill_manual(name = "Fermentation Stage", 
                             limits = c("Pre Fermentation", "Early Fermentation", "Late Fermentation"),
                             values = c("#00AFBB", "#E7B800", "#FC4E07"))
c4




                ### Surface Area Data ######


              ## Integument Surface Area ##

factor2 <- (Data$Fermentation_Stage)
E <- (Data$integument_SA)
perimeter_pixels5 <- (Data$integument_SA)

model <- lm(E ~ factor2)
ANOVA <- aov(model)
print(ANOVA)
summary(ANOVA)

# # # Tukey test to study each pair of treatment :

TUKEY <- TukeyHSD(x=ANOVA, "factor2", conf.level=0.95)

# # # # # Tukey test representation :
plot(TUKEY)
print(TUKEY)
tuk_plot(TUKEY, "Differences in Mean Levels", "Fermentation Stage Comparison", c("PF-LF", "PF-EF", "LF-EF"))
print(TUKEY)

# # # # # Tukey test representation :
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labelsfactor2=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labelsfactor2) , ]
  return(Tukey.labels)
}

summary(ANOVA)
tukey.test <- TukeyHSD(ANOVA)
tukey.test
tukey.test2 <- HSD.test(ANOVA, trt='factor2')
print(tukey.test2)

# Apply the significant difference stat function (a,b,c) on my data set
## LABELS is defined as these sig fig letters
LABELS <- generate_label_df(TUKEY , "factor2")
print(LABELS)


treatment2 <- as.factor(factor2)

# Draw the basic boxplot with no axis titles
## Fill in box plot colors
a5 <- ggplot(Data, aes(x = treatment2, y = perimeter_pixels5, fill = treatment2)) + 
  
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  
  
  coord_cartesian(ylim=c(0,80), xlim = c(1.2, NA), clip = "off") + 
  annotate("text", x = c('Early Fermentation', 'Late Fermentation', 'Pre Fermentation'), y=80, label = c(LABELS)) +
  labs(title = "B. Integument Surface Area", size= 10, x = NULL, y = NULL) +  
  geom_boxplot(width=0.5, lwd=1.0) +
  #geom_text(x = 40, y = 20, label = "Total Air Space", size = 20/.pt, inherit.aes = FALSE) 
  ###  geom_dotplot(binaxis = 'y', stackdir = 'center') if you want the dots, geom_jitter if you want random variation and no overlap ##
  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(size = 10), legend.position = "none")

##theme(plot.title=element_text(hjust = 1, vjust = 0.5, margin = margin(t=40, b=-18)), aspect.ratio = 4/3,
##  axis.text.x = element_blank(),
## axis.ticks.x = element_blank())

a5

b5 <- a5+scale_x_discrete(limits = c('Pre Fermentation', 'Early Fermentation', 'Late Fermentation')) 
b5

c5 <- b5 + scale_fill_manual(name = "Fermentation Stage", 
                             limits = c("Pre Fermentation", "Early Fermentation", "Late Fermentation"),
                             values = c("#00AFBB", "#E7B800", "#FC4E07"))
c5




                  ## Endosperm Surface Area ##

factor2 <- (Data$Fermentation_Stage)
F <- (Data$endosperm_SA)
perimeter_pixels6 <- (Data$endosperm_SA)

model <- lm(F ~ factor2)
ANOVA <- aov(model)
print(ANOVA)
summary(ANOVA)

# # # Tukey test to study each pair of treatment :

TUKEY <- TukeyHSD(x=ANOVA, "factor2", conf.level=0.95)

# # # # # Tukey test representation :
plot(TUKEY)
print(TUKEY)
tuk_plot(TUKEY, "Differences in Mean Levels", "Fermentation Stage Comparison", c("PF-LF", "PF-EF", "LF-EF"))
print(TUKEY)

# # # # # Tukey test representation :
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labelsfactor2=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labelsfactor2) , ]
  return(Tukey.labels)
}

summary(ANOVA)
tukey.test <- TukeyHSD(ANOVA)
tukey.test
tukey.test2 <- HSD.test(ANOVA, trt='factor2')
print(tukey.test2)

# Apply the significant difference stat function (a,b,c) on my data set
## LABELS is defined as these sig fig letters
LABELS <- generate_label_df(TUKEY , "factor2")
print(LABELS)


treatment2 <- as.factor(factor2)

# Draw the basic boxplot with no axis titles
## Fill in box plot colors
a6 <- ggplot(Data, aes(x = treatment2, y = perimeter_pixels6, fill = treatment2)) + 
  
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  
  
  coord_cartesian(ylim=c(0,80), xlim = c(1.2, NA), clip = "off") + 
  annotate("text", x = c('Early Fermentation', 'Late Fermentation', 'Pre Fermentation'), y=80, label = c(LABELS)) +
  labs(title = "D. Endosperm Surface Area", size = 10, x = NULL, y = NULL) +  
  geom_boxplot(width=0.5, lwd=1.0) +
  #geom_text(x = 40, y = 20, label = "Total Air Space", size = 20/.pt, inherit.aes = FALSE) 
  ###  geom_dotplot(binaxis = 'y', stackdir = 'center') if you want the dots, geom_jitter if you want random variation and no overlap ##
  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(size = 10), legend.position = "none")

##theme(plot.title=element_text(hjust = 1, vjust = 0.5, margin = margin(t=40, b=-18)), aspect.ratio = 4/3,
##  axis.text.x = element_blank(),
## axis.ticks.x = element_blank())

a6

b6 <- a6 + scale_x_discrete(limits = c('Pre Fermentation', 'Early Fermentation', 'Late Fermentation')) 
b6

c6 <- b6 + scale_fill_manual(name = "Fermentation Stage", 
                             limits = c("Pre Fermentation", "Early Fermentation", "Late Fermentation"),
                             values = c("#00AFBB", "#E7B800", "#FC4E07"))
c6




                ## Endosperm Pore Space Surface Area ##

factor2 <- (Data$Fermentation_Stage)
G <- (Data$endosperm_pore_space_SA)
perimeter_pixels7 <- (Data$endosperm_pore_space_SA)

model <- lm(G ~ factor2)
ANOVA <- aov(model)
print(ANOVA)
summary(ANOVA)

# # # Tukey test to study each pair of treatment :

TUKEY <- TukeyHSD(x=ANOVA, "factor2", conf.level=0.95)

# # # # # Tukey test representation :
plot(TUKEY)
print(TUKEY)
tuk_plot(TUKEY, "Differences in Mean Levels", "Fermentation Stage Comparison", c("PF-LF", "PF-EF", "LF-EF"))
print(TUKEY)

# # # # # Tukey test representation :
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labelsfactor2=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labelsfactor2) , ]
  return(Tukey.labels)
}

summary(ANOVA)
tukey.test <- TukeyHSD(ANOVA)
tukey.test
tukey.test2 <- HSD.test(ANOVA, trt='factor2')
print(tukey.test2)

# Apply the significant difference stat function (a,b,c) on my data set
## LABELS is defined as these sig fig letters
LABELS <- generate_label_df(TUKEY , "factor2")
print(LABELS)


treatment2 <- as.factor(factor2)

# Draw the basic boxplot with no axis titles
## Fill in box plot colors
a7 <- ggplot(Data, aes(x = treatment2, y = perimeter_pixels7, fill = treatment2)) + 
  
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  
  
  coord_cartesian(ylim=c(0,80), xlim = c(1.2, NA), clip = "off") + 
  annotate("text", x = c('Early Fermentation', 'Late Fermentation', 'Pre Fermentation'), y=80, label = c(LABELS)) +
  labs(title = "F. Endosperm Pore Space Surface Area", size = 10, x = NULL, y = NULL) +  
  geom_boxplot(width=0.5, lwd=1.0) +
  #geom_text(x = 40, y = 20, label = "Total Air Space", size = 20/.pt, inherit.aes = FALSE) 
  ###  geom_dotplot(binaxis = 'y', stackdir = 'center') if you want the dots, geom_jitter if you want random variation and no overlap ##
  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(size = 10), legend.position = "none")

##theme(plot.title=element_text(hjust = 1, vjust = 0.5, margin = margin(t=40, b=-18)), aspect.ratio = 4/3,
##  axis.text.x = element_blank(),
## axis.ticks.x = element_blank())

a7

b7 <- a7 + scale_x_discrete(limits = c('Pre Fermentation', 'Early Fermentation', 'Late Fermentation')) 
b7

c7 <- b7 + scale_fill_manual(name = "Fermentation Stage", 
                             limits = c("Pre Fermentation", "Early Fermentation", "Late Fermentation"),
                             values = c("#00AFBB", "#E7B800", "#FC4E07"))
c7


                      ## Total Pore Space Surface Area ##

factor2 <- (Data$Fermentation_Stage)
H <- (Data$total_pore_space_SA)
perimeter_pixels8 <- (Data$total_pore_space_SA)

model <- lm(H ~ factor2)
ANOVA <- aov(model)
print(ANOVA)
summary(ANOVA)

# # # Tukey test to study each pair of treatment :

TUKEY <- TukeyHSD(x=ANOVA, "factor2", conf.level=0.95)

# # # # # Tukey test representation :
plot(TUKEY)
print(TUKEY)
tuk_plot(TUKEY, "Differences in Mean Levels", "Fermentation Stage Comparison", c("PF-LF", "PF-EF", "LF-EF"))
print(TUKEY)

# # # # # Tukey test representation :
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labelsfactor2=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labelsfactor2) , ]
  return(Tukey.labels)
}

summary(ANOVA)
tukey.test <- TukeyHSD(ANOVA)
tukey.test
tukey.test2 <- HSD.test(ANOVA, trt='factor2')
print(tukey.test2)

# Apply the significant difference stat function (a,b,c) on my data set
## LABELS is defined as these sig fig letters
LABELS <- generate_label_df(TUKEY , "factor2")
print(LABELS)


treatment2 <- as.factor(factor2)

# Draw the basic boxplot with no axis titles
## Fill in box plot colors
a8 <- ggplot(Data, aes(x = treatment2, y = perimeter_pixels8, fill = treatment2)) + 
  
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  
  
  coord_cartesian(ylim=c(0,80), xlim = c(1.2, NA), clip = "off") + 
  annotate("text", x = c('Early Fermentation', 'Late Fermentation', 'Pre Fermentation'), y=80, label = c(LABELS)) +
  labs(title = "H. Total Pore Space Surface Area", x = NULL, y = NULL) +  
  geom_boxplot(width=0.5, lwd=1.0) +
  #geom_text(x = 40, y = 20, label = "Total Air Space", size = 20/.pt, inherit.aes = FALSE) 
  ###  geom_dotplot(binaxis = 'y', stackdir = 'center') if you want the dots, geom_jitter if you want random variation and no overlap ##
  
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(size = 10), legend.position = "none")

##theme(plot.title=element_text(hjust = 1, vjust = 0.5, margin = margin(t=40, b=-18)), aspect.ratio = 4/3,
##  axis.text.x = element_blank(),
## axis.ticks.x = element_blank())

a8

b8 <- a8 + scale_x_discrete(limits = c('Pre Fermentation', 'Early Fermentation', 'Late Fermentation')) 
b8

c8 <- b8 + scale_fill_manual(name = "Fermentation Stage", 
                             limits = c("Pre Fermentation", "Early Fermentation", "Late Fermentation"),
                             labels = c("Field", "Fermentation Start", "Fermentation End"),
                             values = c("#00AFBB", "#E7B800", "#FC4E07"))
c8

a8_legend <- a8 <- ggplot(Data, aes(x = treatment2, y = perimeter_pixels8, fill = treatment2)) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  coord_cartesian(ylim=c(0,80), xlim = c(1.2, NA), clip = "off") + 
  annotate("text", x = c('Early Fermentation', 'Late Fermentation', 'Pre Fermentation'), y=80, label = c(LABELS)) +
  labs(title = "H. Total Pore Space Surface Area", x = NULL, y = NULL) +  
  geom_boxplot(width=0.5, lwd=1.0) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(size = 10), legend.position = "bottom")

b8_legend <- a8_legend + scale_x_discrete(limits = c('Pre Fermentation', 'Early Fermentation', 'Late Fermentation')) 


c8_legend <- b8_legend + scale_fill_manual(name = "Fermentation Stage", 
                                           limits = c("Pre Fermentation", "Early Fermentation", "Late Fermentation"),
                                           labels = c("Field", "Fermentation Start", "Fermentation End"),
                                           values = c("#00AFBB", "#E7B800", "#FC4E07"))
c8_legend

extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}
  
shared_legend <- extract_legend(c8_legend)


##### Number Key #####
 ## c1 = integument volume, c2 = endosperm volume, c3 = endosperm air space volume, c4 = total air space volume
  ## c5 = integument SA, c6 = endosperm SA, c7 = endosperm air space SA, c8 = total air space SA


##### Testing arrange plot


bigfig <- plot_grid(c1, c5, c2, c6, c3, c7, c4, c8,
          ncol = 2, nrow = 4) +
  #labs(title = "Changes in Grape Seed Morphology",
   #    subtitle = "Across the Grape Seed Fermentation Lifecycle") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.text.x.bottom = element_text("Fermentation Stage"))

bigfig
annotate_figure(bigfig, 
                left = text_grob(expression(bold("Volume"*" (mm"^3*")")), rot = 90),
                right = text_grob(expression(bold("Surface Area"*" (mm"^2*")")), rot = -90),
                bottom = shared_legend)








