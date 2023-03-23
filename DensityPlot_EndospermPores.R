### Approaching endosperm pore space data ###
# Non parametric data: data that does not fit a known or well-understood distribution
# Assumption of data: seeds are representative of population of seeds in vineyard (seeds at large)
# Approach:
# 1A. Combine all replicate data
# 1B. Create density plot
# 2. Perform regular ANOVA
# Perform Normality Test:
# 3. Levene's Test for Homogeneity of Variance (center = median)
# 3. First, Kruskal-Wallis Test
#       - This test is a nonparametric method for testing whether samples are
#         originated from the same distribution
# 4. Next, Dunn test
#       - Once your initial ANOVA has found a significant difference in three or more means, 
#         Dunn's Test can be used to pinpoint which specific means are significant from the others 
#         Dunn's Multiple Comparison Test is a post hoc (i.e. it's run after an ANOVA) 
#         non parametric test (a "distribution free" test that doesn't assume your data comes from a particular distribution).
# You should obtain medians from this test and any significant difference
# 5. Create a bar graph with medians (so three bars) and then the significant diff labels
#   - x axis will be the fermentation stage, y axis will be the volume medians
##



## Library ##


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
library(readxl)


library(knitr)
library(emmeans)
library(writexl)
library(ggfortify)
library(ggforce)
library(lme4)
library(lmerTest)
library(broom)
library(kableExtra)
library(multcomp)
library(tidyr)
library(dplyr)
library(sjPlot)
library(viridisLite)
library(viridis)
library(plotly)
library(stringr)
library(qqplotr)
library(GGally)
library(drc)
library(germinationmetrics)
library(car)
library(FSA)



## Loading data 
library(readxl)
allferm_endo_air <- read_excel("Projects/Grape Seed/Data/Pore Number/allferm_endo_air.xlsx") %>%
  mutate(Pore_factor = as.factor(Pore)) # making pore a factor***

summary(allferm_endo_air) # looking at the data***

allferm_endo_air$Label2 <- factor(allferm_endo_air$Label2, levels = c("PF", "EF", "LF"),
                                  labels = c("Field", "Fermentation Start", "Fermentation End"))
view(allferm_endo_air)



## Levene's Test for Homogeneity of Variance (center = median) ##

## Your data appears non normal, right? But let's test it statistically 
leveneTest(Volume ~ Label2, data = allferm_endo_air) #a significant result means that the data is non normal, so its non normal***
leveneTest(log(Volume) ~ Label2, data = allferm_endo_air) #with log transformation still non normal***

leveneTest(SA ~ Label2, data = allferm_endo_air)
leveneTest(log(SA) ~ Label2, data = allferm_endo_air) #less significant than volume

## Moving on to non parametric stats Kruskal Wallis test of ANOVA
kruskal.test(Volume ~ Label2, data = allferm_endo_air) #Highly significant result****

kruskal.test(SA ~ Label2, data = allferm_endo_air) # Also highly significant result****


## Moving on to non parametric pair wise tests, its like a Tukey HSD 
dunnTest <- dunnTest(Volume ~ Label2,
                     data=allferm_endo_air,
                     method="bonferroni")


dunnTest(SA ~ Label2,
         data=allferm_endo_air,
         method="bonferroni")



## We see that LF and PF are the same, but are different than EF, not sure what this means but it's AN answer


## Follow up with bar graph of median values for each fermentation stage and letters over each bar showing differences



DT_grape <-  dunnTest(Volume ~ Label2,
                     data=allferm_endo_air,
                     method="bonferroni")      # Adjusts p-values for multiple comparisons;
res_grape = DT_grape$res



####
multcompView::cldList(P.adj ~ Comparison,
        data = res_grape,
        threshold = 0.05,
        remove.zero=F)



####
cld_grape<- as_tibble(cldList(P.adj ~ Comparison,
                              data = res_grape,
                              threshold = 0.05,
                              remove.zero=F)) %>%
  mutate(Label2 = as.numeric(Group)) %>%
  arrange(Label2); cld_grape


letters_grape<-  cld_grape$Letter

median_grape<- allferm_endo_air %>%
  select(Label2, Volume) %>%
  group_by(Label2) %>%
  summarise(median_F = median(Volume)) %>%
  mutate(letters = letters_grape)
median_grape

median_grape$median_F <- signif(median_grape$median_F, 3)

median_grape$stat <- paste(median_grape$median_F, median_grape$letters)
median_grape

##### Below is for Surface Area ####

DT_grapeSA<-  dunnTest(SA ~ Label2,
                     data=allferm_endo_air,
                     method="bonferroni")      # Adjusts p-values for multiple comparisons;
res_grapeSA = DT_grapeSA$res
cld_grapeSA<- as_tibble(cldList(P.adj ~ Comparison,
                              data = res_grapeSA,
                              threshold = 0.05,
                              remove.zero=F)) %>%
  mutate(Label2 = as.numeric(Group)) %>%
  arrange(Label2); cld_grapeSA


letters_grapeSA<-  cld_grape$Letter
median_grapeSA<- allferm_endo_air %>%
  select(Label2, SA) %>%
  group_by(Label2) %>%
  summarise(median_FSA = median(SA)) %>%
  mutate(lettersSA = letters_grapeSA)
median_grapeSA

median_grapeSA$median_FSA <- signif(median_grapeSA$median_FSA, 3)

median_grapeSA$statSA <- paste(median_grapeSA$median_FSA, median_grapeSA$lettersSA)
median_grapeSA

## Observing the data graphically
# volume figure
V <- ggplot(allferm_endo_air, aes(Volume, fill = Label2)) +
  geom_density(aes(y = ..scaled..), 
               alpha=.8,
               position = "stack",
               bw = .06) + # NOTE this is smoothing your data, kind of a trick, but whatever***
  geom_vline(data = median_grape, aes(xintercept = median_F), color = "black", linetype = "dashed", size = 0.7) + # this adds the median line to the grape
  facet_wrap(~Label2) + # this breaks you data apart by fermentation stage***
  geom_text(
    data = median_grape,
    mapping = aes(x = 0.3, y = 0.8, label = stat)) +
  labs(x = expression(bold("Volume"*" (mm"^3*")")), 
                      y = "Density") +
  scale_fill_manual(name = "Sampling Timepoint", labels = c("Field", "Fermentation Start", "Fermentation End"), 
                    values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  theme_bw() +
  theme(strip.text.x = element_text(size = 10, color = "black", face = "bold.italic"), 
        legend.position = "bottom", axis.title.x = element_text(size = 10, face = "bold"), axis.title.y = element_text(size = 10, face = "bold"))
V


# surface area figure
SA <- ggplot(allferm_endo_air, aes(SA, fill = Label2)) +
  geom_density(aes(y = ..scaled..), 
               alpha=.8,
               position = "stack",
               bw = 1) + # NOTE this is smoothing your data, kind of a trick, but whatever***
  geom_vline(data = median_grapeSA, aes(xintercept = median_FSA), color = "black", linetype = "dashed", size = 0.7) + # this adds the median line to the grape
  facet_wrap(~Label2) + # this breaks you data apart by fermentation stage***
  geom_text(
    data = median_grapeSA,
    mapping = aes(x = 10, y = 0.8, label = statSA)) +
  labs(x = expression(bold("Surface Area"*" (mm"^2*")")), 
       y = "Density") +
  scale_fill_manual(name = "Sampling Timepoint", labels = c("Field", "Fermentation Start", "Fermentation End"), 
                    values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  theme_bw() +
  theme(strip.text.x = element_text(size = 10, color = "black", face = "bold.italic"), 
        legend.position = "none", axis.title.x = element_text(size = 10, face = "bold"), axis.title.y = element_text(size = 10, face = "bold"))
SA


Figure <- grid.arrange(SA, V, ncol = 1)











 #### EXTRA INFO BELOW NOT NEEDED FOR PUBLICATION ###########  

##### Bar Plot with Median Values and Sig fig Labels ########

grape_fig<- ggplot(median_grape, aes(x = factor(Label2,
                                             levels = c("Field","Fermentation Start","Fermentation End")),
                                   y = median_F)) +
  geom_col(fill=c("#00AFBB", "#E7B800", "#FC4E07")) +
  geom_text(aes(y = median_F,label=letters),
            size=6)+
  labs(x="", y=expression(bold("Volume"*" (mm"^3*")"))) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(color = "black", size = 10, face = "bold"),
        axis.text.y = element_text(color = "black", size = 10, face = "bold"))

grape_fig











###############
# volume figure
V <- ggplot(allferm_endo_air, aes(Volume, fill = Label2)) +
  geom_density(aes(y = ..scaled..), 
               alpha=.8,
               position = "stack",
               bw = .06) + # NOTE this is smoothing your data, kind of a trick, but whatever***
  geom_vline(data = median_grape, aes(xintercept = median_F), color = "black", linetype = "dashed", size = 0.7) + # this adds the median line to the grape
  facet_wrap(~Label2) + # this breaks you data apart by fermentation stage***
  geom_text(
    data = median_grape,
    mapping = aes(x = 0.2, y = 0.8, label = stat)) +
  labs(x = expression(bold("Volume"*" (mm"^3*")")), 
       y = "Density") +
  scale_fill_manual(name = "Sampling Timepoint", labels = c("Field", "Fermentation Start", "Fermentation End"), 
                    values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"), 
        legend.position = "bottom", axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"))
V

  








############### NOTES #########################

# Note: %>% is called the forward pipe operator in R. It provides a mechanism for 
# chaining commands with a new forward-pipe operator, %>%. This operator will forward a value, 
# or the result of an expression, into the next function call/expression

## mutate() adds new variables and preserves existing ones; transmute() adds new variables and drops existing one


### The Bonferroni correction is used to reduce the chances of obtaining false-positive results (type I errors) 
### when multiple pair wise tests are performed on a single set of data. Put simply, 
### the probability of identifying at least one significant result due to chance increases as more hypotheses are tested.


