####################################################################################
# Elif Kardas
# 20 Nov 2019
# Plant Reproduction Project 
# Time partitioning in foraging for Centris decolorata 
####################################################################################

# IMPORTING THE DATA 

library(readxl)
cn <- read_excel("C:/Users/Juan Daniel/Desktop/ELIF/PlantReproduction-FinalProjectPinones/Pinones_Centrisdecoloratabisbis.xlsx", 
                 col_types = c("text", "text", "numeric", 
                               "text", "numeric", "numeric", "text", 
                               "numeric", "text"))
####################################################################################
#
# 1. Difference in relative frequency of visits by time, plant species, & gender
#
####################################################################################
# QUALITATIVE ANALYSIS : 
# Boxplots of relative frequency of visits by time, plant species, & gender

require(ggplot2)
p1 <- ggplot(data = cn, aes(x=cn$host_plant, y=cn$rel_vis)) + 
  geom_boxplot(aes(fill=bee_gender))
p1 + labs(title = "Relative frequency of plant visitations by plant/time and by bee sex") + 
  xlab("Host plant (AM vs. PM)") + ylab("Relative frequency of visit (#visit/#tot_flower)")+
  guides(fill=guide_legend(title="Bee sex"))

# QUANTITATIVE ANALYSIS : 
# ANOVA:
attach(cn) ## to not have to put cn$..., the variables are attached to the data
summary(fm1 <- aov(rel_vis ~ bee_gender*host_plant, data = cn))

####################################################################################
#
# 2. Difference in relative frequency of visits by nest position for both gender
#
####################################################################################

# decription :

# QUALITATIVE ANALYSIS : 
# Boxplots of relative frequency visitations by nest position

p2 <- ggplot(data = cn, aes(x=cn$pos, y=cn$rel_vis)) + 
  geom_boxplot(aes(fill=bee_gender))
p2 + labs(title = "Relative frequency of plant visitations by position and by bee sex") + 
  xlab("Position of flower relative to nest (top vs. non-top)") + ylab("Relative frequency of visit (#visit/#tot_flower)")+
  guides(fill=guide_legend(title="Bee sex"))

# QUANTITATIVE ANALYSIS : 
# ANOVA:
summary(fm2 <- aov(rel_vis ~ bee_gender*pos, data = cn))

####################################################################################
#
# 3. Difference in relative frequency of visits by time for both gender
#
####################################################################################

# decription :

# QUALITATIVE ANALYSIS: 
# Boxplots of relative frequency visitations by day time

p3 <- ggplot(data = cn, aes(x=cn$day_time, y=cn$rel_vis)) + 
  geom_boxplot(aes(fill=bee_gender))
p3 + labs(title = "Relative frequency of plant visitations by day time and by bee sex") + 
  xlab("Day time (AM vs. PM)") + ylab("Relative frequency of visit (#visit/#tot_flower)")+
  guides(fill=guide_legend(title="Bee sex"))

# QUANTITATIVE ANALYSIS : 
# ANOVA:
summary(fm3 <- aov(rel_vis ~ day_time*bee_gender, data = cn))


####################################################################################
#
# 4. Difference in relative frequency of visits by plants and day time for males only
#
####################################################################################

# Charge the data in another way such that the plant species is not automatically 
# linked to the AM and PM:
library(readxl)
cn.nt <- read_excel("C:/Users/Juan Daniel/Desktop/ELIF/PlantReproduction-FinalProjectPinones/Pinones_Centrisdecoloratabisbisbis.xlsx", 
                   col_types = c("text", "text", "numeric", 
                                 "text", "numeric", "numeric", "text", 
                                 "numeric", "text"))

#View(cn.nt)
cn.nt.m <- cn.nt[which(cn.nt$bee_gender=='M'),] # only select the males within the data

# QUALITATIVE ANALYSIS: 
# Boxplots of relative frequency visitations by plants and day time
attach(cn.nt.m)
p4 <- ggplot(data = cn.nt.m, aes(x=host_plant, y=rel_vis))+
  geom_boxplot(aes(fill=day_time))+
  scale_fill_manual(values=c("#FFCC33", "#FF3300"))
p4 + labs(title = "Rel. freq. of plant visits for MALES by host plant and day time") + 
  xlab("Host plant") + 
  ylab("Relative frequency of visit (#visit/#tot_flower)")+
  guides(fill=guide_legend(title="Day time"))

# QUANTITATIVE ANALYSIS : 
# ANOVA:
summary(fm4 <- aov(rel_vis ~ host_plant*day_time, data = cn.nt.m))
