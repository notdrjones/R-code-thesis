library(tidyverse)
library(lme4)
library(multcomp)
library(ggpubr)
library(pscl)


merged_df = readxl::read_excel("S:/Staff/Rachel/phase contrast microscope aq/250207/allVelocities_corrected.xlsx")

genotypes <- c("WT","d353","d360","dbot")
merged_df$Strain <- factor(merged_df$Strain, levels = genotypes)



# contrasts(merged_df$Strain) <- cbind(c(1,-(1/6),-(1/6),-(1/6),-(1/6),-(1/6),-(1/6)),
#                                       c(0,0,0,0,0,0,0),
#                                      c(0,0,0,0,0,0,0),
#                                      c(0,0,0,0,0,0,0),
#                                      c(0,0,0,0,0,0,0),
#                                      c(0,0,0,0,0,0,0))
# 


# Create a boxplot
my_plot <- ggplot(merged_df, aes(x = Strain, y = Speed)) +
  geom_boxplot(fill = "#00000000", color = "black") +
  labs(title = "Boxplot of Speed by Strain",
       x = "Strain",
       y = "Speed") +
  
  theme(panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  + coord_cartesian(ylim = c(0, 3))

# Explicitly print the plot
print(my_plot)

#for anova
res_aov <- aov(Speed ~ Strain, data = merged_df)
summary(res_aov)
#for post-hoc Tukey's test
post_test <- glht(res_aov, linfct = mcp(Strain = "Tukey"))
summary(post_test)


