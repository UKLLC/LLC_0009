##############################
### Create plots for paper ###
##############################


# Aim: To create figures 1 and 2 for the paper.


# Libraries
library(ggplot2)
library(viridis)

# Load tables
table <- read.csv("./compare_results.csv") # Load data
table$outcome <- factor(table$outcome, levels = c("Any hospital admission", "Emergency urgent care sensitive", "Vaccine-preventable ambulatory care sensitive", "Chronic ambulatory care sensitive", "Acute ambulatory care sensitive", "Any ambulatory care sensitive")) # Define ordering of factor for presentation

# Convert to odds ratios
table$estimate <- exp(table$estimate) 
names(table)[names(table) == "estimate"] <- "odds_ratio" # Rename variable
table$lower_ci <- exp(table$lower_ci) 
table$higher_ci <- exp(table$higher_ci) 

# Figure 1
fig1 <- ggplot() + # Data to be plotted (model 1)
  geom_vline(xintercept = 1, colour = gray(1/2), lty = 2) + # Add line for OR = 1
  geom_point(data = table[table$model == 1,], aes(y = outcome, x = odds_ratio, group = adjusted, color = adjusted), position=position_dodge(width = 0.5), size = 2) + # Plot measures by outcome and group by whether adjusted or not
  geom_linerange(data = table[table$model == 1,], aes(y = outcome, xmin = lower_ci, xmax = higher_ci, group = adjusted, color = adjusted), lwd = 1, position=position_dodge(width = 0.5)) + # Add confidence intervals onto plot
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0.1,1,2,4,8,16), limits = c(0.1,20)) + # Log transform axis
  scale_color_viridis(discrete=TRUE, option = "H", begin = 0.2, end = 0.8) + #Make colour blind friendly
  ylab("Outcome variable") + # Add labels
  xlab("Odds Ratio") +
  labs(color = "Model") + 
  theme(text = element_text(size = 12)) # Change text size
fig1 # Print to visualize in viewer

# Save
ggsave("./fig1_updated.tiff", dpi = 300)
ggsave("./fig1_updated_lowres.jpeg")

# Figure 2
table$outcome <- factor(table$outcome, levels = c("Any ambulatory care sensitive", "Acute ambulatory care sensitive", "Chronic ambulatory care sensitive", "Vaccine-preventable ambulatory care sensitive", "Emergency urgent care sensitive", "Any hospital admission")) # Define ordering of factor for presentation

fig2 <- ggplot() + # Data to be plotted (model 1)
  geom_vline(xintercept = 1, colour = gray(1/2), lty = 2) + # Add line for OR = 1
  geom_point(data = table[table$model == 2,], aes(y = measure, x = odds_ratio, group = adjusted, color = adjusted), position=position_dodge(width = 0.5), size = 2) + # Plot measures by outcome and group by whether adjusted or not
  geom_linerange(data = table[table$model == 2,], aes(y = measure, xmin = lower_ci, xmax = higher_ci, group = adjusted, color = adjusted), lwd = 1, position=position_dodge(width = 0.5)) + # Add confidence intervals onto plot
  #geom_text(data = table[table$model == 2,], aes(y = measure, x = odds_ratio, group = adjusted, label = paste("OR = ", round(odds_ratio,2), ", CI = ", round(lower_ci,2), "-", round(higher_ci,2))), hjust = -0.5, vjust = -1, position=position_dodge(width = 0.5), size = 3) + # Add values as text label
  facet_wrap(~outcome) +
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = c(0.1,1,2,4,8)) + # Log transform axis
  scale_color_viridis(discrete=TRUE, option = "H", begin = 0.2, end = 0.8) + #Make colour blind friendly
  ylab("Outcome variable") + # Add labels
  xlab("Odds Ratio") +
  labs(color = "Model") + 
  theme(text = element_text(size = 12)) + # Change text size all everything
  theme(strip.text = element_text(size = 7.5)) # Make facet labels smaller so fit in
fig2

ggsave("./fig2_updated.tiff", dpi = 300)
ggsave("./fig2_updated_lowres.jpeg")
