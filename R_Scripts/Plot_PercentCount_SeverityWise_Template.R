# Load the necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Create a dataframe with your data
data <- data.frame(
  Type = c("Control", "Diseased", "Control", "Control", "Diseased", "Control", "Control"),
  Benton = c(0, 1, 0, 0, 2, 0, 0),
  Clock = c(0, 1, 0, 1, 0, 0, 1),
  COGSTATE = c(0, 0, 1, 1, 2, 0, 0),
  Epworth = c(1, 0, 0, 0, 1, 1, 0),
  Geriatric_Depression = c(1, 1, 0, 1, 1, 0, 0),
  Hopkins_Recall = c(2, 0, 0, 0, 0, 1, 0)
)

# Reshape the data for plotting
data_long <- data %>%
  gather(key = "Independent_Var", value = "Value", -Type)

# Calculate the total count for each combination of Independent_Var, Type, and Value
total_counts <- data_long %>%
  group_by(Independent_Var, Type, Value) %>%
  summarise(Count = n())

# Calculate the total count for each combination of Independent_Var and Value
total_counts_total <- total_counts %>%
  group_by(Independent_Var, Value) %>%
  summarise(TotalCount = sum(Count))

# Calculate the percentage of TotalCount for each combination of Independent_Var, Type, and Value
percentage_data <- total_counts %>%
  left_join(total_counts_total, by = c("Independent_Var", "Value")) %>%
  mutate(Percentage = (Count / TotalCount) * 100)

# Create separate stacked bar plots for each independent variable
plots <- list()
independent_vars <- unique(percentage_data$Independent_Var)

for (var in independent_vars) {
  plot_data <- filter(percentage_data, Independent_Var == var)
  
  p <- ggplot(plot_data, aes(x = Value, y = Percentage, fill = Type)) +
    geom_bar(stat = "identity") +
    labs(title = paste("", var), x = "", y = "Percentage") +
    scale_y_continuous(limits = c(0, 100)) +  # Ensure the y-axis goes from 0 to 100
    theme_minimal() + scale_x_continuous (breaks = seq(0, 2, by = 1)) +
    theme(axis.text.x = element_text(size=18, angle=0, vjust = 0.99, hjust=1, color="black")) +
    theme(axis.text.y = element_text(size=18, color="black")) +
    theme(axis.title=element_text(face='bold')) + theme(plot.title = element_text(face="bold", size=20)) +
    theme(axis.text = element_text(color="black", face="bold")) + theme(text = element_text(size=18)) +
    theme(legend.title = element_blank()) + 
    theme(legend.position=c(0.50, 0.88), legend.background = element_rect(fill="azure", size=0.1,
    linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  plots[[var]] <- p
}

# Print the plots
plots
