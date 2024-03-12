
library(tidyverse)
library(readr)
library(readxl)

# To anonymize this submission, we do not display the workind directory, since it contains a username
# please paste your working directory below
setwd("C:/Users/luisr/OneDrive/Dokumente/Master/NHH/2_Courses/BAN402/Project 3/R Plots")


#########################################
# Part A-------
#########################################

# Load the data
data_A <- read_excel("Part_A_data.xlsx")

# create line plot of forecast and observation over all periods
plot_A <- data_A %>% 
  ggplot() +
  geom_line(aes(x = as.factor(Quarter), y = Forecast, color = "Forecast", group = 1)) +
  geom_line(aes(x = as.factor(Quarter), y = Observation, color = "Observation", group = 1)) +
  ylab("PIFED") +
  xlab("Period") +
  scale_color_manual(values = c("Forecast" = "blue", "Observation" = "red"), 
                     labels = c("Forecast", "Observation")) +
  labs(color = "") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  scale_x_discrete(breaks = unique(data_A$Quarter[seq(1, length(unique(data_A$Quarter)), 12)]))

# show plot A
plot_A

# save as .svg for better quality. The .png in the folder is created from this .svg file
svg("Plot_A.svg", width = 16, height = 9)
print(plot_A)
dev.off()




#########################################
# Part C-------
#########################################

# load the data
data_C <- read_excel("Part_C_1_data.xlsx")

# only plot the supply
data_C_supply <- data_C %>% 
  filter(!is.na(Supply)) %>% 
  select(Volume, Supply, `Supply linear`) %>% 
  mutate(Type = rep(c("cdf", "prior"), 20))

plot_C_supply <- data_C_supply %>% 
  ggplot() + 
    scale_fill_manual(values = c("black", "white")) +
    geom_segment(aes(x = lag(Volume), y = lag(Supply),
                     xend = Volume, yend = Supply,
                     lty = Type)) +
    xlab("Quantity") +
    ylab("Price") +
    scale_linetype_manual(values = c("dashed", "solid"))


# only plot the demand
data_C_demand <- data_C %>% 
  filter(!is.na(Demand)) %>% 
  select(Volume, Demand, `Demand linear`) %>% 
  mutate(Type = rep(c("cdf", "prior"), 20))

plot_C_demand <- data_C_demand %>% 
  ggplot() + 
  scale_fill_manual(values = c("black", "white")) +
  geom_segment(aes(x = lag(Volume), y = lag(Demand),
                   xend = Volume, yend = Demand,
                   lty = Type)) +
  xlab("Quantity") +
  ylab("Price") +
  scale_linetype_manual(values = c("dashed", "solid"))


# plot both supply and demand
data_C_final <- 
  data_C_supply %>% 
  add_column(Volume_Demand = data_C_demand$Volume) %>% 
  add_column(Demand = data_C_demand$Demand) %>% 
  add_column(Demand_Linear = data_C_demand$`Demand linear`)

data_C_final <- data_C_final %>% 
  filter(!is.na(`Supply linear`)) %>% 
  mutate(Type = "linear",
         Supply = NA,
         Demand = NA) %>%
  bind_rows(data_C_final %>% mutate(`Supply linear` = NA, Demand_Linear = NA))


plot_C_1 <- data_C_final %>% 
  ggplot() + 
  geom_line(aes(x = Volume, y = `Supply linear`, lty = Type, color = "Supply Linearized (dashed)"), na.rm = T) +
  geom_line(aes(x = Volume_Demand, y = Demand_Linear, lty = Type, color = "Demand Linearized (dashed)"), na.rm = T) +
  geom_segment(aes(x = lag(Volume), y = lag(Supply),
                   xend = Volume, yend = Supply,
                   lty = Type,
                   color = "Supply"), na.rm = T) +
  geom_segment(aes(x = lag(Volume_Demand), y = lag(Demand),
                   xend = Volume_Demand, yend = Demand,
                   lty = Type,
                   color = "Demand"), na.rm = T) +
  scale_color_manual(values = c("Demand" = "red", "Demand Linearized (dashed)" = "red", "Supply" = "blue", "Supply Linearized (dashed)" = "blue"), 
                     labels = c("Demand", "Demand Linearized (dashed)", "Supply", "Supply Linearized (dashed)")) +
  scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
  labs(color = "") +
  xlab("Quantity") +
  ylab("Price") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.box = "horizontal",
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  guides(linetype = "none")

# display the plot
plot_C_1


# comment this in if you want the file as .png
# the .png in the folder is created from the svg file for better quality
# png("plot_C_1PNG.png", width = 800, height = 600)
# plot_C_1
# dev.off()

# save the plot as .svg
svg("plot_C_1.svg", width = 16, height = 9)
print(plot_C_1)
dev.off()






























