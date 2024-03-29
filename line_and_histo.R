library(readxl)
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)


trial <- read_excel("Pre Order Profitability 2023.xlsx")
trial$Date <- as.character(trial$Date)
trial <- trial %>% select(Date, `Customer Group`, Earnings, EPH)


# histogram - symmetric
# preparing data
symmetric <- trial %>% filter(`Customer Group` == "Lands'End")
outliers <- boxplot(symmetric$EPH, plot=FALSE)$out
symmetric <- symmetric[-which(symmetric$EPH %in% outliers), ]

symmetric %>%
  select(EPH) %>%
  ggplot(aes(x = EPH)) +
  geom_histogram(color = "black", 
                 fill = "brown") +
  theme_bw() +
  labs(title = "Distribution of EPH values for Lands'End Customer Group",
       x = "Earnings per hour",
       y = "Frequency")


# histogram - positive skewed
# preparing data
pos.skewed <- trial %>% filter(`Customer Group` == "SPEEDO NA")
outliers <- boxplot(pos.skewed$EPH, plot=FALSE)$out
pos.skewed <- pos.skewed[-which(pos.skewed$EPH %in% outliers), ]

pos.skewed %>%
  select(EPH) %>%
  ggplot(aes(x = EPH)) +
  geom_histogram(color = "black", 
                 fill = "brown") +
  theme_bw() +
  labs(title = "Distribution of EPH values for SPEEDO Customer Group",
       x = "Earnings per hour",
       y = "Frequency")


# histogram - negatively skewed
# preparing data
neg.skewed <- trial %>% filter(`Customer Group` == "PELEG NIL LTD")

neg.skewed %>%
  select(EPH) %>%
  ggplot(aes(x = EPH)) +
  geom_histogram(color = "black", 
                 fill = "brown") +
  theme_bw() +
  labs(title = "Distribution of EPH values for PELEG NIL LTD Customer Group",
       x = "Earnings per hour",
       y = "Frequency")



# Line chart
# preparing data
line_data <- trial %>% group_by(Date) %>% 
  summarise(Mean.Earnings = mean(Earnings)) %>% drop_na()


# line chart
line_data %>%
  ggplot(aes(x = Date, y = Mean.Earnings, group=1)) +
  geom_line(color="brown", size=1) +
  theme_bw() +
  labs(title = "Line chart of mean earning values",
       x = "Date",
       y = "Average earnings")
