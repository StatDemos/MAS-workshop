
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(DescTools)

data <- read_xlsx("Pre Order Profitability 2023 - Selected.xlsx")

View(data)

glimpse(data)

# ------------------- Descriptive -------------------------- #

# slide number: 38
# one way frequency table
table(data$`Shipping Type`)

# slide number: 39
# barchart
data %>%
  select(`Shipping Type`) %>%
  drop_na() %>%
  ggplot(aes(x = as.factor(`Shipping Type`))) +
  geom_bar(color="brown", 
           fill="brown" ) +
  theme_bw() +
  labs(title = "Composition of the sample by Shipping Type",
       x = "Shipping Type",
       y = "Frequency") 

# slide number: 40
# pie chart
data.frame(Shipping_Type = c("Air Collect", "Courier", "Sea Collect"),
           Frequency = c(87, 86, 937)) %>%
  ggplot(aes(x = "",
             y = Frequency,
             fill = Shipping_Type)) +
  geom_bar(stat="identity", 
           width=1) +
  coord_polar("y", 
              start=0) + 
  geom_text(aes(label = paste0(
    round((Frequency/sum(Frequency))*100), "%")), 
    position = position_stack(vjust = 0.5)) +
  theme_bw() +
  scale_fill_manual(values=c("brown3", "salmon2", "peachpuff3")) +
  labs(x = NULL, 
       y = NULL, 
       fill = "Shipping Type", 
       title = "Composition of the sample by Shipping Type") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))

# slide number: 41
# summary measures

# mean
mean(data$SMV, 
     na.rm = TRUE)

# median
median(data$SMV,
       na.rm = TRUE)

# mode
Mode(data$SMV, 
     na.rm = TRUE)

# slide number: 42
# histogram
data %>%
  select(SMV) %>%
  drop_na() %>%
  ggplot(aes(x = SMV)) +
  geom_histogram(color = "brown", 
                 fill = "brown") +
  theme_bw() +
  labs(title = "Distribution of SMV",
       x = "SMV",
       y = "Frequency")

# slide number: 43
# boxplot
data %>%
  select(SMV) %>%
  drop_na() %>%
  ggplot(aes(x = SMV)) +
  geom_boxplot(color = "brown", 
               fill = "brown" ) +
  theme_bw() +
  labs(title = "Distribution of SMV")

# slide number: 46
# two way frequency table
table(data$`Shipping Type`, 
      data$Plant)

# slide number: 47
# cluster bar chart
data %>%
  drop_na() %>%
  ggplot(aes(x = Plant, y = `Shipping Type`, fill = `Shipping Type`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  scale_fill_manual(values=c("brown3", "salmon2", "peachpuff3")) +
  labs(x = "Plant", 
       y = "Frequency", 
       fill = "Shipping Type", 
       title = "Composition of the sample by Shipping Type and Plant")

# slide number: 48
# stacked bar chart
data %>%
  drop_na() %>%
  ggplot(aes(x = Plant, y = "", fill = `Shipping Type`)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = c("brown3", "salmon2", "peachpuff3")) +
  labs(x = "Plant", 
       y = "Frequency", 
       fill = "Shipping Type", 
       title = "Composition of the sample by Shipping Type and Plant")

# slide number: 49
# scatterplot 
data %>% 
  ggplot(aes(x = `Order Qty`,
             y = Earnings)) +
  geom_point(col = "brown") +
  theme_bw() +
  labs(title = "Scatterplot of Order Quantity and Earnings",
       x = "Order Quantity",
       y = "Earnings")

# slide number: 50
# boxplot with groups
data %>%
  select(SMV, 
         `Shipping Type`) %>%
  drop_na() %>%
  ggplot(aes(x = SMV,
             y = `Shipping Type`,
             fill = `Shipping Type`)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values=c("brown3", "salmon2", "peachpuff3")) +
  labs(x = "SMV", 
       y = "Shipping Type", 
       fill = "Shipping Type", 
       title = "Distribution of SMV by Shipping Type")


# ------------ Correlation Analysis ------------------------ #

# scatterplots

data %>% 
  ggplot(aes(x = SMV,
             y = `Order Qty`)) +
  geom_point() +
  theme_bw()
# no linear relationship

data %>% 
  ggplot(aes(x = SMV,
             y = Earnings)) +
  geom_point() +
  theme_bw()
# no linear relationship

data %>% 
  ggplot(aes(x = SMV,
             y = `Std Hrs`)) +
  geom_point() +
  theme_bw()
# no linear relationship

data %>% 
  ggplot(aes(x = `Order Qty`,
             y = Earnings)) +
  geom_point() +
  theme_bw()
# has linear relationship

data %>% 
  ggplot(aes(x = `Order Qty`,
             y = `Std Hrs`)) +
  geom_point() +
  theme_bw()
# has linear relationship

data %>% 
  ggplot(aes(x = Earnings,
             y = `Std Hrs`)) +
  geom_point() +
  theme_bw()
# has linear relationship

# `Order Qty` and Earnings were selected


# slide number: 53

# scatterplot
data %>% 
  ggplot(aes(x = `Order Qty`,
             y = Earnings)) +
  geom_point(col = "brown") +
  theme_bw() +
  labs(title = "Scatterplot of Order Quantity and Earnings",
       x = "Order Quantity",
       y = "Earnings")
# positive linear relationship

# correlation value
cor(x = data$`Order Qty`,
    y = data$Earnings)
# r = 0.9371457



