
library(readxl)
library(dplyr)
library(ggplot2)
library(magrittr)
library(DescTools)

data_descriptive <- read_xlsx("Descriptive Statistics - Data.xlsx")

View(data_descriptive)

glimpse(data_descriptive)

# ------------------- Descriptive -------------------------- #

# slide number: 41
# one way frequency table
table(data_descriptive$`Shipping Type`)

# slide number: 42
# barchart
data_descriptive %>%
  select(`Shipping Type`) %>%
  na.omit() %>%
  ggplot(aes(x = as.factor(`Shipping Type`))) +
  geom_bar(color="black", 
           fill="brown" ) +
  theme_bw() +
  labs(title = "Composition of the sample by Shipping Type",
       x = "Shipping Type",
       y = "Frequency") 

# slide number: 43
# pie chart
data.frame(Shipping_Type = c("Air Collect", "Courier", "Sea Collect"),
           Frequency = c(87, 86, 937)) %>%
  ggplot(aes(x = "", y = Frequency,
             fill = Shipping_Type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(
    round((Frequency/sum(Frequency))*100), "%")), 
    position = position_stack(vjust = 0.5)) +
  theme_bw() +
  scale_fill_manual(values=c("brown3", "salmon2", "peachpuff3")) +
  labs(x = NULL, y = NULL, 
       fill = "Shipping Type", 
       title = "Composition of the sample by Shipping Type") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

# slide number: 44
# summary measures

# mean
mean(data_descriptive$SMV, 
     na.rm = TRUE)

# median
median(data_descriptive$SMV,
       na.rm = TRUE)

# mode
Mode(data_descriptive$SMV, 
     na.rm = TRUE)

# slide number: 45
# histogram - asymmetric
data_descriptive %>%
  select(SMV) %>%
  na.omit() %>%
  ggplot(aes(x = SMV)) +
  geom_histogram(color = "black", 
                 fill = "brown") +
  theme_bw() +
  labs(title = "Distribution of SMV",
       x = "SMV",
       y = "Frequency")

# slide number: 46
# histogram - positively skewed
data_descriptive %>%
  select(`Order Qty`) %>%
  na.omit() %>%
  ggplot(aes(x = `Order Qty`)) +
  geom_histogram(color = "black", 
                 fill = "brown") +
  theme_bw() +
  labs(title = "Distribution of Order Quatity",
       x = "Order Quantity",
       y = "Frequency")

# slide number: 47
# boxplot
data_descriptive %>%
  select(SMV) %>%
  na.omit() %>%
  ggplot(aes(x = SMV)) +
  geom_boxplot(color = "black", 
               fill = "brown" ) +
  theme_bw() +
  labs(title = "Distribution of SMV")

# slide number: 51
# two way frequency table
table(data_descriptive$`Shipping Type`, 
      data_descriptive$Plant)

# slide number: 52
# stacked bar chart
data_descriptive %>%
  na.omit() %>%
  ggplot(aes(x = Plant, y = "", fill = `Shipping Type`)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = c("brown3", "salmon2", "peachpuff3")) +
  labs(x = "Plant", 
       y = "Frequency", 
       fill = "Shipping Type", 
       title = "Composition of the sample by Shipping Type and Plant") +
  theme(legend.position = "bottom")

# slide number: 53
# cluster bar chart
table(data_descriptive$`Shipping Type`, 
      data_descriptive$Plant) %>%
  as.data.frame() %>%
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  theme_bw() +
  scale_fill_manual(values=c("brown3", "salmon2", "peachpuff3")) +
  labs(x = "Plant", 
       y = "Frequency", 
       fill = "Shipping Type", 
       title = "Composition of the sample by Shipping Type and Plant") +
  theme(legend.position = "bottom")

# slide number: 54
# scatterplot - positive linear
data_descriptive %>%
  ggplot(aes(x = `Order Qty`,
             y = Earnings)) +
  geom_point(col = "brown") +
  theme_bw() +
  labs(title = "Scatterplot of Order Quantity and Earnings",
       x = "Order Quantity",
       y = "Earnings")

# slide number: 55
# scatterplot - no linear
data_descriptive %>% 
  na.omit() %>%
  ggplot(aes(x = SMV,
             y = Earnings))+
  geom_point(col = "brown") +
  theme_bw() +
  labs(title = "Scatterplot of SMV and Earnings",
       x = "SMV",
       y = "Earnings")

# slide number: 56
# boxplot with groups
data_descriptive %>%
  select(SMV, 
         `Shipping Type`) %>%
  na.omit() %>%
  ggplot(aes(x = SMV,
             y = `Shipping Type`,
             fill = `Shipping Type`)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values=c("brown3", "salmon2", "peachpuff3")) +
  labs(x = "SMV", 
       y = "Shipping Type", 
       fill = "Shipping Type", 
       title = "Distribution of SMV by Shipping Type") +
  theme(legend.position = "bottom")


# ------------ Correlation Analysis ------------------------ #

# scatterplots

data_descriptive %>% 
  na.omit() %>%
  ggplot(aes(x = SMV,
             y = `Order Qty`)) +
  geom_point() +
  theme_bw()
# no linear relationship

data_descriptive %>% 
  na.omit() %>%
  ggplot(aes(x = SMV,
             y = Earnings)) +
  geom_point() +
  theme_bw()
# no linear relationship

data_descriptive %>%
  na.omit() %>%
  ggplot(aes(x = SMV,
             y = `Std Hrs`)) +
  geom_point() +
  theme_bw()
# no linear relationship

data_descriptive %>%
  na.omit() %>%
  ggplot(aes(x = `Order Qty`,
             y = Earnings)) +
  geom_point() +
  theme_bw()
# has linear relationship

data_descriptive %>%
  na.omit() %>%
  ggplot(aes(x = `Order Qty`,
             y = `Std Hrs`)) +
  geom_point() +
  theme_bw()
# has linear relationship

data_descriptive %>%
  na.omit() %>%
  ggplot(aes(x = `Std Hrs`,
             y = Earnings,)) +
  geom_point() +
  theme_bw()
# has linear relationship

# `Order Qty` and Earnings were selected and SMV and Earnings were selected


# slide number: 63

# scatterplot - positive linear
data_descriptive %>% 
  na.omit() %>%
  ggplot(aes(x = `Order Qty`,
             y = Earnings)) +
  geom_point(col = "brown") +
  theme_bw() +
  labs(title = "Scatterplot of Order Quantity and Earnings",
       x = "Order Quantity",
       y = "Earnings")
# positive linear relationship

# correlation value
cor(x = data_descriptive$`Order Qty`,
    y = data_descriptive$Earnings)
# r = 0.9371457

# slide number: 64

# scatterplot - no linear
data_descriptive %>% 
  na.omit() %>%
  ggplot(aes(x = SMV,
             y = Earnings))+
  geom_point(col = "brown") +
  theme_bw() +
  labs(title = "Scatterplot of SMV and Earnings",
       x = "SMV",
       y = "Earnings")
# no linear relationship

# correlation value
cor(data_descriptive$SMV,
    data_descriptive$Earnings,
    use = "complete.obs")
# 0.1142413




