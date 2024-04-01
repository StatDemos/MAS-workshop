# install packages
install.packages("readxl", "dplyr", "ggplot2", "magrittr", "car", "DescTools",
                 "tidyr", "janitor")


# load the packages

library(readxl)    # to load the data set 
library(dplyr)     # select operator
library(ggplot2)   # to create plots
library(magrittr)  # pipe operator
library(car)       # to obtain vif value
library(DescTools) # to obtain the mode
library(tidyr)
library(janitor)



# Descriptive Statistics

## Load the data

data_descriptive <- read_xlsx("Descriptive Statistics Data.xlsx")


## Glimpse on the dataset

glimpse(data_descriptive)


# slide number: 43
# one way frequency table

table(data_descriptive$`Shipping Type`)
tabyl(data_descriptive$`Shipping Type`,sort=TRUE,show_na = FALSE) 
# to obtain tidy output


# slide number: 44
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


# slide number: 45
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


# slide number: 46
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


# slide number: 47
# histogram - symmetric


# preparing data
symmetric <- data_descriptive %>% filter(`Customer Group` == "Lands'End")
outliers <- boxplot(symmetric$EPH, plot=FALSE)$out
symmetric <- symmetric[-which(symmetric$EPH %in% outliers), ]

# histogram
symmetric %>%
  select(EPH) %>%
  ggplot(aes(x = EPH)) +
  geom_histogram(color = "black", 
                 fill = "brown") +
  theme_bw() +
  labs(title = "Distribution of EPH values for Lands'End Customer Group",
       x = "Earnings per hour",
       y = "Frequency")


# slide number: 48
# histogram - positively skewed

# preparing data
pos.skewed <- data_descriptive %>% filter(`Customer Group` == "SPEEDO NA")
outliers <- boxplot(pos.skewed$EPH, plot=FALSE)$out
pos.skewed <- pos.skewed[-which(pos.skewed$EPH %in% outliers), ]

#histogram
pos.skewed %>%
  select(EPH) %>%
  ggplot(aes(x = EPH)) +
  geom_histogram(color = "black", 
                 fill = "brown") +
  theme_bw() +
  labs(title = "Distribution of EPH values for SPEEDO Customer Group",
       x = "Earnings per hour",
       y = "Frequency")


# slide number: 49
# histogram - negatively skewed

# preparing data
neg.skewed <- data_descriptive %>% filter(`Customer Group` == "PELEG NIL LTD")

#histogram
neg.skewed %>%
  select(EPH) %>%
  ggplot(aes(x = EPH)) +
  geom_histogram(color = "black", 
                 fill = "brown") +
  theme_bw() +
  labs(title = "Distribution of EPH values for PELEG NIL LTD Customer Group",
       x = "Earnings per hour",
       y = "Frequency")


# slide number: 50
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
# dot plot

data_descriptive %>%
  ggplot(aes(x = SMV)) +
  geom_dotplot(method="histodot", binwidth = 0.12,col = "brown") +
  labs(x = "SMV", y = "Count", title = "Distribution of SMV") +
  theme_bw()

# slide number: 54
# two way frequency table

table(data_descriptive$`Shipping Type`, 
      data_descriptive$Plant)


# slide number: 55
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


# slide number: 56
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


# slide number: 57
# scatterplot - positive linear

data_descriptive %>%
  ggplot(aes(x = `Order Qty`,
             y = Earnings)) +
  geom_point(col = "brown") +
  theme_bw() +
  labs(title = "Scatterplot of Order Quantity and Earnings",
       x = "Order Quantity",
       y = "Earnings")


# slide number: 58
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


# slide number: 59
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

# slide number: 61
# line chart

# preparing data
line_data <- trial %>% group_by(Date) %>% 
  summarise(Mean.Earnings = mean(Earn)) %>% drop_na()

# line chart
line_data %>%
  ggplot(aes(x = Date, y = Mean.Earnings, group=1)) +
  geom_line(color="brown", size=1) +
  theme_bw() +
  labs(title = "Line chart of mean earning values",
       x = "Date",
       y = "Average earnings")


# Correlation Analysis

# slide number: 65

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

# slide number: 66
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


# Hypothesis Testing

## One sample test for mean - Slide no 73

# loading dataset
Hypothesis.data <- read_excel("Hypothesis Data.xlsx")

### Step 1: Check whether Earnings per hour values are normally distributed

# Normal probability plot
  
ggplot(Hypothesis.data, aes(sample = Earnings.per.hour)) + stat_qq() + 
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# Normality test
  
shapiro.test(Hypothesis.data$Earnings.per.hour)


### Step 2: Perform the t-test

t.test(Hypothesis.data$Earnings.per.hour, alternative = "less", mu = 50)


## Two sample test for comparison between means - Slide no 75

# Loading relevant data
two.sample.data <- Hypothesis.data %>% filter(Product.name %in% c("Hoody", "T-shirt"))

### Step 1: Check whether Earnings per hour values are normally distributed

# Normal probability plot
  
ggplot(two.sample.data, aes(sample = Earnings.per.hour)) + stat_qq() + 
  stat_qq_line() + facet_grid(.~Product.name) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# Normality test
  
test1 <- two.sample.data %>% filter(Product.name == "Hoody")
shapiro.test(test1$Earnings.per.hour)

test2 <- two.sample.data %>% filter(Product.name == "T-shirt")
shapiro.test(test2$Earnings.per.hour)


### Step 2: Check for equality of variance

var.test(Earnings.per.hour ~ Product.name, data = two.sample.data,
         alternative = "two.sided")


### Step 3: Perform the t-test

t.test(Earnings.per.hour ~ Product.name, data = two.sample.data,
       alternative = "two.sided", var.equal = TRUE)


# Simple linear regression analysis - Slide no 83

# load the data set
Reg_data <- read_excel("Textile.xlsx")

# scatter plot of Earnings and Standard Hours

ggplot(Reg_data, aes(x=`No. of pcs day`, y=Efficiency)) + 
  geom_point() + geom_smooth(method = lm, se = FALSE, color = "red") 
ggtitle("Scatterplot of Efficiency and No.of pieces per day")

## Fit the model

model1 <- lm(Efficiency~`No. of pcs day`, data = Reg_data)
summary(model1)

## Checking assumptions

### Check the constant variance assumption of residuals

# obtain the residuals
residuals <- resid(model1)

### Check the constant variance assumption of residuals

# obtain the residuals
residuals <- resid(model1)

plot(fitted(model1), residuals, xlab="Fitted values",ylab="Residuals",
     main="Residual vs Fitted value plot", pch=20, cex=1)


# add a horizontal line at 0 
abline(0,0) 

plot(fitted(model1), residuals, xlab="Fitted values",ylab="Residuals",
     main="Residual vs Fitted value plot", pch=20, cex=1)

# Q-Q plot for residuals
qqnorm(residuals, pch=20)

# add a straight diagonal line to the plot
qqline(residuals) 

# Normality test
shapiro.test(residuals)
