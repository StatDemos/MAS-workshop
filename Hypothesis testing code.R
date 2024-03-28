##############################################################################
#########     Hypothesis Testing    #########
##############################################################################

# Loading libraries
library(readxl)
library(ggplot2)
library(magrittr)
library(dplyr)

# loading dataset
Hypothesis.data <- read_excel("Hypothesis Data.xlsx")

#########   One sample test for mean - Slide 61   #########

## Step 1: Check whether Earnings per hour values are normally distributed

# Normal probability plot
ggplot(Hypothesis.data, aes(sample = Earnings.per.hour)) + stat_qq() + 
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# Normality test
shapiro.test(Hypothesis.data$Earnings.per.hour)

# Hypothesis to be tested:
# H0: Data are normally distributed.
# H1: Data are not normally distributed.

# According to the Shapiro-Wilk normality test p-value = 0.3806 > 0.05.
# Hence, We can conclude that Earnings per hour values are normally distributed.

# Step 2: Perform the t-test
t.test(Hypothesis.data$Earnings.per.hour, alternative = "less", mu = 50)

# Since p-value = 1.89e-08 < 0.05, we reject null hypothesis.
# Hence, there is sufficient evidence to suggest that the mean earnings 
# per hour for the Outer Known customer group is less than 50.


#########   Two sample test for comparison between means - Slide 63   #########

# Loading relevant data
two.sample.data <- Hypothesis.data %>% filter(Product.name %in% c("Hoody", "T-shirt"))

## Step 1: Check whether Earnings per hour values are normally distributed

# Normal probability plot
ggplot(two.sample.data, aes(sample = Earnings.per.hour)) + stat_qq() + 
  stat_qq_line() + facet_grid(.~Product.name) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# Normality test
test1 <- two.sample.data %>% filter(Product.name == "Hoody")
shapiro.test(test1$Earnings.per.hour)
  
test2 <- two.sample.data %>% filter(Product.name == "T-shirt")
shapiro.test(test2$Earnings.per.hour)

# Hypothesis to be tested:
# H0: Data are normally distributed.
# H1: Data are not normally distributed.

# According to the Shapiro-Wilk normality test both p-values > 0.05.
# Hence, We can conclude that Earnings per hour values of the two categories
# are normally distributed.

# Step 2: Check for equality of variance
var.test(Earnings.per.hour ~ Product.name, data = two.sample.data,
         alternative = "two.sided")

# Hypothesis to be tested:
# H0: Two population variances are equal.
# H1: Two population variances are not equal.

# According to the F test both p-values = 0.5739 > 0.05.
# Hence, We can conclude that Two population variances are equal.

# Step 3: Perform the t-test
t.test(Earnings.per.hour ~ Product.name, data = two.sample.data,
       alternative = "two.sided", var.equal = TRUE)

# Since p-value = 0.2524 > 0.05, we do not reject null hypothesis.
# Hence, there is sufficient evidence to conclude that there is a significant 
# difference in earnings per hour between the two product types.

