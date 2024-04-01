library(readxl)
library(tidyverse)


data_descriptive <- read_xlsx("Descriptive Statistics - Data.xlsx")

trial <- read_excel("Pre Order Profitability 2023.xlsx")
trial$Date <- as.character(trial$Date)
trial <- trial %>% select(Date, `Customer Group`, Earnings, EPH)
trial <- trial %>% rename(Earn = Earnings)


data1 <- data_descriptive %>% select(`Shipping Type`, SMV, Plant, `Order Qty`,
                                     Earnings)
data2 <- trial %>% select(Date, `Customer Group`, Earn, EPH)

merge(data1, data2)

cbind(data1, data2)

library(writexl)

write_xlsx(data1, "data1.xlsx")
write_xlsx(data2, "data2.xlsx")
