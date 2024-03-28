library(readxl)
mas <- read_excel("mas.xlsx")
View(mas)

# Create new data set for regression
reg_data <- mas %>%
  select(`MOH Value`, `Earnings`,`Order Qty`,`Std Hrs`,`RMC Final`,SMV) 

ggplot(reg_data, aes(x=`MOH Value`, y=Earnings)) + 
  geom_point() + geom_smooth(method = lm, se = FALSE, color = "red") 

ggplot(reg_data, aes(x=`Std Hrs`, y=Earnings)) + 
  geom_point() + geom_smooth(method = lm, se = FALSE, color = "red") 

ggplot(reg_data, aes(x=`Order Qty`, y=Earnings)) + 
  geom_point() + geom_smooth(method = lm, se = FALSE, color = "red") 

ggplot(reg_data, aes(x=`RMC Final`, y=Earnings)) + 
  geom_point() + geom_smooth(method = lm, se = FALSE, color = "red") 


ggplot(reg_data, aes(x=`SMV`, y=Earnings)) + 
  geom_point() + geom_smooth(method = lm, se = FALSE, color = "red") 

# check for multicollinearity

cor(reg_data, use='pairwise.complete.obs')

model2 <- lm(Earnings~`Std Hrs`+ `RMC Final`, data = reg_data)
summary(model2)
vif(model2)

model3 <- lm(Earnings~`Std Hrs`+ `MOH Value`, data = reg_data)
summary(model3)
vif(model3) #somewhat good

model4 <- lm(Earnings~`Order Qty`+ `MOH Value`, data = reg_data)
summary(model4)
vif(model4)

# residual analysis
res<- resid(model3)

#produce residual vs. fitted plot
plot(fitted(model3), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))




