airquality = read.table("PM25.csv", header = TRUE, sep=",")
airquality

resp_dis = read.table("chron-resp.csv", header = TRUE, sep=",")
resp_dis

smoking=read.table("smoking.csv", header = TRUE, sep=",")
smoking

#merging the air quality data and the respiratory diseases rates in one table
data1 = na.omit(merge(airquality, resp_dis, by = "Countries"))
data1

#merging all of the data together
#we had to divide the merging in 2 steps, 
#because the merge function could not handle combining
#all of the 3 columns at once
data = na.omit(merge(data1, smoking, by = "Countries"))
data

#setting the variables for easier use
pm25 = data$pm25
crd = data$crd
smoking = data$Smoking

#checking differences between correlations without outliers

subset_indices = which(data$pm25 < 60)
pm25 = data$pm25[subset_indices]
crd = data$crd[subset_indices]
#smoking = data$Smoking[subset_indices]
#smoking
cor(pm25, crd)
#cor(smoking, crd)

#data overview
summary(pm25)
summary(crd)
summary(smoking)


#graphs
par(mfrow=c(3,3))
hist(crd, col = "lavender", xlab = "Death rate from chronic respiratory diseases (per 100K people)", main="Distribution of deaths from respiratory diseases \n in countries worldwide in 2019", probability = TRUE)
abline(v=mean(crd), col="maroon")
qqnorm(crd, col="maroon")
qqline(crd)
boxplot(crd,horizontal = T, main="Boxplot of \n chronic respiratory diseases variable", col = "lavender")

hist(pm25, col = "lavender", xlab = "Average PM2.5 concentration (μg/m³)", main="Distribution of PM2.5 concentration \n in countries worldwide in 2019", probability = TRUE)
abline(v=mean(pm25), col="maroon")
qqnorm(pm25, col="maroon")
qqline(pm25)
boxplot(pm25,horizontal = T, main = "Boxplot of \n air quality variable", col = "lavender")

hist(smoking, col = "lavender", xlab = "Prevalence of tobacco use in 2019 (% of adults)", main="Distribution of the smoking rates \n in countries worldwide in 2019", probability = TRUE)
abline(v=mean(smoking), col="maroon")
qqnorm(smoking, col="maroon")
qqline(smoking)
boxplot(smoking,horizontal = T, main = "Boxplot of \n smoking variable", col = "lavender")


#fit a linear regression model
mod = lm(crd ~ pm25 + Smoking, data = data)
summary(mod)



#plotting partial effect of PM2.5 on CRD, holding Smoking constant
par(mfrow=c(1,1))
plot(data$pm25, data$crd, xlab = "PM25", ylab = "CRD", main = "Regression Line for PM25 with Smoking Constant")
pm25_vals <- seq(min(data$pm25), max(data$pm25), length.out = 100)
mean_Smoking <- mean(data$Smoking, na.rm = TRUE)
predicted_crd <- predict(mod, newdata = data.frame(pm25 = pm25_vals, Smoking = mean_Smoking))
lines(pm25_vals, predicted_crd, col = "maroon")



#plotting partial effect of Smoking on CRD, holding PM2.5 constant
plot(data$Smoking, data$crd, xlab = "Smoking", ylab = "CRD", main = "Regression Line for Smoking with PM25 Constant")
smoking_vals <- seq(min(data$Smoking), max(data$Smoking), length.out = 100)
mean_pm25 <- mean(data$pm25, na.rm = TRUE)
predicted_crd <- predict(mod, newdata = data.frame(Smoking = smoking_vals, pm25 = mean_pm25))
lines(smoking_vals, predicted_crd, col = "maroon")


#residuals vs. fitted plot
plot(mod$fitted.values, mod$residuals, xlab = "Fitted values \n lm(crd ~ pm25 + smoking)", ylab = "Residuals", main = "Residuals vs. Fitted")
abline(h = 0, col = "maroon")

#residuals qq plot
qqnorm(residuals(mod), main = "Residuals' Q-Q Plot")
qqline(residuals(mod), col = "maroon")

#breusch-pagan test
library(lmtest)
bptest(mod)