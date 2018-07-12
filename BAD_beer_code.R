

#Beer Data Code.....
#This is what you SHOULD NOT DO! 
#This is what you SHOULD NOT DO! 
#This is what you SHOULD NOT DO! 
#This is what you SHOULD NOT DO! 

beer <- read.table("./data/quarterly-beer-production-in-aus.csv", sep=",", header=T)
head(beer)
Beer <- cbind(1:nrow(beer),beer$Megalitres)
Beer2 <- cbind(1:nrow(beer),beer$Megalitres);colnames(Beer2)=c("Time", "Megalitres")
colnames(Beer)=c("Time", "Megalitres")
Beer <- as.data.frame(Beer[1:75,])
head(Beer)
plot(Beer, main="Beer Production in Megalitres")
Best.Model <- lm(Megalitres~I(Time^2), Beer)
Best.Model
summary(Best.Model)
anova(Best.Model)
par(mfrow=c(2,1))
plot(residuals(Best.Model), main="Obviously Non-Constant Variance, fix that with a simple transformation")
qqnorm(residuals(Best.Model),main="Who Cares, call it Data Science and move on...");abline(mean(residuals(Best.Model)),sd(residuals(Best.Model)))
par(mfrow=c(1,1))


#Just to be EXTRA sure we are doing it correctly, lets bootstrap, because why not, thats what you are supposed to do...
#Just to be EXTRA sure we are doing it correctly, lets bootstrap, because why not, thats what you are supposed to do...
#Can you feel the sarcasm? 
#Can you feel the sarcasm? 
Beta1 <- numeric()
Beta2 <- numeric()
system.time(for (i in 1:1000){
	index.to.use <- sample(c(1:75), size=35, replace=T)
	Beer.to.use <- Beer[index.to.use,]
	Beta1[i] <- coef(lm(Megalitres~I(Time^2), Beer))[1]
	Beta2[i] <- coef(lm(Megalitres~I(Time^2), Beer))[2]
	
})

par(mfrow=c(2,1))
plot(density(Beta1), main="Nonparametric Density Estimate of the Distribution of Bootstrapped Intercept Parameter");abline(v=median(Beta1))
median(Beta1)
plot(density(Beta2), main="Nonparametric Density Estimate of the Distribution of Bootstrapped Slope Parameter");abline(v=median(Beta2))
median(Beta2)
par(mfrow=c(1,1))


#So we want to predict a few months out because that is reasonable after all...

New.Data <- data.frame(Time=c(1:154))

My.Predictions <- predict(Best.Model, newdata = New.Data); My.Predictions
plot(Beer2, xlab="Time in Quarters", ylim=c(190, 1200), main="Prediction of Beer Production in Megalitres");points(My.Predictions, col="red", pch=20)







