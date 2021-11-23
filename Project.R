data <- read.csv("https://raw.githubusercontent.com/GeniusGA/FinalProject/main/boston.csv")
head(data)

m1 = lm(medv ~., data =data )
summary(m1)
anova(m1)

### The relationship betweeen each variable
pairs(data)

# The 90% confidence interval for the variable
confint(m1,level=0.9)


par(mfrow=c(1,2))
plot(fitted(m1), resid(m1), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",cex=2,
     main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
qqnorm(resid(m1), col = "grey",pch=20,cex=2)
qqline(resid(m1), col = "dodgerblue", lwd = 2)

library(faraway)
vif(m1)



### backward Selection
fit_back_aic = step(m1, direction = "backward")

### Forward Selection
fit_null = lm(medv~1,data=data)
fit_forw_aic = step(fit_null, 
                    scope = mdev ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat, 
                    direction = "forward")

### Stepwise Selection
fit_null = lm(medv~1,data=data)
fit_forw_aic = step(fit_null, 
                    scope = mdev ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat, 
                    direction = "both")

# After the Stepwise function
fit_step = lm(medv~lstat + rm + ptratio + dis + nox + chas + black + zn+ crim+rad+tax,data = data)
summary(fit_step)

### Compute the VIF Score
vif(fit_step)

### Plot the graph
par(mfrow=c(1,2))
plot(fitted(fit_step), resid(fit_step), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",cex=2,
     main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
qqnorm(resid(fit_step), col = "grey",pch=20,cex=2)
qqline(resid(fit_step), col = "dodgerblue", lwd = 2)

### Print the covariance matrixx for model parameter
vcov(fit_step)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit_step)
