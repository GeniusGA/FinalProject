data <- read.csv("https://raw.githubusercontent.com/GeniusGA/SS9859A/main/boston.csv")
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

