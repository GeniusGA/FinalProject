data <- read.csv("https://raw.githubusercontent.com/GeniusGA/SS9859A/main/boston.csv")
head(data)

m1 = lm(medv ~., data =data )
summary(m1)
anova(m1)