load("Regression.RData")
head(plantnutrition)
# regression line calculation

reg_result = lm(PlantSp~ NutrientNo, data = plantnutrition)
reg_anotherresult = lm(NutrientNo ~ PlantSp, data = plantnutrition)
reg_result
reg_anotherresult
# graphical representation
par(mfrow =c(1,2))
plot(PlantSp~NutrientNo, data = plantnutrition, col = "red", pch =19)
abline(reg_result)
#plot(NutrientNo ~ PlantSp, data = plantnutrition, col = "red", pch = 19) 
#abline(reg_anotherresult)


# checking residuals for normality
residual_values = reg_result$residuals
shapiro.test(residual_values)
# significance of regression line slope
summary(reg_result)
# predicted CI for nutrient type 2
predict(reg_result, data.frame(NutrientNo = 2), interval = "prediction")



head(height_weight)
# first subset the data for "O1"
row_belongs_to_O1 = height_weight[ ,"occupation"] == "O1"
job1 = height_weight[row_belongs_to_O1, ]
# be careful about the order of variables
# we want to predict weight from height
reg_result_job1 = lm(weight ~ height, data = job1)
summary(reg_result_job1)
# same analysis with all individuals in the data set
reg_result_all = lm(weight ~ height, data = height_weight)
summary(reg_result_all)
predict(reg_result_job1, data.frame(height = c(165, 175)), interval = "prediction")
predict(reg_result_all, data.frame(height = c(165, 175)), interval = "prediction")



head(iris)
plot(Sepal.Length~Petal.Width, data = iris)
#there seems to be a positive association
model=lm(Sepal.Length~Petal.Width, data = iris)
par(mfcol=c(2,2))
plot(model)
shapiro.test(model$residuals)
par(mfcol=c(1,1))
plot(Sepal.Length~Petal.Width, data = iris)
abline(model)
summary(model)
predict(model,data.frame(Petal.Width=0.8))
#then visually checking if it is logical or not is always a good idea