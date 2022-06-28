load("w12.rdata")
head(pufferfish)
class(pufferfish)
class(pufferfish$Resemblance)
class(pufferfish$Predators)
plot(pufferfish$Resemblance, pufferfish$Predators,
     col = "red", pch = 19, xlab = "similarity", ylab = "predators",
     main = "mimicry")
qqnorm(pufferfish$Resemblance); qqline(pufferfish$Resemblance)
qqnorm(pufferfish$Predators); qqline(pufferfish$Predators)
shapiro.test(pufferfish$Resemblance)
shapiro.test(pufferfish$Predators)
cor_result = cor.test(pufferfish$Resemblance, pufferfish$Predators)
cor_result
cor_result$estimate
cor_result$p.value
head(height_weight)
job1 = height_weight[height_weight$occupation=="O1", ]
cor_result2 = cor.test(job1$height, job1$weight)
cor_result2$estimate
cor_result2$p.value