x = c(50, 50, 60, 70, 75, 80, 90, 85)
y = c(55, 75, 80, 90, 105, 65)
par(mfrow = c(1,2))
hist(x, col = "gray40")
qqnorm(x,pch = 19,col = "darkred"); qqline(x, lwd = 2, col = "blue")
hist(y, col = "gray40")
qqnorm(y, pch = 19,col = "darkred"); qqline(y, lwd = 2, col = "blue")
# sample size is too small. Question does not give information about the distribution of the data
# quantile plots looks OK but histograms does not look like normally distributed for variable x.
# Therefore, we should use non-parametric test:
wilcox.test(x, y, correct = FALSE, paired = FALSE)



x = c(509, 517, 502, 629, 830, 911, 847, 803, 727, 853, 757, 730, 774, 718, 904)
y = c(517, 508, 523, 730, 821, 940, 818, 821, 842, 842, 709, 688, 787, 780, 901)
par(mfrow = c(1,2))
hist(x, col = "gray40")
hist(y, col = "gray40")
qqnorm(x, pch = 19, col = "darkred"); qqline(x, lwd = 2, col = "blue")
shapiro.test(x)
qqnorm(y, pch = 19,col = "darkred"); qqline(y, lwd = 2, col = "blue")
shapiro.test(y)
# Sample size is not large. There are deviations at the tails of quantile plots.
# y is barely significant in shapiro test.
# Better to use a non-parametric test:
wilcox.test(x, y, correct = FALSE, paired = TRUE)


death=as.numeric(UKDriverDeaths)
par(mfrow=c(1,2))
hist(death,col="orange",breaks = 40)
hist(death,col="orange",prob=T,breaks = 40)
lines(density(death),col="blue",lwd=2)
# Histogram of the data looks normal but right skewed.
qqnorm(death,pch=19,col="darkred");qqline(death,lwd=2,col="blue")
# There are big deviations from expectation at the tails in quantile plot.
shapiro.test(death)
# shapiro test gives significant result indicating non-normality.
# let's try log2 transformation and check distribution of the data again:
par(mfrow=c(1,2))
hist(log2(death),col="gray40",breaks = 40)
hist(log2(death),col="orange",prob=T,breaks=40)
lines(density(log2(death)),col="blue",lwd=2)
# histogram looks less skewed, more like normal.
par(mfrow=c(1,1))
qqnorm(log2(death),pch=19,col="darkred");qqline(log2(death),lwd=2,col="blue")
# no big deviations in the quantile plot
shapiro.test(log2(death))
# test result is not significant, indicating normality
# So, we can use parametric test on log2 transformed data:
t.test(log2(death),mu =log2(1600))
# p value is less than 0.05, we reject the null hypothesis, mean car deaths are not 1600