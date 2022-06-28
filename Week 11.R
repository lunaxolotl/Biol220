load("Exercise11.RData")
# H0 : 3 populations, from which samples are taken, have equal mean.
# H0 : At least one of the populations have a different mean.
disordersAnova = aov(PLP1Expression~group, data = disorders)
disordersAnovaTable = anova(disordersAnova)
disordersAnovaTable
TukeyHSD(disordersAnova)
SSg = disordersAnovaTable["group","Sum Sq"]
SSe = disordersAnovaTable["Residuals","Sum Sq"]
Rsq = SSg / (SSg+SSe)
Rsq



stripchart(offspringCondition ~ male, data = dung, vertical = T,
           pch = 1, col = "red", main = "Male Dung Beetle's Offspring", xlab = "Father Id",
           ylab = "Body-Condition Score", cex.axis = 0.8, method = "jitter")
dungAnova = aov(offspringCondition~male, data = dung)
dungAnovaTable = anova(dungAnova)
dungAnovaTable
MSG = dungAnovaTable["male","Mean Sq"]
MSE = dungAnovaTable["Residuals","Mean Sq"]
sa_sq = (MSG - MSE)/3
repeatibility = sa_sq/(sa_sq+MSE)
repeatibility




# Writing data
Iso = c(6,6,8,6,9)
Long = c(5,7,7,5,15)
Med = c(12,4,18,4,11)
Cont = c(9,13,16,21,11)
Data = data.frame("Generations" = c(Iso,Long,Med,Cont))
Categories = rep(c("Isolated","Long","Medium","Continous"),each=5)
plant = cbind(Data,Categories)
plant
kruskal.test(Generations~Categories,data = plant)



birdsAnovaTable = anova(aov(Plumage~Populations, data = birds))
birdsAnovaTable
# preliminary calculations for t statistic
meanA = mean(birds[birds[,2] == "A",1])
meanD = mean(birds[birds[,2] == "D",1])
nA = sum(birds[,2] == "A")
nD = sum(birds[,2] == "D")
MSe = birdsAnovaTable["Residuals","Mean Sq"]
SE = sqrt( MSe * (1/nA + 1/nD) )
# t statistic
t = (meanA - meanD)/SE
# degrees of freedom (N-k)
df = length(birds[,2]) - length(levels(birds[,2]))
# check on which tail the calculated t is
# then calculate two-sided p-value
pvalue = if (t<0){
  pt(t,df)*2
}else{
  pt(t,df,lower.tail = F)*2
}
pvalue