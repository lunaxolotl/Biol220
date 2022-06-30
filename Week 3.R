#1
getwd()
object=read.csv(file="desertbird.csv", row.names = 1)
head(object)
class(object$X)
categorical
object$X
dim(object)
43
barplot
barplot(object$abundance, names.arg = row.names(object)[order(object$abundance)], las=2, cex.names = 0.7, ylim = c(0, 700), sub = "X", xlab = "X", col=1:43, main = "desertbird")
colors()
object$abundance
order(object$abundance)
row.names(object)[order(object$abundance)]
#2
object1=read.csv("spider.csv", row.names = 1)
head(object1)
dim(object1)
32/2
before=object1$treatment=="before"
before
beforea=object1[before,]
after=object1$treatment=="after"
aftera=object1[after,]
boxplot(beforea$speed, aftera$speed, names=c("before", "after"), col = c("yellow", "darkblue"), main= "spidersspeed", xlab="treatment", ylab="speed")
boxplot(object1$speed~object1$treatment)
levels(object1$treatment) #alphabetical order
after
c(mean(beforea$speed), sd(beforea$speed))
mean(beforea$speed)
mean(aftera$speed)
sd(beforea$speed)
sd(aftera$speed)
#when we look at the mean values, hypothesis looks right, further after amputation sd value is bigger than that of the before so it may further amplifies the hypothesis' estimation positively.
#3
object2=cars
plot(object2$dist, object2$speed, pch=19, col=1:50, xlab="dist", ylab="speed", main="cars")
or
plot(object2$dist~object2$speed,...)
dim(object2)
#we can see from the plot that distance shows generally an increasing pattern with increasing speed.
#4
object3=DNase
class(object3$Run)
class(object3$conc)
class(object3$density)
DNase.r3=object3$Run=="3"
DNase3=object3[DNase.r3,]
DNase3
or
DNase3=DNase[DNase$Run==3,]
DNase.r5=object3$Run=="5"
DNase5=object3[DNase.r5,]
DNase5
DNase.r8=object3$Run=="8"
DNase8=object3[DNase.r8,]
DNase8
DNase.r11=object3$Run=="11"
DNase11=object3[DNase.r11,]
DNase11
par(mfrow=c(2,2))
plot(DNase3$density, DNase3$conc, pch=19, col=1:16, main="DNase3", xlab="density", ylab="conc")
plot(DNase5$density, DNase5$conc, pch=19, col=1:16, main="DNase5", xlab="density", ylab="conc")
plot(DNase8$density, DNase8$conc, pch=19, col=1:16, main="DNase8", xlab="density", ylab="conc")
plot(DNase11$density, DNase11$conc, pch=19, col=1:16, main="DNase11", xlab="density", ylab="conc")
or
levels(DNase$Run)
plot(DNase$conc, DNase$density, pch=19, col= rainbow(11)[DNase$Run])
selection=DNase$Run==3|DNase$Run==5|DNase$Run==8|DNase$Run==11
plot(DNase$conc[selection], DNase$density[selection], pch=19, col= rainbow(11)[DNase$Run][selection])
#yes, they show a consistency among them.
?DNase
coplot(density ~ conc | Run, data = DNase,
       show.given = FALSE, type = "b")
par(mfrow=c(1,1))


