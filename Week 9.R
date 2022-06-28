load("labweek9.RData")
head(temp)
t.test(temp$temperature, mu = 98.6)
hist(temp$temperature, col = "gray")

head(blackbird)
View(blackbird)
Ab_before = blackbird$Ab.conc[blackbird$Group == "before"]
Ab_after = blackbird$Ab.conc[blackbird$Group == "after"]

hist(Ab_before - Ab_after) 

l_Ab_before = blackbird$log.Ab.conc[blackbird$Group == "before"]
l_Ab_after = blackbird$log.Ab.conc[blackbird$Group == "after"]

hist(l_Ab_before - l_Ab_after)

t.test(blackbird$log.Ab.conc ~ blackbird$Group, paired = T)
# p value is bigger than 0.05 so we can failed to reject null hypothesis there 
#is no significant change in immune system after the testosterone treatment


#h0 is alive <= killed
#ha is alive > killed 

head(lizard)
summary(lizard)
t.test(lizard$HornLength ~ lizard$Survival, var.equal = T, alternative = "greater" )

killed = lizard$HornLength[lizard$Survival == "killed"]
alive = lizard$HornLength[lizard$Survival == "living"]

t.test(alive, killed, var.equal = T, alternative = "greater" )
t.test(killed, alive , var.equal = T, alternative = "less")

dolphins = c(77.7, 84.8, 79.4, 84.0, 99.6, 93.6, 89.4, 97.2)
mean(dolphins)
t.test(x=dolphins, mu = 50, alternative = "greater")$conf.int
t.test(x=dolphins, mu = 50, alternative = "greater", conf.level = 0.99)$conf.int


pg = PlantGrowth
head(pg)
control = subset(x = pg, group == "ctrl", reindex.all = F)
treatment2 = subset(x = pg, group == "trt2", reindex.all = F)
t.test(treatment2$weight, control$weight, alternative = 'greater',
       var.equal = T)


seal = data.frame(Nonfeeding = c(42.2,51.7,59.8,66.5,81.9,82.0,81.3,81.3,96.0,104.1),
                  Feeding = c(71.0,77.3,82.6,96.1,106.6,112.8,121.2,126.4,127.5,143.1))
head(seal)
diff = seal$Feeding - seal$Nonfeeding
mean(diff)
t.test(seal$Feeding, seal$Nonfeeding, paired = T, alternative = "greater")
t.test(seal$Feeding, seal$Nonfeeding, paired = T, alternative = "greater",
       conf.level = 0.99)$conf.int