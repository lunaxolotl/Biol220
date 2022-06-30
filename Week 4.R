# prob of 461 success out of 2428 from a 15% obese population 
# why we use dbinom function why we used therotþcal disitirbition D BÝNOM because 
# probability not cumulative 
dbinom(461, size = 2428, prob = 0.15)
# prob of 461 or more success out of 2428 from a 15% obese population
#why ve use pbinom function uppertail lowertail <<< because cumulative 
pbinom(460, size = 2428, prob = 0.15, lower.tail = FALSE)
pbinom(18, size = 100, prob = 0.15, lower.tail = FALSE)


# no seedling
dpois(0, lambda = 1.32)
# exactly 1 seedling
dpois(1, lambda = 1.32)
# more than 1 seedling
ppois(1, lambda = 1.32, lower.tail = FALSE)
# 3 or more seedlings
ppois(2, lambda = 1.32, lower.tail = FALSE)
# less then 2 seedlings
ppois(1, lambda = 1.32, lower.tail = TRUE)
# 2 or less seedlings
ppois(2, lambda = 1.32, lower.tail = TRUE)


# prob of a single fish heavier than 85 grams
pnorm(85, mean = 82, sd = 6.7, lower.tail = FALSE)
# prob of each of 25 fish heavier than 85 grams
pnorm(85, mean = 82, sd = 6.7, lower.tail = FALSE)^25
# prob of mean of 25 fish heavier than 85 grams
# we can use central limit theorem why mean weigths asked mean weights of 25 fish so we use clt
sem = 6.7/sqrt(25)
pnorm(85, mean = 82, sd = sem, lower.tail = FALSE)



# a single baby less than 3000 grams
pnorm(3000, mean = 3339, sd = 612)
# a single baby between 3500-4500
pnorm(4500, mean = 3339, sd = 612) - pnorm(3500, mean = 3339, sd = 612)
# mean of 40 babies less then 3000
sem = 612/sqrt(40)
pnorm(3000, mean = 3339, sd = sem)
# mean of 40 babies btw 3500-4500
sem = 612/sqrt(40)
pnorm(4500, mean = 3339, sd = sem) - pnorm(3500, mean = 3339, sd = sem)
# sum probability of mean of 40 babies
# for being more than mean+500
# for being and less than mean-500
sem = 612/sqrt(40)
pnorm(3339+500, mean = 3339, sd = sem, lower.tail = FALSE) +
  pnorm(3339-500, mean = 3339, sd = sem, lower.tail = TRUE)

#5
#binomial dsitribution

dbinom(180, 365, 0.5)
pbinom(180, 365, 0.8, lower.tail = FALSE)


# We will store in this vector, how many wet days we will have
# in each replication
wetDaysPerSummer = c()
# we will repeat the same scenario for 1000 times
for (i in 1:1000){
  # each time we start we set number wetdays to zero
  wetdays = 0
  # Let's say summer is 90 days long
  for (j in 1:90){
    # for each day we will decide if its rainy or not
    day = sample(c("rain","no rain"), 1, prob = c(0.13,0.87))
    # if its rainy we will toss a coin
    # if it is not, we will do nothing and proceed to next day
    if (day == "rain"){
      coin = sample(c("H","T"), 1)
      # if we get heads from coin toss, we don't take an umbrella
      # and get wet, hence we increase number of wet days by 1
      # if we don't get heads from coin toss, we take an umbrella
      # with us, hence won't increase number of wet days
      if (coin == "H"){
        wetdays = wetdays + 1
      }
    }
  }
  #at the end of each summer, or each replication
  # we record number of wet days on that replication
  wetDaysPerSummer[i] = wetdays
}
#at the end of the simulation,
# we count number of replicatins that had less than
# 10 wetdays, and by dividing that number by total
# replication number, we get ratio of replications
# with less than 10 wetdays.
# this estimates the probability.
sum(wetDaysPerSummer<10)/length(wetDaysPerSummer)

wetDaysPerSummer
wetdays
head(wetDaysPerSummer)

# each of these 1000 values correspond to number of rainy days in a summer
numberOfRainyDays = rbinom(1000, 90, 0.13)
# 1000 times, toss as many coins as number of rainy days (for a particular summer)
# and count number of one of the outcomes in that many coin tosses
# this corresponds to number of rainy days, on which we took no umbrella
numberOfRainyDaysWithNoUmbrella = rbinom(1000,numberOfRainyDays,0.5)
# get the ratio of wet days
sum(numberOfRainyDaysWithNoUmbrella<10)/length(numberOfRainyDaysWithNoUmbrella)
mean(numberOfRainyDaysWithNoUmbrella<10)


a_sample = sample(c("yellow","green"), size = 20, replace = T, prob = c(0.7,0.3))
sum(a_sample == "yellow")
yellow_counts = c()
for (i in 1:500){
  a_sample = sample(c("yellow","green"), size = 20, replace = T, prob = c(0.7,0.3))
  yellow_counts[i] = sum(a_sample == "yellow")
}
hist(yellow_counts, main = "Beans in Samples n=20",
     freq = F, col = "red", xlab = "# of yellow")
sum(yellow_counts < 10)
sum(yellow_counts < 10) / length(yellow_counts < 10)

g1 = sample(c(700,0), size = 1, prob = c(0.2,0.8))
g2 = sample(c(700,0), size = 1, prob = c(0.3,0.7))
g3 = sample(c(700,0), size = 1, prob = c(0.5,0.5))
g4 = sample(c(700,0), size = 1, prob = c(0.6,0.4))
g5 = sample(c(700,0), size = 1, prob = c(0.9,0.1))
e1 = runif(n = 1, min = 200, max = 900)
e2 = rpois(n = 1, lambda = 500)
g1+g2+g3+g4+g5+e1+e2
samplingDist = c()
for (i in 1:1000){
  g1 = sample(c(700,0), size = 1, prob = c(0.2,0.8))
  g2 = sample(c(700,0), size = 1, prob = c(0.3,0.7))
  g3 = sample(c(700,0), size = 1, prob = c(0.5,0.5))
  g4 = sample(c(700,0), size = 1, prob = c(0.6,0.4))
  g5 = sample(c(700,0), size = 1, prob = c(0.9,0.1))
  e1 = runif(n = 1, min = 200, max = 900)
  e2 = rpois(n = 1, lambda = 500)
  samplingDist[i] = g1+g2+g3+g4+g5+e1+e2
}
hist(samplingDist, main = "Simulated Baby Weights",
     xlab = "Weight", ylab = "Frequency", col = "red")


#examples 
for (i in 1:10)
  print(i)

#################

for (i in 1:10) {
  if (i == 5){print(i)}
}


#################

# nested loops 
for (i in 1:10){
  for (k in letters[1:5]){
    print(paste(i,k))
  }
}

#################
vec = 1:10 
vec^2 

sqrs = c()
for (i in 1:length(vec)){
  sqrs[i] = vec[i]^2
}
sqrs
vec


##################
vec = 2


for (i in 2:20){
  vec[i] = vec[i-1]*2+3
}

vec

#####################
col = 10
row = 7

t_ = c(row,col)
s = c(row,col)
for (i in 1:row){
  t_ [i] = print("_")
  s [i] = print("*")
  for (j in 1:col){
    
  }
}
s
t_

t_ = c(row,col)
s = c(row,col)
for (i in 1:col){
   t_ [i] = print("_")
   s [i] = print("*")
   for (j in 1:row){
     print(s)
  }
}
t_
s
