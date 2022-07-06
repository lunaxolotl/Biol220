# confidence intervals 

load(file = "w5_labData.Rdata")
ls()
head(humangenes)
mean(humangenes$length)

genelengths = humangenes$length

head(genelengths)
length(genelengths)
mean(genelengths)

popmean = mean(genelengths)
popmean

#used sizes 10,50,500

#n = 10
s_10 = sample(genelengths, size = 10) # randomly chooses 10 samples from whole data
s_10
s10_mean = mean(s_10)
s10_mean
s10se = sd(s_10)/ sqrt(length(s_10)) #central limit theorem standard deviation over square root of sample size 
s10se
s10ci = c(s10_mean- 2*s10se,
          s10_mean + 2*s10se)
s10ci

# 18.4kb -84kb this interval very wide 

# n = 50 
s_50 = sample(genelengths, size = 50)
s_50
s50_mean = mean(s_50)
s50_mean
s50se = sd(s_50)/ sqrt(length(s_50))
s50se
s50ci = c(s50_mean- 2*s50se,
          s50_mean + 2*s50se)
s50ci
# 38.7 - 104kb

# n = 500
s_500 = sample(genelengths, size = 500)
s_500
s500_mean = mean(s_500)
s500_mean
popmean
s500se = sd(s_500)/ sqrt(length(s_500))
s500se
s500ci = c(s500_mean- 2*s500se,
          s500_mean + 2*s500se)
s500ci
# 48.8kb - 64.8kb


# hypothesis testing 
### toads

## H0 : p_right == 0.5
## H0 : p_right == 0.5

14 - 9 ## this is the too-much-rh eq
9 - 4 ## this is the too-few-rh eq

## creating the null dist 

toads_null = c()
for (i in 1:1000){
  our_sample = sample(c("R", "L"),
                      size = 18,
                      replace = T,
                      prob = c(0.5,0.5))
  toads_null[i] = sum(our_sample == "R")
}
toads_null
our_sample
length(toads_null)
mean(toads_null)

#barplot(table(toads_null))
hist(toads_null,col = "pink")
abline(v = 14, col = "darkred", lwd = 3)
abline(v = 4, col = "orange", lwd = 3)
abline(v = 9, col = "black", lwd = 3)

toads_freq = table(toads_null)
toads_freq

toads_freq = as.data.frame(toads_freq) # we can convert our data to data frame with the help of as.data.frame function  
toads_freq
#14 or more right handed AND or less right handed 

left = toads_freq[toads_freq$toads_null <= 4,] # we need to convert factors to numeric so that use this expression
class(toads_freq$toads_null)
# firstly check the class of data 

toads_freq$toads_null = as.numeric(as.character(
  toads_freq$toads_null))
# we can convert factors to numeric 
# and then we can use this expression 

class(toads_freq$toads_null)
left = toads_freq[toads_freq$toads_null <= 4,]
right= toads_freq[toads_freq$toads_null >= 14,]
left 
right

s_left = sum(left$Freq)
s_right = sum(right$Freq)


s_left
s_right

s_all = s_left + s_right
s_all
pval = s_all / 1000
pval

alpha = 0.05

pval < alpha # null hypothesis rejected which means alternative hypothesis is supported

#catching fish example

## x = 3

### H0 : lambda == 7
### HA : lambda < 7


fish_null = c()
for (i in 1:1000) {
  fish_null[i] = rpois(n = 1, lambda = 2) + rpois(n = 1, lambda = 5)
}
fish_null
a = rpois(n = 1, lambda = 2)
b = rpois(n = 1, lambda = 5)
a+b 

dpois(x, lambda = ,log = FALSE)
qpois_try = c()
for (i in 1:1000) {
  qpois_try[i] = qpois(p = 0.9,  lambda = 10)
}
qpois_try

#for loop is just null distribution
head(fish_null)
hist(fish_null, col = "pink")
abline(v = 3, col = "darkred", lwd = 3)

fish_freq = table(fish_null)
fish_freq = as.data.frame(fish_freq)
fish_freq$fish_null = as.numeric(as.character(fish_freq$fish_null))
class(fish_freq$fish_null)
head(fish_freq)

leftfish = fish_freq[fish_freq$fish_null <= 3,]
rightfish= fish_freq[fish_freq$fish_null >= 14,]

s_leftfish = sum(leftfish$Freq)
s_leftfish
pvalue = s_leftfish/1000
pvalue
alpha

pvalue < alpha # comment




##########Exercises
load(file = "w5_excData.RData")

head(cereal)
sugarmean = mean(cereal$sugarContent)
sugarmean
sugar_se = sd(cereal$sugarContent) / sqrt(length(cereal$sugarContent)) # central limit theorem this is calculated via formula 
sugar_se
sugar_ci = c(sugarmean - 2*sugar_se, sugarmean + 2*sugar_se) # confidence interval formula ci is the confidence interval and also this is calculated via formula 
sugar_ci


flowers = c()
for(i in 1:1000){
  our_sampleflower = sample(c("R", "L"), size=27, replace = T, prob=c(0.75,0.25))
  flowers[i] = sum(our_sampleflower == "L")
}
our_sampleflower
flowers

hist(flowers, col='pink', main='')
abline(v = 6, col='darkred', lty=2, lwd=3)

flowers_freq = as.data.frame(table(flowers))
flowers_freq$flowers = as.numeric(as.character(flowers_freq$flowers))
head(flowers_freq)

flowers_obs = 6   ##
flowers_exp = 27 * 0.25
deviation = abs(flowers_exp - flowers_obs) # absolute value 
left_boundary = flowers_exp - deviation
right_boundary = flowers_exp + deviation
left_boundary;right_boundary

leftflower = flowers_freq[flowers_freq$flowers <= left_boundary, ]
rightflower = flowers_freq[flowers_freq$flowers >= right_boundary, ]
s_leftflo = sum(leftflower$Freq)
s_rightflo = sum(rightflower$Freq)
s_allflo = s_leftflo + s_rightflo
pvalueflo = s_all / 1000
pvalueflo

##pval = mean(flowers <= left_boundary) + mean(flowers >= right_boundary)

 
#h0 lambda = 10
#ha lambda > 10


right_boundarycell = 12
pval = ppois(11, lambda = 10, lower.tail = F) # 11 is coming from 12-1
pval1 = ppois(11, lambda = 10, lower.tail = T)
pval
pval1
a = 1 - pval 
a
# we can say that being true or false makes the function rest of that is (1 - var)
#failed to rejected our null hypothesis alpha is smaller than our p value that means that is not significant  


## HA is "less than" so observation becomes the left boundary

#lambda = 5 
#lambda < 5

left_boundarybus = 2

pval = ppois(left_boundary, lambda = 5, lower.tail = T)
pval

#failed to be rejected our null hypothesis alpha is smaller than p value
#that means p value is not significant 


head(titanic)
firstclass = titanic[titanic$pclass == 1, ] # a bit confusing take a look at them
first_total = nrow(firstclass)
first_survived = sum(firstclass$survived == 1)
first_died = sum(firstclass$survived == 0)
first_expected = first_total * 0.5


first_null = c()
for(i in 1:1000){
  our_sampletitanic = sample(c("survive", "die"), size=first_total,
                      replace=T, prob = c(0.5, 0.5))
  first_null[i] = sum(our_sampletitanic == "survive")
}
first_null



deviation = abs(first_expected - first_survived)
deviation
left_boundary = first_expected - deviation
right_boundary = first_expected + deviation
left_boundary

right_boundary

hist(first_null, col = 'palegreen', main='')
abline(v = c(left_boundary, right_boundary) , 
       col=c('darkred', 'orange'), lty=2, lwd=2)

#pvaluetit = mean()

first_freq = as.data.frame(table(first_null))
first_freq$first_null = as.numeric(as.character(first_freq$first_null))
left_boundary

right_boundary

head(first_freq)
tail(first_freq)

s_left = sum(left$Freq)
s_right = sum(right$Freq)
s_all = s_left + s_right
s_all

pval = s_all / 1000
pval


####################

P(heads) = 0.5
p(heads) != 0.5

head(gamecoins)
coins_expected = 100 * 0.5
coins_observed = 62
abs_deviation = abs(coins_observed - coins_expected)
abs_deviation
left_boundary = coins_expected - abs_deviation
right_boundary = coins_expected + abs_deviation
left_boundary
right_boundary
leftgamecoins = gamecoins[gamecoins$numHeads <= 38, ]
rightgamecoins = gamecoins[gamecoins$numHeads >= 62, ]
leftgamecoins
rightgamecoins

s_leftg = sum(leftgamecoins$count)
s_rightg = sum(rightgamecoins$count)
s_allg = s_leftg + s_rightg
pvall = s_allg / 1000
pvall
#we can rejected the hypothesis because alpha is bigger than our p values 
#that means p value is significant and supports alternative hypothesis


#one-sided
pvalr = s_rightg / 1000
pvalr


