
#H0: proportion of survivals >= 0.8
#HA: proportion of survivals < 0.8
binom.test(14.0, n = 20, p = 0.8, alternative = "less")

# p value ?= 0.05 or not
  # p value = 0.1958 hence not significant. p value supports null hypothesis 

# 2nd part: estimation
14/20
binom.test(14, n = 20, conf.level = 0.95)
# p = ... in the function does not affect anything 

# we can also show the confidence interval like this
binom.test(14, n = 20)$conf.int


# CI (confidence interval), 95% confidence level if the sample size increase,
  #confidence interval will be narrower

##### 2nd exercise
binom.test(2, n = 40, p = 0.2, alternative = "less")
# we have small p value reject the null hypothesis. 
# p value supports the alternative hypothesis

#H0: prop >= 0.2 (opposite of producers)
#HA: prop < 0.2 (producers allies)
  #reject to null hypothesis 
  # we have a significant p value


### 95% CI
binom.test(6101, n = 9821, conf.level = 0.95)$conf.int #not reasonable 50:50
#based on this data we support Murphy's Law?
#H0: prop <= 0.5
#HA: prop > 0.5
binom.test(6101, n = 9821, p = 0.5, alternative = "greater")$p.val

binom.test(6101, n = 9821, p = 0.5, alternative = "greater")

# significant p value very low p value 


############ My questions 
# what does actually mean less two sided and greater alternative vectors in the function?
# how they work?