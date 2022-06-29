#First quesiton
x = 20
r = 0.4
times = 1:8

first_generation = x*(1+r)^times
print(first_generation)

x = 10
r = 0.6

second_generation = x*(1+r)^times
print(second_generation)

first_generation-second_generation

# Second Question
x = 3.79*2^-0.84
y = 1.88*log(2.18,2)

true_result = (x-y)^2/(x-y)^3
print(true_result)

rtrue_result = round(true_result,2)
print(rtrue_result)

print(x)
rounded_x = round(x,2)
print(rounded_x)

print(y)
rounded_y = round(y,2)
print(rounded_y)

early_rounded = (rounded_x-rounded_y)^2/(rounded_x-rounded_y)^3
print(early_rounded)

c = true_result-early_rounded
print(c)

# Third question

a = 5
b = 6
print(a)
print(b)

c = a
a = b
b = 5

print(a)
print(b)

#
a = 5
b = 6

a = (b - a) + (b = a)
print(a)
print(b)

#or (general)
a <- 5
b <- 6

a <- a + b
b <- a - b
a <- a - b

print(a)
print(b)

# or (not necessary)
a = 5
b = 6
print(a)
print(b)
swap <- function(x,y) {
  eval( parse( text = paste(
    "swap_unique_var_a<-", substitute(x), ";",
    substitute(x), "<-", substitute(y), ";",
    substitute(y), "<-swap_unique_var_a") ), env=parent.frame() )
}

swap(a,b)
print(a)
print(b)

# Fourth question


# Fifth question


# is is realted to print funciton and comparison between two really close numbers
# There appears to be some confusion regarding this exercise. Growth goes like this:
# x1 = x0 * (1+r)
# x2 = x1 * (1+r)
# x3 = x2 * (1+r)
# this can also written as:
#   x1 = x0 * (1+r)
# x2 = x0 * (1+r) * (1+r)
# x3 = x0 * (1+r) * (1+r) * (1+r)
# which is:
#   x1 = x0 * (1+r)^1
# x2 = x0 * (1+r)^2
# x3 = x0 * (1+r)^3
# and we can summarize it like this:
#   xt = x0 * (1+r)^t
# 
# When computing the population numbers for multiple generations, we use a property in R as follows: It is obvious how the calculation works when every object (x0, r, t) is a scalar (single number). But when one of them is a vector having multiple values (like t in our case), R repeats the same calculation for each element in that vector, and gives us multiple output values (in the form of a vector) that correspond to each input value. When several objects in the expression are multi-element vectors, things may get complicated and we don't prefer to use such expressions.

# Sixth question



