#' HW3: Function factory
#'
#' In this exercise, we will write a function computing the k^{th} order statistics and a function computing the density of a mixture model.
#' Order Statistics Reference: https://en.wikipedia.org/wiki/Order_statistic.
#'
#' 1. Implement a function `kth_order(vec, k)` that takes as input a vector and returns its 
#' k^{th} order statistics. If `k` is not an index of the vector, the function should 
#' return `NA`.
#'    Use the `sort()` and `seq_along()` function to create this function.
## Do not modify this line!
kth_order <- function(vec, k) {
  if (!(k %in% seq_along(vec))) {
    return(NA)
  }
  return(sort(vec)[k])
}

kth_order(c(10,5,3,4,6,8),5)
kth_order = function(vec,k){
  if (k>length(vec)|k<1) return(NA)
  else{
    sort(vec)[k]
  }
}


#' 2. Implement a function `order_factory(k)` that takes as input an index `k` and returns a function that compute the k^{th} order statistics of a vector `vec` using `kth_order`. Don't forget to use `force()`.
## Do not modify this line!

order_factory<-function(k){
  force(k)
  function(vec)
  {
    kth_order(vec, k)
  }
}

order_factory(6)(c(2,7,3,1,5,1))

#' 3. Create a function `my_mixture(x, c1, mu1, sigma1, c1, mu1, sigma1)` that evaluates 
#' the probability density function `p(x) = c1 x N(x; mu1, sigma1) + c2 x N(x; mu2, sigma2)` 
#' at `x`. The sigma here is the standard deviation.
#'    You can assume that 0 <= c1 <= 1, 0 <= c2 <= 1, c1 + c2 = 1, sigma1 > 0, sigma2 > 0.
## Do not modify this line!
my_mixture = function(x, c1, mu1, sigma1, c2, mu2, sigma2){
  c1*dnorm(x,mean = mu1,sd = sigma1)+c2*dnorm(x,mean=mu2,sd= sigma2)
}     


#' 4. Implement a function `mixture_factory(c1, mu1, sigma1, c2, mu2, sigma2)`. This function makes a function that evaluates the probability density function `p(x) = c1 x N(x; mu1, sigma1) + c2 x N(x; mu2, sigma2)`. The sigma here is the standard deviation.
#'    You can assume that 0 <= c1 <= 1, 0 <= c2 <= 1, c1 + c2 = 1, sigma1 > 0, sigma2 > 0. Use the functions `force` and `dnorm`.
## Do not modify this line!
mixture_factory = function(c1, mu1, sigma1, c2, mu2, sigma2){
  force(c1)
  function(x){
    c1 * dnorm(x,mean = mu1,sd = sigma1) + c2*dnorm(x,mean=mu2,sd= sigma2)
  }
  
}
mixture_factory(0.5, 0, 1, 0.5, 0, 1)(31)



#' HW3: function factory 2
#'
#' 1. Create a function `f` that takes an input value `x` and evalutes `f(x) = x^4-2x^3-7x^2+9`.
## Do not modify this line!
f = function(x){
  return(x^4 - 2*x^3 - 7*x^2 + 9)
}

f(2)
#' 2. Create a function factory `polynomial_factory` that takes an input vector `a` and returns a polynomial function, for example,
#'    `polynomial_factory(a=c(9,0,-7,-2,1))` will output the function defined in the previous exercise.
#'    Reminder: use `force`.
## Do not modify this line!
polynomial_factory<-function(a){
  force(a)
  function(x){
    p<-0
    for(i in 0:(length(a)-1)){
      p<-p+a[i+1]*x^(i)
    }
    p
  }
}
pf = polynomial_factory(c(9,0,-7,-2,1))
pf(4)
polynomial_factory(a=c(9,0,-7,-2,1))(1)

#' 3. Use `optimise` to calculate the minimum point `xmin` and minimum value `ymin` between `(-5,10)` for the function `f` in exercise 1.
## Do not modify this line!

ymin = optimise(f,-5:10)$objective
xmin = optimise(f,-5:10)$minimum
optimise(f,-5:10)
#' 4. Use `map`, `polynomial_factory` and `optimise` to calculate the minimum of `f(x)=3x^3-8x^2+2x-7`, `f(x)=x^5-2x^4-3x^2+5x`, and `f(x)=x^4-2x^2+7x+6` within range `(-2,5)`,
#'    save the answer into a list `v`, with length of 3, each component consists of a `minimum` and an `objective` which represent the minimum point and minimum value respectively.
#'    Try to write the code in one line.
## Do not modify this line!
library(purrr)
ls = list(c(-7,2,-8,3),c(0,5,-3,0,-2,1),c(6,7,-2,0,1))
ls
r = map(ls,polynomial_factory)
v = map(r,function(x) optimise(x,lower=-2,upper=5))


#' HW3: Normal distribution factory
#'
#' 1. Set the random seed to zero and save the random seed vector to `seed`. (hint: use the command `seed <- .Random.seed`).
## Do not modify this line!

set.seed(0)
seed = .Random.seed

#' 2. Write a function `norm_factory` that takes as input two arguments `mu` and `var` and returns a function
#' that takes a number `n` as argument and returns `n` samples from a normal distribution with mean `mu` and
#' variance `var`. Use `rnorm` to simulate the sampling.
#' Note: you should use `force()` in `norm_factory`.
## Do not modify this line!

norm_factory = function(mu,var){
  mu = force(mu)
  var = force(var)
  function(n){
    r = rnorm(n,mu,sqrt(var))
    r
  }
}
norm_factory(mu = c(8, -10, -1, 3, 10, -4, -2, -6, 7, 4), var = 5)(10)

#' 3. Use `norm_factory` to create two functions allowing to sample from distributions with the following
#' characteristics:
#' - `d0_1`: from a normal distribution with mean zero and standard deviation 1
#' - `d0_2`: from a normal distribution with mean zero and standard deviation 2
#' Then, use the two functions to generate two vectors of 1000 samples from each distributions,
#' and store them respectively in `s0_1` and `s0_2`.
#' Note 1: respect the order when you sample (i.e., use `d0_1` and only then use `d0_2`), otherwise the automated won't work.
#' Note 2: you can't call `rnorm` directly in this question.
## Do not modify this line!

d0_1 = function(x){
  norm_factory(0,1)(x)
}
d0_2 = function(x){
  norm_factory(0,4)(x)
}
s0_1 = d0_1(1000)
s0_2 = d0_2(1000)

#' 4. Compute the empirical mean and variance of each vector and assign them to `m_s0_1`, `m_s0_2`, `v_s0_1`,
#' `v_s0_2`. You should use the built-in functions `mean` and `var`.
## Do not modify this line!

m_s0_1 = mean(s0_1)
m_s0_2 = mean(s0_2)
v_s0_1 = var(s0_1)
v_s0_2 = var(s0_2)

#' 5. Write a function `m_norm_factory` that takes as input three arguments `m`, `mu` and `var` where `m` is
#' a positive integer, `mu` is a vector of size `m` and `var` is a positive real number. It should return a
#' function that takes a number `n` as argument and returns `n` samples from a multivariate normal distribution
#' with `m` components, mean `mu` and a diagonal covariance matrix in which the variance of each component is
#' `var` if `var` is smaller than 2 and 2 otherwise. To do so, load the package `MASS` and use its function
#' `mvrnorm` to simulate the sampling.
#' Note: you should use `force()` in `m_norm_factory`.
#'
## Do not modify this line!
library(MASS)
library(purrr)
multivariate_norm_factory = function(m,mu,var){
  force(m)
  force(mu)
  covar = diag(min(var,2), m)
  function(n){
    mvrnorm(n, mu, covar)
  }
  
}
n = 10
m= 10
mu = c(-7, 7, 1, -4, -3, 10, 9, 5, -9, -5)
var = 3

multivariate_norm_factory(m,mu,var)(n)
#diag(var, n)



