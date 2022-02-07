Homework 1 - readable and efficient R code
================
Shuang Du
02/07/2022

# Question 1 - “function-alize” this code

Read over the code below and perform the following:

-   Wrap it into a function `foobar0` which has arguments `x` and `z`
    and which returns the vector `x` at the end of the following code.
-   Rewrite this into a function `foobar` which is easier to read, by
    reducing repetitive code. E.g. `foobar` might call a function to
    check the input, and another function to perform the three lines of
    computation.
-   Check that the two versions produce the same output using the
    function `all.equal`.

``` r
set.seed(1)
x <- rnorm(100)
z <- rnorm(100)
if (sum(x >= .001) < 1) {
  stop("step 1 requires 1 observation(s) with value >= .001")
}
fit <- lm(x ~ z)
r <- fit$residuals
x <- sin(r) + .01
if (sum(x >= .002) < 2) {
  stop("step 2 requires 2 observation(s) with value >= .002")
}
fit <- lm(x ~ z)
r <- fit$residuals
x <- 2 * sin(r) + .02
if (sum(x >= .003) < 3) {
  stop("step 3 requires 3 observation(s) with value >= .003")
}
fit <- lm(x ~ z)
r <- fit$residuals
x <- 3 * sin(r) + .03
if (sum(x >= .004) < 4) {
  stop("step 4 requires 4 observation(s) with value >= .004")
}
fit <- lm(x ~ z)
r <- fit$residuals
x <- 4 * sin(r) + .04
x
```

    ##          1          2          3          4          5          6          7 
    ## -1.0992886  1.4796352 -0.6516207  0.7865093  3.7873388 -0.2055179  3.8327000 
    ##          8          9         10         11         12         13         14 
    ##  2.0871822  3.2219962 -3.0933769  0.6399144  4.0325676 -0.7808135 -0.8326365 
    ##         15         16         17         18         19         20         21 
    ##  0.7073736 -3.2414091 -2.8376440  0.9776866  1.5608532  3.0227535  1.0117977 
    ##         22         23         24         25         26         27         28 
    ##  1.8922308 -1.0474880 -0.4636773  2.8222909 -3.4535959 -3.9591095 -0.5208395 
    ##         29         30         31         32         33         34         35 
    ## -1.9366900  4.0313764  0.7254867 -3.7758444  4.0170200 -3.2639844 -0.4572754 
    ##         36         37         38         39         40         41         42 
    ## -2.5234159 -2.5371644 -3.4025549  0.6499197  1.7909888 -3.9557224 -3.5524879 
    ##         43         44         45         46         47         48         49 
    ##  2.0066135  3.2915276 -0.9643167 -0.8508112  3.8888448  1.7701559 -3.8041387 
    ##         50         51         52         53         54         55         56 
    ##  0.9535984  4.0340296 -1.0616984  3.8507707 -0.6058260  0.4776386  0.5361922 
    ##         57         58         59         60         61         62         63 
    ## -2.6133980 -0.5347483  3.0972939 -3.9543801  1.0621806 -3.1799992  2.4340472 
    ##         64         65         66         67         68         69         70 
    ## -2.1800910 -0.7436082  1.3185751 -0.4879213  0.4959666  0.8098709  0.7497175 
    ##         71         72         73         74         75         76         77 
    ##  3.9472930 -0.6991699  2.9570004 -0.4460233 -0.5400743  3.3793674 -1.9905970 
    ##         78         79         80         81         82         83         84 
    ## -2.7888504 -1.2126583 -0.9658716 -1.4647239 -3.9458928  0.7544273 -0.7574175 
    ##         85         86         87         88         89         90         91 
    ##  3.0949240  3.7880760  1.0486519 -3.3233027  3.9897095  3.1324172 -1.4436191 
    ##         92         93         94         95         96         97         98 
    ##  0.7777796  0.5976052  2.3300564  0.5549268  3.2229364 -0.2445148 -1.4071426 
    ##         99        100 
    ## -0.4052558 -1.9274082

The function that can wrap up the above calculation is:

``` r
footbar0<- function(x,z){
  if (sum(x >= .001) < 1) {
    stop("step 1 requires 1 observation(s) with value >= .001")
  }
  fit <- lm(x ~ z)
  r <- fit$residuals
  x <- sin(r) + .01
  if (sum(x >= .002) < 2) {
    stop("step 2 requires 2 observation(s) with value >= .002")
  }
  fit <- lm(x ~ z)
  r <- fit$residuals
  x <- 2 * sin(r) + .02
  if (sum(x >= .003) < 3) {
    stop("step 3 requires 3 observation(s) with value >= .003")
  }
  fit <- lm(x ~ z)
  r <- fit$residuals
  x <- 3 * sin(r) + .03
  if (sum(x >= .004) < 4) {
    stop("step 4 requires 4 observation(s) with value >= .004")
  }
  fit <- lm(x ~ z)
  r <- fit$residuals
  x <- 4 * sin(r) + .04
  return (x)
}
```

A more efficient function can be written as:

``` r
# Help check the condition to proceed
fooCheck <- function(x, i){
  if (sum(x >= (.001*i)) < i) {
  stop(paste0("step ", i, " requires ", i, " observation(s) with value >= ", i*.001))
}
}

# Perform the three lines of computation
fooComp <- function(x, z, i){
  x <- i * sin(lm(x ~ z)$residuals) + .01 * i
  return(x)
}

# A new version of the function which gets rid of repetitive sections
footbar <- function(x, z){
  for (i in 1:4){
    fooCheck(x, i)
    x <- fooComp(x, z, i)
  }
  return(x)
}
```

Other attemps:

``` r
#attemp1
footbar1 <- function(x, z) {
  lapply(seq_len(4), function(i) (i * sin(lm(x ~ z)$residuals) + .01 * i))
  return (x)
}
```

``` r
#attemp2
footbar2 <- function(x, z) {
  for (i in 1:4) {
    x <- i * sin(lm(x ~ z)$residuals) + .01 * i
    if (sum(x >= .001 * (i)) < i) {
      stop("step",i, "requires",i, "observation(s) with value >= .001")
    }
  }
  return (x)
}
```

``` r
library(microbenchmark)
all.equal(footbar0(x,z),footbar(x,z))
```

    ## [1] TRUE

``` r
microbenchmark(footbar0(x,z),footbar(x,z),footbar1(x,z),footbar2(x,z))
```

    ## Unit: milliseconds
    ##            expr      min       lq     mean   median       uq      max neval
    ##  footbar0(x, z) 2.019194 2.346350 3.921502 2.813043 4.093031 21.28203   100
    ##   footbar(x, z) 2.035504 2.366185 4.870570 2.831796 4.719420 51.70443   100
    ##  footbar1(x, z) 2.049888 2.296284 3.460804 2.726287 3.613392 14.83574   100
    ##  footbar2(x, z) 2.024293 2.378327 3.670994 2.609516 3.320735 45.24616   100

### Conclusion: The new function `footbar(x,z)` has get rid of repetitive codes and has the same output as the original function `footbar0(x,z)`.

# Question 2 - vectorize this code and benchmark

-   Take the following function `f0` and rewrite it as a function `f`,
    which is faster and easier to read, by removing the loop of `i` from
    1 to `m`.
-   Benchmark `f` and `f0` using `microbenchmark`. How much faster is
    `f`?

``` r
n <- 30
p <- 50
p2 <- 25
m <- 1000
set.seed(1)
x <- matrix(rnorm(n*p),nrow=n,ncol=p)
b <- matrix(rnorm(m*p),nrow=m,ncol=p)
a <- matrix(rnorm(m*p2),nrow=m,ncol=p2)

f0 <- function(x,b,a) {
  out <- numeric(0)
  for (i in seq_len(m)) {
    bb <- b[i,]
    aa <- a[i,]
    out <- c(out, sum(x %*% bb) + sum(aa))
  }
  return (out)
}
```

``` r
library(microbenchmark)
#attemp1
f00<- function(x,b,a) {
  re <- sapply(seq_len(m), function(i) (sum(x %*% b[i,]) + sum(a[i,])))
  unname(re)
}
```

``` r
#attemp2
f1 <- function(x,b,a) {
  ans <- numeric(length(m))
  for (i in seq_len(m)) {
    ans[i] <- sum(x %*% b[i,]) + sum(a[i,])
  }
  ans
}
```

``` r
#attemp3
s = rowSums(a)
f2 <- function(x,b,a){
  y <- function(t){
    x %*% t
  }
  sum(apply(b,1,y)) + s
} 
```

``` r
# attemp4 
f <- function(x, b, a){
  colSums(x %*% t(b))+rowSums(a)
}
```

``` r
microbenchmark(f0(x,b,a), f00(x,b,a),f1(x,b,a),f(x,b,a))
```

    ## Unit: milliseconds
    ##          expr      min       lq      mean   median       uq      max neval
    ##   f0(x, b, a) 5.191191 6.138589 10.218805 7.060311 9.305395 56.76013   100
    ##  f00(x, b, a) 4.741296 5.420542  9.525226 6.331161 9.045848 59.38473   100
    ##   f1(x, b, a) 4.246348 4.826605  7.633341 5.475383 6.742031 39.99076   100
    ##    f(x, b, a) 1.078594 1.150422  1.624669 1.312441 1.637980 13.29807   100

### Conclusion: function f(x,b,a) has the best performance on runtime, saved more than 50% per cent of original runtime.

# Question 3 - build a faster t-test

-   Rewrite the following function `getT0` which computes `m` two-sample
    t-tests (equal variance) between two groups as a function `getT`,
    which is faster by using vectorized operations over the `m` sets of
    observations. (There are functions in R packages, such as
    `genefilter::rowttests` which will quickly perform this operation,
    but I want you to write your own function using simple R functions
    like `rowSums`, etc.)
-   Benchmark `getT` and `getT0`. How much faster is `getT`?

``` r
m <- 400
n <- 50
little.n <- n/2
set.seed(1)
x <- matrix(rnorm(m*n),nrow=m,ncol=n)
f <- gl(2,little.n)

getT0 <- function(x, f) {
  ts <- sapply(seq_len(m), function(i) t.test(x[i,] ~ f, var.equal=TRUE)$statistic)
  unname(ts)
}
```

``` r
# calculate the variance for each row in a matrix
rowVars <- function(x) {
  rowSums((x-rowMeans(x))^2) / (ncol(x)-1)
}

# A faster t test function
getT <- function (x, f){
  xbar.diff <- rowMeans(x[, which(f==1)]) - rowMeans(x[, which(f==2)]) # calculate the difference in row means
  n1 <- sum(f==1) # sample size in group 1
  n2 <- sum(f==2) # sample size in group 2
  sp.sq <- ((n1-1)*rowVars(x[, which(f==1)])+(n2-1)*rowVars(x[, which(f==2)]))/(n1 + n2 - 2) #s_p squared
  ts <- xbar.diff/sqrt(sp.sq*(1/n1 + 1/n2)) # output a vector of t test statistics
  return(ts)
}
```

``` r
all.equal(getT0(x,f),getT(x,f))
```

    ## [1] TRUE

``` r
microbenchmark(getT0(x,f),getT(x,f))
```

    ## Unit: microseconds
    ##         expr        min         lq        mean     median          uq
    ##  getT0(x, f) 321649.864 365729.159 462514.7730 412372.295 517877.4495
    ##   getT(x, f)    381.229    438.161    711.9642    535.431    634.2435
    ##         max neval
    ##  951939.332   100
    ##    7761.457   100

### Conclusion: By rewrting the function, `getT(x,f)` is more efficieny than the original function.
