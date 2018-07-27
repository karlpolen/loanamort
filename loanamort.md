---
title: "Level payment loan  functions in R"
author: "Karl Polen"
date: "Thursday, July 03, 2014"
output: 
  html_document:
      keep_md: TRUE
---


### Abstract

Mortgage loans, car loans, some commercial real estate loans and other loans are structured with a fixed interest rate and equal payments that amortize the loan.  In this post, we review the math of level payment loans and present a function for analyzing them. Files for this post are found at https://github.com/karlpolen/loanamort

### Defined terms

When considering level payment loans, there are four variables:

$r$ is the interest rate, expressed as a decimal, not a percent.  $r > -1$.  Values of $r$ equal to -1 or less imply a lender does not want to be repaid and do not need to be considered.  Values be 0 and -1 are useful because lending rates can be below zero and, more importantly, they are useful for ex post analysis of loans that earn a negative return because they were not repaid according to their terms.

$n$ is the number of periods in the loan.  Initially, we only consider periodicity where the interest $r$ is the same time period as $n$.  In the actual R function accompanying this post, we provide the capability to state the interest rate as an "APR", which by market convention is 12 times a monthly interest rate for mortgages and other loans with monthly payments.

$pmt$ is the periodic payment.

$bal0$ is the initial loan balance.

If you state three of the above values, you can calculate the remaining one.

We add two derivative variable for convenience in presenting formulas.

$d$, a discounting factor for determing present values is $\frac{1}{1+r}$.  $d^n$ is the required discount to convert a payment in period $n$ to its present value.

$f$, a factor to convert a current value to a future value is $1+r$.  $f^n$ is the future value factor for period $n$

Some useful identities follow:

$f=\frac{1}{d}$

$r=\frac{1}{d}-1 = \frac{1-d}{d}$

$\frac{1}{r}= \frac{d}{1-d}$


### Derivation of value of perpetuity

Let's consider the case where $n = \infty$

$perp= \sum_{i=1}^{\infty} pmt \cdot d^i$

$d \cdot perp = \sum_{i=2}^{\infty} pmt \cdot d^i$

$perp \cdot (1-d) = pmt \cdot d$

$perp = pmt \cdot \frac{d}{1-d} = \frac {pmt}{r} \:$ **(1)**

### Derivation of value of finite stream of payments

$bal0 = \sum_{t=1}^{n} pmt \cdot d^t$

$d \cdot bal0 = \sum_{t=2}^{n+1} pmt \cdot d^t$

$bal0 \cdot (1-d) = pmt \cdot d - pmt \cdot d^{n+1}$

$bal0 = \frac {pmt}{r} - pmt \cdot d^n \cdot \frac {d}{1-d}$

$bal0 = \frac {pmt}{r} - \frac {pmt}{r} \cdot d^n = \frac {pmt}{r} \cdot (1 - d^n) = \frac  {pmt}{r} \cdot (1-{(\frac{1}{1+r})}^n) = perp - perp \cdot d^n \:$ **(2)**

Note:  this formula amounts to saying the value of an annuity for n payments at time zero is the same as the value of a perpetuity at time zero minus the present value (at time zero) of a sale of a perpetuity at time n

### Derivation of the required level payment to fully amortize a loan over n payments

from **(2)**

$pmt = \frac{bal0 \cdot r}{1-d^n} = \frac{bal0 \cdot r}{1-{(\frac{1}{1+r})}^n} \:$ **(3)**

### Derivation of number of payments given the remaining information

from **(3)**

$1-d^n = \frac{bal0 \cdot r}{pmt}$

$d^n= 1-\frac{bal0 \cdot r}{pmt}$

$n \cdot log(d) = log(1-\frac{bal0 \cdot r}{pmt})$

$n = \frac {log(1-\frac{bal0 \cdot r}{pmt} )}{log(d)}$

### Finding the interest rate given the remaining information

The cash flow of the loan is in the form of a polynomial.

$0= -bal0 + pmt \cdot d^1 + pmt \cdot d^2 + \dots + pmt \cdot d^n$

and we are looking for the value of $d$ where the polynomial evaluates to zero.  From the rule of signs, we know that this polynomial has a single positive root.  

Recall that $d=\frac{1}{1+r}$ .  Let's look at a plot of $d$ as a function of $r$


```r
r=seq(-3,2,.01)
d=1/(1+r)
plot(r,d,type='l',col='blue',ylim=c(-10,10))
abline(v=0)
abline(h=0)
```

![](loanamort_files/figure-html/unnamed-chunk-1-1.png)<!-- -->
Note the plot is discontinuous when $r=-1$, i.e. the rate is -100%.  So, it turns out that stating the polynomial in a form where $d$ is the unknown variable has a useful property.  We can solve for the one positive root $d$ which can then be further evaluated to the single interest rate (which can be positive or negative) that causes the net present value of the cash flow to be zero.  

As discussed above, we need not concern ourselves with $r \leq -1$.  Therefore, we need not calculate roots for $d \leq 0$,

R provides a function `uniroot` which can reliably be used to find a single root in a known range.  

Code as simple as the following will do the trick.  For calculation efficiency, you can confine the search for values of `d` between 0 and 1 for situations where the sum of the cash flows is greater than zero and the interest rate we are looking for is, therefore, known to be greater than zero.

```r
cf=c(-bal0,rep(pmt,n))
if(0<=sum(cf)) {
  rootrange=c(0,1.01)} else {
    rootrange=c(1,1000)
  }
d=(uniroot(function(d) {sum(cf*d^(0:n))}),c(0,1000))$root
r=(1/d)-1
```

### Calculating the amortization of the loan

Now that we have determined methods for finding the values of $n$, $bal0$, $r$, and $pmt$ the next task is to consider how a level payment loan will amortize.  We will want to know the balance of the loan after a given payment and how each payment is divided between interest and principal.  We define three new variables:

$bal_t$, a time series of the loan balance after payment $t$, subject to $bal \geq 0$, and $t$ is the time after payments $1:n$ 

$int_t$, a time series of the interest portion of payment $t$

$prin_t$, a time series of the principal portion of payment $t$

$bal_i$ is a particular balance after payment $i$.  This can be calculated as the future value of $bal0$ at $i$ minus the future value of $i$ payments.

$bal_i = f^i \cdot bal0 - \sum_{j=0}^{i-1} f^j \cdot pmt$

The summation term of this equation can be simplified as follows.

$pmt_{fv}=\sum_{j=0}^{i-1} f^j \cdot pmt$

$f \cdot pmt_{fv} = \sum_{j=1}^{i} f^j \cdot pmt$

$(1-f) \cdot pmt_{fv} = pmt-pmt \cdot f^i$

Noting that $1-f=-r$

$pmt_{fv}=\frac{pmt \cdot f^i - pmt}{r}$

Substituting into the original equation, we get

$bal_i = bal0 \cdot f^i -\frac{pmt \cdot f^i - pmt}{r}$

Once we know the equation for $bal_i$ the remaining equations are easy.

$int_i = r* bal_{i-1}$

$prin_i = bal_i -bal_{i-1}$

### The loan amort function

We're now ready to right code.  Let's make a function `loanamort` that takes as input the four variables `r`, `n`, `pmt` and `bal0`.  It requires that at least three of these values are provided and if only three are provided it calculates the missing value.  It then calculates an amortization table for a loan with these parameters.

To make the function more practical, we add three additional optional variables that allow you to return zoo objects with actual dates.  `start` is a starting date of `bal0`.  `freq` is the frequency of payments, 12 for monthly.  If `freq` is  not equal to 1, then `r` will be converted to a rate for the frequency.  `apr` is a logical variable indicating if the given rate is stated as an "annual percentage rate".  If `TRUE`, then `r` is divided by `freq` before calculating.  Otherwise `r` will be converted to a rate for `freq` taking a root of `1+r` based on `freq`.  If `apr` is `TRUE` then `n` is multiplied by `freq` before calculations are done.  In other words, if you want to analyze a thirty year mortgage loan quoted in the conventional way, you set `apr` to `TRUE`, provide the annual percentage rate for `r` and provide 30 for `n`.

The function returns a list of values for the four loan parameters plus time series for the loan parameters.

Here is the code for the function.


```r
require(zoo)
```

```
## Warning: package 'zoo' was built under R version 3.5.1
```

```r
require(lubridate)
loanamort=function(r=NULL,bal0=NULL,pmt=NULL,n=NULL,apr=FALSE,start=NULL,freq=1) {
  ans=list()
  risnull=is.null(r)
  bal0isnull=is.null(bal0)
  pmtisnull=is.null(pmt)
  nisnull=is.null(n)
  if(1<sum(c(risnull,bal0isnull,pmtisnull,nisnull))) stop('loanamort error -- need to provide at least three parameters')
  n.f=n
  if(apr) n.f=n*freq
  if(!risnull) {
    if(apr) {
      r.f=r/freq
    } else {
      r.f=-1+(1+r)^(1/freq)
    }
  } else {
    cf=c(-bal0,rep(pmt,n.f))
    if(0<=sum(cf)) {
      rootrange=c(0,1.01) } else {
      rootrange=c(1,1000)
      }
    d=(uniroot(function(d) {sum(cf*d^(0:n.f))},rootrange))$root
    r.f=(1/d)-1
  }
  d=1/(1+r.f)
  f=1+r.f
  if(pmtisnull) pmt=(bal0*r.f)/(1-d^n.f)
  perp=pmt/r.f
  if(bal0isnull) bal0=perp-perp*(d^n)
  if(pmt<=(r.f*bal0)) stop(paste(pmt,r.f*bal0,'payment must be greater than interest'))
  if(nisnull) n.f= ceiling(log((1-(bal0*r.f)/pmt))/log(d))
  i=1:n.f
  bal=pmax(0,((bal0*f^i)-(((pmt*f^i)-pmt)/r.f)))
  balall=c(bal0,bal)
  int=balall[i]*r.f
  prin=-diff(balall)
  if(!is.null(start)) {
    bal=zooreg(bal,start=start+1/freq,freq=freq)
    int=zooreg(int,start=start+1/freq,freq=freq)
    prin=zooreg(prin,start=start+1/freq,freq=freq)
  }
  if(apr) {
    ans$r=r.f*freq
    ans$n=n.f/freq
  } else {
    ans$r=-1+((1+r.f)^freq)
    ans$n=n.f
  }
  ans$pmt=pmt
  ans$bal0=bal0
  ans$freq=freq
  ans$start=start
  ans$apr=apr
  ans$bal=bal
  ans$prin=prin
  ans$int=int
  return(ans)
}
```


### Illustrations of use of function

*In May of 2013, I bought a car borrowing 20,000 at 8% apr with monthly payments for 5 years.  What's the payment?*


```r
ans1=loanamort(bal0=20000,r=.08,n=5,apr=TRUE,freq=12)
ans1$pmt
```

```
## [1] 405.5279
```

*What is the total interest paid on theloan?*


```r
sum(ans1$int)
```

```
## [1] 4331.673
```

*What is the balance after a year?*


```r
ans1$bal[12]
```

```
## [1] 16611.2
```

*A year later, I get a raise and decide to increase my payment to $500 per month.  When will the loan be paid off?*


```r
start=as.yearmon("2014-5")
ans1a=loanamort(bal0=ans1$bal0,r=.08,pmt=500,apr=TRUE,freq=12)
start+ans1a$n
```

```
## [1] "Apr 2018"
```

*I know I borrowed $20,000 for my car and the original payments were $405.50 per month for 5 years, but lost the papers and am curious what my interest rate is.*


```r
ans1b=loanamort(bal0=20000,pmt=ans1$pmt,apr=TRUE,n=5,freq=12)
ans1b$r
```

```
## [1] 0.07989653
```

*I borrowed $100,000 to buy a house in March, 2013 at 5% interest with monthly payments for 30 years.  I am a calendar year tax payer and want to know my interest payments by year for the duration of the loan.*  In this case, the function returns the amortization vectors as zoo objects with the index as `yearmon` class.  You can `aggregate` by year in the way shown in the second line of code.


```r
ans2=loanamort(bal0=100000,r=.05,n=30,apr=TRUE,freq=12,start=as.yearmon("2013-3"))
aggregate(ans2$int,list(year(time(ans2$int))),sum)
```

```
##       2013       2014       2015       2016       2017       2018 
## 3731.80043 4910.23681 4831.87608 4749.50628 4662.92228 4571.90847 
##       2019       2020       2021       2022       2023       2024 
## 4476.23823 4375.67332 4269.96331 4158.84498 4042.04163 3919.26239 
##       2025       2026       2027       2028       2029       2030 
## 3790.20154 3654.53768 3511.93301 3362.03241 3204.46261 3038.83125 
##       2031       2032       2033       2034       2035       2036 
## 2864.72586 2681.71292 2489.33669 2287.11812 2074.55366 1851.11401 
##       2037       2038       2039       2040       2041       2042 
## 1616.24276 1369.35505 1109.83610  837.03966  550.28644  248.86238 
##       2043 
##   13.32792
```

*Show an amortization schedule for this loan by calendar year.*  In the below code, we aggregate by year, summing for the cash flow and taking the last item in each year for the balance sheet. We show a function `lastinvec` which grabs the last item in a vector. 


```r
lastinvec=function(x) tail(x,1)
Interest=aggregate(ans2$int,list(year(time(ans2$int))),sum)
Principal=aggregate(ans2$prin,list(year(time(ans2$prin))),sum)
Balance=aggregate(ans2$bal,list(year(time(ans2$bal))),lastinvec)
data.frame(Interest,Principal,Balance)
```

```
##        Interest Principal   Balance
## 2013 3731.80043  1099.594 98900.406
## 2014 4910.23681  1531.623 97368.783
## 2015 4831.87608  1609.983 95758.800
## 2016 4749.50628  1692.353 94066.447
## 2017 4662.92228  1778.937 92287.509
## 2018 4571.90847  1869.951 90417.558
## 2019 4476.23823  1965.621 88451.937
## 2020 4375.67332  2066.186 86385.751
## 2021 4269.96331  2171.896 84213.855
## 2022 4158.84498  2283.014 81930.840
## 2023 4042.04163  2399.818 79531.022
## 2024 3919.26239  2522.597 77008.425
## 2025 3790.20154  2651.658 74356.767
## 2026 3654.53768  2787.322 71569.446
## 2027 3511.93301  2929.926 68639.519
## 2028 3362.03241  3079.827 65559.692
## 2029 3204.46261  3237.397 62322.295
## 2030 3038.83125  3403.028 58919.267
## 2031 2864.72586  3577.134 55342.133
## 2032 2681.71292  3760.147 51581.987
## 2033 2489.33669  3952.523 47629.464
## 2034 2287.11812  4154.741 43474.723
## 2035 2074.55366  4367.306 39107.417
## 2036 1851.11401  4590.745 34516.671
## 2037 1616.24276  4825.617 29691.055
## 2038 1369.35505  5072.504 24618.550
## 2039 1109.83610  5332.023 19286.527
## 2040  837.03966  5604.820 13681.707
## 2041  550.28644  5891.573  7790.134
## 2042  248.86238  6192.997  1597.137
## 2043   13.32792  1597.137     0.000
```





