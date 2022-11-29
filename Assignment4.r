#Q1 The probability distribution of X, the number of imperfections per 10 meters of a
#synthetic fabric in continuous rolls of uniform width, is given as
#x    0 1 2 3 4
#p(x) 0.41 0.37 0.16 0.05 0.01
#Find the average number of imperfections per 10 meters of this fabric.
#(Try functions sum( ), weighted.mean( ), c(a %*% b) to find expected value/mean.

x &lt;- c(0,1,2,3,4)
px &lt;- c(0.41,0.37,0.16,0.05,0.01)
sum(x*px)
weighted.mean(x,px)
c(x%*%px)

#Q2 The time T, in days, required for the completion of a contracted project is a random
#variable with probability density function f(t) = 0.1 e
#(-0.1t) for t > 0 and 0 otherwise. Find
#the expected value of T.
#Use function integrate( ) to find the expected value of continuous random variable T.

func &lt;-function(t){t*0.1*exp(-0.1*t)}
mean &lt;- integrate(func,lower = 0,upper = Inf)
print(mean$value)

#Q3. A bookstore purchases three copies of a book at $6.00 each and sells them for $12.00
#each. Unsold copies are returned for $2.00 each. Let X = {number of copies sold} and
#Y = {net revenue}. If the probability mass function of X is
#x 0 1 2 3
#p(x) 0.1 0.2 0.2 0.5
#Find the expected value of Y.

x &lt;- c(0,1,2,3)
px &lt;- c(0.1,0.2,0.2,0.5)
mean &lt;- sum((10*x-12)*px)
print(mean)


#4 4. Find the first and second moments about the origin of the random variable X with
#probability density function f(x) = 0.5e-|x|, 1 < x < 10 and 0 otherwise. Further use the
#results to find Mean and Variance.
#(kth moment = E(Xk), Mean = first moment and Variance = second moment – Mean2
func1 &lt;- function(x){x*0.5*exp(-1*abs(x))}

func2 &lt;- function(x){x*x*0.5*exp(-1*abs(x))}
func3 &lt;- function(M1,M2){return(M2-M1*M1)}
moment1 &lt;- integrate(func1,lower=1,upper=10)
moment2 &lt;- integrate(func2,lower=1,upper=10)
variance &lt;- func3(moment1$value,moment2$value)
print(moment1$value)
print(variance)


#5Let X be a geometric random variable with probability distribution

#f(x) =
#  3 1 x−1 ( ) 4 4
#, x = 1,2,3, ...

#Write a function to find the probability distribution of the random variable Y = X2 and
#find probability of Y for X = 3. Further, use it to find the expected value and variance of
#Y for X = 1,2,3,4,5.

func &lt;- function(x){return(3/4*(1/4)^(x-1))}
X &lt;- c(1,2,3,4,5)
e1 &lt;- sum(X^2*func(X))
e2 &lt;- sum(X^4*func(X))
variance &lt;- e2-e1^2
mean &lt;- e1
print(func(9))
print(mean)
print(variance)
