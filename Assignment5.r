#1. Consider that X is the time (in minutes) that a person has to wait in order to take a flight.
#If each flight takes off each hour X ~ U(0, 60). Find the probability that
#(a) waiting time is more than 45 minutes, and


a=1-punif(45,min=0,max=60)
print(a)

#(b) waiting time lies between 20 and 30 minutes.
b=punif(30,min=0,max=60)-punif(20,min=0,max=60)
print(b)



#2. The time (in hours) required to repair a machine is an exponential
#distributed random variable with parameter λ = 1/2.
#(a) Find the value of density function at x = 3.
a= dexp(3,rate=1/2)
print(a)

#(b) Plot the graph of exponential probability distribution for 0 ≤ x ≤ 5.
x<-seq(0,5,by=0.02)
fx<-dexp(x,rate=1/2)
plot(x,fx)

#(c) Find the probability that a repair time takes at most 3 hours.
c=pexp(3,rate=1/2)
print(c)

#(d) Plot the graph of cumulative exponential probabilities for 0 ≤ x ≤ 5.
x<-seq(0,5,by=0.02)
fx<-pexp(x,rate=1/2)
plot(x,fx)

#(e) Simulate 1000 exponential distributed random numbers with λ = 1⁄2 and plot the simulated data.
n<-1000
x_sim<-rexp(n,rate=1/2)
plot(density(x_sim))


#3. The lifetime of certain equipment is described by a random variable X
#that follows Gamma distribution with parameters α = 2 and β = 1/3.
#(a) Find the probability that the lifetime of equipment is at least 1 unit of
#time.
alpha<-2
beta<-1/3
ans= pgamma(1,shape=alpha,scale=beta,lower.tail = FALSE)
print(ans)
#(b) What is the value of c, if P(X ≤ c) ≥ 0.70? (Hint: try quantile function qgamma())
alpha<-2
beta<-1/3
anqgamma(0.70,shape=alpha,scale=beta)
print(ans)
