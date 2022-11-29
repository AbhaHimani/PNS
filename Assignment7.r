#(1) Use the rt(n, df) function in r to investigate the t-distribution for n = 100 and df = n − 1 and plot
#the histogram for the same

n=100
df=n-1
sample=rt(n,df)
hist(sample)


#(2) Use the rchisq(n, df ) function in r to investigate the chi-square distribution with n = 100
#and
#df = 2, 10, 25.

n=100
df=c(2,10,25)
rchisq(n,df[1])
rchisq(n,df[2])
rchisq(n,df[3])

#(3) Generate a vector of 100 values between -6 and 6. Use the dt() function in r to find the values of
#a t-distribution given a random variable x and degrees of freedom 1,4,10,30. Using these values plot the
#density function for students t-distribution with degrees of freedom 30. Also shows a comparison of
#probability density functions having different degrees of freedom (1,4,10,30).

x<-seq(-6,6,lenght=100)
df=c(1,4,10,30)
colour=c("black","yellow","green","red")
dt(x,df[1])
dt(x,df[2])
dt(x,df[3])
dt(x,df[4])
plot(x,dt(x,df[4]),type="1",xlab = "t-value",y-lab="Density",col=colour[4])
for(i in 1:3){
  lines(x,dt(x,df[i]),type = "1",col=colour[i])
}

#(4) Write a r-code
#(i) To find the 95
#th percentile of the F -distribution with (10, 20) degrees of freedom.
qf(0.95,df1=10,df2=20)
#(ii) To calculate the area under the curve for the interval [0, 1.5] and the interval [1.5, +∞) of a F -curve
#with v1 = 10 and v2 = 20 (USE pf ()).

pf(1.5,df1=10,df=20)
pf(1.5,df1=10,df2=20,lower.tail = FALSE)

#(iii) To calculate the quantile for a given area (= probability) under the curve for a F -curve with v1 =
# 10 and v2 = 20 that corresponds to q = 0.25, 0.5, 0.75 and 0.999. (use the qf ())

qf(0.25,df1=10,df2=20)
qf(0.5,df1=10,df2=20)
qf(0.75,df1=10,df2=20)
qf(0.999,df1=10,df2=20)

#(iv) To generate 1000 random values from the F -distribution with v1 = 10 and v2 = 20 (use
#rf ())and plot a histogram.

sample= rt(1000,df1=10,df2=20)
hist(sample)
