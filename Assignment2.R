
#1) (a) Suppose there is a chest of coins with 20 gold, 30 silver and 50 bronze coins. You
#randomly draw 10 coins from this chest. Write an R code which will give us the sample
#space for this experiment. (use of sample(): an in-built function in R)

chest<- c(rep("gold",20),rep("silver",30),rep("brown",50))
sample(chest,10)

sample(c("success","failure"),10,replace=TRUE,prob=c(0.9,0.1))


#(2) A room has n people, and each has an equal chance of being born on any of the 365
#days of the year. (For simplicity, we’ll ignore leap years). What is the probability that two
#people in the room have the same birthday? (a) Use an R simulation to estimate this for
#various n.

n=20
probability=1- ((choose(365,n)*factorial(n))/(365^n))
print(probability)

n=28
probability=1-((choose(365,n)*factorial(n))/(365^n))
print(probability)

#(b) Find the smallest value of n for which the probability of a match is greater than .5.
n=23
probability=1-((choose(365,n)*factorial(n))/(365^n))
print(probability)


#(3) Write an R function for computing conditional probability. Call this function to do the
#following problem: suppose the probability of the weather being cloudy is 40%. Also
#suppose the probability of rain on a given day is 20% and that the probability of clouds
#on a rainy day is 85%. If it’s cloudy outside on a given day, what is the probability that it
#will rain that day?

bayesian<- function(Pa,Pb,Pba){
  Pab= (Pa*Pba)/Pb
  return(Pab)
}
Pa= 0.2
Pb=0.4
Pba=0.85
Pab= bayesian(Pa,Pb,Pba)
print(Pab)

#(4) The iris dataset is a built-in dataset in R that contains measurements on 4 different
#attributes (in centimeters) for 150 flowers from 3 different species. Load this dataset
#and do the following:
# (a) Print first few rows of this dataset.
#(b) Find the structure of this dataset.
#(c) Find the range of the data regarding the sepal length of flowers.
#(d) Find the mean of the sepal length.
#(e) Find the median of the sepal length.
#(f) Find the first and the third quartiles and hence the interquartile range.
#(g) Find the standard deviation and variance.
#(h) Try doing the above exercises for sepal.width, petal.length and petal.width.
#(i) Use the built-in function summary on the dataset Iris.

data<-iris
head(data)
str(data)
rng <-range(data$Sepal.Length)
print(rng)
q1<- quantile(data$Sepal.Length,0.25)
q3<- quantile(data$Sepal.Length,0.75)
iqr=q3-q1
print(iqr)
lapply(data[,1:4],sd)
summary(data)




#R does not have a standard in-built function to calculate mode. So we create a user
#function to calculate mode of a data set in R. This function takes the vector as input
#and gives the mode value as output.

#5
#no inbuilt function for mode
getmode<- function(v){
  uniquev<-unique(v)
  uniquev[which.max(tabulate(match(v,uniquev)))]
}
v<-c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
v<-c("o","it","the","it","it")
result<-getmode(v)
print(result)


