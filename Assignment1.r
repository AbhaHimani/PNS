                         Assignment 1
                         
n= as.integer(readline(prompt="Enter the number"))
fact=1
if(n<0){
  print("error")
}else{
  for(i in 1:n){
    fact=fact*i
  }
  print(fact)
}

n=as.integer(readline(prompt="Enter the number"))
a=0
b=1
print(a)
print(b)
for(i in 2:n){
  c=a+b
  print(c)
  a=b
  b=c
}

add<- function(x,y){
  return (x+y)
}
subtract<- function(x,y){
  return(x-y)
}
multiply<- function(x,y){
  return (x*y)
}
divide<- function(x,y){
  return (x/y)
}

choice= as.integer(readline(prompt="Enter the number"))
a= as.integer(readline(prompt("Enter number 1")))
b= as.integer(readline(prompt("Enter number 2")))
result = switch(choice,"1"=add(a,b),"2"=subtract(a,b),"3"=multiply(a,b),"4"=divide(a,b))
print(paste)
