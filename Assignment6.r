#(1) The joint probability density of two random variables X and Y is
#f(x, y) =
 # 
#
#
#2(2x + 3y)/5; 0 ≤ x, y ≤ 1
#0; elsewhere
#Then write a R-code to
#(i) check that it is a joint density function or not? (Use integral2())
#(ii) find marginal distribution g(x) at x = 1.
#(iii) find the marginal distribution h(y) at y = 0.
#(iv) find the expected value of g(x, y) = xy.


#q1
f2<-function(x,y){
  return (2*(2*x+3*y)/5)
}

#install.packages("pracma")
#library("pracma")

integral2(f2,xmin=0,xmax=1,ymin=0,ymax=1)

fy=function(y){f2(1,y)}
print(yx)

gx1=integrate(fy,0,1)
gx1

fx=function(x){f2(x,0)}

hy1=integrate(fx,0,1)
hy1

f3=function(xx,yy){return (xx*yy*f2(xx,yy))}

exy2=integral2(f3,xmin=0,xmax=1,ymin=0,ymax=1)
exy2



#(2) The joint probability mass function of two random variables X and Y is
#f(x, y) = {(x + y)/30; x = 0, 1, 2, 3; y = 0, 1, 2}
#Then write a R-code to
#(i) display the joint mass function in rectangular (matrix) form.
#(ii) check that it is joint mass function or not? (use: Sum())
#(iii) find the marginal distribution g(x) for x = 0, 1, 2, 3. (Use:apply())
#(iv) find the marginal distribution h(y) for y = 0, 1, 2. (Use:apply())
#(v) find the conditional probability at x = 0 given y = 1.
#(vi) find E(x), E(y), E(xy), V ar(x), V ar(y), Cov(x, y) and its correlation coefficient.


f<-function(x,y){
  return ((x+y)/30)
}

f1<-function(x,y){
  return (x*y*(x+y)/30)
}

m<-matrix(c(f(0,0:2),f(1,0:2),f(2,0:2),f(3,0:2)),nrow=4,ncol=3,byrow = T)
print(m)

if(sum(m)==1){
  print("joint pmf")
}else{
  print("not joint pmf")
}

print(sum(m))

gx=apply(m,1,sum)
hy=apply(m,2,sum)

print((m[1,2]/hy[2]))

ex=sum(0:3*gx)
ex

ey=sum(0:2*fy)
ey

x=c(0:3)
y=c(0:2)
m1<-matrix(c(f1(0,0:2),f1(1,0:2),f1(2,0:2),f1(3,0:2)),nrow=4,ncol=3,byrow = T)
m1
exy=sum(m1)
exy

vx=sum(x*x*gx)-(ex)*(ex)
vx
vy=sum(y*y*hy)-ey*ey
vy

vxy=exy-ex*ey
vxy

corr=vxy/((vx*vy)^0.5)
corr

