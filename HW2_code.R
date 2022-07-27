library(ISLR)
library(class)
## Problem 1                               


hw2.p1=rbind( c(1,2,-1),
       c(1,0,3),
      c(0,3,0),
      c(0,2,-1),
      c(2,0,-1),
      c(1,4,1)
)

dist(hw2.p1)

 ## problem 3
library('mlbench')
data(Ionosphere)

summary(Ionosphere)
problem3.data= Ionosphere[,-2]

x.train=problem3.data[,-34]
y.train=problem3.data[,34]
y.train=factor(y.train)
y.train=unclass(y.train)

knn.prep= knn(train= x.train, test= x.train, cl= y.train, k=1)

table(knn.prep, y.train)

set.seed(4323)
problem3.data= Ionosphere[,-2]
train=sample(1:351,246)
x.train=problem3.data[train,-34]
y.train=problem3.data[train,34]
x.test=problem3.data[-train,-34]
y.test= problem3.data[-train,34]
knn.prep3=knn(train=x.train,test=x.test, cl= y.train, k=3)
knn.prep5=knn(train=x.train,test=x.test, cl= y.train, k=5)
knn.prep7=knn(train=x.train,test=x.test, cl= y.train, k=7)
mean(knn.prep3!= y.test)
mean(knn.prep5!= y.test)
mean(knn.prep7!= y.test)

boxplot(Ionosphere)
                                  ## problem 4
data(Auto)
Auto$mpg01= ifelse(Auto$mpg>median(Auto$mpg),1,0)
problem4.data=Auto[,-c(1,9)]


#b
par(mfrow=c(3,3))
boxplot(Auto$cylinders~Auto$mpg01)
boxplot(Auto$displacement~Auto$mpg01)
boxplot(Auto$horsepower~Auto$mpg01)
boxplot(Auto$weight~Auto$mpg01)
boxplot(Auto$acceleration~Auto$mpg01)
boxplot(Auto$year~Auto$mpg01)
boxplot(Auto$origin~Auto$mpg01)

#c
set.seed(1)
train2=sample(1:392,274)
x.train4= problem4.data[train2,-8]
y.train4=(problem4.data[train2,8])
x.test4=problem4.data[-train2,-8]
y.test4=(problem4.data[-train2,8])

#d

error=c(1:50)
for (x in seq(1,by=2, len=50)) {
  knn.prep=knn(train=x.train4, test = x.test4, cl=y.train4,k = x)
  error[(x-1)/2+1]=mean(knn.prep!=y.test4)
}
plot(seq(1,by=2, len=50), error)

#e

x.train4e= scale(problem4.data[train2, -8])
x.test4e= scale(problem4.data[-train2, -8],
                center = attr(x.train4e, 'scaled:center'), 
                scale = attr(x.train4e, 'scaled:scale'))

error4e=c(1:50)
for (x in seq(1,by=2, len=50)) {
  knn.prep=knn(train=x.train4e, test = x.test4e, cl=y.train4,k = x)
  error4e[(x-1)/2+1]=mean(knn.prep!=y.test4)
}
plot(seq(1,by=2, len=50), error4e)

                                    ## problem 5
set.seed(1)
problem5.data=problem4.data
mean(knn.cv(train= problem5.data[,-8], cl = problem5.data[,8], k=1)!= problem5.data[,8])
mean(knn.cv(train= problem5.data[,-8], cl = problem5.data[,8], k=3)!= problem5.data[,8])
mean(knn.cv(train= problem5.data[,-8], cl = problem5.data[,8], k=10)!= problem5.data[,8])

#5c

set.seed(1)
mean(knn.cv(train= scale(problem5.data[,-8]), cl = problem5.data[,8], k=1)!= problem5.data[,8])
mean(knn.cv(train= scale(problem5.data[,-8]), cl = problem5.data[,8], k=3)!= problem5.data[,8])
mean(knn.cv(train= scale(problem5.data[,-8]), cl = problem5.data[,8], k=10)!= problem5.data[,8])
     
     
     
     