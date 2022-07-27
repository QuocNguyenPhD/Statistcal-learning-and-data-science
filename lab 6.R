data(iris)
head(iris)


plot(Sepal.Length ~ Sepal.Width, data=iris,
     col=c("red","blue","green")[as.integer(Species)],
     pch=c(1,2,5)[as.integer(Species)])
legend(x="bottomright",
       legend=c("setosa","versicolor","virginica"),
       col=c("red","blue","green"),
       pch=c(1,2,5))

plot(Petal.Width ~ Petal.Length, data=iris,
     col=c("red","blue","green")[as.integer(Species)],
     pch=c(1,2,5)[as.integer(Species)])
legend(x="bottomright",
       legend=c("setosa","versicolor","virginica"),
       col=c("red","blue","green"),
       pch=c(1,2,5))

library(rgl)
plot3d(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length,
       type = "s",
       size = 1,
       xlab = "Sepal.Length",
       ylab = "Sepal.Width",
       zlab = "Petal.Length",
       col = c("red","blue","green")[as.integer(iris$Species)])
legend3d("top",
         pch=1,
         cex = 0.8,
         2
         horiz = TRUE,
         legend = levels(iris$Species),
         col =  c("red","blue","green"))



 library(GGally)
 ggpairs(iris[,1:4])
 
 library(stats)
 pc.out <- prcomp(iris[,-5], scale = T)
 pc.out 
 summary(pc.out)
sum(pc.out$sdev^2) 


ggpairs(data.frame(pc.out$x
                   ))


library(ggfortify)
library(ggplot2)
autoplot(pc.out, data = iris, scale=1, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4)
biplot(pc.out)
