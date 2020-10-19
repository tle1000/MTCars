help(mtcars)
str(mtcars)
names(mtcars)
head(mtcars)
tail(mtcars)
summary(mtcars)
library(ggplot2)


ggplot(mtcars, aes(mpg)) + 
  geom_histogram(binwidth = 5) + xlab('Miles per Gallon') + ylab('Number of Cars') +
  ggtitle('Histogram of Cars by Mileage')
ggplot(mtcars, aes(cyl)) + 
  geom_histogram(binwidth = 1) + xlab('Cylinders') + ylab('Number of Cars') +
  ggtitle('Histogram of Cars by Cylinders')
ggplot(mtcars, aes(hp)) + 
  geom_histogram(binwidth = 30) + xlab('horsepower') + ylab('Number of Cars') +
  ggtitle('Histogram of Cars by Horsepower')
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) + geom_point() + 
  xlab("Weight") + ylab("Miles/gallon") + ggtitle("MPG vs Weight vs Displacement")

pairs (~mpg+disp+hp+drat+wt, data = mtcars, pch = 19, cex = 0.8, 
       lower.panel = NULL, main='Scatterplot Matrix ')

library(car)
scatterplotMatrix(~mpg+disp+hp+wt|cyl, data=mtcars,
                  main="Three Cylinder Grouped scatterplot matrix")

mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <-c("AT", "MT")
mtcars$vs <- as.factor(mtcars$vs)
levels(mtcars$vs) <- c("V-shaped", "Straight")

boxplot(mpg~cyl, data = mtcars, main = "Mileage Boxplot",
        xlab = "Number of Cylinders", ylab = "Mpg")
boxplot(hp ~ cyl, data = mtcars, main = "Horsepower vs Cylinders Boxplot",
        xlab = "cylinders", ylab = "horsepower")
boxplot(hp ~ am, data = mtcars, main = "Horsepower vs Transmission Boxplot",
        xlab = "Transmission", ylab = "Horsepower")
boxplot(hp ~ vs, data = mtcars, main = "Horsepower vs Cylinders Boxplot",
        xlab = "Engine", ylab = "Horse Power")

boxplot(mpg~am, data = mtcars, main = "Mileage BoxPlot",
        xlab = "Transmiss", ylab = "Mpg")


t.test(mpg ~ am, data =mtcars)


linear <- lm(mpg~wt, data=mtcars)
summary(linear)

mlinear <- lm(mpg~ am + cyl + wt + hp, data = mtcars)
summary(mlinear)

plot (mlinear)

anova(linear, mlinear)