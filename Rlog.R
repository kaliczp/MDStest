library(MASS)

obj4=read.csv('csipkes3.csv', header=TRUE, sep = ",", dec = ".")
obj4$purpose <- as.factor(obj4$purpose)
levels(obj4$purpose) <- c("c", "s")

## Multidimensional scaling
o.dist <- dist(obj4[,-19])
o.scal <- cmdscale(o.dist, k = 2)
plot(o.scal, type = "n")
text(o.scal, lab = as.character(obj4[,19]))
