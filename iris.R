data(iris3)
ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
ir.species <- factor(c(rep("s", 50), rep("c", 50), rep("v", 50)))

## Multidimensional scaling
ir.scal <- cmdscale(dist(ir), k = 2, eig = T)
ir.scal$points[, 2] <- -ir.scal$points[, 2]

eqscplot(ir.scal$points, type ="n")
text(ir.scal$points, labels = as.character(ir.species[-143]),
     col = as.numeric(ir.species))
