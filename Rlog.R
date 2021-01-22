library(MASS)

obj4=read.csv('csipkes3.csv', header=TRUE, sep = ",", dec = ".")
obj4$purpose <- as.factor(obj4$purpose)
levels(obj4$purpose) <- c("c", "s")

## Multidimensional scaling
o.dist <- dist(obj4[,-19])
o.scal <- cmdscale(o.dist, k = 2)
plot(o.scal, type = "n")
text(o.scal, lab = as.character(obj4[,19]))
axis(1, at=500, tck=1)

## Observation nr
plot(o.scal, type = "n")
text(o.scal, lab = rownames(obj4),cex=0.7)

## Dominant variables search based on separation line
grp <- ifelse(o.scal[,1] < 500, "g1", "g2")
plot(o.scal, type = "n")
text(o.scal, lab = grp)

## Visualisation of c–s
par(mfcol=c(6,3), mar=c(0,0,0,0))
for(tti in 1:18) boxplot(obj4[, tti] ~ obj4[,19], main = colnames(obj4)[tti])

## Visualsation of g1–g2
for(tti in 1:18) boxplot(obj4[, tti] ~ grp, main = colnames(obj4)[tti])
par(mfcol=c(1,1), mar=c(5.1,4.1,4.1,2.1))

tti <- 3 # Ca
tti <- 7 # Mn
tti <- 8 # C_
tti <- 12 # Ca_sza
tti <- 16 # Mn_sza
tti <- 17 # C_sza

boxplot(obj4[, tti] ~ grp, main = colnames(obj4)[tti])
