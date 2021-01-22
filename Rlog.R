library(tidyverse)
library(caret)
library(MASS)

Obj3=read.csv('csipkes3.csv', header=TRUE, sep = ",", dec = ".")
str(obj3)
obj3$purpose <- as.factor(obj3$purpose)
set.seed(123)
training.samples <- obj3$purpose %>%
+     createDataPartition(p = 0.8, list = FALSE)
train.data <- obj3[training.samples, ]
test.data <- obj3[-training.samples, ]
preproc.param <- train.data %>% 
+     preProcess(method = c("center", "scale"))
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

model <- lda(purpose~., data = train.transformed)

plot(model)

Normalizing test:

library(tidyverse)
library(ggpubr)
library(rstatix)

obj4=read.csv('csipkes3.csv', header=TRUE, sep = ",", dec = ".")
obj4$purpose <- as.factor(obj4$purpose)
ggdensity(obj4$P, fill = "lightgray")

obj_sample=obj4[obj4$purpose == 'sample', ]
obj_control=obj4[obj4$purpose == 'control', ]



obj4 %>% shapiro_test(P, Ca_.mg.kg., K_.mg.kg., Mg_.mg.kg., Fe_.mg.kg., Mn_.mg.kg., C_., N_.)
# A tibble: 8 x 3
  variable   statistic        p
  <chr>          <dbl>    <dbl>
1 C_.            0.864 1.03e-10
2 Ca_.mg.kg.     0.935 1.46e- 6
3 Fe_.mg.kg.     0.733 1.39e-15
4 K_.mg.kg.      0.949 1.80e- 5
5 Mg_.mg.kg.     0.852 2.63e-11
6 Mn_.mg.kg.     0.945 8.10e- 6
7 N_.            0.968 1.09e- 3
8 P              0.961 2.01e- 4


From the output above, the p-value < 0.05 implying that the distribution of the data are significantly different from normal distribution. In other words, we cannot assume the normality.

library(stats)

