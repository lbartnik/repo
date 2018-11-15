# ---- iris-model ----
library(dplyr)

x <- iris

x <- x %>%
  mutate(Sepal.Area = Sepal.Length * Sepal.Width,
         Petal.Area = Petal.Length * Petal.Width)

virginica <- x %>%
  mutate(Virginica = Species == "virginica")

m <- lm(Virginica ~ Sepal.Area + Petal.Area, virginica)

virginica$predict <- predict(m, virginica)

with(virginica, plot(predict, Virginica))

with(virginica, plot(1-cumsum(!Virginica[order(predict)])/100,
                     1-cumsum(Virginica[order(predict)])/50,
                     xlab = "FPR", ylab = "TPR"))
