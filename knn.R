library(tidyverse)
library(ggplot2)
library(ggimage)
library(class)

n = 10
set.seed(29827)
even_logical = c(rep(FALSE, 3*n/2), rep(TRUE, 3*n/2))
permuted_logical = sample(even_logical, size = length(even_logical), replace = FALSE)
data = tibble(
  musical_ability = c(rnorm(n, -1, 0.3), rnorm(n, 1, 0.3), rnorm(n, 0, 0.3)),
  green_hair = c(rnorm(n, 1, 0.3), rnorm(n, 1, 0.3), rnorm(n, 0, 0.3)),
  flavour = c(rep("Harris", n), rep("Flint", n), rep("Richards", n)),
  test = permuted_logical
  )

data_test = data %>%
  filter(test) %>%
  select(musical_ability, green_hair)
data_train = data %>%
  filter(!test)%>%
  select(musical_ability, y)
keithnn =  knn(data_train,data_test,cl=data$flavour[!data$test],k=3)
images = tibble(names = c("Harris", "Flint", "Richards"),
                imgs = c("img/harris.png","img/flint.jpg","img/richards.jpeg"))

data_predictions = data %>%
  filter(test) %>%
  mutate(prediction = keithnn)
data_predictions = left_join(data_predictions, images, by = c("flavour" = "names"))

data_predictions %>%
  ggplot(aes(x=musical_ability, y=green_hair)) +
  geom_image(aes(image = imgs), size = 0.1)
