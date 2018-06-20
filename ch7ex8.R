library(ISLR)
library(boot)
library(gam)

ggplot(data = Auto) +
  geom_jitter(mapping = aes(x = weight, y = mpg))

ggplot(data = Auto) +
  geom_jitter(mapping = aes(x = displacement, y = mpg, color = weight))

