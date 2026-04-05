library(openintro)
library(tidyverse)

births14 |>
  ggplot(aes(x = weight)) +
  geom_histogram() + 
  theme_minimal() + 
  labs(x = "Birth weight (lbs)") + 
  theme(text = element_text(size = 25))


births14 |>
  ggplot(aes(x = weight)) +
  geom_histogram() + 
  theme_minimal() + 
  geom_vline(xintercept = mean(births14$weight), color = "red", size = 2) + 
  labs(x = "Birth weight (lbs)") + 
  theme(text = element_text(size = 25))



births14 |>
  ggplot(aes(x = weeks, y = weight)) + 
  geom_point() +
  geom_smooth(method = "lm", size = 1.5) + 
  theme_minimal() + 
  labs(
    x = "Length of pregnancy (weeks)",
    y = "Birth weight (lbs)"
  ) + 
  theme(text = element_text(size = 25))