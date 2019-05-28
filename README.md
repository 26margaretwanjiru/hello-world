# hello-world
Just another repository

Trying to figure out how Github works.
Finally, i feel it in my bones that i am getting it.Wish me the best!


library(ggplot2)
library(gganimate)

ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

https://github.com/26margaretwanjiru/hello-world/blob/master/Rlogobanana.gif
