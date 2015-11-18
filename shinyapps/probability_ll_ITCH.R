df <- data.frame(t1 = seq(5, 100, by = 10))


library(dplyr)

p_ll <- function(beta_intercept, beta_xa, beta_xr, beta_ta, beta_tr, x1, x2, t1, t2) {
  x_rat <- (x2 - x1)/ ((x2 + x1)/2)
  t_rat <- (t2 - t1)/ ((t2 + t1)/2)
  reg <- beta_intercept + beta_xa*(x2 - x1) + beta_xr*(x_rat) + 
    beta_ta*(t2 - t1) + beta_tr*(t_rat)
  plogis(reg)
} 

p_ll(0, .1, .1, -.1, -.1, 10, 20, 10, 11)

df <- df %>% 
  mutate(t2 = t1 + 1,
         x1 = 10, 
         x2 = 20,
         beta_intercept = 0, beta_xa = .1, beta_xr  = .1, 
         beta_ta  = -.1, beta_tr = -.1,
         p_ll = p_ll(beta_intercept, beta_xa, beta_xr, beta_ta, beta_tr, x1, x2, t1, t2))

df2 <-  data.frame(t1 = seq(1, 100, by = 3))


df2 <- df2 %>% 
  mutate(t2 = t1 + 5,
         x1 = 10, 
         x2 = 20,
         beta_intercept = 0, beta_xa = .1, beta_xr  = .1, 
         beta_ta  = -.1, beta_tr = -.1,
         p_ll = p_ll(beta_intercept, beta_xa, beta_xr, beta_ta, beta_tr, x1, x2, t1, t2))

p_ll(0, .1, .1, -.1, -.1, 10, 20, 5, 6)/p_ll(0, .1, .1, -.1, -.1, 10, 20, 10, 11)
p_ll(0, .1, .1, -.1, -.1, 10, 20, 5, 6)-p_ll(0, .1, .1, -.1, -.1, 10, 20, 10, 11)

p_ll(0, .1, .1, -.1, -.1, 10, 20, 5, 15)/p_ll(0, .1, .1, -.1, -.1, 10, 20, 10, 20)
p_ll(0, .1, .1, -.1, -.1, 10, 20, 5, 15)-p_ll(0, .1, .1, -.1, -.1, 10, 20, 10, 20)

