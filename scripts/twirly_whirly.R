# load libraries---------------
library(ggplot2)
library(here)


# function without randomness-------------
twirly_whirly <- function(n, x_1, y_1, a, b) {

# create empty vectors
x <- vector(mode = "double", length = n)
y <- vector(mode = "double", length = n)

# starting values
x[1] <- x_1
y[1] <- y_1

# populate vectors
for (i in 2:n) {
  
  x[i] <- a*cos(x[i-1]) + b*sin(y[i-1])
  y[i] <- a*sin(x[i-1]) - b*cos(y[i-1])
  
}

# create data frame
data.frame(x = x, y = y)

}

# create a few twirly whirlies
df1 <- twirly_whirly(n = 100000, x_1 = 0.1, y_1 = 0.1, a = 1.5, b = 2.2)
df2 <- twirly_whirly(n = 100000, x_1 = 0.1, y_1 = 0.1, a = 2, b = 0.5)
df3 <- twirly_whirly(n = 100000, x_1 = 0.1, y_1 = 0.1, a = 0.7, b = 5)
df4 <- twirly_whirly(n = 100000, x_1 = 0.1, y_1 = 0.1, a = 1.2, b = -1.2)

# plot
ggplot(df1) + 
  geom_point(aes(x, y), 
             shape = 16, alpha = 0.1, size = 0.05) + 
  theme_void() + 
  ggsave(here("plots", "tw_1.png"), height = 3, width = 3, units = "in")

ggplot(df2) + 
  geom_point(aes(x, y), 
             shape = 16, alpha = 0.05, size = 0.05) + 
  theme_void() +
  ggsave(here("plots", "tw_2.png"), height = 3, width = 3, units = "in")

ggplot(df3) + 
  geom_point(aes(x, y), 
             shape = 16, alpha = 0.1, size = 0.05) + 
  theme_void() +
  ggsave(here("plots", "tw_3.png"), height = 3, width = 3, units = "in")

ggplot(df4) + 
  geom_point(aes(x, y), 
             shape = 16, alpha = 0.05, size = 0.05) + 
  theme_void() +
  ggsave(here("plots", "tw_4.png"), height = 3, width = 3, units = "in")


# function with randomness-------------
twirly_whirly_runif <- function(n, x_1, y_1, a, b) {
  
  # create empty vectors
  x <- vector(mode = "double", length = n)
  y <- vector(mode = "double", length = n)
  
  # starting values
  x[1] <- x_1
  y[1] <- y_1
  
  # populate vectors
  for (i in 2:n) {
    
    set.seed(25)
    x[i] <- a*cos(x[i-1]) + b*sin(y[i-1])*runif(1)
    y[i] <- a*sin(x[i-1]) - b*cos(y[i-1])*runif(1) 
    
  }
  
  # create data frame
  data.frame(x = x, y = y)
  
}

# create and plot twirly whirlies with random element
df_runif <- twirly_whirly_runif(n = 100000, x_1 = 0.1, y_1 = 0.1, a = 1.6, b = pi)

ggplot(df_runif) + 
  geom_point(aes(x, y), 
             shape = 16, alpha = 0.2, size = 0.05) + 
  theme_void() +
  ggsave(here("plots", "tw_runif.png"), height = 3, width = 3, units = "in")
