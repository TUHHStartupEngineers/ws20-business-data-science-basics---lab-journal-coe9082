round(3.1415)

sqrt(16)

mean(1:6)

sample(die, size = 2, replace = TRUE)

sum(1:6)

roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

roll2 <- function(faces = 1:6, number_of_dice = 2) {
  dice <- sample(x = faces, size = number_of_dice, replace = TRUE)
  sum(dice)
}

roll2(faces = 1:4, number_of_dice = 4)

calc_EOQ <- function(D = 1000) {
  K <- 5
  h <- 0.25
  Q <- sqrt(2*D*K/h)
  Q
}

calc_EOQ(D = 4000)

#change the probability of rolling a 6 to 50%
roll3 <- function(faces = 1:6, number_of_dice = 1) {
  dice <- sample(x = faces, size = number_of_dice, replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
  sum(dice)
}

# You can run the function 100 times, store the results and plot a histogram to varify your function
results <- replicate(n = 100, expr = roll3(), simplify=TRUE)
hist(results)