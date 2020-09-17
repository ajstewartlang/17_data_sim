library(tidyverse) # Usual tidyverse packages

# We can use the rnorm() function to sample x times from a normal distribution where we specify
# the mean and the sd.  

rnorm(5, 0, 1)
rnorm(5, 0, 1)

set.seed(1234)
rnorm(5, 0, 1)
set.seed(1234)
rnorm(5, 0, 1)

# As we haven't used set.seed() the random samples are different from each other. Below we use
# set.seed() to ensure we can reproduce the output.

# Here we sample 50 times from a normal distribution with a mean of zero 
# and sd of 1.  We use the set.seed() function so we can reproduce this random sample.

set.seed(1234)
my_data <- rnorm(50, 0, 1)
hist(my_data)

set.seed(1234)
my_data <- rnorm(5000000, 0, 1)
hist(my_data, breaks = 1000)

my_data %>%
  as_tibble() %>%
  filter(value > 1.96 | value < -1.96) %>%
  count() 

my_data %>%
  as_tibble() %>%
  transmute(z_score = value) %>%
  ggplot(aes(x = z_score)) +
  geom_histogram(bins = 1000) +
  geom_vline(xintercept = 1.96, colour = "red") +
  geom_vline(xintercept = -1.96, colour = "red") +
  ylab("Density") + 
  xlab("Z-Score") + 
  scale_y_discrete(labels = NULL)

# Plotting two distributions on the same graph

set.seed(1234)
condition1 <- rnorm(1000000, 0, 1)
condition2 <- rnorm(1000000, 1.96, 1)
my_data <- as_tibble(cbind(condition1, condition2))

ggplot(my_data) + 
  geom_density(aes(x = condition1, y = ..density.., colour = "red")) +
  geom_density(aes(x = condition2, y = ..density.., colour = "green")) + 
  xlab("Data") + 
  guides(colour = FALSE)

# First let's simulate data from a one factor between participants experiment.
# Each of the 24 participant will have 1 measures - one for each level of the factor.

# First let's create a vector for our participants.  It will range from 1 to 24

participant <- rep(1:24)

# We can check we have what we want by typing the variable name

participant

# Now we need to creaee the conditions - Condition 1 we will label 'fast' and Condition 2 we will label 'slow'
# We use the c() function to combine the arguments that follow it (i.e., "fast" and "slow") into a vector.
# The first 12 participants are in the "fast" condition, and the second 12 the "slow"

condition <- c(rep("fast", times = 12), rep("slow", times = 12))

# We now have "fast" and "slow" repeated 12 times each - test by typing "condition"

condition
  
  
