#################### 
## chapter 3
####################
## the n's justify the means

## population parameter
## use pull function to extract values as a number

## increasing sample size brings the number closer to the truth
## relative errors
## population parameter
## point estimate


## relative error as a percentage
## relative error vs sample size
## geom_smooth(method = "loess")
## 

# Generate a simple random sample of 10 rows 
attrition_srs10 <- attrition_pop %>% slice_sample(n=10)

# Calculate the proportion of employee attrition in the sample
mean_attrition_srs10 <- attrition_srs10 %>% summarize(mean(Attrition == "Yes"))

# Calculate the relative error percentage
rel_error_pct10 <- abs(100*(mean_attrition_srs10 - mean_attrition_pop)/mean_attrition_pop)

# See the result
rel_error_pct10


# Generate a simple random sample of 10 rows 
attrition_srs100 <- attrition_pop %>% slice_sample(n=100)

# Calculate the proportion of employee attrition in the sample
mean_attrition_srs100 <- attrition_srs100 %>% summarize(mean(Attrition == "Yes"))

# Calculate the relative error percentage
rel_error_pct100 <- abs(100*(mean_attrition_srs100 - mean_attrition_pop)/mean_attrition_pop)

# See the result
rel_error_pct100







###########################################
###########################################
##### same code, different answer
###########################################
###########################################
### same code, 1000 times

### replicate(n=1000, expr = )
### tibble 
### sampling distribution is a distribution of several replicates of point estimates
### 
ggplot(sample_means, aes(sample_mean)) + geom_histogram(binwidth = 0.1)

## different sample sizes
## sample size 6
## bigger sample sizes give you more accurate results
## 

# From previous step
mean_attritions <- replicate(
    n = 500,
    attrition_pop %>% 
        slice_sample(n = 20) %>% 
        summarize(mean_attrition = mean(Attrition == "Yes")) %>% 
        pull(mean_attrition)
)

# Store mean_attritions in a tibble in a column named sample_mean
sample_means <- tibble(sample_mean = mean_attritions)

# Plot a histogram of the `sample_mean` column, binwidth 0.05
sample_means %>% ggplot(aes(x=sample_mean)) + geom_histogram(binwidth=0.05)















###############
### 4 dice
###############
###############
library(tidyr)
dice <- expand_grid(die1 = 1:6,
                    die2 = 1:6,
                    die3 = 1:6,
                    die4 = 1:6)
##############
##############
# mean roll
##############
##############

# Exact sampling distribution
# is impossible when the number of dices is large in the context of this problem
# 
sample_means_1000 <- replicate(n=1000,
                               expr = {four_rolls <- sample(1:6, size=4, replace = TRUE)
                               mean(four_rolls)})

# can use approximations to figure out what the sampling distribution behaves
#

dice <- expand_grid(
    die1 = 1:8,
    die2 = 1:8,
    die3 = 1:8,
    die4 = 1:8,
    die5 = 1:8
) %>% 
    # Add a column of mean rolls
    mutate(mean_roll = (die1 + die2 + die3 + die4 + die5) / 5)


#
# From previous step
dice <- expand_grid(
    die1 = 1:8,
    die2 = 1:8,
    die3 = 1:8,
    die4 = 1:8,
    die5 = 1:8
) %>% 
    mutate(mean_roll = (die1 + die2 + die3 + die4 + die5) / 5)

# Using dice, draw a bar plot of mean_roll as a factor
ggplot(dice, aes(x=factor(mean_roll))) + geom_bar()



# From previous steps
sample_means_1000 <- replicate(
    n = 1000,
    expr = {
        five_rolls <- sample(1:8, size = 5, replace = TRUE)
        mean(five_rolls)
    }
)
sample_means <- tibble(
    sample_mean = sample_means_1000
)

# Using sample_means, draw a bar plot of sample_mean as a factor
sample_means %>% ggplot(aes(x=factor(sample_mean))) + geom_bar()

#
# Delightful sampling distribution distinguishing! The exact sampling distribution can only be calculated 
# if you know what the population is, and if the problems is small and simple 
# enough to compute. Otherwise, the approximate sampling distribution must be used.

################################
### Err on the side of Gaussian
#################################
### approximate sampling distribution of mean cup points
### consequences of the central limit theorem
### averages of independent samples have approximately normal distributions as the sample size increases
### the distribution of the averages gets closer to being normally distributed
### and the width of the sampling distribution gets narrower
### population mean over square root sample size
### 

# Calculate the mean across replicates of the mean 
# attritions in sampling_distribution_5
mean_of_means_5 <- sampling_distribution_5 %>% summarize(mean(mean_attrition))


# Do the same for sampling_distribution_50
mean_of_means_50 <- sampling_distribution_50 %>% summarize(mean(mean_attrition))


# ... and for sampling_distribution_500
mean_of_means_500 <- sampling_distribution_500 %>% summarize(mean(mean_attrition))


# See the results
mean_of_means_5
mean_of_means_50
mean_of_means_500

# The standard deviation of the sampling distribution is approximately equal to the population 
# standard deviation divided by the square root of the sample size.