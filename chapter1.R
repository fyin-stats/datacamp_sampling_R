########################
##############
# sampling
##############
########################
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    try(sapply(pkg, require, character.only = TRUE), silent = TRUE)
}
packages <- c("fst", "dplyr", "ggplot2", "tidyr")
ipak(packages)
##########################
# estimate the population of France
# censuses are really expensive
# most countries only take it every 5-10 years
# cheapter to ask a small number of households
# and use statistics to make estimates about the whole 
# population

# working with a subset of the whole population is called sampling

# population vs sample
# population is the complete dataset
# typically don't know what the whole population is
# sample, is subset of data you work with

# coffee rating dataset

# 
coffee <- read_fst("./data/coffee_ratings_full.fst")

# each row represents coffee

# points vs flavor: population
pts_vs_flavor_samp <- coffee %>% dplyr::select(total_cup_points, flavor) %>% slice_sample(n=10) # select a random subset of rows

# slice_sample() for data frames, and sample() for vectors
# 
cup_points_samp <- sample(coffee$total_cup_points, size=10)

# population parameters and point estimates
# 
mean(pts_vs_flavor_samp)

# summarize function to get means

# 

# From previous step
spotify_sample <- spotify_population %>% 
    slice_sample(n = 1000)

# Calculate the mean duration in mins from spotify_population
mean_dur_pop <- spotify_population %>% 
    summarize(mean_dur = mean(duration_minutes))

# Calculate the mean duration in mins from spotify_sample
mean_dur_samp <- spotify_sample %>% 
    summarize(mean_dur = mean(duration_minutes))

# See the results
mean_dur_pop
mean_dur_samp



# Get the loudness column of spotify_population
loudness_pop <- spotify_population$loudness

# Sample 100 values of loudness_pop
loudness_samp <- sample(loudness_pop, size = 100)

# See the results
loudness_samp


#################################################
#########
# From previous step
loudness_pop <- spotify_population$loudness
loudness_samp <- sample(loudness_pop, size = 100)

# Calculate the standard deviation of loudness_pop
sd_loudness_pop <- sd(loudness_pop)

# Calculate the standard deviation of loudness_samp
sd_loudness_samp <- sd(loudness_samp)

# See the results
sd_loudness_pop
sd_loudness_samp


####################################################
############
# a little too convenient
############
# the literary digest election prediction
# prediction: Landon gets 57%, Roosevelt gets 43%
# in reality, Roosevelt landslide
# sample not representative of population
# causing sample bias
# collecting data by the easiest method is called convenience sampling
# need better sampling method to get unbiased estimate

# finding the mean age of French people
# survey 10 people at the entrance of Disney
# people who visit Disneyland aren't representative of the whole population
# 
coffee %>% summarize(mean_cup_points = mean(total_cup_points))

# slice_head() in dplyr
# head() from base R

# convenience sample is not representative of the whole sample
# visualizing selection bias
# 

# 
# Update the histogram to use spotify_mysterious_sample2
# Set the x-axis limits from 0 to 15
ggplot(spotify_population, aes(duration_minutes)) +
    geom_histogram(binwidth = 0.01)

# 
# Delightful duration distribution analysis! 
# The duration values in the sample show a similar distribution to those 
# in the whole population, 
# so the results are generalizable. 

# How does Sue do sampling?
# what does random mean? 
# made done, happening, or chosen without method of concious decision

# true random numbers
# generated from physical processes, like flipping coins
# hotbits uses radioactivity decay

# pseudo random number generation
# next random number calculated from previous random number
# the first random number calculated from a seed

# 
seed <- 1
calc_next_random(seed)

# random number generating functions
# visualizing random numbers
# 
rbeta(5000, shape1=2, shape2 = 2)

#
randoms <- data.frame(beta = rbeta(5000, shape1=2, shape2 = 2))

# 
ggplot(randoms, aes(beta)) + geom_histogram(binwidth = 0.1)

# random numbers seeds
set.seed(20000229)
# rnorm(5)

# set.seed(20000229)

# this makes our code reproducible

# 
# Generate random numbers from ...
randoms <- data.frame(
    # a uniform distribution from -3 to 3
    uniform = runif(n_numbers, -3, 3),
    # a normal distribution with mean 5 and sd 2
    normal = rnorm(n_numbers, 5, 2)
)

#
# From previous step
randoms <- data.frame(
    uniform = runif(n_numbers, min = -3, max = 3),
    normal = rnorm(n_numbers, mean = 5, sd = 2)
)

# Plot a histogram of normal values, binwidth 0.5
randoms %>% ggplot(aes(x = normal)) + geom_histogram(binwidth = 0.5)

# x and y have identical values
set.seed(123)
x <- c(rnorm(5), rnorm(5))
set.seed(123)
y <- rnorm(10)