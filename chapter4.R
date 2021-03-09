###########################
###########################
###########################
### sampling without replacement
###########################
###########################
###########################

# sampling with replacement (resampling)
# population, sample
# sample with replacement


# population of coffees
# can treat coffee ratings data as being a sample of a larger population of all coffees
# think about each coffee in our sample as being representative of many different coffees
# that we don't have in our sample, but do exist in the population

# sampling with replacement is a proxy for including different members of these groups in our sample
# coffee data preparation
# row_id_column
# slice_sample
# count(rowid)
# missing coffees
# n_distinct()

#############################
#############################
# Bootstrapping, the opposite of sampling from a population
# sampling: going from a population to a smaller sample
# bootstrapping: builing up a theoretical population from your sample
# bootstrapping use case:
# develop understanding of sampling variability using a single sample

# bootstrapping process
# make a resample of the same size as the original sample
# calculate the statistic of interest for this bootstrap sample
# repeat steps 1 and 2 many times
# the resulting statsitics are called bootstrap statistic and when viewed to see their variability a bootstrap distribution
# slice_sample(replace = TRUE)


################################
################################
# Radical replacement reasoning! The key to deciding whether to sample without or with replacement is 
# whether or not your dataset is best thought of as being the whole population or not.


# From previous steps
mean_danceability_1000 <- load_step_4()

# Store the resamples in a tibble
bootstrap_distn <- tibble(
    resample_mean = mean_danceability_1000
)

# Draw a histogram of the resample means with binwidth 0.002
bootstrap_distn %>% ggplot(aes(x=resample_mean)) + geom_histogram(binwidth=0.002)





###################################
###################################
# slice sample generates resample
###################################
###################################
# the bootstrap distribution mean is usually almost identical to the sample mean
# it may not be a good estimate of the population mean
# bootstrapping cannot correct biases due to differences between your sample and the population
# sd(resample_mean)

# standard error is the standard deviation of the statistic of interest

# standard error times the square root of sample size estimates the population standard deviation
# estimated standard error is the standard deviation of the bootstrap distribution for a sample statistic
# the bootstrap distribution standard error times the square root of the sample size estimates the standard
# deviation in the population

# Super standard error reasoning! If the sample is not closely representative of the population, 
# then the mean of the bootstrap distribution will 
# not be representative of the population mean. This is less of a problem for standard errors.





####################################
############
# Generate a bootstrap distribution
mean_popularity_2000_boot <- replicate(
    # Use 2000 replicates
    n = 2000,
    expr = {
        # Start with the sample
        spotify_sample %>% 
            # Sample same number of rows with replacement
            slice_sample(prop = 1, replace = TRUE) %>% 
            # Calculate the mean popularity
            summarize(mean_popularity = mean(popularity)) %>% 
            # Pull out the mean popularity
            pull(mean_popularity)
    }
)

# See the result
mean_popularity_2000_boot

#
# Calculate the true population mean popularity
pop_mean <- spotify_population %>% 
    summarize(mean(popularity))

# Calculate the original sample mean popularity
samp_mean <- spotify_sample %>% 
    summarize(mean(popularity))

# Calculate the sampling dist'n estimate of mean popularity
samp_distn_mean <- sampling_distribution %>% 
    summarize(mean(sample_mean))

# Calculate the bootstrap dist'n estimate of mean popularity
boot_distn_mean <- bootstrap_distribution %>% 
    summarize(mean(resample_mean))

## See the results
c(pop = pop_mean, samp = samp_mean, samp_distn = samp_distn_mean, boot_distn = boot_distn_mean)


#
# Calculate the true population std dev popularity
pop_sd <- spotify_population %>% 
    summarize(sd(popularity))

# Calculate the original sample std dev popularity
samp_sd <- spotify_sample %>% 
    summarize(sd(popularity))

# Calculate the sampling dist'n estimate of std dev popularity
samp_distn_sd <- sampling_distribution %>% 
    summarize(sd(sample_mean) * sqrt(500))

# Calculate the bootstrap dist'n estimate of std dev popularity
boot_distn_sd <- bootstrap_distribution %>% 
    summarize(sd(resample_mean) * sqrt(500))

## See the results
c(pop = pop_sd, samp = samp_sd, sam_distn = samp_distn_sd, boot_distn = boot_distn_sd)



#
# Super standard devations! This is an important property of the bootstrap distribution. 
# When you don't know have all the values from the true population, 
# you can use bootstrapping to get a good estimate of the population standard deviation. 














########################## Venus infers
########################## confidence intervals
########################## How predictable is US weather?
########################## weather prediction
########################## point estimate
########################## range of plausible high temperature values
########################## 40 to 54 is a confidence interval
# mean of resamples
# PDF bell curve
# CDF integrate to get area under bell curve
# Inv. CDF
# standard error method for confidence interval
# 

# Generate a 95% confidence interval using the std error method
conf_int_std_error <- bootstrap_distribution %>% 
    summarize(point_estimate = mean(resample_mean),
              standard_error = sd(resample_mean), 
              lower = qnorm(0.025, mean = point_estimate, sd = standard_error),
              upper = qnorm(0.975, mean = point_estimate, sd = standard_error))






# See the result
conf_int_std_error


#############################
### Recap
### the standard deviation of the sampling distribution of a statistic is well-approximated by the standard
### deviation


### 