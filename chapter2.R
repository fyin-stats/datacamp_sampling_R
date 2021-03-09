#####################
#### simple random sampling of coffees
######################
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    try(sapply(pkg, require, character.only = TRUE), silent = TRUE)
}
packages <- c("fst", "dplyr", "ggplot2", "tidyr", "tibble")
ipak(packages)
#### simple random sampling in R

#### systematic sampling (regular intervals)
#### sample_size <- 5
#### pop_size <- nrow(coffee)
#### interval <- pop_size %/% sample_size
#### row_indexes <- seq_len(sample_size) * interval
#### 
coffee %>% slice(row_indexes)

#### the trouble with systematic sampling
#### what if there is a trend in how the data are organized
#### smooth line shows a trend line
coffee %>% rowid_to_column() %>% ggplot(aes(x = rowid, y=aftertaste)) + 
    geom_point() + geom_smooth()

#### Making systematic sampling safe
#### shuffle before sampling
#### slice_sample(prop = 1)
#### once it is shuffled, systematic sampling is equivalent to simple random sampling
#### 

# View the attrition_pop dataset
View(attrition_pop)

# Set the seed
set.seed(2021)

attrition_samp <- attrition_pop %>% 
    # Add a row ID column
    rowid_to_column() %>% 
    # Get 200 rows using simple random sampling
    slice_sample(n=200)

# View the attrition_samp dataset
View(attrition_samp)


#
# From previous step
sample_size <- 200
pop_size <- nrow(attrition_pop)
interval <- pop_size %/% sample_size

# Get row indexes for the sample
row_indexes <- seq_len(sample_size) * interval

attrition_sys_samp <- attrition_pop %>% 
    # Add a row ID column
    rowid_to_column() %>% 
    # Get 200 rows using systematic sampling
    slice(row_indexes)

# See the result
View(attrition_sys_samp)

#
# Add a row ID column to attrition_pop
attrition_pop_id <- attrition_pop %>% 
    rowid_to_column()

# Using attrition_pop_id, plot YearsAtCompany vs. rowid
ggplot(attrition_pop_id, aes(rowid, YearsAtCompany)) +
    # Make it a scatter plot
    geom_point() +
    # Add a smooth trend line
    geom_smooth()

# 




############
## Can't get no stratification
############

## filtering for 6 countries
## top_counted_countries <- c("Mexico", "Colombia", "Guatemala",
## "Brazil", "Taiwan", "United States")
## 
coffee_ratings_top <- coffee %>% filter() # use filter to get the subsample

# semi_join

# counts of a simple random sample
# slice_sample 
# just by chance, in this sample, Guatemala coffees form a disproportionately high
# percentage of the dataset, and Colombian coffees form a disproportionately low percentage
# the different makeup of the sample compared to the population could be a problem
# if want to analyze the country of origin.
# proportional stratified sampling
# coffee_ratings_strat 
# equal counts stratified sampling
# coffee_ratings_eq
# weighted random sampling
# slice_sample, prop, weight_by
# need to correct for under or overrepresentation of demographic groups
# 

# Classy classification! Stratified sampling is useful if you care about subgroups. 
# Otherwise, simple random sampling is more appropriate.

# 
# From previous step
attrition_pop %>% 
    count(Education, sort = TRUE) %>% 
    mutate(percent = n / sum(n))


# 
# From previous step
attrition_pop %>% 
    count(Education, sort = TRUE) %>% 
    mutate(percent = n / sum(n))

# Use proportional stratified sampling to get 
# 40% of each Education group
attrition_strat <- attrition_pop %>% 
    group_by(Education) %>% 
    slice_sample(prop = 0.4) %>% 
    ungroup()

# See the result
attrition_strat


# From previous steps
attrition_pop %>% 
    count(Education, sort = TRUE) %>% 
    mutate(percent = 100 * n / sum(n))
attrition_strat <- attrition_pop %>% 
    group_by(Education) %>% 
    slice_sample(prop = 0.4) %>% 
    ungroup()

# Get the counts and percents from attrition_strat
education_counts_strat <- attrition_strat %>% 
    count(Education, sort = TRUE) %>% 
    mutate(percent = 100 * n / sum(n))

# See the results
education_counts_strat



#
# From previous step
attrition_eq <- attrition_pop %>%
    group_by(Education) %>% 
    slice_sample(n = 30) %>%
    ungroup()

# Get the counts and percents from attrition_strat
education_counts_eq <- attrition_eq %>% 
    count(Education, sort = TRUE) %>% 
    mutate(percent = 100 * n / sum(n))

# See the results
education_counts_eq



###############################
###############################
### Weighted sampling
###############################
###############################
# From previous step
attrition_weight <- attrition_pop %>%
    slice_sample(n = 400, weight_by = YearsAtCompany)

# Calculate mean YearsAtCompany using attrition_pop
attrition_pop %>% summarize(mean(YearsAtCompany))

# Calculate mean YearsAtCompany using attrition_weight
attrition_weight %>% summarize(mean(YearsAtCompany))







####################################
####################################
### One problem with stratified sampling is that you
### need to collect data from every subgroup
### in cases where collecting data is expensive, it can 
### make you analysis prohibitively expensive
### 

### stratified sampling vs clustering sampling
### stratified sampling:
### split the population into subgroups
### use simple random sampling on every subgroup

### cluster sampling:
### use simple random sampling to pick some subgroups
### use simple random sampling on only those subgroups
###


### 
varieties_pop <- unique(coffee$variety) 

### 
varieties_samp <- sample(varieties_pop, size = 3)

### slice_sample with filter step before 
### equal_count sampling (fix n when slice sample)
### 
# Delightful decision making! The main benefit of cluster sampling 
# over stratified sampling is that you can 
# save time or money by not including every subgroup in your sample.

# From previous step
job_roles_pop <- unique(attrition_pop$JobRole)
job_roles_samp <- sample(job_roles_pop, size = 4)

# Filter for rows where JobRole is in job_roles_samp
attrition_filtered <- attrition_pop %>% 
    filter(JobRole %in% job_roles_samp)

# Randomly sample 10 employees from each sampled job role
attrition_clus <- attrition_filtered %>% 
    group_by(JobRole) %>% 
    slice_sample(n = 10)

# See the result
attrition_clus













######################
######################
## Straight to the top
######################
######################
## simple random sampling
## stratified sampling
## cluster sampling

## calculating mean cup points population
## population mean, in typical scenario, we don't know it
## mean cup points 

## with clustering sampling, while the sample means are pretty close
## to the population mean,
## the obvious limitation is that you only get values for the two countries
## that were included in the sample

## 
# Get unique values of RelationshipSatisfaction
satisfaction_unique <- unique(attrition_pop$RelationshipSatisfaction)

# Randomly sample for 2 of the unique satisfaction values
satisfaction_samp <- sample(satisfaction_unique, size = 2)

# Perform cluster sampling on the selected group 
# getting 0.25 of the population
attrition_strat <- attrition_pop %>%
    filter(RelationshipSatisfaction %in% satisfaction_samp) %>% 
    group_by(RelationshipSatisfaction) %>% 
    slice_sample(n = nrow(attrition_pop) / 4) %>% 
    ungroup()


#
# Calculate the same thing for the simple random sample 
mean_attrition_srs <- attrition_srs %>% summarize(mean_attrition = mean(Attrition == "Yes"))



# See the result
mean_attrition_srs

# Calculate the same thing for the stratified sample 
mean_attrition_strat <- attrition_pop %>% group_by(RelationshipSatisfaction) %>% summarise(mean_attrition = mean(Attrition == "Yes"))



# See the result
mean_attrition_strat


# Calculate the same thing for the cluster sample 
mean_attrition_clust <-  attrition_clust %>% 
    group_by(RelationshipSatisfaction) %>% 
    summarize(mean_attrition = mean(Attrition == "Yes"))



# See the result
mean_attrition_clust