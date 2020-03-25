
library(Metrics)
library(tidyverse)
library(caret)
library(plotly)
library(fastDummies)

businesses <- as.data.frame(read_csv("C:/Users/Valued Customer/Documents/GitHub/crux/portfolio/yelp/yelp_academic_dataset_business.csv"))

reviews <- as.data.frame(read_csv("C:/Users/Valued Customer/Documents/GitHub/crux/portfolio/yelp/yelp_academic_dataset_review.csv"))

head(businesses)
str(businesses)


# The number of reviews in reviews doesn't match the review_count in businesses. Options are to join the two df's and use the individual reviews from reviews (some data lost), or use the review_count from businesses to weight the avg stars for each business. The latter gives us more data.


businesses <- businesses %>% select(name, business_id, categories, stars, review_count)


businesses.expanded <- businesses[rep(row.names(businesses), businesses$review_count), 1:4]

# Compare businesses conducted on a more personal level with the rest of businesses

personal_categories <- c('Nail Salons', 'Hair Salons','Veterinarians','Dentists','General Dentistry','Chiropractors','Trainers','Massage','Tattoo','Photographers','Dermatologists','Doctors','Hair Removal','Law', 'Hair Salons', 'Real Estate Agents','Physical Therapy','Teachers')

# Split dataframe into one containing only 'personal' business and one that is all other categories
personal_biz <- businesses.expanded %>% filter(str_detect(categories, paste(personal_categories, collapse='|')))
other_biz <- businesses.expanded %>% filter(!(str_detect(categories, paste(personal_categories, collapse='|'))))
head(businesses)
personal_stars <- count(personal_biz, stars, normalized=T) %>% mutate(freq=n/sum(n), personal=1)
other_stars <- count(other_biz, stars, normalized=T) %>% mutate(freq=n/sum(n), personal=0)

personal_v_other <- rbind(personal_stars, other_stars) %>% mutate(personal=factor(personal))

personal_avg_stars <- round(mean(personal_biz$stars), digits=2)
other_avg_stars <- round(mean(other_biz$stars), digits=2)
personal_avg_stars
other_avg_stars

# plot the histogram for each
ggplot() + 
  geom_bar(data=personal_v_other, aes(x=stars, y=n, fill=personal), position='dodge', stat='identity') +
  geom_vline(xintercept=personal_avg_stars, color='blue') +
  geom_vline(xintercept=other_avg_stars, color='red') +
  annotate(geom="text", x=4.3, y=.32, label=paste('Avg:',toString(personal_avg_stars)," stars"), color="blue") +
  annotate(geom="text", x=3.4, y=.32, label=paste('Avg: ',toString(other_avg_stars), " stars"), color="red")

# Check if difference is statistically significant
# is the population all the businesses like below or all businesses with personal business removed??

mu <- mean(businesses.expanded$stars)
sd <- sd(businesses.expanded$stars)

x_bar <- mean(personal_biz$stars)
z <- (x_bar - mu) / (sd / sqrt(nrow(personal_biz)))

pvalue <- pnorm(z, lower.tail=F)
pvalue

# p-value < .001
# We can conclude that the increase in average stars is statistically significant, with extremely high certainty


# Now let's compare especially impersonal businesses with the population

impersonal_categories <- c('Pizza','Restaurants','Shopping',"Women's Clothing",'Hardware', 'Bars','Nightlife','Hotels & Travel','Electronics','Car Wash','Oil Change Station','Fashion','Breweries','Food','Department Stores',"Men's Clothing",'Coffee','Performing Arts','Books','Thrift stores','Pet Stores','Beer','Mattresses','Furniture','Jewelry')

impersonal_biz <- businesses.expanded %>% filter(str_detect(categories, paste(impersonal_categories, collapse='|')))
other_biz_2 <- businesses.expanded %>% filter(!(str_detect(categories, paste(impersonal_categories, collapse='|'))))

impersonal_biz_stars <- count(impersonal_biz, stars, normalized=T) %>% mutate(freq=n/sum(n), impersonal=1)
other_biz_2_stars <- count(other_biz_2, stars, normalized=T) %>% mutate(freq=n/sum(n), impersonal=0)

impersonal_v_other_biz_2 <- rbind(impersonal_biz_stars, other_biz_2_stars) %>% mutate(impersonal=factor(impersonal))

impersonal_avg_stars <- round(mean(impersonal_biz$stars), digits=2)
other_biz_2_avg_stars <- round(mean(non_personal_biz$stars), digits=2)
impersonal_avg_stars
other_biz_2_avg_stars

# plot the histogram for each
ggplot() + 
  geom_bar(data=impersonal_v_other_biz_2, aes(x=stars, y=n, fill=impersonal), position='dodge', stat='identity') +
  geom_vline(xintercept=impersonal_avg_stars, color='blue') +
  geom_vline(xintercept=other_biz_2_avg_stars, color='red') +
  annotate(geom="text", x=3.35, y=.39, label=paste('Avg:',toString(impersonal_avg_stars)," stars"), color="blue") +
  annotate(geom="text", x=4.2, y=.39, label=paste('Avg: ',toString(other_biz_2_avg_stars), " stars"), color="red")


mu <- mean(businesses.expanded$stars)
sd <- sd(businesses.expanded$stars)

x_bar <- mean(impersonal_biz$stars)
z <- (x_bar - mu) / (sd / sqrt(nrow(impersonal_biz)))

pvalue <- pnorm(z, lower.tail=T)
pvalue


# p-value < .001
# We can conclude that the increase in average stars is statistically significant, with extremely high certainty



# Compare businesses with 'Family' in the name or category with the rest of businesses

non_family_biz <- businesses.expanded %>% filter(!(grepl('Family', name) | grepl('Family', categories)))
family_biz <- businesses.xpanded %>% filter(grepl('Family', name) | grepl('Family', categories))

non_family_biz_stars <- count(non_family_biz, stars, normalized=T) %>% mutate(n=n/sum(n), family=0)
family_biz_stars <- count(family_biz, stars, normalized=T) %>% mutate(n=n/sum(n), family=1)

family_vs_non <- rbind(non_family_biz_stars, family_biz_stars) %>% mutate(family=factor(family), stars=factor(stars))

family_avg_stars <- round(mean(family_biz$stars), digits=2)
non_family_avg_stars <- round(mean(non_family_biz$stars), digits=2)
family_avg_stars
non_family_avg_stars


# plot the histogram for each
ggplot() + 
  geom_bar(data=family_vs_non, aes(x=stars, y=n, fill=family), position='dodge', stat='identity') 

# The average rating for 'Family' businesses is actually lower

# Check hypothesis that family related businesses will be higher rated than normal businesses
# # one tail test
# 
# mu <- mean(businesses.expanded$stars)
# sd <- sd(businesses.expanded$stars)
# 
# x_bar <- mean(family_biz$stars)
# 
# z <- (x_bar - mu) / (sd / sqrt(nrow(family_biz)))
# pvalue <- pnorm(z, lower.tail=F)
# pvalue




# Is it possible the people that use those services just happen to give higher reviews in general?
# Perhaps these more personal businesses are more indulgent in general and may be patronized by happier customers that give high reviews across the board
# let's check how their other reviews (non-personal categories) compare to the population

# combine reviews with businesses by business_id
biz_join_reviews <- left_join(businesses, (reviews %>% select(user_id, business_id, review_id, stars)), by=c('business_id'='business_id'))

# filter for reviewers that have reviewed 'personal' businesses
personal_reviewers <- biz_join_reviews %>% 
  filter(str_detect(categories, paste(personal_categories, collapse='|'))) %>% 
  distinct(user_id)

# subset the reviews data for those reviewers and filter for their non personal reviews
non_personal_review_ids <- biz_join_reviews %>% 
  filter(user_id %in% pull(personal_reviewers, user_id), !(str_detect(categories, paste(personal_categories, collapse='|')))) %>%
  pull(review_id)

# These are all of the reviews made by reviewers that reviewed businesses deemed as personal, filtering out those personal biz reviews
non_personal_reviews <- reviews %>% filter(review_id %in% non_personal_review_ids)



# compare the avg stars of personal_reviews with all reviews
# Check the null hypothesis that the sample mean approximates the population mean
mu <- mean(reviews$stars)
sd <- sd(reviews$stars)

x_bar <- mean(non_personal_reviews$stars)
n <- nrow(non_personal_reviews)

z <- (x_bar - mu) / (sd / sqrt(n))
pvalue <- 2 * pnorm(abs(z), lower.tail=F)
pvalue
n
# The p-value is < .001, confirming that reviewers that reviewed personal categories give higher ratings on average for all categories




# are people more likely to follow previous ratings?

# Is there any relation between the date and number of stars?
# perhaps people are more likely to give a good rating if a business is already highly rated?

# Find some businesses with a lot of reviews for a sufficient sample size to explore data
most_reviewed_businesses <- reviews %>% 
  group_by(business_id) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

# isolate the businesses with the most reviews

for (i in seq(1:10)) {
  most_reviewed_biz <- reviews %>% 
    filter(business_id %in% pull(most_reviewed_businesses, business_id)[i]) %>% 
    mutate(review_date = date) %>%
    select(review_date, stars) %>% 
    group_by(review_date) %>%          # to deal with multiple reviews on the same day, take the average for each day
    summarize(avg_stars = mean(stars))
  
  # convert date to numeric to find correlation
  startdate <- most_reviewed_biz$review_date[1]
  most_reviewed_biz$num_days  <- difftime(most_reviewed_biz$review_date, startdate ,units="days") %>%
    str_replace(" *","") %>%
    as.numeric()
  
  # print correlation for top 10 most popular businesses
  most_reviewed_biz %>% select(-review_date) %>% cor() %>% .['num_days','avg_stars'] %>% print()
  
}

# all have very low correlations between date and stars

# plot one of the top business date vs avg_stars
ggplot(most_reviewed_biz, aes(num_days, avg_stars)) +
  geom_jitter(stat='identity', alpha=.25, width=.1, height=.1)




# REGRESSION
# combine business and reviews
biz_reviews <- left_join(reviews, businesses, by=c('business_id','business_id'))

# Randomly reduce dataset to manageable size
target_sample_size = floor(0.33*nrow(biz_reviews))
set.seed(948)

# randomly split data in r
picked = sample(seq_len(nrow(biz_reviews)),size = target_sample_size)
biz_reviews = biz_reviews[picked,]



# Select features, rename columns, drop NAs
# business_id?
biz_reviews <- biz_reviews %>% 
  select(review_count, city, date, categories, stars.x) %>%
  rename(total_reviews_of_biz = review_count,
         review_date = 'date',
         review_stars = stars.x)
  


# drop NAs
sapply(biz_reviews, function(x) sum(is.na(x)))
biz_reviews <- drop_na(biz_reviews)

# convert date to numeric to use as predictor
startdate <- min(biz_reviews$review_date)
biz_reviews$num_days  <- difftime(biz_reviews$review_date, startdate ,units="days") %>%
  str_replace(" *","") %>%       # remove "days" unit to convert to numeric
  as.numeric() 

# Normalize num_days
biz_reviews$num_days <- (biz_reviews$num_days - min(biz_reviews$num_days)) /
                        (max(biz_reviews$num_days) - min(biz_reviews$num_days))


# Remove date col
biz_reviews$review_date = NULL



# normalize total_reviews_of_biz
biz_reviews$total_reviews_of_biz <- (biz_reviews$total_reviews_of_biz - min(biz_reviews$total_reviews_of_biz)) /
                                  (max(biz_reviews$total_reviews_of_biz) - min(biz_reviews$total_reviews_of_biz))



# 'Batman' this column. (clean up the cities)
biz_reviews$city <- biz_reviews$city %>%
  tolower() %>%
  str_replace('north|^n[ \\.]|west|^w |^e |^c |n e |n w |south|east|\\(.*\\)|,.*','') %>%   # remove cardinal directions
  str_replace('\\(.*\\)', '') %>%
  str_replace("[[:punct:][:blank:]]+", " ") %>%
  str_replace('^st ','saint') %>%
  str_replace('mc farland','mcfarland') %>%
  str_replace('de forest','deforest') %>%
  str_replace('glendale az', 'glendale') %>%
  str_replace('lake las vegas|las vegass', 'las vegas') %>%
  str_replace('pheonix|phoenix sky harbor center','phoenix') %>%
  str_replace("and las vegas","") %>%
  trimws() %>%
  str_replace(" ", "_")

sort(table(biz_reviews$city), decreasing=T)

# Create dummy columns for city
biz_reviews <- biz_reviews %>% 
  dummy_cols(select_columns = 'city') %>%
  select(-city)


# clean categories
biz_reviews$categories <- biz_reviews$categories %>%
  tolower %>%
  str_replace_all(" & |\\/", "_") %>%
  str_replace_all(" +", "_") %>%
  str_replace_all("\\(|\\)","")


# find unique categories
unique_categories <- biz_reviews$categories %>% 
  str_split(';') %>% 
  unlist %>% 
  unique


# Create dummy variable for categories
# Make a list of the categories column with individual categories
split_cats <- strsplit(biz_reviews$categories, split=';')
matches <- lapply(unique_categories, function(i) sapply(split_cats, function(j) +(any(grepl(i, j), na.rm = TRUE))))
cat_dummy_vars <- as.data.frame(Reduce(cbind, matches))
names(cat_dummy_vars) <- unique_categories


# combine dummy variables with other columns, remove original categories column
biz_reviews <- cbind(biz_reviews, cat_dummy_vars) %>%
  select(-categories)


head(biz_reviews)

# Add subjective is_personal column

biz_reviews <- biz_reviews %>% mutate(is_personal = ifelse(nail_salons==1 |
                                                        hair_salons==1 |
                                                        veterinarians==1 |
                                                        dentists==1 |
                                                        general_dentistry==1 |
                                                        chiropractors==1 |
                                                        trainers==1 |
                                                        massage==1 |
                                                        tattoo==1 |
                                                        photographers==1 |
                                                        dermatologists==1 |
                                                        doctors==1 |
                                                        hair_removal==1 |
                                                        real_estate_agents==1 |
                                                        physical_therapy==1 |
                                                        barbers==1 |
                                                        beauty_spas==1 |
                                                        makeup_artists==1 |
                                                        life_coach==1 |
                                                        psychiatrists==1 |
                                                        hair_removal==1 |
                                                        speech_therapists==1 |
                                                        pediatric_dentists==1 |
                                                        nanny_services==1 |
                                                        "men's_hair_salons"==1 |
                                                        psychics_astrologers==1 |
                                                        interior_design==1 |
                                                        career_counseling==1 |
                                                        personal_assistants==1 |
                                                        tutoring_centers==1 |
                                                        lactation_services==1 |
                                                        college_counseling==1 |
                                                        eyelash_service==1 |
                                                        day_spas==1 |
                                                        occupational_therapy==1 |
                                                        cosmetic_dentists==1 |
                                                        hair_stylists==1 |
                                                        waxing==1 |
                                                        fertility==1 |
                                                        private_tutors==1 |
                                                        blow_dry_out_services==1 |
                                                        massage_therapy==1 |
                                                        counseling_mental_health==1 |
                                                        nutritionists==1 |
                                                        midwives==1,
                                                      1, 0))

# Calculate correlations of all columns vs review_stars
corr <- cor(biz_reviews)
review_stars_corr <- corr[2,]
review_stars_corr[order(review_stars_corr, decreasing = T)]

# Linear Regression

test_index <- createDataPartition(biz_reviews$review_stars, times = 1, p = 0.2, list = FALSE)

train <- biz_reviews %>% slice(-test_index) %>% select(-is_personal)
test <- biz_reviews %>% slice(test_index)

# X = train %>% select(-review_stars, -is_personal)
# y= train %>% select(review_stars)
# y = y[['review_stars']]
# X

# Squared Loss if we simply guess with average
m <- mean(train$review_stars)
mean((m - test$review_stars)^2)
rmse(test$review_stars, m)

fit <- lm(review_stars ~ ., train)
fit$coef[order(fit$coef, decreasing=T)]['is_personal']

y_hat <- predict(fit, test)
rmse(test$review_stars, y_hat)

fit <- lm(review_stars ~ ., train)
y_hat <- predict(fit, test)
rmse(test$review_stars, y_hat)



fitControl <- trainControl(## 10-fold CV
                          method = "cv",
                          number = 4)

train()

biz_reviews[biz_reviews$slovakian==1,]


fit <- train(review_stars ~  -is_personal, method = 'lm', data = train, trControl=fitControl)
y_hat <- predict(fit, test)
rmse(test$review_stars)


fit <- train(review_stars ~ ., data = train, method = 'lm', trControl=fitControl)
y_hat <- predict(fit, test)
rmse(test$review_stars, y_hat)

# If I'm not going to use the whole dataset, would it be better to cross validate with new unused sections from the original data?

train <- biz_reviews %>% slice(-test_index) %>% select(total_reviews_of_biz, review_stars, num_days, is_personal)
test <- biz_reviews %>% slice(test_index) %>% select(total_reviews_of_biz, review_stars, num_days, is_personal)

biz_reviews %>%summarize(sum)
apply(biz_reviews, sum, MARGIN=2)
mean(biz_reviews$is_personal)

colnames(biz_reviews)
str(biz_reviews[,c(1:129, 832)])

train <- biz_reviews[,c(1:3, 832)] %>% slice(-test_index)
test <- biz_reviews[,c(1:3, 832)] %>% slice(test_index)

fit <- lm(review_stars ~ ., train)
y_hat <- predict(fit, test)
rmse(test$review_stars, y_hat)

summary(fit)$r.squared
table(biz_reviews)

resids <- resid(fit)
plot(biz_reviews$is_personal, resids, 
            ylab="Residuals", xlab="Waiting Time", 
            main="Old Faithful Eruptions") 
          abline(0,0)       # the horizon
          train
fit <- lm(review_stars ~ is_personal, train)
y_hat <- predict(fit, test)
rmse(test$review_stars, y_hat)
y_hat <- mean(train$review_stars)
