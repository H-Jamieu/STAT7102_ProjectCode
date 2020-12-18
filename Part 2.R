df <- read.csv('AB_US_2020_V2.csv')
df$neighbourhood = substr(df$neighbourhood, 1, nchar(df$neighbourhood)-1)
df$neighbourhood_group = substr(df$neighbourhood_group, 1, nchar(df$neighbourhood_group)-1)

df <- df[df[, "price"] <=1000,]
df <- df[df[, "price"] >8,]

lengths(lapply(df, unique))

head(sort(table(df$neighbourhood)), 10)

hist(df$price)

# Turn other useful values into factor.
df$neighbourhood_group = as.factor(df$neighbourhood_group)
df$room_type = as.factor(df$room_type)
df$city = as.factor(df$city)

# Fit Model
library(glm2)

fit_poi <- glm2(price~neighbourhood_group+room_type+city+minimum_nights+
                  number_of_reviews+last_review+reviews_per_month+
                  calculated_host_listings_count+availability_365, 
                data=df, family='poisson')
summary(fit_poi)

fit_gau <- glm2(price~neighbourhood_group+room_type+city+minimum_nights+
                  number_of_reviews+last_review+reviews_per_month+
                  calculated_host_listings_count+availability_365, 
                data=df, family=gaussian)
summary(fit_gau)

fit0 <- glm2(price~neighbourhood_group+room_type+city+minimum_nights+
               number_of_reviews+last_review+reviews_per_month+
               calculated_host_listings_count+availability_365, 
             data=df, family=gaussian(link='log'))
summary(fit0)

# omit neighbourhood_group
fit1 <- glm2(price~room_type+city+minimum_nights+number_of_reviews+last_review+
               reviews_per_month+calculated_host_listings_count+availability_365, 
             data=df, family=gaussian(link='log'))
summary(fit1)

# omit city
fit2 <- glm2(price~room_type+neighbourhood_group+minimum_nights+number_of_reviews+
               last_review+reviews_per_month+calculated_host_listings_count+
               availability_365, data=df, family=gaussian(link='log'))
summary(fit2)
