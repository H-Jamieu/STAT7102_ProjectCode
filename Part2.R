library(corrplot)
library(glm2)
library(nlme)
library(lme4)
library(glmulti)
library(ggplot2)
library(gam)
library(locfit)
library(MASS)
####### 2. data preprocessing ####### 
Airbnb <-read.csv(file="AB_US_2020_3.csv", header=TRUE, sep=",",
                  encoding = 'UTF-8', stringsAsFactors = FALSE) 
# 2.1 delete nan
Airbnb <- Airbnb[Airbnb[,'neighbourhood_group']!='nana',]
Airbnb <- Airbnb[Airbnb[,'neighbourhood_group']!='',]
Airbnb <- Airbnb[Airbnb[,'last_review']!=0,]
Airbnb <- Airbnb[!is.na(Airbnb[,'reviews_per_month']),]
# 2.2 set bar with mad
Mad_transform <- function(tt, n){
  len <- length(tt)
  median0 <- median(tt)
  mad0 <- mad(tt)
  for(i in 1:len){
    x <- tt[i] 
    if(x > median0+n*mad0){ tt[i] <- median0+n*mad0 }
    else if (x < median0-n*mad0){ tt[i] <- median0-n*mad0}
  } return(tt) }
Custom_transform <- function(tt, a, b){
  len <- length(tt)
  for(i in 1:len){
    x <- tt[i] 
    if(x > a){ tt[i] <- a } 
    else if (x < b){ tt[i] <- b }
  } return(tt) }
Airbnb[,11] <- Mad_transform(Airbnb[,11], 5)
Airbnb[,12] <- Mad_transform(Airbnb[,12], 5)
Airbnb[,13] <- Mad_transform(Airbnb[,13], 5)
Airbnb[,14] <- Mad_transform(Airbnb[,14], 5)
Airbnb[,15] <- Custom_transform(Airbnb[,15], 50,0)
# 2.3 rearray
col1 <- c("neighbourhood", "neighbourhood_group", "room_type", "city")
col0 <- c("latitude", "longitude", "minimum_nights", "number_of_reviews", 
          "last_review", "reviews_per_month", "calculated_host_listings_count", 
          "availability_365")
Airbnb <- Airbnb[,c('id',col1,col0,'price')]
col <- colnames(Airbnb)  # colnames
col
# 2.4 save data
# write.table(Airbnb,"AB_US_2020_V2.csv", row.names=FALSE, col.names=TRUE, sep=",")
####### 3. data pre analysising ####### 
# 3.1 correlation heat map
df <- cor(Airbnb[c(col0, 'price')])
corrplot(corr=df, method = "color", type = "upper", 
         tl.srt=45, number.cex= 7/ncol(df), addCoef.col="black")
# 3.2 histogram and KDE
tt<- sort(Airbnb[Airbnb$price<=1000,"price"])
kde1 <- density(tt, bw = "nrd", kernel =  "gaussian") 
hist(tt, col = "grey", freq = FALSE, main='Histogram of Airbnb$price', xlab = 'Airbnb$price')
lines(kde1, col = "blue", lwd = 2)  

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
fit_poi <- glm2(price~neighbourhood_group+room_type+city+minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365, data=df, family='poisson')
summary(fit_poi)

fit_gau <- glm2(price~neighbourhood_group+room_type+city+minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365, data=df, family=gaussian)
summary(fit_gau)

fit0 <- glm2(price~neighbourhood_group+room_type+city+minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365, data=df, family=gaussian(link='log'))
summary(fit0)

# omit neighbourhood_group
fit1 <- glm2(price~room_type+city+minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365, data=df, family=gaussian(link='log'))
summary(fit1)

# omit city
fit2 <- glm2(price~room_type+neighbourhood_group+minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365, data=df, family=gaussian(link='log'))
summary(fit2)

library(glm2)


AirBnbData <- read.csv("AB_US_2020_V2.csv")
BnbData <- AirBnbData

mu <- mean(BnbData$price)
sd <- sd(BnbData$price)
reducedBnbData <- subset(BnbData, select=c('neighbourhood', 'neighbourhood_group','Host_id','room_type',
                                           'city','minimum_nights','number_of_reviews','last_review','reviews_per_month',
                                           'availability_365','price'))
refNeighbour <- sum(lengths(unique(reducedBnbData$neighbourhood)))
reducedBnbData$neighbourhood <- relevel(as.factor(reducedBnbData$neighbourhood),ref = refNeighbour)
refNeighbourG <- sum(lengths(unique(reducedBnbData$neighbourhood_group)))
reducedBnbData$neighbourhood_group <- relevel(as.factor(reducedBnbData$neighbourhood_group),ref = refNeighbourG)
refRmType <- sum(lengths(unique(reducedBnbData$room_type)))
reducedBnbData$room_type <- relevel(as.factor(reducedBnbData$room_type),ref = refRmType)
refCity <- sum(lengths(unique(reducedBnbData$city)))
reducedBnbData$city <- relevel(as.factor(reducedBnbData$city),ref = refCity)
refHost <- sum(lengths(unique(reducedBnbData$Host_id)))
reducedBnbData$Host_id <- relevel(as.factor(reducedBnbData$Host_id),ref = refHost)

reducedBnbData <- reducedBnbData[reducedBnbData[, "price"] <=1000,]
reducedBnbData <- reducedBnbData[reducedBnbData[, "price"] >8,]

mu2 <- mean(reducedBnbData$price)
sd2 <- sd(reducedBnbData$price)

#Draw KDE
kde1 <- density(reducedBnbData$price , kernel = 'epanechnikov')
hist(reducedBnbData$price, col = "grey", breaks = seq(0, 1500, 50),
     freq = FALSE, main = "Histogram price", xlab = "price")
lines(kde1, col = "blue", lwd = 2)

#Sampling from different host may cause random effect based on hist.

sampleTest <- function(modelT){
  d <- reducedBnbData[sample(nrow(reducedBnbData), 1), ]
  price2 <- d[length(d)]
  d <- d[1:(length(d)-1)]
  price <- predict(modelT, d, type = 'response')
  difference <- price-price2
  return(c(d,difference))
}


#Find best linear model based on parameters filtered by AIC
#Poisson
fitMP <- glmulti(price~room_type+neighbourhood_group+city+minimum_nights+number_of_reviews+
                   last_review+reviews_per_month+availability_365, family=poisson,
                 data=reducedBnbData,level = 3 , method = "h", crit = aicc,chunks = 8)

#Gaussian
fitMG <- glmulti(price~room_type+neighbourhood_group+city+minimum_nights+number_of_reviews+
                   last_review+reviews_per_month+availability_365, family=gaussian(link='log'),
                 data=reducedBnbData,level = 3 , method = "h", crit = aicc,chunk = 1,chunks = 8)

#Gamma
fitMGa <- glmulti(price~room_type+neighbourhood_group+city+minimum_nights+number_of_reviews+
                    last_review+reviews_per_month+availability_365, family=Gamma,
                  data=reducedBnbData,level = 3 , method = "h", crit = aicc,chunk = 1,chunks = 8)

#Inverse guassian
fitMIG <- glmulti(price~room_type+neighbourhood_group+city+minimum_nights+number_of_reviews+
                    last_review+reviews_per_month+availability_365, family=inverse.gaussian('log'),
                  data=reducedBnbData,level = 3 , method = "h", crit = aicc,chunk = 1,chunks = 8)

summary(fitMP)
summary(fitMG)
summary(fitMGa)
summary(fitMIG)



#Fitting the best model according to output of AICC

fitGauBest <- glm2(price ~ 1 + room_type + neighbourhood_group + minimum_nights +            
                     number_of_reviews + last_review + reviews_per_month + availability_365,data=reducedBnbData,
                   family = gaussian(link = 'log'))

fitPoiBest <- glm2(price ~ 1 + room_type + neighbourhood_group + minimum_nights +           
                     number_of_reviews + last_review + reviews_per_month + availability_365
                   , family = poisson,data=reducedBnbData )
fitGamBest <- glm2(price ~ 1 + room_type + neighbourhood_group + minimum_nights +           
                     number_of_reviews + last_review + reviews_per_month + availability_365
                   , family = Gamma('log'),data=reducedBnbData )
fitIGBest <- glm2(price ~ 1 + room_type + neighbourhood_group + minimum_nights +           
                    number_of_reviews + last_review + reviews_per_month + availability_365
                  , family = inverse.gaussian('log'),data=reducedBnbData )
summary(fitGauBest)
summary(fitPoiBest)
summary(fitGamBest)
summary(fitIGBest)

#get true AIC
phi <- 1/3.27398

#Test if fit in GAM
testGam <- gam(price ~ room_type + neighbourhood_group + s(minimum_nights) +           
                 s(number_of_reviews) + s(last_review) + s(reviews_per_month) + s(availability_365),
               data = reducedBnbData,family = inverse.gaussian('log'))
summary(testGam)

plot(testGam,se = TRUE, shade = TRUE,main='TestGam')

prodGam <- gam(price ~ room_type + neighbourhood_group + s(minimum_nights) +           
                 number_of_reviews + s(last_review) + s(reviews_per_month) + s(availability_365),
               data = reducedBnbData,family = inverse.gaussian('log'))
summary(prodGam)

prodGam$coefficients

attach(mtcars)
par(mfrow=c(2,2))
plot(prodGam,se = TRUE, shade = TRUE,main='ProdGam')

prodGam2 <- gam(price ~ room_type + neighbourhood_group + s(minimum_nights) +           
                  number_of_reviews + s(last_review) + reviews_per_month + s(availability_365),
                data = reducedBnbData,family = inverse.gaussian('log'))
summary(prodGam2)

#Loess emitting different data
#Emit 
testLoess <- loess(price ~ minimum_nights +           
                     number_of_reviews + last_review + availability_365,
                   data = reducedBnbData)
summary(testLoess)
#Emit last_review
testLoess2 <- loess(price ~ minimum_nights + reviews_per_month+          
                      number_of_reviews  + availability_365,
                    data = reducedBnbData)
summary(testLoess2)
#Emit number_of_reviews
testLoess3 <- loess(price ~ minimum_nights + reviews_per_month+          
                      last_review  + availability_365,
                    data = reducedBnbData)
summary(testLoess3)
#Find best span by AIC and GCV
aicLoess <- aicplot(price ~ minimum_nights +           
                      number_of_reviews + last_review + availability_365,
                    alpha = seq(0.01,0.9,by=0.01),data = reducedBnbData)
gcvLoess <- gcvplot(price ~ minimum_nights +           
                      number_of_reviews + last_review + availability_365,
                    alpha = seq(0.01,0.9,by=0.01) ,data = reducedBnbData)
#Plot result
plot(gcvLoess$alpha , gcvLoess$values , main = "Smoothing parameter",
     xlab= "Smoothing Parameter", ylab = " GCV", pch = 20 , cex = 0.7)
plot(aicLoess$alpha , aicLoess$values , main = "Smoothing parameter",
     xlab= "Smoothing Parameter", ylab = " AIC", pch = 20 , cex = 0.7)

fitLoess <- loess(price ~ minimum_nights +           
                    number_of_reviews + last_review + availability_365,
                  data = reducedBnbData, span = 0.5)

#Residual plots
fv0 <- testLoess$fitted
resid0 <- testLoess$residuals
plot(fv0, resid0, pch = 20, xlab = "Fitted Values", ylab = "Residuals")

fv1 <- fitLoess$fitted
resid1 <- fitLoess$residuals
plot(fv1, resid1, pch = 20, xlab = "Fitted Values", ylab = "Residuals")

#Proposed random effect model
fitR1 <- glmer(price ~ 1 + room_type + neighbourhood_group + minimum_nights +            
                 number_of_reviews + last_review + reviews_per_month + availability_365 + (1|Host_id),
               data=reducedBnbData, family = gaussian(link = 'log'))
summary(fitR1)

fitR2 <- glmer(price ~ 1 + room_type + neighbourhood_group + minimum_nights +            
                 number_of_reviews + last_review + reviews_per_month + availability_365 + (0+room_type|Host_id),
               data=reducedBnbData, family = gaussian(link = 'log'))
summary(fitR2)
# indepdent 
fitR3 <- glmer(price ~ 1 + room_type + neighbourhood_group + minimum_nights +            
                 number_of_reviews + last_review + reviews_per_month + availability_365 + (1+room_type||Host_id),
               data=reducedBnbData, family = gaussian(link = 'log'))
summary(fitR3)
#Correlated
fitR4 <- glmer(price ~ 1 + room_type + neighbourhood_group + minimum_nights +            
                 number_of_reviews + last_review + reviews_per_month + availability_365 + (1+room_type|Host_id),
               data=reducedBnbData, family = gaussian(link = 'log'))
summary(fitR4)

fitR5 <- glmer(price ~ 1 + room_type + neighbourhood_group + minimum_nights +            
                 number_of_reviews + last_review + reviews_per_month + availability_365 + (1+minimum_nights||Host_id),
               data=reducedBnbData, family = gaussian(link = 'log'))
summary(fitR5)

fitR6 <- glmer(price ~ 1 + room_type + neighbourhood_group + minimum_nights +            
                 number_of_reviews + last_review + reviews_per_month + availability_365 + (1+minimum_nights|Host_id),
               data=reducedBnbData, family = gaussian(link = 'log'))
summary(fitR6)

