library(glm2)
library(nlme)
library(lme4)
library(glmulti)
library(ggplot2)
library(gam)
library(locfit)
library(MASS)

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






