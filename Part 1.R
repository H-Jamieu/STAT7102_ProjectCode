####### 2. data preprocessing #######

Airbnb <-read.csv(file="AB_US_2020.csv", header=TRUE, sep=",",
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
    else if (x < median0-n*mad0)
    { 
      tt[i] <- median0-n*mad0
      }
   return(tt) 
  }
}

Custom_transform <- function(tt, a, b){
  len <- length(tt)
  for(i in 1:len){
    x <- tt[i] 
    if(x > a){ tt[i] <- a } 
    else if (x < b){ 
      tt[i] <- b 
      }
  } 
  return(tt) 
}

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
# 2.4 save data will be used in next sessions
write.table(Airbnb,"AB_US_2020_V2.csv", row.names=FALSE, col.names=TRUE, sep=",")
####### 3. data pre analysising ####### 
# 3.1 correlation heat map
library(corrplot)
df <- cor(Airbnb[c(col0, 'price')])
corrplot(corr=df, method = "color", type = "upper", 
         tl.srt=45, number.cex= 7/ncol(df), addCoef.col="black")
# 3.2 histogram and KDE
tt<- sort(Airbnb[Airbnb$price<=1000,"price"])
kde1 <- density(tt, bw = "nrd", kernel =  "gaussian") 
hist(tt, col = "grey", freq = FALSE, main='Histogram of Airbnb$price', xlab = 'Airbnb$price')
lines(kde1, col = "blue", lwd = 2)  
