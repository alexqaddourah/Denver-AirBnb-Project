calendar <- read.csv("C:\\Users\\newls\\Desktop\\R Summer\\denver_calendar.csv",stringsAsFactors=FALSE)
listings <- read.csv("C:\\Users\\newls\\Desktop\\R Summer\\denver_listings.csv",stringsAsFactors=FALSE)
reviews <- read.csv("C:\\Users\\newls\\Desktop\\R Summer\\denver_reviews.csv",stringsAsFactors=FALSE)
URL <- read.csv("C:\\Users\\newls\\Desktop\\R Summer\\URLs.csv",stringsAsFactors=FALSE)
head(URL)


install.packages('dplyr')
require('dplyr')

install.packages('leaflet')
require('leaflet')

#Copy for testing:

listings2 <- listings

#Convert all price column in listing2 to numeric:

listings2$price <- sub(pattern="$",replacement="",listings2$price,fixed=TRUE)
listings2$price <- as.numeric(listings2$price)
head(listings2$price)

listings2$weekly_price <- sub(pattern="$",replacement="",listings2$weekly_price,fixed=TRUE)
listings2$weekly_price <- as.numeric(listings2$weekly_price)
head(listings2$weekly_price)


#Filling in "NA's" with 0's for assignment.

listings2[is.na(listings2)] <- 0


#Attempt to fill empty weekly prices with price/night * 7

listings2$price_WEEK <- rep(0,times=nrow(listings2))

listings2[listings2$weekly_price == 0,'price_WEEK'] <- listings2[listings2$weekly_price == 0,'price']*7

listings2[listings2$weekly_price != 0,'price_WEEK'] <- listings2[listings2$weekly_price != 0,'weekly_price']

head(listings2$price_WEEK,100)#Test


#FINAL COLUMNS INFORMATION FOR NIGHT/WEEK/MONTH
head((listings2$price),25)
head((listings2$price_WEEK),25)

######################
#Using Aggregate to find Mean Prices per NB

zNIGHT <- aggregate(price~neighbourhood, FUN=mean, data=listings2)
zWEEK <- aggregate(price_WEEK~neighbourhood, FUN=mean, data=listings2)

zNIGHT$price <- round(zNIGHT$price)
head(zNIGHT)
zWEEK$price_WEEK <- round(zWEEK$price_WEEK)
head(zWEEK)

zNIGHT
zWEEK

########################
#Using aggregate to find average Rating per NB

zRATING <- aggregate(review_scores_rating~neighbourhood, FUN=mean, data=listings2)

zRATING$review_scores_rating <- round(zRATING$review_scores_rating)

zRATING

######################

zLINK <- listings2$url_listings

#Using aggregate to find count of property type per NB

#This gives me total # of properties available(could be useful?)

zTYPE <- aggregate(property_type~neighbourhood, FUN=length, data=yTYPE)
zTYPE

#####################

zDATA <- cbind(zRATING,zNIGHT,zWEEK,NBmeans,zTYPE,URL,txtPET)
zDATA <- subset(zDATA,select=-c(3,5,7,10,12))
head(zDATA)


lats <- c("latitude", "longitude", "neighbourhood")
newdata <- listings2[lats]
head(newdata)

aggregate(latitude ~ longitude, FUN=mean, data=newdata)
latitudeAverage <- aggregate(latitude ~ neighbourhood, FUN=mean, data=newdata)
longitudeAverage <- aggregate(longitude~ neighbourhood, FUN=mean, data=newdata)

NBmeans <- cbind(latitudeAverage, longitudeAverage) 

head(NBmeans)
NBmeans <- subset(NBmeans,select=-c(3))
head(NBmeans)

########################
library(leaflet)
library(shiny)

#########MAPPING##########

getColor <- function(zNIGHT) {
  sapply(zNIGHT$price, function(price) {
  if(price <= 100) {
    "green"
  } else if(price <= 200) {
    "orange"
  } else {
    "red"
  } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(zNIGHT)
)

for (i in 1:length(zDATA$review_scores_rating)){
	if(zDATA$review_scores_rating[i] >= 90){
		zDATA$review_scores_rating[i] <- "\u2b50?\u2b50?\u2b50?\u2b50?\u2b50?"
	}else if(zDATA$review_scores_rating[i] >= 80){
		zDATA$review_scores_rating[i] <-'\u2b50?\u2b50?\u2b50?\u2b50?'
	}else if(zDATA$review_scores_rating[i] >= 70){
		zDATA$review_scores_rating[i] <-'\u2b50?\u2b50?\u2b50?'
	}else if(zDATA$review_scores_rating[i] >= 60){
		zDATA$review_scores_rating[i] <-'\u2b50?\u2b50?'
	}else{
		zDATA$review_scores_rating[i] <-'\u2b50?'
	}
}

txtPET <- listings2[grepl("no pet|no dog|no dogs|no cats|no cat",listings2$house_rules),]

notpetfriendly <- aggregate(property_type~neighbourhood,data=txtPET,FUN=length)
zPet <- merge(zRATING, notpetfriendly, by='neighbourhood',all=TRUE)
zPet[is.na(zPet)] <- 0

for (i in 1:length(zPet $property_type)){
	if(zPet $property_type[i] == 0){
		zPet $property_type[i] <- "\U0001f436"
	}else{
		zPet $property_type[i] <- '\U0001f46b'
		}
}

leaflet(zNIGHT) %>% addTiles() %>%  
  addAwesomeMarkers(lng = zDATA$longitude, lat = zDATA$latitude,icon=icons,
             popup = paste("Neighbourhood:", zDATA$neighbourhood, "<br>",
                           "Avg Score:", zDATA$review_scores_rating, "<br>",
                           "Avg Price/Night:", zDATA$price, "<br>",
                           "Avg Price/Week:", zDATA$price_WEEK, "<br>",
				   "Number of Properties:",zDATA$property_type, "<br>",
				   "Suitable for:",zPet$property_type, "<br>",
                           "<a href='", URL$URL , "' target='_blank'>", "AirBnb Properties in this Neighbourhood:</a>"))



########################
#######Regression#######
########################


#Sloane Lake Home Prices vs Bedrooms/Bath in Sloane Lake.

lakeDATA <- subset(listings2, neighbourhood='Sloane Lake')


lakeDATA

summary(lm(price~bedrooms+bathrooms,data=lakeDATA))

#Price Estimation off beds and baths:
19.974 +  51.106(beds) + 24.784(baths)

PRICE = 19.974 +  51.106*(4) + 24.784*(2)
#$273.97/night


#Graph of above:
ggplot (data=lakeDATA,aes(y=price,x=bedrooms+bathrooms))+
	geom_point()+
	geom_smooth(method='lm')+
	theme_bw()




