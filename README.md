# Denver-AirBnb-Project
This project was started in the summer semester during my Masters in Business Analytics program at CU Boulder. It is intended to be an exploratory project into the basics of R.

## Data Cleaning
#### Let's start by uploading the necessary CSV's into R and install any necessary packages. For this project the only package used was leaflet.

``` r
listings <- read.csv("denver_listings.csv",stringsAsFactors=FALSE)
URL <- read.csv("URLs.csv",stringsAsFactors=FALSE)

install.packages('leaflet')
require('leaflet')
```
#### For starters, we need to do a little bit of cleaning with the price columns. The data set has information on both "price" which is what we assume is nightly vs "weekly price" which is what my group assumed was the price for renting this AirBNB for the entire week:

#### We need to take the price out of character form and change it into numeric so it is easy to find means and other stats. Let's remove the dollar sign and the quotes, and then change it into numeric for both price and weekly_price.
``` r
listings2$price <- sub(pattern="$",replacement="",listings2$price,fixed=TRUE)
listings2$price <- as.numeric(listings2$price)

listings2$weekly_price <- sub(pattern="$",replacement="",listings2$weekly_price,fixed=TRUE)
listings2$weekly_price <- as.numeric(listings2$weekly_price)
```

#### Filling in "NA's" with 0's.
``` r
listings2[is.na(listings2)] <- 0
```

#### Some of the weekly values in our data set do not have prices for the week. We could assign NA to them or throw them out, but lets try and give them a value using their corresponding price/night value. We can take that value and multiply to 7 to give us some estimation for the weekly price.
``` r
listings2$price_WEEK <- rep(0,times=nrow(listings2))

listings2[listings2$weekly_price == 0,'price_WEEK'] <- listings2[listings2$weekly_price == 0,'price']*7

listings2[listings2$weekly_price != 0,'price_WEEK'] <- listings2[listings2$weekly_price != 0,'weekly_price']
```

#### Now that we have cleaned both our price/night and price/week columns, let's see how they look!
``` r
head((listings2$price),25)
# [1]  56 140  61  42  70  95  76  65 110 111 300 179 120  75  70  60 125 411
# [19] 195  35  32  66 235 300  70
head((listings2$price_WEEK),25)
#  [1]  392  980  375  295  490  665  532  455  770  649 2100 1253  840  399
# [15]  455  364  900 2877 1365  900  350  950 1645 2100  450
```

## Variable Creation

####  Now we need to think about the map in leaflet that we ultimately want to create. We want to highlight each neighbourhood in the dataset and feed different information in like prices, ratings, etc. For each thing we want to feed in, we need to get an aggregate of that item for each neighbourhood. 

  * **Ultimately, we will feed all newly created variables (remember: all are per NB = 78 rows) into a final data set called zDATA. This is the dataset that will be fed into our leaflet mapping tool.**

  * **NOTE: NB = neighbourhood**
  
#### Using Aggregate to find Mean Prices per NB and Rounding into variables zNIGHT and zWEEK:

``` r
zNIGHT <- aggregate(price~neighbourhood, FUN=mean, data=listings2)
zWEEK <- aggregate(price_WEEK~neighbourhood, FUN=mean, data=listings2)

zNIGHT$price <- round(zNIGHT$price)
zWEEK$price_WEEK <- round(zWEEK$price_WEEK)

zNIGHT
zWEEK
```

#### Using Aggregate to find Mean Rating per NB and Rounding into variables zRATING:

``` r
zRATING <- aggregate(review_scores_rating~neighbourhood, FUN=mean, data=listings2)

zRATING$review_scores_rating <- round(zRATING$review_scores_rating)

zRATING
```

#### Using Aggregate to find Count of Total Properties Listed per NB and put into variable zTYPE:

``` r
zTYPE <- aggregate(property_type~neighbourhood, FUN=length, data=listings2)
zTYPE
```

#### The variable NBmeans required more work. To be loaded into leaflet, we needed a MEAN latitude and longitude for each NB. Pulled columns latitude, longitude, and neighbourhood into a temporary dataset named newdata. Then we can find the average latitude and longitude for each neighbourhood here. Then cbind it and put it into NBmeans.
``` r
lats <- c("latitude", "longitude", "neighbourhood")
newdata <- listings2[lats]
latitudeAverage <- aggregate(latitude ~ neighbourhood, FUN=mean, data=newdata)
longitudeAverage <- aggregate(longitude~ neighbourhood, FUN=mean, data=newdata)
NBmeans <- cbind(latitudeAverage, longitudeAverage) 
```

#### Let's see how NBmeans looks now:
``` r
head(NBmeans)
# neighbourhood latitude neighbourhood longitude
# 1   Athmar Park 39.70341   Athmar Park -105.0130
# 2       Auraria 39.74840       Auraria -105.0042
# 3         Baker 39.71921         Baker -104.9931
# 4        Barnum 39.71878        Barnum -105.0354
# 5   Barnum West 39.72107   Barnum West -105.0443
# 6   Bear Valley 39.66221   Bear Valley -105.0663
```

#### NBmeans looks good but we don't need neighbourhood to repeat more than once. Let's take out the 3rd column.
``` r
NBmeans <- subset(NBmeans,select=-c(3))
head(NBmeans)
#  neighbourhood latitude longitude
# 1   Athmar Park 39.70341 -105.0130
# 2       Auraria 39.74840 -105.0042
# 3         Baker 39.71921 -104.9931
# 4        Barnum 39.71878 -105.0354
# 5   Barnum West 39.72107 -105.0443
# 6   Bear Valley 39.66221 -105.0663
```


#### Because this was an exploratory project, we wanted to explore a little bit of text mining and add it into our zDATA data set for the leaflet map. In this code, we search for homes that have text stating "no dog(s) or no cat(s)". This text search isn't perfect - but it gives a good foundation to build on.

#### Let's put all of the homes that we find with these "no pets" rules into txtPET.
``` r
txtPET <- listings2[grepl("no pet|no dog|no dogs|no cats|no cat",listings2$house_rules),]
head(txtPET)
```
#### We can create another variable called "notpetfriendly" that is an aggregate of each NB and how many of these "no pet" properties are there.
```r
notpetfriendly <- aggregate(property_type~neighbourhood,data=txtPET,FUN=length)
```
#### Let's make this interesting and instead of a number for each NB, we can add an emoji of a Dog if pets are allowed, or an emoji of a couple, to suggest that no pets were mentioned in the text.
``` r
zPet <- merge(zRATING, notpetfriendly, by='neighbourhood',all=TRUE)
zPet[is.na(zPet)] <- 0

for (i in 1:length(zPet $property_type)){
	if(zPet $property_type[i] == 0){
		zPet $property_type[i] <- "\U0001f436"
	}else{
		zPet $property_type[i] <- '\U0001f46b'
		}
}
```
#### Our pet text mining information is now housed inside of zPet, which you'll notice was merged with neighbourhood and zRATING here. We will clean this up later in the code - and for each instance of zPet will house the emoji code.

## Mapping Set Up

#### Now we are ready to merge everything into zDATA. We can do a simple cbind here with all of the variables we want to include. Most of these are going to have their own NB column, so we can just keep the first instance of this. The subset command helps us to remove which repeat columns we don't want so we can get a cleaner dataset.

  * **NOTE: the variable URL is what was loaded at the beginning of this code. This was manually created to get a URL for each Neighbourhood vs. for each property like in the listings dataset.**
``` r
zDATA <- cbind(zRATING,zNIGHT,zWEEK,NBmeans,zTYPE,URL,zPet)
zDATA <- subset(zDATA,select=-c(3,5,7,10,12, 14, 15))
```

#### Great! Our zDATA is now ready to roll and be placed inside our leaflet mapping structure.

## Call Packages
``` r
library(leaflet)
library(shiny)
```

## Let's Map!

#### The leaflet mapping structure has a few components to it and I will do my best to explain what each does.

#### This function takes zNIGHT (our price/night variable) and tests it to see which threshold it falls into. If below $100, we can color our popup icon green. If between $101-200, the icon will be orange. If above this, it will be red.
``` r
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
```
#### The icons variable calls awesomeIcons which is built in, and the markerColor is received from our getColor function which we performed above.
``` r
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(zNIGHT)
)
```
#### The large for loop below does something similar to our pricing and the colors. Instead, it takes the average rating for each NB, and assigns a "star" emoji to it. If 90 or above, for example, it will paste 5 stars into the pop up menu.
``` r
for (i in 1:length(zDATA$review_scores_rating)){
	if(zDATA$review_scores_rating[i] >= 90){
		zDATA$review_scores_rating[i] <-'\u2b50?\u2b50?\u2b50?\u2b50?\u2b50?'
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
```
#### The code below is the official leaflet structure. We want to add tiles based on the zNIGHT variable (this is a judgement call, it can be weekly_price too with some alteration to the code above). The addAwesomeMarkers will require a longitude and a latitude, which has been taken from our zDATA. The icon variable = icons from the code above. To add a popup, which we have done, you'll need to do "popup = " and can put as many or as little things as possible. 

#### The format for the pop up, as you can see, is some sort of text label (Neighbourhood: "), the data where you want it to come from (zDATA$neighbourhood), and "<br>" at the end followed by commas for each variable you want to include. If you want to include a link, as I did, the write up is slightly different.
``` r
leaflet(zNIGHT) %>% addTiles() %>%  
  addAwesomeMarkers(lng = zDATA$longitude, lat = zDATA$latitude,icon=icons,
             popup = paste("Neighbourhood:", zDATA$neighbourhood, "<br>",
                           "Avg Score:", zDATA$review_scores_rating, "<br>",
                           "Avg Price/Night:", zDATA$price, "<br>",
                           "Avg Price/Week:", zDATA$price_WEEK, "<br>",
				   "Number of Properties:",zDATA$property_type, "<br>",
				   "Suitable for:",zDATA$property_type.1, "<br>",
                           "<a href='", zDATA$URL , "' target='_blank'>", "AirBnb Properties in this Neighbourhood:</a>"))
 ```
 
 ## Final Result
 
 ![Image of Map](https://user-images.githubusercontent.com/56977428/73803082-eca56300-477c-11ea-93e3-9a1e3a94901a.png)
 

                           
  
  
  
  
  
  
  
  
  
