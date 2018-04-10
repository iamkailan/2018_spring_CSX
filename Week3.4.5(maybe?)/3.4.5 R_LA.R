#親愛的老師和好心助教們，我消化菜單消化得太慢了（基於這龜速我在研究看懂它前決定先會改成我看的懂得當作業交上來）
#這是我學期修新聞所新來的瑞士老師的課抓Twitter上用的code，我有看這堂課給的菜單再搭配著看
#也是上週我主要研究的部分，學習改成自己的看懂再去抓（所有作業的集中週XD)
#我是抓「＃Metoo」這個tweets然後取8000個
#再改geocode分別設在美國三個城市看不同地方和這個tweets相關的tweets有什麼不同
#再改成以那個地點為中心100miles/60 miles去搜
#再畫出我的ggplot和word cloud
#然後改word cloud的大小和樣式這樣
#最後我觀察出在LA,Chicago跟NY跑出來的結果都不同
#如附上的文字雲 在LA最相關的是ricebunny推測可能和當地的人口結構有關（ricebunny是米兔諧音metoo)
#在NY則最多為在奧斯卡典禮上公開發表其演說的名主持人jimmykimmel和前陣子被指控的明星billcosby
#其背後可能故事和意義再希望能從分析資料再找出來
#(我一開始也有不限地區或或以台北為中心去搜，但因為跑出很多無法顯示的文字我猜是韓文
#加上我還不知道怎麼顯示所有語言...><所以就移去美國)
#不知道這樣可不可以，雖然沒有研究完老師的菜單前面的作業除了第一週的有懂
#第二週只能打安全牌...
#是真的想學會的所以就給老師看我走到哪裡這樣


#Recap: Download data for today ####
# always load the package
library(rtweet)
# you need the Twitter token in the environment - if not there, load it now

# we download some tweets for today - choose your keyword or hashtag:
tweets <- search_tweets("#Metoo", n = 12000, type = "recent", include_rts = TRUE, token = twitter_token)

head(tweets)

tail(tweets)

# you can also go by geo location: example tweets from Taipei (10 miles radius)
# check here for geo locations: https://www.findlatitudeandlongitude.com/?loc=taipei+taiwan
# leave the search query empty "" if you want all tweets from the geo location
tweets <- search_tweets("#Metoo", n =8000, type = "recent", include_rts = TRUE, token = twitter_token, geocode = "34.0485666141445,-118.22776315625003,60mi")
# as you can see, only few have a geocode
# however, if the account has the location -> will also be included
user <- users_data(tweets)
# we only want unique user:
duplicated(user$user_id)[1:100]
# returns TRUE, if element is duplicated
# so we want non-duplicated -> we use the !
# then we get TRUE for all non-duplictaed
user <- user[ !duplicated(user$user_id) , ]

# Repetition: Which language?

# table to DF
language_df <- as.data.frame(table(user$account_lang), stringsAsFactors = F)
language_df <- language_df[order(language_df$Freq, decreasing = T), ]
language_top <- language_df[1:20,]

# we have to load the package
library(ggplot2)

# y_step steps on the y-axis
# we use seq() to create a vector with numbers that indicate tick positions
y_max <- max(language_top$Freq) + 10 # the 10 as margin
y_step <- 50 # every 10 steps a tick
seq(from= 0, to = y_max, by = y_step)

ggplot()+geom_bar(data=language_top, aes(x = reorder(Var1, Freq), y= Freq), stat="identity") +
  theme_bw() + xlab(NULL) + theme(axis.text.x=element_text(angle=90,size=14, hjust = 1, vjust=0.5)) + 
  scale_y_continuous(limits = c(0,y_max), expand=c(0,0), breaks = seq(from= 0, to = y_max, by = y_step))

# Scatterplot ####
# We use the user object

# structure of the data
str(user)
max(user$followers_count)
min(user$followers_count)
# let's use the function which()
user[which(max(user$followers_count) == user$followers_count), ]
# or you can also directly check (not recommended)
user[max(user$followers_count) == user$followers_count, ]

## start ggplot with (data, aes(x=name of column, y=name of column)) 
ggplot(user, aes(x = followers_count, y = friends_count)) +  # define variables
  geom_point()      # we add for the scatterplot geometric element point

## just as a test with geom_line£()
ggplot(user, aes(x = followers_count, y = friends_count)) +  # define variables
  geom_line()      # with lines it looks strange

# we only focus on ussers with less than 10k follower and followees
# logical Variable
str(user$followers_count)
user$followers_count < 20000
user$followers_count < 200
# subest
userslimited_df <- user[user$followers_count <10000 & user$friends_count <10000, ]
nrow(userslimited_df) # nrow zeigt an, wieviele Rows ein DF hat

# second try
ggplot(userslimited_df, aes(x=friends_count, y=followers_count)) +  # Variablen definieren
  geom_point()      # für Scatterplot setzen wir in diesem Schritt Punkte

# now with regression line
ggplot(userslimited_df, aes(x=friends_count, y=followers_count)) +
  geom_point() +   
  geom_smooth(method=lm)   # Linear regression line (with 95% confidence region)

# use a third variable ("statuses_count") as contrast
ggplot(userslimited_df, aes(x=friends_count, y=followers_count, color=statuses_count)) +
  geom_point() +    # 
  geom_smooth(method=lm)   # Linear regression line (mit 95% confidence region)

# change size of points
ggplot(userslimited_df, aes(x=friends_count, y=followers_count, color=statuses_count)) +
  geom_point(size=2) +    # different size
  geom_smooth(method=lm)   # Linear regression line (mit 95% confidence region)


# change style
ggplot(userslimited_df, aes(x=friends_count, y=followers_count, color=statuses_count)) +
  geom_point(size=2) +    # different size
  geom_smooth(method=lm) +  # Linear regression line (mit 95% confidence region)
  theme_bw()              # theme_bw


# we don't want margins - use expand for x- and y-axis
ggplot(userslimited_df, aes(x=friends_count, y=followers_count, color=statuses_count)) +
  geom_point(size=2) +    # different size
  geom_smooth(method=lm) +  # Linear regression line (mit 95% confidence region)
  theme_bw() +           # theme_bw
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand = c(0,0)) 
# change y- and x-axis with scale_y/x_continious

# let's calculate the regression model
summary(lm(followers_count ~ friends_count, data=userslimited_df))

# usually we save after each step into an object
# in the first step we save the general plot
p1 <- ggplot(userslimited_df, aes(x=friends_count, y=followers_count, color=statuses_count)) +
  geom_point(size=2) +    # different size
  geom_smooth(method=lm) +  # Linear regression line (mit 95% confidence region)
  theme_bw()          # theme_bw
# then you can anytime check the visualization
p1
# again, change in a second step the margins
# additionally we want unique labels for the y-axis
# we can use specific strings
# add additional "aguments" to the existing visualization with +
p2 <- p1 + scale_y_continuous(expand=c(0,0),
                              breaks=c(2500,5000,7500),labels=c("famous", "star", "superstar")) + # add labels with breaks
  scale_x_continuous(expand = c(0,0)) 
# let's check:
p2 

# change the axis titles witht ylab() and xlab()
p3 <- p2 + xlab("Followee") + ylab("Follower")
p3  

# let's add one more element - age of account.
# th larger, the younger the account (POSIXct - seconds since 1970)
# Class "POSIXct" represents the (signed) number of seconds since the beginning of 1970 (in the UTC time zone) as a numeric vector.
p4 <- p3 + geom_point(aes(size=as.numeric(account_created_at)))
p4


# cope with the rate limit ####

# I created a simple function we can use
# run the next few lines of code, then you have a new function (can see it in the environment)

download_tweets <- function(searchstring, n_search, max_id=NULL, token, temp_save=FALSE, ...) {
  # first create a list for the tweets
  tweets <- NULL
  
  # define the counter
  i = 1
  
  # run the loop until the condition is reached (n runs)
  while(i < (n_search + 1) ) {
    tweets_temp <- search_tweets(searchstring, n=6000, type="recent", max_id=max_id, token=token, retryonratelimit=T, verbose=T, ...)
    
    # if no more tweets, it will be an empty data frame and have 0 row
    # if nrow(tweets[[i]]) == 0 TRUE return the list with the already downloaded tweets and end function
    if( nrow(tweets_temp) == 0 ) {
      message("no more new tweets")
      # return ends function and returns the tweets list
      return(tweets)
    }
    # get max id from last search and use for next search
    max_id <- tweets_temp[nrow(tweets_temp), ]$status_id
    # merge new with old tweets
    tweets <- rbind(tweets, tweets_temp)
    
    # if we want temporary save
    if( temp_save ) {
      saveRDS(tweets, file="temp_tweets.RDS")
    }
    
    # show message in console
    message(paste("Total ", nrow(tweets), " tweets ", tweets_temp[nrow(tweets_temp), ]$created_at))
    # increase counter
    i <- i + 1
  }
  # return the list
  tweets
}

# nor we can use the new function
# if you use max_id = NULL all tweets from starting from now will be downloaded
# n means number of downloads - 1 download includes 6000 tweets
# n = 3 means 18000 tweets

tweetspinguo_all <- download_tweets("tw.appledaily.com", n = 3, max_id = NULL , token = twitter_token)
# problem: we have to download the user information seperately
str( unique(tweetspinguo_all$user_id) )

# you can download up to 90k users every 15 min.
pinguo_user <- lookup_users(users = unique(tweetspinguo_all$user_id), token = twitter_token)

# timeseries ####
# in the past plottint the over time was a tedious process
# rtweet now has a specific function for this
## plot time series of tweets
# you can also use mins or days
# we use 15 mins
ts_plot(tweetspinguo_all, "1 hours") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses with URL from 蘋果日報",
    subtitle = "Twitter status (tweet) counts aggregated using 15-minutes intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) 

# we can add some information
p_ts <- ts_plot(tweetspinguo_all, "1 hours") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses with URL from 蘋果日報",
    subtitle = "Twitter status (tweet) counts aggregated using 15-minutes intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) 
p_ts

# let's add some better breaks and rotate the labels
p_ts +  scale_x_datetime(date_breaks = "12 hours") + 
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust=0.5)) 

# slightly different style
p_ts +  scale_x_datetime(date_breaks = "12 hours") + 
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust=0.5)) + geom_point(aes(x=time, y=n))

# you can also manually define the date labels
# then you have to use the argument date_labels
?strptime
# let's try only the date - fixed format with %D

p_ts +  scale_x_datetime(date_breaks = "12 hours", date_labels = "%D") + 
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust=0.5)) + geom_point(aes(x=time, y=n))

# check the media URL
# here we need unlist because some tweets have more than one URL
# we have a list in this row (it has something to do with the tibble-class)
class(tweetspinguo_all$urls_expanded_url)
# that is why we use unlist - "flattens" the list - creates a vector
pinguo_urls <- as.data.frame(table( unlist(tweetspinguo_all$urls_expanded_url )), stringsAsFactors=F)
pinguo_urls <- pinguo_urls[order(pinguo_urls$Freq, decreasing = T), ]
browseURL(pinguo_urls[1, ]$Var1)

# different queries ####

# if you want to use a more complicated search with different OR
# example with OR: we need quotes inside of a string - have to use excape character \
""Climate change" OR climatechange OR "global warming" OR globalwarming OR "greenhouse effect""
# vs
"\"Climate change\" OR climatechange OR \"global warming\" OR globalwarming OR \"greenhouse effect\""

# this one you can use as a query
# more examples and explanations: https://developer.twitter.com/en/docs/tweets/search/guides/standard-operators
# wordclouds and hashtags ####
install.packages("stringr")
library(stringr)

### Extract Hashtags with simple Regex pattern ####

library(stringr)
tweetshashtag_df <- tweets[,c("text","screen_name")]

# simple regex pattern: Hashtag followed by numbers or characters
# short form is \\w, the + says: at least one or more characters
hashtags <- str_extract_all(tweetshashtag_df$text, "#\\w+")
str(hashtags)
# we have now a list - need to unlist an create a vector: unlist()
hashtags <- unlist(hashtags)
# now we only want lower case as #Trump and #trump are the same
tolower(c("Trump" , "TRUMP"))
hashtags <- tolower(hashtags)
# as usual table...
hashtags_df <- table(hashtags)
# to data frame
hashtags_df <- as.data.frame(hashtags_df, stringsAsFactors = F)
# order
hashtags_df <- hashtags_df[order(hashtags_df[,2], decreasing = T), ]

# create a wordcloud 
install.packages("wordcloud")
library(wordcloud)
?wordcloud
head(hashtags_df)
wordcloud(words=hashtags_df[1:50,1], freq=hashtags_df[1:50,2], random.order=F) 

# We don't want the first place
wordcloud(hashtags_df[2:50,1], hashtags_df[2:50,2], random.order=F) 

#we can check for unique users
# "The number of times a hashtag appears in a tweet can sometimes be misleading, because a hashtag might be only
# used by a few bots (machine controlled accounts) that constantly tweet." A. Rauchfleisch
hashtags_df[ ,3] <- NA
colnames(hashtags_df)[3] <- "unique_users"

# for every hashtag we check how many unique users have used that hashtag
# If you want to use, cite: http://www.climatematters.hamburg/2015/12/two-weeks-on-twitter-cop21-smoking-heads-and-tweets-from-outer-space/

for(i in 1:nrow(hashtags_df)) {
  # filter by hashtag and get user names
  x <- tweetshashtag_df[grepl(paste(hashtags_df[i, 1], "\\b", sep=""), tweetshashtag_df$text, ignore.case = T), ]$screen_name
  # count how many unique users have used a hashtag
  hashtags_df[i,3] <- length(unique(x))
  # show progress in console
  if( i%%20 == 0 ) { 
    cat(paste(i, "...", sep = ""))
  }
}


hashtags_unique_df <- hashtags_df[order(hashtags_df[,3], decreasing = T),]          

# create wordcloud
wordcloud(hashtags_unique_df[1:50,1], hashtags_unique_df[1:50,3], random.order=F)

# again, ignore the first one
wordcloud(hashtags_unique_df[2:50,1], hashtags_unique_df[2:50,3], random.order=F)

# let's check the ratio:
hashtags_unique_df$ratio_uniqueuser <- hashtags_unique_df$unique_users / hashtags_unique_df$Freq

# how to export high res plots?
# open a connection to file
jpeg("plot_wordcloud_uniqueusers.jpeg", width = 7, height = 7, units = 'in', res = 300)
# run the plot
wordcloud(hashtags_unique_df[2:50,1], hashtags_unique_df[2:50,3], random.order=F)
# if you have already saved the plot in a object, just call the object here

# close the connections
dev.off()

installed.packages("RColorBrewer")
library(RColorBrewer)
# with this library we can get nice colors
pal <- brewer.pal(9,"Blues")
pal <- pal[9:1]
# we can also use a dark  background
jpeg("plot_wordcloud_uniqueusers_dark.jpeg", width = 6, height = 6, units = 'in', res = 300)
par(bg = 'black')
wordcloud(hashtags_unique_df[2:50,1], hashtags_unique_df[2:50,3], random.order=F,
          colors =  pal) 
dev.off()


