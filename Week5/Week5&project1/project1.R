#載入需要的函式codeload requred libraries
library(tm)
#library(tmcn)
library(jsonlite)

#從obama的臉書帳號再去搜尋他的id，因為歐巴馬在六月的貼文較少因此把時間從三月開始拉read data via Facebook Graph API for posts later than 2018-06-01
since <- "since=2018-03-05"
token <- "access_token=EAACEdEose0cBAGeYt4pqKW7ILQHfw08vssnoA23bkyoOuCQULVAOnNgknB8TNRnUObmhNIDANS0NFYXBZBjZC1ZC5bG6VXXKJeZAUWNLDemsEZCouJXEDO7qBlzC6RYOa1MltvekStNXGncZBa2Nd53Yf3lbzlGoKbjCRl1muGx3gjpcCa915kFQQq2K0dDicZD"
params <- paste(since, token, sep="&")
url <- "https://graph.facebook.com/v3.0/6815841748/feed?"
#組合網址
source <- paste(url, params, sep="")
#從facebook把資料拉回來，我們只要文章內文所以把文章內文拉出來
raw <- fromJSON(source)
msg <- as.character(raw$data$message)

#把拿回來的文章內容轉成dataframe，convert message to data frame
msg_df <- data.frame(as.list(msg), stringsAsFactors=FALSE)
colnames(msg_df) <- c(1:ncol(msg_df))
inspect(corpus[2])
#把剛才得到的dataframe讀進來，拿掉空白拿掉標點符號拿掉數字，convert to corpus and clean up based on multiple criteria
#表情符號是不能辨識的所以清掉
corpus <- Corpus(VectorSource(msg_df))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, function(word) {gsub("[^\x01-\x7F]", "", word)})


#清完了corpus就變成分好詞的語料庫，然後把這語料庫分好的詞做好矩陣陣列，每一篇的文章差出來是長什麼樣子
#perform TF-IDF computation，結果會顯示在result.csv
tdm = TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
mtx <- as.matrix(tdm)

write.csv(mtx, "result.csv")

