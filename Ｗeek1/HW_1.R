### hw_1_question

########################################################### Task 1

# 查看內建資料集: 鳶尾花(iris)資料集
iris

# 使用dim(), 回傳iris的列數與欄數
dim(iris)

# 使用head() 回傳iris的前六列
head(iris)[6,]

# 使用tail() 回傳iris的後六列
tail(iris)[6,]
# 使用str() 
str(iris)

# 使用summary() 查看iris敘述性統計、類別型資料概述。
summary(iris)

########################################################### Task 2

# 使用for loop 印出九九乘法表
# Ex: (1x1=1 1x2=2...1x9=9 ~ 9x1=9 9x2=18... 9x9=81)
for (i in c(1:9)){for (j in c(1:9)){print (paste(i,"x",j,"=",i*j))}}
########################################################### Task 3

# 使用sample(), 產出10個介於10~100的整數，並存在變數 nums
nums<-sample(10:100,size=10)

# 查看nums
str(nums)  
# 1.使用for loop 以及 if-else，印出大於50的偶數，並提示("偶數且大於50": 數字value)
# 2.特別規則：若數字為66，則提示("太66666666666了")並中止迴圈。
for (num in nums) {
  if (num == 66) {
    print("太66666666666了")
    break
  } else if ((num %% 2 == 0) && (num > 50)) {
    print(paste("偶數且大於50：", num))
  }
}

  
  
  
  



########################################################### Task 4

# 請寫一段程式碼，能判斷輸入之西元年分 year 是否為閏年

year <- 2100
ifelse(year %% 4 == 0 && year %% 100 != 0 || year %% 400 == 0, "是閏年", "不是閏年")







########################################################### Task 5

# 猜數字遊戲
# 1. 請寫一個由電腦隨機產生不同數字的四位數(1A2B遊戲)
# 2. 玩家可重覆猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

ans <- sample(0:9, 4)
guess.count <- 0

repeat {
  print("Please input 4 non-repetitive numbers.[integers between 0 to 9, aka c(0:9)")
  guess <- scan(nmax = 4)
  
  a <- b <- 0
  
  if (!any(duplicated(guess))){
    guess.count <- guess.count + 1
    
    for (i in 1:4) {
      if (guess[i] == ans[i]) {
        a <- a + 1
      } else {
        for (j in 1:4) {
          if (guess[i] == ans[j]) {
            b <- b + 1
          }
        }
      }
    }
    
    cat("==== Your guess :", guess, ", Match : ", a, "A", b, "B\n")
    
    if (a == 4) {
      cat("==== CORRECT! You guess for", guess.count, "times")
      break
    }
    
  } else {
    cat("==== Input Error: Please input 4 <non-repetitive> numbers.\n")
  }
}






