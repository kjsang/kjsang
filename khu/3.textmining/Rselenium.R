if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  httr, urltools, RSelenium,
  rvest, jsonlite,
  googlesheets4
  )


library(RSelenium)
rd <- rsDriver(port = 4444L, browser = c("firefox")) # 포트 이름은 그냥 무작위로 정했는데 맞는건지는 모르겠습니다 
remDr <- rd[["client"]]
remDr$navigate("http://www.bbc.com")
remDr$close()


title_you <- "인공지능"     # 검색어 객체화



remD$navigate(paste0("https://www.youtube.com/results?search_query=", title_you)) #이 홈페이지로 이동 
# paste0() 입력값을 붙여주는 함수


## 영상제목 크롤링 - chrome 웹스토어 활용 ##

html <- remD$getPageSource()[[1]] 
html <- read_html(html) #페이지의 소스 읽어오기 

library(RSelenium)
remDr<-remoteDriver(port=4444L, browserName = "chrome")
 remDr$open(silent = TRUE)#Open the browser


# selector gadget를 크롬 웹스토어 검색-활용

youtube_title <- html %>% html_nodes("#video-title") %>% html_text() #선택된 노드를 텍스트 화

youtube_title[1:10] #1~10개가져오기 


# 불필요한 문구 삭제
youtube_title <- gsub("\n", "", youtube_title) 
youtube_title <- trimws(youtube_title)  # 공백제거
youtube_title


# 텍스트파일로 데이터 저장

write.table(youtube_title, 
            file = "F:\\Rtextmining\\2021 bigdata camp\\youtube 검색\\youtube_title.txt",
            sep=",",
            row.names=FALSE,
            quote = FALSE)

View(youtube_title)

