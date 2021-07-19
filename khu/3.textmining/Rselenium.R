if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  httr, urltools, RSelenium,
  rvest, jsonlite
  )

library(RSelenium)

library(RSelenium)
library(httr)
library(rvest)
library(RSelenium)
library(googlesheets4)
library(RSelenium)
remDr<-remoteDriver(port=4444L, browserName = "chrome")
remDr$open(silent = TRUE)#Open the browser

## RSelenium활용 서버 연결

remD <- remoteDriver(remoteServerAddr = 'localhost', 
                     port = 4444L, # 포트번호 입력 
                     browserName = "chrome") 
remD$open() #서버에 연결


title_you <- "인공지능"     # 검색어 객체화



remD$navigate(paste0("https://www.youtube.com/results?search_query=", title_you)) #이 홈페이지로 이동 
# paste0() 입력값을 붙여주는 함수


## 영상제목 크롤링 - chrome 웹스토어 활용 ##

html <- remD$getPageSource()[[1]] 
html <- read_html(html) #페이지의 소스 읽어오기 




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


if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  httr, urltools, RSelenium,
  rvest, jsonlite
  )
coronaurl <- GET(
  url = "http://ncov.mohw.go.kr",
  path = "bdBoardList_Real.do",
  query = list("brdId" = "1",
               "brdGubun" = "14")
)
print(x = coronaurl)
class(x = coronaurl)
global <- coronaurl %>% 
  read_html() %>% 
  html_node(css = "div.data_table.mgt16 > table.num") %>% 
  html_table(fill = T)
colnames(x = global) <- c("대륙", "국가", "death")
global %>% 
  select(death) %>% 
  mutate(death = death %>% 
           str_remove_all(pattern = "[\r\n\t,]+") %>% 
           str_split(pattern = "명\\(사망 ", n = 2, simplify = T) %>% 
           unlist()) %>% 
  select(death) %>% 
  mutate(감염 = death[,1] %>% 
              str_remove(pattern = "명") %>% 
              as.double()) %>% 
  mutate(사망 = death[,2] %>% 
              str_remove(pattern = "\\)") %>% 
              as.double()) %>% 
  bind_cols(global %>% 
              select(대륙, 국가)) %>% 
  select(대륙, 국가, 감염, 사망)

global
