if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, magrittr,
  tidytext, KoNLP
  )
temp.text <- 
  tibble(lineid = 1:3,
         text=c("한국(韓國), 조선(朝鮮), 또는 코리아(Korea)는 동아시아에 위치한 지역 또는 헌법상의 국가로, 현대사에서는 한반도의 대한민국과 조선민주주의인민공화국을 통틀어 이르는 말이다.",
         "근현대사에서 한국은 고종이 수립한 대한제국을 일컫는 말이었다.",
         "넓은 의미로 한국은 고조선 이후 한반도에서 설립된 여러 한민족의 국가를 통칭하는 말이다. 한국의 역사를 한국사라고 한다."))
temp.text

temp.text %>%
  group_by(lineid) %>%
  mutate(
    text_matched = SimplePos09(text) %>%
      unlist() %>%
      paste(collapse = ' ') %>%
      str_extract_all(regex('[^\\s]+/N')) %>% #POS에서 명사부만 뽑겠습니다.
      unlist() %>%
      paste(collapse = ' ') %>%
      str_remove_all('/N') %>% #태그는 지우지요.
      str_remove_all(stopping_ko_end)
  ) %>%
  ungroup() %>%
  unnest_tokens(word, text_matched) %>%
  select(-text) %>%
  anti_join(stopping_ko)
