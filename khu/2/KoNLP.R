# https://www.java.com/ko/download/ 자바 설치
install.packages("rJava")
install.packages("multilinguer")
multilinguer::install_jdk() 
rJava::.jinit()
install.packages("KoNLP", 
    repos = c("https://forkonlp.r-universe.dev",
              "https://cloud.r-project.org"),
    INSTALL_opts = c("--no-multiarch")
  )
library(KoNLP)
install.packages("rJava")
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")
install.packages(“wordcloud”)
install.packages(“tm”)
