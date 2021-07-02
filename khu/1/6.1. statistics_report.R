# 3. 어떤 음료회사가 신제품을 개발하여 기존의 자사제품보다 소비자 선호도가 좋을지를 사전에 파악하기 위해서 고객에게 제품을 시식하고 평가를 받은 데이터이다. 기존 제품 맛과 신제품에 대한 전체 집단의 평균이 동일한지에 대해 95% 신뢰수준 하에서 검정하고자 합니다.

#### 0. 패키지 로드 ####
pacman::p_load(tidyverse, infer, tidyr, tidy)

#### 1. 데이터 불러오기 ####
read_csv("score3.csv") -> raw3
?tidyr
#### 2. 데이터 살펴보기 ####
raw3 %>% 
  dplyr::glimpse()
Rows: 60
Columns: 2
# $ group <chr> "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "…
# $ score <dbl> 200, 235, 267, 300, 375, 500, 399, 423, 357, 244, 453, 4…

raw3 %>% 
  group_by(group) %>% 
  summarise(mean_score = mean(score))
# # A tibble: 2 x 2
# group mean_score
# <chr>      <dbl>
# 1 b           354.
# 2 c           399 


#### 3. 데이터 전처리 ####
# 3.1. 결측값 확인
raw3 %>% 
  summarise(group_NA = sum(is.na(group)),
            score_NA = sum(is.na(score)))
# # A tibble: 1 x 2
# group_NA score_NA
# <int>    <int>
#   1        0        0

#### 4. 데이터 분석 ####
  
raw3 %>% 
  t_test(formula = score ~ group, # 그룹별 점수에 대한 t-test
         order = c("b", "c"),
         conf_level = 0.95, # 신뢰도 95% 수준에서 검정
         alternative = "two-sided") # 평균 동일한지이기 때문에 양측검정

# # A tibble: 1 x 6
# statistic  t_df p_value alternative lower_ci upper_ci
# <dbl> <dbl>   <dbl> <chr>          <dbl>    <dbl>
# 1     -1.87  26.5  0.0730 two.sided      -93.9     4.47

#### 4.1. 데이터 시각화 ####
raw3 %>% 
  ggplot(aes(x = group, y = score)) +
  geom_boxplot()

raw3_observed_statistic <- raw3 %>%
  specify(score ~ group) %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_observed_statistic

raw3_null_distribution_2_sample_permute <- raw3 %>%
  specify(score ~ group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_null_distribution_2_sample_theoretical <- raw3 %>%
  specify(score ~ group) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_null_distribution_2_sample_permute %>%
  visualize() + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_null_distribution_2_sample_theoretical %>%
  visualize(method = "theoretical") + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_null_distribution_2_sample_permute %>%
  visualize(method = "both") + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_p_value_2_sample <- raw3_null_distribution_2_sample_permute %>%
  get_p_value(obs_stat = raw3_observed_statistic,
              direction = "two-sided")

raw3_p_value_2_sample

#### 5. 분석결과 해석 ####



# 4. 자동차 회사는 고객만족도를 조사하였다. 각 회사의 고객만족도에 차이가 있는지 95% 신뢰수준 하에서 검정하고자 한다.


#### 0. 패키지 로드 ####
pacman::p_load(tidyverse, infer, tidyr)

#### 1. 데이터 불러오기 ####
read_csv("score3.csv") -> raw3
?tidyr
#### 2. 데이터 살펴보기 ####
raw3 %>% 
  dplyr::glimpse()
# Rows: 60
# Columns: 2
# $ group <chr> "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "…
# $ score <dbl> 200, 235, 267, 300, 375, 500, 399, 423, 357, 244, 453, 4…

raw3 %>% 
  group_by(group) %>% 
  summarise(mean_score = mean(score))
# # A tibble: 2 x 2
# group mean_score
# <chr>      <dbl>
# 1 b           354.
# 2 c           399 


#### 3. 데이터 전처리 ####
# 3.1. 결측값 확인
raw3 %>% 
  summarise(group_NA = sum(is.na(group)),
            score_NA = sum(is.na(score)))
# # A tibble: 1 x 2
# group_NA score_NA
# <int>    <int>
#   1        0        0

# 3.2. 이상치 확인
raw3 %>% 
  group_by(group) %>% 
  identify_outliers(score)
# # A tibble: 4 x 4
# group score is.outlier is.extreme
# <chr> <dbl> <lgl>      <lgl>     
# 1 c       278 TRUE       FALSE     
# 2 c       279 TRUE       FALSE     
# 3 c       282 TRUE       FALSE     
# 4 c       181 TRUE       TRUE 

#### 4. 데이터 분석 ####

raw3 %>% 
  t_test(formula = score ~ group, # 그룹별 점수에 대한 t-test
         order = c("b", "c"),
         conf_level = 0.95, # 신뢰도 95% 수준에서 검정
         alternative = "two-sided") # 평균 동일한지이기 때문에 양측검정

# # A tibble: 1 x 6
# statistic  t_df p_value alternative lower_ci upper_ci
# <dbl> <dbl>   <dbl> <chr>          <dbl>    <dbl>
# 1     -1.87  26.5  0.0730 two.sided      -93.9     4.47

#### 4.1. 데이터 시각화 ####
raw3 %>% 
  ggplot(aes(x = group, y = score)) +
  geom_boxplot()


raw3_observed_statistic <- raw3 %>%
  specify(score ~ group) %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_observed_statistic

raw3_null_distribution_2_sample_permute <- raw3 %>%
  specify(score ~ group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_null_distribution_2_sample_theoretical <- raw3 %>%
  specify(score ~ group) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "t", order = c("b", "c"))

raw3_null_distribution_2_sample_permute %>%
  visualize() + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_null_distribution_2_sample_theoretical %>%
  visualize(method = "theoretical") + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_null_distribution_2_sample_permute %>%
  visualize(method = "both") + 
  shade_p_value(raw3_observed_statistic,
                direction = "two-sided")

raw3_p_value_2_sample <- raw3_null_distribution_2_sample_permute %>%
  get_p_value(obs_stat = raw3_observed_statistic,
              direction = "two-sided")

raw3_p_value_2_sample

#### 5. 분석결과 해석 ####



# 0. 패키지 로드 
## install.package("rstatix")
library(rstatix)

# 1. 데이터 불러오기
raw4 <- read_csv("score4.csv")

# 2. 데이터 살펴보기 
raw4 %>% 
  glimpse()
raw4 %>% 
  group_by(group) %>% 
  summarise(mean_score = mean(score))
# # A tibble: 3 x 2
# group mean_score
# <chr>      <dbl>
# 1 P           70.6
# 2 Q           53.6
# 3 R           51.5

boxplot(score ~ group, raw4, col=rainbow(3))

aov(score ~ group, data=raw4) %>% 
  tidy()
# # A tibble: 2 x 6
# term         df sumsq meansq statistic p.value
# <chr>     <dbl> <dbl>  <dbl>     <dbl>   <dbl>
# 1 group         2 2099.  1049.      3.65  0.0401
# 2 Residuals    26 7477.   288.     NA    NA   

# 등분산성 테스트
bartlett.test(score ~ group, data=raw4) %>% 
  tidy()
# A tibble: 1 x 4
# statistic p.value parameter method                                   
# <dbl>   <dbl>     <dbl> <chr>                                    
#   1      12.3 0.00216         2 Bartlett test of homogeneity of variances


