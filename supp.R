library(magrittr)
library(tidyverse)

# 모든 데이터 불러오기 
load("shiny/data/parking_data.RData")

# 인구수 데이터 합쳐주기 
pop_dat = resident_dat %>% left_join(working_dat) %>% 
  left_join(living_dat) %>% 
  left_join(codebook) %>% 
  rename(주거인구 = 총주민등록인구,
             직장인구 = 종사자수)

pop_bound_dat = dong_boundary %>% left_join(pop_dat, by = c('id' = '행정구역코드'))


# 주차장 면수 데이터
parking_dat = parking_area %>% dplyr::select(-분류, -운영세부) %>% 
  group_by(자치구, 행정동, 운영) %>% 
  mutate(rn = row_number()) %>% 
  ungroup %>%
  spread(운영, 면수) %>% 
  group_by(자치구, 행정동) %>% 
  summarise(공영 = sum(공영, na.rm = T), 
              민영 = sum(민영, na.rm = T),
              합계 = 공영 + 민영) %>% 
  ungroup() %>% 
  left_join(codebook)

parking_bound_dat = dong_boundary %>% left_join(parking_dat, by = c('id' = '행정구역코드'))

color_list = colorRampPalette(c("#f5e1bc", "#e55f63"))

save(list = c('pop_dat','pop_bound_dat', 'parking_dat', 'parking_bound_dat', 'color_list'), file="./shiny/data/shiny_data.RData")





