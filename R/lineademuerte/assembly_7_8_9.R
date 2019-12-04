library(tidyverse)
setwd( "~/tmp/work/probas/")
profits_to_compare = data.table(
  month = c(201806, 201807, 201808, 201809, 201810, 201811, 201812, 201901, 
            201902, 201903, 201904),
  profit = c(12505500, 10298000, 11132500, 11939500, 9620500, 10382500, 
             11073000, 9479000, 10528000, 10423000, 9122500),
  positives = c(1294, 1201, 1222, 1197, 1010, 1070, 1103, 985, 1085, 1119, 918)
)

## Graba un archivo con las ganacias por mes
calculate_profits = function(the_dataset, the_cutoff) {
  the_profit_dataset = the_dataset %>%
    filter(prob_positivo > the_cutoff) %>%
    mutate(winning = ifelse(clase01 == 1, 19500, -500)) %>%
    select(foto_mes, winning) %>%
    group_by(foto_mes) %>%
    summarise_all(sum) %>%
    mutate(cutoff = the_cutoff)
  
  write_tsv(x = the_profit_dataset, path = file_profits, append = TRUE)
}

## 1001
prob_1001_201806 = read_tsv("lightgbm_dead_line_modelitos17105_201806_probabilidades.txt") %>% mutate(foto_mes = 201806)
prob_1001_201807 = read_tsv("lightgbm_dead_line_modelitos17105_201807_probabilidades.txt") %>% mutate(foto_mes = 201807)
prob_1001_201808 = read_tsv("lightgbm_dead_line_modelitos17105_201808_probabilidades.txt") %>% mutate(foto_mes = 201808)
prob_1001_201809 = read_tsv("lightgbm_dead_line_modelitos17105_201809_probabilidades.txt") %>% mutate(foto_mes = 201809)
prob_1001_201810 = read_tsv("lightgbm_dead_line_modelitos17105_201810_probabilidades.txt") %>% mutate(foto_mes = 201810)
prob_1001_201811 = read_tsv("lightgbm_dead_line_modelitos17105_201811_probabilidades.txt") %>% mutate(foto_mes = 201811)
prob_1001_201812 = read_tsv("lightgbm_dead_line_modelitos17105_201812_probabilidades.txt") %>% mutate(foto_mes = 201812)
prob_1001_201901 = read_tsv("lightgbm_dead_line_modelitos17105_201901_probabilidades.txt") %>% mutate(foto_mes = 201901)
prob_1001_201902 = read_tsv("lightgbm_dead_line_modelitos17105_201902_probabilidades.txt") %>% mutate(foto_mes = 201902)
prob_1001_201903 = read_tsv("lightgbm_dead_line_modelitos17105_201903_probabilidades.txt") %>% mutate(foto_mes = 201903)
prob_1001_201904 = read_tsv("lightgbm_dead_line_modelitos17105_201904_probabilidades.txt") %>% mutate(foto_mes = 201904)

## 1007
prob_1007_201806 = read_tsv("lightgbm_dead_line_1007_201806_probabilidades.txt")
prob_1007_201807 = read_tsv("lightgbm_dead_line_1007_201807_probabilidades.txt")
prob_1007_201808 = read_tsv("lightgbm_dead_line_1007_201808_probabilidades.txt")
prob_1007_201809 = read_tsv("lightgbm_dead_line_1007_201809_probabilidades.txt")
prob_1007_201810 = read_tsv("lightgbm_dead_line_1007_201810_probabilidades.txt")
prob_1007_201811 = read_tsv("lightgbm_dead_line_1007_201811_probabilidades.txt")
prob_1007_201812 = read_tsv("lightgbm_dead_line_1007_201812_probabilidades.txt")
prob_1007_201901 = read_tsv("lightgbm_dead_line_1007_201901_probabilidades.txt")
prob_1007_201902 = read_tsv("lightgbm_dead_line_1007_201902_probabilidades.txt")
prob_1007_201903 = read_tsv("lightgbm_dead_line_1007_201903_probabilidades.txt")
prob_1007_201904 = read_tsv("lightgbm_dead_line_1007_201904_probabilidades.txt")

## Join
list_of_cutoff = list(0.025, 0.0265, 0.0272)

list_of_index = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

list_1001 = list(prob_1001_201806, prob_1001_201807, prob_1001_201808,
                 prob_1001_201809, prob_1001_201810, prob_1001_201811,
                 prob_1001_201812, prob_1001_201901, prob_1001_201902,
                 prob_1001_201903, prob_1001_201904)

list_1007 = list(prob_1007_201806, prob_1007_201807, prob_1007_201808,
                 prob_1007_201809, prob_1007_201810, prob_1007_201811,
                 prob_1007_201812, prob_1007_201901, prob_1007_201902,
                 prob_1007_201903, prob_1007_201904)

file_profits = "ensamble7_profits.csv"

for (a_cutoff in list_of_cutoff) {
  for (at_index in list_of_index) {
    prob_1001_index = list_1001[[at_index]]
    prob_1001_index = prob_1001_index %>% 
      rename(prob_positivo_1001 = prob_positivo)
    
    prob_1007_index = list_1007[[at_index]]
    prob_1007_index = prob_1007_index %>% 
      rename(prob_positivo_1007 = prob_positivo)
    
    prob_all_index = prob_1001_index %>% 
      inner_join(prob_1007_index, by = c("numero_de_cliente", "foto_mes"))
    
    prob_all_index = prob_all_index %>%
      select(-c(clase01.x)) %>%
      rename(clase01 = clase01.y) %>%
      mutate(prob_positivo = rowMeans(
        select(., prob_positivo_1001, prob_positivo_1007))) %>%
      select(-c(prob_positivo_1001, prob_positivo_1007))
    
    calculate_profits(prob_all_index, a_cutoff)
  }
}

## Calculo la entrega
choose_cutoff = 0.025
#choose_cutoff = 0.0265
#choose_cutoff = 0.0272

file_presentation = "ensamble8_entrega_00265.txt"
prob_1001_201906 = read_tsv("lightgbm_dead_line_modelitos17105_201906_probabilidades.txt") %>% mutate(foto_mes = 201906)
prob_1007_201906 = read_tsv("lightgbm_dead_line_1007_201906_probabilidades.txt")

prob_1001_201906 = prob_1001_201906 %>% 
  rename(prob_positivo_1001 = prob_positivo)

prob_1007_201906 = prob_1007_201906 %>% 
  rename(prob_positivo_1007 = prob_positivo)

prob_all_201906 = prob_1001_201906 %>% 
  inner_join(prob_1007_201906, by = c("numero_de_cliente", "foto_mes"))

prob_all_201906 = prob_all_201906 %>%
  select(-c(clase01.x)) %>%
  mutate(prob_positivo = rowMeans(
    select(., prob_positivo_1001, prob_positivo_1007))) %>%
  select(-c(prob_positivo_1001, prob_positivo_1007)) %>%
  arrange(desc(prob_positivo))

presentation = prob_all_201906 %>% 
  filter(prob_positivo > choose_cutoff) %>%
  select(numero_de_cliente)

write_tsv(x = presentation, path = file_presentation, col_names = FALSE)
