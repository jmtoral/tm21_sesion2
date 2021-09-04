# TF-IDF

pacman::p_load(readxl, tidyverse, tidytext)

prop <- read_excel("input/baseDatosCandidatos_030621.xls")

prop %>% 
  filter(!is.na(MOTIVO_CARGO_PUBLICO)) %>% 
  mutate(dup = stringi::stri_duplicated(MOTIVO_CARGO_PUBLICO)) %>% 
  mutate(dup = as.numeric(dup))  %>% 
  group_by(PARTIDO_COALICION) %>% 
  summarise(duplicados = sum(dup))


palparty <- prop %>% 
  filter(!is.na(MOTIVO_CARGO_PUBLICO)) %>% 
  mutate(dup = stringi::stri_duplicated(MOTIVO_CARGO_PUBLICO)) %>%
  filter(dup == T) %>% 
  distinct(NOMBRE_CANDIDATO, .keep_all = T) %>%
  unnest_tokens(palabra, MOTIVO_CARGO_PUBLICO) %>% 
  filter(!palabra %in% tm::stopwords("es")) %>% 
  filter(!palabra %in% c("ser", "seguir", "30")) %>% 
  filter(!is.na(palabra)) %>% 
  count(PARTIDO_COALICION, palabra, sort=T)  %>%
  bind_tf_idf(palabra, PARTIDO_COALICION, n)