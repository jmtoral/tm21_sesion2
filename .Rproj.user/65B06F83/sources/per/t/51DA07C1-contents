## Sesión 2: Scraping 
## Fechas: 28 de agosto de 2021


# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,
               tidytext,
               SnowballC, #algoritmo de Porter
               rvest) #harvest #Scraper


# Extraer datos -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

url  <- "https://www.constituteproject.org/constitutions?lang=es"

## Paso 1 ------------------------------------------------------------------------

## Extraer todos los vínculos a las constituciones

enlaces <- read_html(url) %>% #Ctrl + shift + m
  html_nodes("a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  filter(str_detect(value, "constitution\\/[a-zA-Z]+")) %>% 
  mutate(value = str_c("https://www.constituteproject.org", value))

enlaces[1]

txt <- read_html("https://www.constituteproject.org/constitution/Cuba_2019?lang=es") %>% ## Sólo un link
  html_nodes("p") %>% 
  html_text() %>% 
  as_tibble()

## Extraer el texto de todas las constituciones

txts <- lapply(enlaces$value, function(loc){
  loc %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    as_tibble()
  
  #cat("voy en ", loc)
})


txts[[1]]

## Definir los nombre de la lista
names(txts) <- enlaces$value %>% 
  str_remove("https://www.constituteproject.org/constitution/") %>% 
  str_remove("\\?lang=es")


## Contruir un tibble con todos los objetos de la lista

txts_df <- bind_rows(txts, .id = "pais")



# Análisis explotario -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Número de palabras

txts_df %>% 
  unnest_tokens(palabras, value) %>% 
  group_by(pais) %>% 
  summarise(num_pal = n()) %>% 
  arrange(-num_pal)
  
## Número de palabras únicas

txts_df %>% 
  unnest_tokens(palabra, value) %>% 
  distinct(pais, palabra) %>% 
  group_by(pais) %>% 
  summarise(num_pal = n()) %>% 
  arrange(-num_pal) 

## Búsqueda de palabras

txts_df  %>% 
  unnest_tokens(palabra, value) %>% 
  filter(str_detect(palabra, "mujer|mujeres")) %>% 
  count(pais, sort=T)


enunciados <- txts_df  %>% 
  unnest_tokens(palabra, value, token = "sentences")%>% 
  filter(str_detect(palabra, "mujer|mujeres")) %>% 
  count(pais, sort=T)

txts_df  %>% 
  unnest_tokens(palabra, value, token = "sentences")%>% 
  filter(str_detect(palabra, "mujer|mujeres")) %>% 
  filter(pais == "Iran_1989")




## TF-IDF

txts_df  %>% 
  filter(pais %in% c("Mexico_2015", "Cuba_2019", "Spain_2011")) %>% 
  unnest_tokens(palabras, value) %>% 
  count(pais, palabras, sort=T) %>% 
  filter(str_detect(palabras, "[a-zA-Záéíóúñ]")) %>% 
  filter(!palabras %in% c("méxico", "españa", "cuba")) %>% 
  bind_tf_idf(palabras, pais, n) %>% # texto, grupo, frecuencia
  arrange(pais, -tf_idf) %>% 
  group_by(pais) %>% 
  top_n(5, tf_idf)



## Stemas

stem <- txts_df  %>% 
  filter(pais %in% c("Mexico_2015", "Cuba_2019", "Spain_2011")) %>% 
  unnest_tokens(palabras, value) %>% 
  mutate(stem = wordStem(palabras, "es")) %>% # Stema SnowballC
  count(pais, stem, sort=T) 

## Utilicé el stema no las palabras

stem %>% 
  filter(str_detect(stem, "[a-zA-Záéíóúñ]")) %>% 
  filter(!stem %in% c("méxico", "españa", "cuba")) %>% 
  bind_tf_idf(stem, pais, n)%>% # texto, grupo, frecuencia
  arrange(pais, -tf_idf) %>% 
  group_by(pais) %>% 
  top_n(10, tf_idf)













