if(!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               tidytext,
               rvest,
               here)



url <- "https://www.constituteproject.org/constitutions?lang=es"

## Paso 1 --------

enlaces <- read_html(url) %>% 
  html_nodes("a") %>% 
  html_attr('href') %>% 
  tibble(direccion=.) %>% 
  filter(str_detect(direccion, "constitution\\/[a-zA-Z]+")) %>% 
  mutate(nombres = str_extract(direccion, "(?<=constitution\\/)(.*)(?=\\?)")) %>% 
  mutate(direccion = str_c("https://www.constituteproject.org", direccion)) %>% 
  select(nombres, direccion)

## Paso 2 ----

txt <- lapply(enlaces$direccion[1:10], function(url){
  art <-  tryCatch(
    url %>%
      as.character() %>% 
      read_html() %>% 
      html_nodes('p') %>% 
      html_text() %>% 
      as_tibble(), 
    error = function(e){NA}    # a function that returns NA regardless of what it's passed
  )
})

names(txt) <- enlaces$nombres[1:10]

txt <- bind_rows(txt, .id = "meta_information")



# Para hacer una columna con artículos
#url <- "https://www.constituteproject.org/constitution/Cuba_2018D?lang=es"

#html <- read_html(url)

#foo <- html %>% 
 # html_nodes('section .article-list .level2') 

#final <- map_dfr(foo, ~ tibble(
 # titulo = html_nodes(.x, '.float-left') %>% 
  #  html_text(),
  #content = list(html_nodes(.x, "p") %>% 
   #                html_text()))) %>% 
#  filter(!grepl("^SEC", titulo)) %>% 
 # unnest_longer(content)


x <- seq(0,100,by=0.1)
plot(x,log(1/x),typ="l",col="blue")
