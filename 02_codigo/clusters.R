# Sesión 2: Distancias euclidianas
# Fechas: 28 de febrero


# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,
               tidytext,
               cluster, 
               tm)


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ine <- readxl::read_excel("01_datos/tablas/baseDatosCandidatos.xls")


# Explorar ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ine <- ine %>% 
  select(PARTIDO_COALICION, contains("PROPUESTA")) %>% 
  mutate(propuesta_c = str_c(PROPUESTA_1,
                             PROPUESTA_2,
  #                           PROPUESTA_GENERO,
                             sep = " ")) %>% 
  select(PARTIDO_COALICION, propuesta_c)


ine.tm <- ine %>% 
  unnest_tokens( palabras, propuesta_c) %>% # Matriz de términos
  filter(!palabras %in% tm::stopwords("es"))

### Matriz de document - término


ine.dtm <- ine.tm %>%
  count(PARTIDO_COALICION, palabras) %>% 
  cast_dtm(PARTIDO_COALICION, palabras, n)

## Recorte de sparsity 

ine.sub <- tm::removeSparseTerms(ine.dtm, 0.3)

tm::inspect(ine.sub)

# Distancia

d <- dist(ine.sub, method = "euclidian")

clus <- hclust(d, method = "complete")

plot(clus)


e <- dist(ine.dtm, method = "euclidian")

clus <- hclust(e, method = "complete")

plot(clus)


d <- dist(ine.sub, method = "manhattan")

clus <- hclust(d, method = "complete")

plot(clus)


e <- dist(ine.dtm, method = "manhattan")

clus <- hclust(d, method = "complete")

plot(clus)

# Términos

ine.term <- tm::removeSparseTerms(ine.dtm, 0.05)

d <- dist(ine.term, method = "euclidian")

clus <- hclust(d, method = "complete")

plot(clus)
