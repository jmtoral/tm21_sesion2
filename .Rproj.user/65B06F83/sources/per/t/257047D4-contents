# Análisis de sentimientos
# Fecha: 28 de agosto de 2021



# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,
               tidytext,
               textdata,
               readtext,
               tm,
               syuzhet) # coreNLP

# Sentometrics
# Quanteda

## Ejemplos de diccionarios tidytext: #AFINN, #Bing, #NRC

get_sentiments("afinn") %>% 
  arrange(value)

get_sentiments("bing")

get_sentiments("nrc")


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pp <- readtext("01_datos/literatura/pedro_páramo.pdf") %>% 
  mutate(text = str_replace_all(text, "\n", " "),
         text = str_squish(text))

pp_sen <- get_sentences(pp$text)

method <- "nrc"

lang <- "spanish"


vec_sent <- get_sentiment(pp_sen, method = method, language = lang) # -8 a 8


rest <- tibble(enunciado = pp_sen,
               sentimiento = vec_sent) %>% 
  filter(vec_sent != 0)

## Valencia emocional

plot(vec_sent, 
     type = "l",
     main = "Trayectoria emocional de Pedro Páramo",
     xlab = "Tiempo narrativo",
     ylab = "Valencia emocional")

tsent <- tibble(enunciado = seq_along(vec_sent),
                vec_sent)

rest %>% 
mutate(n_enunciado = seq_along(enunciado)) %>% 
  ggplot(aes(x= n_enunciado,
             y= sentimiento)) +
  geom_line() 



