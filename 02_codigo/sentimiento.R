### Sentimientos

library(syuzhet)

path_to_a_text_file <- system.file("extdata", "quijote.txt",package = "syuzhet")
my_text <- get_text_as_string(path_to_a_text_file)
char_v <- get_sentences(my_text)
method <- "nrc"
lang <- "spanish"
my_text_values <- get_sentiment(char_v, method=method, language=lang)


my_example_text <- "I begin this story with a neutral statement.  
  Basically this is a very silly test.  
  You are testing the Syuzhet package using short, inane sentences.  
  I am actually very happy today. 
  I have finally finished writing this package.  
  Tomorrow I will be very sad. 
  I won't have anything left to do. 
  I might get angry and decide to do something horrible.  
  I might destroy the entire package and start from scratch.  
  Then again, I might find it satisfying to have completed my first R package. 
  Honestly this use of the Fourier transformation is really quite elegant.  
  You might even say it's beautiful!"
s_v <- get_sentences(my_example_text)
s_v_sentiment <- get_sentiment(s_v)
