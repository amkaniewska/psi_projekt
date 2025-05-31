


#' ---
#' title: "Analiza tekstów piosenek Taylor Swift: LDA, asocjacje, bigramy"
#' author: "Antonina Kaniewska, Zofia Hasslinger "
#' date:   "maj 2025"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: journal     
#'     highlight: kate      
#'     toc: true            
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false
#' --- 


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)





#' # Wymagane pakiety
# Wymagane pakiety ----

library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)

#' # 0. Funkcja top_terms_by_topic_LDA
# 0. Funkcja top_terms_by_topic_LDA ----
# która wczytuje tekst 
# (wektor lub kolumna tekstowa z ramki danych)
# i wizualizuje słowa o największej informatywności
# przy metody użyciu LDA
# dla wyznaczonej liczby tematów



top_terms_by_topic_LDA <- function(input_text, 
                                   plot = TRUE,
                                   k = number_of_topics) 
{    
  corpus <- VCorpus(VectorSource(input_text))
  DTM <- DocumentTermMatrix(corpus)
  
  # usunięcie wszystkich pusty wierszy w macierzy częstości
  # ponieważ spowodują błąd dla LDA
  unique_indexes <- unique(DTM$i)
  DTM <- DTM[unique_indexes,]    
  

  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta") 
  
  # pobranie dziesięciu najczęstszych słów dla każdego tematu
  top_terms <- topics  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) 
  
  
  
  #wykres 
  if(plot == T){
    # dziesięć najczęstszych słów dla każdego tematu
    top_terms %>%
      mutate(term = reorder(term, beta)) %>% 
      ggplot(aes(term, beta, fill = factor(topic))) + 
      geom_col(show.legend = FALSE) + 
      facet_wrap(~ topic, scales = "free") +
      labs(x = "Terminy", y = "β (ważność słowa w temacie)") +
      coord_flip() +
      theme_minimal() +
      scale_fill_manual(values = c("#684551", "#E8998D","#8D7B68", "#093824", "#C6D8AF","#BAC7F2"))
  }else{ 
    # jeśli użytkownik nie chce wykresu
    # wtedy zwróć listę posortowanych słów
    return(top_terms)
  }
  
  
}




#' # Dane tekstowe
# Dane tekstowe ----


# Tworzenie korpusu dokumentów tekstowych

data <- read.csv("songs.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
corpus <- VCorpus(VectorSource(data$Lyrics))


# inspect(corpus)


corpus[[1]]
corpus[[1]][[1]]
corpus[[1]][2]



#' # 1. Przetwarzanie i oczyszczanie tekstu
# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usunięcie zbędnych znaków ----

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))


# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usunięcie zbędnych znaków lub pozostałości url, html itp.

# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze słowem (zazw. nazwa użytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CAŁY adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko ukośnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozostałość po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozostałości
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")


# Sprawdzenie
corpus[[1]][[1]]

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


# Sprawdzenie
corpus[[1]][[1]]

# manualne usuwanie słów specyficznych dla piosenek
corpus <- tm_map(corpus, removeWords, c("taylor","swift","verse","prechorus","outro","break","chorus",
                                        "bridge","colbie","caillat","postchorus","ohohoh",
                                        "radidididididididididada", "dada"))

corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]]

corpus_completed <- corpus


#' # Tokenizacja
# Tokenizacja ----


#' # 2. Zliczanie częstości słów
# 2. Zliczanie częstości słów ----
# (Word Frequency Count)

# Macierz częstości TDM 

tdm <- TermDocumentMatrix(corpus_completed)
tdm_m <- as.matrix(tdm)


# Zlicz same częstości słów w macierzach
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)



#' # 3. Eksploracyjna analiza danych
# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 10,random.order = FALSE,
          colors = c("#DA95D3", "#979D58", "#F1BF98", "#C36D52", "#D02711", "#8C0357"))
          


# Wyświetl top 10
print(head(tdm_df, 10))



#' # 4. Inżynieria cech w modelu Bag of Words:
#' # Reprezentacja słów i dokumentów w przestrzeni wektorowej
# 4. Inżynieria cech w modelu Bag of Words: ----
# Reprezentacja słów i dokumentów w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)

# - podejście surowych częstości słów
# (częstość słowa = liczba wystąpień w dokumencie)
# (Raw Word Counts)



#' # UCZENIE MASZYNOWE NIENADZOROWANE
# UCZENIE MASZYNOWE NIENADZOROWANE ----
# (Unsupervised Machine Learning)




#' # Modelowanie tematów: ukryta alokacja Dirichleta
# Modelowanie tematów: ukryta alokacja Dirichleta (LDA) ----




# Rysowanie dziesięciu słów 
# o największej informatywności według tematu
# dla wyznaczonej liczby tematów 


# liczba tematów = 2
number_of_topics = 2
top_terms_by_topic_LDA(input_text = sapply(corpus_completed, as.character), k = number_of_topics)


# liczba tematów = 3
number_of_topics = 3
top_terms_by_topic_LDA(input_text = sapply(corpus_completed, as.character), k = number_of_topics)


# liczba tematów = 4
number_of_topics = 4
top_terms_by_topic_LDA(input_text = sapply(corpus_completed, as.character), k = number_of_topics)


# liczba tematów = 6
number_of_topics = 6
top_terms_by_topic_LDA(tdm_df$word)

#' # 5 Asocjacje - znajdowanie współwystępujących słów
# 5 Asocjacje - znajdowanie współwystępujących słów ----


# terminy, które badamy pod kątem asocjacji


findAssocs(tdm,"love",0.5)
findAssocs(tdm,"dream",0.5)
findAssocs(tdm,"remember",0.5)
findAssocs(tdm,"feel",0.5)
findAssocs(tdm,"bad",0.5)
findAssocs(tdm,"time",0.5)
findAssocs(tdm,"patriarchy",0.5)
findAssocs(tdm,"wounds",0.5)


#' # Wizualizacja asocjacji
# Wizualizacja asocjacji ----


# Wytypowane słowo i próg asocjacji
target_word <- "love"
cor_limit <- 0.4


# Obliczenie asocjacje dla tego słowa
associations <- findAssocs(tdm, target_word, corlimit = cor_limit)
assoc_vector <- associations[[target_word]]
assoc_sorted <- sort(assoc_vector, decreasing = TRUE)


# Ramka danych
assoc_df <- data.frame(
  word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),
  score = assoc_sorted
)


# Wykres lizakowy (lollipop chart):
ggplot(assoc_df, aes(x = score, y = reorder(word, score))) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), color = "#a6bddb", size = 1.2) +
  geom_point(color = "#8b1c62", size = 4) +
  geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
  scale_x_continuous(limits = c(0, max(assoc_df$score) + 0.1), expand = expansion(mult = c(0, 0.2))) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste0("Asocjacje z terminem: '", target_word, "'"),
    subtitle = paste0("Próg r ≥ ", cor_limit),
    x = "Współczynnik korelacji Pearsona",
    y = "Słowo"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

# Wytypowane słowo i próg asocjacji
target_word_1 <- "wounds"
cor_limit <- 0.6


# Obliczenie asocjacje dla tego słowa
associations <- findAssocs(tdm, target_word_1, corlimit = cor_limit)
assoc_vector <- associations[[target_word_1]]
assoc_sorted <- sort(assoc_vector, decreasing = TRUE)


# Ramka danych
assoc_df <- data.frame(
  word = factor(names(assoc_sorted), levels = names(assoc_sorted)[order(assoc_sorted)]),
  score = assoc_sorted
)

# Wykres lizakowy z natężeniem
# na podstawie wartości korelacji score:
ggplot(assoc_df, aes(x = score, y = reorder(word, score), color = score)) +
  geom_segment(aes(x = 0, xend = score, y = word, yend = word), size = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = round(score, 2)), hjust = -0.3, size = 3.5, color = "black") +
  scale_color_gradient(low = "#FADADD", high = "#C71585") +
  scale_x_continuous(
    limits = c(0, max(assoc_df$score) + 0.1),
    expand = expansion(mult = c(0, 0.2))
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste0("Asocjacje z terminem: '", target_word_1, "'"),
    subtitle = paste0("Próg r ≥ ", cor_limit),
    x = "Współczynnik korelacji Pearsona",
    y = "Słowo",
    color = "Natężenie\nskojarzenia"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right"
  )

#' # 6. Analiza bigramów
# Analiza bigramów ----
# Dodatek: najczęstsze frazy dwuwyrazowe

bigrams <- data %>%
  select(Lyrics) %>%
  unnest_tokens(output = bigram, input = Lyrics, token = "ngrams", n = 2)

# Oczyszczenie bigramów

# Lista bigramów do usunięcia (ręczna)
remove_bigrams <- c("i i", "verse 2", "pre chorus", "taylor swift", "on the", 
                    "in the", "oh oh", "and i")

# Filtrowanie
bigrams_clean <- bigrams %>%
  filter(!bigram %in% remove_bigrams)

# Liczenie czystych bigramów
bigrams_count_clean <- bigrams_clean %>%
  count(bigram, sort = TRUE)


# Dziesięć najczęstszych bigramów po oczyszczeniu
head(bigrams_count_clean, 10)

