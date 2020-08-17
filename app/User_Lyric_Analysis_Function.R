# loading libs and key------------------------------------------------------------
library(tidyverse)
library(Rspotify)
library(lubridate)
library(tidytext)
library(geniusr)
library(stringr)
library(rvest)
library(remotes)
library(genius)
library(stringi)
library(tools)
library(textdata)
nrc_data <- read_rds("Data/nrc_data.rds")
afinn_data <- read_rds("Data/afinn_data.rds")
Lyric_Analysis_Function <- function(Lyrics){
  
  #only works if there is at least 1 song in the track_data object
  if(nrow(Lyrics)==0){
    return("Sorry, no lyrics in this input!")
  }
  
  #Using the EmoLex to get word sentiments
  word_sentiment <- nrc_data
  
  ##Getting it into useable form, so you can see which emotions a given word does or does not match with
  word_sentiment <- word_sentiment %>% mutate(na=NA) %>% pivot_wider(names_from = sentiment, values_from = sentiment) %>%
    dplyr::select(-na)
  
  #Changing to dummy vars with 1s and 0s
  word_sentiment <- word_sentiment %>% mutate(trust=ifelse(is.na(trust),0,1),
                                              fear=ifelse(is.na(fear),0,1),
                                              negative=ifelse(is.na(negative),0,1),
                                              sadness=ifelse(is.na(sadness),0,1),
                                              anger=ifelse(is.na(anger),0,1),
                                              surprise=ifelse(is.na(surprise),0,1),
                                              positive=ifelse(is.na(positive),0,1),
                                              disgust=ifelse(is.na(disgust),0,1),
                                              joy=ifelse(is.na(joy),0,1),
                                              anticipation=ifelse(is.na(anticipation),0,1))
  
  ##Giving a unique row identifier which I will use to bind with song sentiment information
  Lyrics <- Lyrics %>% mutate(Unique_id=1:length(Lyrics))
  
  ##Getting an initial row to connect song sentiments to
  song_sentiment <- "Placeholder" %>% 
    tibble(word=.) %>%
    unnest_tokens(output = "word",input = "word",token = "words") %>% 
    left_join(., word_sentiment, by="word") %>% 
    na.omit %>% 
    mutate(`Total Words`=nrow(.)) %>% 
    mutate_if(.predicate = is.double, .funs= ~sum(.)) %>% 
    mutate_if(is.double, funs(`percent` = round(./`Total Words`*100,1))) %>% 
    head(1) %>% 
    mutate(Song=Lyrics$Song[[1]], Artist=Lyrics$Artist[[1]],
           Unique_id=Lyrics$Unique_id[[1]]) %>% 
    select(-word) %>% 
    select(Song, Artist, Unique_id, trust:anticipation_percent) %>% 
    head(0)
  
  
  for (i in 1:nrow(Lyrics)) {
    print(i)
    tryCatch({
      sentiments_to_bind <- Lyrics$Lyrics[[i]] %>% 
        tibble(word=.) %>%
        unnest_tokens(output = "word",input = "word",token = "words") %>% 
        left_join(., word_sentiment, by="word") %>% 
        na.omit %>% 
        mutate(`Total Words`=nrow(.)) %>% 
        mutate_if(.predicate = is.double, .funs= ~sum(.)) %>% 
        mutate_if(is.double, funs(`percent` = round(./`Total Words`*100,1))) %>% 
        head(1) %>% 
        mutate(Song=Lyrics$Song[[i]], Artist=Lyrics$Artist[[i]],
               Unique_id=Lyrics$Unique_id[[i]]) %>% 
        select(-word) %>% 
        select(Song, Artist, Unique_id, trust:anticipation_percent) 
      song_sentiment <- rbind(song_sentiment, sentiments_to_bind)
    }, error=function(e){print(e)})
  }
  
  song_sentiment <- song_sentiment %>% rename_if(str_detect(names(.),"_percent"),~paste("Percent of",.,"words",sep = " ")) %>% 
    rename_at(vars(trust:anticipation),~paste("Number of",.,"Words",sep = " ")) 
  
  colnames(song_sentiment) <- song_sentiment %>% colnames() %>% str_remove("_percent") %>% toTitleCase()
  
  Lyrics <- left_join(Lyrics, song_sentiment, by=c("Unique_id", "Song", "Artist"))
  
  
  # Overall Song Sentiment --------------------------------------------------
  afinn <- afinn_data
  
  song_sentiment_2 <- "placeholder" %>% 
    tibble(word=.) %>%
    unnest_tokens(output = "word",input = "word",token = "words") %>% 
    left_join(., afinn, by="word") %>% 
    na.omit %>% 
    mutate(`Overall Sentiment`=mean(value), 
           Song=Lyrics$Song[1],
           Artist=Lyrics$Artist[1]) %>% 
    select(Song, Artist, `Overall Sentiment`) %>% 
    head(0)
  
  for (i in 1:nrow(Lyrics)) {
    print(i)
    tryCatch({
      if(is.double(Lyrics$Lyrics[[i]])==F){
        sentiments_to_bind <- Lyrics$Lyrics[[i]] %>% 
          tibble(word=.) %>%
          unnest_tokens(output = "word",input = "word",token = "words") %>% 
          left_join(., afinn, by="word") %>% 
          na.omit %>% 
          mutate(`Overall Sentiment`=mean(value), 
                 Song=Lyrics$Song[i],
                 Artist=Lyrics$Artist[i]) %>% 
          select(Song, Artist, `Overall Sentiment`) %>% 
          head(1)} else {
            sentiments_to_bind <- tibble(Song=Lyrics$Song[i],
                                         Artist=Lyrics$Artist[i],
                                         `Overall Sentiment`=NA)
          }
      
      song_sentiment_2 <- rbind(song_sentiment_2, sentiments_to_bind)
    }, error=function(e){print(e)})
  }
  
  left_join(Lyrics,song_sentiment_2) %>% select(-Unique_id)
  
}


