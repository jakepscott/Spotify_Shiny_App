# loading libs------------------------------------------------------------
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


Lyric_Generation_Function <- function(track_data){
  #only works if there is at least 1 song in the track_data object
  if(nrow(track_data)==0){
    return("Sorry, no tracks in this input!")
  }
  
  
  # Lyrics Loading --------------------------------------------------------
  Lyrics <- track_data %>% select(Id,Song,Artist,Album) %>% distinct()
  
  # Getting lyrics from Genius using the genius::genius_lyrics function --------
  Lyrics <- Lyrics %>% mutate(Lyrics=1)
  # Attempt 4. Simplifying a lot- parentheses, hypens, punctuation --------
  ##Making a fourth attempt
  
  ##Throwing the kitchen sink to try to get it to work- this time ridding of non-english letters/accents
  Lyrics <- Lyrics %>% mutate(song2=Song) %>% 
    separate(col = song2, into = c("song2", "extra"), sep = " [(]") %>%
    select(-extra) %>% 
    separate(song2, into = c("song2", "extra"), sep = " -") %>% select(-extra) %>% 
    separate(song2, into = c("song2", "extra"), sep = "-") %>% select(-extra) %>%
    mutate(song2=str_remove_all(string = song2, pattern = "[[:punct:]]"),
           artist2=str_remove_all(string = Artist, pattern = "[[:punct:]]")) %>%
    mutate(song2= stri_trans_general(str = song2, id = "Latin-ASCII"),
           artist2= stri_trans_general(str = artist2, id = "Latin-ASCII"))
  
  
  ##Trying to get lyrics using simplified names
  for (i in 1:nrow(Lyrics)) {
    tryCatch({
      print(i)
      Lyrics$Lyrics[i] <- genius_lyrics(artist = Lyrics$artist2[i],
                                        song = Lyrics$song2[i],
                                        info = "simple") %>% 
        dplyr::select(lyric)
    }, error=function(e){print(e)}
    )
  }
  
  missed <- Lyrics[1,]
  missed <- Lyrics[-1,]
  for (i in 1:nrow(Lyrics)) {
    print(i)
    if (!is.character(Lyrics$Lyrics[[i]])) {
      missed[i,] <- Lyrics[i,]
    }   else {
      missed[i,] <- NA
    }
  }
  
  missed <- missed %>% filter(!is.na(Song))
  percent_captured <- round((nrow(Lyrics)-nrow(missed))/nrow(Lyrics)*100,digits = 1)
  cat("I managed to get lyrics for", percent_captured, "percent of songs!") 
  Lyrics <- Lyrics %>% select(-c(song2,artist2)) 
  return(Lyrics)
}
