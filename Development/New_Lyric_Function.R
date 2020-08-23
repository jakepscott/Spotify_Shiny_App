New_Lyrics_Function <- function(track_data){
  # Set up NRC Word Sentiment Information-------------------------------------------------------------------
  nrc_data <- read_rds("Data/nrc_data.rds")
  afinn_data <- read_rds("Data/afinn_data.rds")
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
  
  # Lyrics Loading --------------------------------------------------------
  songs <- track_data %>% select(Id,Song,Artist,Album) %>% distinct()
  
  
  ##Throwing the kitchen sink to try to get it to work- this time ridding of non-english letters/accents
  songs <- songs %>% mutate(song2=Song) %>% 
    separate(col = song2, into = c("song2", "extra"), sep = " [(]") %>%
    select(-extra) %>% 
    separate(song2, into = c("song2", "extra"), sep = " -") %>% select(-extra) %>% 
    separate(song2, into = c("song2", "extra"), sep = "-") %>% select(-extra) %>%
    mutate(song2=str_remove_all(string = song2, pattern = "[[:punct:]]"),
           artist2=str_remove_all(string = Artist, pattern = "[[:punct:]]")) %>%
    mutate(song2= stri_trans_general(str = song2, id = "Latin-ASCII"),
           artist2= stri_trans_general(str = artist2, id = "Latin-ASCII"))
  
  lyrics <- "placeholder"
  
  
  ##Getting an initial row to connect nrc sentiments to
  nrc_sentiment <- lyrics %>% 
    tibble(word=.) %>%
    unnest_tokens(output = "word",input = "word",token = "words") %>% 
    left_join(., word_sentiment, by="word") %>% 
    na.omit %>% 
    mutate(`Total Words`=nrow(.)) %>% 
    mutate_if(.predicate = is.double, .funs= ~sum(.)) %>% 
    mutate_if(is.double, funs(`percent` = round(./`Total Words`*100,1))) %>% 
    head(1) %>% 
    mutate(Song=Lyrics$Song[[1]], Artist=Lyrics$Artist[[1]]) %>% 
    select(-word) %>% 
    select(Song, Artist, trust:anticipation_percent) %>% 
    head(0)
  
  ##Getting an initial row to connect afinn sentiments to
  afinn_sentiment <- "placeholder" %>% 
    tibble(word=.) %>%
    unnest_tokens(output = "word",input = "word",token = "words") %>% 
    left_join(., afinn, by="word") %>% 
    na.omit %>% 
    mutate(`Overall Sentiment`=mean(value), 
           Song=Lyrics$Song[1],
           Artist=Lyrics$Artist[1]) %>% 
    select(Song, Artist, `Overall Sentiment`) %>% 
    head(0)
  
  
  for (i in 1:nrow(songs)) {
    tryCatch({
      print(i)
      # Get the Lyrics of Song i ------------------------------------------------
      lyrics <- genius_lyrics(artist = songs$artist2[i],
                              song = songs$song2[i],
                              info = "simple") %>%
        pull(lyric)
      
      
      # Make a tibble of NRC sentiments -----------------------------------------
      nrc_sentiments_to_bind <- lyrics %>% 
        tibble(word=.) %>%
        unnest_tokens(output = "word",input = "word",token = "words") %>% 
        left_join(., word_sentiment, by="word") %>% 
        na.omit %>% 
        mutate(`Total Words`=nrow(.)) %>% 
        mutate_if(.predicate = is.double, .funs= ~sum(.)) %>% 
        mutate_if(is.double, funs(`percent` = round(./`Total Words`*100,1))) %>% 
        head(1) %>% 
        mutate(Song=Lyrics$Song[[i]], Artist=Lyrics$Artist[[i]]) %>% 
        select(-word) %>% 
        select(Song, Artist, trust:anticipation_percent) 
      nrc_sentiment <- rbind(nrc_sentiment, nrc_sentiments_to_bind)
      
      
      # Make a tibble of afinn sentiments ---------------------------------------
      afinn <- afinn_data
      
      afinn_sentiments_to_bind <- lyrics %>% 
        tibble(word=.) %>%
        unnest_tokens(output = "word",input = "word",token = "words") %>% 
        left_join(., afinn, by="word") %>% 
        na.omit %>% 
        mutate(`Overall Sentiment`=mean(value), 
               Song=Lyrics$Song[i],
               Artist=Lyrics$Artist[i]) %>% 
        select(Song, Artist, `Overall Sentiment`) %>% 
        head(1)
      
      afinn_sentiment <- rbind(afinn_sentiment, afinn_sentiments_to_bind)
      
      
    }, error=function(e){print(e)}
    )
  }
  
  # Join the ID data, the NRC Sentiment, and Afinn Sentiment ----------------
  songs <- songs %>% left_join(nrc_sentiment) %>% left_join(afinn_sentiment)
  return(songs)
}
