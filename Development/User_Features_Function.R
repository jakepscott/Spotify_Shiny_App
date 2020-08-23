library(stringr)
library(tidyverse)
library(Rspotify)
library(tictoc)
library(tools)
load("keys")

Features_Function <- function(track_data, features=
                                c("Song Features", "Release Dates", "Genre Information", "Explicit Status of Songs")){
  #only works if there is at least 1 song in the track_data object
  if(nrow(track_data)==0){
    return("Sorry, no tracks in this input!")
  }
  
  #This spreads the playlist column so that if a song appears in two playlists, it only has one row and thus I get the features
  #for it only once. This saves time. Later I will use pivotFeatures_longer to get the playlist column back, which I can use with
  #group_by()
  tracks <- track_data %>% 
    distinct() %>% 
    mutate(indicator=1) %>% 
    pivot_wider(names_from = Playlist,values_from=indicator,names_prefix="Playlist: ") %>% 
    mutate(across(-c(Song:Album_id),~if_else(is.na(.),0,1))) 
  
  ids <- tracks$Id
  
  if ("Song Features" %in% features) {
    # Getting Features --------------------------------------------------------
    Features <- data.frame(id=as.character(),danceability=as.double(), energy=as.double(),
                           key=as.integer(),loudness=as.double(), mode=as.integer(),
                           speechiness=as.double(),acousticness=as.double(),instrumentalness=as.integer(),
                           liveness=as.double(),valence=as.double(),tempo=as.double(),duration_ms=as.integer(),
                           time_signature=as.integer(),uri=as.character(),analysis_url=as.character())

    for(i in 1:length(ids)){
      tryCatch({
        Sys.sleep(.01)
        Features<-rbind(Features,getFeatures(ids[i],token=keys))},
        error=function(e){})
    }
    #Fixing the column names to make them pretty
    Features <- Features %>% rename(`Duration`="duration_ms",
                                    `Time Signature`="time_signature") %>% 
      select(id:mode,valence:Duration)
    colnames(Features) <- Features %>% colnames() %>% tools::toTitleCase()
    
    tracks <- left_join(tracks,Features) %>% 
      mutate(`Duration (Minutes)`=round(Duration/60000,1)) %>% 
      select(-Duration) %>%
      mutate(Key=ifelse(Key==-1,NA,Key),
             Mode=ifelse(Mode==1,"Major","Minor"),
             Key=as.factor(Key))
    
    
    
     
    cat("\nGot Song Features!\n")
  }
  
  
  
  # Getting Explicit/non-Explicit and Featured Artist name and id ------------------------------------------------------
  if ("Explicit Status of Songs" %in% features) {
    ##Download features about the tracks
    track_info <- data.frame(track_id=as.character(),name=as.character(),explicit=as.logical(),
                             popularity=as.integer(),artists=as.character(),
                             artists_id=as.character(), album=as.character(),album_id=as.character())
    track_info <- track_info[-1,]
    
    for(i in 1:length(ids)){#for each song
      tryCatch({
        Sys.sleep(.01)
        track_info<-rbind(track_info,getTrack(ids[i],token=keys))
      }, error=function(e){})
    }
    
    track_info <- track_info %>% rename("Id"=track_id,
                                        "Explicit?"=explicit) %>% select(Id,`Explicit?`)
    
    tracks <- left_join(tracks,track_info)
    cat("\nGot Explicit Status!\n")
  }
  
  
  
  # Ablum Info --------------------------------------------------------------
  if ("Release Dates" %in% features) {
    Sys.sleep(.001)
    Album_info <- data.frame(id=as.character(),artist=as.character(),name=as.character(),
                             label=as.character(),popularity=as.integer(),
                             release_date=as.character(),album_type=as.character(),
                             track_total=as.integer())
    album_ids <- unique(tracks$Album_id)
    
    for(i in 1:length(album_ids)){
      tryCatch({
        Sys.sleep(.01)
        Album_info<-rbind(Album_info,getAlbumInfo(album_ids[i],token=keys)[1,])
      }, error=function(e){})
    }
    
    Album_info <- Album_info %>% rename("Album_id"=id, `Album Release Date`=release_date) %>% 
      dplyr::select(Album_id,`Album Release Date`)
    
    #Fixing the album release date.
    #Some release dates are just a year, using nchar I find these, and correct them by just 2020-01-01, for example
    fixed_rds <- as_tibble(Album_info) %>% select(`Album Release Date`) %>% distinct() %>%
      mutate(`Album Release Date`=as.character(`Album Release Date`), nchar=nchar(`Album Release Date`)) %>% 
      mutate(album_rd_correct=case_when(nchar==10~`Album Release Date`,
                                        nchar==7~paste(`Album Release Date`, "01", sep = "-"),
                                        nchar==4~paste(`Album Release Date`,"01","01",sep = "-"))) %>% 
      select(-nchar) %>% 
      mutate(`Days Since Release`=as.numeric(Sys.Date()-as.Date(album_rd_correct)),
             `Years Since Release`=as.numeric(round(`Days Since Release`/365,digits = 2))) %>% 
      select(-album_rd_correct,
             -`Days Since Release`) 
    
    Album_info <- left_join(Album_info,fixed_rds) %>% as_tibble() %>% select(-`Album Release Date`)
    tracks <- left_join(tracks,Album_info) 
    cat("\nGot Release Dates!\n")
  }
  
  
  
  # Artist Info -------------------------------------------------------------
  if ("Genre Information" %in% features) {
    ##Getting each artist just one time to save time
    unique_artists <- unique(tracks$Artist_id)
    
    ##Just use a for loop, pretty standard
    #These just initialize the tibble I will be binding my newly created rows to in the for loop
    Artist_info <- data.frame(name=as.character(),id=as.character(),popularity=as.integer(), 
                              followers=as.integer(),genres=as.character())
    
    
    for(i in 1:length(unique_artists)){#for each song
      tryCatch({
        Sys.sleep(.01)
        Artist_info<-rbind(Artist_info,getArtist(unique_artists[i],token=keys))
      }, error=function(e){})
    }
    
    Artist_info <- Artist_info %>% rename("Artist_id"=id,"Artist"=name,`Artist Genres`=genres) %>% 
      mutate(`Artist Genres`=toTitleCase(str_replace_all(`Artist Genres`,";", ", "))) %>% 
      select(Artist_id,Artist,`Artist Genres`)
    tracks <- left_join(tracks,Artist_info) %>% select(-Artist_full)
    cat("\nGot Genres!\n")
  }
  
  
  
  # Joining with Gender of Artist -------------------------------------------
  genders <- read_rds("Data/artist_genders.rds") %>% rename("Artist"=artist,`Artist Gender`=genders)
  tracks <- left_join(tracks,genders)
  
  
  # Removing IDs ------------------------------------------------------------
  
  tracks <- tracks %>% select(-c(Album_id,Artist_id))   
  
  # Making a Column for Playlists,renaming some columns -------------------------------------------
  tracks <- tracks %>% pivot_longer(cols=contains("Playlist: "), names_to="Playlist") %>% 
    mutate(Playlist=str_remove(Playlist,"Playlist: ")) %>% 
    filter(value==1) %>% 
    select(-value) 
  tracks
}

