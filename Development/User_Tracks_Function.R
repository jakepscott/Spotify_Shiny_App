library(stringr)
library(tidyverse)
library(Rspotify)
library(tictoc)
load("keys")

Tracks_Function <- function(user,playlists){
  # Cleaning the input ------------------------------------------------------
  #This makes it so that we just have the username of the user
  if (str_detect(user,"spotify")==T) {
    user <- str_remove(user,"spotify:user:")
  }
  # Playlist Loading --------------------------------------------------------
  #getting all playlists
  user_playlists <- getPlaylists(user,token = keys) %>% as_tibble() %>% filter(name %in% playlists,tracks>0)
  
  
  tracks <- tibble(tracks=as.character(),id=as.character(),popularity=as.integer(),artist=as.character(),
                   artist_full=as.character(), artist_id=as.character(),
                   album=as.character(), album_id=as.character(),playlist=as.character())
  
  for (i in 1:nrow(user_playlists)) {
    print(i)
    tryCatch({
      tracks_to_bind <- getPlaylistSongs(user_playlists$id[i],user_id = "spotify",token=keys) %>% 
        as_tibble() %>% 
        mutate(playlist=user_playlists$name[i])
      tracks <- rbind(tracks,tracks_to_bind)
    }, error=function(e){})
  }
  tracks <- tracks %>% rename("Song Popularity"=popularity,
                              "Song"=tracks)
  colnames(tracks) <- toTitleCase(colnames(tracks))
  tracks
}


