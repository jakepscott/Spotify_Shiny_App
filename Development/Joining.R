source("User_Tracks_Function.R")
source("User_Features_Function.R")
source("User_Lyric_Generation_Function.R")
source("User_Lyric_Analysis_Function.R")

tracks <- Tracks_Function(user = "Jakerocksalot",playlists=c("Angry ", "New 1", "New 17", "New 18"))
#You need to get rid of the playlist column so that it doesn't try to join by that, and you need to do distinct because if a song
#appears in 2+ playlists it will get added twice to each corresponding column in the lefthand side
Features <- Features_Function(track_data = tracks) %>% distinct()
Lyrics <- Lyric_Generation_Function(tracks)
Lyric_Features <- Lyric_Analysis_Function(Lyrics)

Full_Data <- left_join(Features,Lyric_Features)

