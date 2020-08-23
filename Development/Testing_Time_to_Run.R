library(tictoc)
source("User_Tracks_Function.R")
source("User_Features_Function.R")
source("User_Lyric_Generation_Function.R")
source("User_Lyric_Analysis_Function.R")

tracks <- Tracks_Function(user = "Jakerocksalot",playlists=c("Angry ", "New 1", "New 17", "New 18"))

tic()
New_Lyrics_Function(tracks)
toc()

tic()
Lyrics <- Lyric_Generation_Function(tracks)
Lyric_Features <- Lyric_Analysis_Function(Lyrics)
toc()