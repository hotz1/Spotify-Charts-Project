library(tidyverse)
library(dtplyr)
library(dplyr)
library(data.table)
library(xml2)
library(httr)
library(stringr)
library(spotifyr)
library(knitr)
library(kableExtra)
library(lubridate)


spotify_data_raw <- read_csv("spotify_data_weekly_raw.csv")

# Extracting number of streams per week in the country
spotify_cleaned <- spotify_data_raw %>%
  mutate(Streams = stringr::str_remove_all(Streams, ",")) %>%
  mutate(Streams = stringr::str_extract(Streams, "[:digit:]+")) %>%
  mutate(Streams = as.numeric(Streams))

# Extracting weekly Top 200 position in the country
spotify_cleaned <- spotify_cleaned %>%
  mutate(Weekly_Rank = stringr::str_remove_all(Position, ",")) %>%
  mutate(Weekly_Rank = stringr::str_extract(Weekly_Rank, "[:digit:]+")) %>%
  mutate(Weekly_Rank = as.numeric(Weekly_Rank))

# Extracting the Track URLs and Track IDs
spotify_cleaned <- spotify_cleaned %>%
  mutate(Track_URL = stringr::str_extract(Source, "https://open.spotify.com/track/[:alnum:]*")) %>%
  mutate(Track_ID = stringr::str_remove(Track_URL, "https://open.spotify.com/track/"))

# Extracting the required variables
spotify_cleaned <- spotify_cleaned %>%
  select(Track_URL, Track_ID, Country, Country_Code, Weekly_Streams = Streams,
         Weekly_Rank, Week_Start, Week_End = Week_Ending)

# Saving this as a new CSV file
write.csv(spotify_cleaned, "spotify_cleaned.csv", row.names = FALSE)



# Setup for the Spotify API
client_id <- read_file("spotify_client_id.txt")
client_secret <- read_file("spotify_client_secret.txt")
Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
access_token <- get_spotify_access_token()

# Defining a function to get track info from the Spotify API
get_song_info <- function(track_id){
  track_info <- get_track(track_id)
  track_features <- get_track_audio_features(track_id)
  
  # Edge case to deal with tracks that don't have audio features
  if(dim(track_features)[2] == 1){
    song_info <- cbind(track_id, track_info$name, paste(track_info$artists$name, collapse = ", "),
                       track_info$duration_ms, NA, NA, NA)
  }
  else { 
    song_info <- cbind(track_id, track_info$name, paste(track_info$artists$name, collapse = ", "), 
                       track_info$duration_ms, track_features$danceability,
                       track_features$energy, track_features$valence)
  }
  return(song_info)
}

# Get distinct songs 
unique_songs <- spotify_cleaned %>%
  distinct(Track_ID)

# Get info for each distinct song
song_info_table <- c()
for (i in 1:length(unique_songs$Track_ID)){
  song_info_table <- rbind(song_info_table, get_song_info(unique_songs$Track_ID[i]))
  if(numbers::mod(i-1, 100) == 0){
    print(i)
    Sys.sleep(1)
  }
}

# Putting all songs' information into one table
song_info_table <- as_tibble(song_info_table) %>%
  rename(Track_ID = track_id, Song_Name = V2, Artists = V3, Song_Length = V4,
         Danceability = V5, Energy = V6, Valence = V7)

# Computing number of occurrences for each song in the Top 200 lists
song_occurrences <- as.data.frame(table(spotify_cleaned$Track_ID))
names(song_occurrences) <- c("Track_ID", "Occurrences")

# Merging this with the previous song info table
song_info_big_table <- merge(song_info_table, song_occurrences)

# Saving this as a new CSV file
write.csv(song_info_big_table, "songs_info.csv", row.names = FALSE)


# Join individual song data with the Spotify Top 200 data
spotify_full <- merge(spotify_cleaned, song_info_big_table)

# Saving this as a new CSV file
write.csv(spotify_full, "spotify_data_full.csv", row.names = FALSE)

