#----------------------------------------------------------------------------------------------------------------#
get_my_saved_tracks <- function(limit = 20, offset = 0, 
                                market = NULL, 
                                authorization = get_spotify_authorization_code(), 
                                include_meta_info = FALSE) {
  base_url <- 'https://api.spotify.com/v1/me/tracks'
  if (!is.null(market)) {
    if (str_detect(market, '^[[:alpha:]]{2}$')) {
      stop('"market" must be an ISO 3166-1 alpha-2 country code')
    }
  }
  params <- list(
    limit = limit,
    offset = offset,
    market = market
  )
  res <- RETRY('GET', base_url, query = params, config(token = authorization), encode = 'json')
  stop_for_status(res)
  res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)
  
  if (!include_meta_info) {
    res <- res$items
  }
  return(res)
}
#----------------------------------------------------------------------------------------------------------------#

## Libraries
library(dplyr)
library(spotifyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(purrr)
library(tidyverse)
library(knitr)

## Spotify API handshake
Sys.setenv(SPOTIFY_CLIENT_ID = '6668613321514ba9b1e29339211357a8')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '213860ac09cb43f6a5ca2c3ec273ba3f')

access_token <- get_spotify_access_token(
                                          client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), 
                                          client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

# Define the authorization scope the app will need
#https://developer.spotify.com/documentation/general/guides/scopes/

authorization_code <- get_spotify_authorization_code(
                                                    client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                                    client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                                    scope = 'playlist-read-private user-library-read')



all_my_fav_tracks <- ceiling(
                            get_my_saved_tracks(
                                                authorization = authorization_code,
                                                include_meta_info = TRUE)[['total']] / 50) %>%
  seq() %>%
  map(function(x)
    {get_my_saved_tracks(authorization = authorization_code,limit = 50, offset = (x - 1) * 50)}) %>%
  reduce(rbind) %>%
  write_rds('raw_all_my_fav_tracks.rds')

artist_from_fav_tracks <-
  all_my_fav_tracks %>%
  select(track.artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  select(id, name)

track_num_artist <-
  artist_from_fav_tracks %>%
  count(id, sort = TRUE) %>%
  left_join(artist_from_fav_tracks, by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(20, n)

track_num_artist %>%
  
  mutate(
    freq = case_when(
      n > 100 ~ 'More than 100 tracks',
      between(n, 50, 99) ~ '50~99 tracks',
      between(n, 20, 49) ~ '20~49 tracks',
      TRUE ~ 'Less than 20 tracks'
    )
  ) %>%
  mutate(freq = factor(
    freq,
    levels = c(
      'More than 100 tracks',
      '50~99 tracks',
      '20~49 tracks',
      'Less than 20 tracks'
    )
  )) %>%
  
  ggplot(mapping = aes(
    x = n,
    y = reorder(name,n),
    fill = freq
  )) +
  geom_col() +
  labs(fill = NULL,title = 'My Favorite Spotify Artists',caption = 'Data from Spotify using spotiyr') +
  xlab('#Songs') +
  ylab('Artist') +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))+
  theme_light() +
  theme(axis.text.x = element_text(angle = -60),
        axis.title = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 15),
        plot.caption = element_text(hjust = 1,face = 'bold.italic'))

my_id <- 'ferdcoz'
my_plists <- get_user_playlists(my_id,authorization = authorization_code)
my_tracks <- get_playlist_tracks('37i9dQZF1EtrBSb4st59uc')
features <- get_track_audio_features(my_tracks$track.id)

ggplot(data = audio_features, aes(x = valence, y = energy, color= artist_name)) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Turbulent/Angry", fontface = "bold") +
  annotate('text', 1.75 / 2, 0.95, label = "Happy/Joyful", fontface = "bold") +
  annotate('text', 1.75 / 2, 0.05, label = "Chill/Peaceful", fontface = "bold") +
  annotate('text', 0.25 / 2, 0.05, label = "Sad/Depressing", fontface = "bold")
