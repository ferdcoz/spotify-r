---
author: "Fernando Coz"
date: "10/4/2020"
---

# spotify-r
Analyzing Spotify data using R

# Brief
Data is everywhere, public or private, but it exists. In our day-to-day we exchange and interact with several platforms and create data every minute, every second. And Spotify it's not the exception to the rule. We search for new artists, songs, we playback our favorites ones in a constant loop or maybe we just 'like' a song we haven't listened since we were younger.

What if we can explore this data and get some insights? Well...let's start...or should I say PLAY!

# 1. Let's handshake with Spotify
Ok. How can we get this data? Well, I like playing around with APIs, JSON and security tokens, so I decided to go the ['Spotify for developers'](https://developer.spotify.com/) way. If you're not familiar with these terms, don't panic and keep reading hopefully you'll learn something new today.

# Application Registration
1. [Log in](https://developer.spotify.com/dashboard/login) and create a new application.
2. Give the application a name, a description and accept the terms and conditions (No commercial activity).
3. Take note of your application's `CLIENT_ID` and `CLIENT_SECRET`. You'll need them later. And don't share them :)

# 2. spotifyr
It all starts importing the outstanding 'spotifyr' R package developed and broadcasted by [Charlie Thompson](https://www.linkedin.com/in/charlie-t-89980118b/), part of the Spotify Data Scientist team. Following an extract from the [official documentation](https://github.com/charlie86/spotifyr):

'spotifyr' is an R wrapper for pulling track audio features and other information from Spotify’s Web API in bulk. By automatically batching API requests, it allows you to enter an artist’s name and retrieve their entire discography in seconds, along with Spotify’s audio features and track/album popularity metrics. You can also pull song and playlist information for a given Spotify User.

# Authentication
First I'll store `CLIENT_ID` and `CLIENT_SECRET` credentials as R System Environment variables since those will be necessary in each function call as part of the arguments. You can set them manually in each subsequent function call. Not my preferred choice.

``` r
Sys.setenv(CLIENT_ID = 'xxxxx')
Sys.setenv(CLIENT_SECRET = 'xxxxx')

authorization_code <- get_spotify_access_token(
                                                client_id = Sys.setenv("CLIENT_ID"),
                                                client_secret = Sys.setenv("CLIENT_SECRET")
                                                scope='x')
```
# Staging data
In order to avoid any delay and constantly sending requests to the Spotify endpoint, I decided to store the data into a .rds object and work with it along my development.

Spotify allows you to get up to 50 items per request, but that's not enough. Therefore, `offset` come into picture to request as much as we need, sequentially.
``` r
all_my_fav_tracks <-
  ceiling(
    get_my_saved_tracks(
      authorization = authorization_code,
      include_meta_info = TRUE)[['total']] / 50) %>%

  seq() %>%

  map(
   function(x) 
   {
    get_my_saved_tracks(
    authorization = authorization_code,
    limit = 50, offset = (x - 1) * 50)
   }
     ) %>% 

reduce(rbind) %>%

write_rds('my_favorite_songs.rds')
```
# 3. Pure R
Favorite artists
In order to get the list of my favorite artists and how many songs I like, we need to take into account that a certain songs could be written or played by not only one artist. So the `track.artists` is a list itself, reduce helps fanning out the rows.
``` r
artist_from_songs <-
  my_favorite_songs %>%
  select(track.artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  select(id,name)

track_num_artist <-
  artist_from_songs %>%
  count(id, sort = TRUE) %>%
  left_join(artist_from_songs, by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(20, n)
```
# 4. Let's ggplot it
Tables are good, but not as good a bars, so, let's ggpolot it
``` r
track_num_artist %>%
ggplot(
    mapping = aes(
                  x = n,
                  y = reorder(name,n),
                  fill = freq
                  )
       ) +

  geom_col() +

  labs(fill = NULL,
       title = 'My Favorite Spotify Artists',
       caption = 'Data from Spotify using spotiyr') +

  xlab('#Songs') +

  ylab('Artist') +

  geom_text(aes(label = n), position = position_stack(vjust = 0.5))+

  theme_light() +

  theme(axis.text.x = element_text(angle = -60),
        axis.title = element_text(face = 'bold'),

        plot.title = element_text(hjust = 0.5, face = 'bold', size = 15),
        plot.caption = element_text(hjust = 1,face = 'bold.italic'))
```
# 5. Sentiment Analysis
Last but not least. We could also get an index defined by Spotify as: `valence`. Defined as:

"A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."

Combining valance with the energy index for each, we're now able to map songs in a magic quadrant (Inspired in Sentify - created by Charlie Thompson). The 4 quadrants are the following:

 1. Turbulent/Angry: more `ENERGY` less `VALENCE`
 2. Happy/Joyful: more `VALENCE` more `ENERGY`
 3. Sad/Depressing: less `ENERGY` less `VALENCE`
 4. Chill/Peaceful: more `VALANCE` less `ENERGY`

And all the combinations in between. Let's see how my (or most of them) favorite songs and artists distribution looks like:

Turbulent/Angry? - I blame Kurt Cobain, Krist Novoselic and Dave Grohl for that.


--------------------------------------------------------------------------------------------------------------------------------------
# References , credits and useful links:

[Spotify for developers](https://developer.spotify.com/) - All technical details, if needed, to get the data you're looking for.

[Github](https://github.com/charlie86/spotifyr) - Official Github 'spotifyr' repository. Credits to [Charlie Thompson](https://www.linkedin.com/in/charlie-t-89980118b/).

[Sentify](http://rcharlie.net/sentify/) - R shiny app. You can search for your favorite artist and see how the valence/energy quadrant looks like for each song.

--------------------------------------------------------------------------------------------------------------------------------------
