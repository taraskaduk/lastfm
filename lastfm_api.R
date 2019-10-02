# CSV downloads:
#THIS IS THE CORRECT EXPORT!!   https://mainstream.ghan.nl/export.html


# Other music APIs, for future reference
# https://www.discogs.com/developers/#
# https://musicbrainz.org/doc/Development/XML_Web_Service/Version_2
# https://developer.spotify.com/documentation/web-api/reference/tracks/get-several-tracks/
# https://github.com/mikkelkrogsholm/musicbrainz/


# Blog posts, for reference
# http://www.jayblanco.com/blog/2016/7/9/using-lastfm-and-r-to-understand-my-music-listening-habits
# https://www.r-bloggers.com/using-last-fm-to-data-mine-my-music-listening-history/
# https://geoffboeing.com/2016/05/analyzing-lastfm-history/

library(tidyverse)
library(jsonlite)
library(rvest)
library(lubridate)


## Insert your own user name
api_user <- "diezzz_htc"
## Insert your API key
api_key <- Sys.getenv("LASTFM_API")


# Recent tracks -----------------------------------------------------------

recent_tracks_import <- read_csv("data/scrobbles.csv") 
max_uts <- max(recent_tracks_import$uts)

append <- NULL
i <- as.numeric(as.POSIXct(Sys.time()))

while(i > max_uts) {
  
  api_recent_tracks <- paste0("https://ws.audioscrobbler.com/2.0/",
                           "?method=user.getrecenttracks",
                           "&user=", api_user,
                           "&api_key=", api_key,
                           "&limit=200",
                           "&to=",i,
                           "&format=json")
  
  api_response <- fromJSON(api_recent_tracks) %>% 
    .$recenttracks %>% 
    .$track %>% 
    flatten() %>% 
    filter(!is.na(date.uts)) 

  colnames(api_response) <- str_replace(colnames(api_response), ".#text", "")
  colnames(api_response) <- str_replace(colnames(api_response), ".mbid", "_mbid")
  
  api_response <- api_response %>% 
    select(uts = date.uts,
           artist,
           artist_mbid,
           album,
           album_mbid,
           track = name,
           track_mbid = mbid)
  
  
  append <- append %>% 
    bind_rows(api_response)
  
  i <- as.numeric(min(api_response$uts))
}


all_tracks <- recent_tracks_import %>% 
  rbind(append) %>% 
  write_csv("data/scrobbles.csv")


all_tracks <- all_tracks %>% 
  mutate_at(c("artist_mbid"), ~if_else(.=="", NA_character_, .))


#NA stats
all_tracks %>% 
  mutate_all(is.na) %>% 
  summarise_all(sum)


# Artists -----------------------------------------------------------------


all_artists <- all_tracks %>% 
  group_by(artist_mbid, artist) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(artist) %>% 
  summarise(n = sum(n),
            artist_mbid = max(artist_mbid, na.rm = TRUE)) %>% 
  ungroup()


get_artist_mbid <- function(search) {
  url <- paste0("https://musicbrainz.org/ws/2/artist/?query=artist:", search)
  if(fromJSON(url)$count == 0) return(NA_character_) else {
  fromJSON(url)$artists[1,1] %>% as.vector()
    }
}


filtered_artists <- all_artists %>% 
  filter(n >= 25)

missing_mbids_artists <- filtered_artists %>% 
  filter(is.na(artist_mbid))

missing_mbids_artists$search <- map_chr(missing_mbids_artists$artist, URLencode)
missing_mbids_artists$artist_mbid <- map_chr(missing_mbids_artists$search, get_artist_mbid)

found_mbids_artists <- missing_mbids_artists %>% 
  select(-c(search)) %>% 
  filter(!is.na(artist_mbid))

filtered_artists <- filtered_artists %>% 
  filter(!is.na(artist_mbid)) %>% 
  bind_rows(found_mbids_artists) %>% 
  distinct()


all_tracks <- all_tracks %>% 
  select(-artist_mbid) %>% 
  left_join(filtered_artists %>% select(-n) %>%  filter(!is.na(artist_mbid)), by = "artist")

write_csv(all_tracks, "data/scrobbles.csv")

filtered_tracks <- all_tracks %>% 
  semi_join(filtered_artists, by = "artist_mbid")


# Album data and tags -----------------------------------------------------

# "https://musicbrainz.org/ws/2/release/0137159c-0ba4-4e0b-bb81-3eac7cbf9b25?inc=artist-credits+tags+discids+recordings&fmt=json"

get_album_data <- function(album_mbid) {
  if (is.na(album_mbid)) {
    return(NA_character_)
  } else {
    url <- url(
      paste0(
        "https://musicbrainz.org/ws/2/release/",
        album_mbid,
        "?inc=artist-credits+tags+discids+recordings&fmt=json"
      )
    )
    
    json <- fromJSON(url)
    
    if (exists("date", where = json)) {
      return(json[["date"]])
    } else return(NA_character_)
  }
}

release_dates <- read_csv("data/release_dates.csv")

filtered_albums <- filtered_tracks %>%
  group_by(album_mbid, album) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(!is.na(album_mbid) & n > 25)

unknown_dates <- filtered_albums %>% 
  select(album_mbid) %>% 
  anti_join(release_dates, by = "album_mbid") %>% 
  mutate(release_date = map_chr(album_mbid, get_album_data))

release_dates <- bind_rows(release_dates, unknown_dates)

filtered_albums <- filtered_albums %>% 
  left_join(release_dates, by = "album_mbid")

filtered_albums %>% 
  filter(!is.na(release_date) & trimws(release_date) != "") %>% 
  select(album_mbid, release_date) %>% 
  write_csv("data/release_dates.csv")

filtered_albums <- filtered_albums %>% 
  mutate(release_date = if_else(trimws(release_date) == "", NA_character_, release_date),
         release_date = if_else(str_count(release_date) == 4, ymd(paste0(release_date,"0101")) ,as_date(release_date)))






# Get tags ----------------------------------------------------------------


get_tags <- function(artist_mbid) {
  api_artist_tags <- paste0(
    "https://ws.audioscrobbler.com/2.0/",
    "?method=artist.gettoptags", 
    #"&artist=",
    "&mbid=", artist_mbid ,
    "&api_key=", api_key,
    "&format=json"
  )
  
  error <- fromJSON(api_artist_tags)$error
  tags <- fromJSON(api_artist_tags)$toptags$tag
  
  if (!is.null(error) || length(tags) == 0) {
    return(NULL)
  } else {
    api_response <- fromJSON(api_artist_tags) %>%
      .$toptags %>%
      .$tag %>%
      filter(name != "seen live") %>%
      head(10) %>%
      mutate(artist_mbid = artist_mbid,
             name = tolower(name)) %>%
      select(-url)
    return(api_response)
  }
}


all_tags <- read_csv("data/all_tags.csv")

missing_tags <- filtered_artists %>% 
  select(artist_mbid) %>% 
  anti_join(all_tags, by = "artist_mbid") %>% 
  distinct() %>% 
  as_vector() %>% 
  map_dfr(get_tags)

all_tags <- bind_rows(all_tags, missing_tags)

write_csv(all_tags, "data/all_tags.csv")



# Tag cleanup -------------------------------------------------------------

all_tags <- all_tags  %>% 
  mutate(tag = str_replace_all(name, "-", "_"),
         tag = str_replace_all(tag, " ", "_"),
         tag = str_replace_all(tag, "sxe", "straight_edge"))


top_tag <- all_tags %>% 
  filter(count == 100) %>% 
  group_by(artist_mbid) %>% 
  summarise(top_tag = first(tag))

filtered_tracks <- filtered_tracks %>% 
  left_join(top_tag, by = "artist_mbid") %>% 
  mutate(date = as.Date(as.POSIXct(as.integer(uts), origin = "1970-01-01")),
         month = floor_date(date, unit = "months"),
         quarter = floor_date(date, unit = "quarter"))

write_csv(filtered_tracks, "data/filtered_tracks.csv")

top_tags <- all_tags %>%  
  group_by(tag) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(n > 9)

write_csv(top_tags, "data/tags_reclass.csv")


cleaned_tags <- all_tags %>% 
  inner_join(top_tags, by = "tag")

cluster_tags <- cleaned_tags %>% 
  select(artist_mbid, tag) %>% 
  distinct() %>% 
  mutate(n = 1) %>% 
  pivot_wider(artist_mbid, names_from = tag, values_from = n, values_fill = list(n = 0))

co_occur <- cluster_tags %>% select(-artist_mbid) %>% as.matrix() 
out <- crossprod(co_occur)
diag(out) <- 0 
dist_matrix <- dist(out, method = "euclidian")

hc <- hclust(dist_matrix, method="ward.D")
plot(hc, hang=-1, xlab="", sub="")

mycl <- cutree(hc, k = c(3,5,10,15))
clusters <- as_tibble(mycl, rownames = NA) %>% rownames_to_column()
colnames(clusters) <- c("tag", "clust_x3", "clust_x5", "clust_x10", "clust_x15")

cleaned_tags <- cleaned_tags %>% 
  left_join(clusters, by = "tag")

write_csv(cleaned_tags, "data/cleaned_tags.csv")

# Tops ------------------------------------------------------------

top_artists <- recent_tracks %>% 
  group_by(artist_mbid, artist) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(pct = n / sum(n),
         pareto = cumsum(pct) %>% round(2)) %>% 
  filter(!is.na(artist_mbid) & pareto <= 0.90)
  


top_albums <- recent_tracks %>% 
  semi_join(top_artists, by = "artist_mbid") %>% 
  group_by(artist, album, album_mbid) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(desc(n))




tags_timeline <- filtered_tracks %>% 
  mutate(date = as.Date(as.POSIXct(as.integer(uts), origin = "1970-01-01")),
         month = floor_date(date, unit = "months"),
         quarter = floor_date(date, unit = "quarter")) %>% 
  select(month, quarter, artist_mbid) %>% 
  left_join(all_tags, by = "artist_mbid")

tags_timeline %>% 
  rename(tag = name) %>% 
  filter(tag %in% c("nu metal")) %>% 
  group_by(quarter, tag) %>% 
  tally() %>% 
  ggplot(aes(x=quarter, y = n, col = tag)) +
    geom_line()


filtered_tracks %>% distinct() %>% count()


tags_timeline %>% 
  rename(tag = name) %>% 
  filter(tag %in% c('christian') & quarter == '2012-01-01') %>% 
  select(artist_mbid) %>% 
  distinct() %>% 
  left_join(filtered_artists, by = "artist_mbid") %>% 
  arrange(desc(n))





artist_stats <- filtered_tracks %>% 
  group_by(artist_mbid, artist, quarter) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(artist_mbid, artist) %>% 
  summarise(var = var(n),
            sd = sd(n))
