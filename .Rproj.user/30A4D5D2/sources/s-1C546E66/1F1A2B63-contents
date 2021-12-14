# Data Wrangling ----------------------------------------------------------

# Setting up access to the Spotify Api

Sys.setenv(SPOTIFY_CLIENT_ID = "a4ba95a97a124757ac4f567562586c4a")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "aba4119f95024440b3f72949ce918186")

access.token <- get_spotify_access_token()


# Wrangling the playlist data

Canada <- get_playlist_audio_features("Spotify", "37i9dQZEVXbMda2apknTqH")
Japan <- get_playlist_audio_features("Spotify", "37i9dQZEVXbKzoK95AbRy9")
Belgium <- get_playlist_audio_features("Spotify", "37i9dQZEVXbND4ZYa46PaA")
Egypt <- get_playlist_audio_features("Spotify", "37i9dQZEVXbMy2EcFg5F9m")
Brazil <- get_playlist_audio_features("Spotify", "37i9dQZEVXbKzoK95AbRy9")
Isreal <- get_playlist_audio_features("Spotify", "37i9dQZEVXbJ5J1TrbkAF9")


# Separating the data of interest and transforming it to be able to combine the
# individual sets into one full frame for Josh's graphs

c <- Canada %>% 
  #arrange(-instrumentalness) %>% 
  select(track.name, instrumentalness, danceability, acousticness)

c <- add_column(c, "Canada")
names(c)[3] <- "Danceability"
names(c)[4] <- "Acousticness"
names(c)[5] <- "Country"


j <- Japan %>% 
  #arrange(-instrumentalness) %>% 
  select(track.name, instrumentalness, danceability, acousticness)

j <- add_column(j, "Japan")
names(j)[3] <- "Danceability"
names(j)[4] <- "Acousticness"
names(j)[5] <- "Country"


bel <- Belgium %>% 
  #arrange(-instrumentalness) %>% 
  select(track.name, instrumentalness, danceability, acousticness)

bel <- add_column(bel, "Belgium")
names(bel)[3] <- "Danceability"
names(bel)[4] <- "Acousticness"
names(bel)[5] <- "Country" 


e <- Egypt %>% 
  #arrange(-instrumentalness) %>% 
  select(track.name, instrumentalness, danceability, acousticness)

e <- add_column(e, "Egypt")
names(e)[3] <- "Danceability"
names(e)[4] <- "Acousticness"
names(e)[5] <- "Country"


bra <- Brazil %>% 
  #arrange(-instrumentalness) %>% 
  select(track.name, instrumentalness, danceability, acousticness)

bra <- add_column(bra, "Brazil")
names(bra)[3] <- "Danceability"
names(bra)[4] <- "Acousticness"
names(bra)[5] <- "Country"


i <- Isreal %>% 
  #arrange(-instrumentalness) %>% 
  select(track.name, instrumentalness, danceability, acousticness)

i <- add_column(i, "Israel")
names(i)[3] <- "Danceability"
names(i)[4] <- "Acousticness"
names(i)[5] <- "Country"


# Combining all the individual data sets into one full data set
fulllist <- rbind(c, j, bel, e, bra, i)

write.csv(fulllist, "Data - Clean/fulllist.csv")


# Khay's Data cleaning

# Valance -> Objects per country -------------------------------------------------

#Canda
Ca.V <- data.frame(Canada$valence)
Ca.V <- add_column(Ca.V,"Canada")
names(Ca.V)[1] <- "valence"
names(Ca.V)[2] <- "Country"
#Japan
Jp.V <-data.frame(Japan$valence)
Jp.V <- add_column(Jp.V,"Japan")
names(Jp.V)[1] <- "valence"
names(Jp.V)[2] <- "Country"

#Belgium
Be.V <- data.frame(Belgium$valence)
Be.V <- add_column(Be.V,"Belgium")
names(Be.V)[1] <- "valence"
names(Be.V)[2] <- "Country"


#Egypt
Eg.V <- data.frame(Egypt$valence)
Eg.V <- add_column(Eg.V,"Egypt")
names(Eg.V)[1] <- "valence"
names(Eg.V)[2] <- "Country"

#Brazil
Br.V <- data.frame(Egypt$valence)
Br.V <- add_column(Br.V,"Brazil")
names(Br.V)[1] <- "valence"
names(Br.V)[2] <- "Country"

#Isreal
Il.V <- data.frame(Isreal$valence)
Il.V <- add_column(Il.V,"Isreal")
names(Il.V)[1] <- "valence"
names(Il.V)[2] <- "Country"

#combining valences into one object
Valance.C <-rbind(Ca.V, Br.V, Eg.V, Il.V, Jp.V, Be.V)

# Tempo -> object per country ---------------------------------------------

#Canda
Ca.T <- data.frame(Canada$tempo)
Ca.T <- add_column(Ca.T,"Canada")
names(Ca.T)[1] <- "tempo"
names(Ca.T)[2] <- "Country"
#Japan
Jp.T <-data.frame(Japan$tempo)
Jp.T <- add_column(Jp.T,"Japan")
names(Jp.T)[1] <- "tempo"
names(Jp.T)[2] <- "Country"

#Belgium
Be.T <- data.frame(Belgium$tempo)
Be.T <- add_column(Be.T,"Belgium")
names(Be.T)[1] <- "tempo"
names(Be.T)[2] <- "Country"


#Egypt
Eg.T <- data.frame(Egypt$tempo)
Eg.T <- add_column(Eg.T,"Egypt")
names(Eg.T)[1] <- "tempo"
names(Eg.T)[2] <- "Country"

#Brazil
Br.T <- data.frame(Egypt$tempo)
Br.T <- add_column(Br.T,"Brazil")
names(Br.T)[1] <- "tempo"
names(Br.T)[2] <- "Country"

#Isreal
Il.T <- data.frame(Isreal$tempo)
Il.T <- add_column(Il.T,"Isreal")
names(Il.T)[1] <- "tempo"
names(Il.T)[2] <- "Country"

#combining tempos into one object
Tempo.C <-rbind(Ca.T, Br.T, Eg.T, Il.T, Jp.T, Be.T)
Data <- cbind(Tempo.C, Valance.C)

write.csv(Data, "Data - Clean/Data-K.csv")


# Sage Data Cleaning --------------------------------

#mean of each country energy 
mean.canada <- mean(Canada$energy) # 0.60674 
mean.japan <- mean(Japan$energy) # 0.73404
mean.brazil <- mean(Brazil$energy) # 0.73404 
mean.isreal <- mean(Isreal$energy) # 0.61486 
mean.egypt <- mean(Egypt$energy) # 0.65026 
mean.belgium <- mean(Belgium$energy) # 0.65072 

means.all <- c(mean.canada, mean.japan, mean.brazil, mean.isreal, mean.egypt, mean.belgium)
df.means.all <- data.frame(means.all)
names <- c("Canada", "Japan", "Brazil", "Isreal", "Egypt", "Belgium")



# Sorting country data to create visualizations
# Canada 
canada.data <- Canada %>%
  select(track.name, energy, mode)

canada.data <- add_column(canada.data, "Canada")
names(canada.data)[4] <- "Country"

#Japan
japan.data <- Japan %>%
  select(track.name, energy, mode)

japan.data <- add_column(japan.data, "Japan")
names(japan.data)[4] <- "Country"

# Brazil 
brazil.data <- Brazil %>%
  select(track.name, energy, mode)

brazil.data <- add_column(brazil.data, "Brazil")
names(brazil.data)[4] <- "Country"

# Israel 
israel.data <- Isreal %>%
  select(track.name, energy, mode)

israel.data <- add_column(israel.data, "Isreal")
names(israel.data)[4] <- "Country"

# Egypt 
egypt.data <- Egypt %>%
  select(track.name, energy, mode)

egypt.data <- add_column(egypt.data, "Egypt")
names(egypt.data)[4] <- "Country"

# Belgium 
belgium.data <- Belgium %>%
  select(track.name, energy, mode)

belgium.data <- add_column(belgium.data, "Belgium")
names(belgium.data)[4] <- "Country"


# combining all of them 

all.countries <- rbind(canada.data, japan.data, brazil.data, israel.data, egypt.data, belgium.data)

write.csv(all.countries, "Data - Clean/all.countries.csv")


# Next run DataVis.R in the Scripts folder.

