
# 1. Parameters ----------------------------------------------------------------
# ---------------------------------------------------------------------------- #

# https://github.com/fawda123/rStrava/blob/master/README.md

# install.packages('devtools')
# devtools::install_github('fawda123/rStrava')
library(rStrava)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(htmlwidgets)
library(htmltools)
library(gganimate)
library(scales)
library(apexcharter)

# colors
palette_strava = list(orange = "#fc5200",
                      rose   = "#ffb084",
                      grisclair  = "#CCCCCC",
                      #bleu   = "#1e5fea",
                      grisfonce   = "#4D4D4D",
                      noir        = "#000000")
pal_strava = colorRampPalette(palette_strava)

# functions
source("functions/fonctions.R")

# strava app password
config = list()
config = yaml::yaml.load_file(input = file.path("config/config.yml"))


# 2. Get the data --------------------------------------------------------------
# ---------------------------------------------------------------------------- #

# create token
stoken <- httr::config(token = strava_oauth(config$app_name, config$app_client_id, config$app_secret, app_scope="activity:read_all", cache = TRUE))
#stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])


# get athlete informations
myinfo <- get_athlete(stoken, id = config$id_athlete)
myinfo


# get activities
my_acts <- get_activity_list(stoken)
act_data <- compile_activities(my_acts)
act_data$id = as.character(act_data$id)

# List of KOM
kom = get_KOMs(config$id_athlete, stoken)


# 3. summary of the year -------------------------------------------------------
# ---------------------------------------------------------------------------- #

act_data = act_data %>%
  mutate(Date = as.Date(substr(start_date, 1, 10))) %>%
  dplyr::filter(year(Date) < 2022)

act_of_the_year = act_data %>%
  mutate(Date = as.Date(substr(start_date, 1, 10))) %>%
  dplyr::filter(year(Date) == config$year)

# correct virtuals activities
table(act_of_the_year$type)
act_of_the_year = act_of_the_year %>%
  mutate(type = ifelse(type == 'Ride' & is.na(map.summary_polyline), 'VirtualRide', type)) %>%
  mutate(type = ifelse(type == 'VirtualRide' & is.na(map.summary_polyline), 'VirtualRide', type)) %>%
  mutate(type = ifelse(type == 'VirtualRun' & is.na(map.summary_polyline), 'Run', type)) %>%
  mutate(type = ifelse(type == 'Ride', 'Vélo', type)) %>%
  mutate(type = ifelse(type == 'Swim', 'Natation', type)) %>%
  mutate(type = ifelse(type == 'Walk', 'Marche', type)) %>%
  mutate(type = ifelse(type == 'VirtualRide', 'Zwift', type)) %>%
  dplyr::filter(type != "Workout")

# activities per year (last 4 years) ----
comptage = act_data %>%
  mutate(Date = as.Date(substr(start_date, 1, 10))) %>%
  mutate(Annee = year(Date)) %>%
  group_by(Annee) %>%
  summarise(
    nb_activites = n(),
    nb_jours = n_distinct(Date),
    Temps = sum(moving_time, na.rm = TRUE),
    Distance = sum(distance, na.rm = TRUE)) %>%
  mutate(Temps_heures = Temps / 3600) %>%
  dplyr::filter(Annee >= config$year-3) %>%
  mutate(Annee = as.character(Annee))

ggplot(comptage) +
  aes(x = Annee, fill = Annee, y = Temps_heures) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "white") +
  scale_fill_manual(values = pal_strava(length(unique(comptage$Annee)))) +
  labs(title = "Nb d'heures d'activités") +
  geom_text(aes(label = paste(round(Temps_heures, 0), "h")), vjust = 1.6, color = "white", size = 3.5) +
  theme_modern_rc() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggplot(comptage) +
  aes(x = Annee, fill = Annee, y = nb_jours) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "white") +
  scale_fill_manual(values = pal_strava(length(unique(comptage$Annee)))) +
  labs(title = "Nb de jours avec activités") +
  geom_text(aes(label = paste(round(nb_jours, 0), "j")), vjust = 1.6, color = "white", size = 3.5) +
  theme_modern_rc() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggplot(comptage) +
  aes(x = Annee, fill = Annee, y = Distance) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "white") +
  scale_fill_manual(values = pal_strava(length(unique(comptage$Annee)))) +
  labs(title = "Distance en km") +
  geom_text(aes(label = paste(round(Distance, 0), "km")), vjust = 1.6, color = "white", size = 3.5) +
  theme_modern_rc() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


# calendar (last 4 years) ----
comptage = act_data %>%
  mutate(Date = as.Date(substr(start_date, 1, 10))) %>%
  mutate(Annee = year(Date)) %>%
  dplyr::filter(Annee >= config$year-3) %>%
  group_by(Date) %>%
  summarize(Temps = sum(moving_time, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Temps_heures = Temps / 3600)

calendarHeat(comptage$Date, comptage$Temps_heures, ncolors = 5, color = "strava_on", varname="des activités en nb d'heures") # pb sur les jours



# time spent per activity ----
comptage = act_of_the_year %>%
  dplyr::filter(type %in% c('Zwift', 'Vélo', 'Run', 'Natation')) %>%
  group_by(type) %>%
  summarize(Temps = sum(moving_time, na.rm = TRUE),
            nb_activites = n(),
            nb_jours = n_distinct(Date),
            Distance = sum(distance, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(Temps_heures = Temps / 3600) %>%
  arrange(desc(Temps_heures))

comptage$type <- factor(comptage$type, levels = comptage$type) # pour trier par ordre décroissant le graphique

ggplot(comptage) +
  aes(x = type, fill = type, y = Temps_heures) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "white") +
  scale_fill_manual(values = pal_strava(length(comptage$type))) +
  labs(title = "Nb d'heures en 2021 par type d'activité") +
  geom_text(aes(label = paste(round(Temps_heures, 0), "h")), vjust = 1.6, color = "white", size = 3.5) +
  theme_modern_rc() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank())


comptage = comptage %>%
  arrange(desc(Distance))

comptage$type <- factor(comptage$type, levels = comptage$type) # pour trier par ordre décroissant le graphique

ggplot(comptage) +
  aes(x = type, fill = type, y = Distance) +
  geom_bar(stat = "identity", show.legend = FALSE, color = "white") +
  scale_fill_manual(values = pal_strava(length(comptage$type))) +
  labs(title = "Nb de km en 2021 par type d'activité") +
  geom_text(aes(label = paste(round(Distance, 0), "km")), vjust = 1.6, color = "white", size = 3.5) +
  theme_modern_rc() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank())


# time spent per activity and per month ----
comptage = act_of_the_year %>%
  dplyr::filter(type %in% c('Zwift', 'Vélo', 'Run', 'Natation')) %>%
  mutate(Mois = as.character(month(Date))) %>%
  mutate(Mois = gsub(" ","0",sprintf("%2s",Mois))) %>%
  group_by(Mois, type) %>%
  summarize(Temps = sum(moving_time, na.rm = TRUE),
            nb_activites = n(),
            nb_jours = n_distinct(Date),
            Distance = sum(distance, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(Temps_heures = Temps / 3600)

comptage$Mois = factor(comptage$Mois, labels = c('Jan', 'Fév', 'Mar', 'Avr', 'Mai', 'Juin', 'Juil', 'Aoû', 'Sep', 'Oct', 'Nov', 'Déc'))
comptage$type = factor(comptage$type, levels = c('Natation', 'Zwift', 'Vélo', 'Run')) # pour trier par ordre décroissant le graphique


ggplot(comptage) +
  aes(x = Mois, fill = type, y = Temps_heures) +
  geom_bar(position="stack", stat = "identity", show.legend = TRUE, color = "white") +
  scale_fill_manual(values = pal_strava(length(unique(comptage$type)))) +
  labs(x = "", y = "Temps passé en heures", title = "Nb d'heures en 2021 par type d'activité") +
  theme_modern_rc() +
  theme(legend.position="bottom")


# map of ride activities ----
table(act_of_the_year$type)
act_ride_of_the_year = act_of_the_year %>%
  dplyr::filter(type == 'Vélo' & !is.na(start_latitude) # activité avec coordonnées GPS
  )

list_act = act_ride_of_the_year$id

coordonnees_ride = recup_traces_gps(list_act, 1)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Carte des activités vélo")
)  


map = leaflet() %>%
  addProviderTiles(provider = 'CartoDB.DarkMatter',
                   options = providerTileOptions(opacity = 0.95),
                   group = "Carte simplifiée") %>%
  addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga ", attribution = '©Google', group = "Carte GoogleMaps") %>%
  addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = '©Google', group = "Google Satellite") %>%
  addLayersControl(
    baseGroups = c("Carte simplifiée", "Google Satellite", "Carte GoogleMaps"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addControl(title, position = "topleft", className="map-title")

j = 0
for (id_act in list_act) {
  j = j + 1
  print(j)
  
  i = id_act
  coord.df = dplyr::filter(coordonnees_ride, id_act == i)
  
  map = addPolylines(map, lng = coord.df$lon,
                     lat = coord.df$lat,
                     color = palette_strava$orange, opacity = 5/20, weight = 5,
                     popup = paste0(dplyr::filter(act_ride_of_the_year, id == id_act)$name, ", ", round(dplyr::filter(act_ride_of_the_year, id == id_act)$distance, 2), " km"))
}
map

# map of run activities ----
act_run_of_the_year = act_of_the_year %>%
  dplyr::filter(type == 'Run' & !is.na(start_latitude) # activité avec coordonnées GPS
  )

list_act = act_run_of_the_year$id

coordonnees_run = recup_traces_gps(list_act, 1)

title <- tags$div(
  tag.map.title, HTML("Carte des activités running")
)  

map = leaflet() %>%
  addProviderTiles(provider = 'CartoDB.DarkMatter',
                   options = providerTileOptions(opacity = 0.95),
                   group = "Carte simplifiée") %>%
  addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga ", attribution = '©Google', group = "Carte GoogleMaps") %>%
  addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = '©Google', group = "Google Satellite") %>%
  addLayersControl(
    baseGroups = c("Carte simplifiée", "Google Satellite", "Carte GoogleMaps"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addControl(title, position = "topleft", className="map-title")

j = 0
for (id_act in list_act) {
  j = j + 1
  print(j)
  
  i = id_act
  coord.df = dplyr::filter(coordonnees_run, id_act == i)
  
  map = addPolylines(map, lng = coord.df$lon,
                     lat = coord.df$lat,
                     color = palette_strava$rose, opacity = 5/20, weight = 5,
                     popup = paste0(dplyr::filter(act_run_of_the_year, id == id_act)$name, ", ", round(dplyr::filter(act_run_of_the_year, id == id_act)$distance, 2), " km"))
}
map


# animated plot for run activities ----
goal <- 1000
sports <- "Run"
selected.year <- c(2018, 2019, 2020, 2021)

select.df <- act_data[act_data$type %in% sports &
                        year(act_data$start_date_local) %in% selected.year,]

select.df$date <- as.Date(date(select.df$start_date_local))
select.df <- aggregate(distance ~ date, data = select.df, FUN = sum)
select.df <- select.df[order(select.df$date),]

year.df <- data.frame(
  date = unlist(sapply(unique(year(select.df$date)), USE.NAMES = F, FUN = function (yr) {
    as.character(seq(as.Date(paste0(yr, "-01-01")),
                     as.Date(paste0(yr, "-12-31")),
                     by = "day"))
  }))
)
year.df$date <- as.Date(year.df$date)

year.df <- merge(year.df, select.df, by = "date", all.x = T, all.y = F)
year.df <- year.df[order(year.df$date),]
year.df$distance <- ifelse(is.na(year.df$distance), 0, year.df$distance)
year.df$day <- yday(year.df$date)
year.df$year <- year(year.df$date)

year.df %>% group_by(year) %>%
  mutate(cum.distance = round(cumsum(distance))) %>%
  ungroup() %>% as.data.frame() -> year.df

year.df$remaining.distance <- goal - year.df$cum.distance
year.df$days.till.end <- as.numeric(as.Date(paste0(year.df$year, "-12-31")) -
                                      year.df$date)

year.df$dist.per.day.to.goal <- year.df$remaining.distance / year.df$days.till.end
year.df$dist.per.week.to.goal <- year.df$dist.per.day.to.goal * 7

year.df$dist.per.day.to.goal <- round(year.df$dist.per.day.to.goal, 1)
year.df$dist.per.week.to.goal <- round(year.df$dist.per.week.to.goal, 1)

year.df[year.df$dist.per.day.to.goal < 0, "dist.per.day.to.goal"] <- 0
year.df[year.df$dist.per.week.to.goal < 0, "dist.per.week.to.goal"] <- 0
year.df[year.df$remaining.distance < 0, "remaining.distance"] <- 0

year.df[year.df$days.till.end == 0, "dist.per.day.to.goal"] <- "-"
year.df[year.df$days.till.end == 0, "dist.per.week.to.goal"] <- "-"

current.day <- yday(Sys.Date())

p <- ggplot(year.df, aes(x = day, y = cum.distance, col = factor(year), group = year)) +
  geom_line() +
  scale_color_manual(values = pal_strava(length(selected.year))) +
  geom_hline(yintercept = goal, col = "red", lwd = 1) +
  geom_vline(xintercept = current.day, col = alpha("grey", .5), lty = "dashed") +
  geom_segment(x = 1, y = 0, xend = 365, yend = goal, lty = "dashed", col = "red") +
  geom_segment(aes(x = day, y = cum.distance, col = factor(year), group = year),
               xend = 365,
               yend = goal, lty = "dotted") +
  geom_point(size = 2) +
  geom_text(aes(day + 7,
                label = paste0(year, ": ", round(cum.distance))),
            hjust = 0, size = 4) +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(breaks = c(32, 91, 152, 213, 274, 335),
                     labels = c("Fév", "Avr", "Juin", "Août", "Oct", "Déc"),
                     minor_breaks = c(1, 60, 121, 182, 244, 305, 365)) +
  transition_reveal(day) +
  #guides(col = F) +
  labs(x = "", y = "km cumulés", title = "Courses à pied 2018-2021") +
  theme_minimal() +
  theme(plot.margin = margin(5.5, 60, 5.5, 5.5))

p
