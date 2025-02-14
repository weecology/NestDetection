library(dplyr)
library(lubridate)
library(sf)
library(stringr)

# Load field nests
unzip("data/nest_detections_processed.zip", exdir = "data")
detector_nests <- st_read("data/nest_detections_processed.shp") %>%
  st_transform(4326) %>%
  st_centroid() #Force values to points
detector_nest_coords <- st_coordinates(detector_nests) %>%
  as.data.frame() %>%
  select(lat = Y, long = X)
detector_nests <- detector_nests %>%
  as.data.frame() %>%
  bind_cols(detector_nest_coords) %>%
  rename(site = Site, year = Year)

# Load field nest samples
sample_locations <- read.csv("experiments/nest_detector_imagery_sample_locations.csv")

# Load bird predictions

unzip("data/PredictedBirds.zip", exdir = "data")

birds <- st_read("data/PredictedBirds.shp") %>%
  filter(score > 0.3) %>% #Make sure there's a good chance we're focusing on an area with a bird
  st_transform(4326) %>%
  st_centroid() #Force values to points
bird_coords <- st_coordinates(birds) %>%
  as.data.frame() %>%
  select(lat = Y, long = X)
birds <- birds %>%
  as.data.frame() %>%
  bind_cols(bird_coords) %>%
  mutate(species = label) %>%
  mutate(event = as.Date(Date, "%m_%d_%Y")) %>%
  mutate(year = year(event)) %>%
  group_by(Site, Year) %>%
  select(site = Site, year = Year, species, bird_id, lat, long)

# Add random bird locations to field nests
# Provides the sampling locations so reviewers don't know that a
# region they are shown is definitely a field nest

set.seed(26) # Keep same nest number and fake nest locations across runs

get_new_samp_locs <- function(focal_birds, focal_nests, samples) {
  num_nests <- nrow(focal_nests)
  num_samples <- min(25, num_nests)

  # Get list of birds identified as being in nests
  birds_in_nests = c()
  for (match in focal_nests$bird_match){
    birds_in_nests <- unique(c(birds_in_nests, as.numeric(str_split(match, ",")[[1]])))
  }

  random_birds <- focal_birds %>%
    filter(!(bird_id %in% birds_in_nests) ) %>%
    mutate(known_nest = "no", nest_id = bird_id) %>%
    slice_sample(n = num_samples) %>%
    select(site, year, known_nest, nest_id, lat, long, species) %>%
    mutate(num_obs = 1)

  detector_nests <- focal_nests %>%
    mutate(known_nest = "yes") %>%
    select(site, year, known_nest, nest_id, lat, long, species, num_obs) %>%
    slice_sample(n = num_samples) %>%
    rbind(random_birds) %>%
    slice(sample(1:n())) %>% # Randomize order to not give clues to real nests
    mutate(sample_id = seq_len(n())) %>%
    select(site, year, sample_id, known_nest, nest_id, species, lat, long, num_obs)

  return(detector_nests)
}

surveys <- unique(detector_nests[c("site", "year")])
samp_locs_updated <- sample_locations
changes_made <- FALSE
for (i in seq_len(nrow(surveys))) {
  focal_site <- surveys[i, ]["site"][[1]]
  focal_year <- surveys[i, ]["year"][[1]]
  focal_nests <- filter(detector_nests, site == focal_site, year == focal_year)
  focal_sample_locations <- filter(sample_locations, site == focal_site, year == focal_year)
  focal_birds <- filter(birds, site == focal_site, year == focal_year)
  nest_count <- nrow(focal_nests)
  samp_count_yes <- nrow(filter(focal_sample_locations, known_nest == "yes"))
  samp_count_no <- nrow(filter(focal_sample_locations, known_nest == "no"))
  if (samp_count_yes == 0 & samp_count_no == 0) {
    new_samp_locs <- get_new_samp_locs(focal_birds, focal_nests, samples=25)
    samp_locs_updated <- rbind(samp_locs_updated, new_samp_locs)
    changes_made <- TRUE
  } else if (samp_count_yes != nest_count | samp_count_no != nest_count) {
    stop("Field nest counts and sample counts don't match and sample counts are not zero. Help!")
  }
}

if (changes_made){
  message("Updating sampling file with for new detector nests")
  write.csv(samp_locs_updated, "experiments/nest_detector_imagery_sample_locations.csv", row.names = FALSE)
} else {
  message("No updates required")
}
