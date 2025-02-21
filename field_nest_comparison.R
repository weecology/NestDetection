library(dplyr)
library(ggplot2)

calc_recall = function(data, detect_col, ref_col, detect_type){
  
  n_refnests = sum(data[[ref_col]] == "T") # Either field nests or human detects
  n_Udetects = sum(data[[detect_col]] == "U") # number of Uncertains
  test_detections = sum(data[[detect_col]] == "T") # Nest detections
  test_recall = test_detections/(n_refnests - n_Udetects) # recall where U excluded 
  test_recall_wU = test_detections/(n_refnests) # recall with U counted as a fail
  results = data.frame(detect_type, n_refnests, n_Udetects, test_detections, test_recall, test_recall_wU)
  
  return(results)
}

# DATA PROCESSING
## Load observer and algorithm observations and field nest records
obs_data = read.csv("exp-field-nest-eval_072423.csv")
field_data = read.csv("field_nest_sample_locations.csv")

## Filter field data records 
### field data includes 2021 data (our first attempt at flagging nests that revealed how difficult it was)
### and a colony without ground control points
field_data = field_data |> filter(year == 2022)
field_data = field_data |> filter(site != "CypressCity")

## Create unique nest_ids for each record 
obs_data$nest_id = paste(obs_data$year,"_",obs_data$colony,
                         "_",obs_data$sample_id, sep="")
obs_data = obs_data %>% filter(colony != "Joule")
field_data$nest_id = paste(field_data$year,"_",field_data$site,
                         "_",field_data$sample_id, sep="")

## Join field and observer files on unique nest id
merge_obs = inner_join(obs_data, field_data)

# Filter data to keep only real nests and remove uncertain human assessments
## Also changed real_nest = "yes" to T for consistency.
field_nests = merge_obs %>% filter(real_nest == "yes") |> 
  mutate(real_nest = "T") |>
  filter(human_detect != "U")

# Generate raw sample sizes
field_bycolony = field_data |> count(site)
obs_bycolony = obs_data |> count(colony)

# Calculate Recall
## Human observations
recall_results = calc_recall(field_nests, 
                             "human_detect", 
                             "real_nest", 
                             "human-real")

## Calculate and add algorithm results  
recall_results = rbind(recall_results, calc_recall(field_nests,
                                                   "AI_nest",
                                                   "real_nest",
                                                   "computer-real"))
## Calculate and add human vs. algorithm results 
recall_results = rbind(recall_results, calc_recall(field_nests,
                                                   "AI_nest", 
                                                   "human_detect", 
                                                   "computer-human"))


detections_colony = field_nests %>% group_by(colony) %>% 
  summarise(total=length(year), human_U = sum(human_detect == "U"), human=sum(human_detect=="T"),
            computer=sum(AI_nest=="T"))

total_counts = field_nests %>%  summarise(colony = "all", 
                                          total=sum(real_nest == "T"), 
                                          human_U = sum(human_detect == "U"), 
                                          human=sum(human_detect=="T"), 
                                          computer=sum(AI_nest=="T"))
detections_colony = bind_rows(detections_colony, total_counts)

detections_colony = detections_colony |> mutate(human_recall = human/(total - human_U),
                                                computer_recall = computer/total,
                                                comp_human_compare = computer/human)

total_nests=sum(detections_colony$total)

# Find human and algorithm disagreements for post-hoc assessment
disagreedTF = field_nests |> filter(human_detect =="T" & AI_nest == "FALSE")
disagreedFT = field_nests |> filter(human_detect =="F" & AI_nest == "TRUE")
uncertain = field_nests |> filter(human_detect =="U")

# write combined records for all ground monitored nests
write.csv(field_nests, "field__nest_records.csv")


