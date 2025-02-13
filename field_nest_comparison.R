library(dplyr)
library(ggplot2)

# Combine observer records with field nest records
obs_data = read.csv("exp-field-nest-eval_072423.csv")
field_data = read.csv("field_nest_sample_locations.csv")
field_data = field_data |> filter(year == 2022)
field_data = field_data |> filter(site != "CypressCity")

obs_data$nest_id = paste(obs_data$year,"_",obs_data$colony,
                         "_",obs_data$sample_id, sep="")
obs_data = obs_data %>% filter(colony != "Joule")
field_data$nest_id = paste(field_data$year,"_",field_data$site,
                         "_",field_data$sample_id, sep="")

field_bycolony = field_data |> count(site)
obs_bycolony = obs_data |> count(colony)
merge_obs = inner_join(obs_data, field_data)


# Filter out the random bird records and uncertain human assessments
field_nests = merge_obs %>% filter(real_nest == "yes") |> mutate(real_nest = "T") |>
  filter(human_detect != "U")

calc_humanrecall = function(data, detect_col, ref_col, detect_type){
  
  n_refnests = sum(data[[ref_col]] == "T") # reference column. Either field nests or human detects
  n_Udetects = sum(data[[detect_col]] == "U") # uncertain code in test detection
  test_detections = sum(data[[detect_col]] == "T") # test detections
  test_recall = test_detections/(n_refnests - n_Udetects) # recall where U excluded 
  test_recall_wU = test_detections/(n_refnests) # recall with U counted as a fail
  results = data.frame(detect_type, n_refnests, n_Udetects, test_detections, test_recall, test_recall_wU)
  
  return(results)
}

recall_results = calc_humanrecall(field_nests, "human_detect", "real_nest", "human-real")
  
recall_results = rbind(recall_results, calc_humanrecall(field_nests,
                                                          "AI_nest", 
                                                          "real_nest", 
                                                          "computer-real"))
recall_results = rbind(recall_results, calc_humanrecall(field_nests,
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

ggplot(detections_colony, aes(x=human_recall, y=computer_recall)) +
  geom_point(aes(shape = colony), size = 5) + xlim(0.5,1) + ylim(0.5,1) +
  theme(axis.title = element_text(size = 20), axis.text = element_text(size= 15)) +
  geom_abline(intercept=0, slope = 1, color="blue") +
  xlab("human") + ylab("computer") +
  ggtitle("Recall")
# recall
#combined records for all ground monitored nests
write.csv(field_nests, "field__nest_records.csv")

# records of nests where human and AI disagreed
disagreedTF = field_nests |> filter(human_detect =="T" & AI_nest == "FALSE")
disagreedFT = field_nests |> filter(human_detect =="F" & AI_nest == "TRUE")
uncertain = field_nests |> filter(human_detect =="U")
