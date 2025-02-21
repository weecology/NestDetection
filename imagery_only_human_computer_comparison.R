
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(ggbeeswarm)
library(patchwork)

FindDiffs <- function(data, human, computer) {
  disagreed.nests.Tno <- data %>% filter(data[[human]] == "T" &
   data[[computer]]  == "no")
  disagreed.nests.Fyes <- data %>% filter(data[[human]] == "F" &
                                            data[[computer]] == "yes")
  disagreed.nests = rbind(disagreed.nests.Tno, disagreed.nests.Fyes)
  return(disagreed.nests)
  # write.csv(disagreed_nests, "nest_diffs.csv")
}

CalculateMetrics <- function(data, computer_col) {
  # calculates precision and recall
  precision = sum(data$nest == "T" & data[computer_col] == "yes") / sum(data[computer_col] == "yes")
  recall = sum(data$nest == "T" & data[computer_col] == "yes") / sum(data$nest == "T")
  output = c(precision, recall)
  return(output)
}

JoinInputs <- function(human.data, computer.data) {
  # joins human and computer data, drops duplicate columns, and cleans column names
  # keeps only computer data when there is human data
  human.data = human.data |> unite("prime_ID", c(site, year, sample_id), remove = FALSE)
  computer.data = computer.data |> unite("prime_ID", c(site, year, sample_id), remove = FALSE)
  output = left_join(human.data, computer.data, by = "prime_ID")
  output = output |>
    select(-c(site.y, year.y, sample_id.y)) |>
    rename(
      year = year.x,
      site = site.x,
      sample_id = sample_id.x,
    )

}

EvaluateNests = function(data, computer_col){
  #subsets data by year and colony and calculates precision and recall
  data = data |> filter(nest == "T" | nest == "F")
  sites = unique(data$site)
  years = unique(data$year)
  results = data.frame(Year = integer(), 
                       Site=character(), 
                       Precision = double(), 
                       Recall =double())
  for (x in years) {
    for (y in sites) {
      site.year.data = data |> filter(year == x & site == y)
      site.year.metrics = CalculateMetrics(site.year.data, computer_col)
      results = results |> add_row(Year=x, 
                                   Site=y, 
                                   Precision=site.year.metrics[1], 
                                   Recall=site.year.metrics[2])
    }
  }
  return(results)
}

# DATA PROCESSING  

# read human observations (visual.input) and model detections (computer.input)
visual.input = read.csv("imagery_only_human.csv")
computer.input = read.csv("nest_detector_imagery_sample_locations.csv")

# keep only colonies with ground control points and weekly flights
computer.input = computer.input |> filter(site %in% c("Jerrod","Joule", 
                                                      "Vacation", "6thBridge",
                                                      "StartMel", "JetportSouth"))

# merge human and computer files by assessment location

joined.data = JoinInputs(visual.input, computer.input) 

# Create computer nest prediction column for bird-bird-bird 
#    joined.data contains nest predictions (known_nest = yes or no) using the 
#    bird-bird rule (i.e. 2 consecutive bird detections or 3+ detections over the 
#    time series). Because bird-bird-bird (3+ detections over the time series) 
#    is a subset of the bird-bird rule, we filtered observations to remove all 
#    nest detections where num_obs (number of bird detections) = 2.
joined.data = joined.data |> mutate(bird.3.nest = ifelse(known_nest == 'yes' & num_obs == 2, NA, known_nest))

# Make analysis data.frames  
#   for sites with human prediction (nest.data). Sample size for nest.data drops to 
#   794 because nests without a human assessment are dropped in the join. This 
#   includes both unassessed colonies (Joule 2022) and nest locations scored as U (Uncertain)
nest.data = joined.data |>  filter(nest == "T" | nest == "F")

# CALCULATE PRECISION AND RECALL AND SAMPLE SIZE
#   Bird-bird+
bird.bird.sample = nest.data |> group_by(site, year) |> summarise(count = n())
bird.bird.plus = EvaluateNests(nest.data, "known_nest") 
bird.bird.plus$detections = "2+"
avgbird2.recall = mean(na.omit(bird.bird.plus$Recall))
avgbird2.precision = mean(na.omit(bird.bird.plus$Precision))

# Bird-bird-bird
data.bird.bird.bird = nest.data |> filter(!is.na(bird.3.nest))
bird3.nest.sample = data.bird.bird.bird |> group_by(site, year) |> summarise(count = n())
bird.bird.bird = EvaluateNests(data.bird.bird.bird, "bird.3.nest")
bird.bird.bird$detections = "3+"

# POST-HOC INTERPRETATION

## Generate list of nests where algorithm and human differed in assessment
nest.diffs.2 = FindDiffs(nest.data, "nest", "known_nest")
nest.diff.3 = FindDiffs(nest.data, "nest", "bird.3.nest")
false.positive2 = sum(nest.data$nest == "F" & nest.data$known_nest == "yes")
false.negative2 = sum(nest.data$nest == "T" & nest.data$known_nest == "no")
false.positive3 = sum(nest.data$nest == "F" & nest.data$bird.3.nest == "yes")
false.negative3 = sum(nest.data$nest == "T" & nest.data$bird.3.nest == "no")

## Assess Uncertain human assessments
### Extract human assessment = U (uncertain) records
uncertain = joined.data |>  filter(nest == "U")

## Assess impact of Jetport South 2022 colony
drop.Jetport22 = bird.bird.bird |> filter(Site !="JetportSouth" | Year !="2022")
avg.nojet22.recall = mean(na.omit(drop.Jetport22$Recall))
avg.nojet22.precision = mean(na.omit(drop.Jetport22$Precision))

# MAKE RESULTS FIGURES

## Figure 5 and 6: By Year and Colony - bird-bird-bird only (Figure 5)

###   organize data for plots
colony.year.results = bird.bird.bird |> 
  gather(metric, value, Precision:Recall) |>
  filter(!is.na(value))
  
average.metrics = bird.bird.bird |> 
  summarise(Precision = mean(na.omit(Precision)), Recall = mean(na.omit(Recall))) |> 
  gather(key="metric", value = "value")

### Figure 5: By Year
figure5 = ggplot(colony.year.results, aes(x=factor(Year), y=value)) +
  geom_violin(trim=FALSE, draw_quantiles = c(0.5)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1, position = position_dodge(.9)) +
  ylim(0,1) +
  labs(x = "Year", y = "Performance Metric") +
  theme(axis.title = element_text(size = 20), axis.text = element_text(size= 15),
        plot.title = element_text(size = 20), strip.text.y = element_text(size = 20), strip.text = element_text(size = 20)) +
  facet_wrap(~metric, nrow=2) + geom_hline(data=average.metrics, aes(yintercept= value),linetype=3, size=1)

### Figure 6: By Colony
figure6 = ggplot(colony.year.results, aes(x=factor(Site), y=value)) +
  geom_violin(trim=FALSE, draw_quantiles = c(0.5)) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  ylim(0,1) + facet_grid(rows="metric") +
  labs(x = "Site", y = "Performance Metric") +
  theme(axis.title = element_text(size = 20), axis.text.y = element_text(size=15), 
        axis.text.x = element_text(size= 15, angle = 90, vjust=0.5),
        plot.title = element_text(size = 20), strip.text = element_text(size = 20)) +
  facet_wrap(~metric, nrow=2) + geom_hline(data=average.metrics, aes(yintercept= value),linetype=3, size=1)

# Figure 7: Bird-bird+ vs bird-bird-bird

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

##   reorganize data for plotting 
precision.recall.2.3 = left_join(bird.bird.plus, bird.bird.bird, by=c("Site"="Site", "Year"="Year"))
precision.recall.2.3 = precision.recall.2.3 |> rename(Precision2=Precision.x, Precision3 = Precision.y, Recall2=Recall.x, Recall3=Recall.y)
precision.recall.2.3 = precision.recall.2.3 |> select(c(-detections.x, -detections.y)) 

##   Figure 7: recall and precision 
plot.precision.23 = ggplot(precision.recall.2.3, aes(x=Precision2, y=Precision3)) +
  geom_point(aes(shape=factor(Year), color=Site), show.legend=FALSE, size = 4) + 
  scale_color_manual(values=cbPalette) +
  xlim(0.5,1.01) + 
  ylim(0.5,1.01) + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size= 10)) +
  geom_abline(intercept=0, slope = 1) + 
  xlab("bird-bird+") + ylab("bird-bird-bird") +
  ggtitle("Precision") +
  theme_bw()

plot.recall.23 = ggplot(precision.recall.2.3, aes(x=Recall2, y=Recall3)) +
  geom_point(aes(shape = factor(Year), color = Site), size = 4) + 
  scale_color_manual(values=cbPalette) +
  xlim(0.5,1.01) + 
  ylim(0.5,1.01) + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size= 10)) +
  geom_abline(intercept=0, slope = 1) + 
  xlab("bird-bird+") + ylab("bird-bird-bird") +
  labs(shape = "Year") +
  ggtitle("Recall") +
  theme_bw() +
  theme(legend.position="bottom")

figure7 = plot.precision.23/plot.recall.23


# PRINT FIGURES
tiff("figure5.tiff",
     width = 4600,
     height = 4600, 
     units = "px", 
     res = 800, 
     #    compression = "lzw",
     bg = "white", 
     pointsize = 5)
plot(figure5)
dev.off()

tiff("figure6.tiff",
     width = 4600,
     height = 4600, 
     units = "px", 
     res = 800, 
     #    compression = "lzw",
     bg = "white", 
     pointsize = 5)
plot(figure6)
dev.off()

#   bird-bird-bird vs. bird-bird-plus
tiff("figure7.tiff",
    width = 4600,
    height = 4600, 
    units = "px", 
    res = 800, 
#    compression = "lzw",
    bg = "white", 
    pointsize = 5)

# Creating a plot
plot(figure7)
dev.off()
# Closing the graphical device
dev.off() 



