# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(factoextra)
#functions------------------
read_data <- function(file_name){
  read_excel(here("data", "onet", file_name))%>%
    clean_names()%>%
    select(o_net_soc_code, element_name, scale_name, data_value)%>%
    pivot_wider(names_from = scale_name, values_from = data_value)%>%
    mutate(score=sqrt(Importance*Level), #geometric mean of importance and level
           #mutate(score=Level,
           category=(str_split(file_name,"\\.")[[1]][1]))%>%
    unite(element_name, category, element_name, sep=": ")%>%
    select(-Importance, -Level)
}
rescale_custom <- function(x, a = exp(-1), b = 1) {
  a + (x - min(x, na.rm = TRUE)) * (b - a) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# the program------------------------
mapping <- read_excel(here("data","mapping", "onet2019_soc2018_noc2016_noc2021_crosswalk_consolodated.xlsx"))%>%
  mutate(noc2021=str_pad(noc2021, "left", pad="0", width=5))%>%
  unite(noc, noc2021, noc2021_title, sep=": ")%>%
  select(noc, o_net_soc_code = onetsoc2019)%>%
  distinct()

#the onet data-----------------------------------
onet_raw <- tibble(file=c("Skills.xlsx", "Abilities.xlsx", "Knowledge.xlsx", "Work Activities.xlsx"))%>%
  #tbbl <- tibble(file=c("Skills.xlsx"))%>%
  mutate(data=map(file, read_data))%>%
  select(-file)%>%
  unnest(data)%>%
  pivot_wider(id_cols = o_net_soc_code, names_from = element_name, values_from = score)%>%
  inner_join(mapping)%>%
  ungroup()%>%
  select(-o_net_soc_code)%>%
  select(noc, everything())%>%
  group_by(noc)%>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))%>% #mapping not one to one: mean gives one value per NOC
  mutate(across(where(is.numeric), ~ if_else(is.na(.), mean(., na.rm=TRUE), .)))|> #11 nocs 4 measures replace na with mean
  column_to_rownames("noc")

onet_pca <- prcomp(onet_raw, center=TRUE, scale=TRUE)

factoextra::fviz_screeplot(onet_pca)#elbow at 5

onet_scores <- onet_pca$x[, 1:5]#keep first five

onet_similarity<- dist(onet_scores, method = "manhattan")|>
  as.matrix()|>
  as.data.frame()|>
  rownames_to_column("origin")|>
  pivot_longer(cols=-origin, names_to = "destination", values_to = "distance")|>
  filter(origin!=destination)|>
  ungroup()|>
  mutate(similarity=exp(-distance/max(distance)))

#demand (from lmo)

demand <- read_excel(here("data", "lmo", "supply_demand.xlsx"), skip = 3)|>
  filter(Variable=="Job Openings")|>
  pivot_longer(cols=starts_with("2"))|>
  unite(destination, NOC, Description, sep=": ")|>
  mutate(destination=str_replace_all(destination, "#",""))|>
  clean_names()|>
  group_by(destination)|>
  summarize(jo_ten=sum(value))|>
  ungroup()|>
  mutate(demand=rescale_custom(jo_ten))

joined <- fuzzyjoin::stringdist_join(onet_similarity, demand)|>
  rename(destination=destination.x)|>
  select(origin, destination, distance, similarity, jo_ten, demand)

write_rds(joined, here("joined.rds"))





example <- joined|>
  filter(origin=="84110: Chain saw and skidder operators")

#choose a weight on similarity
#then choose and index value
#then add isoquant to plot


weight <- .75

with_index <- example|>
  mutate(index=similarity^weight*demand^(1-weight))|>
  arrange(desc(index))|>
  mutate(rank=row_number())

tenth_index <- with_index$index[with_index$rank==10]

isoquant_data <- tibble(similarity=seq(from=exp(-1),to=1,length=1000),
                        demand=(tenth_index/(similarity^weight))^(1/(1-weight))
                        )|>
  filter(demand<=1.01 & demand>=exp(-1)-.01)

for_dt <- with_index|>
  filter(rank<11)


ggplot()+
  geom_point(data=example, mapping=aes(similarity, demand, fill=index), shape=21, colour="black", stroke=.5, size=2)+
  geom_line(data=isoquant_data, mapping=aes(similarity, demand))+
  scale_fill_viridis_c()+
  theme_minimal()+
  labs(x="Skills similarity",
       y="Rescaled Job Openings")






