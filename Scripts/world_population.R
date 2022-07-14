options(scipen=999)

# Library
library(tidyverse)
library(voronoiTreemap)
library(raster)

# clear global environment
rm(list=ls(all=TRUE))


# Population data is from the World Population Projections 2022
pop <- read.csv("./Data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv") %>% 
  filter(Time == 2021,
         LocTypeName == "Country/Area") %>% 
  group_by(ISO3_code, Location, Time) %>% 
  summarise(PopTotal = sum(PopTotal)) #,
            #PopMale = sum(PopMale),
            #PopFemale = sum(PopFemale))

pop <- pop %>% rename(ISO3 = ISO3_code)

# Get continent
pop <- merge(pop,ccodes()[,c("ISO3","continent")],by.x=,by.y=)


# Calculate weights

pop <- pop %>% arrange(desc(PopTotal)) %>% slice(n = 1:100)

pop$world_pop <- sum(pop$PopTotal)

pop <- pop %>% mutate(Weight = PopTotal/world_pop)


# Assign color to each continent


pop <- pop %>% mutate(Color = case_when(continent == "North America" ~ "#1e6aa6",
                                        continent == "South America" ~ "#bb1e0f",
                                        continent == "Africa" ~ "#fdb00c",
                                        continent == "Oceania" ~ "#f4a261",
                                        continent  == "Europe" ~ "#f5f6e6",
                                        continent == "Asia" ~ "#e8f6f7"
                                        ),
                      Total = "Total")

pop <- pop %>% mutate(PopTotal = round(PopTotal/1000, digits = 2),
                      ISO3 = paste(ISO3, PopTotal))
# Format table to correspond to voronoitreemap package
pop <- pop %>% dplyr::select(Total, continent, Location, Color, Weight, ISO3) %>% 
  rename(h1 = Total,
         h2 = continent,
         h3 = Location,
         color = Color,
         weight = Weight,
         codes = ISO3)

# Make visualization

voronoi <- vt_export_json(vt_input_from_df(pop))
voronoi_graph <- vt_d3(voronoi, color_border = "#172a3a", size_border = "2px", 
                 legend = TRUE,
                 width = 2000,
                 height = 2000)


# Save graph

library(htmlwidgets)

saveWidget(voronoi_graph,"voronoi_graph.html",selfcontained = TRUE)

library(webshot2)

webshot2::webshot("voronoi_graph.html","voronoi_png_2.png",delay = 2,selector = "#htmlwidget_container",
                  zoom = 5, vwidth = 2000)

?webshot

