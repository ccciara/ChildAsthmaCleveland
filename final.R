# install the packages using the install.package()
install.packages("tidyverse")
install.packages("units")
install.packages("dplyr")

# Load the packages with library()
library("tidyverse")
library("units")
library("sf")
library("tmap")
library("dplyr")
library("sp")
library("spdep")
library("raster")
library("stringr")

setwd("/Users/ciaraclarke/Desktop/GEOG0114/finalproject")

#import data
census_tracts <- st_read("tracts.shp")
tracts_geometry <- st_read("tracts_geometry.shp")
census_tracts2010 <- st_read("GISPORTAL_GISOWNER01_OHCENSUSTRACTS10Polygon.shp")
roads <- st_read("Functional_Class.shp")
factories <- st_read("General_Manufacturing_Facilities.shp")
tri <- read.csv("toxic release inventory.csv")
racial_data <- read.csv("race_census_2020.csv")
obesity_data <- read.csv("obesity_data.csv")
smoking_data <- read.csv("smoking_data.csv")
asthma_data <- read.csv("adult asthma rate.csv")
income_data <- read.csv("income.csv")




#shapefiles

#CENSUS TRACTS 2020________________________________________________________
census_tracts_raw <- st_read("tracts.shp")
census_tracts <- st_transform(census_tracts_raw, crs = 4326)
columns_to_keep <- c("TRACT_FIPS", "geometry")
#edit FIPS to include only census tract number
census_tracts <- dplyr::select(census_tracts, one_of(columns_to_keep))
colnames(census_tracts)[colnames(census_tracts) == "TRACT_FIPS"] <- "Census Tract"

tm_shape(census_tracts) + tm_polygons() +
  tm_shape(roads_prj) + tm_lines(col = "red")

#CENSUS TRACTS 2010
census_tracts2010_raw <- st_read("GISPORTAL_GISOWNER01_OHCENSUSTRACTS10Polygon.shp")
census_tracts2010 <- st_transform(census_tracts2010_raw, crs = 4326)
#filter out non-cuyahoga data
census_tracts2010 <- census_tracts2010 %>%
  mutate_at(vars("countyfp10"), ~as.numeric(gsub("[^0-9.]", "", .)))
census_tracts2010 <- census_tracts2010 %>%
  filter(countyfp10 == 35)
census_tracts2010 <- subset(census_tracts2010, tractce10 != 990000)

#get rid of extra columns
colnames(census_tracts2010)[colnames(census_tracts2010) == "tractce10"] <- "Census Tract"
columns_to_keep <- c("Census Tract", "geometry")
census_tracts2010 <- dplyr::select(census_tracts2010, one_of(columns_to_keep))

#merging the two census shape files to account for different tracts records
tracts2010 <- st_drop_geometry(census_tracts2010)
tracts2020 <- st_drop_geometry(census_tracts)
alltracts <- left_join(tracts2010, tracts2020, by = "Census Tract")

#extract the tracts that differ
ct_a <- tracts2010$"Census Tract"
ct_b <- tracts2020$"Census Tract"

#in 2020 but not 2010
only2020 <- setdiff(ct_b, ct_a)

#the extra 2010 values are in alltracts, still need to add extra 2020 values
#create dataframe of the extra 2020 tracts
rows2020 <- data.frame(ID = seq_along(only2020), Values = unlist(only2020))
colnames(rows2020)[colnames(rows2020) == "Values"] <- "Census Tract"
columns_to_keep <- c("Census Tract")
rows2020 <- dplyr::select(rows2020, one_of(columns_to_keep))
#add list to alltracts
alltracts <- rbind(alltracts, rows2020)

#add geometry back in
alltracts1 <- left_join(alltracts, census_tracts, by = "Census Tract") %>%
  left_join(., census_tracts2010, by = "Census Tract")

#combine geometry columns to fill in the 2010 gaps
tracts_geometry <- alltracts1 %>%
  mutate(geometry = ifelse(st_is_empty(geometry.y),
                           st_as_text(geometry.x), st_as_text(geometry.y)))
#get rid of separate geoms
tracts_geometry <- tracts_geometry %>%
  dplyr::select(-c(geometry.x, geometry.y))

#transform to geom type
tracts_geometry <- tracts_geometry %>%
  st_as_sf(wkt = "geometry")

#add crs, make valid
espg_code <- 4326
tracts_geometry <- tracts_geometry %>%
  st_set_crs(espg_code)
tracts_geometry <- st_make_valid(tracts_geometry)

#add area
tracts_geometry$Area <- st_area(tracts_geometry)

#create file
st_write(tracts_geometry, "/Users/ciaraclarke/Desktop/GEOG0114/finalproject/tracts_geometry.shp", 
         driver = "ESRI Shapefile", append = FALSE)

tm_shape(tracts_geometry) + tm_polygons()

#RESIDENTIAL ZONES_____________________________________________________
residential_zones_raw <- st_read("Cuyahoga_Land_Use_2014.shp")
rz_prj <- st_transform(residential_zones_raw, crs = 4326)
rz_prj <- rz_prj[rz_prj$USE_GROUP == "RESIDENTIAL", ]
rz_prj <- st_make_valid(rz_prj)

#remake area column, drop extra columns
rz_prj$rz_area <- st_area(rz_prj)
columns_to_keep <- c("OBJECTID", "USE_CLASS_", "rz_area", "geometry")
rz_prj <- rz_prj %>%
  dplyr::select(columns_to_keep)

st_write(rz_prj, "/Users/ciaraclarke/Desktop/GEOG0114/finalproject/res_zones.shp", 
         driver = "ESRI Shapefile", append = FALSE)


tm_shape(tracts_geometry) + tm_polygons() +
  tm_shape(rz_prj) + tm_polygons(col = "blue", border.alpha = 0) +
  tm_shape(cuyahoga_roads) + tm_lines(col = "red", lwd = "LANES") + 
  tm_shape(factories_prj) + tm_dots(col = "green")

tracts_geometry <- st_read("tracts_geometry.shp")

test_combined1 <- st_join(tracts_geometry, rz_prj)

#ROADS____________________________________________________________________
roads <- st_read("Functional_Class.shp")
roads_prj <- st_transform(roads, crs = 4326)
roads_prj <- roads_prj[roads_prj$MILES >= 0.2, ]
columns_to_keep <- c("ROUTE_NBR", "LANES", "geometry")
roads_prj <- dplyr::select(roads_prj, one_of(columns_to_keep))
roads_prj <- roads_prj[roads_prj$LANES >= 4, ]

#clip to cuyahoga county
cuyahoga_outline <- tracts_geometry %>%
  summarise(area = sum(Area))
tm_shape(cuyahoga_outline) + tm_polygons()

cuyahoga_roads <- roads_prj[cuyahoga_outline,] %>% st_intersection(cuyahoga_outline)

#plot by lane width
tm_shape(tracts_geometry) + tm_polygons() +
  tm_shape(cuyahoga_roads) + tm_lines(col = "red", lwd = "LANES")

#make 300 m and 150 m buffer around roads and calculate how much of the
#tract is within each buffer____________________

#create 150 m buffer zone, merge, make valid
road_buffer150 <- st_buffer(cuyahoga_roads, dist = 150)
road_buffer150 <- road_buffer150 %>%
  summarise(area = sum(st_area(road_buffer150)))
road_buffer150 <- st_make_valid(road_buffer150)

#plot 150 m buffer zone
tm_shape(tracts_geometry) + tm_polygons() +
  tm_shape(road_buffer150) + tm_polygons(col = "red", border.alpha = 0)
'''
#create 300m buffer zone. merge, make valid
road_buffer300 <- st_buffer(cuyahoga_roads, dist = 300)
road_buffer300 <- road_buffer300 %>%
  summarise(area = sum(st_area(road_buffer300)))
road_buffer300 <- st_make_valid(road_buffer300)

#plot both buffer zones
tm_shape(tracts_geometry) + tm_polygons() +
  tm_shape(road_buffer300) + tm_polygons(col = "blue", border.alpha = 0) +
  tm_shape(road_buffer150) + tm_polygons(col = "red", border.alpha = 0)

#remove overlap between 150m and 300m buffer zones, prioritize 150m
road_buffer300 <- st_difference(road_buffer300, road_buffer150)

#check that worked
tm_shape(tracts_geometry) + tm_polygons() +
  tm_shape(road_buffer300) + tm_polygons(col = "blue", border.alpha = 0)
'''
res_zones <- st_read("res_zones.shp")
#merge 150 with rez zones to find overlaps
road_buffer150_rz <- st_intersection(res_zones, road_buffer150)
road_buffer150_rz <- st_make_valid(road_buffer150_rz)

tm_shape(tracts_geometry) + tm_polygons() +
  tm_shape(road_buffer150_rz) + tm_polygons(col = "blue", border.alpha = 0)

#calculate area of overlap
road_buffer150_rz$area_overlap <- st_area(road_buffer150_rz)

#merge with census tracts
road_buffer150_rzct <- st_join(tracts_geometry, road_buffer150_rz)

#make all columns numeric
road_buffer150_rzct <- road_buffer150_rzct %>%
  mutate_at(vars("rz_area", "area_overlap", "CnssTrc"), ~as.numeric(gsub("[^0-9.]", "", .)))


#combine within census tracts
road_buffer150_rzct <- road_buffer150_rzct %>%
  group_by(CnssTrc) %>%
  summarise(
    total_rz_area = sum(rz_area), 
    total_overlap_area = sum(area_overlap)
  )

#find percentage overlap of buffer and res zones within each tract
road150_percentage_sp <- road_buffer150_rzct %>%
  mutate(percentage_overlap = ifelse(total_rz_area != 0, total_overlap_area / total_rz_area, 0))
  
road150plot <- tm_shape(road150_percentage_sp) +
  tm_fill("percentage_overlap", palette = "Greens") +
  tm_shape(cuyahoga_roads) + tm_lines(col = "red", lwd = "LANES")

st_write(road150_percentage_sp, "/Users/ciaraclarke/Desktop/GEOG0114/finalproject/road150m.shp", 
         driver = "ESRI Shapefile", append = FALSE)

#same for 300____________
#merge 300 with rez zones to find overlaps
road_buffer300_rz <- st_intersection(rz_prj, road_buffer150)
road_buffer300_rz <- st_make_valid(road_buffer300_rz)

tm_shape(tracts_geometry) + tm_polygons() +
  tm_shape(road_buffer300_rz) + tm_polygons(col = "blue", border.alpha = 0)

#calculate area of overlap
road_buffer300_rz$area_overlap <- st_area(road_buffer300_rz)

#merge with census tracts
road_buffer300_rzct <- st_join(tracts_geometry, road_buffer300_rz)

#make all columns numeric
road_buffer300_rzct <- road_buffer300_rzct %>%
  mutate_at(vars("rz_area", "area_overlap", "CnssTrc"), ~as.numeric(gsub("[^0-9.]", "", .)))


#combine within census tracts
road_buffer300_rzct <- road_buffer300_rzct %>%
  group_by(CnssTrc) %>%
  summarise(
    total_rz_area = sum(rz_area), 
    total_overlap_area = sum(area_overlap)
  )

#find percentage overlap of buffer and res zones within each tract
road300_percentage_sp <- road_buffer300_rzct %>%
  mutate(percentage_overlap = ifelse(total_rz_area != 0, total_overlap_area / total_rz_area, 0))

road300plot <- tm_shape(road300_percentage_sp) +
  tm_fill("percentage_overlap", palette = "Greens") +
  tm_shape(cuyahoga_roads) + tm_lines(col = "red", lwd = "LANES")

tmap_arrange(road150plot, road300plot, nrow = 1)
#FACTORIES_______________________________________________________________
res_zones <- st_read("res_zones.shp")
tracts_geometry <- st_read("tracts_geometry.shp")
factories <- st_read("General_Manufacturing_Facilities.shp")
factories_prj <- st_transform(factories, crs = 4326)

tm_shape(tracts_geometry) + tm_polygons() + 
  tm_shape(factories_prj) + tm_dots(col = "green")

print(colnames(factories_prj))

#extract relevant info
columns_to_keep <- c("NAICSDESCR", "geometry")
factories_prj <- dplyr::select(factories_prj, one_of(columns_to_keep))

#1KM BUFFER____________________________________
#create buffer zone, merge, make valid
factories_buffer1km <- st_buffer(factories_prj, dist = 1000)
factories_buffer1km <- factories_buffer1km %>%
  summarise(area = sum(st_area(factories_buffer1km)))
factories_buffer1km <- st_make_valid(factories_buffer1km)

tm_shape(tracts_geometry) + tm_polygons() + 
  tm_shape(factories_buffer) + tm_polygons(col = "green", border.alpha = 0)

#merge buffer with rez zones to find overlaps
factories_buffer1km_rz <- st_intersection(res_zones, factories_buffer1km)
factories_buffer1km_rz <- st_make_valid(factories_buffer1km_rz)

#calculate area of overlap
factories_buffer1km_rz$area_overlap <- st_area(factories_buffer1km_rz)

#merge with census tracts
factories_buffer1km_rzct <- st_join(tracts_geometry, factories_buffer1km_rz)

#make all columns numeric
factories_buffer_rzct <- factories_buffer_rzct %>%
  mutate_at(vars("rz_area", "area_overlap", "CnssTrc"), ~as.numeric(gsub("[^0-9.]", "", .)))

#combine within census tracts
factories_buffer_rzct <- factories_buffer_rzct %>%
  group_by(CnssTrc) %>%
  summarise(
    total_rz_area = sum(rz_area), 
    total_overlap_area = sum(area_overlap)
  )

#find percentage overlap of buffer and res zones within each tract
factories1km_percentage_sp <- factories_buffer_rzct %>%
  mutate(percentage_overlap = ifelse(total_rz_area != 0, total_overlap_area / total_rz_area, 0))

#0.5KM BUFFER____________________________
#create buffer zone
factories_buffer0.5 <- st_buffer(factories_prj, dist = 500)
factories_buffer0.5 <- factories_buffer0.5 %>%
  summarise(area = sum(st_area(factories_buffer0.5)))
factories_buffer0.5 <- st_make_valid(factories_buffer0.5)

tm_shape(tracts_geometry) + tm_polygons() + 
  tm_shape(factories_buffer0.5) + tm_polygons(col = "green", border.alpha = 0)

#merge buffer with rez zones to find overlaps
factories_buffer0.5_rz <- st_intersection(res_zones, factories_buffer0.5)
factories_buffer0.5_rz <- st_make_valid(factories_buffer0.5_rz)

#calculate area of overlap
factories_buffer0.5_rz$area_overlap <- st_area(factories_buffer0.5_rz)

#merge with census tracts
factories_buffer0.5_rzct <- st_join(tracts_geometry, factories_buffer0.5_rz)

#make all columns numeric
factories_buffer0.5_rzct <- factories_buffer0.5_rzct %>%
  mutate_at(vars("rz_area", "area_overlap", "CnssTrc"), ~as.numeric(gsub("[^0-9.]", "", .)))

#combine within census tracts
factories_buffer0.5_rzct <- factories_buffer0.5_rzct %>%
  group_by(CnssTrc) %>%
  summarise(
    total_rz_area = sum(rz_area), 
    total_overlap_area = sum(area_overlap)
  )

#find percentage overlap of buffer and res zones within each tract
factories0.5km_percentage_sp <- factories_buffer0.5_rzct %>%
  mutate(percentage_overlap = ifelse(total_rz_area != 0, total_overlap_area / total_rz_area, 0))

#download
st_write(factories0.5km_percentage_sp, "/Users/ciaraclarke/Desktop/GEOG0114/finalproject/factories0.5km.shp", 
         driver = "ESRI Shapefile", append = FALSE)


#TOXIC RELEASE INVENTORY________________________________________________
tri <- read.csv("toxic release inventory.csv")

tm_shape(tri) + tm_polygons(col = "green") +
  tm_shape(tracts_geometry) + tm_polygons()

#extract chemical id from chemical name column
tri$CASStandard <- str_extract(tri$Chemical, " [^ ]*$")
tri$CASStandard <- gsub("[()]", "", tri$CASStandard)
tri$CASStandard <- trimws(tri$CASStandard)

#import toxicity weight chart, keep inhalation toxicity weight
toxweights <- read.csv("chemical_data_rsei_v2311.csv")
columns_to_keep <- c("CASStandard", "ITW")
toxweights <- dplyr::select(toxweights, one_of(columns_to_keep))

#add toxicity weight to tri
tri_w <- merge(tri, toxweights, by = "CASStandard")

#remove entries without air releases
tri_w <- tri_w[tri_w$"Air.Releases..lb." >= 0.001, ]

#transform to shapefile
tri_sf <- st_as_sf(tri_w, coords = c("Longitude", "Latitude"), crs = 4326)
#get rid of extra columns
columns_to_keep <- c("TRI.Facility.Name", "Chemical", 
                     "Air.Releases..lb.", "ITW", "geometry") 
tri_sf <- dplyr::select(tri_sf, one_of(columns_to_keep))
#change air releases to numeric to use while mapping
tri_sf <- tri_sf %>%
  mutate_at(vars("Air.Releases..lb."), ~as.numeric(gsub("[^0-9.]", "", .)))

#create toxicity x amount release
tri_sf$tox_hazard <- tri_sf$"Air.Releases..lb." * tri_sf$ITW


tm_shape(tracts_geometry) + tm_polygons() + 
  tm_shape(tri_sf) + tm_dots(col = "blue", size = "tox_hazard") + 
  tm_scale_bar(position = c("right", "bottom"))

#create buffer zone (1km? 2km?)
tri_buffer1km <- st_buffer(tri_sf, dist = 1000)
tm_shape(tracts_geometry) + tm_polygons() +
  tm_shape(tri_buffer) + tm_polygons(col = "blue")

#merge buffer with rez zones to find overlaps
tri_buffer1km_rz <- st_intersection(rz_prj, tri_buffer1km)
tri_buffer1km_rz <- st_make_valid(tri_buffer1km_rz)

tm_shape(tracts_geometry) + tm_polygons() +
  tm_shape(tri_buffer1km_rz) + tm_polygons(col = "blue", border.alpha = 0)

#calculate area of overlap
tri_buffer1km_rz$area_overlap <- st_area(tri_buffer1km_rz)

#find weighted contribution of each rz/tox area
#hazard area * hazard level
tri_buffer1km_rz$weighted_contribution <- tri_buffer1km_rz$area_overlap * tri_buffer1km_rz$tox_hazard

trial_trigroup <- st_drop_geometry(tri_buffer1km_rz)

#group by zone and calculate sum of weighted contributions and sum of toxic area
trial_trigroup <- trial_trigroup %>%
  group_by(OBJECTID) %>%
  summarize(
    total_weighted_contribution = sum(weighted_contribution),
    rz_area = first(rz_area)
  )

#calculate weighted average
#(toxic area * hazard level) / total area
trial_trigroup$avg_tox_score <- trial_trigroup$total_weighted_contribution / trial_trigroup$rz_area

#add geoms back in, give non-toxic neighborhoods a score of 0
rz_hazards_sp <- left_join(rz_prj, trial_trigroup, by = "OBJECTID")

#merge with census tracts
rz_hazards_sp <- st_join(tracts_geometry, rz_hazards_sp)

#make all columns numeric
rz_hazards_sp <- rz_hazards_sp %>%
  mutate_at(vars("CnssTrc", "avg_tox_score"), ~as.numeric(gsub("[^0-9.]", "", .)))

#replace NAs with 0
rz_hazards_sp <- rz_hazards_sp %>%
  mutate(avg_tox_score = ifelse(is.na(avg_tox_score), 0, avg_tox_score))
rz_hazards_sp <- rz_hazards_sp %>%
  mutate(total_weighted_contribution = ifelse(is.na(total_weighted_contribution), 0, total_weighted_contribution))

#get rid of extra area column
rz_hazards_sp <- rz_hazards_sp %>% dplyr::select(-"rz_area.y")


#combine within census tracts
weighted_avg_hazard <- rz_hazards_sp %>%
  group_by(CnssTrc) %>%
  summarize(
    weighted_ct_tscore = sum(total_weighted_contribution * avg_tox_score),
    total_residential_zone_area = sum(rz_area.x)
  ) %>%
  mutate(
    weighted_avg_hazard_score = weighted_ct_tscore / total_residential_zone_area
  ) %>%
  dplyr::select(CnssTrc, weighted_avg_hazard_score, geometry)

#standardize
weighted_avg_hazard_sd <- weighted_avg_hazard

weighted_avg_hazard_sd$weighted_avg_hazard_score <- weighted_avg_hazard_sd$weighted_avg_hazard_score / 5.137826e+19

numeric_values <- as.numeric(weighted_avg_hazard_sd$weighted_avg_hazard_score)

weighted_avg_hazard_sd$weighted_avg_hazard_score <- st_drop_units(weighted_avg_hazard_sd$weighted_avg_hazard_score)

filtered_data <- weighted_avg_hazard_sd[numeric_values > 0, ]

filtered_data <- weighted_avg_hazard_sd %>%
  filter(weighted_avg_hazard_score > 0)

plottri <- tm_shape(filtered_data) + tm_fill("weighted_avg_hazard_score", style = "quantile",
                                                      breaks = 20, palette = "Greens") +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

plottri

hist(weighted_avg_hazard$weighted_avg_hazard_score, breaks = 2)

#UGHHH WHATEVER FORGET TOXICITY WEIGHTS I GUESS JUST DO DISTANCE :(

#clean CSVs____________________________________________________

#ASTHMA____________________________________________________________________
#check their collection method - is indicator count value the number of pos?
#check against tract pop
asthma_data <- read.csv("adult asthma rate.csv")

#remove 2021 and 2022 data from asthma data
values_to_delete <- c("2014", "2015", "2016", "2017", "2018",
                      "2019", "2019", "2021", "2022")
column_to_check <- "Period.of.Measure"
asthma_data <- asthma_data[!asthma_data[, column_to_check] 
                           %in% values_to_delete, ]
colnames(asthma_data)[colnames(asthma_data) == "Indicator.Rate.Value"] <- "Adult Asthma Rate"

#remove extra columns from asthma data, change column names
columns_to_keep <- c("Location", "Adult Asthma Rate")
asthma <- dplyr::select(asthma_data, one_of(columns_to_keep))

#standardize
asthma$"Adult Asthma Rate" <- asthma$"Adult Asthma Rate" / 100

#create merge column
asthma <- asthma %>%
  mutate("Census Tract" = substr(Location, 6, nchar(Location)))

#fix tract numbers
asthma <- asthma %>%
  mutate_at(vars("Census Tract"), ~as.numeric(gsub("[^0-9.]", "", .)))
columns_to_keep <- c("Census Tract", "Adult Asthma Rate")
asthma <- dplyr::select(asthma, one_of(columns_to_keep))

#merge
asthma_sp <- merge(tracts_geometry, asthma, by.x = "CnssTrc",
                   by.y = "Census Tract")

#download
asthma_csv <- st_drop_geometry(asthma_sp)
write.csv(asthma_csv, "standardized_asthma_sp.csv", row.names = FALSE)


#mapping it
plot1 <- tm_shape(asthma_sp) + tm_fill("Adult Asthma Rate", palette = "Greens") +
  tm_shape(tracts_geometry) + tm_polygons(alpha = 0, border.alpha = 1,
                                          border.col = "black") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

plot1

#OBESITY___________________________________________________________________
obesity_data <- read.csv("obesity_data.csv")
#remove extra years from data
wrong_years <- c("2014", "2015", "2016", "2017", "2018", "2019", "2021")
column_to_check <- "Period.of.Measure"
obesity_data <- obesity_data[!obesity_data[, column_to_check] %in% wrong_years, ]

#remove extra columns from obesity data, change column names
columns_to_keep <- c("Location", "Indicator.Rate.Value")
obesity_data <- dplyr::select(obesity_data, one_of(columns_to_keep))
colnames(obesity_data)[colnames(obesity_data) == "Indicator.Rate.Value"] <- "Adult Obesity Rate"

#create census tract column
obesity_data <- obesity_data %>%
  mutate("Census Tract" = substr(Location, 6, nchar(Location)))
obesity_data <- obesity_data %>%
  mutate_at(vars("Census Tract"), ~as.numeric(gsub("[^0-9.]", "", .)))

#standardize
obesity_data$"Adult Obesity Rate" <- obesity_data$"Adult Obesity Rate" / 100

#merge
columns_to_keep <- c("Census Tract", "Adult Obesity Rate")
obesity_data <- dplyr::select(obesity_data, one_of(columns_to_keep))
obesity_sp <- merge(tracts_geometry, obesity_data, by.x = "CnssTrc",
                    by.y = "Census Tract")
#download
obesity_csv <- st_drop_geometry(obesity_sp)
write.csv(obesity_csv, "standardized_obesity_sp.csv", row.names = FALSE)


#plot
plot2 <- tm_shape(obesity_sp) + tm_fill("Adult Obesity Rate", palette = "Greens") +
  tm_shape(tracts_geometry) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

plot2



#SMOKING_____________________________________________________________________
smoking_data <- read.csv("smoking_data.csv")
#remove extra years from data
values_to_delete <- c("2014", "2015", "2016", "2017", "2018", "2019", "2021")
column_to_check <- "Period.of.Measure"
smoking_data <- smoking_data[!smoking_data[, column_to_check] %in% values_to_delete, ]

#remove extra columns from smoking data, change column names
columns_to_keep <- c("Location", "Indicator.Rate.Value")
smoking_data <- dplyr::select(smoking_data, one_of(columns_to_keep))
colnames(smoking_data)[colnames(smoking_data) == "Indicator.Rate.Value"] <- "Smoking Rate"

#create census tract column
smoking_data <- smoking_data %>%
  mutate("Census Tract" = substr(Location, 6, nchar(Location)))
smoking_data <- smoking_data %>%
  mutate_at(vars("Census Tract"), ~as.numeric(gsub("[^0-9.]", "", .)))

#standardize
smoking_data$"Smoking Rate" <- smoking_data$"Smoking Rate" / 100

#merge
columns_to_keep <- c("Census Tract", "Smoking Rate")
smoking_data <- dplyr::select(smoking_data, one_of(columns_to_keep))
smoking_sp <- merge(tracts_geometry, smoking_data, by.x = "CnssTrc",
                    by.y = "Census Tract")

#download
smoking_csv <- st_drop_geometry(smoking_sp)
write.csv(smoking_csv, "standardized_smoking_sp.csv", row.names = FALSE)

#plot
plot3 <- tm_shape(smoking_sp) + tm_fill("Smoking Rate", palette = "Greens") +
  tm_shape(tracts_geometry) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

plot3


#RACE_______________________________________________________________
#for racial data - split into percentages? each tract has percent white, 
#percent black, percent latino, percent asian, percent other
#
racial_data <- read.csv("race_census_2020.csv")

#remove leading spaces from data
remove_leading_spaces <- function(x) sub("^\\s+", "", x)
racial_data[] <- lapply(racial_data, remove_leading_spaces)
racial_data$"Label..Grouping." <- str_trim(racial_data$"Label..Grouping.")

#switch row names to be racial groups
colnames(racial_data)[colnames(racial_data) == "Label..Grouping."] <- "Race Group"
rownames(racial_data) <- racial_data$"Race Group"
racial_data$"Race Group" <- NULL

#rename columns to be census tracts
racial_data <- racial_data %>%
  rename_with(~gsub("\\D", "", .), everything())
racial_data <- racial_data %>%
  rename_with(~ ifelse(nchar(.) < 6, paste0(., "00"), as.character(.)), everything())

#transpose to match other dataframes
t_racial_data <- as.data.frame(t(as.matrix(racial_data)))

#change to numeric
t_numeric <- t_racial_data %>%
  mutate_all(~as.numeric(gsub("[^0-9.]", "", .)))

#drop unnecessary columns
columns_to_keep <- c("Total:", "White alone", "Black or African American alone", 
                     "Hispanic or Latino", "Asian alone",
                     "Population of two or more races:",
                     "Some Other Race alone",
                     "Native Hawaiian and Other Pacific Islander alone",
                     "American Indian and Alaska Native alone")
t_numeric <- dplyr::select(t_numeric, one_of(columns_to_keep))


#simplify minorities into "other"
tn_simplified <- t_numeric %>%
  mutate("Percent Other" = rowSums(dplyr::select(., starts_with("Population of two or more"), 
                                starts_with("Some Other Race"),
                                starts_with("Native Hawaiian"),
                                starts_with("American Indian")),
                         na.rm = TRUE)) %>%
  dplyr::select("Total:", "White alone", "Black or African American alone", 
                "Hispanic or Latino", "Asian alone", "Percent Other")

#extract the total population column for later
tn_pop <- tn_simplified
tn_pop$"Census Tract" <- rownames(tn_pop)
tn_pop$"Population" <- tn_simplified[, 1]
tract_population <- tn_pop %>%
  dplyr::select("Population", "Census Tract")

#exclude total population
racial_data_without_total <- tn_simplified[, -1]

# Calculate the percentage for each row
race_percent <- racial_data_without_total
col_names <- colnames(race_percent)
race_percent[, col_names] <- race_percent[, col_names] / 
  tract_population$"Population"

#rename rows
colnames(race_percent)[colnames(race_percent) == "White alone"] <- "Percent White"
colnames(race_percent)[colnames(race_percent) == "Black or African American alone"] <- "Percent Black"
colnames(race_percent)[colnames(race_percent) == "Hispanic or Latino"] <- "Percent Latino"
colnames(race_percent)[colnames(race_percent) == "Asian alone"] <- "Percent Asian"

#add census tract back in
race_percent$"Census Tract" <- rownames(race_percent)

#download
write.csv(race_percent, "standardized_race_sp.csv", row.names = FALSE)

#join geoms
race_sp <- merge(tracts_geometry, race_percent, by.x = "CnssTrc",
                   by.y = "Census Tract")

#plot race (one at a time)
plot4 <- tm_shape(race_sp) + tm_fill("Black", palette = "Greens") +
  tm_shape(tracts_geometry) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)


#population density plot
pop_sp <- merge(tracts_geometry, tract_population, by.x = "Census Tract",
                           by.y = "Census Tract")
pop_sp$pop_density <- pop_sp$Population / pop_sp$Area


plot5 <- tm_shape(pop_sp) + tm_fill("pop_density", palette = "Greens") +
  tm_shape(tracts_geometry) + tm_polygons(alpha = 0, border.alpha = 1,
                                          border.col = "black") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

plot1


#POVERTY__________________________________________________________
poverty_data <- read.csv("poverty.csv")

#remove leading spaces from data
remove_leading_spaces <- function(x) sub("^\\s+", "", x)
poverty_data[] <- lapply(poverty_data, remove_leading_spaces)
poverty_data$"Label..Grouping." <- str_trim(poverty_data$"Label..Grouping.")

#get rid of extra columns
poverty_data <- poverty_data %>%
  dplyr::select(1, ends_with("Percent.below.poverty.level..Estimate"))

#get rid of extra rows
poverty_data <- poverty_data[c(1), ]

#make row names the categories
rownames(poverty_data) <- poverty_data$"Label..Grouping."
poverty_data$"Label..Grouping." <- NULL

#rename columns to be census tracts
poverty_data <- poverty_data %>%
  rename_with(~gsub("\\D", "", .), everything())
poverty_data <- poverty_data %>%
  rename_with(~ ifelse(nchar(.) == 4, paste0(., "00"), as.character(.)), everything())

#transpose to match other data frames
t_poverty_data <- as.data.frame(t(as.matrix(poverty_data)))

#change to numeric, standardize
poverty_numeric <- t_poverty_data %>%
  mutate_all(~as.numeric(gsub("[%]", "", .)))
poverty_numeric$"Poverty Rate" <- poverty_numeric$"Poverty Rate" / 100

#add census tract column
poverty_numeric$"Census Tract" <- rownames(poverty_numeric)
colnames(poverty_numeric)[colnames(poverty_numeric) ==
                            "Population for whom poverty status is determined"] <- "Poverty Rate"

#merge with map
poverty_sp <- merge(tracts_geometry, poverty_numeric, by.x = "CnssTrc",
                 by.y = "Census Tract")

#download
poverty_csv <- st_drop_geometry(poverty_sp)
write.csv(poverty_csv, "standardized_poverty_sp.csv", row.names = FALSE)

plot6 <- tm_shape(poverty_sp) + tm_fill("Poverty Rate", palette = "Greens") +
  tm_shape(tracts_geometry) + tm_polygons(alpha = 0, border.alpha = 1,
                                          border.col = "black") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

plot1


#plot all
tmap_arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 2)

#INCOME__________________________________________________________________
income_raw = read.csv("income.csv")

#remove extra column data
names(income_raw)[names(income_raw) == "Label..Grouping."] <- "Label..Grouping..Households..Estimate"
income_raw <- income_raw %>%
  dplyr::select(ends_with("..Households..Estimate"))

#remove extra row data
income_data <- income_raw[12:13, ]

#make row names the categories
rownames(income_data) <- income_data$"Label..Grouping..Households..Estimate"
income_data$"Label..Grouping..Households..Estimate" <- NULL

#rename columns to be census tracts
income_data <- income_data %>%
  rename_with(~gsub("\\D", "", .), everything())
income_data <- income_data %>%
  rename_with(~ ifelse(nchar(.) == 4, paste0(., "00"), as.character(.)), everything())

#transpose to match other data
income_data <- as.data.frame(t(as.matrix(income_data)))

#change to numeric
income_numeric <- income_data %>%
  mutate_all(~as.numeric(gsub(",", "", .)))

#add census tract column
income_numeric$"Census Tract" <- rownames(income_numeric)

#merge with map
income_sp <- merge(tracts_geometry, income_numeric, by.x = "CnssTrc", by.y = "Census Tract")

#download
income_csv <- st_drop_geometry(income_sp)
write.csv(income_csv, "standardized_income_sp.csv", row.names = FALSE)

tm_shape(income_sp) + tm_fill("Mean income (dollars)", palette = "Greens") +
  tm_shape(tracts_geometry) + tm_polygons(alpha = 0, border.alpha = 1,
                                          border.col = "black") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

#INDUSTRY_____________________________________________________________
industry_raw <- read.csv("industry.csv")

#remove extra column data
names(industry_raw)[names(industry_raw) == "Label..Grouping."] <- "Label..Grouping..Total..Estimate"
industry_raw <- industry_raw %>%
  dplyr::select(ends_with("..Total..Estimate"))

#remove extra row data
industry_data <- industry_raw[c(1,4), ]

#make row names the categories
rownames(industry_data) <- industry_data$"Label..Grouping..Total..Estimate"
industry_data$"Label..Grouping..Total..Estimate" <- NULL

#rename columns to be census tracts
industry_data <- industry_data %>%
  rename_with(~gsub("\\D", "", .), everything())
industry_data <- industry_data %>%
  rename_with(~ ifelse(nchar(.) == 4, paste0(., "00"), as.character(.)), everything())

#transpose to match other data
industry_data <- as.data.frame(t(as.matrix(industry_data)))

#change to numeric
industry_numeric <- industry_data %>%
  mutate_all(~as.numeric(gsub(",", "", .)))

#fix names
print(colnames(industry_numeric))
colnames(industry_numeric)[colnames(industry_numeric) == "Civilian employed population 16 years and over"] <- "Working Population"
print(colnames(industry_numeric))
industry_numeric$`Manufacturing` <- industry_numeric$`    Manufacturing`
columns_to_keep <- c("Working Population", "Manufacturing")
industry_numeric <- dplyr::select(industry_numeric, one_of(columns_to_keep))

#find proportion of manufacturing workers
industry_numeric$"Percent Manufacturing" <- 
  ifelse(industry_numeric$"Working Population" != 0,
         industry_numeric$"Manufacturing" / industry_numeric$"Working Population",
         NA)

#add census tract column
industry_numeric$"Census Tract" <- rownames(industry_numeric)

#merge with map
industry_sp <- merge(tracts_geometry, industry_numeric, by.x = "CnssTrc", by.y = "Census Tract")

#download
industry_csv <- st_drop_geometry(industry_sp)
write.csv(industry_csv, "standardized_industry_sp.csv", row.names = FALSE)

tm_shape(industry_sp) + tm_fill("Percent Manufacturing", palette = "Greens") +
  tm_shape(tracts_geometry) + tm_polygons(alpha = 0, border.alpha = 1,
                                          border.col = "black") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(frame = FALSE, legend.title.size = 0.5, legend.text.size = 0.5)

#________________________________________________________________________
#creating correlation table
#asthma_sp, poverty_sp, race_sp, smoking_sp, obesity_sp

#todo
#create table showing % of census tract housing within 150m / 300m of major road
#create table showing % of census tract housing within 1km of tri site

#fix census tracts to be characters in asthma, smoking, obesity

factories_sp <- st_read("factories0.5km.shp")
roads_sp <- st_read("road150m.shp")

factories_matrix <- st_drop_geometry(factories_sp)
roads_matrix <- st_drop_geometry(roads_sp)

colnames(factories_matrix)[colnames(factories_matrix) == "CnssTrc"] <- "Census Tract"
colnames(factories_matrix)[colnames(factories_matrix) == "prcntg_"] <- "Percentage within 1km of factory"

colnames(roads_matrix)[colnames(roads_matrix) == "CnssTrc"] <- "Census Tract"
colnames(roads_matrix)[colnames(roads_matrix) == "prcntg_"] <- "Percentage within 150m of road"

columns_to_keep <- c("Census Tract", "Percentage within 1km of factory")
factories_matrix <- dplyr::select(factories_matrix, one_of(columns_to_keep))

columns_to_keep <- c("Census Tract", "Percentage within 150m of road")
roads_matrix <- dplyr::select(roads_matrix, one_of(columns_to_keep))


asthma$`Census Tract` <- as.character(asthma$`Census Tract`)
smoking_data$`Census Tract` <- as.character(smoking_data$`Census Tract`)
obesity_data$`Census Tract` <- as.character(obesity_data$`Census Tract`)

asthma <- read.csv("standardized_asthma_sp.csv")
smoking_data <- read.csv("standardized_smoking_sp.csv")
poverty_numeric <- read.csv("standardized_poverty_sp.csv")
obesity_data <- read.csv("standardized_obesity_sp.csv")
race_percent <- read.csv("standardized_race_sp.csv")
industry_numeric <- read.csv("standardized_industry_sp.csv")
income_numeric <- read.csv("standardized_income_sp.csv")


merged_df <- merge(asthma, smoking_data, by = "CnssTrc")
merged_df <- merge(merged_df, poverty_numeric, by = "CnssTrc")
merged_df <- merge(merged_df, obesity_data, by = "CnssTrc")
merged_df <- merge(merged_df, race_percent, by = "CnssTrc")
merged_df <- merge(merged_df, industry_numeric, by = "CnssTrc")
merged_df <- merge(merged_df, income_numeric, by = "CnssTrc")
merged_df <- merge(merged_df, factories_matrix, by = "CnssTrc")
merged_df <- merge(merged_df, roads_matrix, by = "CnssTrc")


merged_df <- merged_df %>%
  mutate_all(~as.numeric(gsub("[^0-9.]", "", .)))

cor_matrix <- cor(merged_df)
print(cor_matrix)

install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "color")

merged_sp <- left_join(tracts_geometry, merged_df, by = "Census Tract")


#_______________________________________________________
#haha idk what i'm doing here
modelMLR <- lm(log10("Adult Asthma Rate") ~ log10("Smoking Rate") ~ 
                 log10("Poverty Rate") ~ log10("Adult Obesity Rate") ~
                 log10("Percent White") ~ log10("Percent Black") ~
                 log10("Percent Latino") ~ log10("Other"))






