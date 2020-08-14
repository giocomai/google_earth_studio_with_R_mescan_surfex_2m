library("tidyverse")
library("sf")
# remotes::install_github("giocomai/latlon2map")
library("latlon2map")

if (fs::file_exists("difference_sf.rds")==FALSE) {
  download.file(url = "https://github.com/giocomai/mescan_surfex_2m/releases/download/v1.0/difference_sf.rds",
                destfile = "difference_sf.rds")
}


ll_set_folder(path = "~/R")

fs::dir_create(path = "kml")

kml_cache <- fs::path("kml_4k", "kml_cache")

fs::dir_create(path = kml_cache)


#### colour paletttes functions ####
# limit range to between 0 and 4

pal <- leaflet::colorNumeric(palette = 'YlOrRd',
                             domain = c(0,4),
                             reverse = FALSE)
pal_fix <- function(value) {
  purrr::map_chr(.x = value, .f = function(x) {
    if (x>4) {
      pal(4)
    } else if (x<0) {
      pal(0) 
    } else {
      pal(x) 
    }  
  })
}

##### other functions ######


get_adjusted_label_location <- function(grid, x_percent = 10,
                                        y_percent = 5) {
  purrr::map_dfr(.x = 1:nrow(grid), .f = function(x) {
    grid_sq <- grid %>% slice(x)
    
    ### this is going to mix lower and upper part of the grid cell, but no big deal in this context
    
    reference_points <- grid_sq %>% st_cast("POINT") %>%
      sf::st_coordinates() %>%
      tibble::as_tibble()
    
    y_dist <- max(reference_points$Y)-min(reference_points$Y)
    x_dist <- max(reference_points$X)-min(reference_points$X)
    grid_adjusted_center_coords <- grid_sq %>% 
      st_transform(crs = 3857) %>% 
      st_centroid() %>%
      st_transform(crs = 4326) %>% 
      st_coordinates() - (c(x_dist*(x_percent/100), y_dist*(y_percent/100)))
    
    sf::st_as_sf(grid_adjusted_center_coords %>%
                   as_tibble(),
                 coords = c("X", "Y"), crs = 4326)
  })
}



#### export to kml functions #####


#### get data ######

difference_sf <- readr::read_rds(path = fs::path("difference_sf.rds"))

####### italy plus 300 ######
if (fs::file_exists(path = fs::path(kml_cache, "italy_plus300_base.rds"))==FALSE) {
  italy_plus300_base <- ll_get_nuts_it(level = 1) %>% 
    sf::st_union() %>% 
    sf::st_transform(crs = 3857) %>% 
    sf::st_buffer(dist = units::as_units(300, "km")) %>% 
    sf::st_transform(crs = 4326) 
  saveRDS(object = italy_plus300_base, file = fs::path(kml_cache, "italy_plus300_base.rds"))
} else {
  italy_plus300_base <- readr::read_rds(fs::path(kml_cache, "italy_plus300_base.rds"))
}

italy_plus300_grid_sf_file <- fs::path(kml_cache, "italy_plus300_grid_sf.rds")

if (fs::file_exists(italy_plus300_grid_sf_file)==FALSE) {
  italy_plus300_grid_sf <- difference_sf %>%
    sf::st_transform(3857) %>% 
    sf::st_filter(italy_plus300_base %>% sf::st_transform(3857)) %>% 
    sf::st_transform(4326)
  saveRDS(object = italy_plus300_grid_sf, file = italy_plus300_grid_sf_file)
} else {
  italy_plus300_grid_sf <- readr::read_rds(path = italy_plus300_grid_sf_file)
}



if (fs::file_exists(path = fs::path(kml_cache, "italy_base.rds"))==FALSE) {
  italy_base <- ll_get_nuts_it(level = 1) %>%
    sf::st_transform(3857) %>% 
    sf::st_union() %>% 
    sf::st_transform(crs = 4326) 
  saveRDS(object = italy_base, file = fs::path(kml_cache, "italy_base.rds"))
} else {
  italy_base <- readr::read_rds(fs::path(kml_cache, "italy_base.rds"))
}

italy_grid_sf_file <- fs::path(kml_cache, "italy_grid_sf.rds")

if (fs::file_exists(italy_grid_sf_file)==FALSE) {
  italy_grid_sf <- difference_sf %>%
    sf::st_transform(3857) %>% 
    sf::st_filter(italy_base %>% sf::st_transform(3857)) %>% 
    sf::st_transform(4326)
  saveRDS(object = italy_grid_sf, file = italy_grid_sf_file)
} else {
  italy_grid_sf <- readr::read_rds(path = italy_grid_sf_file)
}


######## Details for video ###########
fs::dir_create(fs::path("kml_4k", "video"))
###### South Tyrol #########


x <- "Trentino-Alto Adige"
current_region_sf <- italy_plus300_grid_sf %>%
  sf::st_transform(crs = 3857) %>% 
  sf::st_filter(ll_get_nuts_it(level = 2) %>%
                  sf::st_transform(crs = 3857) %>% 
                  dplyr::filter(DEN_REG==x) %>% 
                  sf::st_buffer(dist = units::as_units(x = 100, "km"))) %>% 
  dplyr::select(difference) %>% 
  sf::st_transform(crs = 4326)


current_region_file <- fs::path("kml_4k", "video", paste0(x, "_plus_100_grid", ".kml"))

if (fs::file_exists(current_region_file)==FALSE) {
  ll_export_sf_to_kml(sf = current_region_sf,
                      path = current_region_file,
                      name = "difference",
                      keep_other_columns = FALSE,
                      line_width = "12pt",
                      label_text = paste0(round(x = current_region_sf$difference, digits = 1), "°"),
                      label_size = "48pt", 
                      fill_colour = paste0(pal_fix(current_region_sf$difference), "66"), 
                      line_colour = paste0(pal_fix(current_region_sf$difference), "cc"))
}



current_region_file <-fs::path("kml_4k", "video", paste0(x, "_plus_100_label_adjusted_12_12", ".kml"))

if (fs::file_exists(current_region_file)==FALSE) {
  ll_export_sf_to_kml(sf = get_adjusted_label_location(grid = current_region_sf, x_percent = 12, y_percent = 12),
                      path = current_region_file,
                      name = "difference",
                      line_width = "1pt",
                      label_text = paste0(round(x = current_region_sf$difference, digits = 1), "°"),
                      label_scale = 4,
                      icon_url = "",
                      icon_colour = "#ffffff00",
                      fill_colour = paste0(pal_fix(current_region_sf$difference), "66"), 
                      line_colour = paste0(pal_fix(current_region_sf$difference), "cc"))
  
}


trentinoaa_lau_adm <- ll_get_nuts_it(level = "lau") %>% 
  sf::st_filter(y = ll_get_nuts_it(level = 2) %>%
                  dplyr::filter(DEN_REG == "Trentino-Alto Adige"|DEN_REG == "Lombardia"))


ll_export_sf_to_kml(sf = trentinoaa_lau_adm,
                    path = fs::path("kml_4k", "video", "trentinoaa_lau_adm.kml"),
                    line_width = "16px",
                    line_colour = "#f6c7ffcd",
                    fill_colour = "#00000000")

ll_export_sf_to_kml(sf = trentinoaa_lau_adm %>% sf::st_centroid(),
                    path = fs::path("kml_4k", "video", "trentinoaa_lau_adm_labels.kml"),
                    label_text = trentinoaa_lau_adm$COMUNE,
                    icon_colour = "#00000000",
                    label_scale = 4,
)

ll_export_sf_to_kml(sf = ll_get_nuts_it(name = "Martello", level = "lau"),
                    path = fs::path("kml_4k", "video", "martello_lau_adm.kml"),
                    line_width = "16px",
                    line_colour = "#f6c7ffcd",
                    fill_colour = "#fffcc7cd")


martello_pop <- ll_get_population_grid(match_sf = ll_get_nuts_it(name = "Martello", level = "lau"),
                                       match_country = "IT") 

ll_export_sf_to_kml(sf = martello_pop,
                    label_text = martello_pop$TOT_P,
                    fill_colour = "#00000000",
                    line_width = 6,
                    path = fs::path("kml_4k", "video", "martello_pop_grid.kml"))

ll_export_sf_to_kml(sf = get_adjusted_label_location(martello_pop, x_percent = 22, y_percent = 22),
                    label_text = martello_pop$TOT_P,
                    fill_colour = "#00000000",
                    icon_colour = "#00000000",
                    line_width = 6,
                    label_scale = 2.6,
                    path = fs::path("kml_4k", "video", "martello_pop_grid_label_adjusted_22.kml"))


pop_centroid_1 <- ll_find_pop_centre(sf_location = ll_get_nuts_it(name = "Martello", level = "lau"),
                                     sf_population_grid = martello_pop,
                                     power = 1)
pop_centroid_2 <- ll_find_pop_centre(sf_location = ll_get_nuts_it(name = "Martello", level = "lau"),
                                     sf_population_grid = martello_pop,
                                     power = 2)

ll_export_sf_to_kml(sf = pop_centroid_1,
                    path = fs::path("kml_4k", "video", "martello_pop_centroid_flag_scale5_c_y.kml"),
                    icon_url = "https://openclipart.org/image/2000px/svg_to_png/192457/1396291320.png",
                    icon_colour = "#ffff00ff",
                    icon_scale = 5)


###### Emilia-Romagna #########

x <- "Emilia-Romagna"
current_region_sf <- italy_plus300_grid_sf %>%
  sf::st_transform(crs = 3857) %>% 
  sf::st_filter(ll_get_nuts_it(level = 2) %>%
                  sf::st_transform(crs = 3857) %>% 
                  dplyr::filter(DEN_REG==x) %>% 
                  sf::st_buffer(dist = units::as_units(x = 100, "km"))) %>% 
  dplyr::select(difference) %>% 
  sf::st_transform(crs = 4326)


current_region_file <- fs::path("kml_4k", "video", paste0(x, "_plus_100_grid", ".kml"))

if (fs::file_exists(current_region_file)==FALSE) {
  ll_export_sf_to_kml(sf = current_region_sf,
                      path = current_region_file,
                      name = "difference",
                      keep_other_columns = FALSE,
                      line_width = "12pt",
                      label_text = paste0(round(x = current_region_sf$difference, digits = 1), "°"),
                      label_size = "48pt", 
                      fill_colour = paste0(pal_fix(current_region_sf$difference), "66"), 
                      line_colour = paste0(pal_fix(current_region_sf$difference), "cc"))
}



current_region_file <-fs::path("kml_4k", "video", paste0(x, "_plus_100_label_adj_12_12_scale4", ".kml"))

if (fs::file_exists(current_region_file)==FALSE) {
  ll_export_sf_to_kml(sf = get_adjusted_label_location(grid = current_region_sf, x_percent = 12, y_percent = 12),
                      path = current_region_file,
                      name = "difference",
                      line_width = "1pt",
                      label_text = paste0(round(x = current_region_sf$difference, digits = 1), "°"),
                      label_size = "48pt",
                      label_scale = 4,
                      icon_url = "",
                      icon_colour = "#ffffff00",
                      fill_colour = paste0(pal_fix(current_region_sf$difference), "66"), 
                      line_colour = paste0(pal_fix(current_region_sf$difference), "cc"))
  
}

#### set base to higher to prevent rendering issues #### 
current_region_file <-fs::path("kml_4k", "video", paste0(x, "_plus_100_grid_above_ground", ".kml"))

if (fs::file_exists(current_region_file)==FALSE) {
  current_region_sf %>%
    st_coordinates() %>%
    as_tibble() %>%
    mutate(Z = 50) %>% 
    sf::st_as_sf(coords = c("X", "Y", "Z"), dim = "XYZ") %>% 
    group_by(L2) %>% 
    summarise(geometry = st_combine(geometry)) %>% 
    st_cast("POLYGON") %>% 
    dplyr::mutate(altitudeMode="relativeToGround",
                  extrude = TRUE) %>% 
    ll_export_sf_to_kml(path = current_region_file,
                        line_width = "12pt",
                        fill_colour = paste0(pal_fix(current_region_sf$difference), "66"), 
                        line_colour = paste0(pal_fix(current_region_sf$difference), "cc"))
  
  
}

emilia_romagna_lau_adm <- ll_get_nuts_it(level = "lau") %>% 
  sf::st_filter(y = ll_get_nuts_it(level = 2) %>%
                  dplyr::filter(DEN_REG == "Emilia-Romagna"|DEN_REG == "Veneto"|DEN_REG == "Lombardia"))


ll_export_sf_to_kml(sf = emilia_romagna_lau_adm,
                    path = fs::path("kml_4k", "video", "emilia_romagna_lau_adm.kml"),
                    line_width = "16px",
                    line_colour = "#f6c7ffcd",
                    fill_colour = "#00000000")


######### venezia ############

x <- "Veneto"
current_region_sf <- italy_plus300_grid_sf %>%
  sf::st_transform(crs = 3857) %>% 
  sf::st_filter(ll_get_nuts_it(level = 2) %>%
                  sf::st_transform(crs = 3857) %>% 
                  dplyr::filter(DEN_REG==x) %>% 
                  sf::st_buffer(dist = units::as_units(x = 100, "km"))) %>% 
  dplyr::select(difference) %>% 
  sf::st_transform(crs = 4326)




current_region_file <-fs::path("kml_4k", "video", paste0(x, "_plus_100_label_adj_12_12_scale4", ".kml"))

if (fs::file_exists(current_region_file)==FALSE) {
  ll_export_sf_to_kml(sf = get_adjusted_label_location(grid = current_region_sf, x_percent = 12, y_percent = 12),
                      path = current_region_file,
                      name = "difference",
                      line_width = "1pt",
                      label_text = paste0(round(x = current_region_sf$difference, digits = 1), "°"),
                      label_size = "48pt",
                      label_scale = 4,
                      icon_url = "",
                      icon_colour = "#ffffff00",
                      fill_colour = paste0(pal_fix(current_region_sf$difference), "66"), 
                      line_colour = paste0(pal_fix(current_region_sf$difference), "cc"))
  
}

#### set base to higher to prevent rendering issues #### 
current_region_file <-fs::path("kml_4k", "video", paste0(x, "_plus_100_grid_above_ground", ".kml"))

if (fs::file_exists(current_region_file)==FALSE) {
  current_region_sf %>%
    st_coordinates() %>%
    as_tibble() %>%
    mutate(Z = 50) %>% 
    sf::st_as_sf(coords = c("X", "Y", "Z"), dim = "XYZ") %>% 
    group_by(L2) %>% 
    summarise(geometry = st_combine(geometry)) %>% 
    st_cast("POLYGON") %>% 
    dplyr::mutate(altitudeMode="relativeToGround",
                  extrude = TRUE) %>% 
    ll_export_sf_to_kml(path = current_region_file,
                        line_width = "12pt",
                        fill_colour = paste0(pal_fix(current_region_sf$difference), "66"), 
                        line_colour = paste0(pal_fix(current_region_sf$difference), "cc"))
  
  
}



# library("osmdata")

# rubicon_sf <- opq_osm_id(type = "relation", id = 2498212) %>%
#   osmdata_sf ()
# ll_export_sf_to_kml(sf = rubicon_sf$osm_lines,
#                     path = fs::path("kml_4k", "video", "rubicon.kml"),
#                     line_width = "16px",
#                     line_colour = "#1d86ffcd",
#                     fill_colour = "#00000000")


############ test #########
ll_export_sf_to_kml(sf = martello_pop %>% st_centroid(),
                    label_text = martello_pop$TOT_P,
                    fill_colour = "#00000000",
                    label_scale = 5,
                    icon_colour = "#ffffff00",
                    path = fs::path("kml_4k", "video", "martello_pop_grid_labels.kml"))


pop_centroid_1 <- ll_find_pop_centre(sf_location = ll_get_nuts_it(name = "Martello", level = "lau"),
                                     sf_population_grid = martello_pop,
                                     power = 1)
pop_centroid_2 <- ll_find_pop_centre(sf_location = ll_get_nuts_it(name = "Martello", level = "lau"),
                                     sf_population_grid = martello_pop,
                                     power = 2)

ll_export_sf_to_kml(sf = pop_centroid_1,
                    path = fs::path("kml_4k", "video", "martello_pop_centroid1.kml"), 
                    icon_colour = "#ff0079b3")

