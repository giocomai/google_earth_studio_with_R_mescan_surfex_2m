library("av")

base_folder <- ""

segments <- c("00_sky_to_martello",
              "01_martello_combo",
              "02-san_benedetto-cervia_01",
              "03-cervia_venezia_02")


files <- c(fs::dir_ls(fs::path(base_folder, segments, "footage")),
           rev(fs::dir_ls(fs::path(base_folder, "04-venezia_to_world", "footage"))))

## full video
av::av_encode_video(input = files,
                    output = "full.mp4",
                    framerate = 25)

## by segment

purrr::walk(.x = segments, 
            .f = function(x) {
              files <- fs::dir_ls(fs::path(base_folder, x, "footage"))
              av::av_encode_video(input = files,
                                  output = paste0(x, ".mp4"),
                                  framerate = 25)
            })
