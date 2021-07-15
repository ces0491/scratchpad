image_paths <- list.files("C:/Users/Cesaire Tobias/Pictures/test", full.names = TRUE)
image_path_df <- data.frame(path = image_paths)
image_path_df <- image_path_df %>% 
  dplyr::mutate(name = gsub(".*[//]([^.]+)[.].*", "\\1", path))

image_list <- list()

for(img in image_path_df$path) {
  pic <- magick::image_read(img)
  image_list[[img]] <- pic
}

image_df <- image_list %>% 
  tibble::enframe(name = "path", value = "image") %>% 
  dplyr::left_join(image_path_df, by = "path")

comp_images <- function(img_tbl) {

  assertthat::assert_that(all(names(img_tbl) %in% c("image", "image_comp")))
  
  result <- magick::image_compare_dist(img_tbl$image[[1]], img_tbl$image_comp[[1]])
  result[[1]]
}

dist_df <- image_df %>% 
  tidyr::crossing(name_comp = .$name) %>% 
  dplyr::left_join(image_df, by = c("name_comp" = "name")) %>%
  tidyr::unite("id", c("name", "name_comp")) %>% 
  dplyr::select(id, image.x, image.y) %>%
  dplyr::rename(image = image.x,
                image_comp = image.y) %>%
  tidyr::nest(-id, .key = images) %>% 
  dplyr::mutate(distortion = purrr::map(images, comp_images))


### identical images should have a distortion value of ~1, the less similar, the closer to 0.


