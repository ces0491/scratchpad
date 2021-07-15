library(magrittr)

#### use a VGG16 model without the top layers to generate features
vgg16_notop <- keras::application_vgg16(weights = 'imagenet', include_top = FALSE)

allimages <- "C:/Users/Cesaire Tobias/Pictures/sort"

### helper function to extract features for one image into a vector
getImageFeatures <- function(imgf) {
  
  img <- keras::image_load(imgf, target_size = c(224,224))
  x <- keras::image_to_array(img)
  
  dim(x) <- c(1, dim(x))
  x <- keras::imagenet_preprocess_input(x)
  
  # extract features
  features <- vgg16_notop %>% 
    stats::predict(x)
  
  as.numeric(features)
}

#### apply function on all images in the folder and put it into one matrix
#### matrix should have N rows, where N is the number of images
#### and 25088 columns, the dimension of a feature vector
out <- purrr::map(allimages, getImageFeatures)
out2 <- data.frame(out)
featurematrix = t(as.matrix(out2))
dim(featurematrix)

# use the umap package in python on the featurematrix that we just calculated
library(reticulate)
use_condaenv("my_py36")

## remove the vgg_notop object, for some reason it points to 
## conda r-tensorflow env where there is no umap module

rm(vgg16_notop)

umap = import("umap")

embedding = umap$UMAP(
  n_neighbors = 5L,
  n_components = 3L,
  min_dist = 0.25,
  metric='euclidean'
)

## compute UMAP with 3 components
embedding_out = embedding$fit_transform(featurematrix)

## create data for plotly plot
plotdata = data.frame(round(embedding_out,2))
plotdata$image = paste0(
  "<img src='",
  allimages, "'",
  " height='600' width='800'>")

### create linkable plotly and image with crosstalk 
sd <- SharedData$new(plotdata)

p <- plotly::plot_ly(
  sd, x = ~X1, y = ~X2,z = ~X3,
  width	= 800,
  height = 600
) %>% 
  add_markers(alpha = 0.85) %>%
  highlight("plotly_hover", dynamic = TRUE)

tbl = DT::datatable(
  sd, 
  escape = FALSE,  
  options = list(
    dom = 't',
    pageLength = 1
  ),
  rownames = FALSE
)

bscols(widths = c(6, 6), p, tbl)