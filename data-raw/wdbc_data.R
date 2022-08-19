## code to prepare `wdbc_data` dataset goes here

# retrieve paths to datafiles
data.file <- system.file(
  "extdata",
  "wdbc.data",
  package = "titable"
)


# read the two .csv files
wdbc.data <- read.csv(
  data.file,
  stringsAsFactors = TRUE,
  encoding = "UTF-8",
  header = FALSE
)


# Column selection (12 first)
wdbc.data <- wdbc.data[,1:12]

# column renaming
colnames(wdbc.data) <- c("person_id", "diagnosis", "radius", "texture",
                         "perimeter", "area", "smoothness", "compactness",
                         "concavity", "concave_points", "symmetry",
                         "fractal_dimension"
                         )

# Adding binary col for compactness (threshold = 0.1)
wdbc.data[,"compactness_binary"] <- as.factor(
  as.numeric(wdbc.data$compactness > 0.1)
  )

# Adding a col with 4 categories for compactness (quartiles)
wdbc.data[,"compactness_quartile"] <- cut(
  wdbc.data$compactness,
  breaks = c(quantile(wdbc.data$compactness)),
  labels = c(1, 2, 3, 4)
  )

usethis::use_data(wdbc.data, overwrite = TRUE)
