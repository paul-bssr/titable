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


usethis::use_data(wdbc.data, overwrite = TRUE)
