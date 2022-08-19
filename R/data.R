#' Wisconsin Diagnostic Breast Cancer (WDBC).
#'
#' A dataset containing diagnosis (Malign/benign) for 569 patients with breast
#' cancer and features extracted from tumor imaging.
#'
#' Creators:
#' \describe{
#' \item{Dr. William H. Wolberg}{General Surgery Dept., University of
#' Wisconsin,  Clinical Sciences Center, Madison, WI 53792}
#' \item{W. Nick Street}{Computer Sciences Dept., University of
#' Wisconsin, 1210 West Dayton St., Madison, WI 53706}
#' \item{Olvi L. Mangasarian}{Computer Sciences Dept., University of
#' Wisconsin, 1210 West Dayton St., Madison, WI 53706}
#' }
#'
#' @format A data frame with 569 rows and 12 variables:
#' \describe{
#' \item{person_id}{ID number.}
#' \item{diagnosis}{M = malignant, B = benign}
#' \item{radius}{Mean of distances from center to points on the perimeter}
#' \item{texture}{Standard deviation of gray-scale values.}
#' \item{perimeter}{Computed perimeter}
#' \item{area}{see doc}
#' \item{smoothness}{local variation in radius lengths}
#' \item{compactness}{perimeter^2 / area - 1.0}
#' \item{concavity}{Severity of concave portions of the contour}
#' \item{concave_points}{Number of concave portions of the contour}
#' \item{symmetry}{see doc}
#' \item{fractal_dimension}{"coastline approximation" - 1}
#' \item{compactness_binary}{Factor containing binarized compactness
#' (threshold=0.1). Additional toy parameter.}
#' }
#' \item{compactness_quartile}{Factor containing compactness divided in
#' quartiles. Additional toy parameter.}
#' }
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)}
"wdbc.data"
