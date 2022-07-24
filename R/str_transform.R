str_transform_OR_with_IC <- function(OR, IC_min, IC_max, pvalue, digits=3){
  # Function to summarize OR, IC, pvalue from their numerical values
  # in a string

  stopifnot("Input OR must be numeric" = is.numeric(OR))
  stopifnot("Input IC_min must be numeric" = is.numeric(IC_min))
  stopifnot("Input IC_max must be numeric" = is.numeric(IC_max))
  stopifnot("Input pvalue must be numeric" = is.numeric(pvalue))

  OR_str = paste(
    as.character( round( OR, digits = 3) ),
    " (",
    as.character( round( IC_min, digits=3 ) ),
    "-",
    as.character( round( IC_max, digits=3 ) ),
    ", p=",
    as.character( round( pvalue, digits=3 ) ),
    ")",
    sep=""
  )

  return( OR_str )
}
