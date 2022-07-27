extract_OR_to_str <- function(model, studied_var){
  coef <- extract_OR_from_model(model, studied_var)
  str_final <- str_transform(
    OR = coef[1],
    IC_min = coef[2],
    IC_max = coef[3],
    p = coef[4]
  )
  return(str_final)
}
