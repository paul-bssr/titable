extract_OR_from_model <- function(model, studied_var){
  # Function to summarize OR, IC, pvalue from model
  # Returns : OR, IC_min, IC_max, p

  # OR, p extraction from coef
  coef = coef(summary(model))
  cond_studied_var_coef = grepl( studied_var , rownames(coef))
  OR = exp( coef[cond_studied_var_coef ,"Estimate"] )
  p = coef[cond_studied_var_coef,"Pr(>|z|)"]

  # IC_min, IC_max extraction from confint
  conf_int = confint(model)
  cond_conf_int = grepl( studied_var , rownames(conf_int))
  IC_min = exp( conf_int[cond_conf_int, "2.5 %"] )
  IC_max = exp( conf_int[cond_conf_int, "97.5 %"] )
  return( c(OR, IC_min, IC_max, p) )
}
