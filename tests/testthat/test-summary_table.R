test_that("Factor descriptive table works for quantitative variables", {

  table <- summary_table(data = wdbc.data,
                         studied_vars = c("radius", "texture",
                                          "compactness_quartile"),
                         dependent = "diagnosis",
                         univariate = FALSE)

  wdbc.data %>%
    dplyr::group_by(diagnosis) %>%
    dplyr::summarise(
      radius_mean = mean(radius, na.rm=TRUE),
      radius_std = sd(radius, na.rm=TRUE)
    ) -> radius_summary

  radius_summary$str <- paste( round( radius_summary$radius_mean, 1 ) , " (",
                               round( radius_summary$radius_std, 1 ), ")",
                               sep=""
                               )

  expect_equal(
    table[ table$label=="radius", "B"],
    radius_summary[ radius_summary$diagnosis=="B",][["str"]]
  )

  expect_equal(
    table[ table$label=="radius", "M"],
    radius_summary[ radius_summary$diagnosis=="M",][["str"]]
  )

})



test_that("Factor descriptive table works for categorical variables", {

  table <- summary_table(data = wdbc.data,
                         studied_vars = c("radius", "texture",
                                          "compactness_quartile"),
                         dependent = "diagnosis",
                         univariate = FALSE)

  compactness_q_summary <- t(
    table(wdbc.data$diagnosis, wdbc.data$compactness_quartile)
  )

  compactness_q_prop <- apply( compactness_q_summary, MARGIN = 2,
                               function(x) round(prop.table(x)*100, 1) )

  compactness_q_B_str <- paste( round( compactness_q_summary[,1], 1 ) , " (",
                                compactness_q_prop[,1], ")", sep="" )

  compactness_q_M_str <- paste( round( compactness_q_summary[,2], 1 ) , " (",
                                compactness_q_prop[,2], ")", sep="" )

  index_start_row <- which(table$label=="compactness_quartile")
  index_end_row <- nlevels(wdbc.data$compactness_quartile) + index_col -1

  expect_equal(
    table[ index_start_row:index_end_row, "B"],
    compactness_q_B_str
  )

  expect_equal(
    table[ index_start_row:index_end_row, "M"],
    compactness_q_M_str
  )

})




test_that("Checking OR univariate computation for quantitative variables", {

  table <- summary_table(data = wdbc.data,
                         studied_vars = c("radius", "texture",
                                          "compactness_quartile"),
                         dependent = "diagnosis",
                         univariate = TRUE)


  ### Testing radius (quantitative variable)
  model <- glm( diagnosis ~ radius ,
                data = wdbc.data, family=binomial )

  OR_radius <- round( exp( coef(summary(model)) )["radius", "Estimate"],3)
  IC_min_radius <- round( exp(confint(model)["radius","2.5 %"]), 3)
  IC_max_radius <- round( exp(confint(model)["radius","97.5 %"]), 3)
  p_value <- round( coef(summary(model))["radius","Pr(>|z|)"], 3)


  OR_radius_str <- paste( round( OR_radius , 3 ) , " (",
                          round( IC_min_radius, 3 ), "-",
                          round( IC_max_radius, 3 ), ", p=",
                          round( p_value, 3 ), ")",
                          sep=""
  )

  expect_equal(
    table[ table$label=="radius", "OR (univariate)"],
    OR_radius_str
  )

})




test_that("Checking OR univariate computation for qualitative variables", {

  table <- summary_table(data = wdbc.data,
                         studied_vars = c("radius", "texture",
                                          "compactness_quartile"),
                         dependent = "diagnosis",
                         univariate = TRUE)


  ### Testing compactness_quartile (qualitative variable)
  model <- glm( diagnosis ~ compactness_quartile ,
                data = wdbc.data, family=binomial )

  level_sel <- levels(wdbc.data$compactness_quartile)[3]
  name_var <- paste("compactness_quartile", level_sel, sep="")

  OR_cq <- round( exp( coef(summary(model)) )[name_var, "Estimate"],3)
  IC_min_cq <- round( exp(confint(model)[name_var,"2.5 %"]), 3)
  IC_max_cq <- round( exp(confint(model)[name_var,"97.5 %"]), 3)
  p_value <- round( coef(summary(model))[name_var,"Pr(>|z|)"], 3)


  OR_cq_str <- paste( round( OR_cq , 3 ) , " (",
                      round( IC_min_cq, 3 ), "-",
                      round( IC_max_cq, 3 ), ", p=",
                      round( p_value, 3 ), ")",
                      sep=""
  )

  expect_equal(
    table[ table$levels==level_sel, "OR (univariate)"],
    OR_cq_str
  )

})
