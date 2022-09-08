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




test_that("Checking OR univariate computation", {

  table <- summary_table(data = wdbc.data,
                         studied_vars = c("radius", "texture",
                                          "compactness_quartile"),
                         dependent = "diagnosis",
                         univariate = TRUE)

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
