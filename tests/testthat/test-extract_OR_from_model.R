testthat::test_that(
  "Extraction of characteristics coef works for a logistic regression model", {

    model_1 <- glm( diagnosis ~ texture + radius + perimeter,
                    data=wdbc.data,
                    family="binomial")

    model_2 <- glm( diagnosis ~ compactness_binary + radius + perimeter,
                    data=wdbc.data,
                    family="binomial")

    model_3 <- glm( diagnosis ~ compactness_quartile + radius + perimeter,
                    data=wdbc.data,
                    family="binomial")

    testthat::expect_equal(
      signif( extract_OR_from_model(model_1, studied_var = "texture"), 5),
      c(1.2748, 1.1691, 1.3983, 9.2052e-08)
      )

    testthat::expect_equal( signif(
      extract_OR_from_model(model_2,
                            studied_var = "compactness_binary",
                            level=1),
      5),
      c(1.54370, 0.58894, 4.04250, 0.37539)
    )

    testthat::expect_equal( signif(
      extract_OR_from_model(model_3,
                            studied_var = "compactness_quartile",
                            level=3),
      5),
      c(0.78374, 0.19257, 3.1757, 0.73179)
    )
    }
  )



testthat::test_that(
  "Checking processes work", {

    model_1 <- glm( diagnosis ~ texture + radius + perimeter,
                    data=wdbc.data,
                    family="binomial")

    model_2 <- glm( as.integer(perimeter) ~ radius,data=wdbc.data,
                    family="poisson")

    testthat::expect_error( extract_OR_from_model(model_1, studied_var = 3),
                  "Input studied_var must be a character")

    testthat::expect_error( extract_OR_from_model(model_1, studied_var = "area"),
                  "Input studied_var not in model variables")

    testthat::expect_error( extract_OR_from_model(model_2, studied_var = "radius"),
                  "Model must be binomial family glm")

  }
)
