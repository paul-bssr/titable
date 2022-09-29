testthat::test_that(
  "Extraction of characteristics coef works for a logistic regression model", {

    model_1 <- glm( diagnosis ~ texture + radius + perimeter,
                    data=wdbc.data,
                    family="binomial")

    model_2 <- glm( diagnosis ~ compactness_binary + radius + perimeter,
                    data=wdbc.data,
                    family="binomial")

    testthat::expect_equal(
      extract_OR_to_str(model_1, studied_var = "texture"),
      "1.275 (1.169-1.398, p=9e-08)"
    )

    testthat::expect_equal(
      extract_OR_to_str(model_2, studied_var = "compactness_binary", level="1"),
      "1.544 (0.589-4.043, p=0.4)"
    )
    testthat::expect_equal(
      extract_OR_to_str(model_2, studied_var = "compactness_binary", level="1",
                        digits=2, digits_p = 2),
      "1.54 (0.59-4.04, p=0.38)"
    )
    testthat::expect_equal(
      extract_OR_to_str(model_2, studied_var = "compactness_binary", level="1",
                        digits=2, digits_p = 2, p_limit=0.5),
      "1.54 (0.59-4.04, p<0.5)"
    )
  }
)


testthat::test_that(
  "Checking inputs processes", {

    model_1 <- glm( diagnosis ~ texture + radius + perimeter,
                    data=wdbc.data,
                    family="binomial")

    model_2 <- glm( as.integer(perimeter) ~ radius,data=wdbc.data,
                    family="poisson")

    testthat::expect_error( extract_OR_to_str(model_1, studied_var = 3),
                  "Input studied_var must be a character")

    testthat::expect_error( extract_OR_to_str(model_1, studied_var = "area"),
                  "Input studied_var not in model variables")

    testthat::expect_error( extract_OR_to_str(model_2, studied_var = "radius"),
                  "Model must be binomial family glm")

  }
)
