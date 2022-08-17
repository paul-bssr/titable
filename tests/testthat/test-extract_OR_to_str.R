test_that(
  "Extraction of characteristics coef works for a logistic regression model", {

    model_1 <- glm( diagnosis ~ texture + radius + perimeter,
                    data=wdbc.data,
                    family="binomial")

    expect_equal(
      extract_OR_to_str(model_1, studied_var = "texture"),
      "1.275 (1.169-1.398, p=0)"
    )
  }
)


test_that(
  "Rejection of non-binomial glm models (i.e. not logistic regression)", {

    model_1 <- glm( diagnosis ~ texture + radius + perimeter,
                    data=wdbc.data,
                    family="binomial")

    model_2 <- glm( as.integer(perimeter) ~ radius,data=wdbc.data,
                    family="poisson")

    expect_error( extract_OR_to_str(model_1, studied_var = 3),
                  "Input studied_var must be a character")

    expect_error( extract_OR_to_str(model_2, studied_var = "radius"),
                  "Model must be binomial family glm")

  }
)
