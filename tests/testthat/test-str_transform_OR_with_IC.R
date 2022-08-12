test_that("Test of str_transform_OR_with_IC with numeric values", {
  expect_equal(str_transform_OR_with_IC(OR=2.02, IC_min=1.803, IC_max=2.405, pvalue=0.002),
               "2.02 (1.803-2.405, p=0.002)")
  expect_equal(str_transform_OR_with_IC(OR=2.02, IC_min=1.803, IC_max=2.405,
                                        pvalue=0.00205, digits=2, digits_p = 4),
               "2.02 (1.8-2.4, p=0.0021)"
               )
})

test_that("Non-numeric or missing inputs should error", {
  expect_error(str_transform_OR_with_IC(OR="2.0", IC_min=1.8, IC_max=2.4,
                                        pvalue=0.01),
               "Input OR must be numeric")
  expect_error(str_transform_OR_with_IC(OR=2.0, IC_min="1.8", IC_max=2.4,
                                        pvalue=0.01),
               "Input IC_min must be numeric")
  expect_error(str_transform_OR_with_IC(OR=2.0, IC_min=1.8, IC_max="-2.4",
                                        pvalue=0.01),
               "Input IC_max must be numeric")
  expect_error(str_transform_OR_with_IC(OR=2.0, IC_min=1.8, IC_max=2.4,
                                        pvalue="-0.01"),
               "Input pvalue must be numeric")
})


test_that("Negative inputs should error", {
  expect_error(str_transform_OR_with_IC(OR=-2.0, IC_min=1.8, IC_max=2.4,
                                        pvalue=0.01),
               "Input OR must be a positive number")
  expect_error(str_transform_OR_with_IC(OR=2.0, IC_min=-1.8, IC_max=2.4,
                                        pvalue=0.01),
               "Input IC_min must be a positive number")
  expect_error(str_transform_OR_with_IC(OR=2.0, IC_min=1.8, IC_max=-2.4,
                                        pvalue=0.01),
               "Input IC_max must be a positive number")
  expect_error(str_transform_OR_with_IC(OR=2.0, IC_min=1.8, IC_max=2.4,
                                        pvalue=-0.01),
               "Input pvalue must be a positive number")
})
