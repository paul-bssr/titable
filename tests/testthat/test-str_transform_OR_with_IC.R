testthat::test_that("Test of str_transform_OR_with_IC with numeric values", {
  testthat::expect_equal(str_transform_OR_with_IC(OR=2.02, IC_min=1.803, IC_max=2.405, pvalue=0.002),
               "2.02 (1.803-2.405, p=0.002)")
  testthat::expect_equal(str_transform_OR_with_IC(OR=2.02, IC_min=1.803, IC_max=2.405,
                                        pvalue=0.002055, digits=2, digits_p=3),
               "2.02 (1.8-2.4, p=0.00206)"
               )
  testthat::expect_equal(str_transform_OR_with_IC(OR=2.02, IC_min=1.803, IC_max=2.405,
                                        pvalue=0.002055, digits=2, digits_p=3,
                                        p_limit=0.01),
               "2.02 (1.8-2.4, p<0.01)"
  )
  testthat::expect_equal(str_transform_OR_with_IC(OR=2.02, IC_min=1.803, IC_max=2.405,
                                        pvalue=0.002055, digits=2, digits_p=3,
                                        p_limit=0.0001),
               "2.02 (1.8-2.4, p=0.00206)"
  )
})

testthat::test_that("Non-numeric or missing inputs should error", {
  testthat::expect_error(str_transform_OR_with_IC(OR="2.0", IC_min=1.8, IC_max=2.4,
                                        pvalue=0.01),
               "Input OR must be numeric")
  testthat::expect_error(str_transform_OR_with_IC(OR=2.0, IC_min="1.8", IC_max=2.4,
                                        pvalue=0.01),
               "Input IC_min must be numeric")
  testthat::expect_error(str_transform_OR_with_IC(OR=2.0, IC_min=1.8, IC_max="-2.4",
                                        pvalue=0.01),
               "Input IC_max must be numeric")
  testthat::expect_error(str_transform_OR_with_IC(OR=2.0, IC_min=1.8, IC_max=2.4,
                                        pvalue="-0.01"),
               "Input pvalue must be numeric")
})


testthat::test_that("Negative inputs should error", {
  testthat::expect_error(str_transform_OR_with_IC(OR=-2.0, IC_min=1.8, IC_max=2.4,
                                        pvalue=0.01),
               "Input OR must be a positive number")
  testthat::expect_error(str_transform_OR_with_IC(OR=2.0, IC_min=-1.8, IC_max=2.4,
                                        pvalue=0.01),
               "Input IC_min must be a positive number")
  testthat::expect_error(str_transform_OR_with_IC(OR=2.0, IC_min=1.8, IC_max=-2.4,
                                        pvalue=0.01),
               "Input IC_max must be a positive number")
  testthat::expect_error(str_transform_OR_with_IC(OR=2.0, IC_min=1.8, IC_max=2.4,
                                        pvalue=-0.01),
               "Input pvalue must be a positive number")
})
