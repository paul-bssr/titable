test_that("Test of str_transform_OR_with_IC with numeric values", {
  expect_equal(str_transform_OR_with_IC(OR=2.02, IC_min=1.803, IC_max=2.405, pvalue=0.002),
               "2.02 (1.803-2.405, p=0.002)")
})
