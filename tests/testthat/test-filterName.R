test_that("filterName output is correct", {
  f <- filterName$new(k = 2)
  expect_true(any(grepl('filterName', class(f))))
  expect_true(any(grepl('FilterName', class(f$fn))))

  p <- f$filter_names(
    c('Alexander Michaelson', 'Michael Alexander', 'John Johnson', 'Christopher Chris'), 
    c('Michaelson, Alexander G.', 'Alexander, Michael, Jr.', 'Johnson, Jonathan', 'Winkel, Perry')
  )
  expect_identical(
    aggregate(name2~name1, p, length)$name2, rep(2L, 4)
  )
  expect_true(ncol(p) == 2)

  f <- filterName$new(k = 3, return_sim = TRUE)
  p <- f$filter_names(
    c('Alexander Michaelson', 'Michael Alexander', 'John Johnson', 'Christopher Chris'), 
    c('Michaelson, Alexander G.', 'Alexander, Michael, Jr.', 'Johnson, Jonathan', 'Winkel, Perry')
  )
  expect_identical(dim(p), c(12L, 3L))
  expect_contains(colnames(p), 'sim')
  expect_true(all(p$sim<=1 & p$sim>=-1))



})
