test_that("matchName output is correct", {
  
  m <- matchName$new()
  # check class of matchName
  expect_true(any(grepl('matchName', class(m))))
  expect_true(any(grepl('MatchName', class(m$mn))))

  # check prediction type and value
  p <- m$predict_match('John Marcus', 'Marcus, John')
  expect_type(p, 'double')
  expect_true(p>=0 & p<=1)

  # check all_probs method
  p <- m$all_probs(
    c(
      'John Johnson', 'Alex Michaelson', 'Michael Alexander', 
      'Chris Topher', 'Jonathan Johnson', 'Alexander B. Michaelson'
    )
  )
  expect_type(p, 'list')
  expect_true(any(grepl('tbl_df', class(p))))
  expect_true(nrow(p)==36)
  expect_error(m$all_probs())

  # check joining methods
  d1 <- tibble::tibble(
    name1 = c(
      'Alexander Michaelson', 'Michael Alexander', 'John Johnson', 
      'Christopher Chris'
    )
  )
  d2 <- tibble::tibble(
    name2 = c(
      'Michaelson, Alexander G.', 'Alexander, Michael, Jr.', 'Johnson, Jonathan', 
      'Winkel, Perry'
    )
  )

  rm_py_attr <- function(o) {
    out <- o
    attr(out$merged, 'pandas.index') <- NULL
    attr(out$marginal, 'pandas.index') <- NULL
    return(out)
  }

  p <- m$join_name(d1, d2, 'name1', 'name2', how = 'left') |> rm_py_attr()
  p2 <- m$left_join_name(d1, d2, 'name1', 'name2') |> rm_py_attr()
  expect_identical(p, p2)
  expect_equal(nrow(p$merged), 4)
  expect_equal(nrow(p2$merged), 4)
  expect_equal(nrow(p$marginal), 0)
  expect_equal(nrow(p2$marginal), 0)
  p <- p$merged
  p2 <- p2$merged
  expect_false(any(grepl('Winkel', p$name2)))
  expect_false(any(grepl('Winkel', p2$name2)))

  p <- m$join_name(d1, d2, 'name1', 'name2', how = 'right') |> rm_py_attr()
  p2 <- m$right_join_name(d1, d2, 'name1', 'name2') |> rm_py_attr()
  expect_identical(p, p2)
  expect_equal(nrow(p$merged), 4)
  expect_equal(nrow(p2$merged), 4)
  expect_equal(nrow(p$marginal), 0)
  expect_equal(nrow(p2$marginal), 0)
  p <- p$merged
  p2 <- p2$merged
  expect_false(any(grepl('Chris', p$name1)))
  expect_false(any(grepl('Chris', p2$name1)))

  p <- m$join_name(d1, d2, 'name1', 'name2', how = 'inner') |> rm_py_attr()
  p2 <- m$inner_join_name(d1, d2, 'name1', 'name2') |> rm_py_attr()
  expect_identical(p, p2)
  expect_equal(nrow(p$merged), 3)
  expect_equal(nrow(p2$merged), 3)
  expect_equal(nrow(p$marginal), 0)
  expect_equal(nrow(p2$marginal), 0)
  p <- p$merged
  p2 <- p2$merged
  expect_false(any(grepl('Winkel', p$name2)))
  expect_false(any(grepl('Winkel', p2$name2)))
  expect_false(any(grepl('Chris', p$name1)))
  expect_false(any(grepl('Chris', p2$name1)))

  p <- m$join_name(d1, d2, 'name1', 'name2', how = 'outer') |> rm_py_attr()
  p2 <- m$full_join_name(d1, d2, 'name1', 'name2') |> rm_py_attr()
  expect_identical(p, p2)
  expect_equal(nrow(p$merged), 5)
  expect_equal(nrow(p2$merged), 5)
  expect_equal(nrow(p$marginal), 0)
  expect_equal(nrow(p2$marginal), 0)
  p <- p$merged
  p2 <- p2$merged
  expect_true(any(grepl('Winkel', p$name2)))
  expect_true(any(grepl('Winkel', p2$name2)))
  expect_true(any(grepl('Chris', p$name1)))
  expect_true(any(grepl('Chris', p2$name1)))

  # test name deduplication

})

test_that('matchName filtering works as expected', {
  f <- filterName$new(k = 3)
  expect_true(any(grepl('filterName', class(f))))

  m <- matchName$new(filter = f)
  expect_true(any(grepl('FilterName', class(m$filter_match))))

  # check all_probs method
  p <- m$all_probs(
    c(
      'John Johnson', 'Alex Michaelson', 'Michael Alexander', 
      'Chris Topher', 'Jonathan Johnson', 'Alexander B. Michaelson'
    )
  )
  expect_true(nrow(p)==18)
  expect_identical(aggregate(name2~name1, p, length)$name2, rep(3L, 6))

})