test_that("embedName output is correct", {
  en <- embedName$new()
  expect_true(any(grepl('embedName', class(en))))
  expect_true(any(grepl('EmbedName', class(en$en))))

  p <- en$embed_name('John Jonathan')
  expect_true(any(grepl('Tensor', class(p))))
  expect_identical(dim(p$numpy()), c(1L, 1024L))

  p <- en$embed_name(c('John Jonathan', 'Mark Anderson'))
  expect_identical(dim(p$numpy()), c(2L, 1024L))

  
  p <- en$cosine_similarity('John Jonathan', 'Mark Anderson')
  expect_true(p$numpy()<=1 && p$numpy()>=-1)

  p <- en$cosine_similarity(c('John Jonathan', 'Mark Anderson'), c('Jonathan, Jonathan', 'Anderson, Mark E.'))
  expect_identical(dim(p$numpy()), c(2L, 2L))
  expect_true(all(diag(p$numpy())>0.9))

})
