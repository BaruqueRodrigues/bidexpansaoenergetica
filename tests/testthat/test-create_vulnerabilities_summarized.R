

test_that("Check if path it's correct", {
  path_test1 <- "./ETL_pipeline/data/data-output/"
  path_test2 <- "./test_folder/"
  path_test3 <- ""

  expect_true(create_vulnerabilities_summarized(exdir = path_test1))
  expect_true(create_vulnerabilities_summarized(exdir = path_test2))
  expect_error(create_vulnerabilities_summarized(exdir = path_test3))

  # if(file.exists(path_test1)){
  #   file.remove(path_test1)
  #   unlink(dirname(path_test1), recursive=TRUE)
  # }
  #
  # if(file.exists(path_test2)){
  #   file.remove(path_test2)
  #   unlink(dirname(path_test2), recursive=TRUE)
  # }
  #
  # if(file.exists(path_test3)){
  #   file.remove(path_test3)
  #   unlink(dirname(path_test3), recursive=TRUE)
  # }

})
