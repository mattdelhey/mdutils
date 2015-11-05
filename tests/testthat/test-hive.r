context("hive")

test_that("hive() can pull data") {
  vec <- hive("select * from t_location limit 100")
}

test_that("as.hive.tbl() can pull data") {
  out <- as.hive.tbl("select * from t_location limit 100")
}

test_that("read.hive.tbl() can read data") {
  out <- read.hive.tbl("t_location", 10)
}


