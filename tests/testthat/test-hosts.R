test_that("the predicate names exactly the supported hosts", {
  expect_identical(supported_host_predicate(), "host IN ('github')")
  expect_identical(supported_host_predicate("r.host", c("github", "gitlab")), "r.host IN ('github', 'gitlab')")
  expect_identical(supported_host_predicate(hosts = "o'x"), "host IN ('o''x')")
})

test_that("no script filters on a literal host any more", {
  src <- unlist(lapply(list.files("../../scripts", "\\.R$", full.names = TRUE), readLines))
  expect_false(any(grepl("host = 'github'|host='github'|LIKE 'github\\.com/%'", src)))
})
