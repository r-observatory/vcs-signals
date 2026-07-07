test_that("parse_pages_url recovers owner/name from a github.io URL", {
  expect_equal(parse_pages_url("https://thelovelab.github.io/tximeta"),
               list(host = "github", host_domain = "github.com", owner = "thelovelab", name = "tximeta"))
  expect_equal(parse_pages_url("https://owner.github.io")$name, "owner.github.io")
})

test_that("parse_pages_url returns NULL for non-pages URLs", {
  expect_null(parse_pages_url("https://github.com/o/n"))
  expect_null(parse_pages_url("https://example.org/x"))
  expect_null(parse_pages_url(NA_character_))
})
