context(".unlockYamlOverride")

library(checkmate)
library(mockery)

test_that(
  ".unlockYamlOverride return empty list when override yaml doesn't exist",
  {
    stub(.unlockYamlOverride, "file.exists",mock(FALSE))

    x <- .unlockYamlOverride("TestRedcapAPI", list(function(...) TRUE))

    expect_class(x, "list")
    expect_true(length(x) == 0)
  }
)

test_that(
  ".unlockYamlOverride stops if no shelter entry is found",
  {
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml", list())

    expect_error(.unlockYamlOverride("shelter", list(function(...) TRUE), "shelter"),
                 "does not contain required 'shelter' entry")
  }
)

test_that(
  ".unlockYamlOverride stops if no redcapAPI$keys entry is found",
  {
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml", list(shelter=list()))

    expect_error(.unlockYamlOverride("shelter", function(...) TRUE, "shelter"),
                 "does not contain required 'keys' entry")
  }
)

test_that(
  ".unlockYamlOverride stops if a list redcapAPI$keys entry is found",
  {
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml", list(shelter=list(keys=list(TestRedcapAPI=list()))))

    expect_error(.unlockYamlOverride("TestRedcapAPI", list(function(...) TRUE), "shelter"),
                 "does not have API_KEY for")
  }
)

test_that(
  ".unlockYamlOverride stops if a non string redcapAPI$keys entry is found",
  {
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml", list(redcapAPI=list(keys=list(TestRedcapAPI=TRUE))))

    expect_error(.unlockYamlOverride("TestRedcapAPI", list(function(...) TRUE), yaml_tag='redcapAPI'),
                 "invalid entry")
  }
)

test_that(
  ".unlockYamlOverride returns an entry for every connection and passes ...",
  {
    m <- mock(1, 2)
    f <- function(key, ...) m(key, ...)
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml",
                  list(shelter=list(keys=list(TestRedcapAPI='xyz', Sandbox='xyz'))))
    x <- .unlockYamlOverride(c("TestRedcapAPI", "Sandbox"), list(f,f), "shelter",abc=3)
    expect_equal(x$TestRedcapAPI, 1)
    expect_equal(x$Sandbox,2)
    expect_called(m, 2)
    expect_equal(mock_args(m)[[1]][['abc']], 3)
    expect_equal(mock_args(m)[[2]][['abc']], 3)
  }
)

test_that(
  ".unlockYamlOverride returns an entry for every connection renamed as requested",
  {
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml",
                  list(shelter=list(keys=list(TestRedcapAPI='xyz', Sandbox='xyz'))))
    x <- .unlockYamlOverride(c(rcon="TestRedcapAPI", sand="Sandbox"), list(function(...) TRUE, function(...) TRUE), 'shelter')
    expect_true(x$rcon)
    expect_true(x$sand)
  }
)

test_that(
  ".unlockYamlOverride allows local override of ... options",
  {
    m <- mock(1, 2)
    f <- function(key, ...) m(key, ...)
    stub(.unlockYamlOverride, "file.exists", TRUE)
    stub(.unlockYamlOverride, "read_yaml",
                  list(shelter=list(keys=list(TestRedcapAPI='xyz', Sandbox='xyz'), config=list(abc=4, def=5))))
    x <- .unlockYamlOverride(c("TestRedcapAPI", "Sandbox"), list(f,f), "shelter",abc=3,ghi=6)
    expect_called(m, 2)
    expect_equal(mock_args(m)[[1]][['abc']], 4)
    expect_equal(mock_args(m)[[2]][['abc']], 4)
    expect_equal(mock_args(m)[[1]][['def']], 5)
    expect_equal(mock_args(m)[[2]][['def']], 5)
    expect_equal(mock_args(m)[[1]][['ghi']], 6)
    expect_equal(mock_args(m)[[2]][['ghi']], 6)
  }
)

