context(".unlockENVOverride")

library(checkmate)
library(mockery)

test_that(
  ".unlockENVOverride return empty when override ENV doesn't exist",
  {
    stub(.unlockENVOverride, "Sys.getenv", "")
    x <- .unlockENVOverride("TestRedcapAPI", url)
    expect_class(x, "list")
    expect_true(length(x$TestRedcapAPI) == 0)
  }
)

test_that(
  ".unlockENVOverride will stop when only one of two ENV's are found",
  {
    stub(.unlockENVOverride, "sapply", c("", "YO"))
    expect_error(.unlockENVOverride(c("x", "y"), url))
  }
)

test_that(
  ".unlockENVOverride returns an entry for every connection",
  {
    stub(.unlockENVOverride, "Sys.getenv", "xyz")
    stub(.unlockENVOverride, ".connectAndCheck", TRUE)
    x <- .unlockENVOverride(c("TestRedcapAPI", "Sandbox"),
                            list(function(...) TRUE, function(...) TRUE))
    expect_true(x$TestRedcapAPI)
    expect_true(x$Sandbox)
  }
)

test_that(
  ".unlockENVOverride returns an entry for every connection renamed as requested",
  {
    stub(.unlockENVOverride, "Sys.getenv", "xyz")
    stub(.unlockENVOverride, ".connectAndCheck", TRUE)
    x <- .unlockENVOverride(c(rcon="TestRedcapAPI", sand="Sandbox"),
                            list(function(...) TRUE, function(...) TRUE))
    expect_true(x$rcon)
    expect_true(x$sand)
  }
)
