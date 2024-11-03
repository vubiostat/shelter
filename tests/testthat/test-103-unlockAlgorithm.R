context(".unlockAlgorithm")

library(mockery)
library(checkmate)

test_that(
  ".unlockAlgorithm asks for API key if not stored, opens connection and stores",
  {
    m <- mock(TRUE)
    stub(.unlockAlgorithm, ".unlockYamlOverride", list()) # No yaml
    stub(.unlockAlgorithm, ".unlockENVOverride", list()) # No ENV

    stub(.unlockAlgorithm, ".unlockKeyring", TRUE)
    stub(.unlockAlgorithm, "key_list",
         data.frame(service="redcapAPI", username="Nadda"))
    stub(.unlockAlgorithm, "key_set_with_value", m)

    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; "xyz"}

    n <- mock(TRUE)

    x <- .unlockAlgorithm(
      c(rcon="George"),
      list(n),
      keyring="API_KEYs",
      envir=NULL,
      passwordFUN=passwordFUN)

    expect_true("rcon" %in% names(x))
    expect_true(x$rcon)
    expect_called(m, 1) # Called key_set_with_value once
    expect_equal(mock_args(m)[[1]], list("API_KEYs", "shelter", "George", "xyz" ))
    expect_called(n, 1) # Called connectFUNs
  }
)
