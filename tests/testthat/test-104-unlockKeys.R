context("unlockKeys")

library(mockery)
library(checkmate)

test_that(
  "unlockKeys pulls API key and opens connection from keyring returning as list",
  {
    local_mocked_bindings(.unlockYamlOverride=mock(list()), # No yaml
                          .unlockENVOverride=mock(list())) # No ENV

    keyring <- 'sheltertest'

    # In case of previous test failing
    try(keyring_delete(keyring), TRUE)

    keyring_create(keyring, 'abc123@#$')

    key_set(keyring,'testdb','123')

    m <- mock('ABC')

    expect_silent(x <- unlockKeys(
      c(rcon='testdb'),
      keyring,
      function(x, ...) m(x, ...),
      passwordFUN = function(...) 'abc123@#$',
      abc=456))
    expect_true("rcon" %in% names(x))
    expect_equal(x$rcon, 'ABC')

    keyring_delete(keyring)

    # Check the "..." passing
    expect_called(m, 1)
    expect_equal(mock_args(m)[[1]][['abc']], 456)
  }
)

test_that(
  "unlockKeys sends ... to .unlockYamlOverride",
  {
    m <- mock(list(xyz=456))

    local_mocked_bindings(.unlockYamlOverride=m)
    unlockKeys(c("TestRedcapAPI"),
               "shelter",
               function(key, ...) TRUE,
               abc=123)
    expect_called(m, 1)
    expect_equal(mock_args(m)[[1]][['abc']], 123)
  }
)

test_that(
  "unlockKeys sends ... to .unlockENVOverride",
  {
    m <- mock(list(xyz=456))

    local_mocked_bindings(.unlockENVOverride=m)
    unlockKeys(c("TestRedcapAPI"),
               "shelter",
               function(key, ...) TRUE,
               abc=123)
    expect_called(m, 1)
    expect_equal(mock_args(m)[[1]][['abc']], 123)
  }
)

test_that(
  "unlockKeys validates arguments properly",
  {
    stubby <- function(key, ...) TRUE
    expect_error(unlockKeys("Test", TRUE, stubby),
                 "Variable 'keyring': Must be of type 'string'")

    expect_error(unlockKeys("Test", rep("joe",2), stubby), "Variable 'keyring': Must have length 1")
    expect_error(unlockKeys("Test", NULL, stubby), "Variable 'keyring': Must be of type 'string'")

    expect_error(unlockKeys(123, "shelter", stubby), "Variable 'connections': Must be of type 'character'")
    expect_error(unlockKeys("test", "shelter", stubby, envir=TRUE), "Variable 'envir': Must inherit from class 'environment'")
    expect_error(unlockKeys("test", "shelter", TRUE), "Variable 'connectFUN': Must be a function")
    expect_error(unlockKeys("test", "shelter", stubby, passwordFUN=FALSE), "Variable 'passwordFUN': Must be a function")
    expect_error(unlockKeys("test", "shelter", stubby, max_attempts=FALSE), "Variable 'max_attempts': Must be of type 'numeric'")
  }
)

