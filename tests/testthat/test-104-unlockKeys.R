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

