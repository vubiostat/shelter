context("unlockKeys")

library(mockery)
library(keyring)
library(checkmate)


test_that(
  "unlockKeys pulls API_KEY and opens connection from keyring returning as list",
  {
    stub(unlockKeys, ".unlockYamlOverride", list()) # No yaml
    stub(unlockKeys, ".unlockENVOverride", list())  # No ENV
    stub(unlockKeys, ".unlockKeyring", NULL) # Unlocked.

    bf <- backend_file$new()
    keyring <- 'sheltertest'

    # In case of previous test failing
    try(bf$keyring_delete(keyring), TRUE)

    bf$keyring_create(keyring, 'abc123')
    key_set_with_value(
          service='shelter',
          username='testdb',
          password='123',
          keyring=keyring)

    expect_silent(x <- unlockKeys(c(rcon='testdb'), function(x, ...) x, keyring="sheltertest", service='shelter'))
    expect_true("rcon" %in% names(x))
    expect_equal(x$rcon, '123')
    bf$keyring_delete(keyring)
  }
)
