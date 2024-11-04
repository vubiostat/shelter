context("Key argument validation")

test_that("key_* keyring doesn't allow multiple", {
  local_reproducible_output(width = 200)
  keyring <- c('sheltertest', 'bridgetoofar')
  error   <- "'keyring': Must have length 1"
  key     <- 'key'
  secret  <- 'secret'
  expect_error(key_exists(keyring, key), error)
  expect_error(key_get(   keyring, key), error)
  expect_error(key_delete(keyring, key), error)
  expect_error(key_set(   keyring, key, secret), error)
})

test_that("key_* keyring doesn't allow non string type", {
  local_reproducible_output(width = 200)
  error   <- "'keyring': Must be of type 'string'"
  key     <- 'key'
  secret  <- 'secret'

  keyring <- TRUE
  expect_error(key_exists(keyring, key), error)
  expect_error(key_get(   keyring, key), error)
  expect_error(key_delete(keyring, key), error)
  expect_error(key_set(   keyring, key, secret), error)

  keyring <- NULL
  expect_error(key_exists(keyring, key), error)
  expect_error(key_get(   keyring, key), error)
  expect_error(key_delete(keyring, key), error)
  expect_error(key_set(   keyring, key, secret), error)
})

test_that("key_* key doesn't allow multiple", {
  local_reproducible_output(width = 200)
  keyring <- 'sheltertest'
  error   <- "'key': Must have length 1"
  key     <- c('key1', 'key2')
  secret  <- 'secret'
  expect_error(key_exists(keyring, key), error)
  expect_error(key_get(   keyring, key), error)
  expect_error(key_delete(keyring, key), error)
  expect_error(key_set(   keyring, key, secret), error)
})

test_that("key_* key doesn't allow non string type", {
  local_reproducible_output(width = 200)
  error   <- "'key': Must be of type 'string'"
  keyring <- 'sheltertest'
  secret  <- 'secret'

  key <- TRUE
  expect_error(key_exists(keyring, key), error)
  expect_error(key_get(   keyring, key), error)
  expect_error(key_delete(keyring, key), error)
  expect_error(key_set(   keyring, key, secret), error)

  key <- NULL
  expect_error(key_exists(keyring, key), error)
  expect_error(key_get(   keyring, key), error)
  expect_error(key_delete(keyring, key), error)
  expect_error(key_set(   keyring, key, secret), error)
})


test_that("key_* secret doesn't allow multiple secrets", {
  local_reproducible_output(width = 200)
  keyring <- 'sheltertest'
  error   <- "'secret': Must have length 1"
  key     <- 'key1'
  secret  <- c('secret1', 'secret2')
  expect_error(key_set(   keyring, key, secret), error)
})

test_that("key_* key doesn't allow non string type", {
  local_reproducible_output(width = 200)
  error   <- "'secret': Must be of type 'string'"
  keyring <- 'sheltertest'
  key     <- 'key1'

  secret  <- TRUE
  expect_error(key_set(   keyring, key, secret), error)

  secret <- NULL
  expect_error(key_set(   keyring, key, secret), error)
})
