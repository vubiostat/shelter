context("Keyring argument validationn")

keyring_delete('sheltertest')

test_that("keyring_* doesn't allow multiple keyrings", {
  local_reproducible_output(width = 200)
  keyring <- c('sheltertest', 'bridgetoofar')
  error   <- "'keyring': Must have length 1"
  pw      <- 'abc$123'
  expect_error(keyring_delete(keyring    ), error)
  expect_error(keyring_lock(  keyring    ), error)
  expect_error(keyring_locked(keyring    ), error)
  expect_error(keyring_create(keyring, pw), error)
  expect_error(keyring_unlock(keyring, pw), error)
})

test_that("keyring_* doesn't allow non string type", {
  local_reproducible_output(width = 200)
  error   <- "'keyring': Must be of type 'string'"
  pw      <- 'abc$123'

  keyring <- TRUE
  expect_error(keyring_delete(keyring    ), error)
  expect_error(keyring_lock(  keyring    ), error)
  expect_error(keyring_locked(keyring    ), error)
  expect_error(keyring_create(keyring, pw), error)
  expect_error(keyring_unlock(keyring, pw), error)

  keyring <- NULL
  expect_error(keyring_delete(keyring    ), error)
  expect_error(keyring_lock(  keyring    ), error)
  expect_error(keyring_locked(keyring    ), error)
  expect_error(keyring_create(keyring, pw), error)
  expect_error(keyring_unlock(keyring, pw), error)
})

test_that("keyring_* doesn't allow multiple passwords", {
  local_reproducible_output(width = 200)
  keyring <- 'sheltertest'
  error   <- "'password': Must have length 1"
  pw      <- c('abc123', 'abridgetoofar')

  expect_error(keyring_create(keyring, pw), error)
  expect_error(keyring_unlock(keyring, pw), error)
})

test_that("keyring_* doesn't allow multiple passwords", {
  local_reproducible_output(width = 200)
  keyring <- 'sheltertest'
  error   <- "'password': Must be of type 'string'"

  pw      <- TRUE
  expect_error(keyring_create(keyring, pw), error)
  expect_error(keyring_unlock(keyring, pw), error)

  pw      <- NULL
  expect_error(keyring_create(keyring, pw), error)
  expect_error(keyring_unlock(keyring, pw), error)
})

test_that("kerying_create doesn't allow simple passwords", {
  local_reproducible_output(width = 200)
  keyring <- 'sheltertest'
  expect_error(keyring_create(keyring, 'qwerty'),
    "Variable 'password': Must contain a special character")
  expect_error(keyring_create(keyring, 'qwerty'),
    "Variable 'password': Occurs in common password list")
  expect_error(keyring_create(keyring, 'qwerty'),
    "Variable 'password': Must be at least 8 characters")
})

test_that("keyring_create does not allow overwrite of existing keyring", {
  local_reproducible_output(width = 200)
  keyring <- 'sheltertest'
  expect_silent(keyring_create(keyring, 'abc123456'))
  expect_error(keyring_create(keyring, 'abc123456'),
    "Variable 'keyring': Specifies a keyring 'sheltertest' that already exists")
  keyring_delete(keyring)
})
