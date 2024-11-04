context("Keyring functional tests")

# IMPORTANT NOTE: These tests are order dependent functional tests

keyring  <- 'sheltertest'
password <- 'qwer$ty7'

test_that(
  "keyring_create will create necessary directories",{
    td <- tempdir()
    options(shelter_keyring_dir=file.path(td, "more"))
    expect_true(keyring_create(keyring,password))
    options(shelter_keyring_dir=NULL)
    unlink(td)
})

test_that(
  "keyring_delete doesn't throw an error if it doesn't exists",
  expect_true(keyring_delete(keyring))
)

test_that(
  "keyring_list returns no entry for nonexistant sheltertest",{
  expect_silent(x <- keyring_list())
  x <- x[x$keyring == keyring,]
  expect_true(nrow(x) == 0)
  expect_equal(names(x),c('keyring','secrets','locked'))
})

test_that("keyring_exists returns FALSE for nonexistant keyring",
  expect_false(keyring_exists(keyring))
)

test_that("keyring_locked stops for nonexistant keyring",
  expect_error(keyring_locked(keyring),"Keyring '`sheltertest`' does not exist")
)

test_that("keyring_create works",
  expect_true(keyring_create(keyring,password))
)

test_that("keyring_exists post creation",
  expect_true(keyring_exists(keyring))
)

test_that("keyring_list shows created keyring",{
  expect_silent(x <- keyring_list())
  x <- x[x$keyring == keyring,]
  rownames(x) <- NULL
  expect_true(nrow(x) == 1)
  expect_equal(x, data.frame(keyring=keyring, secrets=0, locked=FALSE))
})

test_that("keyring_locked is FALSE post creation",{
  expect_false(keyring_locked(keyring))
})

test_that("key_exists is FALSE for a non-existant key",{
  expect_false(key_exists(keyring, 'key1'))
})

test_that("key_delete returns TRUE for a non-existant key",{
  expect_true(key_delete(keyring, 'key1'))
})

test_that("key_list returns an empty vector",{
  expect_equal(length(key_list(keyring)), 0)
})

test_that("key_get returns a NULL for a non-existant key",{
  expect_null(key_get(keyring, 'key1'))
})

test_that("key_set returns true for setting a secret",
  expect_true(key_set(keyring, 'key1', 'thisisasecret'))
)

test_that("key_list shows newly created key",
  expect_contains(key_list(keyring), 'key1')
)

test_that("key_set can create another key",
  expect_true(key_set(keyring, 'key2', 'thisisanothersecret'))
)

test_that("key_get can retrieve keys set",{
  expect_equal(key_get(keyring, 'key1'), 'thisisasecret')
  expect_equal(key_get(keyring, 'key2'), 'thisisanothersecret')
})

test_that("key_get returns NULL for nonexistant key",{
  expect_null(key_get(keyring, 'scoobydoo'))
})

test_that("keyring_lock is succesful",
  expect_true(keyring_lock(keyring))
)

test_that("keyring_locked is TRUE post locking",
  expect_true(keyring_locked(keyring))
)

test_that("keyring_exists post locking",
  expect_true(keyring_exists(keyring))
)

test_that("key_* function ERROR when called on closed keyring",{
  error <- "Keyring '`sheltertest`' is not unlocked."
  expect_error(key_exists(keyring, 'key1'),      error)
  expect_error(key_get(   keyring, 'key1'),      error)
  expect_error(key_delete(keyring, 'key1'),      error)
  expect_error(key_list(  keyring)        ,      error)
  expect_error(key_set(   keyring, 'key1', 'x'), error)
})

test_that("keyring_unlock with invalid password returns FALSE",
  expect_false(keyring_unlock(keyring, 'baadfadf'))
)

test_that("keyring_locked post locking",
  expect_true(keyring_locked(keyring))
)

test_that("keyring_unlock with correct password returns TRUE",
  expect_true(keyring_unlock(keyring, password))
)

test_that("keyring_locked FALSE post unlock",
  expect_false(keyring_locked(keyring))
)

test_that("key_delete can delete a key",
  expect_true(key_delete(keyring, 'key1'))
)

test_that("key_list has the key remaining",
  expect_contains(key_list(keyring), "key2")
)

test_that(
  "keyring_delete can delete an existing keyring",
  expect_true(keyring_delete(keyring))
)

test_that(
  "keyring_exists is FALSE post delete",
  expect_false(keyring_exists(keyring))
)


