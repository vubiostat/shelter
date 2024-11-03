context("Keyring functional tests")

# IMPORTANT NOTE: These tests are order dependent.

test_that(
  "keyring_delete doesn't throw an error if it doesn't exists",
  {
    expect_true(keyring_delete('sheltertest'))
  }
)


