context(".unlockKeyring")

library(mockery)

test_that(
  ".unlockKeyring pulls password from env and writes back",
  {
    stub(.unlockKeyring, "keyring_list",
                  data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                             num_secrets=0:2,
                             locked=rep(TRUE, 3)))
    stub(.unlockKeyring, ".getPWGlobalEnv", "xyz")
    stub(.unlockKeyring, "keyring_unlock", TRUE)

    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1}

    .unlockKeyring("API_KEYs", passwordFUN, 3)

    expect_true(calls == 0) # No requests for password from user
    expect_true(Sys.getenv("SHELTER_PW") == "xyz")
  }
)

test_that(
  ".unlockKeyring asks user for password when not in env, unlocks and writes to env",
  {
    stub(.unlockKeyring, "keyring_list",
                  data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                             num_secrets=0:2,
                             locked=rep(TRUE, 3)))
    stub(.unlockKeyring, ".getPWGlobalEnv", "")
    stub(.unlockKeyring, "keyring_unlock", TRUE)

    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; "xyz"}

    .unlockKeyring("API_KEYs", passwordFUN, 3)

    expect_true(calls == 1) # Requests password
    expect_true(Sys.getenv('SHELTER_PW') == "xyz")
  }
)

test_that(
  ".unlockKeyring asks user for password and aborts when they cancel",
  {
    stub(.unlockKeyring, "keyring_list",
                  data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                             num_secrets=0:2,
                             locked=rep(TRUE, 3)))
    stub(.unlockKeyring, ".getPWGlobalEnv", "")
    stub(.unlockKeyring, "keyring_unlock", TRUE)

    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; ""}

    expect_error(.unlockKeyring("API_KEYs", passwordFUN, 3), "User aborted keyring")

    expect_true(calls == 1) # Requests password
  }
)


test_that(
  ".unlockKeyring asks user for password when one in env fails, unlocks and writes to env",
  {
    stub(.unlockKeyring, "keyring_list",
                  data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                             num_secrets=0:2,
                             locked=rep(TRUE, 3)))
    stub(.unlockKeyring, "Sys.getenv",
                  mock("fail", ""))
    stub(.unlockKeyring, "keyring_unlock", mock(FALSE, TRUE))

    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; "xyz"}

    .unlockKeyring("API_KEYs", passwordFUN, 3)

    expect_true(calls == 1) # Requests password
    expect_true(Sys.getenv("SHELTER_PW") == "xyz")
  }
)

test_that(
  ".unlockKeyring creates keyring if it doesn't exist",
  {
    Sys.unsetenv("SHELTER_PW")
    ukr <- mock(data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                           secrets=0:2,
                           locked=rep(TRUE, 3)))
    m <- mock(TRUE)

    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; "xyz%$xyz"}
    stub(.unlockKeyring, "keyring_create", m)
    stub(.unlockKeyring, "keyring_list", ukr)

    .unlockKeyring("MakeMe", passwordFUN, 3)

    expect_call(m, 1, keyring_create(keyring,password))
    expect_equal(mock_args(m)[[1]], list("MakeMe", "xyz%$xyz"))
    expect_true(calls == 1) # Asks user for password
    expect_true(Sys.getenv("SHELTER_PW") == "xyz%$xyz") # Stores result
    Sys.unsetenv("SHELTER_PW")
  }
)

test_that(
  ".unlockKeyring creates keyring respects user cancel",
  {
    Sys.unsetenv("SHELTER_PW")
    stub(.unlockKeyring, "keyring_list",
         data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                    secrets=0:2,
                    locked=rep(TRUE, 3)))
    m <- mock(TRUE)

    calls <- 0
    passwordFUN <- function(...) {calls <<- calls + 1; ""}

    with_mocked_bindings(
      {
        expect_error(.unlockKeyring("MakeMe", passwordFUN), "User cancelled")
        expect_called(m, 0)
      },
      keyring_create = m
    )

    expect_true(calls == 1) # Asks user for password
    expect_true(Sys.getenv("SHELTER_PW") == "") # Nothing Stored
  }
)

test_that(
  ".unlockKeyring asks user for password and aborts when they exceed maximum attempts",
  {
    stub(.unlockKeyring, "keyring_list",
         data.frame(keyring=c("Elsewhere", "API_KEYs", "JoesGarage"),
                    num_secrets=0:2,
                    locked=rep(TRUE, 3)))
    stub(.unlockKeyring, ".getPWGlobalEnv", "")
    stub(.unlockKeyring, "keyring_unlock", FALSE)

    calls <- 0
    passwordFUN <- function(...) { calls <<- calls + 1; "WRONG" }

    expect_error(.unlockKeyring("API_KEYs", passwordFUN, 3), "Maximum password entry attempts exceeded")

    expect_true(calls == 3) # Requests password
  }
)
