# Basic utilities for a keyring in R
#
# Copyright (C) 2024 Shawn Garbett, Cole Beck, Vanderbilt University Medical Center
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Local environment to store keyring states
shelter_env <- new.env(parent = emptyenv())

keyring_env <- function(keyring)
{
  if (is.null(shelter_env[[keyring]]))
    shelter_env[[keyring]] <- new.env(parent = emptyenv())
  shelter_env[[keyring]]
}

#' Locks a given keyring
#'
#' Given the name of a keyring lock it.
#'
#' @param character(1); Name of keyring
#' @return logical(1); Success or failure of operation
#' @export
#' @importFrom checkmate makeAssertCollection
#' @importFrom checkmate assert_string
#' @importFrom checkmate assert_disjunct
#' @importFrom checkmate reportAssertions
keyring_lock <- function(keyring)
{
  # Argument validation
  coll <- makeAssertCollection()
  assert_string(keyring, add=coll)
  reportAssertions(coll)

  shelter_env[[keyring]] <- NULL
}

#' Delete a given keyring
#'
#' Given the name of a keyring, delete it and remove all cached information.
#'
#' @param character(1); Name of keyring
#' @return logical(1); Success or failure of operation
#' @export
keyring_delete <- function(keyring)
{
  # Argument validation
  coll <- makeAssertCollection()
  assert_string(keyring, add=coll)
  reportAssertions(coll)

  keyring_lock(keyring)
  unlink(keyring_file(keyring), force = TRUE) == 0
}

#' Is a keyring unlocked for key operations and reading
#'
#' Query if a keyring is unlocked
#'
#' @param character(1); Name of keyring
#' @return logical(1); Success or failure of operation
#' @export
keyring_locked <- function(keyring)
{
  keyring_assert_exists(keyring)

  # Argument validation
  coll <- makeAssertCollection()
  assert_string(keyring, add=coll)
  reportAssertions(coll)

  is.null(shelter_env[[keyring]])
}

keyring_assert_unlocked <- function(keyring)
{
  if(keyring_locked(keyring))
    stop(sprintf("Keyring '`%s`' is not unlocked.", keyring))
}

#' Provides a `data.frame` of information on available keyrings.
#'
#' Looks in a local directory where keyrings are stored for the
#' current user and returns information about keyrings found.
#' Keyrings are stored in `rappdirs::user_config_dir("r-keyring")`
#' and end in `.keyring.RDS`
#'
#' @return data.frame of {keyring, secrets, locked}
#'
#' @examples
#' keyring_list()
#'
#' @export
keyring_list <- function()
{
  files <- dir(path       = keyring_dir(),
               pattern    = KEYRING_REGEX,
               full.names = TRUE)
  names <- sub(KEYRING_EXT, "",  basename(files))

  secrets <- vapply(files, function(f)
  {
    length(readRDS(f)$key_pairs)
  }, integer(1))

  locked <- vapply(names, keyring_locked, logical(1))

  data.frame(
    keyring          = unname(names),
    secrets          = unname(secrets),
    locked           = unname(locked),
    stringsAsFactors = FALSE
  )
}

#' Create a new empty keyring.
#'
#' Create a new empty keyring with of a given name with the specified password.
#'
#' @param character(1); Name of keyring
#' @param character(1); Password for keyring
#' @return logical(1); Success or failure of operation
#' @export
keyring_create <- function(keyring, password)
{
  # Argument validation
  coll <- makeAssertCollection()
  assert_string(keyring, add=coll)
  assert_string(password, add=coll)
  if(length(password) == 1)
  {
    if(!grepl("[\\$!@#%\\^&\\*\\(\\)\\-_\\+\\=\\{\\}\\:;<>,\\.~0-9]", password))
      coll$push("Variable 'password': Must contain a special character or number.")
    if(tolower(password) %in% common_passwords)
      coll$push("Variable 'password': Occurs in common password list which creates security risk.")
    if(nchar(password) < 8)
      coll$push("Variable 'password': Must be at least 8 characters.")
  }
  if(length(keyring) == 1 && keyring %in% keyring_list()$keyring)
    coll$push(sprintf("Variable 'keyring': Specifies a keyring '%s' that already exists.", keyring))

  reportAssertions(coll)

  x <- keyring_store(keyring, list(password=password))

  shelter_env[[keyring]] <- x

  !is.null(x)
}

#' Unlock a keyring.
#'
#' Unlock a given keyring using the specified password. Secrets exist
#' in plain text in memory while a keyring is unlocked.
#'
#' @param character(1); Name of keyring
#' @param character(1); Password for keyring
#' @return logical(1); Success or failure of operation
#' @export
keyring_unlock <- function(keyring, password)
{
  # Argument validation
  coll <- makeAssertCollection()
  assert_string(keyring,  add=coll)
  assert_string(password, add=coll)
  reportAssertions(coll)

  x <- keyring_retrieve(keyring, password)

  shelter_env[[keyring]] <- x

  !is.null(x)
}
