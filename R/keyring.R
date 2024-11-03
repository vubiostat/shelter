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
#' @return boolean(1); Success or failure of operation
#' @export
keyring_lock <- function(keyring)
{
  shelter_env[[keyring]] <- NULL
}

#' Delete a given keyring
#'
#' Given the name of a keyring, delete it and remove all cached information.
#'
#' @param character(1); Name of keyring
#' @return boolean(1); Success or failure of operation
#' @export
keyring_delete <- function(keyring)
{
  keyring_lock(keyring)
  unlink(keyring_file(keyring), force = TRUE) == 0
}

#' Is a keyring unlocked for key operations and reading
#'
#' Query if a keyring is unlocked
#'
#' @param character(1); Name of keyring
#' @return boolean(1); Success or failure of operation
#' @export
keyring_locked <- function(keyring)
{
  is.null(shelter_env[[keyring]])
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

# keyring_create :: KeyRing -> Password -> Bool  # creates memory and disk
# keyring_unlock :: KeyRing -> Password -> Bool  # reads and loads into memory
