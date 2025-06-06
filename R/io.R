# Basic utilities for a keyring in R
#
# Copyright (C) 2024-2025 Shawn Garbett, Cole Beck, Vanderbilt University
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

KEYRING_EXT   <- '.keyring.RDS'
KEYRING_REGEX <- '\\.keyring\\.RDS$'

# Returns directory to store keyrings in
#' @importFrom rappdirs user_config_dir
keyring_dir <- function()
  getOption("shelter_keyring_dir", user_config_dir("r-shelter"))

# Given a keyring name, returns the encypted keyring file location
keyring_file <- function(keyring)
  file.path(keyring_dir(), paste0(keyring, KEYRING_EXT))

# This routine ensures no overlapping writes between processes
#' @importFrom filelock lock
#' @importFrom filelock unlock
atomic_op <- function(file, expr)
{
  l <- lock(paste0(file, ".lck"),
            timeout = getOption("shelter_lock_timeout", 1000))
  if (is.null(l))
    stop(sprintf("Unable to get lock for file '`%s`'.", file))
  on.exit(unlock(l), add = TRUE)
  expr
}

#' Check if a keyring exists.
#'
#' Given a keyring name will check if the keyring file exists.
#'
#' @param keyring character(1); Name of the keyring.
#' @return logical(1); Keyring file store existence status.
#' @export
keyring_exists <- function(keyring) file.exists(keyring_file(keyring))

# Internal assertion that a keyring exists and halt if it doesn't
keyring_assert_exists <- function(keyring)
  if(!keyring_exists(keyring))
    stop(sprintf("Keyring '`%s`' does not exist.", keyring))

# keyring_store    :: Keyring -> KeyringData -> IO ()
#' @importFrom sodium data_encrypt
#' @importFrom sodium hash
#' @importFrom sodium random
keyring_store <- function(keyring, data)
{
  file       <- keyring_file(keyring)
  x          <- data
  password   <- hash(charToRaw(x$password))
  x$password <- NULL
  x$version  <- as.character(getNamespaceVersion('shelter'))

  # In case of no key_pairs, maintain a random check
  x$check    <- data_encrypt(random(32), password)

  # Encrypt key pairs
  if(is.null(x$key_pairs))
  {
    x$key_pairs <- list()
  } else
  {
    for(i in seq_along(x$key_pairs))
      x$key_pairs[[i]] <- data_encrypt(charToRaw(x$key_pairs[[i]]), password)
  }

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  atomic_op(file, saveRDS(x, file))

  data
}

# keyring_retrieve :: Keyring -> Password -> IO KeyringEnv
#' @importFrom sodium data_decrypt
#' @importFrom sodium hash
keyring_retrieve <- function(keyring, password)
{
  keyring_assert_exists(keyring)

  file       <- keyring_file(keyring)

  x <- atomic_op(file, readRDS(file))
  x$password <- password

  password   <- hash(charToRaw(password))

  tryCatch(
    {
      if(is.null(x$key_pairs))
      {
        data_decrypt(x$check, password)
      } else
      {
        for(i in seq_along(x$key_pairs))
          x$key_pairs[[i]] <- rawToChar(data_decrypt(x$key_pairs[[i]], password))
      }
      x
    },
    error=function(e)
      if(grepl('Failed to decrypt',e$message)) return(NULL) else stop(e)
  )
}

