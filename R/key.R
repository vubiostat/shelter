# Basic operations for a key in a keyring
#
# Copyright (C) 2024-2025 Shawn Garbett, Cole Beck, Vanderbilt University Medical Center
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


#' Does a given key exist in a keyring
#'
#' In an unlocked keyring return if a key exists.
#'
#' @param keyring character(1); Name of keyring
#' @param key character(1); Name of key
#' @return logical(1); Existance of key in keyring
#' @examples
#' \dontrun{
#' key_exists('mypersonalkeyring', 'key1')
#' }
#' @export
key_exists <- function(keyring, key)
{
  coll <- makeAssertCollection()
  assert_string(keyring, add=coll)
  assert_string(key,     add=coll)
  reportAssertions(coll)

  keyring_assert_unlocked(keyring)

  key %in% names(shelter_env[[keyring]]$key_pairs)
}

#' Get a secret from a keyring.
#'
#' Get a secret from an unlocked keyring given it's key.
#'
#' @param keyring character(1); Name of keyring
#' @param key character(1); Name of key
#' @return character(1); The requested secret
#'
#' @examples
#' \dontrun{
#' key_get('mypersonalkeyring', 'key1')
#' }
#'
#' @export
key_get <- function(keyring, key)
{
  coll <- makeAssertCollection()
  assert_string(keyring, add=coll)
  assert_string(key,     add=coll)
  reportAssertions(coll)

  keyring_assert_unlocked(keyring)

  shelter_env[[keyring]]$key_pairs[[key]]
}

#' Delete a key from a keyring
#'
#' Delete a key from an unlocked keyring.
#'
#' @param keyring character(1); Name of keyring
#' @param key character(1); Name of key
#' @return logical(1); Success of operation
#'
#' @examples
#' \dontrun{
#' key_delete('mypersonalkeyring', 'key1')
#' }
#'
#' @export
key_delete <- function(keyring, key)
{
  coll <- makeAssertCollection()
  assert_string(keyring, add=coll)
  assert_string(key,     add=coll)
  reportAssertions(coll)

  keyring_assert_unlocked(keyring)

  shelter_env[[keyring]]$key_pairs[[key]] <- NULL

  x <- shelter_env[[keyring]]
  shelter_env[[keyring]] <- NULL
  x <- keyring_store(keyring, x)
  shelter_env[[keyring]] <- x
  !is.null(x)
}

#' Returns vector of keys in a keyring.
#'
#' Return vector key names in a keyring that is unlocked.
#'
#' @param keyring character(1); Name of keyring
#' @return character; Key names
#' @export
#' @examples
#' \dontrun{
#' key_list('mypersonalkeyring')
#' }
key_list <- function(keyring)
{
  coll <- makeAssertCollection()
  assert_string(keyring, add=coll)
  reportAssertions(coll)

  keyring_assert_unlocked(keyring)

  names(shelter_env[[keyring]]$key_pairs)
}

#' Set a key secret in a keyring
#'
#' Sets a key secret in a keyring
#'
#' @param keyring character(1); Name of keyring
#' @param key character(1); Name of key to store in keyring
#' @param secret character(1); The secret to store in keyring
#' @return logical(1); Status of operation
#' @export
#' @examples
#' \dontrun{
#' key_set('mypersonalkeyring','key1','a secret')
#' }
key_set <- function(keyring, key, secret)
{
  coll <- makeAssertCollection()
  assert_string(keyring, add=coll)
  assert_string(key,     add=coll)
  assert_string(secret,  add=coll)
  reportAssertions(coll)

  keyring_assert_unlocked(keyring)

  shelter_env[[keyring]]$key_pairs[[key]] <- secret

  x <- shelter_env[[keyring]]
  shelter_env[[keyring]] <- NULL
  x <- keyring_store(keyring, x)
  shelter_env[[keyring]] <- x
  !is.null(x)
}
