# MIT License
# Copyright (c) 2003 Alec Wong, Gábor Csárdi, Posit Software, PBC
# Copyright (c) 2024 Shawn Garbett, Cole Beck, Vanderbilt University Medical Center
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the “Software”), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

is_locked <- function(keyring)
{
  file_name <- keyring_file(keyring)

  if (!file.exists(file_name))
    stop("Keyring `", keyring, "` does not exist")

  if (!file.exists(file_name) || is_set_keyring_pass(keyring))
  {
    TRUE
  } else
  {
    tryCatch(
      {
        cached <- get_cache(keyring)
        secret_decrypt(
          cached$check,
          cached$nonce,
          get_keyring_pass(keyring)
        )
        FALSE
      },
      error = function(e)
      {
        if(conditionMessage(e) == "Failed to decrypt")
          TRUE
        else
          stop(e)
      }
    )
  }
}

keyring_unlock <- function(keyring, password)
{
  file <- keyring_file(keyring)

  if (!file.exists(file))
    stop("Keyring `", keyring, "` does not exist")

  set_keyring_pass(password, keyring)

  if (is_locked(keyring)) {
    unset_keyring_pass(keyring)
    b_file_error(
      "cannot unlock keyring",
      "The supplied password does not work."
    )
  }

  NULL
}


key_get <- function(keyring, service, username = NULL) {
#  autocreate(keyring)
#  if(is_locked(keyring)) keyring_unlock(keyring)

  cached <- get_cache(keyring)
  all_items <- cached$items
  services <- vapply(all_items, `[[`, character(1L), "service_name")
  item_matches <- services %in% service
  users <- vapply(all_items, function(x) unless(x$user_name, NA_character_), character(1))

  if (!is.null(username)) {
    item_matches <- item_matches & users %in% username
  }

  if (sum(item_matches) < 1L) {
    file_error("cannot get secret", "The specified item could not be found in the keychain.")
  }

  key <- get_keyring_pass(keyring)
  secrets <- lapply(all_items[item_matches], `[[`, "secret")
  keys <- vapply(secrets, secret_decrypt, character(1L), cached$nonce, key)
  names(keys) <- users
  keys
}

key_set_with_value <- function(keyring, service, username = NULL, password) {
#  autocreate(keyring)
  if(is_locked(keyring)) keyring_unlock(keyring, password)

  file_name <- keyring_file(keyring)
  kr_env <- keyring_env(file_name)

  with_lock(file_name, {
    cached <- get_cache(keyring)
    all_items <- cached$items
    services <- vapply(all_items, `[[`, character(1L), "service_name")
    users <- vapply(all_items, function(x) unless(x$user_name, NA_character_), character(1))
    existing <- if (!is.null(username)) {
      services %in% service & users %in% username
    } else {
      services %in% service & is.na(users)
    }
    if (length(existing)) all_items <- all_items[!existing]

    new_item <- list(
      service_name = service,
      user_name = username,
      secret = secret_encrypt(password, cached$nonce, get_keyring_pass(keyring))
    )

    items <- c(all_items, list(new_item))
    write_file(keyring, items = items)
    kr_env$stamp <- file_stamp(file_name)
  })

  kr_env <- keyring_env(file_name)
  kr_env$items <- items
  TRUE # success
}

key_delete <- function(keyring, service, username) {
  #if(is_locked(keyring)) keyring_unlock(keyring)
  file_name <- keyring_file(keyring)
  kr_env <- keyring_env(file_name)

  with_lock(file_name, {
    cached <- get_cache(keyring)
    all_items <- cached$items
    services <- vapply(all_items, `[[`, character(1L), "service_name")
    users <- vapply(all_items, function(x) unless(x$user_name, NA_character_), character(1))
    existing <- if (!is.null(username)) {
      services %in% service & users %in% username
    } else {
      services %in% service & is.na(users)
    }
    if (length(existing) == 0) return(TRUE)

    items <- all_items[!existing]
    write_file(keyring, items = items)
    kr_env$stamp <- file_stamp(file_name)
  })

  kr_env$items <- items
  TRUE # success
}
