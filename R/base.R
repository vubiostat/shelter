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

file_keyrings <- new.env(parent = emptyenv())

#' @importFrom assertthat assert_that
#' @importFrom rappdirs user_config_dir
keyring_dir <- function() user_config_dir("r-keyring")

keyring_file <- function(keyring)
  file.path(keyring_dir(), paste0(keyring, ".keyring.RDS"))

keyring_env <- function(file_name)
{
  env_name <- normalizePath(file_name, mustWork = TRUE)
  kr_env <- file_keyrings[[env_name]]
  if (is.null(kr_env))
  {
    kr_env <- file_keyrings[[env_name]] <- new.env(parent = emptyenv())
  }
  kr_env
}

is_set_keyring_pass <- function(keyring)
  !is.null(keyring_env(keyring_file(keyring))$key)

unset_keyring_pass <- function(keyring)
{
  kr_env <- keyring_env(keyring_file(keyring))
  kr_env$key <- NULL
  invisible(kr_env)
}

#' @importFrom sodium hash
set_keyring_pass <- function(keyring, password)
{
  assertthat::assert_that(is_string(password))
  password <- hash(charToRaw(password))
  kr_env <- keyring_env(keyring_file(keyring))
  kr_env$key <- password
  NULL
}

get_keyring_pass <- function(keyring) {
  kr_env <- keyring_env(keyring_file(keyring))
  if (is.null(kr_env$key)) {
    key <- set_keyring_pass(keyring)
  } else {
    key <- kr_env$key
  }
  assertthat::assert_that(is.raw(key), length(key) > 0L)
  key
}

# FIXME: Password should NEVER get asked for from this level!!!!
#' @importFrom getPass getPass
#' @importFrom utils osVersion
get_pass <- function(msg)
{
  if(grepl('mac', tolower(osVersion))               &&
     requireNamespace("rstudioapi", quietly = TRUE) &&
     rstudioapi::isAvailable(child_ok=TRUE))
  {
    rstudioapi::askForPassword(msg)
  } else getPass(msg)
}
