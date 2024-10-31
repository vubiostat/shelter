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


#' @importFrom filelock lock
#' @importFrom filelock unlock
with_lock <- function(file, expr) {
  timeout <- getOption("keyring_file_lock_timeout", 1000)
  lockfile <- paste0(file, ".lck")
  l <- lock(lockfile, timeout = timeout)
  if (is.null(l)) stop("Cannot lock keyring file")
  on.exit(unlock(l), add = TRUE)
  expr
}

#' @importFrom sodium hex2bin
read_file <- function(keyring) {
  file_name <- keyring_file(keyring)
  with_lock(file_name, {
    stamp <- file_stamp(keyring)
    yml <- readRDS(file_name)
#     yml <- yaml::yaml.load_file(file_name)
  })

  assertthat::assert_that(
    is_list_with_names(yml, names = c("keyring_info", "items")),
    is_list_with_names(
      yml[["keyring_info"]],
      names = c("keyring_version", "nonce", "integrity_check")
    )
  )

  list(
    nonce = hex2bin(yml[["keyring_info"]][["nonce"]]),
    items = lapply(yml[["items"]], validate_item),
    check = yml[["keyring_info"]][["integrity_check"]],
    stamp = stamp
  )
}

#' @importFrom sodium bin2hex data_decrypt
write_file <- function(keyring, key = NULL, nonce = NULL, items = NULL) {
  file_name <- keyring_file(keyring)
  if(is.null(nonce) || is.null(items)) {
    cached <- get_cache(keyring)
    nonce <- unless(nonce, cached$nonce)
    items <- unless(items, cached$items)
  }
  key <- unless(key, get_keyring_pass(keyring))
  rand_letters <- paste(sample(letters, 22L, replace = TRUE), collapse = "")
  kr_info <- list(
    keyring_version = as.character(getNamespaceVersion('shelter')),
    nonce = bin2hex(nonce),
    integrity_check = secret_encrypt(rand_letters, nonce, key)
  )
#   with_lock(file_name, yaml::write_yaml(list(keyring_info = kr_info), file_name))
  with_lock(file_name, saveRDS(list(keyring_info = kr_info, items = items), file = file_name))
}
