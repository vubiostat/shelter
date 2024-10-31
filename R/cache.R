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

update_cache <- function(keyring) {
  kr_env <- keyring_env(keyring_file(keyring))
  kr <- read_file(keyring)
  nonce <- kr[["nonce"]]
  assertthat::assert_that(is.raw(nonce), length(nonce) > 0L)
  kr_env$nonce <- nonce

  check <- kr[["check"]]
  assertthat::assert_that(is.character(check), length(check) > 0L)
  kr_env$check <- check

  kr_env$items <- lapply(kr[["items"]], validate_item)
  kr_env$stamp <- kr$stamp
  kr_env
}

get_cache <- function(keyring) {
  keyring_file <- keyring_file(keyring)
  kr_env <- keyring_env(keyring_file)
  stamp <- kr_env$stamp

  if (is.null(kr_env$nonce) || is.null(stamp) || is.na(stamp) || file_stamp(keyring_file) != stamp) {
    kr_env <- update_cache(keyring)
  }

  assertthat::assert_that(is.raw(kr_env$nonce), length(kr_env$nonce) > 0L)
  assertthat::assert_that(is.character(kr_env$check), length(kr_env$check) > 0L)

  list(
    nonce = kr_env$nonce,
    items = lapply(kr_env$items, validate_item),
    check = kr_env$check)
}
