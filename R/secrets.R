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

split_string <- function(string, width = 78L) {
  assertthat::assert_that(is_string(string))
  paste(
    lapply(
      seq.int(ceiling(nchar(string) / width)) - 1L,
      function(x) substr(string, x * width + 1L, x * width + width)
    ),
    collapse = "\n"
  )
}

merge_string <- function(string) {
  assertthat::assert_that(is_string(string))
  paste(strsplit(string, "\n")[[1L]], collapse = "")
}

#' @importFrom sodium data_encrypt
#' @importFrom sodium bin2hex
secret_encrypt <- function(secret, nonce, key) {
  res <- data_encrypt(charToRaw(secret), key, nonce)
  split_string(bin2hex(res))
}

#' @importFrom sodium hex2bin
#' @importFrom sodium data_decrypt
secret_decrypt <- function(secret, nonce, key) {
  res <- hex2bin(merge_string(secret))
  raw <- data_decrypt(res, key, nonce)
  rawToChar(raw)
}
