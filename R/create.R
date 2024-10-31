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

create <- function(keyring, password) {
  file_name <- keyring_file(keyring)
  if (file.exists(file_name)) {
    confirmation(sprintf("are you sure you want to overwrite %s (type `yes` if so)", file_name))
  }
  if (is.null(password)) stop("Aborted creating keyring")

  dir.create(keyring_dir(), recursive = TRUE, showWarnings = FALSE)
  cat("", file = file_name)
  # "key" is hashed password
  key <- set_keyring_pass(keyring, password)
  nonce <- sodium::random(24L)
  write_file(keyring, key, nonce, items = list())
  TRUE # success
}


delete <- function(keyring) {
  file_name <- keyring_file(keyring)
  unlink(file_name, force = TRUE)
  TRUE # success
}
