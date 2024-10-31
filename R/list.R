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

keyring_list <- function() {
  kr_dir <- keyring_dir()
#   file_ext <- '\\.keyring'
  file_ext <- '\\.keyring\\.RDS'
  files <- dir(kr_dir, pattern = paste0(file_ext, '$'), full.names = TRUE)
  names <- sub(file_ext, "",  basename(files))
  num_secrets <- vapply(files, function(f) {
#     length(yaml::yaml.load_file(f)$items)
    length(readRDS(f)$items)
  }, integer(1))
  locked <- vapply(names, is_locked, logical(1))
  data.frame(
    keyring = unname(names),
    num_secrets = unname(num_secrets),
    locked = unname(locked),
    stringsAsFactors = FALSE
  )
}

key_list <- function(keyring, service = NULL) {
  #autocreate(keyring)
  cached <- get_cache(keyring)
  all_items <- cached$items

  res <- data.frame(
    service = vapply(all_items, `[[`, character(1L), "service_name"),
    username = vapply(all_items, function(x) unless(x$user_name, NA_character_), character(1)),
    stringsAsFactors = FALSE
  )

  if (!is.null(service)) {
    res[res[["service"]] == service, ]
  } else {
    res
  }
}
