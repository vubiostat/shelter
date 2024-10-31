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

unless <- function(x, y) if(!is.null(x)) x else y

confirmation <- function(prompt, yes = 'yes')
{
  ans <- readline(paste0(prompt, ": "))
  if (ans != yes) stop("Aborted", call. = FALSE)
}

is_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x)

is_string_or_null <- function(x) is.null(x) || is_string(x)

is_string_or_raw <- function(x) is.raw(x) || is_string(x)

is_list_with_names <- function(x, names)
{
  is.list(x) &&
    length(x) == length(names) &&
    all(vapply(names, function(i) assertthat::has_name(x, i), logical(1)))
}

validate_item <- function(item)
{
  assertthat::assert_that(
    is_list_with_names(item, names = c("service_name", "user_name", "secret")),
    is_string(item[["service_name"]]),
    is_string_or_null(item[["user_name"]]),
    is_string_or_raw(item[["secret"]])
  )
  invisible(item)
}

#' @importFrom tools md5sum
file_stamp <- function(x) as.character(md5sum(x))

file_error <- function(problem, reason = NULL)
{
  info <- if (is.null(reason))
            problem else
            paste0(problem, ": ", reason)

  stop("keyring error (file-based keyring), ", info, call. = FALSE)
}
