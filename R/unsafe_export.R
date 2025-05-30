# Copyright (C) 2025 Vanderbilt University
# Shawn Garbett, Cole Beck, Hui Wu, Benjamin Nutter, Savannah Obregon
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Export keyring to plain text format as a string.
#'
#' This functions exports a keyring to a file as a convenience function
#' for production deployments.
#'
#' WARNING: It is not recommended to use this function
#' unless you are deploying to a hardened secured production environment. To
#' restate, if you are developing on a personal laptop a report or code
#' this function should NOT be used.
#'
#' @param keyring character(1); Name of keyring.
#' @param format character(1); One of 'yaml' or 'ENV'.
#' @param yaml_tag character(1); Tag to use in 'yaml'. Defaults to 'shelter'
#' @param warn boolean(1); Should the user be warned of the dangers. Defaults to TRUE.
#' @examples
#' \dontrun{
#' cat(unsafe_export('mypersonalkeyring', 'yaml'), file="myproject.yml")
#' }
#' @returns A character(1) string of the desired export.
#' @importFrom checkmate assert_string
#' @importFrom checkmate assertLogical
#' @importFrom checkmate makeAssertCollection
#' @importFrom checkmate reportAssertions
#' @importFrom yaml as.yaml
#' @export
unsafe_export <- function(keyring, format, yaml_tag='shelter', warn=TRUE)
{
  coll <- makeAssertCollection()
  assert_string(keyring,  add=coll)
  assert_string(format,   add=coll, pattern="^yaml$|^ENV$")
  assert_string(yaml_tag, add=coll)
  assertLogical(warn,    add=coll, len=1)
  reportAssertions(coll)

  if(warn) warning("`unsafe_export` provides a plain text version of secrets. Use with caution.")

  keys <- sapply(key_list(keyring), function(x) key_get(keyring, x))

  if(format=='ENV')
  {
    paste(c(paste0("export ", toupper(names(keys)), "='", keys, "'"), '\n'), collapse="\n")
  } else
  {
    result <- list()
    result[[yaml_tag]] <- list(keys=as.list(keys))
    as.yaml(result)
  }
}
