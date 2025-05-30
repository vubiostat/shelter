# Copyright (C) 2021-2025 Vanderbilt University Medical Center,
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

  #############################################################################
 ## invisibly convert existing keyring lockers to new shelter
##
.build_a_bridge <- function(keyring, passwordFUN, msg)
{
  # If a shelter ring exists no bridge is needed
  new_ring <- keyring_list()
  new_ring <- new_ring[new_ring$keyring==keyring,]
  if(nrow(new_ring) > 0) return(NULL)

  # If the keyring package is not installed, nothing to build bridge with
  if(!requireNamespace('keyring', quietly = TRUE)) return(NULL)

  # If a keyring::keyring doesn't exist then no bridge can be built
  old_ring <- keyring::keyring_list()
  old_ring <- old_ring[old_ring$keyring==keyring,]
  if(nrow(old_ring) == 0) return(NULL)

  password <- .getPWGlobalEnv()

  # Unlock old if locked
  locked <- old_ring$locked
  while(locked)
  {
    password <- .getPWGlobalEnv()
    stored   <- !is.null(password) && password != ''
    if(!stored) password <- passwordFUN(msg)
    if(is.null(password) || password == '') stop(paste0("User aborted keyring '",keyring, "' unlock."))

    tryCatch(
      {
        keyring::keyring_unlock(keyring, password)
        .savePWGlobalEnv(password)
        locked <- FALSE
      },
      error = function(e)
      {
        if(stored) .clearPWGlobalEnv()

        msg <<-  paste0("Provided password failed. Please enter password to unlock API keyring '",keyring, "'.")
      }
    )
  }

  # Gather old keys
  old_names <- keyring::key_list("redcapAPI", keyring)$username
  old_keys  <- lapply(old_names, function(x) keyring::key_get("redcapAPI", x, keyring))
  names(old_keys) <- old_names

  # Store in new shelter
  # NOTE: This evades normal functional calls to shelter because
  #       password may not meet current standards. May break
  #       if internal key storage method in package is changed.
  shelter_env[[keyring]] <- keyring_store(keyring,
                                          list(password=password,
                                               key_pairs=old_keys))
}

.savePWGlobalEnv <- function(password)
{
  Sys.setenv(SHELTER_PW=password)

  # Hacked work around for RStudio starting new session for everything
  if(requireNamespace("rstudioapi", quietly = TRUE) &&
     rstudioapi::isAvailable(child_ok=TRUE))
    rstudioapi::sendToConsole(sprintf("Sys.setenv(SHELTER_PW='%s')", password), execute = TRUE, echo=FALSE, focus=FALSE)
}

.clearPWGlobalEnv <- function()
{
  Sys.unsetenv("SHELTER_PW")
  # Hacked work around for RStudio starting new session for everything
  if(requireNamespace("rstudioapi", quietly = TRUE) &&
     rstudioapi::isAvailable(child_ok=TRUE))
    rstudioapi::sendToConsole('Sys.unsetenv("SHELTER_PW")', execute = TRUE, echo=FALSE, focus=FALSE)
}

.getPWGlobalEnv <- function()
{
  Sys.getenv("SHELTER_PW")
}

  #############################################################################
 ## unlock via YAML override if it exists
##
#' @importFrom yaml read_yaml
#' @importFrom utils modifyList
.unlockYamlOverride <- function(connections, connectionFUNs, yaml_tag, ...)
{
  config_file <- file.path("..", paste0(basename(getwd()),".yml"))

  if(!file.exists(config_file)) return(list())

  config <- read_yaml(config_file)
  if(is.null(config[[yaml_tag]])) stop(paste0("Config file '",config_file,"' does not contain required '",yaml_tag,"' entry"))
  config <- config[[yaml_tag]]
  if(is.null(config$keys))      stop(paste0("Config file '",config_file,"' does not contain required 'keys' entry under the '", yaml_tag, "' entry"))
  keys   <- config$keys

  args <- list(...)

  # Allow for config override for network
  if(!is.null(config[['config']]))
    args <- modifyList(args, config[['config']])

  dest <- lapply(seq_along(connections), function(i)
  {
    conn <- connections[i]
    key  <- keys[[conn]]

    if(is.null(key) || length(key)==0)
      stop(paste0("Config file '", config_file, "' does not have API_KEY for '", conn,"' under '", yaml_tag, ": keys:' specified."))
    if(!is.character(key))
      stop(paste0("Config file '", config_file, "' invalid entry for '", conn,"' under '", yaml_tag, ": keys:'."))
    if(length(key) > 1)
      stop(paste0("Config file '", config_file, "' has too may key entries for '", conn,"' under '",yaml_tag,": keys:' specified."))
    do.call(connectionFUNs[[i]], modifyList(list(key=key), args))
  })
  names(dest) <- if(is.null(names(connections))) connections else names(connections)

  return(dest)
}
  #############################################################################
 ## unlock via ENV override if it exists
##
#' @importFrom utils modifyList
.unlockENVOverride <- function(connections, connectionFUNs, ...)
{
  api_key_ENV <- sapply(connections, function(x) Sys.getenv(toupper(x)))

  if(all(api_key_ENV == "")) return(list())

  if(any(api_key_ENV == ""))
    stop(paste("Some matching ENV variables found but missing:",paste0(toupper(connections[api_key_ENV=='']), collapse=", ")))

  args <- list(...)
  dest <- lapply(seq_along(connections), function(i)
  {
    do.call(connectionFUNs[[i]], modifyList(list(key = api_key_ENV[i]), args))
  })
  names(dest) <- if(is.null(names(api_key_ENV))) api_key_ENV else names(api_key_ENV)

  return(dest)
}

  #############################################################################
 ## unlock keyring
##
.unlockKeyring <- function(keyring, passwordFUN)
{
  msg   <- paste0("Please enter password to unlock API keyring '",keyring, "'.")

  .build_a_bridge(keyring, passwordFUN, msg) # Convert old keyring to new

  state <- keyring_list()
  state <- state[state$keyring==keyring,]

  # If so, does it exist?
  if(nrow(state) == 1) # Exists => UNLOCK
  {
    locked <- state$locked
    # Is it locked
    while(locked)
    {
      password <- .getPWGlobalEnv()
      stored   <- !is.null(password) && password != ''
      if(!stored) password <- passwordFUN(msg)
      if(is.null(password) || password == '') stop(paste0("User aborted keyring '",keyring, "' unlock."))

      if(keyring_unlock(keyring, password))
      {
        .savePWGlobalEnv(password)
        locked <- FALSE
      } else
      {
        if(stored) .clearPWGlobalEnv()

        msg <<-  paste0("Provided password failed. Please enter password to unlock API keyring '",keyring, "'.")
      }
    }
  } else # Keyring does not exist => Create
  {
    password <- NULL
    msg      <- paste0("Creating keyring. Enter NEW password for the keyring '",
                                     keyring, "'.")
    while(is.null(password))
    {
      password <- passwordFUN(msg)
      if(is.null(password) || password == '') stop(paste0("User cancelled creation of keyring '", keyring, "'."))

      problems <- assert_password_requirements(password, makeAssertCollection())
      if(!problems$isEmpty())
      {
        msg <- paste(c("Please enter a better password.", problems$getMessages()), collapse="\n")
        password <- NULL
      }
    }

    if(keyring_create(keyring, password)) .savePWGlobalEnv(password)
  }
  NULL
}

  #############################################################################
 ## Find the best password function
## If rstudioapi is loaded and rstudio is running, then use that.
## getOption('askpass') returns a function that does not work on MAC
## when knitting from RStudio, ugh.
#'
#' @importFrom getPass getPass
.default_pass <- function()
{
  if(isTRUE(grepl('mac', tolower(utils::osVersion))) &&
     requireNamespace("rstudioapi", quietly = TRUE)  &&
     rstudioapi::isAvailable(child_ok=TRUE))
  {
    rstudioapi::askForPassword
  } else getPass
}

# Main internal algorithm
.unlockAlgorithm <- function(
    connections,
    connectionFUNs,
    keyring,
    envir,
    passwordFUN,
    yaml_tag='shelter',
    ...)
{
  if(is.numeric(envir)) envir <- as.environment(envir)

  # Use YAML config if it exists
  dest <- .unlockYamlOverride(connections, connectionFUNs, yaml_tag, ...)
  if(length(dest) > 0)
    return(if(is.null(envir)) dest else list2env(dest, envir=envir))

  # Use ENV if it exists and YAML does not exist
  dest <- .unlockENVOverride(connections, connectionFUNs, ...)
  if(length(dest) > 0)
    return(if(is.null(envir)) dest else list2env(dest, envir=envir))

  # Proceed to unlock the local keyring
  .unlockKeyring(keyring, passwordFUN)

  # Open Connections
  dest <- lapply(seq_along(connections), function(i)
  {
    stored <- connections[i] %in% key_list(keyring)

    api_key <- if(stored)
    {
      key_get(keyring, connections[i])
    } else
    {
      passwordFUN(paste0("Please enter API key for '", connections[i], "'."))
    }

    if(is.null(api_key) || api_key == '') stop(paste("No API key entered for", connections[i]))

    conn <- NULL
    while(is.null(conn))
    {
      conn <- (connectionFUNs[[i]])(api_key, ...)
      if(is.null(conn))
      {
        key_delete(keyring, unname(connections[i]))
        api_key <- passwordFUN(paste0(
          "Invalid API key for '", connections[i],
          "' in keyring '", keyring,
          "'. Possible causes include: mistyped, renewed, or revoked.",
          " Please enter a new key or cancel to abort."))
        if(is.null(api_key) || api_key == '') stop("unlockAPIKEY aborted")
      } else if(!stored)
      {
        key_set(
          keyring,
          unname(connections[i]),
          api_key)
      }
    }
    conn
  })
  names(dest) <- if(is.null(names(connections))) connections else names(connections)

  if(is.null(envir)) dest else list2env(dest, envir=envir)
}

#' Open an API key and use it build a connection.
#'
#' Opens a set of connections  from API keys stored in an encrypted keyring.
#' If the keyring does not exist, it will ask for password to this keyring to use on
#' later requests. Next it
#' will ask for the API keyss specified in `connections`. If an API key does not
#' work, it will request again. On later executions it will use an open keyring
#' to retrieve all API_KEYs or for a password if the keyring is currently
#' locked.
#'
#' If one forgets the password to this keyring, or wishes to start over:
#' `keyring_delete("<NAME_OF_KEY_RING_HERE>")`
#'
#' For production servers where the password must be stored in a readable
#' plain text file, it will search for `../<basename>.yml`. DO NOT USE
#' this unless one is a sysadmin on a production hardened system, as this defeats the security and purpose of
#' a local encrypted file. The expected structure of this yaml file is
#' as follows:
#'
#' \preformatted{
#' other-config-stuff1: blah blah
#' shelter:
#'   keys:
#'     intake: THIS_IS_THE_INTAKE_DATABASE_APIKEY
#'     details: THIS_IS_THE_DETAILS_DATABASE_APIKEY
#' other-config-stuff2: blah blah
#' other-config-stuff3: blah blah
#' }
#'
#' For production servers the use of ENV variables is also supported. The connection
#' string is converted to upper case for the search of ENV. If a YAML file
#' and ENV definitions both exist, the YAML will take precedence.
#'
#' IMPORTANT: Make sure that R is set to NEVER save workspace to .RData
#' as this *is* writing the API_KEY to a local file in clear text because
#' connection objects contain the unlocked key in memory. One can use the
#' following in .Rprofile, `usethis::edit_r_profile()`:
#' \preformatted{
#' newfun <- function (save = "no", status = 0, runLast = TRUE)
#'   .Internal(quit(save, status, runLast))
#' pkg <- 'base'
#' oldfun <- 'q'
#' pkgenv <- as.environment(paste0("package:", pkg))
#' unlockBinding(oldfun, pkgenv)
#' utils::assignInNamespace(oldfun, newfun, ns = pkg, envir = pkgenv)
#' assign(oldfun, newfun, pkgenv)
#' lockBinding(oldfun, pkgenv)
#' }
#'
#' @param connections character vector. A list of strings that define the
#'          connections with associated API_KEYs to load into environment. Each
#'          name should correspond to a REDCap project for traceability, but
#'          it can be named anything one desires.
#'          The name in the returned list is this name.
#' @param envir environment. The target environment for the connections. Defaults to NULL
#'          which returns the keys as a list. Use [globalenv()] to assign in the
#'          global environment. Will accept a number such a '1' for global as well.
#' @param keyring character(1). Name of keyring.
#' @param passwordFUN function. Function to get the password for the keyring. Usually defaults `getPass::getPass`.
#'          On MacOS it will use rstudioapi::askForPassword if available.
#' @param connectFUN function or list(function). A function that takes a key and returns a connection.
#'          the function should call `stop` if the key is invalid in some manner. The
#'          first argument of the function is the API key. The validation of the
#'          key via a connection test is important for the full user interaction
#'          algorithm to work properly. If one wished to just retrieve an API key
#'          and not test the connection this would work `function(x, ...) x`, but
#'          be aware that if the key is invalid it will not query the user as
#'          the validity is not tested.
#' @param yaml_tag character(1). Only used as an identifier in yaml override files.
#'          Defaults to package name `shelter`.
#' @param \dots Additional arguments passed to `connectFUN()`.
#' @return If `envir` is NULL returns a list of opened connections. Otherwise
#'         connections are assigned into the specified `envir`.
#'
#' @examples
#' \dontrun{
#' unlockKeys(c(test_conn    = 'Testshelter',
#'              sandbox_conn = 'SandboxAPI'),
#'              keyring      = '<NAME_OF_KEY_RING_HERE>',
#'              envir        = globalenv(),
#'              passwordFUN  = function(x, ...) x)
#' }
#' @importFrom checkmate makeAssertCollection
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_list
#' @importFrom checkmate assert_function
#' @importFrom checkmate reportAssertions
#' @export
unlockKeys <- function(connections,
                       keyring,
                       connectFUN  = NULL,
                       envir       = NULL,
                       passwordFUN = .default_pass(),
                       yaml_tag    = 'shelter',
                       ...)
{
   ###########################################################################
  # Check parameters passed to function
  coll <- makeAssertCollection()

  if(is.numeric(envir)) envir <- as.environment(envir)

  assert_character(x = keyring,      null.ok = FALSE, add = coll)
  assert_character(x = connections,  null.ok = FALSE, add = coll)
  assert_function( x = passwordFUN,  null.ok = FALSE, add = coll)
  assert_class(    x = envir,        null.ok = TRUE,  add = coll, classes="environment")
  if(inherits(connectFUN, "list"))
  {
    assert_list(x=connectFUN, any.missing = FALSE, len=length(connections), add=coll, types="function")
  } else {
    assert_function(x = connectFUN, null.ok = FALSE,  add = coll, nargs=1)
  }
  reportAssertions(coll)

   ###########################################################################
  ## Create callback list of connection functions if necessary
  if(inherits(connectFUN, "function"))
  {
    n <- length(connections)
    connectionFUNs <- vector('list', n)
    for(i in seq(n)) connectionFUNs[[i]] <- function(key, ...) connectFUN(key, ...)
  }

   ###########################################################################
  ## Do it
  .unlockAlgorithm(connections,
                   connectionFUNs,
                   keyring,
                   envir,
                   passwordFUN,
                   yaml_tag=yaml_tag,
                   ...)
}

