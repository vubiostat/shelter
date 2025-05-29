![](https://cranlogs.r-pkg.org/badges/grand-total/shelter)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
![r-cmd-check.yml](https://github.com/vubiostat/shelter/actions/workflows/r-cmd-check.yml/badge.svg?branch=main)

shelter
=========

`shelter` is an [R](https://www.r-project.org) package to simplify secure
management of API keys or other secrets. The general goal is that the secure
method is also the easiest method and it should require no code changes 
across environments and prevent public or hacked leakage.

The goals of this package are to do the following:

* Store API keys in a backend file using best cryptography practices.
* Interact with a developer to create a local keyring of secrets and once created minimize interaction required. 
* Pull from ENV or a local YAML file in a production environment as an override.
* Work consistently across Windows, Mac OS and Linux. 
* Support testing keys via inversion of control using a specified function.

### Differences from `keyring`

Why not use the package `keyring`? What sets this package apart from `keyring`?
These are relevant questions and the answer depends upon your needs.

The `keyring` package assumes the user is managing their keys and it's goal was
to work with the different system based keyrings, e.g. Mac vs PC, in a
consistent manner. This package originally used `keyring` but it wasn't a 
perfect fit for the needs and goals we had. A user still had to write and manage
code to deal with keyrings--and that code had poor portability to production
environments.

This package works on the command line, inside RStudio and in automated
production environments. It does everything it can to hide the details of 
key storage from the user. User flow and state is complex and tricky, but if
handled properly makes dealing with secrets mostly hidden. 

### Important Note

Use of this package alone is insufficient for good security. One must make sure to never save workspace to .Rdata on exit and never set
cache=TRUE in `quarto` or `rmarkdown` files for a block that contains private health information (PHI) or private identifying information (PII).

In RStudio, look under `Tools -> Global Options -> General` and make sure save is set to NEVER and load is not clicked. This should be done for history as well. 

For base R the following can be ensured using `usethis::edit_r_profile()` and adding this code:

```
newfun <- function (save = "no", status = 0, runLast = TRUE)
  .Internal(quit(save, status, runLast))
pkg <- 'base'
oldfun <- 'q'
pkgenv <- as.environment(paste0("package:", pkg))
unlockBinding(oldfun, pkgenv)
utils::assignInNamespace(oldfun, newfun, ns = pkg, envir = pkgenv)
assign(oldfun, newfun, pkgenv)
lockBinding(oldfun, pkgenv)
```

Further making sure if one needs to store PHI/PII locally that is always stored to an encrypted volume in some form is important. 

### Encryption Details

* Encryption: [XSalsa20](https://en.wikipedia.org/wiki/Salsa20) stream cipher
* Authentication: [Poly1305](https://en.wikipedia.org/wiki/Poly1305) MAC

In 2013, Mouha and Preneel published a proof^[Nicky Mouha; Bart Preneel (2013). "_Towards Finding Optimal Differential Characteristics for ARX: Application to Salsa20_". International Association for Cryptologic Research.] that 15 rounds of Salsa20 was 128-bit secure against differential cryptoanalysis. It has no differential characteristic with higher probability than 2^−130, so differential cryptoanalysis would be more difficult than 128-bit key exhaustion.

## Quick Start Guide

One simply needs to include the library and specify a connection function. Here
is an example that works with splunk and [redcapAPI](https://github.com/vubiostat/redcapAPI).

```
library(shelter)

###########################################################
## Splunk Routines
library(httr2)
splunkurl <- "https://<yoursplunkurl>/services/collector"
postSplunk <- function(api_key, url  = splunkurl, body = NA)
{
  auth <- paste0("Splunk ", api_key)
  request(url)                        |>
  req_headers("Authorization" = auth) |>
  req_body_json(body)                 |>
  req_method("POST")                  |>
  req_perform(req)
}

# Splunk connection function
connectSplunk <- function(key, url=splunkurl)
{
  postSplunk(key, url, "API key test")
  function(body=NA) postSplunk(key, url, body)
}

###########################################################
## redcapAPI Routines
library(redcapAPI)

###########################################################
## shelter Store and Retrieve API KEYS
unlockKeys(
  c('splunk', 'redcap'),
  'mykeyring',
  list(connectSplunk,
       function(key) function(key) connectAndCheck(key, '<URL>')),
  envir=1
)

```

## Community Guidelines

This package exists to serve the research community and would not exist without community support. We are interested in volunteers who would like to translate the documentation into other languages.

### Contribute

If you wish to contribute new features to this software, we are open to [pull requests](https://github.com/vubiostat/shelter/pulls). Before doing a lot of work, it would be best to open [issue](https://github.com/vubiostat/shelter/issues) for discussion about your idea. 

### Report Issues or Problems

Report issues or seek support [here](https://github.com/vubiostat/shelter/issues).

## Documentation

The help pages for the functions is fairly extensive. Try `?unlockKeys` for the 
principal recommended function to use.

## Back Matter

The keyring management code was inspired by the [keyring](https://github.com/r-lib/keyring) package, but is
a complete rewrite to match this project's goals. Special thanks to [Gábor Csárdi](https://github.com/gaborcsardi) for his work over the years leading
to this current work.

### License

`shelter` A method to securely deal with API keys across multiple environments

Copyright (C) 2023-2025 Shawn Garbett, Cole Beck, Hui Wu, Benjamin Nutter, Savannah Obregon, Vanderbilt University

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).
