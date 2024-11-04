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
* Pull from ENV or a local YAML file in a development environment as an override.
* Work consistently across Windows, Mac OS and Linux. 
* Support testing keys via inversion of control using a specified function.

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
  list(connectSplunk, connectREDCap),
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
a complete rewrite to match different project goals. Special thanks to [Gábor Csárdi](https://github.com/gaborcsardi) for his work over the years leading
to this current work.

### License

`shelter` A method to securely deal with API keys across multiple environments

Copyright (C) 2023-2024 Shawn Garbett, Cole Beck, Hui Wu, Benjamin Nutter, Savannah Obregon, Vanderbilt University Medical Center

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
