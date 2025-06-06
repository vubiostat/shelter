% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keyring.R
\name{keyring_list}
\alias{keyring_list}
\title{Provides a `data.frame` of information on available keyrings.}
\usage{
keyring_list()
}
\value{
data.frame of (keyring, secrets, locked)
}
\description{
Looks in a local directory where keyrings are stored for the
current user and returns information about keyrings found.
Keyrings are stored in `rappdirs::user_config_dir("r-shelter")`
and end in `.keyring.RDS`
}
\examples{
keyring_list()

}
