% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/key.R
\name{key_get}
\alias{key_get}
\title{Get a secret from a keyring.}
\usage{
key_get(keyring, key)
}
\arguments{
\item{keyring}{character(1); Name of keyring}

\item{key}{character(1); Name of key}
}
\value{
character(1); The requested secret
}
\description{
Get a secret from an unlocked keyring given it's key.
}
\examples{
\dontrun{
key_get('mypersonalkeyring', 'key1')
}

}
