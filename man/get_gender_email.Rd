% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_gender.R
\name{get_gender_email}
\alias{get_gender_email}
\title{Function to query Gender API v2 by email address}
\usage{
get_gender_email(
  email,
  api_token,
  country = NULL,
  locale = NULL,
  ip = NULL,
  id = NULL
)
}
\arguments{
\item{email}{email address as a string}

\item{api_token}{your API token as a string (Bearer token)}

\item{country}{optional ISO 3166 ALPHA-2 country code (e.g. "US")}

\item{locale}{optional browser locale (e.g. "en_US")}

\item{ip}{optional IPv4 or IPv6 address for localization}

\item{id}{optional alphanumeric ID (max 50 chars) for tracking requests}
}
\value{
a data frame with gender prediction results
}
\description{
This function uses Gender API v2 to predict gender based on email address
}
