% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_schema_rand.R
\name{check_user_permission}
\alias{check_user_permission}
\title{Check User Permissions}
\usage{
check_user_permission(
  postgres_module,
  study_id,
  user_id,
  permission_type,
  action = "view"
)
}
\arguments{
\item{postgres_module}{A postgresModule instance}

\item{study_id}{Character string identifying the study}

\item{user_id}{User identifier}

\item{permission_type}{Type of permission to check}

\item{action}{Specific action (view, edit, delete, decrypt)}
}
\value{
Boolean indicating if user has permission
}
\description{
Verifies if a user has specific permissions
}
