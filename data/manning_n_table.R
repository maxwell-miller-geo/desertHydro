#' Manning's n Table for NLCD Land Cover Types
#'
#' A dataset containing Manning's roughness coefficients for various NLCD land cover types.
#'
#' @format A data frame with 15 rows and 4 variables:
#' \describe{
#'   \item{NLCD_Value}{(integer) NLCD classification code.}
#'   \item{Normal_Manning_n}{(numeric) Typical Manning's n value.}
#'   \item{Allowable_Range}{(character) Range of Manning's n values.}
#'   \item{Land_Cover_Definition}{(character) Description of the land cover type.}
#' }
#' @source \doi{10.3133/wsp2339} (USGS Manning's n Values Guide)
#' @examples
#' data(manning_n_table)
#' head(manning_n_table)
"manning_n_table"
