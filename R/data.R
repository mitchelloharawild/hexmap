#' Australian federal election map and winners
#'
#' Simplified Australian federal electoral division boundaries and the party
#' that won each division at the 2022 (`aec2022`) and 2025 (`aec2025`)
#' Australian federal elections.
#'
#' @format An sf object with one row per electoral division and columns:
#' \describe{
#'   \item{division}{Name of the electoral division.}
#'   \item{area}{Area of the division in square kilometres.}
#'   \item{state}{State or territory abbreviation.}
#'   \item{party}{Abbreviation of the winning candidate's party.}
#'   \item{winner}{Winning party grouped into ALP, LNP (Liberal/National
#'     Coalition), GRN (Greens) and Other.}
#'   \item{geometry}{Boundary of the division.}
#' }
#'
#' @source The data incorporates electoral boundary data and election result
#' data from © Commonwealth of Australia (Australian Electoral Commission)
#' \url{https://www.aec.gov.au/} and boundaries from the Australian Bureau of
#' Statistics \url{https://www.abs.gov.au/} (2025), licensed under CC BY 4.0.
#' @name aec
"aec2022"

#' @rdname aec
"aec2025"
