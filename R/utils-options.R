#' Options used by future.p2p
#'
#' Below are the \R options and environment variables that are used by the
#' \pkg{future.p2p} package.\cr
#' \cr
#' _WARNING: Note that the names and the default values of these options may
#'  change in future versions of the package.  Please use with care until
#'  further notice._
#'
#' @section Packages must not change these options:
#'
#' Just like for other R options, as a package developer you must _not_ change
#' any of the below options.  Only the end-user should set these.
#' If you find yourself having to tweak one of the options, make sure to
#' undo your changes immediately afterward.
#'
#' @section Options:
#' \describe{
#'  \item{\option{future.p2p.wormhole}:}{(character)
#'    Specifies the absolute path to the `wormhole` executable. If not
#'    specified, a default one will be installed.
#     (Default: not set)
#'  }
#' }
#'
#' @section Options for debugging:
#' \describe{
#'  \item{\option{future.p2p.debug}:}{(logical)
#'    If `TRUE`, extensive debug messages are generated.
#     (Default: `FALSE`)
#'  }
#' }
#'
#'
#' @section Environment variables that set R options:
#' All of the above \R \option{future.p2p.*} options can be set by
#' corresponding environment variable \env{R_FUTURE_P2P_*} _when the
#' \pkg{future.p2p} package is loaded_.
#' This means that those environment variables must be set before
#' the \pkg{future.p2p} package is loaded in order to have an effect.
#' For example, if `R_FUTURE_P2P_DEBUG=true`, then option
#' \option{future.p2p.debug} is set to `TRUE` (logical).
#'
#' @examples
#' # See debug messages
#' options(future.p2p.debug = TRUE)
#'
#' @aliases
#' future.p2p.options
#'
#' future.p2p.debug
#' future.p2p.wormhole
#'
#' R_FUTURE_P2P_DEBUG
#' R_FUTURE_P2P_WORMHOLE
#'
#' @name zzz-future.p2p.options 
NULL


setOption <- function(name, value) {
  oldValue <- getOption(name)
  args <- list(value)
  names(args) <- name
  do.call(options, args = args)
  invisible(oldValue)
}


# Set an R option from an environment variable
update_package_option <- function(name, mode = "character", default = NULL, split = NULL, trim = TRUE, disallow = c("NA"), force = FALSE, debug = FALSE) {
  ## Nothing to do?
  value <- getOption(name)
  if (!force && !is.null(value)) return(getOption(name, default = default))

  ## name="future.plan.disallow" => env="R_FUTURE_PLAN_DISALLOW"
  env <- gsub(".", "_", toupper(name), fixed = TRUE)
  env <- paste("R_", env, sep = "")

  env_value <- value <- Sys.getenv(env, unset = NA_character_)
  ## Nothing to do?
  if (is.na(value)) {  
    if (debug) mdebugf("Environment variable %s not set", sQuote(env))
    if (!is.null(default)) setOption(name, default)
    return(getOption(name, default = default))
  }
  
  if (debug) mdebugf("%s=%s", env, sQuote(value))

  ## Trim?
  if (trim) value <- trim(value)

  ## Nothing to do?
  if (!nzchar(value)) {
    if (!is.null(default)) setOption(name, default)
    return(getOption(name, default = default))
  }

  ## Split?
  if (!is.null(split)) {
    value <- strsplit(value, split = split, fixed = TRUE)
    value <- unlist(value, use.names = FALSE)
    if (trim) value <- trim(value)
  }

  ## Coerce?
  mode0 <- storage.mode(value)
  if (mode0 != mode) {
    suppressWarnings({
      storage.mode(value) <- mode
    })
    if (debug) {
      mdebugf("Coercing from %s to %s: %s", mode0, mode, commaq(value))
    }
  }

  if (length(disallow) > 0) {
    if ("NA" %in% disallow) {
      if (anyNA(value)) {
        stopf("Coercing environment variable %s=%s to %s would result in missing values for option %s: %s", sQuote(env), sQuote(env_value), sQuote(mode), sQuote(name), commaq(value))
      }
    }
    if (is.numeric(value)) {
      if ("non-positive" %in% disallow) {
        if (any(value <= 0, na.rm = TRUE)) {
          stopf("Environment variable %s=%s specifies a non-positive value for option %s: %s", sQuote(env), sQuote(env_value), sQuote(name), commaq(value))
        }
      }
      if ("negative" %in% disallow) {
        if (any(value < 0, na.rm = TRUE)) {
          stopf("Environment variable %s=%s specifies a negative value for option %s: %s", sQuote(env), sQuote(env_value), sQuote(name), commaq(value))
        }
      }
    }
  }
  
  if (debug) {
    mdebugf("=> options(%s = %s) [n=%d, mode=%s]",
            dQuote(name), commaq(value),
            length(value), storage.mode(value))
  }

  setOption(name, value)
  
  getOption(name, default = default)
}


## Set options based on environment variables
update_package_options <- function(debug = FALSE) {
  update_package_option("future.p2p.debug", mode = "logical")
  update_package_option("future.p2p.wormhole", mode = "character")
}
