#' Handle deprecated argument
#'
#' @param old_arg the deprecated argument value
#' @param new_arg the new argument value
#' @param old_name the old argument name (with dot)
#' @param new_name the new argument name (with underscore)
#' @param fn_name the function name
#' @param version the version when deprecated
#' @return the value to use (new_arg if provided, otherwise old_arg)
#' @keywords internal
#' @autoglobal
deprecate_arg <- function(
  old_arg,
  new_arg,
  old_name,
  new_name,
  fn_name,
  version = "1.0.0"
) {
  if (lifecycle::is_present(old_arg)) {
    lifecycle::deprecate_warn(
      version,
      paste0(fn_name, "(", old_name, ")"),
      paste0(fn_name, "(", new_name, ")")
    )
    old_arg
  } else {
    new_arg
  }
}
