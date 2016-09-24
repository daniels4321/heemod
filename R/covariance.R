#' Title
#'
#' @param x Result from \code{\link{run_probabilistic}}.
#' @param model Name or index of model.
#' @param linear Look at linear relations?
#' @param scale_x Scale inputs?
#' @param scale_y Scale outputs?
#'
#' @return A data frame.
#' @export
#'
compute_covariance <- function(x, model = 1, linear = TRUE,
                               scale_x = TRUE, scale_y = FALSE) {
  model_ref <- attr(x, "model")
  
  check_model_index(x = model_ref, i = model)
  
  if (is.numeric(model)) {
    model <- get_model_names(model_ref)[model]
  }
  
  tab <- x[x$.model_names == model,
           c(attr(x, "var_names"), ".cost", ".effect")]
  
  if (scale_x) {
    for (n in attr(x, "var_names")) {
      tab[[n]] <- scale(tab[[n]])
    }
  }
  
  if (scale_y) {
    for (n in c( ".cost", ".effect")) {
      tab[[n]] <- scale(tab[[n]])
    }
  }
  
  if (linear) {
    form_cost <- as.formula(paste(
      ".cost",
      paste(attr(x, "var_names"), collapse = "+"),
      sep = "~"
    ))
    form_effect <- as.formula(paste(
      ".effect",
      paste(attr(x, "var_names"), collapse = "+"),
      sep = "~"
    ))
    
    res_cost <- coef(lm(form_cost, data = tab))[-1]
    res_effect <- coef(lm(form_effect, data = tab))[-1]
  } else {
    stop("Not yet implemented!")
  }
  
  data.frame(
    var = names(res_cost),
    cost = res_cost,
    effect = res_effect
  )
}