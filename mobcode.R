logit <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
    glm(y ~ 0 + x, family = binomial, start = start, ...)
  }