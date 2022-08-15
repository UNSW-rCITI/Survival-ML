#' @export
makeRLearner.surv.reg <- function() {
  makeRLearnerSurv(
    cl = "surv.reg",
    package = "survival",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "dist", default = "weibull", values = c("weibull", "exponential", "lognormal", "loglogistic"))
    ),
    properties = c("numerics", "factors", "weights"),
    name = "Parametric Survival Model",
    short.name = "survreg"
    # callees = c("coxph", "coxph.control")
  )
}
#' @export
trainLearner.surv.reg <- function(.learner, .task, .subset, .weights = NULL, ...) {
  f <- getTaskFormula(.task)
  data <- getTaskData(.task, subset = .subset)
  if (is.null(.weights)) {
    survival::survreg(formula = f, data = data, ...)
  } else {
    survival::survreg(formula = f, data = data, weights = .weights, ...)
  }
}
#' @export
predictLearner.surv.reg <- function(.learner, .model, .newdata, ...) {
  -predict(.model$learner.model, newdata = .newdata, type = "lp", ...)
  # survival::predict.survreg(.model$learner.model, newdata = .newdata, type = "response", ...)
}
# note:
# with the help of https://stackoverflow.com/questions/70499145/how-to-create-parametric-survival-learner-for-mlr-in-r
# and coxPH learner in mlr github
