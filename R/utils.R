#####################################################
# Utility functions
#####################################################

utils::globalVariables(c("a", "x", "y", "X", "X0", "X1", "X_dstar", "ipw", "treated", "control"))

`%notin%` <- Negate(`%in%`)

# logical or infix function
`%||%` <- function(a, b) if (!is.null(a)) a else b

# get args from a fitted object and its class (for lm, glm, gbm, and ps)
get_args <- function(object, class){
  if(class %in% c("lm", "glm", "gbm")){
    args <- object$call
    args[[1]] <- NULL
  } else if(class == "ps"){
    args <- object$parameters
    args[[1]] <- NULL
  } else args <- NULL
  as.list(args)
}

# get formula from a fitted object and its class
get_formula <- function(object, class){

  if(class %in% c("lm", "glm", "gbm", "ps")){
    form <- eval(get_args(object, class)[["formula"]])
  } else if(class %in% c("pbart", "wbart")){
    x <- attr(object$varcount.mean, "names")
    form <- as.formula(paste(y, " ~ ", paste(x, collapse= "+")))
  } else return(NULL)
  environment(form) <- parent.frame()
  form
}

# get model frame except Y from formula and data
mframe <- function(formula, data){

  mf <- model.frame(formula, data, na.action = NULL)
  mf[, -1, drop = FALSE]
}

# fit a model given class, args, and newdata
fit <- function(class, formula, args, newdata){
  if(class %in% c("pbart", "wbart")){
    X <- as.matrix(mframe(formula, data = newdata))
    Y <- model.frame(formula, data = newdata, na.action = NULL)[[1]]
    args <- list(x.train = X, y.train = Y, rm.const = FALSE)
    sink(tempfile()); on.exit(sink(), add = TRUE)
    do.call(get(class), args)
  } else {
    args$data <- newdata
    sink(tempfile()); on.exit(sink(), add = TRUE)
    do.call(get(class), args)
  }
}


impute <- function(model, mf){

  mf1_control <- mf[control, , drop = FALSE]
  mf0_treated <- mf[treated, , drop = FALSE]
  mf1_control[, a] <- d
  mf0_treated[, a] <- dstar

  sink(tempfile()); on.exit(sink(), add = TRUE)
  imp_y1_control <- pred(model, mf1_control)
  imp_y0_treated <- pred(model, mf0_treated)

  list(imp_y1_control, imp_y0_treated)
}

pure <- function(imp, class, args, family){

  imp_y1_control <- imp[[1]]
  imp_y0_treated <- imp[[2]]

  if(class %in% c("pbart", "wbart")){

    args_imp_y1 <- list(x.train = as.matrix(X_dstar),
                        y.train = imp_y1_control,
                        x.test = as.matrix(X),
                        rm.const = FALSE)
    args_imp_y0 <- list(x.train = as.matrix(X1),
                        y.train = imp_y0_treated,
                        x.test = as.matrix(X),
                        rm.const = FALSE)

    sink(tempfile()); on.exit(sink(), add = TRUE)
    model_imp_y1 <- do.call("wbart", args_imp_y1)
    model_imp_y0 <- do.call("wbart", args_imp_y0)

    imp_y1 <- model_imp_y1[["yhat.test.mean"]]
    imp_y0 <- model_imp_y0[["yhat.test.mean"]]

  } else{

    args_imp_y0 <- args_imp_y1 <- args

    args_imp_y1$formula <- as.formula(paste("imp_y1_control", " ~ ",
                                            paste(x, collapse= "+")))
    args_imp_y0$formula <- as.formula(paste("imp_y0_treated", " ~ ",
                                            paste(x, collapse= "+")))

    if(class == "gbm"){
      X_dstar$imp_y1_control <- imp_y1_control
      X1$imp_y0_treated <- imp_y0_treated
      args_imp_y1$distribution <- args_imp_y0$distribution <- "gaussian"

    } else if(!is.null(family) && family[["family"]] %in% c("binomial", "poisson")){
      args_imp_y1$family <- args_imp_y0$family <- paste0("quasi", family[["family"]])
    }

    args_imp_y1$data <- X_dstar
    args_imp_y0$data <- X1

    model_imp_y1 <- do.call(get(class), args_imp_y1)
    model_imp_y0 <- do.call(get(class), args_imp_y0)

    imp_y1 <- pred(model_imp_y1, X)
    imp_y0 <- pred(model_imp_y0, X)
  }

  imp_Ey1 <- mean(imp_y1, na.rm = TRUE)
  imp_Ey0 <- mean(imp_y0, na.rm = TRUE)

  c(imp_Ey1, imp_Ey0)
}

hybrid <- function(imp){

  imp_y1_control <- imp[[1]]
  imp_y0_treated <- imp[[2]]

  imp_Ey1 <- sum(imp_y1_control * ipw[control], na.rm = TRUE)/sum(ipw[control], na.rm = TRUE)
  imp_Ey0 <- sum(imp_y0_treated * ipw[treated], na.rm = TRUE)/sum(ipw[treated], na.rm = TRUE)

  c(imp_Ey1, imp_Ey0)
}

# Calculate bootstrap p-values
pval <- function(x){
  if(all(is.na(x))){
    return(NA)
  } else{
    out <- 1 - 2 * abs(mean(x < 0, na.rm = TRUE) - 0.5)
  }
  out
}
