bernoulliBOW_x = data.frame("dog" = c(0,0,1,0,1,0,1,1,0,1), "cat" = c(1,1,0,1,1,0,0,0,1,0),
                            "head" = c(1,0,1,1,1,0,0,0,1,1), "neck" = c(1,0,0,0,1,0,0,1,1,0),
                            "bear" = c(1,0,1,1,0,1,0,0,0,0), "age" = c(0,0,0,0,1,1,1,1,0,0))

fun <- function(formula, data) {
  ## handle formula
  
  print(inherits(data, "formula"))
  m <- match.call(expand.dots = FALSE)
  #m$... <- NULL
  #m$laplace = NULL
  #m$na.action <- na.action
  m[[1L]] <- quote(stats::model.frame)
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  #if (any(attr(Terms, "order") > 1))
  #  stop("naiveBayes cannot handle interaction terms")
  Y <- model.extract(m, "response")
  X <- m[,-attr(Terms, "response"), drop = FALSE]
  
  print(Y)
  print(class(Y))
  print(X)
  print(is.data.frame(X))
}

fun(dog ~., bernoulliBOW_x)

