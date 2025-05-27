has_default_transform <- function(scale) {
  transform_method <- environment(scale$transform)$f
  identical(default_transform, transform_method) || identical(identity, transform_method)
}

default_transform <- function(self, x) {
  transformation <- self$get_transformation()
  new_x <- transformation$transform(x)
  check_transformation(x, new_x, transformation$name, call = self$call)
  new_x
}


