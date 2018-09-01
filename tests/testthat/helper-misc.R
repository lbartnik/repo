expect_not_null <- function (object, info = NULL, label = NULL) {
  act <- quasi_label(rlang::enquo(object), label)
  expect(!is.null(act$val), sprintf("%s is null.", act$lab),
         info = info)
  invisible(act$val)
}

expect_node <- function (node, ...) {

  cond <- list(...)
  expect_true(all(names(cond) %in% names(node)))

  lapply(names(cond), function (name) {
    expect_equal(cond[[name]], node[[name]], info = name)
  })

  invisible(TRUE)
}

dummy_plot <- function () {
  on.exit(dev.off())
  png(tempfile(fileext = '.png'))
  dev.control("enable")
  plot(seq(10))
  recordPlot()
}



