#' Read an XMILE file into R
#'
#'\code{read_xmile} returns a list for constructing deSolve functions and graphs
#'
#' This function extracts the xml from the file specified via \code{filepath}
#' to generate a list of objects. Such a list contains a summary of the model,
#' the inputs for simulating through \link[deSolve]{deSolve}, and the inputs for
#' creating a \link[igraph]{igraph} object.
#' @param filepath A string that indicates a path to a file with extension .stmx
#'   or .xmile. Vensim files (.mdl) are not xmile files. They must be exported
#'   from Vensim with extension .xmile
#'
#' @return This function returns a list with three elements. The first element,
#' \emph{description}, is a list that contains the simulation parameters, and
#' the names and equations (including graphical functions) for each stock or
#' level, variable and constant. The second element, \emph{deSolve_components},
#' is a list that contains initial values, constants and the function for
#' simulating via deSolve. The third element, \emph{igraph} contains the
#' data.frames for creating a graph with igraph.
#' @export
#'
#' @examples
#' path <- system.file("models", "SIR.stmx", package = "readsdr")
#' read_xmile(path)
read_xmile <- function(filepath) {

  model_structure    <- extract_structure_from_XMILE(filepath)
  deSolve_components <- get_deSolve_elems(model_structure)

  igraph_inputs      <- tryCatch(
    error = function(cnd) {
      warning("This model cannot be converted into a graph (network)",
              call. = FALSE)
      NULL
    },
    get_igraph_inputs(model_structure)
  )

  list(
    description        = model_structure,
    deSolve_components = deSolve_components,
    graph_dfs          = igraph_inputs
  )
}

#' Parse XMILE to deSolve components
#'
#' \code{xmile_to_deSolve} returns a list that serves as an input for
#' deSolve's ODE function.
#'
#' This function extracts the xml from the file specified via \code{filepath}
#' to generate a list with the necessary elements to simulate with
#' \link[deSolve]{deSolve}.
#'
#' @inheritParams read_xmile
#'
#' @return This function returns a list with at least four elements.
#' \emph{stocks}, a numeric vector that contains initial values. \emph{consts},
#' a numeric vector with the model's constants. \emph{func}, the function that
#' wraps the model's equations. \emph{sim_params}, a list with control
#' parameters. If the model includes a table or graphical function, this
#' function returns the element \emph{graph_funs}, a list with these functions.
#'
#' @export
#'
#' @examples
#' path <- system.file("models", "SIR.stmx", package = "readsdr")
#' xmile_to_deSolve(path)
xmile_to_deSolve <- function(filepath) {
  model_structure    <- extract_structure_from_XMILE(filepath)
  deSolve_components <- get_deSolve_elems(model_structure)
}
