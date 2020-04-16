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
#' \donttest{
#' read_xmile("mdl.xmile")
#' }
#' \donttest{
#' read_xmile("mdl.stmx")
#' }

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
