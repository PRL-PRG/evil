
.onLoad <- function(libname, pkgname) {
    set_evil_eval()
}

.onDetach <- function(libpath) {
    set_base_eval()
}
