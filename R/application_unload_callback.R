
application_unload_callback <- function(context, application) {
    data <- get_data(context)

    rows <- as.list(data)
    new_data <- do.call(rbind, rows)

    set_data(context, new_data)
}
