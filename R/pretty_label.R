pretty_label <- function(x) {
   rlang::enquo(x) %>%
    rlang::eval_tidy() %>%
    rlang::as_name() %>%
    gsub("_|\\.", " ", .) %>%
    trimws() %>%
    tools::toTitleCase()
}
