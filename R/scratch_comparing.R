# library(dplyr)
#
# out1
#
#
# out_jz %>%
#   mutate(variable = gsub(pattern = "OM", replacement = "", x = variable)) %>%
#   mutate(variable = gsub(pattern = "MB", replacement = "B", x = variable)) ->
#   out_jz
#
# out1$model <- "mind"
# out_jz$model <- "jz"
#
#   data.table::melt(data.table::as.data.table(out3),
#                    measure.vars = names(MEND_model$state),
#                    variable.name = "variable",
#                    value.name = 'value') %>%
#   mutate(model = "trying again") ->
#   out3
#
# bind_rows(out1, out_jz, out3) %>%
#  # filter(time <= 100) %>%
# ggplot() +
#   geom_line(aes(time, value, color = model, linetype = model)) +
#   facet_wrap("variable", scales = "free")
