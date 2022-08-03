# Format the example data to be used in the fit data example.

DIR <- here::here("data-raw")

# Load the data example data.
data <- read.csv(file.path(DIR, "Ultisol_control.csv"))

# Format the data into a long data frame.
obs_long <- melt(data, "time")
write.csv(obs_long,
          here::here("inst", "example", "exmaple_data.csv"),
          row.names = FALSE)

# The initial values for ultisol that we were given.
Ultisol_state <-
  c(
    P = 4.71,
    M = 17.67,
    Q = 0,
    D = 0.148,
    B = 0.82,
    EP = 0.0082,
    EM = 0.0082,
    IC = 0,
    Tot = 23.484
  )
state <- data.frame(state = names(Ultisol_state),
                    value = Ultisol_state)
write.csv(state,
          here::here("inst", "example", "exmaple_initial.csv"),
          row.names = FALSE)
