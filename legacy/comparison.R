# Compare the MEMC results with the results from JZ's original model.

library(dplyr)
library(ggplot2)

jz_results <- read.csv(here::here("legacy", "jz_results.csv"))
memc_results <- read.csv(here::here("legacy", "memc_results.csv"))

# 1. MEND ---------------------------------------------------------------------------------------
memc_results %>%
  filter(name == "MEND") %>%
  inner_join(jz_results) %>%
  mutate(dif = memc-value) ->
  mend_df

ggplot(data = mend_df) +
  geom_line(aes(time, value, color = "jz")) +
  geom_line(aes(time, value, color = "memc")) +
  facet_wrap("variable", scales = "free")

assert_that(mean((mend_df$dif)^2) < 1e-4)

ggplot(data = mend_df) +
  geom_line(aes(time, dif)) +
  facet_wrap("variable", scales = "free")


# 2. COM ---------------------------------------------------------------------------------------
memc_results %>%
  filter(name == "COM") %>% head()
inner_join(jz_results) %>%
  mutate(dif = memc-value) ->
  com_df

com_df %>%
  filter(time == 0)


ggplot(data = com_df) +
  geom_line(aes(time, value, color = "jz")) +
  geom_line(aes(time, value, color = "memc")) +
  facet_wrap("variable", scales = "free")

assert_that(mean((com_df$dif)^2) < 1e-4)

ggplot(data = com_df) +
  geom_line(aes(time, dif)) +
  facet_wrap("variable", scales = "free")


# 3. LIN ---------------------------------------------------------------------------------------
memc_results %>%
  filter(name == "LIN") %>%
  inner_join(jz_results) %>%
  mutate(dif = memc-value) ->
  lin_df

lin_df %>%
  filter(time == 0)


ggplot(data = lin_df) +
  geom_line(aes(time, value, color = "jz")) +
  geom_line(aes(time, value, color = "memc")) +
  facet_wrap("variable", scales = "free")

assert_that(mean((lin_df$dif)^2) < 1e-4)

ggplot(data = lin_df) +
  geom_line(aes(time, dif)) +
  facet_wrap("variable", scales = "free")



# 4. TOY  ---------------------------------------------------------------------------------------

memc_results %>%
  filter(name == "TOY") %>%
  inner_join(jz_results) %>%
  mutate(dif = memc-value) ->
  toy_df

toy_df %>%
  filter(time == 0)


ggplot(data = toy_df) +
  geom_line(aes(time, value, color = "jz")) +
  geom_line(aes(time, value, color = "memc")) +
  facet_wrap("variable", scales = "free")

assert_that(mean((com_df$dif)^2) < 1e-4)

ggplot(data = com_df) +
  geom_line(aes(time, dif)) +
  facet_wrap("variable", scales = "free")






