library(readxl) # nolint: undesirable_function_linter.
library(dplyr) # nolint: undesirable_function_linter.
library(stringr) # nolint: undesirable_function_linter.
library(tidyr) # nolint: undesirable_function_linter.
library(janitor) # nolint: undesirable_function_linter.


svar <- read_excel(here::here("data/260612-spmogsvar.xlsx")) |>  clean_names()

fasit <- read_excel(here::here("data/260629-fasit.xlsx")) |>
  clean_names() |>
  mutate(autograf = "Fasit") |>
  filter(fullforingstidspunkt > lubridate::mdy("06172026"))

fasit <- cbind(fasit[, 1:5],
               tibble(tidspunkt_for_siste_endring = fasit$fullforingstidspunkt),
               fasit[, 6:ncol(fasit)])
colnames(fasit) <- colnames(svar)


raw <- bind_rows(svar,
                 fasit)
rm(list = c("svar", "fasit"))


meta_cols <- names(raw)[1:8]
smaaspoersmaal <- names(raw)[9:16]
irak <- names(raw)[17:23]
senegal <- names(raw)[24:30]
frankrike <- names(raw)[31:37]
joker <- names(raw)[38]
grupper <- names(raw)[39:50]
naar_ryker <- names(raw)[51:57]
finale <- names(raw)[58:66]
avslutning <- names(raw)[67:72]

#### Tipping gruppespill ####
smaaspoersmaal_ind <- seq(1, length(smaaspoersmaal))
smaaspoersmaal_poeng <- c(rep(1, 7), 2)
smaaspoersmaal_spm <- smaaspoersmaal

smaaspoersmaal_svar <- raw |>
  filter(!is.na(id)) |>
  select(id, all_of(smaaspoersmaal))

smaaspoersmaal_spm <- tibble(spm = paste0("Q", smaaspoersmaal_ind),
                             tekst = smaaspoersmaal_spm,
                             poeng = smaaspoersmaal_poeng)
colnames(smaaspoersmaal_svar)[
                              2:ncol(smaaspoersmaal_svar)] <- smaaspoersmaal_spm$spm

#### Irakkampen ####
irak_ind <- max(smaaspoersmaal_ind) + seq_along(irak)
irak_poeng <- c(1, 1, 2, 2, 2, 2, 2)
irak_spm <- irak
irak_svar <- raw |>
  filter(!is.na(id)) |>
  select(id, all_of(irak))

irak_spm <- tibble(spm = paste0("Q", irak_ind),
                   tekst = irak_spm,
                   poeng = irak_poeng)
colnames(irak_svar)[2:ncol(irak_svar)] <- irak_spm$spm

#### Senegal ####

senegal_ind <- max(irak_ind) + seq_along(senegal)
senegal_poeng <- c(1, 1, 2, 2, 2, 2, 2)
senegal_spm <- senegal
senegal_svar <- raw |>
  filter(!is.na(id)) |>
  select(id, all_of(senegal))

senegal_spm <- tibble(spm = paste0("Q", senegal_ind),
                      tekst = senegal_spm,
                      poeng = senegal_poeng)
colnames(senegal_svar)[2:ncol(senegal_svar)] <- senegal_spm$spm



#### Frankrike ####

frankrike_ind <- max(senegal_ind) + seq_along(frankrike)
frankrike_poeng <- c(1, 1, 2, 2, 2, 2, 2)
frankrike_spm <- frankrike
frankrike_svar <- raw |>
  filter(!is.na(id)) |>
  select(id, all_of(frankrike))

frankrike_spm <- tibble(spm = paste0("Q", frankrike_ind),
                        tekst = frankrike_spm,
                        poeng = frankrike_poeng)
colnames(frankrike_svar)[2:ncol(frankrike_svar)] <- frankrike_spm$spm



#### Jokerspm ####

joker_ind <- max(frankrike_ind) + seq_along(joker)
joker_poeng <- 0
joker_spm <- joker
joker_svar <- raw |>
  filter(!is.na(id)) |>
  select(id, all_of(joker))

joker_spm <- tibble(spm = paste0("Q", joker_ind),
                    tekst = joker_spm,
                    poeng = joker_poeng)
colnames(joker_svar)[2:ncol(joker_svar)] <- joker_spm$spm



#### Gruppesortering ####
grupper_spm <- raw |>
  select(id, all_of(grupper)) |>
  rename_with(~LETTERS[1:12],
              all_of(grupper)) |>
  pivot_longer(
    cols = LETTERS[1:12],
    names_to = "question_id",
    values_to = "answer"
  ) |>
  separate(
    answer,
    into = c("first", "second", "third", "fourth"),
    sep = ";",
    remove = FALSE
  )  |>
  mutate(across(c(first, second, third, fourth), str_trim)) |>
  pivot_longer(
    cols = c(first, second, third, fourth),
    names_to = "position",
    values_to = "team"
  ) |>
  mutate(
    spm = case_match(question_id,
      "A" ~ joker_ind + 1,
      "B" ~ joker_ind + 2,
      "C" ~ joker_ind + 3,
      "D" ~ joker_ind + 4,
      "E" ~ joker_ind + 5,
      "F" ~ joker_ind + 6,
      "G" ~ joker_ind + 7,
      "H" ~ joker_ind + 8,
      "I" ~ joker_ind + 9,
      "J" ~ joker_ind + 10,
      "K" ~ joker_ind + 11,
      "L" ~ joker_ind + 12
    ),
    delspm = case_match(position,
                        "first" ~ paste0("Q", spm, ".1"),
                        "second" ~ paste0("Q", spm, ".2"),
                        "third" ~ paste0("Q", spm, ".3"),
                        "fourth" ~ paste0("Q", spm, ".4")),
    position = recode(position,
      first  = paste0("Gruppevinner ", question_id),
      second = paste0("Gruppetoer ", question_id),
      third  = paste0("Gruppetreer ", question_id),
      fourth = paste0("Gruppetaper ", question_id)
    )
  )




grupper_svar <- grupper_spm |>
  select(id, delspm, team) |>
  pivot_wider(names_from = delspm, values_from = team)

grupper_spm <- grupper_spm |>
  select(spm = delspm,
         tekst = position) |>
  mutate(poeng = 1)




#### Når ryker? ####

naar_ryker_spm <- tibble(spm = paste0("Q", 43:49),
                         tekst = naar_ryker,
                         poeng = 3)

naar_ryker_svar <- raw |>
  select(id,
         all_of(naar_ryker)) |>
  filter(!is.na(id)) |>
  rename_with(~paste0("Q", 43:49),
              all_of(naar_ryker))


#### finaler ####
finale_spm <- tibble(spm = paste0("Q", 50:58),
                     tekst = finale,
                     poeng = c(5, rep(3, 8)))

finale_svar <- raw |>
  select(id, all_of(finale)) |>
  filter(!is.na(id))
colnames(finale_svar) <- c("id", paste0("Q", 50:58))



#### Toppscorer ####

avslutning_spm <- tibble(spm = paste0("Q", 59:64),
                         tekst = avslutning,
                         poeng = c(rep(3, 5), 5))

avslutning_svar <- raw |>
  select(id, all_of(avslutning)) |>
  filter(!is.na(id))
colnames(avslutning_svar) <- c("id", paste0("Q", 59:64))


#### Samlet data ####
metadata <- raw |> select(all_of(meta_cols)) |> filter(!is.na(id))

alle_svar <- smaaspoersmaal_svar |>
  left_join(irak_svar, by = join_by(id)) |>
  left_join(senegal_svar, by = join_by(id)) |>
  left_join(frankrike_svar, by = join_by(id)) |>
  left_join(joker_svar, by = join_by(id)) |>
  left_join(grupper_svar, by = join_by(id)) |>
  left_join(naar_ryker_svar, by = join_by(id)) |>
  left_join(finale_svar, by = join_by(id)) |>
  left_join(avslutning_svar, by = join_by(id)) |>
  pivot_longer(starts_with("Q"), names_to = "spm", values_to = "svar") |>
  replace_na(list(svar = "Mangler svar"))

alle_spm <- rbind(
  smaaspoersmaal_spm,
  irak_spm,
  senegal_spm,
  frankrike_spm,
  joker_spm,
  grupper_spm,
  naar_ryker_spm,
  finale_spm,
  avslutning_spm
)

rm(list = c(
  "avslutning", "avslutning_spm", "avslutning_svar",
  "finale", "finale_spm", "finale_svar",
  "frankrike", "frankrike_ind", "frankrike_poeng",
  "frankrike_spm", "frankrike_svar",
  "grupper", "grupper_spm", "grupper_svar",
  "irak", "irak_ind", "irak_poeng", "irak_spm", "irak_svar",
  "joker", "joker_ind", "joker_poeng", "joker_spm", "joker_svar",
  "meta_cols", "raw",
  "naar_ryker", "naar_ryker_spm", "naar_ryker_svar",
  "senegal", "senegal_ind", "senegal_poeng", "senegal_spm", "senegal_svar",
  "smaaspoersmaal", "smaaspoersmaal_ind", "smaaspoersmaal_poeng",
  "smaaspoersmaal_spm", "smaaspoersmaal_svar"
))



#### Telle poeng ####
alle_spm$tekst <- gsub("_", " ", alle_spm$tekst)
alle_spm$tekst <- gsub(
  " riktig svar gir like mange poeng som norge far i gruppespillet",
  "",
  alle_spm$tekst
)
alle_spm$tekst <- gsub(" riktig svar gir 5 poeng", "", alle_spm$tekst)
alle_spm$tekst <- gsub(" riktig svar gir 3 poeng", "", alle_spm$tekst)
alle_spm$tekst <- gsub(" riktig svar gir 2 poeng", "", alle_spm$tekst)


fasitsvar <- metadata |>
  filter(autograf == "Fasit") |>
  mutate(fasitdato = paste("Fasit", klubblaget_ditt)) |>
  select(id, fasitdato)


fasit <- alle_svar |>
  inner_join(fasitsvar, by = join_by(id)) |>
  select(-id) |>
  pivot_wider(names_from = fasitdato, values_from = svar)

alle_spm <- alle_spm |>
  left_join(fasit,
            by = join_by(spm)) |>
  distinct()

alle_spm$poeng[alle_spm$spm == "Q30"] <- as.numeric(gsub(
  " poeng",
  "",
  alle_spm$`Fasit 29.06.2026`[alle_spm$spm == "Q30"]
))


alle_svar <- alle_svar |>
  anti_join(fasitsvar,
            by = join_by(id))

alle_svar$svar[alle_svar$spm == "Q46" & alle_svar$id == 11] <- "Svarte ikke"

rm(list = c("fasitsvar", "fasit"))
