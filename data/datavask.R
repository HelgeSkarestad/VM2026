library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)


raw <- read_excel("data/spmogsvar.xlsx") |> 
  clean_names()

meta_cols <- names(raw)[1:5]
sant_usant <- names(raw)[6:12]
toppscorer_gruppe <- names(raw)[13:13]
rangering <- names(raw)[14:25]
kvartfinaler <- names(raw)[26]
lengst_duell <- names(raw)[27:32]
finaler <- names(raw)[33:42]
toppscorer <- names(raw)[43]
vinner <- names(raw)[44]

#### Tipping gruppespill ####
sant_usant_ind <- seq(1, length(sant_usant))
sant_usant_poeng <- rep(1,length(sant_usant))
sant_usant_spm <- raw |> 
  slice(3) |> 
  select(all_of(sant_usant)) |> 
  mutate(across(everything(),~substr(.,27,nchar(.)))) |> 
  as.character()
sant_usant_svar <- raw |> 
  filter(!is.na(id)) |> 
  select(id,all_of(sant_usant))

sant_usant_spm <- tibble(spm = paste0("Q",sant_usant_ind),
                         tekst = sant_usant_spm,
                         poeng = sant_usant_poeng)
colnames(sant_usant_svar)[2:ncol(sant_usant_svar)] <- sant_usant_spm$spm

#### Toppscorer gruppespill ####

toppsc_gruppe_ind <- max(sant_usant_ind) + 1
toppsc_gruppe_poeng <- 2
toppsc_gruppe_spm <- raw |> 
  select(all_of(toppscorer_gruppe)) |> 
  slice(1) |> 
  as.character()
toppsc_gruppe_svar <- raw |> 
  filter(!is.na(id)) |> 
  select(id,all_of(toppscorer_gruppe))

toppsc_gruppe_spm <- tibble(spm = paste0("Q",toppsc_gruppe_ind),
                            tekst = toppsc_gruppe_spm,
                            poeng = toppsc_gruppe_poeng)
colnames(toppsc_gruppe_svar)[2:ncol(toppsc_gruppe_svar)] <- toppsc_gruppe_spm$spm

#### Rangering puljer ####

rangering_spm <- raw |> 
  filter(!is.na(id)) |> 
  select(id,all_of(rangering)) |>
  rename_with(~LETTERS[1:12],
              all_of(rangering)) |> 
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
  ) |>
  mutate(across(c(first, second, third, fourth), str_trim)) |>
  pivot_longer(
    cols = c(first, second, third, fourth),
    names_to = "position",
    values_to = "team"
  ) |>
  mutate(
    spm = case_match(question_id,
                     "A" ~ toppsc_gruppe_ind + 1,
                     "B" ~ toppsc_gruppe_ind + 2,
                     "C" ~ toppsc_gruppe_ind + 3,
                     "D" ~ toppsc_gruppe_ind + 4,
                     "E" ~ toppsc_gruppe_ind + 5,
                     "F" ~ toppsc_gruppe_ind + 6,
                     "G" ~ toppsc_gruppe_ind + 7,
                     "H" ~ toppsc_gruppe_ind + 8,
                     "I" ~ toppsc_gruppe_ind + 9,
                     "J" ~ toppsc_gruppe_ind + 10,
                     "K" ~ toppsc_gruppe_ind + 11,
                     "L" ~ toppsc_gruppe_ind + 12
    ),
    delspm = case_match(position,
                        "first" ~ paste0("Q",spm,".1"),
                        "second" ~ paste0("Q",spm,".2"),
                        "third" ~ paste0("Q",spm,".3"),
                        "fourth" ~ paste0("Q",spm,".4")),
    position = recode(position,
                      first  = paste0("Gruppevinner ",question_id),
                      second = paste0("Gruppetoer ",question_id),
                      third  = paste0("Gruppetreer ",question_id),
                      fourth = paste0("Gruppetaper ",question_id)
    )
  )


rangering_svar <- rangering_spm |> 
  select(id,delspm, team) |> 
  pivot_wider(names_from = delspm, values_from=team)

rangering_spm <- rangering_spm |> 
  select(spm=delspm,
         tekst = position,
         poeng = 1)

#### Kvartfinalelag ####
kvartfinaler_spm <- tibble(spm = "Q21",
                           tekst = raw |> select(all_of(kvartfinaler)) |> slice(1) |> as.character(),
                           poeng = 2)

kvartfinaler_svar <- raw |> 
  select(id,
         Q21 = all_of(kvartfinaler)) |> 
  filter(!is.na(id))

#### Hvem kommer lengst ####
lengst_duell_spm <- tibble(spm = paste0("Q",22:27),
                           tekst = paste0(raw |> select(all_of(lengst_duell)) |> slice(1) |> as.character(),
                                          ", ",
                                          raw |> select(all_of(lengst_duell)) |> slice(5) |> mutate(across(everything(), ~substr(.,57,nchar(.)))) |> as.character()),
                           poeng = 2
)
lengst_duell_svar <- raw |> 
  select(id, all_of(lengst_duell)) |> 
  filter(!is.na(id))
colnames(lengst_duell_svar) <- c("id",paste0("Q",22:27))

#### finaler ####
finaler_spm <- tibble(spm = paste0("Q",28:37),
                      tekst = raw |> 
                        select(all_of(finaler)) |> 
                        slice(3) |> 
                        mutate(across(everything(), ~substr(.,2,nchar(.)))) |> 
                        as.character(),
                      poeng = c(rep(1,5),rep(2,5))
)
finaler_svar <- raw |> 
  select(id,all_of(finaler)) |> 
  filter(!is.na(id))
colnames(finaler_svar) <- c("id",paste0("Q",28:37))

#### Toppscorer ####

toppsc_spm <- tibble(spm = paste0("Q",38:44),
                     tekst = paste("Toppscorer",seq(1,7)),
                     poeng = 1)

toppsc_svar <- raw |> 
  select(id,all_of(toppscorer)) |> 
  filter(!is.na(id)) |> 
  separate(
    toppscorer,
    into = paste0("Q",38:44),
    sep = ";",
    remove = FALSE
  ) |> 
  select(id, starts_with("Q"))

#### Vinner ####
vinner_spm <- tibble(spm="Q45",
                     tekst="Hvem vinner VM?",
                     poeng = 10)

vinner_svar <- raw |> 
  select(id, all_of(vinner)) |> 
  filter(!is.na(id))
colnames(vinner_svar) <- c("id","Q45")

#### Samlet data #### 
alle_svar <- raw |> select(all_of(meta_cols)) |> filter(!is.na(id)) |> 
  left_join(sant_usant_svar, by = join_by(id)) |> 
  left_join(toppsc_gruppe_svar, by = join_by(id)) |> 
  left_join(rangering_svar, by = join_by(id)) |> 
  left_join(kvartfinaler_svar, by = join_by(id)) |> 
  left_join(lengst_duell_svar, by = join_by(id)) |> 
  left_join(finaler_svar, by = join_by(id)) |> 
  left_join(toppsc_svar, by = join_by(id)) |> 
  left_join(vinner_svar, by = join_by(id)) 
s
alle_spm <- rbind(sant_usant_spm,
      toppsc_gruppe_spm,
      rangering_spm,
      kvartfinaler_spm,
      lengst_duell_spm,
      finaler_spm,
      toppsc_spm,
      vinner_spm)

alle_spm |> View()
  left_join(alle_s)
cat(
paste0("'",
       alle_svar |> slice(1) |> select(starts_with("Q")) |> colnames(),
       "','",
      alle_svar |> slice(1) |> select(starts_with("Q")) |> t() |> as.character(),
       "',\n"
)
)

tibble(spm = alle_svar |> slice(1) |> select(starts_with("Q")) |> colnames(),
      alle_svar |> slice(1) |> select(starts_with("Q")) |> t() |> as.character(),

tribble('Q1','Ja',
        'Q2','Ja',
        'Q3','Ja',
        'Q4','Ja',
        'Q5','Ja',
        'Q6','Ja',
        'Q7','Ja',
        'Q8','England',
        'Q9.1','Mexico',
        'Q9.2','Sør-Korea',
        'Q9.3','Sør-Afrika',
        'Q9.4','Tsjekkia',
        'Q10.1','Canada',
        'Q10.2','Qatar',
        'Q10.3','Bosnia-Hercegovina',
        'Q10.4','Sveits',
        'Q11.1','Marokko',
        'Q11.2','Skottland',
        'Q11.3','Brasil',
        'Q11.4','Haiti',
        'Q12.1','USA',
        'Q12.2','Australia',
        'Q12.3','Paraguay',
        'Q12.4','Tyrkia',
        'Q13.1','Curaçao',
        'Q13.2','Elfenbenskysten',
        'Q13.3','Tyskland',
        'Q13.4','Ecuador',
        'Q14.1','Nederland',
        'Q14.2','Sverige',
        'Q14.3','Japan',
        'Q14.4','Tunisia',
        'Q15.1','Belgia',
        'Q15.2','Iran',
        'Q15.3','Egypt',
        'Q15.4','New Zealand',
        'Q16.1','Spania',
        'Q16.2','Saudi-Arabia',
        'Q16.3','Kapp Verde',
        'Q16.4','Uruguay',
        'Q17.1','Frankrike',
        'Q17.2','Irak',
        'Q17.3','Senegal',
        'Q17.4','Norge',
        'Q18.1','Algerie',
        'Q18.2','Argentina',
        'Q18.3','Østerrike',
        'Q18.4','Jordan',
        'Q19.1','DR Kongo',
        'Q19.2','Portugal',
        'Q19.3','Usbekistan',
        'Q19.4','Colombia',
        'Q20.1','Kroatia',
        'Q20.2','England',
        'Q20.3','Ghana',
        'Q20.4','Panama',
        'Q21','Frankrike;Norge;',
        'Q22','Første lag',
        'Q23','Første lag',
        'Q24','Siste lag',
        'Q25','Første lag',
        'Q26','Første lag',
        'Q27','Første lag',
        'Q28','Ja',
        'Q29','Nei',
        'Q30','Ja',
        'Q31','Nei',
        'Q32','Ja',
        'Q33','Ja',
        'Q34','Nei',
        'Q35','Ja',
        'Q36','Nei',
        'Q37','Ja',
        'Q38','Kylian Mbappe',
        'Q39','Erling Braut Haaland',
        'Q40','Lionel Messi',
        'Q41','Lamine Yamal',
        'Q42','Harry Kane',
        'Q43','Christiano Ronaldo',
        'Q44','Vinicius Jr',
        'Q45','Australia',)
