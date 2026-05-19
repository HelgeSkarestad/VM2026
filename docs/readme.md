---
author:
- Helge & KI
authors:
- Helge & KI
execute:
  echo: true
  message: false
  warning: false
title: VM-tuppekunkurranse -- Resultater
toc-title: Table of contents
---

-   [[1]{.toc-section-number} Innledning](#innledning){#toc-innledning}
-   [[2]{.toc-section-number}
    Datagrunnlag](#datagrunnlag){#toc-datagrunnlag}

## Innledning {#innledning number="1"}

Denne rapporten viser resultatene fra VM-tuppekunkurransen. Poeng er
beregnet ved å sammenligne hvert tips mot **fasit-raden**
(`Navn = fasit`) i Excel-filen.

------------------------------------------------------------------------

## Datagrunnlag {#datagrunnlag number="2"}

:::: cell
``` {.r .cell-code}
ggplot(leaderboard, aes(x = reorder(Navn, Total), y = Total)) +
  geom_col(fill = "#2C7FB8") +
  coord_flip() +
  labs(
    title = "Leaderboard – VM-tipping",
    x = NULL,
    y = "Poeng"
  ) +
  geom_text(aes(label = Total), hjust = -0.2) +
  theme_minimal()
```

::: cell-output-display
![](readme_files/figure-markdown/leder-1.png)
:::

``` {.r .cell-code}
# 
# poeng_df %>%
#   select(Navn, ends_with("_poeng"))
```
::::
