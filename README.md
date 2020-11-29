
<!-- README.md is generated from README.Rmd. Please edit that file -->

# senadoRES

<!-- badges: start -->

<!-- badges: end -->

The goal of senadoRES is to provide information about the [Senate of
Spain](https://en.wikipedia.org/wiki/Senate_of_Spain). It uses the [Open
Data](https://www.senado.es/web/relacionesciudadanos/datosabiertos/catalogodatos/index.html)
available and some other XML files I found while exploring the website.

## Installation

You can install the released version of senadoRES from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("senadoRES")
```

or

``` r
remotes::install_github("llrs/senadoRES")
```

## Example

To make it easier to work and show some data I’ll use some packages:

``` r
library("senadoRES")
library("dplyr")
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library("ggplot2")
```

### Senadores

This function return the members of the Senate:

``` r
ts <- senadores()
head(ts)
#>         nombre       apellidos legislatura ultCredencial procedTipo
#> 1     FERNANDO    ABAD BÉCQUER           5           260          D
#> 2      ENRIQUE  ABAD BENEDICTO          10           034          E
#> 3     ELISABET     ABAD GIRALT          12           051          D
#> 4 JOSÉ ANTONIO ABAD LÓPEZ-BREA           3           039          E
#> 5     PATRICIA ABASCAL JIMÉNEZ          14           199          E
#> 6       ADOLFO   ABEJÓN ORTEGA           9           081          E
#>   procedLiteral            procedLugar               grupoCod grupoSiglas
#> 1     Designado     Asamblea de Madrid     Asamblea de Madrid         800
#> 2        Electo                Sevilla                Sevilla         800
#> 3     Designada Parlamento de Cataluña Parlamento de Cataluña         807
#> 4        Electo                 Zamora                 Zamora          CP
#> 5        Electa     Eivissa-Formentera     Eivissa-Formentera         800
#> 6        Electo             Valladolid             Valladolid         801
#>   grupoNombre    sex
#> 1         GPS   male
#> 2         GPS   male
#> 3         GPN female
#> 4        GPCP   male
#> 5         GPS female
#> 6         GPP   male
```

We can see the increase of females on the Senate along the legislatures:

``` r
ts %>%
    group_by(legislatura) %>%
    count(sex) %>%
    mutate(total = sum(n)) %>%
    filter(!is.na(sex)) %>%
    mutate(ratio = n/total) %>%
    filter(sex != "male") %>% 
    ggplot() +
    geom_point(aes(legislatura, ratio, col = sex, shape = sex), size = 5) +
    geom_hline(yintercept = 0.5, linetype = 2, col = "red") +
    scale_x_continuous(breaks = seq_len(15)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                       breaks = seq(from = 0, to = 0.7, by = .1),
                       expand = expansion(add = c(0, 0.01)), limits = c(0, NA)) +
    theme_minimal() +
    labs(title = "Ratio of women", x  = "Legislatura", y = "% of women") +
    guides(col = FALSE, shape = FALSE)
```

<img src="man/figures/README-senadoras-1.png" width="100%" />

Or the change of the parties along the years:

``` r
ts %>% 
  group_by(legislatura) %>%
  count(grupoNombre) %>% 
  mutate(grupoNombre = case_when(legislatura == "4" ~ gsub("^G", "GP", grupoNombre),
                                 TRUE ~ grupoNombre)) %>% # Fix an obvious error
  ggplot() +
  geom_tile(aes(legislatura, 
                forcats::fct_reorder2(grupoNombre, legislatura != 0, -legislatura, .fun = sum)),
            fill = "black") +
  scale_x_continuous(breaks = seq_len(15)) +
  theme_minimal() +
  labs(title = "Grupos políticos", x  = "Legislatura", y = element_blank())
#> Warning: Removed 1 rows containing missing values (geom_tile).
```

<img src="man/figures/README-grupos-1.png" width="100%" />
