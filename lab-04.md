Lab 04 - La Quinta is Spanish for next to Denny’s, Pt. 1
================
Olivia Zhang
02/14/2025

### Load packages and data

``` r
#install.packages("devtools")
#devtools::install_github("rstudio-education/dsbox")

library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
```

### Exercise 1

The Denny’s dataset has 1643 rows and 6 columns/variables.

### Exercise 2

The Quinta’s dataset has 909 rows and 6 columns/variables.

### Exercise 3

There are some La Quinta’s outside of US. They are in countries like
Canada, Mexico, China, New Zealand, Turkey, United Arab Emirates, Chile,
Colombia, Ecuador. However, I don’t think there’s any Danny’s outside of
the US.

### Exercise 4

There are a few ways to determine whether there’s any establishments
that are not in the US. 1. We can create a new variable called US and
check weather the state variable in two datasets are the same or
different from the states dataset. 2. If we know the longitude and
latitude of the US border, techniquely, we can filter out the
establishments outside of US.

### Exercise 5

``` r
dn <- dennys %>%
  filter(!(state %in% states$abbreviation))
```

There are 0 Denny’s locations outside the US.

### Exercise 6

``` r
dennys %>%
  mutate(country = "United States")
```

    ## # A tibble: 1,643 × 7
    ##    address                        city    state zip   longitude latitude country
    ##    <chr>                          <chr>   <chr> <chr>     <dbl>    <dbl> <chr>  
    ##  1 2900 Denali                    Anchor… AK    99503    -150.      61.2 United…
    ##  2 3850 Debarr Road               Anchor… AK    99508    -150.      61.2 United…
    ##  3 1929 Airport Way               Fairba… AK    99701    -148.      64.8 United…
    ##  4 230 Connector Dr               Auburn  AL    36849     -85.5     32.6 United…
    ##  5 224 Daniel Payne Drive N       Birmin… AL    35207     -86.8     33.6 United…
    ##  6 900 16th St S, Commons on Gree Birmin… AL    35294     -86.8     33.5 United…
    ##  7 5931 Alabama Highway, #157     Cullman AL    35056     -86.9     34.2 United…
    ##  8 2190 Ross Clark Circle         Dothan  AL    36301     -85.4     31.2 United…
    ##  9 900 Tyson Rd                   Hope H… AL    36043     -86.4     32.2 United…
    ## 10 4874 University Drive          Huntsv… AL    35816     -86.7     34.7 United…
    ## # ℹ 1,633 more rows

### Exercise 7

``` r
lq <- laquinta %>%
  filter(!(state %in% states$abbreviation))
lq
```

    ## # A tibble: 14 × 6
    ##    address                                  city  state zip   longitude latitude
    ##    <chr>                                    <chr> <chr> <chr>     <dbl>    <dbl>
    ##  1 Carretera Panamericana Sur KM 12         "\nA… AG    20345    -102.     21.8 
    ##  2 Av. Tulum Mza. 14 S.M. 4 Lote 2          "\nC… QR    77500     -86.8    21.2 
    ##  3 Ejercito Nacional 8211                   "Col… CH    32528    -106.     31.7 
    ##  4 Blvd. Aeropuerto 4001                    "Par… NL    66600    -100.     25.8 
    ##  5 Carrera 38 # 26-13 Avenida las Palmas c… "\nM… ANT   0500…     -75.6     6.22
    ##  6 AV. PINO SUAREZ No. 1001                 "Col… NL    64000    -100.     25.7 
    ##  7 Av. Fidel Velazquez #3000 Col. Central   "\nM… NL    64190    -100.     25.7 
    ##  8 63 King Street East                      "\nO… ON    L1H1…     -78.9    43.9 
    ##  9 Calle Las Torres-1 Colonia Reforma       "\nP… VE    93210     -97.4    20.6 
    ## 10 Blvd. Audi N. 3 Ciudad Modelo            "\nS… PU    75010     -97.8    19.2 
    ## 11 Ave. Zeta del Cochero No 407             "Col… PU    72810     -98.2    19.0 
    ## 12 Av. Benito Juarez 1230 B (Carretera 57)… "\nS… SL    78399    -101.     22.1 
    ## 13 Blvd. Fuerza Armadas                     "con… FM    11101     -87.2    14.1 
    ## 14 8640 Alexandra Rd                        "\nR… BC    V6X1…    -123.     49.2

There are 14 Denny’s locations outside the US.

### Exercise 8

``` r
laquinta <- laquinta %>%
  mutate(country = case_when(
    state %in% state.abb ~ "United States",
    state %in% c("ON", "BC") ~ "Canada",
    state == "ANT" ~ "Colombia",
    state == "FM" ~ "Honduras",
    state %in% c("AG", "QR", "CH", "NL", "VE", "PU", "SL") ~ "Mexico",
  ))
```

``` r
laquinta <- laquinta %>%
  filter(country == "United States")
```

### Exercise 9

``` r
dennys %>%
  count(state) %>%
  inner_join(states, by = c("state" = "abbreviation"))
```

    ## # A tibble: 51 × 4
    ##    state     n name                     area
    ##    <chr> <int> <chr>                   <dbl>
    ##  1 AK        3 Alaska               665384. 
    ##  2 AL        7 Alabama               52420. 
    ##  3 AR        9 Arkansas              53179. 
    ##  4 AZ       83 Arizona              113990. 
    ##  5 CA      403 California           163695. 
    ##  6 CO       29 Colorado             104094. 
    ##  7 CT       12 Connecticut            5543. 
    ##  8 DC        2 District of Columbia     68.3
    ##  9 DE        1 Delaware               2489. 
    ## 10 FL      140 Florida               65758. 
    ## # ℹ 41 more rows

### Exercise 10

``` r
dennys <- dennys %>%
  mutate(establishment = "Denny's")
laquinta <- laquinta %>%
  mutate(establishment = "La Quinta")
```

``` r
dn_lq <- bind_rows(dennys, laquinta)
```

``` r
ggplot(dn_lq, mapping = aes(
  x = longitude,
  y = latitude,
  color = establishment
)) +
  geom_point()
```

![](lab-04_files/figure-gfm/plot-1.png)<!-- -->

### Exercise 11

``` r
dn_lq %>%
  filter(state == "NC") %>%
  ggplot(dn_lq, mapping = aes(
    x = longitude,
    y = latitude,
    color = establishment
    )) +
  geom_point(alpha = .7) +
  labs(
  title = "Danny's and La Quinta Locations",
  subtitle = "in North Carolina",
  x = "Longitude of establishements", 
  y = "Latitude of establishements", 
  color = "Establishment"
     )
```

![](lab-04_files/figure-gfm/plot-nc-1.png)<!-- --> Mitch Hedberg’s joke
mostly hold in North Carolina except for a few La Quintas on the upper
left corner of the plot, which are distant from their nearest Denny’s.

### Exercise 12

``` r
dn_lq %>%
  filter(state == "TX") %>%
  ggplot(dn_lq, mapping = aes(
    x = longitude,
    y = latitude,
    color = establishment
    )) +
  geom_point(alpha = .5) +
  labs(
  title = "Danny's and La Quinta Locations",
  subtitle = "in Texas",
  x = "Longitude of establishements", 
  y = "Latitude of establishements", 
  color = "Establishment"
     )
```

![](lab-04_files/figure-gfm/plot-tx-1.png)<!-- --> Mitch Hedberg’s joke
seems to hold less in Texas, since there are more La Quintas than
Denny’s, and not all La Quinta has a Denny’s next to it. However, all
Denny’s are near La Quintas.
