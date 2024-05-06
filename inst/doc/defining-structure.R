## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(edibble)

## -----------------------------------------------------------------------------
design("Wheat field trial")

## -----------------------------------------------------------------------------
demo <- design("Demo for defining units") %>% 
  set_units(site = 4)

## -----------------------------------------------------------------------------
demo

## -----------------------------------------------------------------------------
serve_table(demo)

## -----------------------------------------------------------------------------
design("Character vector input demo") %>% 
  set_units(site = c("Narrabri", "Horsham", "Parkes", "Roseworthy")) %>% 
  serve_table()

## -----------------------------------------------------------------------------
design("Numeric vector input demo") %>% 
  set_units(site = c(1, 2, 3, 4)) %>% 
  serve_table()

## -----------------------------------------------------------------------------
design("Single numeric level demo") %>% 
  set_units(site = lvls(4)) %>% 
  serve_table()

## -----------------------------------------------------------------------------
demo2 <- demo %>% 
  set_units(plot = 72)

## -----------------------------------------------------------------------------
serve_table(demo2)

## -----------------------------------------------------------------------------
demo %>% 
  set_units(plot = nested_in(site, 18)) %>% 
  serve_table()

## -----------------------------------------------------------------------------
demo2 %>% 
  allot_units(site ~ plot) %>% 
  assign_units(order = "systematic-fastest") %>% 
  serve_table()

## -----------------------------------------------------------------------------
design("Crossed experiment") %>% 
  set_units(row = 6,
            col = 3,
            plot = crossed_by(row, col)) %>% 
  serve_table()

## -----------------------------------------------------------------------------
options(pillar.print_min = 20)

## -----------------------------------------------------------------------------
complex <- design("Complex structure") %>% 
  set_units(site = c("Narrabri", "Horsham", "Parkes", "Roseworthy"),
            col = nested_in(site, 6),
            row = nested_in(site, 3),
            plot = nested_in(site, crossed_by(row, col))) 

serve_table(complex)

## -----------------------------------------------------------------------------
serve_table(complex, label_nested = c(row, col))

## -----------------------------------------------------------------------------
complexd <- design("Complex structure with different dimensions") %>% 
  set_units(site = c("Narrabri", "Horsham", "Parkes", "Roseworthy"),
             col = nested_in(site, 
                      c("Narrabri", "Roseworthy") ~ 9,
                                                . ~ 6),
             row = nested_in(site, 3),
            plot = nested_in(site, crossed_by(row, col))) 

complextab <- serve_table(complexd, label_nested = everything())
table(complextab$site)

## -----------------------------------------------------------------------------
factrt <- design("Factorial treatment") %>% 
  set_trts(variety = c("a", "b"),
           fertilizer = c("A", "B"),
           amount = c(0.5, 1, 2)) 

## -----------------------------------------------------------------------------
trts_table(factrt)

## -----------------------------------------------------------------------------
factrtc <- design("Factorial treatment with control") %>% 
  set_trts(variety = c("a", "b"),
           fertilizer = c("none", "A", "B"),
           amount = conditioned_on(fertilizer,
                                    "none" ~ 0,
                                         . ~ c(0.5, 1, 2)))

## -----------------------------------------------------------------------------
trts_table(factrtc)

## -----------------------------------------------------------------------------
complexd + factrtc

## -----------------------------------------------------------------------------
alloted1 <- (complexd + factrtc) %>% 
  allot_trts(    fertilizer ~ row,
             amount:variety ~ plot)

## -----------------------------------------------------------------------------
design1 <- alloted1 %>% 
  assign_trts(order = c("systematic", "random"),
              seed = 2023) %>% 
  serve_table(label_nested = c(row, col))

## ----design2------------------------------------------------------------------
design2 <- (complexd + factrtc) %>% 
  allot_table(fertilizer ~ row,
                  amount ~ plot,
                 variety ~ plot, 
              order = c("systematic", "random", "random"),
              label_nested = c(row, col),
              seed = 2023)

## ----design3------------------------------------------------------------------
design3 <- alloted1 %>% 
  assign_trts(order = c("systematic", "random"),
              seed = 2023, 
              constrain = list(row = "site", plot = "row")) %>% 
  serve_table(label_nested = c(row, col))

## ----design2------------------------------------------------------------------
design2 <- (complexd + factrtc) %>% 
  allot_table(fertilizer ~ row,
                  amount ~ plot,
                 variety ~ plot, 
              order = c("systematic", "random", "random"),
              label_nested = c(row, col),
              seed = 2023)

## ----design3------------------------------------------------------------------
design3 <- alloted1 %>% 
  assign_trts(order = c("systematic", "random"),
              seed = 2023, 
              constrain = list(row = "site", plot = "row")) %>% 
  serve_table(label_nested = c(row, col))

