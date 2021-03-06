# serve

    Code
      design(name = "unlinked units with table") %>% set_units(block = 3, plot = 2) %>%
        serve_table()
    Output
      # unlinked units with table 
      # An edibble: 0 x 2
      # ... with 2 variables: block <unit(3)>, plot <unit(2)>

---

    Code
      design(name = "one unit") %>% set_units(block = 3) %>% serve_table()
    Output
      # one unit 
      # An edibble: 3 x 1
            block
        <unit(3)>
      1    block1
      2    block2
      3    block3

---

    Code
      design(name = "serve nested units") %>% set_units(block = 3, plot = nested_in(
        block, 2)) %>% serve_table()
    Output
      # serve nested units 
      # An edibble: 6 x 2
            block      plot
        <unit(3)> <unit(6)>
      1    block1     plot1
      2    block1     plot2
      3    block2     plot3
      4    block2     plot4
      5    block3     plot5
      6    block3     plot6

---

    Code
      design() %>% set_trts(vaccine = c("AZ", "M", "P")) %>% serve_table()
    Output
      # An edibble: 3 x 1
         vaccine
        <trt(3)>
      1       AZ
      2       M 
      3       P 

---

    Code
      design() %>% set_units(site = 2, row = nested_in(site, 1 ~ 3, 2 ~ 2), col = nested_in(
        site, 1 ~ 3, 2 ~ 2), plot = nested_in(site, ~ row:col)) %>% set_trts(trt = c(
        "A", "B")) %>% allot_table(trt ~ plot, seed = 1)
    Output
      # An edibble: 13 x 5
              site       row       col       plot      trt
         <unit(2)> <unit(5)> <unit(5)> <unit(13)> <trt(2)>
       1     site1      row1      col1     plot1         A
       2     site1      row2      col1     plot2         B
       3     site1      row3      col1     plot3         A
       4     site1      row1      col2     plot4         B
       5     site1      row2      col2     plot5         A
       6     site1      row3      col2     plot6         B
       7     site1      row1      col3     plot7         A
       8     site1      row2      col3     plot8         B
       9     site1      row3      col3     plot9         A
      10     site2      row4      col4     plot10        A
      11     site2      row5      col4     plot11        B
      12     site2      row4      col5     plot12        B
      13     site2      row5      col5     plot13        A

