# list of colours used in the umar palette.

umar_colours <- c(`rdeca` = rgb(161, 3, 5, maxColorValue = 255),
                  `roza` = rgb(212, 101, 101, maxColorValue = 255),
                  `siva` = rgb(167, 174, 180, maxColorValue = 255),
                  `temno_modra` = rgb(52, 61, 88, maxColorValue = 255),
                  `turkizna` = rgb(23, 111, 139, maxColorValue = 255),
                  `zelena` = rgb(152, 197, 118, maxColorValue = 255),
                  `vijolicna` = rgb(50, 20, 67, maxColorValue = 255),
                  `sinja` = rgb(102, 138, 182, maxColorValue = 255),
                  `gridlines` = rgb(191, 191, 191, maxColorValue = 255),
                  `emph` = rgb(80, 87, 94, maxColorValue = 255))
usethis::use_data(umar_colours, internal = TRUE, overwrite = TRUE)

# lookup for unit translation
unit_lookup <- structure(list(id = c(1L, 2L, 3L, 4L, 8L, 14L, 18L, 31L, 32L,
                      33L, 34L, 36L, 37L, 38L, 39L, 40L, 52L, 53L, 58L, 62L),
               name = c("1000",
                        "mio eur", "%", "odstotne to\u010dke", "indeks", "\u0161tevilo",
                        "ravnote\u017eje v odstotnih to\u010dkah",
                        "eur", "osebe (1000)", "opravljene delovne ure (1000)", "1000 toe",
                        "toe/mio eur 2010", "mwh/mio eur 2010", "toe/preb.", "kwh/preb.",
                        "t/toe", "m2", "m3", "% od bdp", "predhodno leto=100"),
               name_en = c("1000",
                           "Million EUR", "%", "Percentage points", "Index", "Number", "Balance in percentage points",
                           "EUR", "Persons (1000)", "Hours worked (1000)", "1000 toe", "toe/million EUR 2010",
                           "mWh/million EUR 2010", "toe/cap.", "kWh/cap.", "t/toe", "m2",
                           "m3", "% of GDP", "Previous year = 100")),
          row.names = c(NA,
                        -20L), class = c("tbl_df", "tbl", "data.frame"))

usethis::use_data(umar_colours, unit_lookup, internal = TRUE, overwrite = TRUE)

