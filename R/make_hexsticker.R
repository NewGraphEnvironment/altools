library(hexSticker)
library(ggplot2)
library(ggimage)

logo <- "./man/figures/new graph circle.png"

##after making use default viewer to resize to 150 width
sticker(logo, package="duncan_ko", p_size=18, s_x=1, s_y=.75,
         s_width = 0.6,
         s_height = 0.75,
         h_fill = "black", p_y = 1.5, h_color = "#46C667",
         white_around_sticker = FALSE,
         filename="./man/figures/package_hexsticker.png")


