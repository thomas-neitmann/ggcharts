library(ggcharts)
library(dplyr)
library(hexSticker)
library(showtext)
font_add_google("Fira Sans")

chart <- biomedicalrevenue %>%
  filter(year == 2018) %>%
  column_chart(company, revenue, top_n = 5) +
  scale_y_continuous(
    breaks = c(0, 25, 50, 75),
    expand = expansion(c(0, .05))
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_line(size = .2),
    panel.grid.major.y = element_line(size = .1),
    plot.background = element_blank()
  )

hex <- sticker(
  chart,
  package = "ggcharts",
  p_family = "Fira Sans",
  l_y = 1.1,
  p_size = 20,
  p_color = "#002451",
  h_fill = "#F4F7FC",
  h_color = "#002451",
  s_x = 1,
  s_y = .86,
  s_width = 1.4,
  s_height = 1,
  url = "Created by Thomas Neitmann",
  u_size = 3,
  u_color = "#002451",
  filename = paste0("man/figures/ggcharts_hex.png")
)

png(
  filename = "man/figures/ggcharts_hex.png",
  width = 43.9,
  height = 50.8,
  bg = "transparent",
  units = "mm",
  type = "cairo-png",
  antialias = "subpixel",
  res = 300
)
print(hex)
dev.off()
