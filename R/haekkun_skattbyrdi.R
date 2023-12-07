library(tidyverse)
library(hagstofa)
library(metill)
library(ggtext)
library(geomtextpath)

theme_set(theme_metill())


url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Samfelag/launogtekjur/3_tekjur/1_tekjur_skattframtol/TEK01002.px"



d <- hg_data(
  url
) |> 
  filter(
    `Tekjur og skattar` == "Skattar á greiðslugrunni",
    Eining == "Miðgildi - skilyrt",
    Kyn == "Allir",
    Ár %in% c("2014", "2022")
  ) |> 
  collect()


p <- d |> 
  janitor::clean_names() |> 
  select(
    sveitarfelag, ar, skattar = 6
  ) |> 
  pivot_wider(names_from = ar, values_from = skattar) |> 
  rename(
    pre = 2, post = 3
  ) |> 
  mutate(
    diff = post - pre
  ) |> 
  drop_na() |> 
  mutate(
    diff = diff * 1e3,
    color = if_else(sveitarfelag == "Reykjavík", "#08519c", "#636363"),
    linewidth = if_else(sveitarfelag == "Reykjavík", 1, 0.5),
    size = if_else(sveitarfelag == "Reykjavík", 4, 2),
    alpha = if_else(sveitarfelag == "Reykjavík", 1, 0.5),
    sveitarfelag = fct_reorder(sveitarfelag, diff)
  ) |> 
  ggplot(aes(diff, sveitarfelag)) +
  geom_labelvline(
    data = ~filter(.x, sveitarfelag == "Alls"),
    aes(xintercept = diff, label = "Á landsvísu"),
    lty = 2,
    alpha = 0.5,
    linewidth = 0.5
  ) +
  geom_labelvline(
    x = 627000,
    label = "Tala Hildar Björnsdóttur",
    hjust = 0.1
  ) +
  geom_point(
    data = ~filter(.x, sveitarfelag != "Alls"),
    aes(color = color, size = size)
  ) +
  geom_segment(
    data = ~filter(.x, sveitarfelag != "Alls"),
    aes(xend = 0, yend = sveitarfelag, color = color, linewidth = linewidth, alpha = alpha)
  ) +
  scale_x_continuous(
    expand = expansion(),
    labels = label_isk(scale = 1e-3),
    breaks = c(0, 250, 500, 627, 750) * 1000
  ) +
  scale_y_discrete(
    labels = function(x) {
      if_else(
        x == "Reykjavík",
        "<b style='color:#08519c;font-size:15px;'>Reykjavík</b>",
        x
      )
    }
  ) +
  scale_color_identity() +
  scale_linewidth_identity() +
  scale_size_identity() +
  scale_alpha_identity() +
  labs(
    x = NULL,
    y = NULL,
    title = "Hækkun skattbyrði frá 2014 til 2022 eftir sveitarfélagi",
    caption = str_c(
      "Gögn Hagstofu: https://px.hagstofa.is/pxis/pxweb/is/Samfelag/Samfelag__launogtekjur__3_tekjur__1_tekjur_skattframtol/TEK01002.px",
      "\n",
      "Kóði: "
    )
  ) +
  theme(
    axis.text.y = element_markdown()
  )

ggsave(
  plot = p,
  filename = "Figures/mynd.png",
  width = 8, height = 0.621 * 8, scale = 1.8
)