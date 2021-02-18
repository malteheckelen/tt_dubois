library(tidyverse)
library(magick)
library(cowplot)
library(grid)
library(gridExtra)

tuesdata <- tidytuesdayR::tt_load(2021, week = 8)
georgia_pop <- tuesdata$georgia_pop

breaks_years <- unique(georgia_pop$Year)
breaks_percent <- seq(0,100,5)
pop_plot <- georgia_pop %>%
  gather(white_colored, percent_increase, Colored, White) %>%
  ggplot(aes(y=percent_increase, x=Year)) +
  geom_line(aes(lty=white_colored)) +
  scale_linetype_manual(values=c('solid', 'longdash')) +
  scale_x_continuous(expand = c(0,0), breaks=breaks_years) +
  scale_y_reverse(breaks=breaks_percent, expand=c(0,0)) +
  coord_flip() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(family = 'Courier', size = 20),
        legend.position = 'none',
        panel.grid = element_line(alpha('red', alpha=.3)),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill='transparent'),
        panel.border=element_rect('black', fill='transparent'),
        plot.background = element_rect(colour=NA,fill='transparent')
        )

img <- image_read("images/background.png") 
img <- image_modulate(img, brightness = 130)
img_path <- image_read("images/path.png")
png('out.png', width=1024, height=1287)
ggdraw() +
  draw_image(img) +
  draw_image(img_path,
             width = .56,
             x=.25,
             y=-.36) +
  draw_text('COMPARATIVE INCREASE OF WHITE AND COLORED\nPOPULATION OF GEORGIA.',
            fontface='bold',
            size=30,
            y=.925) +
  draw_text(text = '_   _   _   _', 
            fontface='bold',
            x=.75, 
            y=.1) +
  draw_text(text = 'WHITE = ', 
            size=10,
            x=.69, 
            y=.095) +
  draw_text(text = '= COLORED', 
            size=10,
            x=.38, 
            y=.095) +
  draw_text(text = '__________', 
            fontface='bold',
            x=.3, 
            y=.1) +
  draw_text(text = 'PERCENTS', 
            size=10,
            x=.53, 
            y=.11) +
  draw_plot(plot_grid(NA,NA,NA,
                      NA,pop_plot,NA,
                      NA,NA,NA,
                      ncol=3,
                      rel_widths=c(.2,.6,.2),
                      rel_heights=c(.15,.7,.15)))
dev.off()

