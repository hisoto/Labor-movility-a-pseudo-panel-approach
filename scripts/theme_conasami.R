theme_conasami <- function(
    # Tamaños de texto
  base_size = 10,
  title_size = base_size + 2,
  subtitle_size = base_size + 1,
  caption_size = base_size - 1,
  axis_text_size = base_size,
  axis_title_size = base_size,
  legend_text_size = base_size,
  strip_text_size = base_size,
  
  # Colores
  strip_fill = "grey",
  grid_color = '#F0F0F0FF',
  border_color = "#222831",
  axis_title_color = "black",
  
  # Espaciados
  panel_spacing = 1,
  plot_margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
  
  # Otros
  axis_text_angle = 0,
  legend_position = "bottom",
  axis_title_face = "plain"
) {
  
  theme(
    # Configuraciones generales
    text = element_text(family = "Noto Sans", size = base_size),
    plot.background = element_rect(fill = 'transparent', color = NA),
    
    # Configuración de paneles
    panel.background = element_rect(fill = 'white'),
    panel.border = element_rect(colour = border_color, linewidth = 0.5, fill = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = grid_color, linewidth = 0.5),
    panel.spacing = unit(panel_spacing, "lines"),
    
    # Configuración de ejes
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    axis.text = element_text(colour = "black", size = axis_text_size),
    axis.text.x = element_text(angle = axis_text_angle, vjust = 0.5),
    axis.text.y = element_text(hjust = 0.5),
    axis.title = element_text(size = axis_title_size, color = axis_title_color, face = axis_title_face),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10), angle = 90),
    
    # Configuración de leyenda
    legend.position = legend_position,
    legend.background = element_rect(fill = 'transparent', color = NA),
    legend.key = element_blank(),
    legend.text = element_text(size = legend_text_size),
    
    # Configuración de facetas
    strip.background = element_rect(fill = strip_fill, color = "black"),
    strip.text = element_text(face = "bold", size = strip_text_size),
    
    # Configuración del plot
    plot.margin = plot_margin,
    plot.title = element_text(
      hjust = 0.5, 
      face = "bold", 
      size = title_size,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, 
      face = "bold", 
      size = subtitle_size,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      hjust = 0.5, 
      size = caption_size,
      margin = margin(t = 10))
  )
}

library(extrafont)
# font_import()
# y
loadfonts(device = "win")




