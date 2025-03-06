

############## SETUP CLEAN THEME
# Custom ggplot theme to make pretty plots
# Get the font at https://fonts.google.com/specimen/Barlow+Semi+Condensed
theme_clean <- function() {
  theme_minimal(base_family = "Barlow Semi Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(family = "BarlowSemiCondensed-Bold"),
          axis.title = element_text(family = "BarlowSemiCondensed-Medium"),
          strip.text = element_text(family = "BarlowSemiCondensed-Bold",
                                    size = rel(1), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA))
}

set_theme(base = theme_clean())



############## SETUP Colour Palettes
#https://www.r-bloggers.com/2022/06/custom-colour-palettes-for-ggplot2/

## list of color pallettes
my_colors = list(
  politics = c("#184aff","#5238bf", "#4f4a52" ,"#84649c", "#ff0000"),
  blackred = c("black","red"),
  greys = c("#707070","#999999","#C2C2C2"),
  greens = c("#ADC69D","#81A06D","#567E39","#2D5D16","#193E0A"),
  smallgreens = c("#ADC69D","#567E39","#193E0A"), ## MALE FEMALE OTHER
  olives = c("#CDCEA1","#B8B979","#A0A054","#78783F","#50502A","#35351C"),
  lightblues = c("#96C5D2","#61A2B2","#3C8093","#2C6378","#1F4A64"),
  darkblues = c("#7AAFE1","#3787D2","#2A73B7","#225E96","#1A4974","#133453"),
  reds = c("#D9B8BD","#CE98A2","#B17380","#954E5F","#78263E","#62151F"),
  traffic = c("#CE98A2","#81A06D","yellow"),
  questions = c("#B17380","#3787D2", "#567E39", "#EE897F"),
  tools= c("#D55662","#EE897F","#F5D0AD","#A0B79B","#499678","#2D363A"), #? ... design.....vis...... programming
  encounter = c("#8E8E8E","#729B7D"), ##SCROLL ENGAGE
  actions2 = c("#8E8E8E","#729B7D"),
  actions4 = c("#8E8E8E", "#A3A3A3","#729B7D","#499678"),
  actions3 = c("#8E8E8E","#99b898ff","#fdcea8ff"),
  actions = c("#8E8E8E","#2A363B","#99b898ff","#fdcea8ff","#ff837bff","#e84a60ff"),
  samples = c("#8B69B5", "#7AAFE1","#ADC69D","#66818e"), #tumblr, datacollar, bluecollar, general
  repeated = c("#2A363B","#C2C2C2"), #before/after
  platforms = c("#5D93EA","#FF70CD", "#3BD3F5", "#8B69B5","black"),
  amy_gradient =  c("#ac57aa", "#9e5fa4", "#90689f", "#827099", "#747894", "#66818e", "#578988", "#499183", "#3b997d", "#2da278", "#1faa72"),
  my_favourite_colours = c("#702963", "#637029",    "#296370")
)

## function for using palettes
my_palettes = function(name, n, all_palettes = my_colors, type = c("discrete","continuous"), direction = c("1","-1")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  out = switch(direction,
               "1" = out,
               "-1" = palette[n:1])
  structure(out, name = name, class = "palette")
}




##################### COMPOSITE DISTRIBUTION BLOCKS 

############## RETURNS SD STACKED AND COLORED BY BY X
## LOOP STYLE
multi_sd <- function (data, left, right, x, y, color) {
  
  # g <- ggplot(df, aes(y = .data[[x]], x = {{y}}, color = {{color}}))+
  g <- ggplot(data, aes(y = .data[[x]], x = .data[[y]], color = .data[[color]]))+
    geom_boxplot(width = 0.5) +
    geom_jitter(width = 0.1, alpha=0.5) +
    
    scale_y_continuous(limits=c(-1,101)) +
    labs(x="", y="") +
    coord_flip() +
    guides(
      y = guide_axis_manual(labels = left),
      y.sec = guide_axis_manual(labels = right)
    ) + theme_minimal()
  
  return(g)
}


############## RETURNS SINGLE SD 
## LOOP STYLE
single_sd <- function (data, left, right, x) {
  
  g <- ggplot(data, aes(y = {{x}}, x = ""))+
    geom_boxplot(width = 0.5) +
    geom_jitter(width = 0.1, alpha=0.5) +
    scale_y_continuous(limits=c(-1,101)) +
    labs(x="", y="") +
    coord_flip() +
    guides(
      y = guide_axis_manual(labels = left),
      y.sec = guide_axis_manual(labels = right)
    ) + theme_minimal()
  
  return(g)
}


# ######## RETURNS SINGLE SD
# ##  APPLY STYLE
plot_sd = function (data, column, type, mean, facet, facet_by, boxplot, labels) {
  
  ggplot(data, aes(y = .data[[column]], x="")) +
    {if(boxplot) geom_boxplot(width = 0.5) } +
    geom_jitter(width = 0.1, alpha=0.2, {if(facet) aes(color=.data[[facet_by]])}) +
    {if(mean)
      stat_summary(fun="mean", geom="point", shape=20, size=3, color="blue", fill="blue")
    } +
    {if(mean)
      ## assumes data has been passed in with mean column at m
      # stat_summary(fun="mean", geom="text", colour="blue",  fontface = "bold",
      #            vjust=-1.25, hjust = 0.50, aes( label=round(..y.., digits=0)))
      stat_summary(fun="mean", geom="text", colour="blue",  fontface = "bold",
                   vjust=-1.25, hjust = 0.50, aes( label=round(..y.., digits=0)))
    } +
    
    {if(facet) facet_grid(.data[[facet_by]] ~ .)} +
    # scale_y_continuous(limits=c(-1,101)) +
    labs(x="", y="") +
    coord_flip()  +
    {if(type == "S")
      guides(
        y = guide_axis_manual(labels = labels[column,"left"]),
        y.sec = guide_axis_manual(labels = labels[column,"right"])
      )} +
    {if(type == "Q")
      guides(
        y = guide_axis_manual(labels = labels[q,"left"]),
        y.sec = guide_axis_manual(labels = labels[q,"right"])
      )} +
    theme_minimal()  +
    labs (
      subtitle = column
      #caption = column
    ) + easy_remove_legend()
}
