workrate <- function(worktable){
  # Attitude and work
  worktable %>%
    select(rank, xG, deep, CP, ppda) %>%
    mutate(active=deep/CP*100,
           ppda=-ppda) %>%
    ggplot() +
    geom_circle(aes(x0 = active, y0 = ppda/4, r = xG/1000, fill=xG, alpha=1), show.legend=TRUE) +
    scale_fill_gradient2(low='red', mid='white', high='steelblue', midpoint=mean(worktable$xG)) +
    geom_text(aes(active, ppda/4, label=rank), size=3) +
    theme_minimal() +
    labs(title='Directness, activeness, workrate',
         x='Active Possession',
         y='Passes per Defensive Action') +
    theme(legend.position='right',
          axis.title=element_text(size=16),
          plot.title=element_text(vjust=2.1, size=15),
          axis.text=element_blank(),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10)) +
    guides(alpha=FALSE) +
    scale_x_continuous(position = 'top') +
    geom_segment(aes(x=min(active)-0.3, xend=max(active)+0.3, y=max(ppda/4)+0.3, yend=max(ppda/4)+0.3), size=1,
                 arrow = arrow(length = unit(0.6,"cm")))  +
    geom_segment(aes(x=min(active)-0.3, xend=min(active)-0.3, y=max(ppda/4)+0.3, yend=min(ppda/4)-0.1), size=1,
                 arrow = arrow(length = unit(0.6,"cm")))
  }