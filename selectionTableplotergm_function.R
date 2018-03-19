#' @title selectionTable.plot.ergm
#'
#' @description Creates selection table plot
#' @param net network object
#' @param model ERGM model
#' @param vname Attribute Name
#' @param effNames Effect names
#' @param name Network name to be used in plot
#' @param levls Levels
#' @param levls.alt Alter Levels (by defult the same as the levls/ego levels)
#' @param quad  TRUE/FALSE - default is TRUE (plotting the quadratic function)
#' @param separation separation - 0 by default, but can be adjusted if curves overlap too much (0.1 or 0.2)
#' @export
#' @return Selection Table plot

selectionTable.plot.ergm <- function(net, model,vname, effNames,name,
                                     levls, levls.alt=levls,
                                     quad=TRUE, separation=0){
  ST<- selectionTable.basis.ergm(net, model,vname, effNames,levls, levls.alt)
  STdf<-ST$df
  STdf$ego <- as.character(STdf$vego)
  STdf$valter <- as.numeric(as.character(STdf$valter))
  STdf$select <- as.numeric(as.character(STdf$select))
  vselect<-STdf
  vselect$select <- vselect$select + separation*as.numeric(vselect$ego)
  sp <- ggplot(vselect, aes(valter, select, group=ego, colour=ego))
  Lvname<-tolower(vname)
  if (quad) {
    gs <- geom_smooth(size=1.2, span=3)
  } else {
    gs <- geom_line(size=1.2)
  }
  sp + geom_point() + gs + scale_colour_hue() +
    scale_x_continuous(breaks=levls.alt) +
    theme(legend.key=element_blank())+
    labs(x=paste(Lvname),
         y=paste('selection function'),
         title=paste('Effect',Lvname,'on',name),
         colour=paste(Lvname)) +
    theme_grey(base_size=14, base_family="") +
    theme(legend.key.width=unit(1, "cm")) +
    theme(plot.title=element_text(hjust=0.5))
}


