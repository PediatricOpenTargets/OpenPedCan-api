####################################
# add data table options
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

viewDataTable <- function(dat, pageLength){
  DT::datatable(dat,
                extensions = c('Buttons'),
                selection = "single",
                filter = "bottom",
                options = list(#scrollY = "400px",
                               dom = 'Bfrtip',
                               buttons = list('colvis','pageLength',
                                              list(extend = "collection", 
                                                   buttons = c('csv'), 
                                                   text = 'Download')),
                               pageLength = pageLength,
                               searchHighlight = TRUE,
                               lengthMenu = list(c(5, 10, 15, 20, 25, -1), c('5', '10', '15', '20', '25', 'All')),
                               initComplete = JS("function(settings, json) {",
                                                 "$(this.api().table().header()).css({'background-color': '#005ab3', 'color': '#fff'});",
                                                 "}"),
                               scrollX = TRUE)
                )
}