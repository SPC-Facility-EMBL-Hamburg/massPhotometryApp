packages <- c(
  "shinydashboard","shinycssloaders","plotly","shinyalert",
  "reticulate","reshape2","rhandsontable","colourpicker"
)

invisible(lapply(packages, library, character.only = TRUE))

appName     <- "PhotoMol"
user        <- Sys.info()['user']

reticulate::use_python(paste0("/home/",user,"/myenv/bin/python"), required = TRUE)

# developer path
base_dir <- paste0("/home/",user,"/spc_shiny_servers/massPhotometryApp/appFiles/",appName,"/")

# path for the docker user
if (user == 'shiny') {
  base_dir <- paste0("/home/shiny/",appName,'/')
}

cstFactorForContrast <- 1e3 # Contrasts will be multiplied by this value

myrenderer <- "function(instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.TextRenderer.apply(this, arguments);
                if (instance.params) {
                    hcols = instance.params.col_highlight
                    hcols = hcols instanceof Array ? hcols : [hcols]
                    hrows = instance.params.row_highlight
                    hrows = hrows instanceof Array ? hrows : [hrows]
                    
                    for (i = 0; i < hcols.length; i++) { 
                        if (hcols[i] == col && hrows[i] == row) {
                            td.style.background = instance.getDataAtCell(row, col);
                        }
                    }
                }
  }"     

myrendererBoolean <- "function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.CheckboxRenderer.apply(this, arguments);}"


