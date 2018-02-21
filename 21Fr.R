# 21 F año 2018
Sys.setlocale(locale = "es_ES.UTF-8")

pqkgs <- c("rio", "magrittr", "stringr", "dplyr", "plyr",  "repmis", "knitr", "rgdal", 
           "highcharter", "geojsonio", "spdplyr", "rmapshaper", "kableExtra", "knitr", "tidyverse")

lapply(pqkgs, function(x) require(x, character.only = TRUE))

bases <- list.files(recursive = T)
bases <- bases[!grepl(".Rproj|.geojson|.R|.html|.Rmd", bases)]
datos <- list()

for(i in 1:length(bases)) {
  datos[[i]] <- rio::import(bases[i])
}

vars <- c("PAÍS", "SI", "NO", "CODIGO", "DEPARTAMENTO", "MUNICIPIO", "MAS-IPSP", 
          "VÁLIDOS", "EMITIDOS", "INSCRITOS HABILITADOS")

for(i in 1:length(datos)) {
  datos[[i]] %<>% select(one_of(vars)) 
}

for(i in c(1,3)) {
  datos[[i]] %<>% select(-PAÍS) %>% 
    group_by(CODIGO, DEPARTAMENTO, MUNICIPIO) %>% 
    summarise_all(funs(sum)) %>% 
    mutate(MAS = round(`MAS-IPSP`/VÁLIDOS*100, 2))
}

datos[[5]] %<>% select(-PAÍS) %>% 
  group_by(CODIGO, DEPARTAMENTO, MUNICIPIO) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(MAS = round(SI/VÁLIDOS*100, 2))

datos[[2]] %<>% group_by(PAÍS) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(MAS = round(`MAS-IPSP`/VÁLIDOS*100, 2))

datos[[4]] %<>% group_by(PAÍS) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(MAS = round(SI/VÁLIDOS*100, 2),
         NO.VOTARON = `INSCRITOS HABILITADOS` - EMITIDOS,
         NO.VOTARON.PROP = round(prop.table(NO.VOTARON)*100), 2)

# CREAR VARIABLES
datos[[3]]$CAIDA <- datos[[5]]$MAS - datos[[3]]$MAS
datos[[4]]$CAIDA <- datos[[4]]$MAS - datos[[2]]$MAS
datos[[4]]$CAIDA.EMITIDOS.CIFRA <- datos[[4]]$EMITIDOS - datos[[2]]$EMITIDOS

# GRAFICOS  EXTERIOR

#emitidos
exterior1 <- highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Voto en el exterior") %>% 
  hc_subtitle(text = "33 países") %>% 
  hc_xAxis(categories = c("2014:Elección Nacional",
                          "2016: Referendo para habilitar a Evo Morales como candidato")) %>% 
  hc_add_series(data = c(sum(datos[[2]]$`INSCRITOS HABILITADOS`), 
                         sum(datos[[4]]$`INSCRITOS HABILITADOS`)),
                name = "Inscritos para votar",
                showInLegend = T)  %>%
  hc_add_series(data = c(sum(datos[[2]]$EMITIDOS), 
                         sum(datos[[4]]$EMITIDOS)),
                name = "Votos emitidos",
                showInLegend = T)  %>%
  hc_exporting(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_smpl()) 

# porcentaje que no fueron a votar
myhc_add_series_labels_values <- function (hc, labels, values, text, colors = NULL, ...) 
{
  assertthat::assert_that(is.highchart(hc), is.numeric(values), 
                          length(labels) == length(values))
  df <- dplyr::data_frame(name = labels, y = values, text=text)
  if (!is.null(colors)) {
    assert_that(length(labels) == length(colors))
    df <- mutate(df, color = colors)
  }
  ds <- list_parse(df)
  hc <- hc %>% hc_add_series(data = ds, ...)
  hc
}

exterior2 <- highchart() %>% 
  hc_chart(type = "pie", data=datos[[4]]) %>% 
  myhc_add_series_labels_values(labels=datos[[4]]$PAÍS, values=datos[[4]]$NO.VOTARON, 
                                text=datos[[4]]$NO.VOTARON) %>% 
  hc_tooltip(crosshairs=TRUE, borderWidth=5, sort=TRUE, shared=TRUE, table=TRUE,
             pointFormat=paste('<br><b>Proporción sobre los que no votaron: {point.percentage:.1f}%</b><br>No emitieron su voto estando inscritos: {point.text}')) %>%
  hc_title(text="Voto en el exterior: quienes no fueron a votar", 
           margin=20, style=list(color="#144746", useHTML=TRUE))  %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_smpl())


# caida de añpoyo en el exterior
exterior3 <- highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Porcentaje de apoyo a Evo Morales") %>% 
  hc_subtitle(text = "Caída de 21%") %>% 
  hc_xAxis(categories = c("2014:Elección Nacional",
                          "2016: Referendo para habilitarlo como candidato")) %>% 
  hc_add_series(data = c(round(sum(datos[[2]]$`MAS-IPSP`)/sum(datos[[2]]$VÁLIDOS)*100, 2),
                round(sum(datos[[4]]$SI)/sum(datos[[4]]$VÁLIDOS)*100, 2)),
                name = "% de apoyo",
                showInLegend = F)  %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_colors("green")


# caida por paises
datos[[4]] %<>% arrange(CAIDA)

exterior4 <- highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Perdida de apoyo en el referendo de 2016 respecto a la elección de 2014 (%)") %>% 
  hc_subtitle(text = "Pérdida en 31 países, estancamiento en 1, mejora en 1") %>% 
  hc_xAxis(categories = datos[[4]]$PAÍS) %>% 
  hc_add_series(data = datos[[4]]$CAIDA,
                name = "% de ganancia o pérdida de apoyo",
                showInLegend = F)  %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_colors("red") 


# VOTO EN BOLIVIA
mapa <- jsonlite::fromJSON("municipios.339.geojson", 
                           simplifyVector = F)

a <- datos[[3]] %>% select(CODIGO, CAIDA) %>% 
  mutate(AÑO = "2016")

b <- datos[[5]] %>% select(CODIGO, CAIDA = MAS) %>% 
  mutate(AÑO = "2014") 

b <- rbind(a, b)
b$DEPARTAMENTO <- NULL


ds1 <- b %>% 
  group_by(CODIGO) %>% 
  do(item = list(
    CODIGO = first(.$CODIGO),
    sequence = .$CAIDA,
    value = first(.$CAIDA))) %>% 
  .$item

# perdida gradual
hc1 <- highchart(type = "map") %>% 
  hc_add_series(data = ds1,
                name = "Magnitud de la perdida de apoyo",
                mapData = mapa,
                joinBy = "CODIGO",
                borderWidth = 0.01,
                tooltip = list(valueDecimals = 2, valueSuffix = "%")) %>% 
  hc_colorAxis(dataClasses = color_classes(c(seq(-42, 0, by = 10), 0, 13))) %>%
  hc_legend(layout = "vertical", reversed = TRUE,
            floating = TRUE, align = "right", verticalAlign = "top") %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_title(text = "Magnitud de la pérdida de apoyo en los 339 municipios") %>% 
  hc_subtitle(text = "Referendo reelección 2016 vs. elección general 2014")  %>% 
  hc_credits(text = "rafa.lopez.v")
  
  
hc <- highchart(type = "map") %>% 
    hc_add_series(data = ds1,
                  name = "Magnitud de la perdida de apoyo",
                  mapData = mapa,
                  joinBy = "CODIGO",
                  borderWidth = 0.01,
                  tooltip = list(valueDecimals = 2, valueSuffix = "%")) %>% 
    hc_colorAxis(dataClasses = color_classes(c(seq(-42, 0, by = 42), 13))) %>%
    hc_legend(layout = "vertical", reversed = TRUE,
              floating = TRUE, align = "right", verticalAlign = "bottom") %>% 
    hc_add_theme(hc_theme_smpl()) %>% 
  hc_title(text = "En 324 de 339 municipios (95%) disminuye el apoyo a Evo Morales") %>% 
    hc_subtitle(text = "Referendo reelección 2016 vs. elección general 2014")  %>% 
  hc_credits(text = "rafa.lopez.v")

# mapa de caida en el alto y compañia




htmlwidgets::saveWidget(exterior1, file="exterior1.html")
htmlwidgets::saveWidget(exterior2, file="exterior2.html")
htmlwidgets::saveWidget(exterior3, file="exterior3.html")
htmlwidgets::saveWidget(exterior4, file="exterior4.html")
htmlwidgets::saveWidget(hc, file="hc.html")
htmlwidgets::saveWidget(hc1, file="hc1.html")
