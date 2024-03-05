# Este script descarga las tasas de interés activas y pasivas en peso y USD
# para República Dominicana

get_tasas <- function(frecuencia = "diaria", entidad = "BM"){
  checkmate::assert_choice(
    frecuencia,
    choices = c("diaria", "mensual", "trimestral", "anual"))

    checkmate::assert_choice(
      entidad,
      choices = c("BM", "AAYP", "BAC", "CDC"))

    # algo que dependiendo del tipo de tasa que pida, ejecute cada función

}




get_tasa_activa_RD <- function(frecuencia = "diaria", entidad = "BM") {
  checkmate::assert_choice(
    frecuencia,
    choices = c("diaria", "mensual", "trimestral", "anual"))


  years <- switch(entidad,
                   "BM" = 2007:lubridate::year(Sys.Date()),
                   "AAYP" = 2000:lubridate::year(Sys.Date()),
                   "BAC" = 2000:lubridate::year(Sys.Date()),
                   "CDC" = 2018:lubridate::year(Sys.Date()),
  )

  ext_BM <- ifelse(years >= 2007 & years <= 2015, ".xls", ".xlsx")


  # names_act_RD <- c(
  #   "periodo", "0-90d", "91-180d", "181-360d", "2-5a", "5+a", "prom_ponderado",
  #   "prom_simple", "comercio", "consumo_yo_personal", "hip_yo_desarrollo",
  #   "pref_prom_ponderado", "pref_comercio", "pref_consumo_yo_personal",
  #   "pref_hip_yo_desarrollo")


  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas",
    "/sector-monetario-y-financiero/documents/tasas_diarias",
    entidad, "-", years, ext_BM
  )

  files_path <- tempfile(pattern = as.character(years), fileext = ext_BM)

  save_download <- purrr::possibly(utils::download.file, otherwise = NA) # nolint

  purrr::walk2(
    url_descarga,
    files_path,
    \(url, file) save_download(url, file, mode = "wb", quiet = TRUE)
  ) |> suppressWarnings()

  files_path <- files_path[file.exists(files_path)]

  suppressMessages(
    suppressWarnings(
      tasa_activa_rd <- purrr::map(
        files_path,
        readxl::read_excel,
        sheet = "ACTRD$",
        col_names = FALSE, skip = 9, na = "n.d.")  #|>
        #stats::setNames(names_act_RD)
    )
  )

  tasa_activa_rd1 <- tasa_activa_rd |>
    purrr::map(
      ~.x |>
        janitor::clean_names() |>
        dplyr::mutate(yr = ifelse(grepl("^\\d{4}$", x1), as.numeric(x1), as.numeric(stringr::str_extract(x1, "\\b\\d{4}\\b"))),
                      mn = ifelse(grepl("^[A-Za-záéíóúÁÉÍÓÚ]+$", x1), x1, NA)
                      ) |>
        tidyr::fill(yr, mn)
        ) |>
    dplyr::bind_rows(.id = "year") |>
    dplyr::mutate(mes = crear_mes(mes,
                                  type = "text_to_number"),
                  fecha = lubridate::make_date(year, mes, "1"),
                  trimestre = lubridate::quarter(fecha, with_year = TRUE))



}
