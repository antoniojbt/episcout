#' @title Extraer datos del CURP en Mexico
#' @description Esta funcion descompone un CURP en sus componentes principales segun las posiciones definidas en su estructura.
#' @param curp Un vector de texto que contiene uno o mas CURPs. Cada CURP debe tener exactamente 18 caracteres.
#' @return Un `tibble` con los componentes del CURP organizados en columnas:
#'   \itemize{
#'     \item \code{CURP}: El CURP original.
#'     \item \code{PrimeraLetraApellidoPaterno}: Primera letra del apellido paterno.
#'     \item \code{PrimeraVocalApellidoPaterno}: Primera vocal del apellido paterno.
#'     \item \code{PrimeraLetraApellidoMaterno}: Primera letra del apellido materno.
#'     \item \code{PrimeraLetraNombre}: Primera letra del nombre.
#'     \item \code{AnoNacimiento}: Ano completo de nacimiento (e.g., "1990", "2002").
#'     \item \code{MesNacimiento}: Mes de nacimiento (2 digitos).
#'     \item \code{DiaNacimiento}: Dia de nacimiento (2 digitos).
#'     \item \code{Sexo}: Genero registrado (H para hombre, M para mujer).
#'     \item \code{EntidadFederativa}: Entidad federativa de nacimiento (codigo de 2 letras).
#'     \item \code{PrimerasConsonantes}: Consonantes internas (3 caracteres).
#'     \item \code{Homoclave}: Caracter alfanumerico asignado para evitar duplicados.
#'     \item \code{DigitoVerificador}: Digito para validacion final.
#'   }
#' @examples
#' # Ejemplo de uso con un solo CURP
#' epi_clean_curp("GOMC900514HDFRLA07")
#'
#' # Ejemplo con varios CURPs
#' curps <- c(
#' "GOMC900514HDFRLA07",  # Antes del 2000
#' "LOAM020715MMCRSR09"   # Despues del 2000
#' )
#'
#' # Aplicar la funcion a un vector de CURPs
#' resultados <- purrr::map_dfr(curps, epi_clean_curp)
#'
#' # Mostrar resultados
#' print(t(resultados))
#'
#' @importFrom tibble tibble
#' @export

# Funcion para extraer datos del CURP usando posiciones
epi_clean_curp <- function(curp) {
  if (nchar(curp) != 18) stop("El CURP debe tener exactamente 18 caracteres.")

# Aplicar extraccion por posiciones para cada CURP
  resultados <- lapply(curp, function(c) {
    tibble::tibble(
      CURP = c,
      PrimeraLetraApellidoPaterno = substr(c, 1, 1),
      PrimeraVocalApellidoPaterno = substr(c, 2, 2),
      PrimeraLetraApellidoMaterno = substr(c, 3, 3),
      PrimeraLetraNombre = substr(c, 4, 4),
      AnoNacimiento = ifelse(as.numeric(substr(c, 5, 6)) <= 22,
                             paste0("20", substr(c, 5, 6)),
                             paste0("19", substr(c, 5, 6))),
      MesNacimiento = substr(c, 7, 8),
      DiaNacimiento = substr(c, 9, 10),
      Sexo = substr(c, 11, 11),
      EntidadFederativa = substr(c, 12, 13),
      PrimerasConsonantes = substr(c, 14, 16),
      Homoclave = substr(c, 17, 17),
      DigitoVerificador = substr(c, 18, 18)
    )
  })

  # Combinar los resultados en un unico tibble
  do.call(rbind, resultados)
}

# La primera letra del primer apellido y la primera vocal del primer apellido.
# Primera letra del Segundo apellido.
# Primera letra del primer nombre (Excepto los nombres compuestos cuando estos se  antepongan los nombre de Jose y Maria) Ejemplo: si tu nombre es Maria Isabel, se utilizara la I del segundo nombre.
# Los siguientes 6 digitos corresponden a tu fecha de nacimiento que aparece en tu acta de nacimiento, se comienza desde el ano, mes y dia.
# Letra del Genero de la persona, H hombres y M mujeres.
# Los 2 digitos de tu Entidad de Nacimiento, aparecen en tu acta de nacimiento. Recuerda no confundirla con la entidad de tu registro. Si naciste en otro pais se debe de poner NE (nacido en el extranjero).
#
# Siguiente consonante del primer apellido.
# Primera consonante interna del segundo apellido .
# Primera consonante interna del primer nombre.
# Homoclave 2 ultimos digitos para evitar duplicaciones.
#
