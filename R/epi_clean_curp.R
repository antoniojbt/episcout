#' @title Extraer datos del CURP en México
#' @description Esta función descompone un CURP en sus componentes principales según las posiciones definidas en su estructura.
#' @param curp Un vector de texto que contiene uno o más CURPs. Cada CURP debe tener exactamente 18 caracteres.
#' @return Un `tibble` con los componentes del CURP organizados en columnas:
#'   \itemize{
#'     \item \code{CURP}: El CURP original.
#'     \item \code{PrimeraLetraApellidoPaterno}: Primera letra del apellido paterno.
#'     \item \code{PrimeraVocalApellidoPaterno}: Primera vocal del apellido paterno.
#'     \item \code{PrimeraLetraApellidoMaterno}: Primera letra del apellido materno.
#'     \item \code{PrimeraLetraNombre}: Primera letra del nombre.
#'     \item \code{AñoNacimiento}: Año completo de nacimiento (e.g., "1990", "2002").
#'     \item \code{MesNacimiento}: Mes de nacimiento (2 dígitos).
#'     \item \code{DiaNacimiento}: Día de nacimiento (2 dígitos).
#'     \item \code{Sexo}: Género registrado (H para hombre, M para mujer).
#'     \item \code{EntidadFederativa}: Entidad federativa de nacimiento (código de 2 letras).
#'     \item \code{PrimerasConsonantes}: Consonantes internas (3 caracteres).
#'     \item \code{Homoclave}: Carácter alfanumérico asignado para evitar duplicados.
#'     \item \code{DigitoVerificador}: Dígito para validación final.
#'   }
#' @examples
#' # Ejemplo de uso con un solo CURP
#' epi_clean_curp("GOMC900514HDFRLA07")
#'
#' # Ejemplo con varios CURPs
#' curps <- c(
#' "GOMC900514HDFRLA07",  # Antes del 2000
#' "LOAM020715MMCRSR09"   # Después del 2000
#' )
#'
#' # Aplicar la función a un vector de CURPs
#' resultados <- map_dfr(curps, epi_clean_curp)
#'
#' # Mostrar resultados
#' print(t(resultados))
#'
#' @importFrom tibble tibble
#' @export


# La primera letra del primer apellido y la primera vocal del primer apellido.
# Primera letra del Segundo apellido.
# Primera letra del primer nombre (Excepto los nombres compuestos cuando estos se  antepongan los nombre de José y María) Ejemplo: si tu nombre es María Isabel, se utilizara la I del segundo nombre.
# Los siguientes 6 dígitos corresponden a tu fecha de nacimiento que aparece en tu acta de nacimiento, se comienza desde el año, mes y día.
# Letra del Género de la persona, H hombres y M mujeres.
# Los 2 dígitos de tu Entidad de Nacimiento, aparecen en tu acta de nacimiento. Recuerda no confundirla con la entidad de tu registro. Si naciste en otro país se debe de poner NE (nacido en el extranjero).
#
# Siguiente consonante del primer apellido.
# Primera consonante interna del segundo apellido .
# Primera consonante interna del primer nombre.
# Homoclave 2 últimos dígitos para evitar duplicaciones.
#


# Librería para manipulación de datos
library(tidyverse)


# Función para extraer datos del CURP usando posiciones
epi_clean_curp <- function(curp) {
  if (nchar(curp) != 18) stop("El CURP debe tener exactamente 18 caracteres.")

# Aplicar extracción por posiciones para cada CURP
  resultados <- lapply(curp, function(c) {
    tibble(
      CURP = c,
      PrimeraLetraApellidoPaterno = substr(c, 1, 1),
      PrimeraVocalApellidoPaterno = substr(c, 2, 2),
      PrimeraLetraApellidoMaterno = substr(c, 3, 3),
      PrimeraLetraNombre = substr(c, 4, 4),
      AñoNacimiento = ifelse(as.numeric(substr(c, 5, 6)) <= 22,
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

  # Combinar los resultados en un único tibble
  do.call(rbind, resultados)
}

