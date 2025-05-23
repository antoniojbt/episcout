% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epi_clean_curp.R
\name{epi_clean_curp}
\alias{epi_clean_curp}
\title{Extraer datos del CURP en México}
\usage{
epi_clean_curp(curp)
}
\arguments{
\item{curp}{Un vector de texto que contiene uno o más CURPs. Cada CURP debe tener exactamente 18 caracteres.}
}
\value{
Un \code{tibble} con los componentes del CURP organizados en columnas:
\itemize{
\item \code{CURP}: El CURP original.
\item \code{PrimeraLetraApellidoPaterno}: Primera letra del apellido paterno.
\item \code{PrimeraVocalApellidoPaterno}: Primera vocal del apellido paterno.
\item \code{PrimeraLetraApellidoMaterno}: Primera letra del apellido materno.
\item \code{PrimeraLetraNombre}: Primera letra del nombre.
\item \code{AñoNacimiento}: Año completo de nacimiento (e.g., "1990", "2002").
\item \code{MesNacimiento}: Mes de nacimiento (2 dígitos).
\item \code{DiaNacimiento}: Día de nacimiento (2 dígitos).
\item \code{Sexo}: Género registrado (H para hombre, M para mujer).
\item \code{EntidadFederativa}: Entidad federativa de nacimiento (código de 2 letras).
\item \code{PrimerasConsonantes}: Consonantes internas (3 caracteres).
\item \code{Homoclave}: Carácter alfanumérico asignado para evitar duplicados.
\item \code{DigitoVerificador}: Dígito para validación final.
}
}
\description{
Esta función descompone un CURP en sus componentes principales según las posiciones definidas en su estructura.
}
\examples{
# Ejemplo de uso con un solo CURP
epi_clean_curp("GOMC900514HDFRLA07")

# Ejemplo con varios CURPs
curps <- c(
"GOMC900514HDFRLA07",  # Antes del 2000
"LOAM020715MMCRSR09"   # Después del 2000
)

# Aplicar la función a un vector de CURPs
resultados <- map_dfr(curps, epi_clean_curp)

# Mostrar resultados
print(t(resultados))

}
