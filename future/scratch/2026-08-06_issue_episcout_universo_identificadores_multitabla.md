# Propuesta para episcout: universo de identificadores multitabla en PostgreSQL

**Estado:** borrador genérico listo para convertirse en issue.

## Título sugerido

`feat(sec): auditar y materializar un universo de identificadores multitabla en PostgreSQL`

## Resumen

Añadir a `episcout` una operación de seguridad e identidad que construya y audite el universo único de identificadores observado en varias relaciones PostgreSQL. La función debe trabajar dentro de PostgreSQL, devolver diagnósticos agregados y libres de valores por defecto y permitir materializar una tabla restringida únicamente después de validar un contrato explícito.

Esta etapa debe ser anterior e independiente de la generación de seudónimos. Su finalidad es conocer qué identificadores existen, en qué fuentes aparecen, qué problemas presentan y si una regla de normalización produciría colisiones. No debe inferir que cada identificador equivale necesariamente a una persona real ni decidir enlaces probabilísticos.

## Problema

Las fuentes longitudinales o multisistema suelen distribuir identificadores en varias tablas. Antes de crear un registro de identidades o un puente de seudonimización es necesario responder, como mínimo:

- cuántas filas e identificadores aporta cada relación;
- cuántos valores son nulos, blancos, inválidos o repetidos;
- cuántos identificadores exactos son únicos en el conjunto completo;
- qué identificadores aparecen en más de una relación;
- qué pares de relaciones se solapan;
- si una normalización revisada cambia la cardinalidad;
- si valores crudos distintos colapsan al mismo valor normalizado;
- si el universo puede materializarse sin perder trazabilidad ni mezclar namespaces.

Estas preguntas no deben resolverse extrayendo identificadores a un data frame de R ni mediante SQL específico de cada consumidor.

## Capacidades existentes relacionadas

`episcout` ya proporciona componentes reutilizables para:

- inventariar relaciones PostgreSQL;
- construir y validar diccionarios con clasificación de privacidad;
- definir metadatos de enlace longitudinal;
- inicializar un registro de identidades restringido;
- auditar y aplicar seudonimización multitabla;
- devolver issues libres de valores y resultados con métodos de impresión redactados;
- ejecutar escrituras PostgreSQL de forma transaccional y con locks acotados.

El contrato actual de enlace exige que exactamente una tabla pueda enrolar identidades nuevas. Eso es apropiado cuando existe una fuente maestra, pero no cubre el caso en que el universo inicial debe obtenerse de la unión revisada de varias relaciones equivalentes.

## Objetivos

- Declarar varias fuentes de identificadores mediante metadatos revisables y sin valores.
- Validar que todas las fuentes pertenecen a un namespace de identidad compatible.
- Auditar calidad, cardinalidad, duplicados, solapamientos y colisiones dentro de PostgreSQL.
- Mantener los valores identificadores fuera de los resultados ordinarios de R.
- Permitir una materialización restringida, transaccional y explícita del universo único.
- Producir un resultado reutilizable por los flujos existentes de registro y seudonimización.
- Seguir los patrones de seguridad, auditoría, errores y clases `epi_sec_*` ya establecidos.

## No objetivos

- Generar tokens o seudónimos dentro de esta función.
- Inferir que dos identificadores distintos representan la misma entidad.
- Realizar enlace probabilístico, fuzzy matching o deduplicación basada en nombres, fechas o domicilios.
- Corregir automáticamente valores inválidos.
- Elegir una fila ganadora entre registros conflictivos.
- Extraer o exportar una lista de identificadores a CSV.
- Sustituir el registro estable de identidades ni las funciones de seudonimización existentes.
- Definir reglas semánticas propias de una fuente concreta.
- Proporcionar control de divulgación o afirmar que los resultados seudonimizados son anónimos.

## Terminología

- **Identificador crudo:** valor exactamente almacenado en una relación fuente.
- **Identificador canónico:** resultado de una normalización explícita y revisada.
- **Universo:** conjunto distinto de identificadores canónicos dentro de un namespace.
- **Membresía:** presencia de un identificador canónico en una relación fuente.
- **Colisión de normalización:** dos o más valores crudos distintos que producen el mismo valor canónico.
- **Namespace:** ámbito dentro del cual dos identificadores idénticos pueden interpretarse como la misma identidad declarada.

La palabra “universo” describe identificadores observados, no personas confirmadas.

## API propuesta

Los nombres son provisionales y deben alinearse con las convenciones finales del paquete.

### 1. Contrato de fuentes

```r
identity_spec <- epi_sec_identity_universe_spec(
  sources = sources,
  normalization = "identity",
  validity_regex = NULL,
  invalid = "report"
)
```

`sources` sería un data frame o CSV de metadatos con:

| Campo | Propósito |
| --- | --- |
| `source_schema` | Schema PostgreSQL de la relación. |
| `source_table` | Tabla o vista fuente. |
| `id_column` | Columna que contiene el identificador. |
| `identity_namespace` | Namespace declarado. |
| `provenance` | Procedencia de la decisión de enlace. |
| `validation_status` | Estado de revisión; debe ser `confirmed`. |

El contrato no debe aceptar valores identificadores.

### 2. Auditoría o materialización

```r
audit <- epi_sec_identity_universe_db(
  con,
  spec = identity_spec,
  mode = "audit"
)
```

```r
result <- epi_sec_identity_universe_db(
  con,
  spec = identity_spec,
  mode = "materialise",
  output_schema = "restricted_identity",
  output_table = "identity_universe",
  existing = "error"
)
```

`mode = "audit"` debe ser el default y no debe escribir objetos. `mode = "materialise"` debe repetir toda la auditoría dentro de la transacción antes de publicar.

## Contrato de identificadores

### Tipos admitidos

Reutilizar, salvo razón documentada en contrario, las familias ya admitidas por el flujo de seudonimización:

- `text` y `varchar` con comparación determinista;
- tipos enteros;
- `uuid`.

Rechazar tipos cuyo significado de igualdad sea ambiguo o no esté soportado, incluidos textos con collation no determinista y caracteres de ancho fijo si el comportamiento de relleno no puede preservarse de forma inequívoca.

### Nulos y blancos

- Contabilizar nulos por fuente.
- Para identificadores textuales, contabilizar cadenas vacías y valores compuestos sólo por whitespace.
- Considerarlos issues bloqueantes para materialización salvo una política futura explícita.
- No convertir nulos o blancos en un identificador artificial.

### Validez formal

`validity_regex` debe ser opcional y aplicable únicamente a identificadores textuales. La política inicial puede admitir:

- `invalid = "report"`: conservar el conteo como issue y bloquear materialización;
- `invalid = "retain_and_flag"`: permitir la materialización con una columna booleana de validez, siempre que no existan otros bloqueantes;
- `invalid = "error"`: devolver un resultado bloqueado cuando aparezca el primer conjunto agregado de inválidos.

La función no debe devolver valores inválidos por defecto.

## Normalización

La normalización debe ser explícita, determinista y de una lista cerrada. Un contrato inicial podría admitir:

- `identity`;
- `trim`;
- `trim_upper`.

Reglas:

- No inferir una normalización a partir de los datos.
- Aplicar la misma regla a todas las fuentes del mismo namespace.
- Rechazar transformaciones incompatibles con el tipo de identificador.
- Informar la cardinalidad antes y después de normalizar.
- Detectar con comparación byte a byte si varios valores crudos colapsan al mismo canónico.
- Bloquear la materialización ante cualquier colisión, salvo que una futura extensión reciba un crosswalk explícito y revisado.
- No devolver los valores implicados salvo una opción sensible, deliberada y claramente marcada.

La auditoría podría calcular diagnósticos comparativos para reglas candidatas sin aprobarlas. Esos diagnósticos no deben cambiar el contrato ni producir un universo materializado.

## Auditorías requeridas

### Por fuente

Devolver una tabla agregada con, como mínimo:

- `source_schema`;
- `source_table`;
- `id_column`;
- `identity_namespace`;
- `n_input`;
- `n_null`;
- `n_blank`;
- `n_invalid`;
- `n_observed`;
- `n_distinct_raw`;
- `n_distinct_canonical`;
- `n_duplicate_excess`;
- `max_frequency`;
- `status`.

### Universo completo

Devolver una fila por namespace con:

- número de fuentes;
- filas totales;
- identificadores observados;
- identificadores crudos distintos;
- identificadores canónicos distintos;
- identificadores presentes en una sola fuente;
- identificadores presentes en varias fuentes;
- colisiones de normalización;
- inválidos retenidos o bloqueantes;
- estado y siguiente acción.

### Solapamiento

Devolver una matriz o tabla larga agregada por par de fuentes con:

- fuente A y fuente B;
- identificadores distintos en A;
- identificadores distintos en B;
- intersección;
- exclusivos de A;
- exclusivos de B;
- proporción de cobertura en cada dirección.

No devolver identificadores individuales en esta tabla.

### Issues

Seguir el contrato de issues de seguridad existente, con campos como:

- `issue_code`;
- `severity`;
- `stage`;
- `source_schema`;
- `source_table`;
- `source_column`;
- `n_affected`;
- `message`;
- `recommended_action`;
- `sensitive`.

Códigos iniciales sugeridos:

```text
missing_identifier
blank_identifier
invalid_identifier
normalization_collision
incompatible_identifier_type
mixed_identity_namespace
nondeterministic_collation
source_relation_missing
source_column_missing
unconfirmed_linkage_metadata
destination_exists
unsafe_output_schema
```

## Diagnósticos sensibles opcionales

Para investigación controlada puede ofrecerse `sensitive_issues = FALSE` siguiendo el patrón existente:

- `FALSE` por default;
- cuando sea `TRUE`, devolver únicamente las filas mínimas necesarias para resolver colisiones o inválidos;
- marcar el componente con una clase específica y redactar `print()` y `str()`;
- no persistirlo automáticamente;
- advertir que extraer, imprimir o guardar el componente es responsabilidad explícita del caller.

Siempre que sea posible, la resolución preferida debe hacerse mediante consultas restringidas dentro de PostgreSQL.

## Semántica de `mode = "audit"`

- Establecer una transacción de sólo lectura y aislamiento `REPEATABLE READ`.
- Validar schemas, relaciones, columnas, tipos, collations y permisos.
- Construir consultas con identificadores citados mediante DBI; no interpolar nombres sin quoting.
- Ejecutar un único snapshot lógico para que todos los conteos concilien.
- No crear tablas temporales persistentes ni escribir al catálogo.
- Devolver `status = "audit_complete"` si no hay bloqueantes o `status = "blocked"` en caso contrario.
- No recoger identificadores individuales en R para calcular conteos, duplicados u overlaps.

## Semántica de `mode = "materialise"`

- Exigir `output_schema` y `output_table` como identificadores válidos.
- Confirmar que el schema de salida existe y que no es uno de entrada.
- Exigir privilegios mínimos y rechazar escrituras fuera del destino declarado.
- Repetir todos los checks dentro de una transacción `REPEATABLE READ`.
- Adquirir un advisory lock transaccional y acotado.
- Crear el universo mediante SQL `UNION ALL` y `SELECT DISTINCT` o una estrategia equivalente ejecutada en PostgreSQL.
- Publicar una tabla ordinaria con una fila por namespace e identificador canónico.
- Añadir, cuando se haya solicitado, la bandera de validez formal.
- Conservar conteos de membresía sin duplicar filas fuente.
- Crear una clave única sobre namespace e identificador canónico.
- Ejecutar `ANALYZE` cuando corresponda.
- Escribir tabla, metadata y manifiesto de forma atómica.
- No modificar las relaciones fuente.

Una estructura mínima de salida podría contener:

```text
identity_namespace
canonical_id
id_is_valid
n_source_relations
```

La tabla materializada contiene identificadores directos y debe tratarse como restringida. La función no debe conceder privilegios de analista ni exportarla.

## Procedencia y manifiesto

El resultado debe incluir metadata libre de valores con:

- modo y estado;
- timestamp de inicio o finalización;
- fuentes y columnas declaradas;
- namespaces;
- normalización y regla de validez;
- snapshot o aislamiento utilizado;
- schema y tabla de salida, cuando existan;
- conteos agregados;
- versión de `episcout`;
- fingerprint determinista del contrato;
- siguiente acción recomendada.

La materialización debe publicar un manifiesto machine-readable que permita verificar posteriormente que la tabla corresponde al mismo contrato y fuentes observadas, sin incluir identificadores.

## Integración con las funciones existentes

La mejora debe complementar, no duplicar, los componentes actuales:

- reutilizar validación de diccionario y clasificación `direct_identifier` con acción `bridge`;
- reutilizar los checks de tipos, collations, permisos, locks y errores sanitizados del flujo `epi_sec_pseudonymise_db()`;
- permitir que una tabla de universo materializada sea declarada después como fuente autorizada de enrolment;
- evaluar si `epi_sec_linkage_spec()` necesita admitir una relación de universo explícita además del modelo actual de una única tabla con `can_enrol = TRUE`;
- mantener la creación de tokens y el registro estable fuera de esta función;
- evitar dos implementaciones distintas de normalización o detección de colisiones.

La API final debe dejar claro qué objeto se pasa al siguiente paso: el contrato validado, la tabla restringida materializada o ambos.

## Rendimiento

- Realizar agregaciones, uniones y conteos dentro de PostgreSQL.
- Evitar `collect()` de identificadores.
- No ejecutar una consulta por valor.
- Reutilizar CTEs o tablas temporales transaccionales sólo cuando reduzcan scans sin dejar residuos.
- Documentar los índices que pueden ayudar, sin crearlos sobre las fuentes de forma implícita.
- Permitir un `statement_timeout` o límite equivalente.
- Reportar consultas canceladas mediante condiciones sanitizadas.
- Probar fuentes vacías, cardinalidad alta y solapamientos densos.

## Clases y métodos de resultado

Introducir una clase como `epi_sec_identity_universe_result` con:

- `status`;
- `metadata`;
- `source_audit`;
- `universe_audit`;
- `overlap_audit`;
- `issues`;
- `manifest`;
- `sensitive_issues`, sólo cuando se solicite.

Añadir métodos `print()` y `str()` que:

- nunca enumeren identificadores;
- muestren estado, conteos, bloqueantes y siguiente acción;
- redacten cualquier componente sensible;
- distingan claramente auditoría sin escrituras de materialización completada.

## Idempotencia y objetos existentes

- `existing = "error"` debe ser el default.
- La primera implementación puede omitir `replace` para reducir riesgo.
- Si se admite `existing = "replace"`, debe limitarse a la tabla ordinaria exactamente declarada, sin `CASCADE`, después de repetir la auditoría y dentro de la misma transacción.
- Un fallo no debe dejar una tabla parcial ni metadata de éxito.
- Repetir `mode = "audit"` sobre el mismo snapshot lógico debe producir los mismos conteos.

## Pruebas requeridas

### Unitarias

- contrato válido de dos o más fuentes;
- metadata incompleta, duplicada o no confirmada;
- mezcla accidental de namespaces;
- tipos compatibles e incompatibles;
- normalizaciones admitidas y no admitidas;
- regex válida e inválida;
- campos de resultado y métodos de impresión redactados;
- fingerprint estable ante el mismo contrato y sensible a cambios relevantes.

### Integración PostgreSQL

- unión de varias tablas con identificadores exclusivos y compartidos;
- filas repetidas dentro de una tabla;
- nulos, vacíos y whitespace;
- valores inválidos según una regex sintética;
- colisión producida por `trim`;
- colisión producida por `trim_upper`;
- tabla fuente vacía;
- source relation o columna ausente;
- collation no determinista;
- auditoría completamente read-only;
- materialización con clave única y conteos conciliados;
- bloqueo sin escritura cuando existe una colisión;
- rollback completo ante fallo;
- rechazo de destino existente;
- permisos insuficientes y output schema inseguro;
- timeout y advisory lock ocupado;
- ausencia de identificadores en mensajes, resultados ordinarios y snapshots de tests.

### Compatibilidad

- las funciones actuales de linkage, registro y seudonimización mantienen su comportamiento;
- un universo materializado puede alimentar el flujo de enrolment mediante el contrato documentado;
- los diccionarios y catálogos existentes no cambian de formato sin una migración explícita.

### Documentación

- ejemplo reproducible con datos completamente sintéticos;
- vignette audit-first que muestre revisión, bloqueo por colisión, corrección del contrato y materialización;
- aclaración visible de que un universo de identificadores no demuestra un universo de personas;
- advertencias sobre datos identificables, roles, logs, backups y exportaciones;
- ejemplo de integración posterior con el registro y la seudonimización sin mezclar responsabilidades.

## Criterios de aceptación

- Se pueden declarar varias relaciones PostgreSQL del mismo namespace mediante metadata libre de valores.
- `mode = "audit"` no escribe y devuelve conteos por fuente, universo, overlaps e issues sin identificadores.
- Nulos, blancos, inválidos, duplicados y colisiones de normalización se detectan de forma explícita.
- No se extraen identificadores a R para producir los resultados ordinarios.
- `mode = "materialise"` sólo se ejecuta con un contrato confirmado y sin bloqueantes.
- La tabla restringida contiene exactamente un identificador canónico por namespace y una clave única verificable.
- Las fuentes permanecen sin cambios y cualquier fallo revierte toda escritura.
- Los métodos de impresión y errores no exponen identificadores.
- El resultado materializado puede integrarse con el flujo existente de identidad y seudonimización mediante un contrato documentado.
- Existen pruebas sintéticas unitarias, PostgreSQL y de documentación para éxito, bloqueo y rollback.

## Preguntas para resolver en el issue

- ¿Conviene extender `epi_sec_linkage_spec()` o crear un contrato independiente para el universo?
- ¿Debe la primera versión materializar únicamente `normalization = "identity"` y dejar otras reglas sólo para auditoría?
- ¿Debe existir una tabla de membresía restringida además del universo, o bastan conteos agregados por fuente?
- ¿Cómo debe declararse el universo materializado como fuente de enrolment para la seudonimización?
- ¿Se admite `retain_and_flag` para inválidos o toda invalidez debe bloquear inicialmente?
- ¿Debe existir `existing = "replace"` en la primera versión?
- ¿Qué fingerprints de estructura o snapshot pueden registrarse de forma portable sin depender de extensiones PostgreSQL?
- ¿Qué límites de número de fuentes, tiempo o cardinalidad deben ser configurables?
