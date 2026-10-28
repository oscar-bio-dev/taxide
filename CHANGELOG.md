# Changelog

Todos los cambios notables en este proyecto serán documentados en este archivo.

El formato está basado en [Keep a Changelog](https://keepachangelog.com/es-ES/1.0.0/),
y este proyecto se adhiere a [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [0.2.1] - 2026-10-28

### 🐛 Corregido

#### `taxide-engine`
- **Tiempo de ejecución del pipeline:** `runPipeline` medía ~0 ms por evaluación perezosa; ahora fuerza el resultado (`evaluate . force`) antes de cerrar el cronómetro.
- **Propagación topológica y clausura transitiva (`Taxide.Engine.Dependencies`):** las reglas se aplican en el orden topológico calculado por Kahn y un controlador ya forzado a `-` por un antecesor dispara a sus dependientes (p. ej. `9,2 10,3 20,1` → dos `TAX-DEP-002` y carácter 20 inaplicable).
- **Discretización de caracteres `IN`:** `discretizeTaxaValues` ahora acepta `AttrInteger` además de `AttrReal`.
- **Pérdida silenciosa de estados:** el bitset admite ahora 64 estados (estándar DELTA, antes 62) y un estado fuera de `[1, 64]` emite `TAX-BND-002` en lugar de descartarse.
- **Carácter repetido en un ítem:** se reporta como `TAX-DATA-003` (antes se conservaba el último valor sin aviso) en ítems primarios, variantes y variantes huérfanas.

### ⚡ Rendimiento
- `TAX-WARN-001` agrupa taxones por firma de fila en un `Map` (de $O(N^2M)$ a $O(NM\log N)$ + salida).

### 🧪 Pruebas
- Motor: 25 pruebas (5 nuevas de regresión; la prueba de cascada ahora verifica el carácter 20 y los dos diagnósticos).

### 📚 Documentación
- `doc/taxide.tex` reescrito y actualizado a v0.2.1 (estado de implementación, catálogo de diagnósticos, capacidad del bitset).

---

## [0.2.0] - 2026-10-24

### 🚀 Añadido

#### `taxide-engine` (Motor de Resolución Semántica e Inferencia en Memoria)
- **Herencia de Variantes Taxonómicas (`Taxide.Engine.Variants`):**
  - Recorrido secuencial y unión monoidal sesgada a la izquierda (`Map.union`) entre los atributos explícitos de la variante (`#+`) y su taxón principal precedente (`#`).
  - Detección estructurada de ítems variantes huérfanos sin padre (`TAX-VAR-001`).
- **Inyección Determinista de Valores Implícitos (`Taxide.Engine.Implicits`):**
  - Inyección de estados por defecto `*IMPLICIT VALUES (c, s:t)` sobre caracteres omitidos ($s$) o no puntuados ($t$).
  - Validación de tipos de caracteres: restricción estricta a caracteres cualitativos (`UM`, `OM`, `EUM`, `EOM`), rechazando variables cuantitativas (`IN`, `RN`) y de texto (`TE`) con código `TAX-IMP-001`.
- **DAG de Dependencias y Propagación Reactiva (`Taxide.Engine.Dependencies`):**
  - Compilación estática del grafo de dependencias de `*DEPENDENT CHARACTERS` y validación de aciclicidad mediante ordenamiento topológico (algoritmo de Kahn), detectando ciclos dirigidos con código `TAX-DEP-001`.
  - Propagación de inhabilitación en orden topológico siguiendo la regla canónica de Dallwitz & Paine: inactiva cuando el conjunto observado no es vacío y es subconjunto de los estados inhabilitantes ($S_{obs} \neq \emptyset \land S_{obs} \subseteq S_{inh}$), soportando polimorfismos parciales válidos (ej. `10,1/2 12,1/-`).
  - Detección de violaciones biológicas contradictorias cuando un carácter dependiente posee un estado activo explícito bajo un controlador inhabilitado (`TAX-DEP-002`), forzando de forma resiliente el estado a `Inapplicable` (`'-'`).
- **Discretización Cuantitativa Dinámica (`Taxide.Engine.Discretize`):**
  - Transformación funcional pura de variables continuas y de conteo (`IN`/`RN`) a multiestado ordenado (`OM`).
  - Soporte para la Regla de la Raíz Cuadrada ($k = \lceil \sqrt{N} \rceil$) y Regla de Sturges ($k = \lceil 1 + 3.322 \log_{10}(N) \rceil$), con acotamiento estricto $k \in [2, 30]$.
  - Particionamiento dinámico de rangos estadísticos utilizando el valor central o el punto medio normal de `StatRange`.
- **Matriz Columnar Resuelta SoA (`Taxide.Engine.Matrix`):**
  - Representación *Structure of Arrays* (SoA) altamente eficiente para hardware moderno.
  - Caracteres discretos empacados en vectores sin encajonar `Data.Vector.Unboxed Word64` con vector paralelo `Word8` para etiquetas de pseudo-valores (`Valid`, `Unknown`, `Inapplicable`, `Variable`).
  - Columnas cuantitativas desacopladas en cinco vectores `Vector.Unboxed Double` (`min`, `normalLow`, `central`, `normalHigh`, `max`).
- **Pipeline Orquestador y Cronometraje Monotónico (`Taxide.Engine.Pipeline`):**
  - Pipeline puro y determinista de resolución en 5 etapas en memoria (cero archivos temporales en disco).
  - Medición de tiempos de ejecución de alta precisión en milisegundos (`GHC.Clock.getMonotonicTime`).
- **Ejecutable CLI `taxide-lint`:**
  - Auditoría semántica completa de colecciones DELTA (`SPECS`, `CHARS`, `ITEMS`).
  - Salida formateada para terminal con desglose por código de regla, ubicación taxonómica y severidad (`Error`, `Warning`, `Info`).
  - Opción `--json` estructurada para integración sin fricción en pipelines de CI/CD.
  - Opción `-v` / `--verbose` con métricas de la matriz SoA calculada en memoria.
- **Suite de Pruebas `taxide-engine-test`:**
  - Pruebas unitarias de los ejemplos canónicos oficiales de Dallwitz & Paine (1986).
  - Pruebas basadas en propiedades con `Hedgehog` verificando la invariante del DAG (inactivación biológica estricta) y la preservación dimensional en la transposición SoA.
- **Linter Ontológico y Cladístico Avanzado (`Taxide.Engine.Linter`):**
  - Detección de caracteres invariantes sin variación topológica informativa para parsimonia (`TAX-CLAD-001` - Warning).
  - Detección de autapomorfías singulares no informativas para resolución de clados (`TAX-CLAD-002` - Info).
  - Identificación de caracteres 100% no codificados sugiriendo su exclusión (`TAX-DATA-001` - Warning).
  - Auditoría de taxones principales duplicados con idéntico nombre y autoridad (`TAX-DATA-002` - Warning).
  - Validación matemática de coherencia en rangos cuantitativos `StatRange` (`TAX-MATH-001` - Error bloqueante).
  - Identificación de taxones biológicamente indistinguibles causantes de ramas de longitud cero (`TAX-WARN-001` - Warning).

---

## [0.1.0] - 2026-10-24

### 🚀 Añadido

#### `taxide-core` (Núcleo Semántico Biológico)
- **Tipos de Datos Algebraicos Fundamentales:**
  - `CharacterType` modelando las cinco dimensiones morfológicas de DELTA: `UM` (Multiestado desordenado), `OM` (Multiestado ordenado), `EUM` (Exclusivo desordenado), `EOM` (Exclusivo ordenado), `IN` (Entero discreto), `RN` (Numérico continuo) y `TE` (Texto libre descriptivo).
  - `PseudoValue` para ausencias e indeterminaciones biológicas: `Unknown` (`'U'`), `Inapplicable` (`'-'`) y `Variable` (`'V'`).
  - `StatRange a` capturando mediciones cuantitativas complejas con extremos absolutos y rangos normales: `(min-)n1-med-n2(-max)`.
- **AST Crudo vs. Fase Resuelta:**
  - `RawStateExpr` y `RawAttribute` que preservan la sintaxis exacta, comentarios por estado (`r1<e1>`) y operadores lógicos `/` (OR), `&` (AND) y `-` (RangeTo), garantizando el teorema de Round-Trip sin pérdida semántica.
  - `ResolvedState` y `ResolvedAttribute` con bitmasks unboxed de 64 bits (`Word64`) para operaciones de parsimonia en $O(1)$ de hardware.
- **Modelado de Taxones y Metadatos EML:**
  - `ItemDef` con soporte para taxones principales (`PrimaryItem` / `#`) y variantes infraespecíficas (`VariantItem` / `#+`).
  - Separación explícita de `itemAuthority :: !(Maybe Text)` en `ItemDef`.
  - Estructura `DatasetMetadata` y constructor `emptyDatasetMetadata` para la generación de metadatos en *Ecological Metadata Language* (EML 2.1.1) orientados a Darwin Core Archive (DwC-A).
- **Invariantes Biológicas:**
  - Formalización y verificación en `Invariants.hs` de la regla del subconjunto activador (*Subset Trigger Rule*) para `*DEPENDENT CHARACTERS`.
  - Verificación estática de exclusión mutua para caracteres `EUM`/`EOM` ante operadores de polimorfismo simultáneo (`&`).
  - Validación de coherencia de rangos estadísticos en `StatRange`.

#### `taxide-delta` (Parser y Serializador Canónico)
- Analizador combinatorio en `Megaparsec` para directivas del archivo `SPECS` (`*NUMBER OF CHARACTERS`, `*CHARACTER TYPES`, `*DEPENDENT CHARACTERS`, `*IMPLICIT VALUES`, etc.).
- Desambiguador léxico contextual para el carácter `/` (terminador de definición vs. operador lógico OR vs. texto literal).
- Parser y pretty-printer canónico para catálogos de caracteres (`CHARS`) y descripciones taxonómicas (`ITEMS`).
- Soporte para comentarios anidados arbitrariamente profundos `<...<...>>`.
- Extracción automática de la autoridad taxonómica (`dwc:scientificNameAuthorship`) desde comentarios `<...>` en los nombres de taxones.
- Ejecutable CLI `taxide-fmt` para auditoría (`--check`) y reformateo canónico idempotente de colecciones DELTA.

#### Documentación Científica y Runbooks
- Manifiesto fundacional y ontológico en LaTeX ([`doc/taxide.tex`](doc/taxide.tex)) y versión compilada en PDF ([`doc/taxide.pdf`](doc/taxide.pdf), 495 KB).
- Formalización matemática del mapeo al **Esquema en Estrella (*Star Schema*)** de Darwin Core Archive (DwC-A / GBIF):
  - Core `taxon.txt` (`dwc:Taxon`), extensión `measurementorfact.txt` (`dwc:MeasurementOrFact`) y extensión `description.txt` (`gbif:Description`).
  - Generación determinista en memoria de `meta.xml`, `eml.xml` y empaquetado ZIP sin archivos temporales en disco.
- Compilador inteligente y resiliente en Haskell ([`doc/pdfltex.hs`](doc/pdfltex.hs)) con fallback automático a Docker TeXLive.
- Manual de Operaciones y Runbook Técnico exhaustivo ([`doc/RUNBOOK.md`](doc/RUNBOOK.md)).
- Guía de inicio rápido (*Quick Start*) integrada en el [`README.md`](README.md).

#### Infraestructura, Pruebas y CI/CD
- Monorepositorio multi-paquete orquestado por `cabal.project` bajo GHC 9.10+ y Cabal 3.12+.
- Suite de pruebas unitarias (`tasty`, `tasty-hunit`) con 9 pruebas de invariantes biológicas en `taxide-core-test`.
- Suite de pruebas de Round-Trip y pruebas basadas en propiedades con `Hedgehog` (100 iteraciones por propiedad) en `taxide-delta-test`.
- Flujo de Integración Continua en GitHub Actions ([`.github/workflows/ci.yml`](.github/workflows/ci.yml)) con matriz GHC 9.10 / GHC 9.8, caché de dependencias y compilación automatizada de documentación.
- Configuración de normalización de saltos de línea LF y atributos de lenguaje en [`.gitattributes`](.gitattributes).
- Archivo [`.gitignore`](.gitignore) categorizado para Haskell, LaTeX, herramientas filogenéticas y bases de datos.
- Configuración de gobernanza del proyecto: [`LICENSE`](LICENSE) (Apache-2.0), [`CITATION.cff`](CITATION.cff), [`CONTRIBUTING.md`](CONTRIBUTING.md) y [`CODE_OF_CONDUCT.md`](CODE_OF_CONDUCT.md).
- Dataset biológico canónico de validación: Medusas dulceacuícolas *Craspedacusta* ([`examples/craspedacusta`](examples/craspedacusta)).
