# Changelog

Todos los cambios notables en este proyecto serán documentados en este archivo.

El formato está basado en [Keep a Changelog](https://keepachangelog.com/es-ES/1.0.0/),
y este proyecto se adhiere a [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
