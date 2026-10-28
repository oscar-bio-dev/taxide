# Manual de Operaciones y Runbook Técnico de TAXIDE
## Taxonomic Integrated Development Environment

<p align="center">
  <strong>Guía exhaustiva para desarrolladores, taxónomos computacionales y administradores de sistemas sobre aprovisionamiento, flujos de trabajo CLI, validación formal, resolución de incidencias y mantenimiento.</strong>
</p>

---

## 📋 Tabla de Contenidos

1. [Resumen Ejecutivo y Filosofía de Diseño](#1-resumen-ejecutivo-y-filosofía-de-diseño)
2. [Arquitectura del Monorepositorio y Componentes](#2-arquitectura-del-monorepositorio-y-componentes)
3. [Aprovisionamiento del Entorno de Desarrollo (Zero to Hero)](#3-aprovisionamiento-del-entorno-de-desarrollo-zero-to-hero)
4. [Manual de Operaciones y Flujos de Trabajo CLI](#4-manual-de-operaciones-y-flujos-de-trabajo-cli)
   - [4.1 Normalización y Validación Canónica con `taxide-fmt`](#41-normalización-y-validación-canónica-con-taxide-fmt)
   - [4.2 Diagnóstico y Validación Semántica con `taxide-lint`](#42-diagnóstico-y-validación-semántica-con-taxide-lint)
   - [4.3 Transpilación Cross-Target y Exportación Multi-Objetivo](#43-transpilación-cross-target-y-exportación-multi-objetivo)
   - [4.4 Exportación a Darwin Core Archive (DwC-A / GBIF)](#44-exportación-a-darwin-core-archive-dwc-a--gbif)
   - [4.5 Motores de Inferencia Cladística y Claves Dicotómicas](#45-motores-de-inferencia-cladística-y-claves-dicotómicas)
   - [4.6 Sesión Interactiva en Terminal (TUI)](#46-sesión-interactiva-en-terminal-tui)
5. [Guía Paso a Paso: Gestión de Datasets Taxonómicos](#5-guía-paso-a-paso-gestión-de-datasets-taxonómicos)
6. [Protocolo de Pruebas, Invariantes y Calidad de Software](#6-protocolo-de-pruebas-invariantes-y-calidad-de-software)
7. [Pipeline de Documentación Formal (`pdfltex.hs`)](#7-pipeline-de-documentación-formal-pdfltexhs)
8. [Matriz de Diagnóstico y Resolución de Incidencias (Troubleshooting)](#8-matriz-de-diagnóstico-y-resolución-de-incidencias-troubleshooting)

---

## 1. Resumen Ejecutivo y Filosofía de Diseño

**TAXIDE** es un entorno de desarrollo integrado en terminal (TUI) y un conjunto de herramientas de compilación escrito íntegramente en **Haskell puro (GHC 9.10+)**. Su objetivo es sustituir de forma definitiva la constelación de herramientas legadas del estándar CSIRO DELTA (`CONFOR`, `KEY`, `DIST`, `Free Delta Editor`), las cuales sufren de:
- Fragmentación de archivos planos sin validación cruzada (`SPECS`, `CHARS`, `ITEMS`, `CNOTES`).
- Dependencia de binarios monolíticos de 16/32 bits y scripts intermedios en disco.
- Falta de tipado estático, tolerando estados inválidos y dependencias circulares.

### Principios Rectores de Operación
1. **Taxonomía como Código (*Taxonomy as Code*):** Los datos morfológicos, caracteres y variantes se tratan como código fuente inmutable, respaldado por un sistema de tipos algebraicos (ADTs), analizadores combinatorios formales (`Megaparsec`) y comprobación estática de tipos.
2. **Cero Archivos Temporales en Disco:** Toda la computación —desde la resolución del Grafo Acíclico Dirigido (DAG) de dependencias biológicas hasta la generación de contenedores ZIP de Darwin Core— se ejecuta en la memoria RAM mediante funciones funcionales puras.
3. **Invariantes Garantizadas por Construcción:** Los caracteres dependientes cuyo controlador biológico declara inaplicables no pueden evaluarse; los estados exclusivos (`EUM`/`EOM`) prohíben polimorfismos simultáneos (`&`), y la herencia de subespecies (`#+`) es resuelta por monoides estrictos.

---

## 2. Arquitectura del Monorepositorio y Componentes

El proyecto está orquestado mediante `cabal.project` como un espacio de trabajo modular con separación rigurosa de responsabilidades:

```
taxide/
├── cabal.project                # Orquestador del espacio de trabajo Cabal
├── doc/                         # Manifiesto formal LaTeX y compilador inteligente
│   ├── pdfltex.hs               # Script Haskell de compilación (Nativo / Docker)
│   ├── taxide.tex               # Manifiesto matemático y ontológico
│   ├── taxide.pdf               # Salida compilada (495 KB)
│   └── RUNBOOK.md               # Este manual operativo
├── examples/                    # Datasets biológicos de validación
│   ├── craspedacusta/           # Dataset DELTA original (Medusas Craspedacusta)
│   └── craspedacusta_canonical/ # Salida generada por taxide-fmt
├── taxide-core/                 # Tipos algebraicos fundamentales, AST y metadatos EML
│   ├── src/Taxide/Core/
│   │   ├── Character.hs         # Definición formal de caracteres (UM, OM, IN, RN, TE)
│   │   ├── Diagnostic.hs        # Niveles de severidad y mensajes de diagnóstico
│   │   ├── Invariants.hs        # Reglas biológicas e invariantes matemáticas
│   │   ├── Item.hs              # Taxones primarios (#), variantes (#+) y autoridad
│   │   ├── Specs.hs             # Especificaciones globales del dataset
│   │   └── Types.hs             # AST crudo, resuelto, StatRange y DatasetMetadata
│   └── test/Main.hs             # Pruebas unitarias de invariantes biológicas
├── taxide-delta/                # Parser combinatorio y serializador canónico DELTA
│   ├── app/Main.hs              # CLI taxide-fmt
│   ├── src/Taxide/Delta/
│   │   ├── Parser/              # Lexer, Specs, Chars e Items con Megaparsec
│   │   ├── Serializer/          # Renderizadores canónicos sin pérdida
│   │   └── Pipeline.hs          # Carga y serialización unificada de datasets
│   └── test/Main.hs             # Pruebas de Round-Trip y propiedades Hedgehog
├── taxide-engine/               # [Fase 2 y 4] Inferencia en memoria, DAG, Fitch, Sankoff, Gower
├── taxide-export/               # [Fase 3] Compiladores NEXUS, TNT, DwC-A, JSON, Markdown, HDF5
└── taxide-tui/                  # [Fase 5] Interfaz interactiva Brick 24-bit TrueColor
```

### Tabla de Responsabilidades

| Componente | Capa | Responsabilidad Primaria |
|---|---|---|
| `taxide-core` | Dominio Puro | Tipos algebraicos inmutables, invariantes de dependencias, rangos estadísticos y metadatos EML. |
| `taxide-delta` | I/O Sintáctica | Análisis léxico/sintáctico resiliente con `Megaparsec` y formateo canónico idempotente. |
| `taxide-engine` | Lógica & Análisis | Resolución monoidal de variantes (`#+`), evaluación del DAG de dependencias, optimización de Fitch (`Word64` bitsets), programación dinámica de Sankoff y distancias de Gower. |
| `taxide-export` | Transpilación | Compilación multi-target hacia NEXUS, TNT, Darwin Core Archive (DwC-A), Markdown botánico y HDF5. |
| `taxide-tui` | Interfaz de Usuario | TUI interactiva con arquitectura Model-View-Update (`brick`/`vty`), navegación VIM y linter reactivo. |

---

## 3. Aprovisionamiento del Entorno de Desarrollo (Zero to Hero)

### 3.1 Requisitos Previos del Sistema

- **Sistema Operativo:** Linux (Ubuntu 22.04+, Debian 12+, Fedora 38+, Arch) o macOS (12+).
- **GHC (Glasgow Haskell Compiler):** Versión **9.6.x** o superior (recomendado: **GHC 9.10.x**).
- **Cabal:** Versión **3.10.x** o superior (recomendado: **Cabal 3.12+ / 3.16+**).
- **GHCup:** Gestor oficial de toolchains de Haskell ([ghcup.haskell.org](https://www.haskell.org/ghcup/)).
- **Docker / Podman (Opcional):** Para la compilación aislada de documentación LaTeX si no se dispone de `pdflatex` nativo en el sistema anfitrión.

### 3.2 Instalación Rápida de Herramientas Haskell vía GHCup

```bash
# 1. Instalar GHCup (si no está instalado en el sistema)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# 2. Asegurar GHC 9.10 y Cabal reciente
ghcup install ghc 9.10.3
ghcup set ghc 9.10.3
ghcup install cabal 3.16.1.0
ghcup set cabal 3.16.1.0

# 3. Verificar instalación
ghc --version    # Debe reportar: The Glorious Glasgow Haskell Compilation System, version 9.10.x
cabal --version  # Debe reportar: cabal-install version 3.12+ o superior
```

### 3.3 Clonación y Construcción Inicial del Repositorio

```bash
# Clonar el monorepositorio
git clone https://github.com/ecotech/taxide.git
cd taxide

# Actualizar el índice de paquetes de Cabal
cabal update

# Compilar todos los paquetes y ejecutables del espacio de trabajo
cabal build all

# Ejecutar las suites de pruebas formales con salida detallada
cabal test all --test-show-details=streaming
```

### 3.4 Herramientas de Desarrollo y Productividad Recomendadas

Para desarrollo activo y ciclos de retroalimentación inmediata:

```bash
# Sesión REPL interactiva cargando el núcleo
cabal repl taxide-core

# Sesión REPL cargando el módulo de análisis sintáctico
cabal repl taxide-delta

# Ciclo continuo de recarga automática al guardar archivos (usando ghcid)
# En una terminal dedicada:
ghcid -c "cabal repl taxide-delta" -T "Main.main"
```

---

## 4. Manual de Operaciones y Flujos de Trabajo CLI

### 4.1 Normalización y Validación Canónica con `taxide-fmt`

El ejecutable `taxide-fmt` ingesta los tres archivos cardinales de DELTA (`SPECS`, `CHARS`, `ITEMS`), valida la integridad léxica y estructural, y genera versiones canónicas limpias respetando reglas estrictas de sangrado y terminación.

```bash
# Ver ayuda y parámetros disponibles
cabal run taxide-fmt -- --help
```

#### Parámetros Aceptados

| Bandera | Argumento | Valor por Defecto | Descripción |
|---|---|---|---|
| `-i`, `--input-dir` | `DIR` | `.` (Directorio actual) | Ruta al directorio que contiene los archivos `SPECS`, `CHARS` e `ITEMS`. |
| `-o`, `--output-dir` | `OUTDIR` | Ninguno (Opcional) | Ruta al directorio donde se guardarán los archivos reformateados canónicamente. |
| `-c`, `--check` | Ninguno | `False` | Modo auditoría/verificación: solo analiza sintaxis y reporta errores sin escribir archivos en disco. |

#### Casos de Uso Comunes

**Caso A: Validación en Pipelines de CI/CD (Modo `--check`)**
```bash
cabal run taxide-fmt -- -i examples/craspedacusta --check
```
*Salida esperada:* Retorna código de salida `0` si el dataset es válido, o código `1` con el detalle del error sintáctico.

**Caso B: Limpieza y Reemisión Canónica de un Dataset Histórico**
```bash
cabal run taxide-fmt -- -i ./legacy_dataset -o ./canonical_dataset
```
*Efecto:* Genera en `./canonical_dataset/` los archivos `SPECS`, `CHARS` e `ITEMS` formateados de acuerdo con la norma TAXIDE, eliminando ambigüedades en `/` y preservando comentarios `<...>`.

---

### 4.2 Diagnóstico y Validación Semántica con `taxide-lint`

El analizador semántico en memoria (`taxide-lint`) ejecuta las siguientes auditorías biológicas:

1. **Topología del DAG de Dependencias:** Verifica que la directiva `*DEPENDENT CHARACTERS` no contenga ciclos dirigidos ($A \to B \to A$) ni referencias a caracteres inexistentes.
2. **Cumplimiento de Invariantes Inaplicables:** Comprueba que ningún taxón registre estados fenotípicos activos en caracteres declarados biológicamente inaplicables por su carácter controlador.
3. **Exclusividad de Estados (`EUM` y `EOM`):** Rechaza el uso del operador lógico `&` (polimorfismo simultáneo en el mismo espécimen) en caracteres marcados como mutuamente excluyentes.
4. **Coherencia Cuantitativa de Rangos:** Valida que para todo `StatRange` se cumpla $\text{min} \le n_1 \le \text{med} \le n_2 \le \text{max}$.

```bash
# Ejecutar auditoría semántica completa
taxide lint --specs=SPECS --chars=CHARS --items=ITEMS
```

---

### 4.3 Transpilación Cross-Target y Exportación Multi-Objetivo

TAXIDE actúa como un compilador multi-objetivo unificado, reemplazando scripts externos de conversión:

```bash
D=examples/craspedacusta

# 1. NEXUS (.nex) para PAUP*, MrBayes y Mesquite. Polimorfismo (12) vs incertidumbre {12};
#    los caracteres IN/RN se discretizan a OM (--discretize sqrt|sturges|N)
cabal run taxide-compile -- -i $D --target nexus -o matrix.nex

# 2. TNT (.tnt) con xread, cnames y ccode (+ ordenado, - desordenado)
cabal run taxide-compile -- -i $D --target tnt -o matrix.tnt

# 3. Monografías en lenguaje natural (Markdown / HTML5). Opciones: --lang en|es,
#    --omit-implicit, --no-units, --no-text, --no-headings
cabal run taxide-compile -- -i $D --target natural --format md   -o flora.md
cabal run taxide-compile -- -i $D --target natural --format html -o flora.html

# 4. JSON/GeoJSON tipado (ingesta en PostGIS) y tensores binarios columnares
#    alineados a 64 bytes (contenedor TAXIHPC1, no es un fichero HDF5)
cabal run taxide-compile -- -i $D --target json -o matrix.json
cabal run taxide-compile -- -i $D --target hdf5 -o matrix.hpc
```

Si el linter reporta errores el compilador se detiene; use `--force` para compilar de todos modos. Los objetivos binarios (`dwca`, `hdf5`) exigen `--output`.

---

### 4.4 Exportación a Darwin Core Archive (DwC-A / GBIF)

La compilación hacia el estándar internacional Darwin Core Archive proyecta de forma determinista (no biyectiva: se omiten las celdas `U` y `-`) la matriz resuelta hacia el **Esquema en Estrella (*Star Schema*)**:

```bash
cabal run taxide-compile -- -i examples/craspedacusta --target dwca -o craspedacusta_dwca.zip \
  --title "Craspedacusta" --creator "Autor Apellido" --license CC-BY-4.0 --contact autor@example.org
```

#### Estructura Interna del Archivo ZIP Generado en Memoria

```
craspedacusta_dwca.zip
├── taxon.txt                 # Core taxonómico (dwc:Taxon)
├── measurementorfact.txt     # Extensión relacional de mediciones y rasgos morfológicos
├── description.txt           # Extensión de prosa morfológica y citas (gbif:Description)
├── meta.xml                  # Descriptor XML del Star Schema según TDWG
└── eml.xml                   # Metadatos del dataset en EML 2.1.1 (DatasetMetadata)
```

#### Campos Mapeados en el Archivo TSV

- **`taxon.txt`:**
  * `dwc:taxonID`: Clave primaria del taxón.
  * `dwc:scientificName`: Epíteto científico extraído limpiamente.
  * `dwc:scientificNameAuthorship`: Autor y año extraídos automáticamente desde los comentarios `<...>` de la cabecera DELTA.
  * `dwc:parentNameUsageID`: Enlace jerárquico de variantes (`#+`) hacia el taxón progenitor.
- **`measurementorfact.txt`:**
  * `dwc:coreID`: Clave foránea al taxón.
  * `dwc:measurementType`: Característica morfológica extraída de `charFeature`.
  * `dwc:measurementValue`: Estado cualitativo o rango numérico normal ($\text{n1}-\text{n2}$).
  * `dwc:measurementUnit`: Unidad dimensional extraída de `charUnits` (ej. `mm`, `cm`).
  * `dwc:measurementRemarks`: Extremos estadísticos (`srAbsoluteMin`, `srAbsoluteMax`) y notas de carácter.

---

### 4.5 Motores de Inferencia Cladística y Claves Dicotómicas

#### Búsqueda de Árboles Filogenéticos de Máxima Parsimonia

```bash
# Ejecutar optimización de parsimonia con 8 hilos nativos
taxide tree --input=SPECS,CHARS,ITEMS --algorithm=fitch --threads=8 -o best_tree.nwk
```
- **Caracteres `UM`:** Procesados mediante el núcleo de **Fitch en hardware** utilizando bitmasks vectorizables `Word64` en tiempo $O(1)$ por nodo.
- **Caracteres `OM`:** Procesados mediante la recurrencia de Bellman de **Sankoff/Wagner** sobre vectores inmutables sin encajonar (`Data.Vector.Unboxed Double`).

#### Generación de Claves Dicotómicas de Identificación

```bash
taxide key --input=SPECS,CHARS,ITEMS --entropy=shannon -o clave_dicotomica.txt
```
- Evalúa en cada bifurcación el carácter que maximiza la **ganancia de información (Entropía de Shannon)**, minimizando el número promedio de pasos de observación requeridos para determinar un espécimen.

---

### 4.6 Sesión Interactiva en Terminal (TUI)

```bash
taxide tui --dataset=./examples/craspedacusta
```

#### Atajos de Teclado y Controles Modales (Estilo VIM)

- `h`, `j`, `k`, `l` o `Flechas`: Desplazamiento del cursor bidimensional sobre la cuadrícula de taxones y caracteres.
- `i` / `Enter`: Modo edición de celda para el atributo seleccionado.
- `Esc`: Volver al modo Normal.
- `:w`: Guardar dataset canónico en memoria / disco.
- `:q`: Salir de la sesión TUI.
- `Tab`: Alternar entre panel de Hoja de Cálculo, Inspector de Diagnósticos Linter y Árbol Cladístico.
- `/`: Búsqueda reactiva de taxones o caracteres por nombre.

---

## 5. Guía Paso a Paso: Gestión de Datasets Taxonómicos

### Anatomía de un Dataset DELTA en TAXIDE

Un dataset biológico mínimo requiere tres archivos situados en el mismo directorio:

#### 1. Archivo `SPECS` (Especificaciones del Proyecto)
```
*SHOW Flora de Ejemplo - Especificaciones
*NUMBER OF CHARACTERS 3
*MAXIMUM NUMBER OF STATES 2
*MAXIMUM NUMBER OF ITEMS 2
*CHARACTER TYPES 1,UM 2,RN 3,TE
*NUMBERS OF STATES 1,2
*DEPENDENT CHARACTERS 1,2:2
```

#### 2. Archivo `CHARS` (Catálogo de Caracteres y Estados)
```
*SHOW Flora de Ejemplo - Caracteres
*CHARACTER DESCRIPTIONS
#1. hojas/
   1. presentes/
   2. ausentes/
#2. longitud foliar<cm>/
#3. observaciones de hábitat/
```

#### 3. Archivo `ITEMS` (Descripciones de Taxones y Atributos)
```
*SHOW Flora de Ejemplo - Descripciones
*ITEM DESCRIPTIONS
# Quercus robur <L.>/
   1,1 2,7-14 3,Bosques templados caducifolios/
#+ Quercus robur subsp. pedunculiflora/
   2,5-10
# Ephedra distachya <L.>/
   1,2 2,- 3,Zonas áridas y dunas costeras/
```

### Reglas Clave de Formato
- **Comentarios en Nombres de Taxón:** Todo comentario `<...>` colocado en la cabecera `# Taxon <Autor, Año>/` es extraído automáticamente como `itemAuthority`.
- **Variantes Taxonómicas (`#+`):** Heredan todos los atributos del taxón `#` inmediatamente precedente, sobrescribiendo únicamente los caracteres explicitados.
- **Inaplicabilidad (`-`):** Cuando el carácter controlador 1 toma el estado 2 (hojas ausentes), el carácter dependiente 2 (longitud foliar) debe puntuarse como `-` (inaplicable) o ser omitido para que el motor aplique la regla del DAG.

---

## 6. Protocolo de Pruebas, Invariantes y Calidad de Software

TAXIDE utiliza una jerarquía de pruebas basada en la verificación formal:

```bash
# Ejecutar la suite completa de pruebas
cabal test all --test-show-details=streaming
```

### Categorías de Pruebas Automatizadas

1. **Pruebas de Invariantes Biológicas (`taxide-core-test`):**
   - Regla del subconjunto activador de dependencias (*Subset Trigger Rule*): comprueba que estados polimórficos como `1/2` no disparen la inhabilitación si el estado `1` es activo.
   - Rechazo estático de polimorfismo simultáneo (`&`) en tipos `EUM` y `EOM`.
   - Validación estricta de límites estadísticos en `StatRange`.
2. **Pruebas Basadas en Propiedades (*Property-Based Testing* con Hedgehog en `taxide-delta-test`):**
   - **Teorema del Round-Trip:** $\forall a \in \text{AST}, \quad \text{parseDelta}(\text{renderDelta}(a)) \equiv \text{Right}(a)$.
   - Generación de árboles sintácticos arbitrarios con asociatividad a izquierda (`StateOr`, `StateAnd`, `RangeTo`, `StatRange`).
3. **Golden Tests con Datasets Históricos:**
   - Comparación byte a byte contra descripciones canónicas de referencia (*Grass Genera of the World*, *Craspedacusta*).

---

## 7. Pipeline de Documentación Formal (`pdfltex.hs`)

Para mantener la documentación matemática y formal alineada con el código fuente, TAXIDE incluye un script orquestador en Haskell puro: [`doc/pdfltex.hs`](file:///home/ecotech/Projects/taxide/doc/pdfltex.hs).

### Modos de Compilación

El script selecciona de forma automática el mejor backend disponible en el sistema:
1. **Modo Nativo:** Si `pdflatex` está presente en el `$PATH` del anfitrión.
2. **Modo Contenedor (Fallback Automático):** Si no hay TeX Live local pero `docker` está en ejecución, utiliza la imagen verificada `texlive/texlive:latest-medium`, montando el directorio `/work:z` y preservando permisos con `--user <UID>:<GID>`.

```bash
# Compilar taxide.tex a PDF ejecutando 2 pasadas automáticas
runhaskell doc/pdfltex.hs doc/taxide.tex

# Limpiar archivos auxiliares generados (.aux, .log, .out, .toc)
runhaskell doc/pdfltex.hs doc/taxide.tex --clean
```

---

## 8. Matriz de Diagnóstico y Resolución de Incidencias (Troubleshooting)

### Síntoma 1: Error `cannot decode byte sequence starting from 243` al compilar LaTeX
- **Causa:** El ejecutable `pdflatex` o el archivo de log emitió caracteres con codificación ISO-8859-1 (Latin-1) y el proceso intentó leerlos con decodificación UTF-8 estricta.
- **Solución implementada:** `pdfltex.hs` utiliza `safeReadProcess` con decodificación resiliente `latin1`, evitando fallos de decodificación durante la captura de diagnósticos.

### Síntoma 2: Error `Argument of \language@active@arg" has an extra }` en LaTeX
- **Causa:** El paquete `babel` en modo español activa las comillas dobles `"` como caracteres de atajo gramatical.
- **Solución:** Utilizar comillas tipográficas de LaTeX (``\text{``...''}``) o desactivar atajos con `\deactivatequoting`.

### Síntoma 3: Error `Overfull \hbox` en fórmulas matemáticas largas
- **Causa:** Proyecciones matemáticas extensas como la de Darwin Core Archive sobrepasan el ancho de la columna de texto.
- **Solución:** Dividir la expresión en múltiples líneas utilizando el entorno `align` de `amsmath` con `\nonumber`.

### Síntoma 4: Fallo de permisos en Docker (`Permission denied` al escribir `.aux`)
- **Causa:** El contenedor Docker corre por defecto como `root` y crea archivos que el usuario local no puede modificar o viceversa.
- **Solución:** `pdfltex.hs` resuelve dinámicamente `getRealUserID` y `getRealGroupID` mediante `System.Posix.User`, inyectando la bandera `--user <UID>:<GID>` en la invocación de Docker.

### Síntoma 5: Carácter dependiente puntuado con valor activo
- **Causa:** Un taxón define un estado activo para un carácter cuyo controlador fue evaluado en el estado de inhabilitación.
- **Solución:** Ejecutar `taxide-lint` para localizar la celda infractora. Corregir el estado en `ITEMS` asignándole `-` (inaplicable) u omitiéndolo para resolución automática por el DAG.

---

<p align="center">
  <sub>Documento mantenido por el <strong>TAXIDE Architectural & Core Engineering Working Group</strong> (2026).</sub>
</p>
