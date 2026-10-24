# Guía de Contribución a TAXIDE

¡Gracias por tu interés en contribuir a **TAXIDE** (*Taxonomic Integrated Development Environment*)!

TAXIDE es un proyecto de software científico de alto rendimiento que combina programación funcional pura en **Haskell**, diseño de compiladores/DSLs y sistemática filogenética/fenética formal. Para mantener la integridad matemática, la ausencia de fugas de memoria (*space leaks*) y la excelencia de la base de código, solicitamos a todos los colaboradores seguir estas pautas rigurosas.

---

## 1. Principios de Diseño y Filosofía de Ingeniería

1. **Taxonomía como Código:** Todo el dominio biológico es un cálculo inmutable modelado con Tipos de Datos Algebraicos (ADTs). Los estados inválidos deben ser irrepresentables por diseño.
2. **Pureza Referencial Absoluta:** La lógica de parsing, inferencia, resolución de dependencias, cladística y exportación vive en paquetes 100% puros (`taxide-core`, `taxide-delta`, `taxide-engine`, `taxide-export`). La interacción con el sistema operativo y la terminal se confina exclusivamente a la cáscara de `taxide-tui` y `taxide-app`.
3. **Erradicación Quirúrgica de Space Leaks:**
   - Todo módulo del núcleo debe declarar `{-# LANGUAGE StrictData #-}`.
   - Usar `Data.Vector.Unboxed` para cómputo numérico y bitsets de estados (`Word64`).
   - Evitar pliegues perezosos (`foldl` o `foldr` acumulativos); usar siempre `foldl'` estricto y forzar valores con `deepseq` (`NFData`).
4. **Verificación Formal y Pruebas Basadas en Propiedades:** Cada transformación sintáctica debe demostrar la propiedad de *round-trip* (`parse (render x) == Right x`) mediante `Hedgehog` o `QuickCheck`.

---

## 2. Entorno de Desarrollo y Requisitos

TAXIDE utiliza el ecosistema estándar de Haskell orquestado con `ghcup` y `cabal`:

* **GHC:** 9.10.x o superior
* **Cabal:** 3.12.x o superior
* **Formateador:** `fourmolu` o `ormolu`
* **Linter:** `hlint`
* **Docker (Opcional):** Para compilación de documentación en LaTeX vía imagen `texlive/texlive:latest-medium`

### Configuración Inicial

Clona el repositorio e inicializa las dependencias:

```bash
git clone https://github.com/ecotech/taxide.git
cd taxide

# Verificar toolchain de GHC y Cabal
ghc --version
cabal --version

# Construir todo el workspace multi-paquete
cabal update
cabal build all --enable-tests --enable-benchmarks
```

---

## 3. Estructura del Workspace Multi-Paquete

El proyecto se divide en 6 paquetes independientes bajo `taxide-*`:

```text
taxide/
├── taxide-core/     # ADTs biológicos, Pseudo-valores ('U','-','V'), rangos estadísticos
├── taxide-delta/    # Lexer y Parsers Megaparsec para archivos SPECS, CHARS, ITEMS
├── taxide-engine/   # Resolución '#+', DAG de dependencias, Fitch, Sankoff, Gower, Claves
├── taxide-export/   # Compiladores a NEXUS (.nex), TNT (.tnt), JSON, Markdown, HDF5
├── taxide-tui/      # Terminal User Interface en Brick/Vty (Gruvbox Dark 24-bit)
└── taxide-app/      # Ejecutable CLI 'taxide'
```

---

## 4. Flujo de Trabajo y Estándares de Código

### 4.1 Creación de Ramas

Crea ramas descriptivas basadas en la rama principal `main`:
* `feat/nombre-de-la-caracteristica` (ej. `feat/sankoff-step-matrix`)
* `fix/descripcion-del-bug` (ej. `fix/megaparsec-slash-ambiguity`)
* `perf/optimizacion` (ej. `perf/unboxed-fitch-bitset`)
* `docs/documentacion` (ej. `docs/algorithm-proofs`)

### 4.2 Convención de Mensajes de Commit

Adoptamos [Conventional Commits](https://www.conventionalcommits.org/):
```text
feat(engine): implementar asignacion ancestral de fitch con Word64 bitsets
fix(delta): corregir ambiguedad contextual del delimitador '/' en atributos
perf(core): desempaquetar campos estrictos en StatRange
test(delta): anadir pruebas de round-trip con Hedgehog para comentarios anidados
```

### 4.3 Pruebas Automatizadas

Antes de someter cualquier Pull Request, ejecuta la suite de pruebas completa:

```bash
# Ejecutar todas las pruebas con salida detallada
cabal test all --test-show-details=streaming

# Comprobar advertencias del compilador (-Wall -Werror)
cabal build all --ghc-options="-Wall -Wcompat -Werror"

# Verificar estilo y sugerencias con HLint
hlint taxide-*/src
```

### 4.4 Cobertura de Código (HPC)

Para módulos críticos en `taxide-core`, `taxide-delta` y `taxide-engine`, se exige una cobertura de expresiones superior al **90%**:

```bash
cabal test all --enable-coverage
```

---

## 5. Lista de Verificación para Pull Requests (PR Checklist)

Antes de solicitar revisión, asegúrate de cumplir con:

- [ ] El código compila sin ninguna advertencia bajo `-Wall -Wcompat`.
- [ ] No se introducen `String` perezosos en estructuras persistentes (usar `Data.Text` o `Data.ByteString`).
- [ ] Todo tipo nuevo en el dominio implementa `Eq`, `Show`, `Generic` y `NFData`.
- [ ] Las funciones recursivas o pliegues están protegidas contra desbordamiento de pila (*stack overflow*) y acumulación de thunks.
- [ ] Se incluyen pruebas unitarias (`Hspec` / `Tasty`) y propiedades (`Hedgehog` / `QuickCheck`).
- [ ] Se preservan todos los comentarios y signaturas de tipos explícitas en funciones exportadas.
- [ ] La documentación en `/doc` o comentarios Haddock se actualiza acordemente.

---

## 6. Reporte de Errores e Inconsistencias Biológicas

Si identificas un error en la interpretación sintáctica del estándar DELTA o una inconsistencia matemática en los algoritmos cladísticos/fenéticos:
1. Abre un issue detallado en GitHub.
2. Incluye una muestra mínima reproducible en formato DELTA (un fragmento de `SPECS`, `CHARS` o `ITEMS`).
3. Especifica el comportamiento observado frente al comportamiento matemáticamente esperado (citando publicaciones de referencia de CSIRO, Fitch 1971, Sankoff 1975 o Gower 1971 cuando aplique).

¡Tu contribución fortalece la ciencia abierta y el rigor computacional en la biología evolutiva!
