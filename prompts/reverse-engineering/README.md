# CardDemo COBOL Reverse Engineering Prompt Suite

## Overview

This prompt suite provides a comprehensive methodology for reverse engineering the CardDemo mainframe COBOL codebase. Following the RUB-001 rubric, these prompts extract and document domain knowledge, data models, architecture, and modernization recommendations.

## Quick Start

```bash
# Execute prompts in order (see Execution Order below)
# Each prompt generates documentation in docs/reverse-engineering/

# Phase 1: Foundation (run in parallel)
# Execute RE-001 and RE-002 simultaneously

# Phase 2-7: Follow dependency chain
# See Execution Order section
```

## Prompt Inventory

| Prompt | Purpose | Output Directory |
|--------|---------|------------------|
| [RE-000-master-index.md](RE-000-master-index.md) | Generate master index | `docs/reverse-engineering/` |
| [RE-001-domain-model.md](RE-001-domain-model.md) | Domain model extraction | `docs/reverse-engineering/01-domain-model/` |
| [RE-002-data-model.md](RE-002-data-model.md) | Data model extraction | `docs/reverse-engineering/02-data-model/` |
| [RE-003-context-model.md](RE-003-context-model.md) | Context model mapping | `docs/reverse-engineering/03-context-model/` |
| [RE-004-c4-architecture.md](RE-004-c4-architecture.md) | C4 architecture (all 4 levels) | `docs/reverse-engineering/04-architecture/` |
| [RE-005-screen-flows.md](RE-005-screen-flows.md) | BMS screen flow mapping | `docs/reverse-engineering/05-specialized/` |
| [RE-006-batch-workflows.md](RE-006-batch-workflows.md) | JCL batch workflow analysis | `docs/reverse-engineering/05-specialized/` |
| [RE-007-security-model.md](RE-007-security-model.md) | Security model analysis | `docs/reverse-engineering/05-specialized/` |
| [RE-008-integration-patterns.md](RE-008-integration-patterns.md) | DB2/IMS/MQ integration patterns | `docs/reverse-engineering/05-specialized/` |
| [RE-009-test-coverage.md](RE-009-test-coverage.md) | Test coverage analysis | `docs/reverse-engineering/06-quality/` |
| [RE-010-modernization-readiness.md](RE-010-modernization-readiness.md) | Modernization assessment | `docs/reverse-engineering/07-modernization/` |
| [RE-011-api-candidates.md](RE-011-api-candidates.md) | API candidate identification | `docs/reverse-engineering/07-modernization/` |

## Execution Order

```
Phase 1: Foundation (Run First - Prerequisites for All Others)
├── RE-001-domain-model.md      ─┐
└── RE-002-data-model.md        ─┴─► Run in parallel

Phase 2: Context (Depends on Phase 1)
└── RE-003-context-model.md

Phase 3: Architecture (Depends on Phase 2)
└── RE-004-c4-architecture.md   (generates L1→L2→L3→L4 sequentially)

Phase 4: Specialized (Can run in parallel, depends on Phase 1)
├── RE-005-screen-flows.md      ─┐
├── RE-006-batch-workflows.md   ─┤
├── RE-007-security-model.md    ─┼─► Run in parallel
└── RE-008-integration-patterns.md─┘

Phase 5: Quality Analysis (Depends on Phase 1)
└── RE-009-test-coverage.md

Phase 6: Modernization Assessment (Depends on Phases 1-4)
├── RE-010-modernization-readiness.md ─┐
└── RE-011-api-candidates.md          ─┴─► Run in parallel

Phase 7: Synthesis (Run Last)
└── RE-000-master-index.md
```

## Output Structure

```
docs/reverse-engineering/
├── index.md                           # Master index
├── 01-domain-model/
│   ├── DOMAIN-MODEL.md
│   ├── UBIQUITOUS-LANGUAGE.md
│   ├── BUSINESS-RULES.md
│   └── DOMAIN-EVENTS.md
├── 02-data-model/
│   ├── DATA-MODEL.md
│   ├── DATA-DICTIONARY.md
│   ├── DATA-LINEAGE.md
│   └── diagrams/er-diagram.md
├── 03-context-model/
│   ├── CONTEXT-MAP.md
│   ├── COMMAREA-SPECIFICATION.md
│   └── NAVIGATION-FLOWS.md
├── 04-architecture/
│   ├── C4-L1-SYSTEM-CONTEXT.md
│   ├── C4-L2-CONTAINER.md
│   ├── C4-L3-COMPONENT.md
│   ├── C4-L4-CODE-PATTERNS.md
│   └── diagrams/
├── 05-specialized/
│   ├── SCREEN-FLOWS.md
│   ├── BATCH-WORKFLOWS.md
│   ├── SECURITY-MODEL.md
│   └── INTEGRATION-PATTERNS.md
├── 06-quality/
│   ├── TEST-COVERAGE.md
│   ├── TEST-SCENARIOS.md
│   └── EDGE-CASES.md
├── 07-modernization/
│   ├── MODERNIZATION-READINESS.md
│   ├── API-CANDIDATES.md
│   ├── API-CONTRACTS.md
│   └── MIGRATION-ROADMAP.md
└── appendices/
    ├── PROGRAM-INVENTORY.md
    ├── COPYBOOK-INVENTORY.md
    ├── FILE-INVENTORY.md
    └── TRANSACTION-INVENTORY.md
```

## Working Directory

Progress tracking and intermediate artifacts are stored in `.work/reverse-engineering/`:

```
.work/reverse-engineering/
├── domain-extraction/
│   ├── progress.yaml
│   ├── program-summaries/
│   └── large-file-chunks/
├── data-model-extraction/
├── context-model/
├── architecture/
└── specialized/
```

## Critical Files

These files require special handling due to size or importance:

| File | Lines | Strategy |
|------|-------|----------|
| `app/cbl/COACTUPC.cbl` | 4,236 | Chunk into 10 segments |
| `app/cbl/COCRDUPC.cbl` | 1,560 | Chunk into 4 segments |
| `app/cbl/COCRDLIC.cbl` | 1,459 | Chunk into 4 segments |
| `app/cpy/COCOM01Y.cpy` | 48 | Load first (COMMAREA) |

## Key Patterns

### COBOL-Specific Conventions
- **Column 7**: Indicator (`*` = comment, `-` = continuation)
- **Columns 8-11**: Area A (divisions, sections, paragraphs)
- **Columns 12-72**: Area B (statements)
- **88-level**: Condition names (domain values)

### Program Naming
- `CO*` prefix: Online CICS programs
- `CB*` prefix: Batch programs
- `*C` suffix: COBOL program files
- `*Y` suffix: Copybooks

### Mainframe Patterns
- CICS pseudo-conversational: `EIBCALEN` check, `RETURN TRANSID`
- COMMAREA state passing via `COCOM01Y.cpy`
- VSAM KSDS with alternate indexes

## Verification

After running each prompt, verify outputs:

```bash
# Check files exist
ls -la docs/reverse-engineering/[category]/

# Verify cross-references
grep -r "](../" docs/reverse-engineering/
```

## Support

For questions or issues with this prompt suite, refer to:
- RUB-001 Rubric for methodology details
- CLAUDE.md for project-specific guidance
