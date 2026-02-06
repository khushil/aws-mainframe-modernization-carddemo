# CardDemo Reverse Engineering - Master Index

**Document Version:** 1.0
**Generated:** 2026-02-05
**Prompt Suite:** RE-000 through RE-011
**Total Documents:** 38 (this index + 35 analysis documents + confidence assessment + deep-dive)

---

## Executive Summary

This documentation suite provides a comprehensive reverse engineering analysis of the **CardDemo** mainframe credit card processing application. The analysis was performed using the RE prompt suite (RE-000 through RE-011), covering domain modeling, data architecture, system context, C4 architecture, specialized analysis, quality assessment, and modernization planning.

### Key Findings

| Finding | Detail |
|---------|--------|
| **Application Size** | 44 COBOL programs (30,175 LOC), 58 copybooks across all directories |
| **Core Programs** | 31 in `app/cbl/` (20,650 LOC); 13 in extension directories (9,525 LOC) |
| **Bounded Contexts** | 7 identified (Authentication, User Admin, Account, Card, Transaction, Bill Payment, Batch) |
| **Core Entities** | 6 (Customer, Account, Card, Transaction, CardCrossReference, User) |
| **Business Rules** | 59+ cataloged across 8 categories |
| **Domain Events** | 23 identified across 6 aggregates |
| **API Candidates** | 15 of 18 core programs suitable for API exposure; 7 high-priority |
| **Security Findings** | 10 findings (6 critical, 2 high, 2 medium); PCI-DSS at 15% |
| **Modernization Readiness** | 3.53/5 (Moderate-Good) - ready with phased approach |

> **Important:** See [CONFIDENCE-ASSESSMENT.md](CONFIDENCE-ASSESSMENT.md) for known gaps, validation blind spots, and remediation priorities in this documentation suite.

### Codebase Statistics

| Metric | Core | Extensions | Total |
|--------|------|------------|-------|
| COBOL Programs | 31 (17 online + 12 batch + 2 utilities) | 13 | **44** |
| Copybooks | 30 (.cpy) + 17 (.CPY BMS) | 15 | **62** |
| BMS Screens | 17 | 4 | **21** |
| JCL Files | 38 (33 .jcl + 5 .JCL) | 8 | **46** |
| VSAM File Clusters | 10 (6 core + 4 reference) | — | **10** |
| IMS Databases | — | 4 DBD + 4 PSB | **8** |
| DB2 Tables | — | 6 DDL | **6** |
| VSAM Alternate Indexes | 3 | — | **3** |
| GDG Bases | 11 | — | **11** |
| Total Lines of Code | 20,650 | 9,525 | **30,175** |

---

## Documentation Navigation

### Phase 1: Foundation

| # | Document | Path | Description |
|---|----------|------|-------------|
| 1 | **Domain Model** | [01-domain-model/DOMAIN-MODEL.md](01-domain-model/DOMAIN-MODEL.md) | Entity definitions, relationships, class diagrams |
| 2 | **Ubiquitous Language** | [01-domain-model/UBIQUITOUS-LANGUAGE.md](01-domain-model/UBIQUITOUS-LANGUAGE.md) | Domain vocabulary and term definitions |
| 3 | **Business Rules** | [01-domain-model/BUSINESS-RULES.md](01-domain-model/BUSINESS-RULES.md) | 59+ business rules across 8 categories |
| 4 | **Domain Events** | [01-domain-model/DOMAIN-EVENTS.md](01-domain-model/DOMAIN-EVENTS.md) | 23 domain events with payloads and flows |
| 5 | **Data Model** | [02-data-model/DATA-MODEL.md](02-data-model/DATA-MODEL.md) | VSAM file layouts, physical schema |
| 6 | **Data Dictionary** | [02-data-model/DATA-DICTIONARY.md](02-data-model/DATA-DICTIONARY.md) | Field-level definitions and data types |
| 7 | **Data Lineage** | [02-data-model/DATA-LINEAGE.md](02-data-model/DATA-LINEAGE.md) | Data flow tracing across programs |
| 8 | **ER Diagram** | [02-data-model/diagrams/er-diagram.md](02-data-model/diagrams/er-diagram.md) | Entity-relationship diagram |

### Phase 2: Context

| # | Document | Path | Description |
|---|----------|------|-------------|
| 9 | **Context Map** | [03-context-model/CONTEXT-MAP.md](03-context-model/CONTEXT-MAP.md) | 7 bounded contexts with relationships |
| 10 | **COMMAREA Specification** | [03-context-model/COMMAREA-SPECIFICATION.md](03-context-model/COMMAREA-SPECIFICATION.md) | Session state structure analysis |
| 11 | **Navigation Flows** | [03-context-model/NAVIGATION-FLOWS.md](03-context-model/NAVIGATION-FLOWS.md) | Screen navigation state machine |

### Phase 3: Architecture

| # | Document | Path | Description |
|---|----------|------|-------------|
| 12 | **C4 L1 - System Context** | [04-architecture/C4-L1-SYSTEM-CONTEXT.md](04-architecture/C4-L1-SYSTEM-CONTEXT.md) | System boundary and external actors |
| 13 | **C4 L2 - Container** | [04-architecture/C4-L2-CONTAINER.md](04-architecture/C4-L2-CONTAINER.md) | Major containers (CICS, VSAM, Batch) |
| 14 | **C4 L3 - Component** | [04-architecture/C4-L3-COMPONENT.md](04-architecture/C4-L3-COMPONENT.md) | Component-level decomposition |
| 15 | **C4 L4 - Code Patterns** | [04-architecture/C4-L4-CODE-PATTERNS.md](04-architecture/C4-L4-CODE-PATTERNS.md) | 7 recurring code patterns |
| 16 | **System Context Diagram** | [04-architecture/diagrams/system-context.md](04-architecture/diagrams/system-context.md) | Mermaid system context diagram |
| 17 | **Container Diagram** | [04-architecture/diagrams/container.md](04-architecture/diagrams/container.md) | Mermaid container diagram |
| 18 | **CICS Component Diagram** | [04-architecture/diagrams/component-cics.md](04-architecture/diagrams/component-cics.md) | Online component diagram |
| 19 | **Batch Component Diagram** | [04-architecture/diagrams/component-batch.md](04-architecture/diagrams/component-batch.md) | Batch component diagram |
| 20 | **Data Flow Diagram** | [04-architecture/diagrams/data-flow.md](04-architecture/diagrams/data-flow.md) | Cross-system data flows |

### Phase 4: Specialized Analysis

| # | Document | Path | Description |
|---|----------|------|-------------|
| 21 | **Screen Flows** | [05-specialized/SCREEN-FLOWS.md](05-specialized/SCREEN-FLOWS.md) | 17 BMS screens, fields, navigation, user journeys |
| 22 | **Batch Workflows** | [05-specialized/BATCH-WORKFLOWS.md](05-specialized/BATCH-WORKFLOWS.md) | 33 JCL jobs, dependencies, recovery procedures |
| 23 | **Security Model** | [05-specialized/SECURITY-MODEL.md](05-specialized/SECURITY-MODEL.md) | 10 security findings, PCI-DSS gap analysis |
| 24 | **Integration Patterns** | [05-specialized/INTEGRATION-PATTERNS.md](05-specialized/INTEGRATION-PATTERNS.md) | DB2/IMS/MQ integration analysis |

### Phase 5: Quality

| # | Document | Path | Description |
|---|----------|------|-------------|
| 25 | **Test Coverage** | [06-quality/TEST-COVERAGE.md](06-quality/TEST-COVERAGE.md) | Current test coverage assessment |
| 26 | **Test Scenarios** | [06-quality/TEST-SCENARIOS.md](06-quality/TEST-SCENARIOS.md) | Functional test scenario catalog |
| 27 | **Edge Cases** | [06-quality/EDGE-CASES.md](06-quality/EDGE-CASES.md) | Boundary conditions and error paths |

### Phase 6: Modernization

| # | Document | Path | Description |
|---|----------|------|-------------|
| 28 | **API Candidates** | [07-modernization/API-CANDIDATES.md](07-modernization/API-CANDIDATES.md) | 18 programs scored for API suitability |
| 29 | **API Contracts** | [07-modernization/API-CONTRACTS.md](07-modernization/API-CONTRACTS.md) | OpenAPI 3.0 contract specifications |
| 30 | **Modernization Readiness** | [07-modernization/MODERNIZATION-READINESS.md](07-modernization/MODERNIZATION-READINESS.md) | Complexity, debt, and readiness scoring |
| 31 | **Migration Roadmap** | [07-modernization/MIGRATION-ROADMAP.md](07-modernization/MIGRATION-ROADMAP.md) | 4-wave phased migration plan |

### Appendices

| # | Document | Path | Description |
|---|----------|------|-------------|
| 32 | **Program Inventory** | [appendices/PROGRAM-INVENTORY.md](appendices/PROGRAM-INVENTORY.md) | All 44 COBOL programs (core + extensions) with LOC, complexity |
| 33 | **Copybook Inventory** | [appendices/COPYBOOK-INVENTORY.md](appendices/COPYBOOK-INVENTORY.md) | All 58 copybooks with purpose and dependencies |
| 34 | **File Inventory** | [appendices/FILE-INVENTORY.md](appendices/FILE-INVENTORY.md) | VSAM clusters, AIX, GDG specifications |
| 35 | **Transaction Inventory** | [appendices/TRANSACTION-INVENTORY.md](appendices/TRANSACTION-INVENTORY.md) | CICS transactions, BMS mapsets, navigation |

### Quality Assurance

| # | Document | Path | Description |
|---|----------|------|-------------|
| 36 | **Confidence Assessment** | [CONFIDENCE-ASSESSMENT.md](CONFIDENCE-ASSESSMENT.md) | Honest assessment of gaps, blind spots, and remediation priorities |
| 37 | **COACTUPC Deep-Dive** | [deep-dives/COACTUPC-ANALYSIS.md](deep-dives/COACTUPC-ANALYSIS.md) | Line-by-line analysis of the 4,236-line monolith |
| 38 | **SME Validation Strategy** | [08-validation-strategy/SME-VALIDATION-STRATEGY.md](08-validation-strategy/SME-VALIDATION-STRATEGY.md) | Interview playbook, question bank, confidence framework for SME validation |

---

## Document Status

| Section | Expected | Actual | Status |
|---------|----------|--------|--------|
| 01-domain-model | 4 | 4 | Complete |
| 02-data-model | 4 | 4 | Complete |
| 03-context-model | 3 | 3 | Complete |
| 04-architecture | 9 | 9 | Complete |
| 05-specialized | 4 | 4 | Complete |
| 06-quality | 3 | 3 | Complete |
| 07-modernization | 4 | 4 | Complete |
| appendices | 4 | 4 | Complete (inventory updated to v2.0) |
| validation | 7 | 7 | Complete (VL-006 revised) |
| deep-dives | 1 | 1 | New — COACTUPC analysis |
| confidence assessment | 1 | 1 | New — quality assurance |
| index | 1 | 1 | Complete (updated) |
| **Total** | **45** | **45** | **Complete** |

---

## Quick Links by Stakeholder Role

### For Developers

Start with code patterns and data structures:
1. [C4 L4 - Code Patterns](04-architecture/C4-L4-CODE-PATTERNS.md) - Understand the 7 recurring patterns
2. [Data Model](02-data-model/DATA-MODEL.md) - VSAM file layouts
3. [Program Inventory](appendices/PROGRAM-INVENTORY.md) - All programs with LOC and complexity
4. [Business Rules](01-domain-model/BUSINESS-RULES.md) - 59+ rules to preserve during migration

### For Architects

Start with system context and modernization:
1. [Context Map](03-context-model/CONTEXT-MAP.md) - 7 bounded contexts for microservices decomposition
2. [C4 L1 - System Context](04-architecture/C4-L1-SYSTEM-CONTEXT.md) - System boundaries
3. [Modernization Readiness](07-modernization/MODERNIZATION-READINESS.md) - Readiness scoring
4. [Migration Roadmap](07-modernization/MIGRATION-ROADMAP.md) - 4-wave migration plan
5. [API Candidates](07-modernization/API-CANDIDATES.md) - API suitability scores

### For Business Analysts

Start with domain model and business rules:
1. [Domain Model](01-domain-model/DOMAIN-MODEL.md) - Core entities and relationships
2. [Ubiquitous Language](01-domain-model/UBIQUITOUS-LANGUAGE.md) - Domain vocabulary
3. [Business Rules](01-domain-model/BUSINESS-RULES.md) - Complete rule catalog
4. [Domain Events](01-domain-model/DOMAIN-EVENTS.md) - Business events and flows
5. [Screen Flows](05-specialized/SCREEN-FLOWS.md) - User interface analysis

### For Security Teams

Start with security findings and compliance:
1. [Security Model](05-specialized/SECURITY-MODEL.md) - 10 findings, PCI-DSS gaps
2. [Modernization Readiness](07-modernization/MODERNIZATION-READINESS.md) - Security debt assessment
3. [API Contracts](07-modernization/API-CONTRACTS.md) - Security scope definitions

### For QA Teams

Start with test coverage and scenarios:
1. [Test Coverage](06-quality/TEST-COVERAGE.md) - Current coverage gaps
2. [Test Scenarios](06-quality/TEST-SCENARIOS.md) - Functional test catalog
3. [Edge Cases](06-quality/EDGE-CASES.md) - Boundary conditions
4. [Business Rules](01-domain-model/BUSINESS-RULES.md) - Rules requiring test coverage

### For Project Managers

Start with readiness and roadmap:
1. [Modernization Readiness](07-modernization/MODERNIZATION-READINESS.md) - Risk assessment, scoring
2. [Migration Roadmap](07-modernization/MIGRATION-ROADMAP.md) - Wave planning, dependencies
3. [Program Inventory](appendices/PROGRAM-INVENTORY.md) - Scope overview

### For Quality/Risk Teams

Start with confidence assessment and validation:
1. [Confidence Assessment](CONFIDENCE-ASSESSMENT.md) - Known gaps, blind spots, remediation priorities
2. [VL-006 Batch Workflow Report](validation/VL-006-batch-workflow-report.md) - Lowest-scoring validation (76.6/100)
3. [VL-007 Security Model Report](validation/VL-007-security-model-report.md) - Security validation
4. [COACTUPC Deep-Dive](deep-dives/COACTUPC-ANALYSIS.md) - Highest-risk program analysis

---

## Prompt Execution Summary

| Prompt | Purpose | Phase | Status |
|--------|---------|-------|--------|
| RE-001 | Domain model extraction | 1 (Foundation) | Complete |
| RE-002 | Data model extraction | 1 (Foundation) | Complete |
| RE-003 | Context model mapping | 2 (Context) | Complete |
| RE-004 | C4 architecture (all 4 levels) | 3 (Architecture) | Complete |
| RE-005 | BMS screen flow mapping | 4 (Specialized) | Complete |
| RE-006 | JCL batch workflow analysis | 4 (Specialized) | Complete |
| RE-007 | Security model analysis | 4 (Specialized) | Complete |
| RE-008 | DB2/IMS/MQ integration patterns | 4 (Specialized) | Complete |
| RE-009 | Test coverage analysis | 5 (Quality) | Complete |
| RE-010 | Modernization readiness assessment | 6 (Modernization) | Complete |
| RE-011 | API candidate identification | 6 (Modernization) | Complete |
| RE-000 | Master index generation | 7 (Index) | Complete |

---

## Generation Metadata

| Attribute | Value |
|-----------|-------|
| Generator | Claude Code (RE-000 prompt) |
| Initial Date | 2026-02-05 |
| Last Updated | 2026-02-06 (confidence assessment + remediation) |
| Source Repository | aws-mainframe-modernization-carddemo |
| COBOL Programs Analyzed | 44 (31 core + 13 extension) |
| Copybooks Analyzed | 58 (46 core + 12 extension) |
| BMS Screens Analyzed | 23 (19 core + 4 extension) |
| JCL Jobs Analyzed | 46 (38 core + 8 extension) |
| Documents Generated | 45 (36 original + 7 validation + 2 new) |
| Total Lines of Code | 30,175 |

---

*This master index was generated as part of the RE-000 Master Index Generation for the CardDemo reverse engineering project. All documents are located under `docs/reverse-engineering/`.*
