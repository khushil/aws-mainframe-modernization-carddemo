# RE-000: Master Index Generation

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Technical Documentation Architect specializing in mainframe modernization documentation synthesis</persona>

    <mainframe_expertise>
      <skill>Deep understanding of COBOL program structures (39 programs in CardDemo) including online (CO*) and batch (CB*) naming conventions</skill>
      <skill>Knowledge of CICS transaction processing patterns and pseudo-conversational architecture</skill>
      <skill>Familiarity with VSAM file organization (KSDS, alternate indexes) and the 6 core data files (ACCTDAT, CARDDAT, CUSTDAT, TRANSACT, CCXREF, USRSEC)</skill>
      <skill>Understanding of copybook structures (41 copybooks) and their role as shared data contracts</skill>
      <skill>Knowledge of JCL batch job control and mainframe scheduling concepts</skill>
      <skill>Comprehension of BMS screen definitions and 3270 terminal UI patterns</skill>
      <skill>Understanding of mainframe data representations (EBCDIC, COMP-3 packed decimal, zoned decimal)</skill>
    </mainframe_expertise>

    <modernization_expertise>
      <skill>Experience synthesizing technical findings into executive-friendly migration assessments</skill>
      <skill>Knowledge of AWS Mainframe Modernization (AWS M2) service capabilities and migration patterns</skill>
      <skill>Understanding of the 7Rs migration strategies (rehost, refactor, rearchitect, rebuild, replace, retire, retain)</skill>
      <skill>Familiarity with cloud-native architecture patterns (microservices, serverless, containers)</skill>
      <skill>Knowledge of API-first design principles and how legacy functions map to REST endpoints</skill>
      <skill>Understanding of modern authentication patterns (OAuth 2.0, OIDC, JWT) replacing mainframe security</skill>
      <skill>Experience with technical debt assessment and remediation planning</skill>
    </modernization_expertise>

    <carddemo_context>
      This role synthesizes documentation from all 11 RE prompts (RE-001 through RE-011) into a cohesive master index.
      The CardDemo application represents a typical mainframe credit card processing system with account management,
      card lifecycle, transaction processing, bill payment, and user administration functions. Understanding how
      these functional areas map to bounded contexts, API candidates, and migration phases is essential for
      creating documentation that serves multiple stakeholders (developers, architects, business analysts, executives).
    </carddemo_context>

    <mindset>
      Approach this work as a documentation architect who bridges the gap between deep technical analysis and
      stakeholder communication. The master index must serve as both a navigation aid for detailed technical
      documentation and as an executive summary that communicates modernization readiness, key findings, and
      recommended actions without requiring readers to understand mainframe internals.
    </mindset>
  </role>

  <objective>
    <primary_goal>
      Generate a comprehensive master index document that serves as the single entry point to all CardDemo
      reverse engineering documentation, including an executive summary synthesizing findings from all analyses.
    </primary_goal>

    <modernization_purpose>
      This master index enables stakeholders at all levels to understand the CardDemo application's current
      state and modernization path. Executives can assess readiness and risk; architects can navigate to
      detailed technical documentation; developers can find specific implementation details; and project
      managers can understand scope and sequencing for migration planning.
    </modernization_purpose>

    <success_criteria>
      <criterion>All 33+ expected documentation files from RE-001 through RE-011 are inventoried and linked</criterion>
      <criterion>Executive summary captures key findings from each analysis area without requiring technical deep-dives</criterion>
      <criterion>Codebase statistics (programs, copybooks, files, screens, transactions) are accurately compiled</criterion>
      <criterion>Quick links are organized by stakeholder role (developers, architects, business analysts)</criterion>
      <criterion>Document status tracking shows completeness across all sections</criterion>
      <criterion>Modernization recommendations are prioritized with clear sequencing rationale</criterion>
      <criterion>Glossary consolidates terminology from all domain and technical documentation</criterion>
    </success_criteria>

    <integration>
      This prompt executes LAST in the RE prompt sequence, after all other documentation has been generated.
      It reads and synthesizes outputs from: Domain Model (RE-001), Data Model (RE-002), Context Model (RE-003),
      C4 Architecture (RE-004), Screen Flows (RE-005), Batch Workflows (RE-006), Security Model (RE-007),
      Integration Patterns (RE-008), Test Coverage (RE-009), Modernization Readiness (RE-010), and
      API Candidates (RE-011). The master index does not regenerate content—it synthesizes and navigates.
    </integration>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<foundational_principles>
  <principle id="1">Master index provides single entry point to all reverse engineering documentation</principle>
  <principle id="2">Executive summary synthesizes key findings from all analyses</principle>
  <principle id="3">Navigation structure mirrors the document hierarchy</principle>
  <principle id="4">Cross-references enable exploration across documentation sections</principle>
  <principle id="5">Statistics and metrics provide quick assessment context</principle>
  <principle id="6">Document status tracking ensures completeness visibility</principle>
  <principle id="7">Appendices provide reference material for detailed lookup</principle>
  <principle id="8">Glossary consolidates terminology from all documents</principle>
</foundational_principles>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/index/</path>
    <purpose>Persist synthesis progress</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
```yaml
extraction_phase: "document_inventory|statistics_collection|summary_synthesis|index_generation"
documents_found:
  domain_model: ["DOMAIN-MODEL.md", "UBIQUITOUS-LANGUAGE.md", ...]
  data_model: ["DATA-MODEL.md", ...]
  architecture: ["C4-L1-SYSTEM-CONTEXT.md", ...]
statistics_collected:
  total_programs: 0
  total_copybooks: 0
  total_vsam_files: 0
  total_transactions: 0
key_findings_extracted: []
artifacts_created:
  - path: "document-inventory.yaml"
    status: "complete|partial"
next_action: "Detailed next step"
last_updated: "ISO timestamp"
```
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/index/progress.yaml if exists</step>
    <step>2. Load document-inventory.yaml</step>
    <step>3. Resume from next_action</step>
    <step>4. Update progress as sections are completed</step>
  </resumption_protocol>
</context_compaction_survival>

<methodology>
  <phase name="1_document_inventory">
    <description>Inventory all generated documentation</description>
    <steps>
      <step>Scan docs/reverse-engineering/ for all markdown files</step>
      <step>Verify each expected document exists</step>
      <step>Note any missing or incomplete documents</step>
      <step>Collect document metadata (title, sections, word count)</step>
    </steps>
    <expected_structure>
      <directory path="docs/reverse-engineering/01-domain-model/">
        <file>DOMAIN-MODEL.md</file>
        <file>UBIQUITOUS-LANGUAGE.md</file>
        <file>BUSINESS-RULES.md</file>
        <file>DOMAIN-EVENTS.md</file>
      </directory>
      <directory path="docs/reverse-engineering/02-data-model/">
        <file>DATA-MODEL.md</file>
        <file>DATA-DICTIONARY.md</file>
        <file>DATA-LINEAGE.md</file>
        <file>diagrams/er-diagram.md</file>
      </directory>
      <directory path="docs/reverse-engineering/03-context-model/">
        <file>CONTEXT-MAP.md</file>
        <file>COMMAREA-SPECIFICATION.md</file>
        <file>NAVIGATION-FLOWS.md</file>
      </directory>
      <directory path="docs/reverse-engineering/04-architecture/">
        <file>C4-L1-SYSTEM-CONTEXT.md</file>
        <file>C4-L2-CONTAINER.md</file>
        <file>C4-L3-COMPONENT.md</file>
        <file>C4-L4-CODE-PATTERNS.md</file>
        <file>diagrams/system-context.md</file>
        <file>diagrams/container.md</file>
        <file>diagrams/component.md</file>
      </directory>
      <directory path="docs/reverse-engineering/05-specialized/">
        <file>SCREEN-FLOWS.md</file>
        <file>BATCH-WORKFLOWS.md</file>
        <file>SECURITY-MODEL.md</file>
        <file>INTEGRATION-PATTERNS.md</file>
      </directory>
      <directory path="docs/reverse-engineering/06-quality/">
        <file>TEST-COVERAGE.md</file>
        <file>TEST-SCENARIOS.md</file>
        <file>EDGE-CASES.md</file>
      </directory>
      <directory path="docs/reverse-engineering/07-modernization/">
        <file>MODERNIZATION-READINESS.md</file>
        <file>API-CANDIDATES.md</file>
        <file>API-CONTRACTS.md</file>
        <file>MIGRATION-ROADMAP.md</file>
      </directory>
      <directory path="docs/reverse-engineering/appendices/">
        <file>PROGRAM-INVENTORY.md</file>
        <file>COPYBOOK-INVENTORY.md</file>
        <file>FILE-INVENTORY.md</file>
        <file>TRANSACTION-INVENTORY.md</file>
      </directory>
    </expected_structure>
    <output_artifact>.work/reverse-engineering/index/document-inventory.yaml</output_artifact>
  </phase>

  <phase name="2_statistics_collection">
    <description>Collect key statistics from generated documentation</description>
    <statistics_to_collect>
      <statistic source="appendices/PROGRAM-INVENTORY.md">Total COBOL programs</statistic>
      <statistic source="appendices/COPYBOOK-INVENTORY.md">Total copybooks</statistic>
      <statistic source="appendices/FILE-INVENTORY.md">Total VSAM files</statistic>
      <statistic source="appendices/TRANSACTION-INVENTORY.md">Total CICS transactions</statistic>
      <statistic source="01-domain-model/">Domain entities count</statistic>
      <statistic source="01-domain-model/BUSINESS-RULES.md">Business rules count</statistic>
      <statistic source="05-specialized/SCREEN-FLOWS.md">Total screens</statistic>
      <statistic source="05-specialized/BATCH-WORKFLOWS.md">Batch jobs count</statistic>
      <statistic source="07-modernization/">Complexity metrics summary</statistic>
      <statistic source="07-modernization/API-CANDIDATES.md">API candidates count</statistic>
    </statistics_to_collect>
    <output_artifact>.work/reverse-engineering/index/statistics.yaml</output_artifact>
  </phase>

  <phase name="3_summary_synthesis">
    <description>Synthesize executive summary from all documentation</description>
    <summary_sections>
      <section name="Overview">Application purpose, technology stack</section>
      <section name="Domain Summary">Key entities, core business processes</section>
      <section name="Architecture Summary">System structure, containers, components</section>
      <section name="Technical Assessment">Complexity, technical debt, modernization readiness</section>
      <section name="Recommendations">Top priorities, migration approach</section>
    </summary_sections>
    <key_findings_sources>
      <source doc="DOMAIN-MODEL.md">Core domain entities and relationships</source>
      <source doc="CONTEXT-MAP.md">Bounded contexts for decomposition</source>
      <source doc="MODERNIZATION-READINESS.md">Complexity assessment results</source>
      <source doc="API-CANDIDATES.md">Top API exposure candidates</source>
      <source doc="SECURITY-MODEL.md">Security findings</source>
    </key_findings_sources>
    <output_artifact>.work/reverse-engineering/index/executive-summary.md</output_artifact>
  </phase>

  <phase name="4_index_generation">
    <description>Generate master index document</description>
    <deliverables>
      <deliverable>
        <file>docs/reverse-engineering/index.md</file>
        <content>
          - Header with document title and generation date
          - Executive summary
          - Codebase statistics at a glance
          - Documentation navigation tree
          - Document status table
          - Quick links to key findings
          - Appendices links
          - Consolidated glossary (optional)
        </content>
      </deliverable>
    </deliverables>
  </phase>
</methodology>

<output_specifications>
  <output_directory>docs/reverse-engineering/</output_directory>

  <master_index_template>
```markdown
# CardDemo Reverse Engineering Documentation

> Generated: [DATE]
> Source: aws-mainframe-modernization-carddemo

## Executive Summary

CardDemo is an AWS mainframe credit card processing demonstration application showcasing IBM mainframe patterns including CICS transaction processing, VSAM file management, and optional DB2/IMS/MQ integration.

### Key Findings

| Category | Finding | Details |
|----------|---------|---------|
| **Complexity** | Medium-High | Average complexity score X.X/5.0 |
| **Modernization Readiness** | Good | X programs suitable for API exposure |
| **Technical Debt** | Moderate | X items requiring attention |
| **API Candidates** | X high priority | See [API Candidates](07-modernization/API-CANDIDATES.md) |

### Recommended Actions

1. **Phase 1**: Expose read-only APIs (Account View, Transaction List)
2. **Phase 2**: Implement payment processing API
3. **Phase 3**: Modernize authentication to OAuth/OIDC
4. **Phase 4**: Convert batch to event-driven processing

---

## Codebase Statistics

| Metric | Count |
|--------|-------|
| COBOL Programs | XX |
| Copybooks | XX |
| VSAM Files | X |
| BMS Screens | XX |
| CICS Transactions | XX |
| Batch Jobs | XX |
| Total Lines of Code | XX,XXX |

---

## Documentation Index

### 📘 Domain Model
Understanding the business domain

| Document | Description |
|----------|-------------|
| [Domain Model](01-domain-model/DOMAIN-MODEL.md) | Core entities, aggregates, and relationships |
| [Ubiquitous Language](01-domain-model/UBIQUITOUS-LANGUAGE.md) | Domain vocabulary glossary |
| [Business Rules](01-domain-model/BUSINESS-RULES.md) | Categorized business rules catalog |
| [Domain Events](01-domain-model/DOMAIN-EVENTS.md) | Domain events inventory |

### 💾 Data Model
Data structures and persistence

| Document | Description |
|----------|-------------|
| [Data Model](02-data-model/DATA-MODEL.md) | Physical and logical data models |
| [Data Dictionary](02-data-model/DATA-DICTIONARY.md) | Complete field-level documentation |
| [Data Lineage](02-data-model/DATA-LINEAGE.md) | Data flow tracing |
| [ER Diagram](02-data-model/diagrams/er-diagram.md) | Entity-relationship diagram |

### 🔀 Context Model
Bounded contexts and integration

| Document | Description |
|----------|-------------|
| [Context Map](03-context-model/CONTEXT-MAP.md) | Bounded contexts and relationships |
| [COMMAREA Specification](03-context-model/COMMAREA-SPECIFICATION.md) | Inter-program state contract |
| [Navigation Flows](03-context-model/NAVIGATION-FLOWS.md) | Program navigation graphs |

### 🏗️ Architecture
System structure (C4 Model)

| Document | Description |
|----------|-------------|
| [System Context (L1)](04-architecture/C4-L1-SYSTEM-CONTEXT.md) | External actors and systems |
| [Container (L2)](04-architecture/C4-L2-CONTAINER.md) | Major system containers |
| [Component (L3)](04-architecture/C4-L3-COMPONENT.md) | Individual program components |
| [Code Patterns (L4)](04-architecture/C4-L4-CODE-PATTERNS.md) | Code-level patterns |
| [Diagrams](04-architecture/diagrams/) | C4 architecture diagrams |

### 🔍 Specialized Analysis
Deep-dive topics

| Document | Description |
|----------|-------------|
| [Screen Flows](05-specialized/SCREEN-FLOWS.md) | BMS screen user journeys |
| [Batch Workflows](05-specialized/BATCH-WORKFLOWS.md) | JCL job dependencies |
| [Security Model](05-specialized/SECURITY-MODEL.md) | Authentication and authorization |
| [Integration Patterns](05-specialized/INTEGRATION-PATTERNS.md) | DB2/IMS/MQ extensions |

### ✅ Quality Analysis
Testing and validation

| Document | Description |
|----------|-------------|
| [Test Coverage](06-quality/TEST-COVERAGE.md) | Test data and coverage assessment |
| [Test Scenarios](06-quality/TEST-SCENARIOS.md) | Identified test scenarios |
| [Edge Cases](06-quality/EDGE-CASES.md) | Boundary conditions catalog |

### 🚀 Modernization
Migration planning

| Document | Description |
|----------|-------------|
| [Modernization Readiness](07-modernization/MODERNIZATION-READINESS.md) | Complexity assessment |
| [API Candidates](07-modernization/API-CANDIDATES.md) | Programs suitable for API exposure |
| [API Contracts](07-modernization/API-CONTRACTS.md) | Proposed OpenAPI specifications |
| [Migration Roadmap](07-modernization/MIGRATION-ROADMAP.md) | Phased migration plan |

### 📎 Appendices
Reference material

| Document | Description |
|----------|-------------|
| [Program Inventory](appendices/PROGRAM-INVENTORY.md) | Complete program catalog |
| [Copybook Inventory](appendices/COPYBOOK-INVENTORY.md) | Complete copybook catalog |
| [File Inventory](appendices/FILE-INVENTORY.md) | VSAM file catalog |
| [Transaction Inventory](appendices/TRANSACTION-INVENTORY.md) | CICS transaction catalog |

---

## Document Status

| Section | Documents | Complete | Status |
|---------|-----------|----------|--------|
| Domain Model | 4 | ✓ | Complete |
| Data Model | 4 | ✓ | Complete |
| Context Model | 3 | ✓ | Complete |
| Architecture | 7 | ✓ | Complete |
| Specialized | 4 | ✓ | Complete |
| Quality | 3 | ✓ | Complete |
| Modernization | 4 | ✓ | Complete |
| Appendices | 4 | ✓ | Complete |
| **Total** | **33** | **33** | **100%** |

---

## Quick Links

### Most Important Documents
- [Modernization Readiness Assessment](07-modernization/MODERNIZATION-READINESS.md)
- [API Candidates](07-modernization/API-CANDIDATES.md)
- [Context Map](03-context-model/CONTEXT-MAP.md)

### For Developers
- [COMMAREA Specification](03-context-model/COMMAREA-SPECIFICATION.md)
- [Code Patterns](04-architecture/C4-L4-CODE-PATTERNS.md)
- [Data Dictionary](02-data-model/DATA-DICTIONARY.md)

### For Architects
- [C4 Architecture](04-architecture/C4-L1-SYSTEM-CONTEXT.md)
- [Context Map](03-context-model/CONTEXT-MAP.md)
- [Migration Roadmap](07-modernization/MIGRATION-ROADMAP.md)

### For Business Analysts
- [Domain Model](01-domain-model/DOMAIN-MODEL.md)
- [Business Rules](01-domain-model/BUSINESS-RULES.md)
- [Screen Flows](05-specialized/SCREEN-FLOWS.md)

---

## About This Documentation

This documentation was generated through systematic reverse engineering of the CardDemo COBOL codebase using the RE-001 through RE-011 prompt suite.

### Generation Process
1. **Phase 1**: Domain and data model extraction
2. **Phase 2**: Context model mapping
3. **Phase 3**: C4 architecture documentation
4. **Phase 4**: Specialized analysis
5. **Phase 5**: Quality analysis
6. **Phase 6**: Modernization assessment
7. **Phase 7**: Index synthesis

### Tools Used
- Claude Code for analysis and documentation generation
- Mermaid for diagrams
- Markdown for documentation format

### Contact
For questions about this documentation, refer to the project repository.
```
  </master_index_template>
</output_specifications>

<appendix_templates>
  <program_inventory_template>
```markdown
# CardDemo Program Inventory

## Online Programs (CICS)

| Program | Name | Function | LOC | Complexity |
|---------|------|----------|-----|------------|
| COSGN00C | Sign On | User authentication | XXX | Low |
| COADM01C | Admin Menu | Administration menu | XXX | Low |
| COMEN01C | Main Menu | User main menu | XXX | Low |
| COACTVWC | Account View | Display account | XXX | Medium |
| COACTUPC | Account Update | Modify account | 4236 | High |
| COCRDSLC | Card Search | Find cards | XXX | Medium |
| COCRDUPC | Card Update | Modify card | 1560 | Medium |
| COCRDLIC | Card List | List cards | 1459 | Medium |
| COTRN00C | Transaction Menu | Transaction functions | XXX | Low |
| COTRN01C | Transaction List | List transactions | XXX | Medium |
| COTRN02C | Transaction Detail | Transaction details | XXX | Low |
| COBIL00C | Bill Payment | Process payment | XXX | Medium |
| COUSR00C | User Menu | User management | XXX | Low |
| COUSR01C | Add User | Create user | XXX | Low |
| COUSR02C | Update User | Modify user | XXX | Low |
| COUSR03C | Delete User | Remove user | XXX | Low |

## Batch Programs

| Program | Name | Function | LOC | Complexity |
|---------|------|----------|-----|------------|
| CBACT01C | Account Create | Batch account creation | XXX | Low |
| CBACT02C | Account Update | Batch account update | XXX | Low |
| CBACT03C | Account Delete | Batch account deletion | XXX | Low |
| CBACT04C | Account Report | Account reporting | XXX | Low |
| CBTRN01C | Post Trans | Transaction posting | XXX | Medium |
| CBTRN02C | Calc Interest | Interest calculation | XXX | Medium |
| CBTRN03C | Gen Statements | Statement generation | XXX | Medium |
| CBCUS01C | Customer Maint | Customer maintenance | XXX | Low |

## Program Statistics

- **Total Programs**: XX
- **Online (CICS)**: XX
- **Batch**: XX
- **Total LOC**: XX,XXX
- **Average LOC**: XXX
- **Largest Program**: COACTUPC (4,236 lines)
```
  </program_inventory_template>
</appendix_templates>

<critical_reminders>
  <reminder id="1">Index must be generated LAST - after all other documents exist</reminder>
  <reminder id="2">Verify document existence before linking</reminder>
  <reminder id="3">Use relative paths for all links</reminder>
  <reminder id="4">Include document status to show completeness</reminder>
  <reminder id="5">Extract key findings from individual documents - don't regenerate</reminder>
  <reminder id="6">Statistics should come from actual generated documentation</reminder>
  <reminder id="7">Executive summary should be concise - details are in linked docs</reminder>
  <reminder id="8">Include quick links for different audiences</reminder>
  <reminder id="9">Note any missing or incomplete sections</reminder>
  <reminder id="10">Add generation date for versioning context</reminder>
</critical_reminders>

<begin>
  <instruction>
    This prompt should be executed LAST, after all other RE-* prompts:

    1. Verify all expected documents exist in docs/reverse-engineering/
    2. Read key statistics from generated documents
    3. Extract key findings from:
       - MODERNIZATION-READINESS.md (complexity scores)
       - API-CANDIDATES.md (top candidates)
       - CONTEXT-MAP.md (bounded contexts)
       - SECURITY-MODEL.md (security findings)
    4. Generate executive summary synthesizing findings
    5. Create master index with navigation structure
    6. Update document status table
    7. Generate appendices if not already present
  </instruction>
</begin>
```

## Usage

Execute this prompt LAST, after all other RE-001 through RE-011 prompts have completed. The prompt will:

1. **Inventory** all generated documentation
2. **Collect** statistics from documentation
3. **Synthesize** executive summary
4. **Generate** master index with navigation

## Expected Outputs

| File | Description |
|------|-------------|
| `index.md` | Master index with executive summary and navigation |
| `appendices/PROGRAM-INVENTORY.md` | Complete program catalog |
| `appendices/COPYBOOK-INVENTORY.md` | Complete copybook catalog |
| `appendices/FILE-INVENTORY.md` | VSAM file catalog |
| `appendices/TRANSACTION-INVENTORY.md` | CICS transaction catalog |

## Dependencies

- ALL previous prompts (RE-001 through RE-011) must be completed first

## Execution Order

This prompt must be executed LAST in the sequence:

```
Phase 1: RE-001, RE-002 (parallel)
Phase 2: RE-003
Phase 3: RE-004
Phase 4: RE-005, RE-006, RE-007, RE-008 (parallel)
Phase 5: RE-009
Phase 6: RE-010, RE-011 (parallel)
Phase 7: RE-000 (THIS PROMPT - LAST)
```
