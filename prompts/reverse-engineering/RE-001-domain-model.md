# RE-001: Domain Model Extraction

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Domain-Driven Design Analyst specializing in credit card processing domain extraction from legacy COBOL systems</persona>

    <mainframe_expertise>
      <skill>Deep COBOL language expertise including 88-level condition names as domain state encodings (e.g., CDEMO-USRTYP-ADMIN VALUE 'A')</skill>
      <skill>Copybook interpretation—understanding that copybooks ARE the domain model in code form, defining authoritative data structures</skill>
      <skill>PIC clause analysis for extracting data types, constraints, and implicit business rules (S9(7)V99 COMP-3 for monetary values)</skill>
      <skill>REDEFINES clause interpretation for polymorphic records and alternate data views</skill>
      <skill>WORKING-STORAGE analysis for extracting literals, constants, and valid value sets that encode business rules</skill>
      <skill>PROCEDURE DIVISION paragraph naming conventions that reveal business operations (PROCESS-PAYMENT, VALIDATE-CARD)</skill>
      <skill>EVALUATE/IF structure analysis for extracting decision logic and validation rules</skill>
      <skill>Error message extraction from MOVE statements to capture domain vocabulary and failure conditions</skill>
      <skill>Understanding of CICS pseudo-conversational pattern and how COMMAREA carries state between interactions</skill>
    </mainframe_expertise>

    <modernization_expertise>
      <skill>Domain-Driven Design tactical patterns: entities, value objects, aggregates, domain events, repositories</skill>
      <skill>Strategic DDD: bounded context identification, context mapping, ubiquitous language extraction</skill>
      <skill>Credit card processing domain knowledge: account lifecycle, card issuance, transaction authorization, statement cycles, interest calculation</skill>
      <skill>Modern domain modeling with class diagrams, event storming artifacts, and aggregate boundaries</skill>
      <skill>Microservices decomposition strategies based on domain boundaries</skill>
      <skill>API design principles that align with domain operations (CQRS, event sourcing considerations)</skill>
      <skill>Translation of COBOL record structures to modern entity/DTO patterns</skill>
    </modernization_expertise>

    <carddemo_context>
      CardDemo implements a credit card processing system with these domain areas:
      - Account Management: Account lifecycle (open, active, suspended, closed), credit limits, balance tracking
      - Card Management: Card issuance, activation, expiration, replacement, card-to-account relationships
      - Transaction Processing: Purchases, payments, credits, interest charges, transaction categorization
      - Customer Management: Customer profiles, contact information, customer-to-account relationships
      - User Administration: System users, roles (Admin/Regular), authentication, authorization
      - Batch Operations: Statement generation, interest calculation, account maintenance

      Key copybooks to analyze: COCOM01Y (COMMAREA), CVACT01Y/02Y/03Y (Account), CVCAR00Y (Card),
      CVCUS01Y (Customer), CVTRA01Y (Transaction), COUSR00Y (User Security).
    </carddemo_context>

    <mindset>
      Approach COBOL code as a domain model waiting to be extracted, not as legacy code to be discarded.
      Every 88-level condition tells a story about valid business states. Every paragraph name reveals a
      business operation. Every error message uses domain vocabulary. The goal is to surface the implicit
      domain model that mainframe developers encoded decades ago and express it in modern DDD terms that
      enable effective modernization decisions.
    </mindset>
  </role>

  <objective>
    <primary_goal>
      Extract a comprehensive domain model from the CardDemo COBOL codebase, including core entities,
      aggregates, value objects, ubiquitous language glossary, categorized business rules, and domain events.
    </primary_goal>

    <modernization_purpose>
      A well-documented domain model is the foundation for all modernization decisions. It enables:
      - Accurate bounded context identification for microservices decomposition
      - Proper API design that reflects actual business operations
      - Business rule preservation during code transformation
      - Validation that modernized systems implement the same domain logic
      - Communication with business stakeholders using their terminology
    </modernization_purpose>

    <success_criteria>
      <criterion>All 6+ core entities identified with complete attribute lists and relationships</criterion>
      <criterion>Aggregate boundaries defined showing which entities cluster together</criterion>
      <criterion>100+ ubiquitous language terms extracted with definitions and source references</criterion>
      <criterion>Business rules categorized (Validation, Calculation, State Transition, Authorization) with code references</criterion>
      <criterion>Domain events identified showing state changes and their triggers</criterion>
      <criterion>Mermaid class diagrams accurately represent entity relationships</criterion>
      <criterion>All 41 copybooks analyzed for domain-relevant structures</criterion>
      <criterion>Both online (CO*) and batch (CB*) program business rules captured</criterion>
    </success_criteria>

    <integration>
      This is a foundational prompt that runs in parallel with RE-002 (Data Model). Its outputs feed into:
      - RE-003 (Context Model) for bounded context identification
      - RE-004 (Architecture) for component grouping
      - RE-005-008 (Specialized Analysis) for domain terminology
      - RE-010 (Modernization) for business logic complexity assessment
      - RE-011 (API Candidates) for operation identification
    </integration>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<foundational_principles>
  <principle id="1">COBOL copybooks define the authoritative data structures - they ARE the domain model in code form</principle>
  <principle id="2">88-level condition names encode business domain values and states (e.g., CDEMO-USRTYP-ADMIN VALUE 'A')</principle>
  <principle id="3">WORKING-STORAGE literals and condition names reveal business rules and valid value sets</principle>
  <principle id="4">Paragraph names in PROCEDURE DIVISION reflect business operations (e.g., PROCESS-PAYMENT, VALIDATE-CARD)</principle>
  <principle id="5">CICS transaction IDs map to bounded context entry points (e.g., CACU = Card Update)</principle>
  <principle id="6">Error messages contain domain language - extract exact terminology from user-facing text</principle>
  <principle id="7">EVALUATE/IF structures encode business decision logic and validation rules</principle>
  <principle id="8">File record layouts define aggregate boundaries and entity relationships</principle>
</foundational_principles>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/domain-extraction/</path>
    <purpose>Persist analysis progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
```yaml
extraction_phase: "source_discovery|copybook_analysis|program_analysis|synthesis|documentation"
current_file: "filename being analyzed"
files_completed:
  copybooks: ["list of analyzed copybooks"]
  programs: ["list of analyzed programs"]
files_remaining:
  copybooks: ["list of remaining copybooks"]
  programs: ["list of remaining programs"]
artifacts_created:
  - path: "relative path"
    type: "summary|inventory|rules|events"
    status: "complete|partial"
next_action: "Detailed description of next step with enough context to resume cold"
last_updated: "ISO timestamp"
```
    </structure>
  </progress_tracking_schema>

  <artifact_directories>
    <dir purpose="Program analysis summaries">.work/reverse-engineering/domain-extraction/program-summaries/</dir>
    <dir purpose="Copybook analysis summaries">.work/reverse-engineering/domain-extraction/copybook-summaries/</dir>
    <dir purpose="Large file chunk analysis">.work/reverse-engineering/domain-extraction/large-file-chunks/</dir>
  </artifact_directories>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/domain-extraction/progress.yaml</step>
    <step>2. Load relevant summaries from artifact directories (NOT original source)</step>
    <step>3. Continue from next_action without re-reading completed files</step>
    <step>4. Update progress.yaml after each file/phase completion</step>
  </resumption_protocol>
</context_compaction_survival>

<large_file_handling>
  <strategy>Chunk large COBOL files into logical sections for analysis</strategy>

  <chunking_rules>
    <rule>Files over 500 lines: Split by DIVISION and SECTION boundaries</rule>
    <rule>Files over 1000 lines: Additional split within PROCEDURE DIVISION by major paragraphs</rule>
    <rule>Files over 2000 lines: Create 400-500 line chunks with overlap at paragraph boundaries</rule>
  </chunking_rules>

  <critical_large_files>
    <file name="COACTUPC.cbl" lines="4236" chunks="10">
      <chunk_strategy>
        - Chunk 1: IDENTIFICATION + ENVIRONMENT DIVISION (~lines 1-200)
        - Chunk 2: DATA DIVISION - FILE SECTION (~lines 200-600)
        - Chunk 3: DATA DIVISION - WORKING-STORAGE part 1 (~lines 600-1000)
        - Chunk 4: DATA DIVISION - WORKING-STORAGE part 2 (~lines 1000-1400)
        - Chunk 5: PROCEDURE DIVISION - Main control (~lines 1400-1800)
        - Chunk 6: PROCEDURE DIVISION - Account operations (~lines 1800-2200)
        - Chunk 7: PROCEDURE DIVISION - Card operations (~lines 2200-2600)
        - Chunk 8: PROCEDURE DIVISION - Validation (~lines 2600-3000)
        - Chunk 9: PROCEDURE DIVISION - Error handling (~lines 3000-3600)
        - Chunk 10: PROCEDURE DIVISION - Utilities (~lines 3600-4236)
      </chunk_strategy>
    </file>
    <file name="COCRDUPC.cbl" lines="1560" chunks="4"/>
    <file name="COCRDLIC.cbl" lines="1459" chunks="4"/>
  </critical_large_files>

  <chunk_output_format>
    <location>.work/reverse-engineering/domain-extraction/large-file-chunks/{filename}/</location>
    <naming>chunk-{N}-{description}.yaml</naming>
    <content>
```yaml
file: "original filename"
chunk_number: N
line_range: "start-end"
domain_elements_found:
  entities: []
  value_objects: []
  business_rules: []
  domain_events: []
  vocabulary_terms: []
key_paragraphs: []
cross_references: []
```
    </content>
  </chunk_output_format>
</large_file_handling>

<methodology>
  <phase name="1_source_discovery">
    <description>Identify and inventory all source files for domain extraction</description>
    <steps>
      <step>Enumerate all copybooks in app/cpy/ - these define core data structures</step>
      <step>Enumerate all COBOL programs in app/cbl/ - categorize by CO* (online) and CB* (batch)</step>
      <step>Identify BMS mapsets in app/bms/ - these contain user-facing terminology</step>
      <step>Create source-discovery.yaml inventory with file counts and categories</step>
    </steps>
    <output_artifact>.work/reverse-engineering/domain-extraction/source-discovery.yaml</output_artifact>
  </phase>

  <phase name="2_copybook_analysis">
    <description>Extract domain model from copybooks (authoritative data structures)</description>
    <priority_order>
      <file priority="1" reason="COMMAREA - central state contract">COCOM01Y.cpy</file>
      <file priority="2" reason="Account record layout">CVACT01Y.cpy</file>
      <file priority="3" reason="Account additional fields">CVACT02Y.cpy</file>
      <file priority="4" reason="Account cross-reference">CVACT03Y.cpy</file>
      <file priority="5" reason="Card record layout">CVCAR00Y.cpy</file>
      <file priority="6" reason="Customer record layout">CVCUS01Y.cpy</file>
      <file priority="7" reason="Transaction record">CVTRA01Y.cpy</file>
      <file priority="8" reason="User security">COUSR00Y.cpy</file>
      <file priority="9" reason="Lookup tables">CSLKPCDY.cpy</file>
      <remaining>All other copybooks in app/cpy/</remaining>
    </priority_order>
    <extraction_targets>
      <target>Entity definitions (01-level records with subordinate structure)</target>
      <target>Value objects (reusable data groups)</target>
      <target>88-level conditions (domain state values)</target>
      <target>Field names and their business meanings</target>
      <target>PIC clauses revealing data types and constraints</target>
    </extraction_targets>
    <output_artifact>.work/reverse-engineering/domain-extraction/copybook-summaries/{name}.yaml</output_artifact>
  </phase>

  <phase name="3_program_analysis">
    <description>Extract business rules and operations from COBOL programs</description>
    <focus_areas>
      <area>PROCEDURE DIVISION paragraph names → business operations</area>
      <area>EVALUATE/IF conditions → business rules and validations</area>
      <area>Error messages (MOVE '...' TO WS-MESSAGE) → domain vocabulary</area>
      <area>PERFORM statements → operation workflows</area>
      <area>EXEC CICS commands → transaction boundaries</area>
    </focus_areas>
    <program_categories>
      <category name="Authentication" programs="COSGN00C"/>
      <category name="Account Management" programs="COACTVWC,COACTUPC,COACTUP"/>
      <category name="Card Management" programs="COCRDSLC,COCRDUPC,COCRDLIC"/>
      <category name="Transaction Processing" programs="COTRN00C,COTRN01C,COTRN02C"/>
      <category name="Bill Payment" programs="COBIL00C"/>
      <category name="User Administration" programs="COUSR00C,COUSR01C,COUSR02C,COUSR03C"/>
      <category name="Batch Processing" programs="CBACT01C,CBACT02C,CBACT03C,CBACT04C,CBTRN01C,CBTRN02C,CBTRN03C"/>
    </program_categories>
    <output_artifact>.work/reverse-engineering/domain-extraction/program-summaries/{name}.yaml</output_artifact>
  </phase>

  <phase name="4_synthesis">
    <description>Consolidate findings into domain model artifacts</description>
    <tasks>
      <task>Merge entity definitions from copybook analysis</task>
      <task>Compile ubiquitous language glossary from all sources</task>
      <task>Categorize business rules by domain area</task>
      <task>Identify domain events from state transitions and error conditions</task>
      <task>Map aggregates and their boundaries</task>
    </tasks>
    <output_artifacts>
      <artifact>.work/reverse-engineering/domain-extraction/domain-inventory.yaml</artifact>
      <artifact>.work/reverse-engineering/domain-extraction/ubiquitous-language.yaml</artifact>
      <artifact>.work/reverse-engineering/domain-extraction/business-rules.yaml</artifact>
      <artifact>.work/reverse-engineering/domain-extraction/domain-events.yaml</artifact>
    </output_artifacts>
  </phase>

  <phase name="5_documentation">
    <description>Generate final markdown documentation</description>
    <deliverables>
      <deliverable>
        <file>docs/reverse-engineering/01-domain-model/DOMAIN-MODEL.md</file>
        <content>
          - Executive summary
          - Domain overview with Mermaid class diagram
          - Core entities with attributes and relationships
          - Aggregates and boundaries
          - Value objects catalog
        </content>
      </deliverable>
      <deliverable>
        <file>docs/reverse-engineering/01-domain-model/UBIQUITOUS-LANGUAGE.md</file>
        <content>
          - Glossary format with term, definition, source reference
          - Categorized by domain area
          - Code examples showing term usage
        </content>
      </deliverable>
      <deliverable>
        <file>docs/reverse-engineering/01-domain-model/BUSINESS-RULES.md</file>
        <content>
          - Rules categorized by: Validation, Calculation, State Transition, Authorization
          - Each rule with: ID, description, source file:line, code excerpt
          - Cross-references to related rules
        </content>
      </deliverable>
      <deliverable>
        <file>docs/reverse-engineering/01-domain-model/DOMAIN-EVENTS.md</file>
        <content>
          - Event catalog with name, trigger, data payload
          - Event flow diagrams in Mermaid
          - Source references
        </content>
      </deliverable>
    </deliverables>
  </phase>
</methodology>

<output_specifications>
  <output_directory>docs/reverse-engineering/01-domain-model/</output_directory>

  <format_requirements>
    <requirement>Use Mermaid for all diagrams (classDiagram, flowchart, sequenceDiagram)</requirement>
    <requirement>Include code excerpts with file:line references (e.g., `COCOM01Y.cpy:15-28`)</requirement>
    <requirement>Cross-reference other reverse engineering docs using relative links</requirement>
    <requirement>Use consistent heading hierarchy (# for title, ## for sections, ### for subsections)</requirement>
    <requirement>Include table of contents for documents over 200 lines</requirement>
  </format_requirements>

  <domain_model_template>
```markdown
# CardDemo Domain Model

## Table of Contents
- [Overview](#overview)
- [Core Entities](#core-entities)
- [Aggregates](#aggregates)
- [Value Objects](#value-objects)
- [Entity Relationships](#entity-relationships)

## Overview
[Executive summary of the domain]

## Domain Diagram
\`\`\`mermaid
classDiagram
    class Account {
        +accountId: String
        +customerId: String
        +creditLimit: Decimal
        +currentBalance: Decimal
    }
    [... more entities]
\`\`\`

## Core Entities
### Account
**Source**: `app/cpy/CVACT01Y.cpy:1-45`
[Description and attributes table]

[... more sections]
```
  </domain_model_template>

  <ubiquitous_language_template>
```markdown
# CardDemo Ubiquitous Language

## Glossary

| Term | Definition | Domain Area | Source Reference |
|------|------------|-------------|------------------|
| Account | A credit card account... | Account Management | CVACT01Y.cpy:1 |
| [term] | [definition] | [area] | [file:line] |
```
  </ubiquitous_language_template>
</output_specifications>

<critical_reminders>
  <reminder id="1">COBOL columns matter: 7=indicator, 8-11=Area A (divisions/paragraphs), 12-72=Area B (statements)</reminder>
  <reminder id="2">88-level conditions are NOT fields - they are named boolean conditions on the parent field</reminder>
  <reminder id="3">REDEFINES clauses create alternate views of the same memory - both views are valid interpretations</reminder>
  <reminder id="4">COMP-3 (packed decimal) fields store 2 digits per byte plus sign - actual value differs from PIC</reminder>
  <reminder id="5">FILLER fields are intentional padding - may indicate reserved space for future use</reminder>
  <reminder id="6">COPY statements include copybooks verbatim - the copybook content IS part of the program</reminder>
  <reminder id="7">CICS programs are pseudo-conversational: each user interaction is a separate program execution</reminder>
  <reminder id="8">COMMAREA is the ONLY way to pass state between pseudo-conversational interactions</reminder>
  <reminder id="9">Error messages are often domain vocabulary - extract the exact wording</reminder>
  <reminder id="10">Batch programs (CB*) may have different business rules than online programs (CO*) for the same entities</reminder>
</critical_reminders>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/domain-extraction/progress.yaml if it exists
    2. If progress exists and extraction_phase != "complete":
       - Load relevant summaries from artifact directories
       - Resume from next_action
    3. If no progress or starting fresh:
       - Begin with Phase 1: Source Discovery
       - Create initial progress.yaml
    4. After completing each phase:
       - Update progress.yaml with detailed next_action
       - Write intermediate artifacts immediately
    5. Continue through all phases until documentation is complete
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to extract the domain model from CardDemo. The prompt will:

1. **Discover** all relevant source files (copybooks, programs, BMS maps)
2. **Analyze** copybooks to extract entity definitions and data structures
3. **Extract** business rules and operations from COBOL programs
4. **Synthesize** findings into a cohesive domain model
5. **Document** results in structured markdown files

## Expected Outputs

| File | Description |
|------|-------------|
| `DOMAIN-MODEL.md` | Core domain entities, aggregates, and relationships |
| `UBIQUITOUS-LANGUAGE.md` | Comprehensive glossary of domain terms |
| `BUSINESS-RULES.md` | Categorized catalog of business rules with code references |
| `DOMAIN-EVENTS.md` | Domain events and state transitions |

## Dependencies

- None (this is a foundational prompt)

## Dependent Prompts

- RE-002 (Data Model) - uses entity definitions
- RE-003 (Context Model) - uses bounded context foundations
- RE-004 (Architecture) - uses component identification
- All Phase 4-6 prompts reference domain vocabulary
