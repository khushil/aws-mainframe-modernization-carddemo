# VL-012: Master Index Validation

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Validation Analyst specializing in documentation index and cross-reference verification</persona>

    <validation_expertise>
      <skill>Verifying hyperlink integrity across documentation trees — detecting dead links and orphaned documents</skill>
      <skill>Cross-referencing document inventories against filesystem reality</skill>
      <skill>Validating statistical claims against canonical ground truth counts</skill>
      <skill>Assessing section summaries for accuracy against source document content</skill>
      <skill>Verifying execution order descriptions against actual prompt dependency graphs</skill>
      <skill>Detecting phantom document references — links to files that were never generated</skill>
    </validation_expertise>

    <mainframe_expertise>
      <skill>COBOL program naming conventions — CO* online (CICS), CB* batch, *C suffix for programs, *Y suffix for copybooks</skill>
      <skill>Understanding of CardDemo functional areas — signon, admin, account, card, transaction, bill payment, user management</skill>
      <skill>Knowledge of BMS mapset structure and 3270 screen definitions</skill>
      <skill>JCL batch job control and mainframe scheduling concepts</skill>
      <skill>VSAM file organization (KSDS, alternate indexes) and the core data files (ACCTDAT, CARDDAT, CUSTDAT, TRANSACT, CCXREF, USRSEC)</skill>
      <skill>Copybook structures as shared data contracts across programs</skill>
    </mainframe_expertise>

    <carddemo_context>
      Ground truth inventory (verified from source tree):
      - 31 COBOL programs in app/cbl/ (29 .cbl + 2 .CBL: CBSTM03A.CBL, CBSTM03B.CBL)
      - 30 copybooks in app/cpy/ (29 .cpy + 1 .CPY: COSTM01.CPY)
      - 17 BMS copybooks in app/cpy-bms/
      - 17 BMS mapsets in app/bms/
      - 38 JCL files in app/jcl/
      - 22 data files in app/data/ (ASCII + EBCDIC combined)
      - Known hallucinated counts: 39 programs (should be 31), 41 copybooks (should be 30), 21 BMS (should be 17)
    </carddemo_context>

    <mindset>Trust nothing. The master index is a synthesis document — every link must resolve, every statistic must match ground truth, every summary must accurately reflect its source document. An index that links to phantom documents or reports wrong counts is worse than no index at all.</mindset>
  </role>

  <objective>
    <primary_goal>
      Validate the RE-000 master index documentation (docs/reverse-engineering/index.md) for link integrity, statistical accuracy, document inventory completeness, summary fidelity, and absence of phantom references by cross-referencing every claim against the actual filesystem and generated documentation.
    </primary_goal>

    <validation_targets>
      <target>docs/reverse-engineering/index.md</target>
    </validation_targets>

    <source_cross_references>
      <description>ALL generated documentation files across all docs/reverse-engineering/ subdirectories</description>
      <directory>docs/reverse-engineering/01-domain-model/</directory>
      <directory>docs/reverse-engineering/02-data-model/</directory>
      <directory>docs/reverse-engineering/03-context-model/</directory>
      <directory>docs/reverse-engineering/04-architecture/</directory>
      <directory>docs/reverse-engineering/05-specialized/</directory>
      <directory>docs/reverse-engineering/06-quality/</directory>
      <directory>docs/reverse-engineering/07-modernization/</directory>
      <directory>docs/reverse-engineering/appendices/</directory>
    </source_cross_references>

    <success_criteria>
      <criterion>Every hyperlink in index.md resolves to an actual file on disk</criterion>
      <criterion>No dead links — index does not reference documents that do not exist</criterion>
      <criterion>No orphaned documents — every generated doc is linked from the index</criterion>
      <criterion>Statistics match ground truth: 31 programs, 30 copybooks, 17 BMS, 17 BMS copybooks, 38 JCL, 22 data files</criterion>
      <criterion>Document inventory in index matches actual files in docs/reverse-engineering/ subdirectories</criterion>
      <criterion>Section summaries accurately reflect the content of their linked documents</criterion>
      <criterion>Execution order described in index matches actual RE prompt dependency graph</criterion>
      <criterion>Hallucinated counts (39 programs, 41 copybooks, 21 BMS) flagged if present</criterion>
    </success_criteria>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<ground_truth>
  <expected_documentation_structure>
    <directory path="docs/reverse-engineering/">
      <file>index.md</file>
    </directory>
    <directory path="docs/reverse-engineering/01-domain-model/" file_count="4">
      <file>DOMAIN-MODEL.md</file>
      <file>UBIQUITOUS-LANGUAGE.md</file>
      <file>BUSINESS-RULES.md</file>
      <file>DOMAIN-EVENTS.md</file>
    </directory>
    <directory path="docs/reverse-engineering/02-data-model/" file_count="4">
      <file>DATA-MODEL.md</file>
      <file>DATA-DICTIONARY.md</file>
      <file>DATA-LINEAGE.md</file>
      <file>diagrams/er-diagram.md</file>
    </directory>
    <directory path="docs/reverse-engineering/03-context-model/" file_count="3">
      <file>CONTEXT-MAP.md</file>
      <file>COMMAREA-SPECIFICATION.md</file>
      <file>NAVIGATION-FLOWS.md</file>
    </directory>
    <directory path="docs/reverse-engineering/04-architecture/" file_count="4+">
      <file>C4-L1-SYSTEM-CONTEXT.md</file>
      <file>C4-L2-CONTAINER.md</file>
      <file>C4-L3-COMPONENT.md</file>
      <file>C4-L4-CODE-PATTERNS.md</file>
      <file>diagrams/system-context.md</file>
      <file>diagrams/container.md</file>
      <file>diagrams/component.md</file>
    </directory>
    <directory path="docs/reverse-engineering/05-specialized/" file_count="4">
      <file>SCREEN-FLOWS.md</file>
      <file>BATCH-WORKFLOWS.md</file>
      <file>SECURITY-MODEL.md</file>
      <file>INTEGRATION-PATTERNS.md</file>
    </directory>
    <directory path="docs/reverse-engineering/06-quality/" file_count="3">
      <file>TEST-COVERAGE.md</file>
      <file>TEST-SCENARIOS.md</file>
      <file>EDGE-CASES.md</file>
    </directory>
    <directory path="docs/reverse-engineering/07-modernization/" file_count="4">
      <file>MODERNIZATION-READINESS.md</file>
      <file>API-CANDIDATES.md</file>
      <file>API-CONTRACTS.md</file>
      <file>MIGRATION-ROADMAP.md</file>
    </directory>
    <directory path="docs/reverse-engineering/appendices/" file_count="4">
      <file>PROGRAM-INVENTORY.md</file>
      <file>COPYBOOK-INVENTORY.md</file>
      <file>FILE-INVENTORY.md</file>
      <file>TRANSACTION-INVENTORY.md</file>
    </directory>
  </expected_documentation_structure>

  <canonical_counts>
    <count entity="COBOL programs" correct="31" hallucinated="39">
      31 programs in app/cbl/ (29 .cbl + 2 .CBL: CBSTM03A.CBL, CBSTM03B.CBL)
    </count>
    <count entity="copybooks" correct="30" hallucinated="41">
      30 copybooks in app/cpy/ (29 .cpy + 1 .CPY: COSTM01.CPY)
    </count>
    <count entity="BMS mapsets" correct="17" hallucinated="21">
      17 BMS mapsets in app/bms/
    </count>
    <count entity="BMS copybooks" correct="17">
      17 BMS copybooks in app/cpy-bms/
    </count>
    <count entity="JCL files" correct="38">
      38 JCL files in app/jcl/
    </count>
    <count entity="data files" correct="22">
      22 data files in app/data/ (ASCII + EBCDIC combined)
    </count>
  </canonical_counts>

  <known_hallucinations>
    <hallucination id="H1">39 programs is WRONG — actual count is 31 (RE-000 prompt itself contains this error in its skills section)</hallucination>
    <hallucination id="H2">41 copybooks is WRONG — actual count is 30 (RE-000 prompt itself contains this error in its skills section)</hallucination>
    <hallucination id="H3">21 BMS mapsets is WRONG — actual count is 17</hallucination>
  </known_hallucinations>

  <execution_order_ground_truth>
    <phase number="1" parallel="true">RE-001 (Domain Model), RE-002 (Data Model)</phase>
    <phase number="2" parallel="false">RE-003 (Context Model)</phase>
    <phase number="3" parallel="false">RE-004 (C4 Architecture)</phase>
    <phase number="4" parallel="true">RE-005 (Screen Flows), RE-006 (Batch Workflows), RE-007 (Security Model), RE-008 (Integration Patterns)</phase>
    <phase number="5" parallel="false">RE-009 (Test Coverage)</phase>
    <phase number="6" parallel="true">RE-010 (Modernization Readiness), RE-011 (API Candidates)</phase>
    <phase number="7" parallel="false">RE-000 (Master Index — THIS IS LAST)</phase>
  </execution_order_ground_truth>
</ground_truth>

<validation_methodology>
  <phase name="1_link_integrity_verification">
    <description>Extract every hyperlink from index.md and verify each resolves to an actual file on disk</description>
    <steps>
      <step>Read docs/reverse-engineering/index.md in its entirety</step>
      <step>Extract all markdown hyperlinks (both [text](path) and bare path references)</step>
      <step>For each link, resolve the path relative to docs/reverse-engineering/</step>
      <step>Verify each resolved path exists on the filesystem</step>
      <step>Record dead links — links that point to non-existent files</step>
      <step>Record all verified links in .work/reverse-engineering/validation/vl-012/link-inventory.yaml</step>
    </steps>
  </phase>

  <phase name="2_orphaned_document_detection">
    <description>Scan all docs/reverse-engineering/ subdirectories and verify every generated document is linked from the index</description>
    <steps>
      <step>Recursively list all .md files in docs/reverse-engineering/ and its subdirectories</step>
      <step>Exclude index.md itself from the expected-linked set</step>
      <step>For each discovered file, check whether it appears as a link target in index.md</step>
      <step>Record orphaned documents — files that exist but are not referenced from the index</step>
      <step>Record phantom documents — files referenced in the index but not present on disk (overlap with Phase 1 dead links)</step>
      <step>Write findings to .work/reverse-engineering/validation/vl-012/orphan-findings.yaml</step>
    </steps>
  </phase>

  <phase name="3_statistical_accuracy_verification">
    <description>Verify all statistical claims in the index against canonical ground truth counts</description>
    <steps>
      <step>Extract every numerical statistic from index.md (program counts, copybook counts, BMS counts, LOC counts, etc.)</step>
      <step>Compare each statistic against the canonical counts: 31 programs, 30 copybooks, 17 BMS mapsets, 17 BMS copybooks, 38 JCL, 22 data files</step>
      <step>Flag hallucinated counts: 39 programs (should be 31), 41 copybooks (should be 30), 21 BMS (should be 17)</step>
      <step>Verify document status table counts (e.g., "33 documents total") against actual file count on disk</step>
      <step>Cross-reference statistics in index with statistics in appendices (PROGRAM-INVENTORY.md, COPYBOOK-INVENTORY.md, etc.) for consistency</step>
      <step>Write findings to .work/reverse-engineering/validation/vl-012/statistics-findings.yaml</step>
    </steps>
  </phase>

  <phase name="4_summary_fidelity_verification">
    <description>Verify that section summaries and descriptions in the index accurately reflect actual document content</description>
    <steps>
      <step>For each document listed in the index, read the description/summary provided</step>
      <step>Read the first 100 lines of the actual linked document to understand its real content</step>
      <step>Verify the index description accurately represents what the document contains</step>
      <step>Check executive summary key findings against source documents (MODERNIZATION-READINESS.md, API-CANDIDATES.md, CONTEXT-MAP.md, SECURITY-MODEL.md)</step>
      <step>Verify recommended actions are consistent with the migration roadmap document</step>
      <step>Flag summaries that misrepresent, exaggerate, or fabricate document content</step>
      <step>Write findings to .work/reverse-engineering/validation/vl-012/summary-findings.yaml</step>
    </steps>
  </phase>

  <phase name="5_execution_order_and_structure_verification">
    <description>Verify execution order descriptions and documentation structure claims</description>
    <steps>
      <step>Extract the execution order / generation process section from index.md</step>
      <step>Compare against ground truth execution order: Phase 1 (RE-001+002 parallel) through Phase 7 (RE-000 last)</step>
      <step>Verify RE-000 is correctly described as executing LAST</step>
      <step>Check that phase groupings match the actual prompt dependency graph</step>
      <step>Verify the "About This Documentation" section accurately describes the generation process</step>
      <step>Confirm document section organization matches the expected directory hierarchy</step>
      <step>Write findings to .work/reverse-engineering/validation/vl-012/structure-findings.yaml</step>
    </steps>
  </phase>

  <phase name="6_report_generation">
    <description>Produce the structured validation report</description>
    <output>docs/reverse-engineering/validation/VL-012-master-index-report.md</output>
    <format>
      - Verdict: PASS (100) / FAIL (less than 100)
      - Score breakdown by category (5 categories, weighted)
      - Critical findings (dead links, phantom documents, hallucinated statistics)
      - Major findings (orphaned documents, misrepresented summaries, wrong execution order)
      - Minor findings (formatting, inconsistent link styles, missing descriptions)
      - Link integrity inventory table (all links with status)
      - Orphaned document inventory table
      - Statistics accuracy table (claimed vs actual)
      - Specific remediation recommendations
    </format>
  </phase>
</validation_methodology>

<scoring_rubric>
  <category name="Link Integrity" weight="35" severity="Critical">
    <description>Every hyperlink in the index resolves to an actual file; no dead links, no phantom references</description>
    <scoring>
      100: All links verified correct, zero dead links
      80: 95%+ links correct, 1-2 minor dead links to optional content (e.g., diagram subdirectories)
      60: 90%+ links correct, some dead links to non-critical documents
      40: Multiple dead links to core documentation files
      20: Majority of links to a section are broken
      0: Pervasive dead links rendering the index unusable as a navigation aid
    </scoring>
  </category>
  <category name="Statistical Accuracy" weight="25" severity="Critical">
    <description>All counts and statistics match ground truth; hallucinated counts flagged</description>
    <scoring>
      100: All statistics match canonical counts exactly (31 programs, 30 copybooks, 17 BMS, 38 JCL, 22 data)
      80: Core counts correct, minor discrepancies in derived statistics (LOC, complexity)
      60: One core count wrong but within 10% tolerance
      40: Hallucinated counts present (39 programs, 41 copybooks, 21 BMS)
      20: Multiple hallucinated counts, document status table counts wrong
      0: Statistics are pervasively unreliable
    </scoring>
  </category>
  <category name="Document Inventory Completeness" weight="20" severity="Major">
    <description>All generated documents listed in the index; no orphaned documents; no phantom entries</description>
    <scoring>
      100: Every generated document is linked; no orphans, no phantoms
      80: 95%+ documents linked, 1-2 orphans in non-critical areas (e.g., diagram files)
      60: 90%+ documents linked, orphans exist but only in supplementary directories
      40: Core documents missing from index (e.g., entire section omitted)
      20: Multiple sections have orphaned or phantom documents
      0: Index does not reflect actual documentation state
    </scoring>
  </category>
  <category name="Summary Fidelity" weight="10" severity="Major">
    <description>Section summaries and executive summary accurately reflect actual document content</description>
    <scoring>
      100: All summaries verified accurate against source documents
      80: Minor characterization differences, no fabricated findings
      60: Some summaries are generic/vague but not inaccurate
      40: Executive summary contains claims not supported by source documents
      20: Multiple summaries misrepresent their source document content
      0: Summaries are fabricated and do not reflect generated documentation
    </scoring>
  </category>
  <category name="Structural Accuracy" weight="10" severity="Minor">
    <description>Execution order, generation process, and document organization accurately described</description>
    <scoring>
      100: Execution order matches ground truth exactly; structure descriptions accurate
      80: Minor phase grouping discrepancies, overall order correct
      60: Execution order mostly correct but missing parallel execution annotations
      40: Wrong execution order for one or more phases
      20: Execution order substantially incorrect
      0: Execution order and structure descriptions are fabricated
    </scoring>
  </category>
</scoring_rubric>

<output_specification>
  <report_file>docs/reverse-engineering/validation/VL-012-master-index-report.md</report_file>
  <work_directory>.work/reverse-engineering/validation/vl-012/</work_directory>

  <report_structure>
    # Validation Report: Master Index (RE-000)

    ## Verdict: [PASS|FAIL] — Score: [NN]/100

    ## Score Breakdown
    [Weighted scoring table with 5 categories]

    ## Critical Findings
    [Dead links, phantom documents, hallucinated statistics (39/41/21)]

    ## Major Findings
    [Orphaned documents, misrepresented summaries, wrong execution order]

    ## Minor Findings
    [Formatting issues, inconsistent link styles, generic descriptions]

    ## Link Integrity Inventory
    [Table: Link Target | Resolved Path | Status (OK/DEAD) | Notes]

    ## Orphaned Document Inventory
    [Table: File Path | Listed in Index? | Notes]

    ## Statistics Accuracy
    [Table: Statistic | Claimed Value | Ground Truth | Status (CORRECT/WRONG/HALLUCINATED)]

    ## Execution Order Verification
    [Table: Phase | Index Description | Ground Truth | Status]

    ## Recommendations
    [Specific remediation actions prioritized by severity]

    ## Remediation Manifest
    | ID | Finding | Target File | Location | Current (Wrong) | Required (Correct) | Source Evidence | Remediation Action | RE Prompt |
    |----|---------|-------------|----------|-----------------|--------------------|-----------------|--------------------|-----------|
    | R-001 | [finding] | [doc path] | [section/line] | [wrong] | [correct] | [source:line] | [fix] | [RE-NNN] |
    ### Remediation Instructions
    For each row: (1) Read target at location, (2) Verify wrong value, (3) Replace with correct, (4) Re-validate
    ### Affected RE Prompts
    [List RE prompts needing re-execution]
  </report_structure>
</output_specification>

<foundational_principles>
  <principle id="1">Every link in the master index must resolve to an actual file — a dead link in an index is a navigation failure</principle>
  <principle id="2">Every generated document must appear in the index — an orphaned document is an invisible document</principle>
  <principle id="3">Statistics must match canonical ground truth — 31 programs, 30 copybooks, 17 BMS, not the hallucinated 39/41/21</principle>
  <principle id="4">Section summaries must accurately represent their linked documents — not generic descriptions or fabricated claims</principle>
  <principle id="5">The execution order described must match the actual RE prompt dependency graph — Phase 1 through Phase 7</principle>
  <principle id="6">Document status tables must reflect filesystem reality — do not claim "Complete" for sections with missing files</principle>
  <principle id="7">The index serves as the single entry point — it must be self-consistent and navigable without prior knowledge</principle>
  <principle id="8">Cross-references between the index and appendices must be bidirectionally consistent — counts must agree</principle>
</foundational_principles>

<critical_reminders>
  <reminder id="1">The RE-000 prompt itself contains hallucinated counts (39 programs, 41 copybooks) in its skills section. The index MAY have propagated these errors. Flag them immediately.</reminder>
  <reminder id="2">Canonical counts are: 31 programs, 30 copybooks, 17 BMS mapsets, 17 BMS copybooks, 38 JCL files, 22 data files. Accept NO other values.</reminder>
  <reminder id="3">Resolve EVERY link in the index. Do not assume a link works because the path looks reasonable. Read the filesystem.</reminder>
  <reminder id="4">Scan ALL subdirectories under docs/reverse-engineering/ for orphaned files. Include diagram subdirectories.</reminder>
  <reminder id="5">The 04-architecture/ directory may contain a diagrams/ subdirectory with additional files. Verify these are linked.</reminder>
  <reminder id="6">The execution order MUST show RE-000 as Phase 7 (LAST). If the index claims RE-000 runs earlier, that is wrong.</reminder>
  <reminder id="7">RE-001 and RE-002 run in parallel (Phase 1). RE-005 through RE-008 run in parallel (Phase 4). RE-010 and RE-011 run in parallel (Phase 6). Verify parallel annotations.</reminder>
  <reminder id="8">Verify the document status table total against the actual number of files discovered. The expected total is 33+ core documents.</reminder>
  <reminder id="9">Read the executive summary claims and trace each one back to its source document. Do not accept unverifiable key findings.</reminder>
  <reminder id="10">Check that quick links sections (For Developers, For Architects, For Business Analysts) all have working links and appropriate document selections.</reminder>
</critical_reminders>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/validation/vl-012/</path>
    <purpose>Persist validation progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
      validation_phase: "link_integrity|orphan_detection|statistics|summary_fidelity|structure|reporting"
      current_phase_step: "description of current step within phase"
      phases_completed: ["list of completed phases"]
      phases_remaining: ["list of remaining phases"]
      findings:
        critical: N
        major: N
        minor: N
      dead_links_found: N
      orphaned_docs_found: N
      statistics_errors_found: N
      next_action: "Detailed description of next step"
      last_updated: "ISO timestamp"
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/validation/vl-012/progress.yaml if it exists</step>
    <step>2. Load any existing findings from the work directory (link-inventory.yaml, orphan-findings.yaml, statistics-findings.yaml, summary-findings.yaml, structure-findings.yaml)</step>
    <step>3. Continue from next_action without re-validating completed phases</step>
    <step>4. Update progress.yaml after each phase completion</step>
  </resumption_protocol>
</context_compaction_survival>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/validation/vl-012/progress.yaml if it exists
    2. If progress exists and validation_phase != "complete":
       - Load existing findings
       - Resume from next_action
    3. If no progress or starting fresh:
       - Begin with Phase 1: Link Integrity Verification
       - Create initial progress.yaml
    4. After completing each phase:
       - Update progress.yaml
       - Write intermediate findings immediately
    5. Continue through all 6 phases until validation report is complete
    6. Write final report to docs/reverse-engineering/validation/VL-012-master-index-report.md
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to validate the RE-000 master index documentation. The prompt will:

1. **Verify link integrity** by resolving every hyperlink in index.md to an actual file on disk
2. **Detect orphaned documents** by scanning all subdirectories and comparing against index references
3. **Check statistical accuracy** by comparing all counts against canonical ground truth (31 programs, 30 copybooks, 17 BMS, 38 JCL, 22 data files)
4. **Assess summary fidelity** by reading source documents and comparing against index descriptions
5. **Validate execution order** by comparing the described generation process against the actual RE prompt dependency graph
6. **Report** findings with severity ratings and remediation guidance

## Expected Output

| File | Description |
|------|-------------|
| `VL-012-master-index-report.md` | Structured validation report with verdict, score, link inventory, orphan inventory, and statistics accuracy table |

## Prerequisites

- RE-000 must have been executed and output exists at `docs/reverse-engineering/index.md`
- ALL RE-001 through RE-011 outputs must exist in their respective `docs/reverse-engineering/` subdirectories
- VL-001 through VL-011 must have been executed (validates the source documents that the index references)
- Access to source code in `app/cbl/`, `app/cpy/`, `app/bms/`, `app/jcl/`, `app/data/`

## Depends On

- VL-001 (Domain Model Validation)
- VL-002 (Data Model Validation)
- VL-003 (Context Model Validation)
- VL-004 (Architecture Validation)
- VL-005 (Screen Flows Validation)
- VL-006 (Batch Workflows Validation)
- VL-007 (Security Model Validation)
- VL-008 (Integration Patterns Validation)
- VL-009 (Test Coverage Validation)
- VL-010 (Modernization Readiness Validation)
- VL-011 (API Candidates Validation)

This is a Phase 7 validation prompt — it executes after all other VL prompts have validated their respective RE outputs.

## Blocks

- VL-000 (Cross-Document Consistency)
