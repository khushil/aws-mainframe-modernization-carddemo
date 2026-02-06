# VL-001: Domain Model Validation

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Validation Analyst specializing in Domain-Driven Design verification for legacy COBOL system documentation</persona>

    <validation_expertise>
      <skill>Cross-referencing domain model claims against COBOL source code</skill>
      <skill>Verifying entity definitions match actual copybook record layouts</skill>
      <skill>Detecting hallucinated file references, field names, and PIC clauses</skill>
      <skill>Assessing completeness of domain extraction against full source inventory</skill>
      <skill>Validating 88-level condition name accuracy and business rule correctness</skill>
      <skill>Confirming Mermaid diagram accuracy against documented entities</skill>
    </validation_expertise>

    <mainframe_expertise>
      <skill>COBOL copybook interpretation — 01-level records, subordinate fields, PIC clauses</skill>
      <skill>88-level condition names as domain state encodings</skill>
      <skill>WORKING-STORAGE analysis for literals, constants, and valid value sets</skill>
      <skill>PROCEDURE DIVISION paragraph naming conventions revealing business operations</skill>
      <skill>EVALUATE/IF structure analysis for decision logic and validation rules</skill>
      <skill>CICS pseudo-conversational pattern and COMMAREA state passing</skill>
    </mainframe_expertise>

    <carddemo_context>
      Ground truth inventory (verified from source tree):
      - 31 COBOL programs in app/cbl/ (29 .cbl + 2 .CBL: CBSTM03A.CBL, CBSTM03B.CBL)
      - 30 copybooks in app/cpy/ (29 .cpy + 1 .CPY: COSTM01.CPY)
      - 17 BMS copybooks in app/cpy-bms/
      - 17 BMS mapsets in app/bms/
      - 38 JCL files in app/jcl/
      - Card copybook is CVCRD01Y.cpy (NOT CVCAR00Y.cpy — that file does not exist)
      - User security copybook is CSUSR01Y.cpy (NOT COUSR00Y.cpy — that file does not exist)
      - Monetary fields use PIC S9(10)V99 (NOT S9(7)V99 — see CVACT01Y.cpy:7)
    </carddemo_context>

    <mindset>Trust nothing. Verify everything against source code. Every file:line reference must be read and confirmed. Every field name must exist in the cited copybook. Every PIC clause must match exactly.</mindset>
  </role>

  <objective>
    <primary_goal>
      Validate the RE-001 domain model documentation for correctness, accuracy, completeness, and absence of hallucinations by cross-referencing every claim against the actual CardDemo source code.
    </primary_goal>

    <validation_targets>
      <target>docs/reverse-engineering/01-domain-model/DOMAIN-MODEL.md</target>
      <target>docs/reverse-engineering/01-domain-model/UBIQUITOUS-LANGUAGE.md</target>
      <target>docs/reverse-engineering/01-domain-model/BUSINESS-RULES.md</target>
      <target>docs/reverse-engineering/01-domain-model/DOMAIN-EVENTS.md</target>
    </validation_targets>

    <success_criteria>
      <criterion>All entity source copybook references verified to exist</criterion>
      <criterion>All field names and PIC clauses match actual copybook definitions</criterion>
      <criterion>All code excerpts verifiable at cited file:line locations</criterion>
      <criterion>All 88-level conditions exist in cited copybooks</criterion>
      <criterion>CVCAR00Y.cpy and COUSR00Y.cpy hallucinations flagged if present</criterion>
      <criterion>S9(7)V99 hallucination flagged if present (actual: S9(10)V99)</criterion>
      <criterion>Completeness: 6+ entities, 100+ terms, all copybooks analyzed</criterion>
      <criterion>Both CO* (online) and CB* (batch) programs covered</criterion>
    </success_criteria>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<ground_truth>
  <cobol_programs count="31">
    <online_programs>
      COACTUPC.cbl, COACTVWC.cbl, COADM01C.cbl, COBIL00C.cbl, COBSWAIT.cbl,
      COCRDLIC.cbl, COCRDSLC.cbl, COCRDUPC.cbl, COMEN01C.cbl, CORPT00C.cbl,
      COSGN00C.cbl, COTRN00C.cbl, COTRN01C.cbl, COTRN02C.cbl,
      COUSR00C.cbl, COUSR01C.cbl, COUSR02C.cbl, COUSR03C.cbl, CSUTLDTC.cbl
    </online_programs>
    <batch_programs>
      CBACT01C.cbl, CBACT02C.cbl, CBACT03C.cbl, CBACT04C.cbl,
      CBCUS01C.cbl, CBEXPORT.cbl, CBIMPORT.cbl,
      CBSTM03A.CBL, CBSTM03B.CBL,
      CBTRN01C.cbl, CBTRN02C.cbl, CBTRN03C.cbl
    </batch_programs>
  </cobol_programs>

  <copybooks count="30">
    COADM02Y.cpy, COCOM01Y.cpy, CODATECN.cpy, COMEN02Y.cpy, COSTM01.CPY, COTTL01Y.cpy,
    CSDAT01Y.cpy, CSLKPCDY.cpy, CSMSG01Y.cpy, CSMSG02Y.cpy, CSSETATY.cpy, CSSTRPFY.cpy,
    CSUSR01Y.cpy, CSUTLDPY.cpy, CSUTLDWY.cpy, CUSTREC.cpy,
    CVACT01Y.cpy, CVACT02Y.cpy, CVACT03Y.cpy, CVCRD01Y.cpy, CVCUS01Y.cpy, CVEXPORT.cpy,
    CVTRA01Y.cpy, CVTRA02Y.cpy, CVTRA03Y.cpy, CVTRA04Y.cpy, CVTRA05Y.cpy,
    CVTRA06Y.cpy, CVTRA07Y.cpy, UNUSED1Y.cpy
  </copybooks>

  <key_record_layouts>
    <record copybook="CVACT01Y.cpy" entity="Account" record_length="300">
      ACCT-ID PIC 9(11), ACCT-ACTIVE-STATUS PIC X(01), ACCT-CURR-BAL PIC S9(10)V99,
      ACCT-CREDIT-LIMIT PIC S9(10)V99, ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99,
      ACCT-OPEN-DATE PIC X(10), ACCT-EXPIRAION-DATE PIC X(10), ACCT-REISSUE-DATE PIC X(10),
      ACCT-CURR-CYC-CREDIT PIC S9(10)V99, ACCT-CURR-CYC-DEBIT PIC S9(10)V99,
      ACCT-ADDR-ZIP PIC X(10), ACCT-GROUP-ID PIC X(10), FILLER PIC X(178)
    </record>
    <record copybook="CVCRD01Y.cpy" entity="Card">Verify actual fields from source</record>
    <record copybook="CVCUS01Y.cpy" entity="Customer">Verify actual fields from source</record>
    <record copybook="CVTRA01Y.cpy" entity="Transaction">Verify actual fields from source</record>
    <record copybook="CSUSR01Y.cpy" entity="User Security">Verify actual fields from source</record>
    <record copybook="CVACT03Y.cpy" entity="Card-Account Cross-Reference">Verify actual fields from source</record>
  </key_record_layouts>

  <known_hallucinations>
    <hallucination id="H1">CVCAR00Y.cpy does NOT exist — actual card copybook is CVCRD01Y.cpy</hallucination>
    <hallucination id="H2">COUSR00Y.cpy does NOT exist — actual user security copybook is CSUSR01Y.cpy</hallucination>
    <hallucination id="H3">S9(7)V99 COMP-3 for monetary fields is WRONG — actual PIC is S9(10)V99 (see CVACT01Y.cpy:7)</hallucination>
  </known_hallucinations>
</ground_truth>

<validation_methodology>
  <phase name="1_load_documentation">
    <description>Read all target documentation files and catalog every verifiable claim</description>
    <steps>
      <step>Read docs/reverse-engineering/01-domain-model/DOMAIN-MODEL.md</step>
      <step>Read docs/reverse-engineering/01-domain-model/UBIQUITOUS-LANGUAGE.md</step>
      <step>Read docs/reverse-engineering/01-domain-model/BUSINESS-RULES.md</step>
      <step>Read docs/reverse-engineering/01-domain-model/DOMAIN-EVENTS.md</step>
      <step>For each document, extract: file references, field names, PIC clauses, code excerpts, entity lists, counts</step>
      <step>Create a claims inventory in .work/reverse-engineering/validation/vl-001/claims-inventory.yaml</step>
    </steps>
  </phase>

  <phase name="2_source_reference_verification">
    <description>For every file:line reference in the documentation, read the actual source and verify</description>
    <steps>
      <step>For each copybook reference: verify the file exists in app/cpy/</step>
      <step>For each program reference: verify the file exists in app/cbl/</step>
      <step>For each file:line citation: read the source at that line and confirm the claim</step>
      <step>Flag any references to CVCAR00Y.cpy (should be CVCRD01Y.cpy)</step>
      <step>Flag any references to COUSR00Y.cpy (should be CSUSR01Y.cpy)</step>
      <step>Record all findings in .work/reverse-engineering/validation/vl-001/source-ref-findings.yaml</step>
    </steps>
  </phase>

  <phase name="3_factual_accuracy_check">
    <description>Verify PIC clauses, field names, 88-level conditions, and logic descriptions</description>
    <steps>
      <step>For each entity: verify all listed field names exist in the cited copybook</step>
      <step>For each PIC clause: compare documented PIC against actual copybook definition</step>
      <step>Flag any S9(7)V99 claims (actual is S9(10)V99 per CVACT01Y.cpy:7)</step>
      <step>For each 88-level condition: verify it exists with the correct VALUE clause</step>
      <step>For each business rule: verify the cited code excerpt matches the source</step>
      <step>For each domain event: verify the triggering code exists as described</step>
    </steps>
  </phase>

  <phase name="4_completeness_assessment">
    <description>Check all expected items are covered in the documentation</description>
    <checks>
      <check>All 30 copybooks analyzed or explicitly noted as out-of-scope</check>
      <check>All 31 COBOL programs referenced (both CO* online and CB* batch)</check>
      <check>6+ core entities identified (Account, Card, Customer, Transaction, User, Cross-Reference)</check>
      <check>100+ ubiquitous language terms in glossary</check>
      <check>Business rules from both online and batch programs</check>
      <check>Aggregate boundaries defined</check>
      <check>Domain events identified with triggers and payloads</check>
    </checks>
  </phase>

  <phase name="5_quality_assessment">
    <description>Evaluate documentation quality, formatting, and diagram correctness</description>
    <checks>
      <check>Mermaid classDiagram syntax is valid and renderable</check>
      <check>Markdown formatting is consistent (heading hierarchy, table formatting)</check>
      <check>Cross-references between domain model documents resolve correctly</check>
      <check>Table of contents present for documents over 200 lines</check>
      <check>Code excerpts are properly formatted in fenced code blocks</check>
    </checks>
  </phase>

  <phase name="6_report_generation">
    <description>Produce the structured validation report</description>
    <output>docs/reverse-engineering/validation/VL-001-domain-model-report.md</output>
    <format>
      - Verdict: PASS (100) / FAIL (less than 100)
      - Score breakdown by category (5 categories, weighted)
      - Critical findings (hallucinations, wrong PICs, fabricated references)
      - Major findings (missing entities, incomplete coverage)
      - Minor findings (formatting, diagram issues)
      - Hallucination inventory table
      - Completeness gaps table
      - Specific remediation recommendations
    </format>
  </phase>
</validation_methodology>

<scoring_rubric>
  <category name="Source Reference Accuracy" weight="35" severity="Critical">
    <description>Every file:line citation verified against actual source</description>
    <scoring>
      100: All references verified correct
      80: 90%+ references correct, no fabricated files
      60: 80%+ correct, minor line number discrepancies
      40: References to non-existent files found
      20: Multiple fabricated references
      0: Majority of references unverifiable
    </scoring>
  </category>
  <category name="Factual Accuracy" weight="25" severity="Critical">
    <description>PIC clauses, field names, logic descriptions match reality</description>
    <scoring>
      100: All facts verified correct
      80: 95%+ facts correct, no wrong PIC clauses
      60: Minor field name variations, PICs correct
      40: Wrong PIC clauses found (e.g., S9(7)V99 vs S9(10)V99)
      20: Multiple factual errors in core entities
      0: Pervasive factual inaccuracies
    </scoring>
  </category>
  <category name="Completeness" weight="20" severity="Major">
    <description>All expected items covered</description>
    <scoring>
      100: All 30 copybooks, all 31 programs, 6+ entities, 100+ terms
      80: 90%+ coverage, all core entities present
      60: 75%+ coverage, some secondary items missing
      40: Major entities or programs missing
      20: Less than 50% coverage
      0: Severely incomplete
    </scoring>
  </category>
  <category name="Quantitative Accuracy" weight="10" severity="Major">
    <description>File counts, record lengths, byte calculations correct</description>
    <scoring>
      100: All counts match ground truth exactly
      80: Counts within 5% tolerance
      60: Minor discrepancies in non-critical counts
      40: Major count errors (e.g., 39 programs instead of 31)
      20: Multiple count errors
      0: Counts are unreliable
    </scoring>
  </category>
  <category name="Documentation Quality" weight="10" severity="Minor">
    <description>Mermaid syntax valid, markdown well-formed, clarity</description>
    <scoring>
      100: Professional quality, all diagrams render, clear writing
      80: Minor formatting issues, diagrams mostly correct
      60: Some broken diagrams, inconsistent formatting
      40: Multiple rendering issues, unclear sections
      20: Poor formatting throughout
      0: Unusable documentation
    </scoring>
  </category>
</scoring_rubric>

<output_specification>
  <report_file>docs/reverse-engineering/validation/VL-001-domain-model-report.md</report_file>
  <work_directory>.work/reverse-engineering/validation/vl-001/</work_directory>

  <report_structure>
    # Validation Report: Domain Model (RE-001)

    ## Verdict: [PASS|FAIL] — Score: [NN]/100

    ## Score Breakdown
    [Weighted scoring table]

    ## Critical Findings
    [Hallucinations, fabricated references, wrong PIC clauses]

    ## Major Findings
    [Missing entities, incomplete coverage, missing programs]

    ## Minor Findings
    [Formatting issues, diagram problems]

    ## Hallucination Inventory
    [Table of every hallucinated claim with reality check]

    ## Completeness Gaps
    [Table of expected vs actual coverage]

    ## Recommendations
    [Specific remediation actions]

    ## Remediation Manifest

    | ID | Finding | Target File | Location | Current (Wrong) | Required (Correct) | Source Evidence | Remediation Action | RE Prompt |
    |----|---------|-------------|----------|-----------------|--------------------|-----------------|--------------------|-----------|
    | R-001 | [finding description] | [doc file path] | [section/line] | [wrong value] | [correct value] | [source file:line] | [specific fix] | [RE-NNN] |

    ### Remediation Instructions
    For each row in the manifest:
    1. Read the target file at the specified location
    2. Verify the current (wrong) value is still present
    3. Replace with the required (correct) value using source evidence as authority
    4. Re-validate after remediation to confirm the fix

    ### Affected RE Prompts
    [List RE prompts requiring re-execution, with reasons]
  </report_structure>
</output_specification>

<foundational_principles>
  <principle id="1">Every copybook reference must resolve to an actual file in app/cpy/</principle>
  <principle id="2">Every program reference must resolve to an actual file in app/cbl/</principle>
  <principle id="3">PIC clauses must match the source exactly — S9(10)V99 is not S9(7)V99</principle>
  <principle id="4">88-level conditions must exist with the exact VALUE clause cited</principle>
  <principle id="5">Code excerpts must match the source at the cited line numbers</principle>
  <principle id="6">Entity field lists must contain only fields that exist in the actual copybook</principle>
  <principle id="7">Record lengths must match the comment in the copybook (e.g., RECLN 300 for Account)</principle>
  <principle id="8">Business rules must cite verifiable code — not paraphrased or invented logic</principle>
</foundational_principles>

<critical_reminders>
  <reminder id="1">CVCAR00Y.cpy does NOT exist — the card copybook is CVCRD01Y.cpy. Flag immediately if referenced.</reminder>
  <reminder id="2">COUSR00Y.cpy does NOT exist — the user security copybook is CSUSR01Y.cpy. Flag immediately if referenced.</reminder>
  <reminder id="3">Monetary PIC is S9(10)V99, NOT S9(7)V99 COMP-3. Verify at CVACT01Y.cpy:7.</reminder>
  <reminder id="4">There are 31 COBOL programs, NOT 39. Count includes CBSTM03A.CBL and CBSTM03B.CBL.</reminder>
  <reminder id="5">There are 30 copybooks, NOT 41. Count includes COSTM01.CPY.</reminder>
  <reminder id="6">Read EVERY cited source file:line — do not trust that code excerpts are accurate.</reminder>
  <reminder id="7">Check that Mermaid classDiagram entities match the documented entity list exactly.</reminder>
  <reminder id="8">Verify that business rules cite real EVALUATE/IF structures, not invented conditions.</reminder>
  <reminder id="9">Confirm aggregate boundaries make domain sense (Account aggregate should include Cards).</reminder>
  <reminder id="10">Check that ubiquitous language terms have real source references, not just plausible-sounding citations.</reminder>
</critical_reminders>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/validation/vl-001/</path>
    <purpose>Persist validation progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
      validation_phase: "load_docs|source_verification|factual_check|completeness|quality|reporting"
      current_document: "filename being validated"
      documents_completed: ["list of validated documents"]
      documents_remaining: ["list of remaining documents"]
      findings:
        critical: N
        major: N
        minor: N
      next_action: "Detailed description of next step"
      last_updated: "ISO timestamp"
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/validation/vl-001/progress.yaml if it exists</step>
    <step>2. Load any existing findings from the work directory</step>
    <step>3. Continue from next_action without re-validating completed documents</step>
    <step>4. Update progress.yaml after each phase completion</step>
  </resumption_protocol>
</context_compaction_survival>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/validation/vl-001/progress.yaml if it exists
    2. If progress exists and validation_phase != "complete":
       - Load existing findings
       - Resume from next_action
    3. If no progress or starting fresh:
       - Begin with Phase 1: Load Documentation
       - Create initial progress.yaml
    4. After completing each phase:
       - Update progress.yaml
       - Write intermediate findings immediately
    5. Continue through all phases until validation report is complete
    6. Write final report to docs/reverse-engineering/validation/VL-001-domain-model-report.md
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to validate the RE-001 domain model documentation. The prompt will:

1. **Load** all domain model documentation files
2. **Verify** every source reference against actual copybooks and programs
3. **Check** factual accuracy of PIC clauses, field names, and business rules
4. **Assess** completeness against the full source inventory
5. **Evaluate** documentation quality and diagram correctness
6. **Report** findings with severity ratings and remediation guidance

## Expected Output

| File | Description |
|------|-------------|
| `VL-001-domain-model-report.md` | Structured validation report with verdict, score, and findings |

## Prerequisites

- RE-001 must have been executed and output exists in `docs/reverse-engineering/01-domain-model/`
- Access to source code in `app/cbl/`, `app/cpy/`, `app/bms/`

## Depends On

- None (this is a Phase 1 validation prompt)

## Blocks

- VL-003 (Context Model Validation)
- VL-000 (Cross-Document Consistency)
