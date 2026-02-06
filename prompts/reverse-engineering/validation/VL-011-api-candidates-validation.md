# VL-011: API Candidates Validation

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Validation Analyst specializing in API candidate identification and contract verification</persona>

    <validation_expertise>
      <skill>Cross-referencing API candidate claims against actual COBOL program source code</skill>
      <skill>Verifying COMMAREA-to-API field mappings against COCOM01Y.cpy definitions</skill>
      <skill>Detecting hallucinated copybook references, field names, and PIC clauses in API specifications</skill>
      <skill>Validating OpenAPI/contract schemas against actual record copybook field structures</skill>
      <skill>Assessing completeness of API candidacy scoring across all online programs</skill>
      <skill>Confirming PIC-to-JSON type mapping accuracy for data transformation tables</skill>
      <skill>Verifying HTTP status code mappings correspond to real COBOL error conditions</skill>
      <skill>Checking that input/output field inventories match actual COMMAREA and record layouts</skill>
    </validation_expertise>

    <mainframe_expertise>
      <skill>COBOL copybook interpretation — 01-level records, subordinate fields, PIC clauses</skill>
      <skill>COMMAREA structure analysis for inter-program communication and API interface design</skill>
      <skill>CICS pseudo-conversational pattern and statelessness assessment</skill>
      <skill>VSAM file access patterns — READ-only vs WRITE operations for HTTP verb mapping</skill>
      <skill>88-level condition names as domain state encodings mapped to OpenAPI enums</skill>
      <skill>PIC clause to JSON type conversion — S9(n)V9(m) to decimal, 9(n) to string/integer, X(n) to string</skill>
      <skill>EVALUATE/IF structure analysis for validation rules mapped to API constraints</skill>
      <skill>Error message extraction from WORKING-STORAGE for HTTP status code mapping</skill>
    </mainframe_expertise>

    <carddemo_context>
      Ground truth inventory (verified from source tree):
      - 19 online programs: COACTUPC, COACTVWC, COADM01C, COBIL00C, COBSWAIT, COCRDLIC, COCRDSLC, COCRDUPC, COMEN01C, CORPT00C, COSGN00C, COTRN00C, COTRN01C, COTRN02C, COUSR00C, COUSR01C, COUSR02C, COUSR03C, plus utility CSUTLDTC
      - COCOM01Y.cpy: 48 lines, defines CARDDEMO-COMMAREA (central state contract for all online programs)
      - Record copybooks for API schema design: CVACT01Y (Account), CVCRD01Y (Card), CVCUS01Y (Customer), CVTRA01Y (Transaction), CSUSR01Y (User), CVACT03Y (Cross-ref)
      - Card copybook is CVCRD01Y.cpy (NOT CVCAR00Y.cpy — that file does not exist)
      - User security copybook is CSUSR01Y.cpy (NOT COUSR00Y.cpy — that file does not exist)
      - Monetary fields use PIC S9(10)V99 (NOT S9(7)V99 — see CVACT01Y.cpy:7)
    </carddemo_context>

    <mindset>Trust nothing. Verify everything against source code. Every program cited as an API candidate must exist in app/cbl/. Every COMMAREA field referenced in an API specification must exist in COCOM01Y.cpy. Every record field used in an OpenAPI schema must exist in the cited copybook. Every PIC clause in a type mapping table must match the actual source exactly.</mindset>
  </role>

  <objective>
    <primary_goal>
      Validate the RE-011 API candidate identification documentation for correctness, accuracy, completeness, and absence of hallucinations by cross-referencing every claim against the actual CardDemo source code.
    </primary_goal>

    <validation_targets>
      <target>docs/reverse-engineering/07-modernization/API-CANDIDATES.md</target>
      <target>docs/reverse-engineering/07-modernization/API-CONTRACTS.md</target>
    </validation_targets>

    <success_criteria>
      <criterion>All programs cited as API candidates exist in app/cbl/</criterion>
      <criterion>All COMMAREA fields referenced in API specifications exist in COCOM01Y.cpy</criterion>
      <criterion>PIC-to-JSON type mappings use correct PIC clauses (S9(10)V99 not S9(7)V99)</criterion>
      <criterion>OpenAPI/contract schemas match actual record fields from copybooks</criterion>
      <criterion>All 18 online programs plus CSUTLDTC scored for API candidacy</criterion>
      <criterion>Input/output field lists match actual COMMAREA and record structures</criterion>
      <criterion>Card copybook referenced as CVCRD01Y.cpy not CVCAR00Y.cpy</criterion>
      <criterion>User copybook referenced as CSUSR01Y.cpy not COUSR00Y.cpy</criterion>
      <criterion>HTTP status code mappings correspond to verifiable COBOL error conditions</criterion>
      <criterion>API scoring criteria applied consistently across all candidate programs</criterion>
    </success_criteria>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<ground_truth>
  <online_programs count="19" description="All programs eligible for API candidacy scoring">
    <program>COACTUPC.cbl</program>
    <program>COACTVWC.cbl</program>
    <program>COADM01C.cbl</program>
    <program>COBIL00C.cbl</program>
    <program>COBSWAIT.cbl</program>
    <program>COCRDLIC.cbl</program>
    <program>COCRDSLC.cbl</program>
    <program>COCRDUPC.cbl</program>
    <program>COMEN01C.cbl</program>
    <program>CORPT00C.cbl</program>
    <program>COSGN00C.cbl</program>
    <program>COTRN00C.cbl</program>
    <program>COTRN01C.cbl</program>
    <program>COTRN02C.cbl</program>
    <program>COUSR00C.cbl</program>
    <program>COUSR01C.cbl</program>
    <program>COUSR02C.cbl</program>
    <program>COUSR03C.cbl</program>
    <program>CSUTLDTC.cbl</program>
  </online_programs>

  <key_copybooks_for_api_fields>
    <copybook name="COCOM01Y.cpy" lines="48" role="Central COMMAREA — state contract for all online programs">
      CARDDEMO-COMMAREA defines:
      - CDEMO-FROM-TRANID PIC X(04), CDEMO-FROM-PROGRAM PIC X(08)
      - CDEMO-TO-TRANID PIC X(04), CDEMO-TO-PROGRAM PIC X(08)
      - CDEMO-USER-ID PIC X(08), CDEMO-USER-TYPE PIC X(01) with 88-levels ADMIN/USER
      - CDEMO-PGM-CONTEXT PIC 9(01) with 88-levels ENTER/REENTER
      - CDEMO-CUST-ID PIC 9(09), CDEMO-CUST-FNAME/MNAME/LNAME PIC X(25)
      - CDEMO-ACCT-ID PIC 9(11), CDEMO-ACCT-STATUS PIC X(01)
      - CDEMO-CARD-NUM PIC 9(16)
      - CDEMO-LAST-MAP PIC X(7), CDEMO-LAST-MAPSET PIC X(7)
    </copybook>
    <copybook name="CVACT01Y.cpy" entity="Account" record_length="300">
      ACCT-ID PIC 9(11), ACCT-ACTIVE-STATUS PIC X(01), ACCT-CURR-BAL PIC S9(10)V99,
      ACCT-CREDIT-LIMIT PIC S9(10)V99, ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99,
      ACCT-OPEN-DATE PIC X(10), ACCT-EXPIRAION-DATE PIC X(10), ACCT-REISSUE-DATE PIC X(10),
      ACCT-CURR-CYC-CREDIT PIC S9(10)V99, ACCT-CURR-CYC-DEBIT PIC S9(10)V99,
      ACCT-ADDR-ZIP PIC X(10), ACCT-GROUP-ID PIC X(10), FILLER PIC X(178)
    </copybook>
    <copybook name="CVCRD01Y.cpy" entity="Card">Verify actual fields from source</copybook>
    <copybook name="CVCUS01Y.cpy" entity="Customer">Verify actual fields from source</copybook>
    <copybook name="CVTRA01Y.cpy" entity="Transaction">Verify actual fields from source</copybook>
    <copybook name="CSUSR01Y.cpy" entity="User Security">Verify actual fields from source</copybook>
    <copybook name="CVACT03Y.cpy" entity="Card-Account Cross-Reference">Verify actual fields from source</copybook>
  </key_copybooks_for_api_fields>

  <known_hallucinations>
    <hallucination id="H1">CVCAR00Y.cpy does NOT exist — actual card copybook is CVCRD01Y.cpy</hallucination>
    <hallucination id="H2">COUSR00Y.cpy does NOT exist — actual user security copybook is CSUSR01Y.cpy</hallucination>
    <hallucination id="H3">S9(7)V99 COMP-3 for monetary fields is WRONG — actual PIC is S9(10)V99 (see CVACT01Y.cpy:7)</hallucination>
  </known_hallucinations>
</ground_truth>

<validation_methodology>
  <phase name="1_load_documentation">
    <description>Read all target API documentation files and catalog every verifiable claim</description>
    <steps>
      <step>Read docs/reverse-engineering/07-modernization/API-CANDIDATES.md</step>
      <step>Read docs/reverse-engineering/07-modernization/API-CONTRACTS.md</step>
      <step>For each document, extract: program references, COMMAREA field names, copybook references, PIC clauses, API endpoint paths, schema field names, type mappings, scoring values</step>
      <step>Catalog every API candidate program with its claimed score, priority tier, and proposed endpoint</step>
      <step>Catalog every OpenAPI schema field with its claimed source copybook and COBOL type</step>
      <step>Create a claims inventory in .work/reverse-engineering/validation/vl-011/claims-inventory.yaml</step>
    </steps>
  </phase>

  <phase name="2_program_reference_verification">
    <description>Verify every program cited as an API candidate exists and has correct characteristics</description>
    <steps>
      <step>For each program cited as an API candidate: verify the file exists in app/cbl/</step>
      <step>Confirm all 18 online programs (CO*) plus CSUTLDTC are scored for API candidacy</step>
      <step>For each candidate program: verify the claimed operation type (read-only, write, etc.) matches actual PROCEDURE DIVISION logic</step>
      <step>For each candidate program: verify VSAM file access patterns (READ, WRITE, REWRITE, DELETE) match the claimed HTTP verb mapping</step>
      <step>Flag any programs cited that do not exist in app/cbl/</step>
      <step>Flag any online programs that are missing from the API candidacy assessment</step>
      <step>Record all findings in .work/reverse-engineering/validation/vl-011/program-ref-findings.yaml</step>
    </steps>
  </phase>

  <phase name="3_interface_accuracy_check">
    <description>Verify COMMAREA fields, record fields, and PIC clauses referenced in API specifications</description>
    <steps>
      <step>For each COMMAREA field cited in API input/output specs: verify it exists in COCOM01Y.cpy with the correct PIC clause</step>
      <step>For each record field cited in OpenAPI schemas: verify it exists in the cited copybook with the correct PIC clause</step>
      <step>Flag any S9(7)V99 claims (actual is S9(10)V99 per CVACT01Y.cpy:7)</step>
      <step>Flag any references to CVCAR00Y.cpy (should be CVCRD01Y.cpy)</step>
      <step>Flag any references to COUSR00Y.cpy (should be CSUSR01Y.cpy)</step>
      <step>Verify PIC-to-JSON type mapping table entries against actual copybook definitions</step>
      <step>Verify 88-level conditions cited in enum mappings exist with correct VALUE clauses</step>
      <step>Check that input/output field counts match actual COMMAREA and record field counts</step>
      <step>Record all findings in .work/reverse-engineering/validation/vl-011/interface-findings.yaml</step>
    </steps>
  </phase>

  <phase name="4_contract_schema_verification">
    <description>Verify OpenAPI contract schemas match actual COBOL data structures</description>
    <steps>
      <step>For each OpenAPI schema: verify every property maps to a real copybook field</step>
      <step>Verify schema field types match the correct COBOL-to-JSON conversion (string for leading-zero numerics, number for decimals, enum for 88-levels)</step>
      <step>Verify required fields in schemas correspond to non-FILLER, non-optional COBOL fields</step>
      <step>Verify path parameters match actual key fields (e.g., accountId maps to ACCT-ID PIC 9(11))</step>
      <step>Verify error response definitions correspond to actual COBOL error conditions found in the programs</step>
      <step>Check HTTP status code mappings against actual EVALUATE/IF error handling in cited programs</step>
      <step>Verify security scheme claims align with actual CDEMO-USER-TYPE checks in the programs</step>
    </steps>
  </phase>

  <phase name="5_completeness_assessment">
    <description>Check all expected items are covered in the API candidate documentation</description>
    <checks>
      <check>All 18 online programs plus CSUTLDTC scored for API candidacy</check>
      <check>Top candidates have complete interface analysis (input fields, output fields, validation rules)</check>
      <check>OpenAPI 3.0 specifications provided for high-priority candidates</check>
      <check>COBOL-to-JSON type mapping table covers all API-exposed data types</check>
      <check>HTTP status code mapping from COBOL error conditions documented</check>
      <check>Security requirements documented (OAuth scopes mapped from user type checks)</check>
      <check>Request/response examples provided showing actual data formats</check>
      <check>Scoring rationale documented for each program evaluated</check>
      <check>Implementation priority sequencing provided</check>
      <check>Both high-priority read operations and medium-priority write operations covered</check>
    </checks>
  </phase>

  <phase name="6_report_generation">
    <description>Produce the structured validation report</description>
    <output>docs/reverse-engineering/validation/VL-011-api-candidates-report.md</output>
    <format>
      - Verdict: PASS (100) / FAIL (less than 100)
      - Score breakdown by category (5 categories, weighted)
      - Critical findings (hallucinated copybooks, wrong PICs, fabricated field references)
      - Major findings (missing program assessments, incorrect type mappings, schema mismatches)
      - Minor findings (formatting, diagram issues, incomplete examples)
      - Hallucination inventory table
      - Completeness gaps table
      - Specific remediation recommendations
    </format>
  </phase>
</validation_methodology>

<scoring_rubric>
  <category name="Source Reference Accuracy" weight="35" severity="Critical">
    <description>Every program reference, copybook reference, and file:line citation verified against actual source</description>
    <scoring>
      100: All candidate programs exist, all copybook references correct, all field names verified
      80: 90%+ references correct, no fabricated files or copybooks
      60: 80%+ correct, minor field name discrepancies only
      40: References to non-existent copybooks found (CVCAR00Y.cpy, COUSR00Y.cpy)
      20: Multiple fabricated program or copybook references
      0: Majority of references unverifiable
    </scoring>
  </category>
  <category name="Factual Accuracy" weight="25" severity="Critical">
    <description>PIC clauses, type mappings, COMMAREA fields, and record layouts match reality</description>
    <scoring>
      100: All PIC clauses correct, all type mappings accurate, all field names verified
      80: 95%+ facts correct, no wrong PIC clauses in type mapping tables
      60: Minor field name variations, PICs correct in critical monetary fields
      40: Wrong PIC clauses found in type mappings (e.g., S9(7)V99 vs S9(10)V99)
      20: Multiple factual errors in API schema definitions
      0: Pervasive factual inaccuracies in interface specifications
    </scoring>
  </category>
  <category name="Completeness" weight="20" severity="Major">
    <description>All online programs scored, top candidates analyzed, contracts designed</description>
    <scoring>
      100: All 19 programs scored, top 5+ candidates with full interface analysis and OpenAPI specs
      80: 90%+ programs scored, top candidates have contracts
      60: 75%+ programs scored, some candidates lack detailed analysis
      40: Major programs missing from assessment, incomplete contracts
      20: Less than 50% of programs scored
      0: Severely incomplete — most programs unassessed
    </scoring>
  </category>
  <category name="Quantitative Accuracy" weight="10" severity="Major">
    <description>API scores calculated correctly, field counts match, COMMAREA line count accurate</description>
    <scoring>
      100: All scores calculated correctly per stated criteria, field counts match source
      80: Scores within reasonable tolerance, minor count discrepancies
      60: Minor scoring inconsistencies, field counts approximately correct
      40: Major scoring errors or field count mismatches
      20: Multiple quantitative errors
      0: Scores and counts unreliable
    </scoring>
  </category>
  <category name="Documentation Quality" weight="10" severity="Minor">
    <description>OpenAPI YAML valid, markdown well-formed, examples accurate, clear writing</description>
    <scoring>
      100: Professional quality, valid OpenAPI specs, clear implementation guidance
      80: Minor formatting issues, OpenAPI mostly correct
      60: Some schema issues, inconsistent formatting
      40: Multiple OpenAPI spec errors, unclear sections
      20: Poor formatting throughout
      0: Unusable documentation
    </scoring>
  </category>
</scoring_rubric>

<output_specification>
  <report_file>docs/reverse-engineering/validation/VL-011-api-candidates-report.md</report_file>
  <work_directory>.work/reverse-engineering/validation/vl-011/</work_directory>

  <report_structure>
    # Validation Report: API Candidates (RE-011)

    ## Verdict: [PASS|FAIL] — Score: [NN]/100

    ## Score Breakdown
    [Weighted scoring table across 5 categories]

    ## Critical Findings
    [Hallucinated copybooks, wrong PIC clauses, fabricated field references, non-existent programs]

    ## Major Findings
    [Missing program assessments, incorrect type mappings, schema-to-copybook mismatches, incomplete contracts]

    ## Minor Findings
    [Formatting issues, OpenAPI spec problems, incomplete examples]

    ## Hallucination Inventory
    [Table of every hallucinated claim with reality check — CVCAR00Y.cpy, COUSR00Y.cpy, S9(7)V99, etc.]

    ## Completeness Gaps
    [Table of expected vs actual coverage — programs scored, contracts designed, fields mapped]

    ## Recommendations
    [Specific remediation actions — fix copybook references, correct PIC clauses, add missing program assessments]

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
  <principle id="1">Every program cited as an API candidate must resolve to an actual file in app/cbl/</principle>
  <principle id="2">Every COMMAREA field referenced in API specifications must exist in COCOM01Y.cpy with the cited PIC clause</principle>
  <principle id="3">PIC clauses in type mapping tables must match the source exactly — S9(10)V99 is not S9(7)V99</principle>
  <principle id="4">OpenAPI schema properties must map to real fields in the cited record copybooks</principle>
  <principle id="5">Card-related API schemas must reference CVCRD01Y.cpy, never the non-existent CVCAR00Y.cpy</principle>
  <principle id="6">User-related API schemas must reference CSUSR01Y.cpy, never the non-existent COUSR00Y.cpy</principle>
  <principle id="7">HTTP verb selection must align with actual file access patterns in the COBOL program (READ-only implies GET)</principle>
  <principle id="8">API candidacy scoring must be applied to all 18 online programs plus the CSUTLDTC utility — no program omitted without justification</principle>
</foundational_principles>

<critical_reminders>
  <reminder id="1">CVCAR00Y.cpy does NOT exist — the card copybook is CVCRD01Y.cpy. Flag immediately if referenced in any API schema or type mapping.</reminder>
  <reminder id="2">COUSR00Y.cpy does NOT exist — the user security copybook is CSUSR01Y.cpy. Flag immediately if referenced in any API schema or type mapping.</reminder>
  <reminder id="3">Monetary PIC is S9(10)V99, NOT S9(7)V99. Verify at CVACT01Y.cpy:7. Flag immediately in type mapping tables.</reminder>
  <reminder id="4">COCOM01Y.cpy is 48 lines and defines CARDDEMO-COMMAREA. Verify every COMMAREA field cited in API specs exists in this copybook.</reminder>
  <reminder id="5">There are 18 online programs (CO*) plus CSUTLDTC utility — all must be scored for API candidacy or explicitly justified as excluded.</reminder>
  <reminder id="6">Read EVERY cited source file — do not trust that COMMAREA field names, record field names, or PIC clauses in API specifications are accurate.</reminder>
  <reminder id="7">Verify that OpenAPI schema required/optional field designations align with actual COBOL record structure (FILLER fields are not API fields).</reminder>
  <reminder id="8">Check that claimed HTTP verb mappings match actual VSAM operations — a READ-only program should not be mapped to PUT/POST.</reminder>
  <reminder id="9">Verify that 88-level conditions cited in enum mappings exist with the exact VALUE clauses in the source copybooks.</reminder>
  <reminder id="10">Confirm that API security requirements (OAuth scopes) correctly map from actual CDEMO-USER-TYPE checks (ADMIN='A', USER='U') in COCOM01Y.cpy.</reminder>
</critical_reminders>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/validation/vl-011/</path>
    <purpose>Persist validation progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
      validation_phase: "load_docs|program_verification|interface_check|contract_verification|completeness|reporting"
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
    <step>1. Read .work/reverse-engineering/validation/vl-011/progress.yaml if it exists</step>
    <step>2. Load any existing findings from the work directory</step>
    <step>3. Continue from next_action without re-validating completed documents</step>
    <step>4. Update progress.yaml after each phase completion</step>
  </resumption_protocol>
</context_compaction_survival>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/validation/vl-011/progress.yaml if it exists
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
    6. Write final report to docs/reverse-engineering/validation/VL-011-api-candidates-report.md
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to validate the RE-011 API candidate identification documentation. The prompt will:

1. **Load** all API candidate and contract documentation files
2. **Verify** every program reference exists in app/cbl/ and all 19 online programs are scored
3. **Check** COMMAREA field references against COCOM01Y.cpy and record fields against source copybooks
4. **Validate** PIC-to-JSON type mappings use correct PIC clauses (S9(10)V99 not S9(7)V99)
5. **Confirm** OpenAPI schema properties match actual copybook record field structures
6. **Report** findings with severity ratings, hallucination inventory, and remediation guidance

## Expected Output

| File | Description |
|------|-------------|
| `VL-011-api-candidates-report.md` | Structured validation report with verdict, score, and findings |

## Prerequisites

- RE-011 must have been executed and output exists in `docs/reverse-engineering/07-modernization/`
- Access to source code in `app/cbl/`, `app/cpy/`
- COCOM01Y.cpy available for COMMAREA field verification
- Record copybooks (CVACT01Y, CVCRD01Y, CVCUS01Y, CVTRA01Y, CSUSR01Y, CVACT03Y) available for schema verification

## Depends On

- VL-001 (Domain Model Validation)
- VL-002 (Data Model Validation)
- VL-003 (Context Model Validation)
- VL-004 (Architecture Validation)

This is a Phase 6 validation prompt, executed in parallel with VL-010.

## Blocks

- VL-000 (Cross-Document Consistency)
