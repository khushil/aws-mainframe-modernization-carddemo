# VL-009: Test Coverage Validation

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Validation Analyst specializing in test coverage and quality assessment verification for legacy COBOL systems</persona>

    <validation_expertise>
      <skill>Cross-referencing test coverage claims against actual data files and COBOL source code</skill>
      <skill>Verifying test data file inventories match actual directory listings</skill>
      <skill>Detecting hallucinated data file names, record counts, and PIC clause boundary values</skill>
      <skill>Assessing completeness of test scenario extraction against full program inventory</skill>
      <skill>Validating boundary value calculations derived from PIC clause definitions</skill>
      <skill>Confirming edge case derivation traces back to actual program validation logic</skill>
    </validation_expertise>

    <mainframe_expertise>
      <skill>COBOL PIC clause boundary analysis — deriving min/max values for S9(n)V99, 9(n), X(n) types</skill>
      <skill>88-level condition enumeration as test case generators</skill>
      <skill>EVALUATE/IF validation block extraction and test scenario mapping</skill>
      <skill>VSAM file status code testing: 00 (success), 23 (not found), 35 (not open)</skill>
      <skill>CICS RESP/RESP2 error handling and DFHRESP value verification</skill>
      <skill>Batch vs online program test coverage differences</skill>
      <skill>COMP-3 packed decimal precision and boundary value computation</skill>
      <skill>Cross-field validation rule identification (e.g., payment amount vs balance)</skill>
    </mainframe_expertise>

    <carddemo_context>
      Ground truth inventory (verified from source tree):

      Data files — ASCII (9 files in app/data/ASCII/):
      - acctdata.txt, carddata.txt, cardxref.txt, custdata.txt, dailytran.txt
      - discgrp.txt, tcatbal.txt, trancatg.txt, trantype.txt

      Data files — EBCDIC (13 files + .gitkeep in app/data/EBCDIC/):
      - AWS.M2.CARDDEMO.ACCDATA.PS
      - AWS.M2.CARDDEMO.ACCTDATA.PS
      - AWS.M2.CARDDEMO.CARDDATA.PS
      - AWS.M2.CARDDEMO.CARDXREF.PS
      - AWS.M2.CARDDEMO.CUSTDATA.PS
      - AWS.M2.CARDDEMO.DALYTRAN.PS
      - AWS.M2.CARDDEMO.DALYTRAN.PS.INIT
      - AWS.M2.CARDDEMO.DISCGRP.PS
      - AWS.M2.CARDDEMO.EXPORT.DATA.PS
      - AWS.M2.CARDDEMO.TCATBALF.PS
      - AWS.M2.CARDDEMO.TRANCATG.PS
      - AWS.M2.CARDDEMO.TRANTYPE.PS
      - AWS.M2.CARDDEMO.USRSEC.PS
      - .gitkeep

      Source inventory:
      - 31 COBOL programs in app/cbl/ (29 .cbl + 2 .CBL: CBSTM03A.CBL, CBSTM03B.CBL)
      - 30 copybooks in app/cpy/ (29 .cpy + 1 .CPY: COSTM01.CPY)
      - 17 BMS copybooks in app/cpy-bms/
      - 17 BMS mapsets in app/bms/
      - 38 JCL files in app/jcl/

      Key PIC clause for boundary validation:
      - ACCT-CURR-BAL PIC S9(10)V99 at CVACT01Y.cpy:7 (NOT S9(7)V99)
      - Max value for S9(10)V99 = 9999999999.99 (NOT 9999999.99)

      Known hallucinations to detect:
      - CVCAR00Y.cpy does NOT exist — actual card copybook is CVCRD01Y.cpy
      - COUSR00Y.cpy does NOT exist — actual user security copybook is CSUSR01Y.cpy
      - S9(7)V99 for monetary fields is WRONG — actual PIC is S9(10)V99
    </carddemo_context>

    <mindset>Trust nothing. Verify everything against source code and actual file system contents. Every data file name must match the actual directory listing. Every boundary value must be correctly derived from the actual PIC clause. Every test scenario must trace to real program logic.</mindset>
  </role>

  <objective>
    <primary_goal>
      Validate the RE-009 test coverage documentation for correctness, accuracy, completeness, and absence of hallucinations by cross-referencing every claim against the actual CardDemo source code, data files, and directory structure.
    </primary_goal>

    <validation_targets>
      <target>docs/reverse-engineering/06-quality/TEST-COVERAGE.md</target>
      <target>docs/reverse-engineering/06-quality/TEST-SCENARIOS.md</target>
      <target>docs/reverse-engineering/06-quality/EDGE-CASES.md</target>
    </validation_targets>

    <success_criteria>
      <criterion>All data file names in TEST-COVERAGE.md match actual files in app/data/ASCII/ and app/data/EBCDIC/</criterion>
      <criterion>Record counts match actual data file content where verifiable (ASCII files)</criterion>
      <criterion>Boundary values correctly derived from actual PIC clauses — S9(10)V99 NOT S9(7)V99</criterion>
      <criterion>Test scenarios reference real program logic and actual EVALUATE/IF conditions</criterion>
      <criterion>Edge cases derived from actual PIC clause limits (e.g., max value for S9(10)V99 = 9999999999.99)</criterion>
      <criterion>All 31 COBOL programs have test coverage assessment</criterion>
      <criterion>Validation rules cited in test scenarios match actual code</criterion>
      <criterion>All 22 data files (9 ASCII + 13 EBCDIC) documented in inventory</criterion>
      <criterion>50+ test scenarios documented across all functional areas</criterion>
      <criterion>Boundary conditions documented for all PIC types used in the application</criterion>
      <criterion>No fabricated data file names (e.g., TRANDATA.txt, USERDATA.txt do NOT exist)</criterion>
      <criterion>CVCAR00Y.cpy and COUSR00Y.cpy hallucinations flagged if present</criterion>
      <criterion>S9(7)V99 hallucination flagged if present (actual: S9(10)V99)</criterion>
    </success_criteria>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<ground_truth>
  <data_files_ascii count="9" directory="app/data/ASCII/">
    <file name="acctdata.txt">Account reference data</file>
    <file name="carddata.txt">Card reference data</file>
    <file name="cardxref.txt">Card-to-account cross-reference data</file>
    <file name="custdata.txt">Customer reference data</file>
    <file name="dailytran.txt">Daily transaction reference data</file>
    <file name="discgrp.txt">Discount group reference data</file>
    <file name="tcatbal.txt">Transaction category balance data</file>
    <file name="trancatg.txt">Transaction category reference data</file>
    <file name="trantype.txt">Transaction type reference data</file>
  </data_files_ascii>

  <data_files_ebcdic count="13" directory="app/data/EBCDIC/">
    <file name="AWS.M2.CARDDEMO.ACCDATA.PS">Account data (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.ACCTDATA.PS">Account data alternate (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.CARDDATA.PS">Card data (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.CARDXREF.PS">Card cross-reference (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.CUSTDATA.PS">Customer data (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.DALYTRAN.PS">Daily transactions (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.DALYTRAN.PS.INIT">Daily transactions initial (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.DISCGRP.PS">Discount groups (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.EXPORT.DATA.PS">Export data (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.TCATBALF.PS">Transaction category balance (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.TRANCATG.PS">Transaction categories (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.TRANTYPE.PS">Transaction types (EBCDIC format)</file>
    <file name="AWS.M2.CARDDEMO.USRSEC.PS">User security (EBCDIC format)</file>
    <note>.gitkeep also present — not a data file but exists in directory listing</note>
  </data_files_ebcdic>

  <nonexistent_data_files>
    <hallucination>TRANDATA.txt does NOT exist in app/data/ASCII/ — actual file is dailytran.txt</hallucination>
    <hallucination>USERDATA.txt does NOT exist in app/data/ASCII/ — no user data in ASCII directory; user security is only in EBCDIC as AWS.M2.CARDDEMO.USRSEC.PS</hallucination>
    <hallucination>ACCTDATA.txt (uppercase) does NOT exist — actual file is acctdata.txt (lowercase)</hallucination>
    <hallucination>CARDDATA.txt (uppercase) does NOT exist — actual file is carddata.txt (lowercase)</hallucination>
    <hallucination>CUSTDATA.txt (uppercase) does NOT exist — actual file is custdata.txt (lowercase)</hallucination>
  </nonexistent_data_files>

  <cobol_programs count="31">
    <online_programs count="19">
      COACTUPC.cbl, COACTVWC.cbl, COADM01C.cbl, COBIL00C.cbl, COBSWAIT.cbl,
      COCRDLIC.cbl, COCRDSLC.cbl, COCRDUPC.cbl, COMEN01C.cbl, CORPT00C.cbl,
      COSGN00C.cbl, COTRN00C.cbl, COTRN01C.cbl, COTRN02C.cbl,
      COUSR00C.cbl, COUSR01C.cbl, COUSR02C.cbl, COUSR03C.cbl, CSUTLDTC.cbl
    </online_programs>
    <batch_programs count="12">
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

  <bms_mapsets count="17">
    COACTUP.bms, COACTVW.bms, COADM01.bms, COBIL00.bms, COCRDLI.bms,
    COCRDSL.bms, COCRDUP.bms, COMEN01.bms, CORPT00.bms, COSGN00.bms,
    COTRN00.bms, COTRN01.bms, COTRN02.bms, COUSR00.bms, COUSR01.bms,
    COUSR02.bms, COUSR03.bms
  </bms_mapsets>

  <jcl_files count="38">38 files in app/jcl/</jcl_files>

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

  <boundary_value_ground_truth>
    <field name="ACCT-CURR-BAL" pic="S9(10)V99" min="-9999999999.99" max="9999999999.99" source="CVACT01Y.cpy:7"/>
    <field name="ACCT-CREDIT-LIMIT" pic="S9(10)V99" min="-9999999999.99" max="9999999999.99" source="CVACT01Y.cpy:8"/>
    <field name="ACCT-CASH-CREDIT-LIMIT" pic="S9(10)V99" min="-9999999999.99" max="9999999999.99" source="CVACT01Y.cpy:9"/>
    <field name="ACCT-CURR-CYC-CREDIT" pic="S9(10)V99" min="-9999999999.99" max="9999999999.99" source="CVACT01Y.cpy:13"/>
    <field name="ACCT-CURR-CYC-DEBIT" pic="S9(10)V99" min="-9999999999.99" max="9999999999.99" source="CVACT01Y.cpy:14"/>
    <field name="ACCT-ID" pic="9(11)" min="00000000000" max="99999999999" source="CVACT01Y.cpy:5"/>
    <wrong_boundary pic="S9(7)V99" max_claimed="9999999.99" actual_pic="S9(10)V99" actual_max="9999999999.99"/>
  </boundary_value_ground_truth>

  <known_hallucinations>
    <hallucination id="H1">CVCAR00Y.cpy does NOT exist — actual card copybook is CVCRD01Y.cpy</hallucination>
    <hallucination id="H2">COUSR00Y.cpy does NOT exist — actual user security copybook is CSUSR01Y.cpy</hallucination>
    <hallucination id="H3">S9(7)V99 for monetary fields is WRONG — actual PIC is S9(10)V99 (see CVACT01Y.cpy:7)</hallucination>
    <hallucination id="H4">TRANDATA.txt does NOT exist in app/data/ASCII/ — actual file is dailytran.txt</hallucination>
    <hallucination id="H5">USERDATA.txt does NOT exist in app/data/ASCII/ — user security is only in EBCDIC</hallucination>
    <hallucination id="H6">Max value 9,999,999.99 is WRONG for monetary fields — actual max is 9,999,999,999.99</hallucination>
  </known_hallucinations>
</ground_truth>

<validation_methodology>
  <phase name="1_load_documentation">
    <description>Read all target test coverage documentation files and catalog every verifiable claim</description>
    <steps>
      <step>Read docs/reverse-engineering/06-quality/TEST-COVERAGE.md</step>
      <step>Read docs/reverse-engineering/06-quality/TEST-SCENARIOS.md</step>
      <step>Read docs/reverse-engineering/06-quality/EDGE-CASES.md</step>
      <step>For each document, extract: data file references, record counts, PIC clauses, boundary values, program references, test scenario counts</step>
      <step>Create a claims inventory in .work/reverse-engineering/validation/vl-009/claims-inventory.yaml</step>
    </steps>
  </phase>

  <phase name="2_data_file_verification">
    <description>Verify every data file reference against actual directory contents</description>
    <steps>
      <step>List all files in app/data/ASCII/ and compare against documented inventory</step>
      <step>List all files in app/data/EBCDIC/ and compare against documented inventory</step>
      <step>Flag any data file names that do not exist (e.g., TRANDATA.txt, USERDATA.txt)</step>
      <step>Verify file name casing matches actual files (lowercase acctdata.txt not uppercase ACCTDATA.txt)</step>
      <step>Check that all 9 ASCII and 13 EBCDIC files are documented</step>
      <step>Verify record counts against actual file content where feasible (line counts for ASCII files)</step>
      <step>Record all findings in .work/reverse-engineering/validation/vl-009/data-file-findings.yaml</step>
    </steps>
  </phase>

  <phase name="3_boundary_value_verification">
    <description>Verify all boundary value calculations against actual PIC clauses in copybooks</description>
    <steps>
      <step>For each boundary value claim: locate the cited PIC clause in the actual copybook</step>
      <step>Verify the PIC clause matches exactly — S9(10)V99 NOT S9(7)V99</step>
      <step>Recalculate min/max values from the actual PIC clause</step>
      <step>For S9(10)V99: verify max = 9999999999.99, min = -9999999999.99</step>
      <step>Flag any S9(7)V99 references (max would be 9999999.99 — three orders of magnitude too small)</step>
      <step>Verify boundary values for non-monetary fields: 9(11) account ID, X(n) string fields</step>
      <step>Check that edge cases correctly derive from PIC clause limits</step>
      <step>Record all findings in .work/reverse-engineering/validation/vl-009/boundary-findings.yaml</step>
    </steps>
  </phase>

  <phase name="4_test_scenario_verification">
    <description>Verify test scenarios reference real program logic and actual validation conditions</description>
    <steps>
      <step>For each test scenario: verify the cited program exists in app/cbl/</step>
      <step>For scenarios citing EVALUATE/IF conditions: read the actual program and confirm the logic exists</step>
      <step>For scenarios citing error messages: verify the exact message text in the source</step>
      <step>For scenarios citing file:line references: read the source at that line and confirm</step>
      <step>Verify validation rules match actual code (e.g., payment amount checks in COBIL00C)</step>
      <step>Check that 50+ test scenarios are documented across all categories</step>
      <step>Verify test scenario categories cover: positive, negative, boundary, state transition, integration, error recovery</step>
      <step>Flag any references to CVCAR00Y.cpy or COUSR00Y.cpy in test scenarios</step>
      <step>Record all findings in .work/reverse-engineering/validation/vl-009/scenario-findings.yaml</step>
    </steps>
  </phase>

  <phase name="5_completeness_assessment">
    <description>Check all expected items are covered in the test coverage documentation</description>
    <checks>
      <check>All 31 COBOL programs have test coverage assessment (both CO* online and CB* batch)</check>
      <check>All 9 ASCII data files documented in test data inventory</check>
      <check>All 13 EBCDIC data files documented in test data inventory</check>
      <check>50+ test scenarios documented</check>
      <check>Boundary conditions documented for all PIC types: S9(10)V99, 9(11), X(n)</check>
      <check>Validation rules extracted from at least 10 programs</check>
      <check>Error condition catalog with program-specific error messages</check>
      <check>Both online and batch program test coverage addressed</check>
      <check>Cross-field validation scenarios documented (e.g., payment vs balance)</check>
      <check>CICS-specific test conditions documented (RESP/RESP2 handling)</check>
      <check>File status code test scenarios documented</check>
      <check>88-level condition values used as test case inputs</check>
    </checks>
  </phase>

  <phase name="6_report_generation">
    <description>Produce the structured validation report</description>
    <output>docs/reverse-engineering/validation/VL-009-test-coverage-report.md</output>
    <format>
      - Verdict: PASS (100) / FAIL (less than 100)
      - Score breakdown by category (5 categories, weighted)
      - Critical findings (hallucinated data files, wrong PIC clauses, fabricated boundary values)
      - Major findings (missing program coverage, incomplete data file inventory)
      - Minor findings (formatting, categorization issues)
      - Hallucination inventory table
      - Data file accuracy table (documented vs actual)
      - Boundary value accuracy table (claimed vs actual)
      - Completeness gaps table
      - Specific remediation recommendations
    </format>
  </phase>
</validation_methodology>

<scoring_rubric>
  <category name="Source Reference Accuracy" weight="35" severity="Critical">
    <description>Every data file name, program reference, and copybook citation verified against actual source</description>
    <scoring>
      100: All data file names correct, all program references valid, all copybook citations accurate
      80: 90%+ references correct, no fabricated data files, minor casing issues
      60: 80%+ correct, one or two fabricated file names (e.g., TRANDATA.txt)
      40: References to non-existent data files found, wrong PIC clauses cited
      20: Multiple fabricated data file names and program references
      0: Majority of file references unverifiable or fabricated
    </scoring>
  </category>
  <category name="Factual Accuracy" weight="25" severity="Critical">
    <description>Boundary values, PIC clauses, record counts, and validation logic descriptions match reality</description>
    <scoring>
      100: All boundary values correctly derived, all PIC clauses match, record counts verified
      80: 95%+ facts correct, boundary values accurate for all monetary fields
      60: Minor boundary value errors for non-critical fields, PIC clauses mostly correct
      40: Wrong PIC clauses found (e.g., S9(7)V99 vs S9(10)V99), incorrect boundary maximums
      20: Multiple factual errors in boundary values and validation logic
      0: Pervasive factual inaccuracies in boundary analysis
    </scoring>
  </category>
  <category name="Completeness" weight="20" severity="Major">
    <description>All expected items covered: 22 data files, 31 programs, 50+ scenarios, all PIC types</description>
    <scoring>
      100: All 22 data files, all 31 programs assessed, 50+ scenarios, all PIC types covered
      80: 90%+ coverage, all major data files and programs present, 40+ scenarios
      60: 75%+ coverage, some data files or programs missing, 30+ scenarios
      40: Major data files or programs missing, fewer than 30 scenarios
      20: Less than 50% of data files or programs covered
      0: Severely incomplete — major categories missing
    </scoring>
  </category>
  <category name="Quantitative Accuracy" weight="10" severity="Major">
    <description>File counts, record counts, scenario counts, boundary value calculations correct</description>
    <scoring>
      100: All counts match ground truth exactly, boundary math correct
      80: Counts within 5% tolerance, boundary values correct for monetary fields
      60: Minor discrepancies in non-critical counts, boundary values mostly correct
      40: Major count errors (e.g., wrong number of data files or programs)
      20: Multiple count errors and incorrect boundary calculations
      0: Counts are unreliable throughout
    </scoring>
  </category>
  <category name="Documentation Quality" weight="10" severity="Minor">
    <description>Test scenarios well-structured, edge cases clearly documented, tables formatted correctly</description>
    <scoring>
      100: Professional quality, clear test scenario format, actionable edge case documentation
      80: Minor formatting issues, test scenarios mostly well-structured
      60: Some inconsistent formatting, test scenarios usable but not ideal
      40: Multiple formatting issues, unclear test scenario descriptions
      20: Poor formatting throughout, test scenarios difficult to use
      0: Unusable documentation
    </scoring>
  </category>
</scoring_rubric>

<output_specification>
  <report_file>docs/reverse-engineering/validation/VL-009-test-coverage-report.md</report_file>
  <work_directory>.work/reverse-engineering/validation/vl-009/</work_directory>

  <report_structure>
    # Validation Report: Test Coverage Analysis (RE-009)

    ## Verdict: [PASS|FAIL] — Score: [NN]/100

    ## Score Breakdown
    | Category | Weight | Score | Weighted |
    |----------|--------|-------|----------|
    | Source Reference Accuracy | 35% | NN/100 | NN |
    | Factual Accuracy | 25% | NN/100 | NN |
    | Completeness | 20% | NN/100 | NN |
    | Quantitative Accuracy | 10% | NN/100 | NN |
    | Documentation Quality | 10% | NN/100 | NN |
    | **Total** | **100%** | | **NN/100** |

    ## Critical Findings
    [Hallucinated data file names, wrong PIC clauses, fabricated boundary values]

    ## Major Findings
    [Missing program coverage, incomplete data file inventory, missing test scenarios]

    ## Minor Findings
    [Formatting issues, categorization problems]

    ## Hallucination Inventory
    | ID | Claim | Reality | Document | Severity |
    |----|-------|---------|----------|----------|
    | H1 | TRANDATA.txt exists | File does not exist; actual is dailytran.txt | TEST-COVERAGE.md | Critical |
    | H2 | S9(7)V99 for monetary fields | Actual PIC is S9(10)V99 per CVACT01Y.cpy:7 | EDGE-CASES.md | Critical |

    ## Data File Accuracy
    | Documented File | Actual File | Status | Notes |
    |-----------------|-------------|--------|-------|
    | acctdata.txt | acctdata.txt | MATCH | |
    | TRANDATA.txt | (does not exist) | HALLUCINATION | Actual: dailytran.txt |

    ## Boundary Value Accuracy
    | Field | Documented PIC | Actual PIC | Documented Max | Actual Max | Status |
    |-------|---------------|------------|----------------|------------|--------|
    | ACCT-CURR-BAL | S9(7)V99 | S9(10)V99 | 9,999,999.99 | 9,999,999,999.99 | ERROR |

    ## Completeness Gaps
    | Category | Expected | Documented | Gap |
    |----------|----------|------------|-----|
    | ASCII data files | 9 | ? | ? missing |
    | EBCDIC data files | 13 | ? | ? missing |
    | Programs assessed | 31 | ? | ? missing |
    | Test scenarios | 50+ | ? | ? short |

    ## Recommendations
    [Specific remediation actions for each finding]

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
  <principle id="1">Every data file name in the documentation must resolve to an actual file in app/data/ASCII/ or app/data/EBCDIC/ — verify by listing the directory</principle>
  <principle id="2">Boundary values must be mathematically derived from the actual PIC clause — S9(10)V99 yields max 9999999999.99, not 9999999.99</principle>
  <principle id="3">Test scenarios must cite real EVALUATE/IF validation logic verifiable in the source program</principle>
  <principle id="4">Record counts for ASCII data files can be verified by counting lines — discrepancies must be flagged</principle>
  <principle id="5">Every program cited in test scenarios must exist in app/cbl/ — verify each reference</principle>
  <principle id="6">88-level conditions used as test inputs must exist in the cited copybook with the exact VALUE clause</principle>
  <principle id="7">Error messages cited in negative test scenarios must appear verbatim in the source program</principle>
  <principle id="8">All 31 programs must have test coverage assessment — missing programs represent coverage gaps that must be reported</principle>
</foundational_principles>

<critical_reminders>
  <reminder id="1">ASCII data files are LOWERCASE: acctdata.txt, carddata.txt, etc. — not ACCTDATA.txt. Flag case mismatches.</reminder>
  <reminder id="2">TRANDATA.txt and USERDATA.txt do NOT exist in app/data/ASCII/. The actual files are dailytran.txt and (for user security) only the EBCDIC AWS.M2.CARDDEMO.USRSEC.PS. Flag immediately if referenced.</reminder>
  <reminder id="3">Monetary PIC is S9(10)V99, NOT S9(7)V99. Max is 9,999,999,999.99, NOT 9,999,999.99. Verify at CVACT01Y.cpy:7.</reminder>
  <reminder id="4">CVCAR00Y.cpy does NOT exist — the card copybook is CVCRD01Y.cpy. Flag immediately if referenced in test scenarios.</reminder>
  <reminder id="5">COUSR00Y.cpy does NOT exist — the user security copybook is CSUSR01Y.cpy. Flag immediately if referenced in test scenarios.</reminder>
  <reminder id="6">There are exactly 9 ASCII data files and 13 EBCDIC data files (plus .gitkeep). Verify the documented count matches.</reminder>
  <reminder id="7">There are 31 COBOL programs (29 .cbl + 2 .CBL), NOT 39 or any other number. All 31 must have test coverage.</reminder>
  <reminder id="8">Read EVERY cited source file:line — do not trust that validation logic excerpts or error messages are accurate.</reminder>
  <reminder id="9">The RE-009 prompt itself contains the S9(7)V99 error in its examples and templates — the validation documentation may have inherited this error. Check carefully.</reminder>
  <reminder id="10">Verify that test scenarios cover both online (CO*) and batch (CB*) programs — batch programs have different test characteristics than online programs.</reminder>
</critical_reminders>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/validation/vl-009/</path>
    <purpose>Persist validation progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
      validation_phase: "load_docs|data_file_verification|boundary_verification|scenario_verification|completeness|reporting"
      current_document: "filename being validated"
      documents_completed: ["list of validated documents"]
      documents_remaining: ["list of remaining documents"]
      findings:
        critical: N
        major: N
        minor: N
      data_files_verified:
        ascii_correct: N
        ascii_hallucinated: N
        ebcdic_correct: N
        ebcdic_hallucinated: N
      boundary_values_verified:
        correct: N
        incorrect: N
      programs_with_coverage: N
      test_scenarios_counted: N
      next_action: "Detailed description of next step"
      last_updated: "ISO timestamp"
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/validation/vl-009/progress.yaml if it exists</step>
    <step>2. Load any existing findings from the work directory</step>
    <step>3. Continue from next_action without re-validating completed documents</step>
    <step>4. Update progress.yaml after each phase completion</step>
  </resumption_protocol>
</context_compaction_survival>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/validation/vl-009/progress.yaml if it exists
    2. If progress exists and validation_phase != "complete":
       - Load existing findings
       - Resume from next_action
    3. If no progress or starting fresh:
       - Begin with Phase 1: Load Documentation
       - Create initial progress.yaml
    4. Phase 2: Verify every data file name against actual directory listings
       - List app/data/ASCII/ and compare against documented file names
       - List app/data/EBCDIC/ and compare against documented file names
       - Flag any fabricated file names (TRANDATA.txt, USERDATA.txt, wrong casing)
    5. Phase 3: Verify boundary values against actual PIC clauses
       - Read copybooks and confirm PIC clauses match documented values
       - Recalculate min/max values from actual PIC clauses
       - Flag S9(7)V99 if present (should be S9(10)V99)
    6. Phase 4: Verify test scenarios against actual program logic
       - Read cited programs and confirm validation logic exists
       - Verify error messages match actual source
       - Check EVALUATE/IF conditions are accurately described
    7. Phase 5: Assess completeness
       - Verify all 31 programs have coverage assessment
       - Count test scenarios (target: 50+)
       - Check all data files documented
    8. Phase 6: Generate validation report
    9. Write final report to docs/reverse-engineering/validation/VL-009-test-coverage-report.md
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to validate the RE-009 test coverage documentation. The prompt will:

1. **Load** all test coverage documentation files from docs/reverse-engineering/06-quality/
2. **Verify** every data file name against actual directory listings in app/data/ASCII/ and app/data/EBCDIC/
3. **Check** boundary value accuracy by reading actual PIC clauses from copybooks
4. **Validate** test scenarios against real program logic and EVALUATE/IF conditions
5. **Assess** completeness against the full source and data file inventory
6. **Report** findings with severity ratings, hallucination inventory, and remediation guidance

## Expected Output

| File | Description |
|------|-------------|
| `VL-009-test-coverage-report.md` | Structured validation report with verdict, score, data file accuracy table, boundary value accuracy table, and findings |

## Prerequisites

- RE-009 must have been executed and output exists in `docs/reverse-engineering/06-quality/`
- Expected files: `TEST-COVERAGE.md`, `TEST-SCENARIOS.md`, `EDGE-CASES.md`
- Access to source code in `app/cbl/`, `app/cpy/`
- Access to data files in `app/data/ASCII/` and `app/data/EBCDIC/`

## Depends On

- VL-001 (Domain Model Validation) — Phase 1 foundation
- VL-002 (Data Model Validation) — Phase 1 foundation, data constraint verification
- This is a Phase 5 validation prompt

## Blocks

- VL-000 (Cross-Document Consistency Validation)
