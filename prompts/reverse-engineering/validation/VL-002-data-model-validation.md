# VL-002: Data Model Validation

## Prompt

```xml
<context>
  <project>CardDemo</project>
  <role>
    <persona>Validation Analyst specializing in data model verification for COBOL-to-relational migration documentation</persona>
    <validation_expertise>
      <skill>Cross-referencing data model claims against COBOL source</skill>
      <skill>Verifying PIC clause to SQL type mappings</skill>
      <skill>Detecting hallucinated record layouts and field definitions</skill>
      <skill>Validating VSAM file organization claims</skill>
      <skill>COMP-3 byte calculation verification</skill>
      <skill>ER diagram accuracy assessment</skill>
    </validation_expertise>
    <mainframe_expertise>
      <skill>COBOL FD entries and SELECT statements</skill>
      <skill>VSAM KSDS key definitions and alternate indexes</skill>
      <skill>PIC clause semantics (S9(n)V9(m), COMP-3 storage)</skill>
      <skill>REDEFINES and OCCURS clause interpretation</skill>
      <skill>File status codes</skill>
      <skill>EBCDIC encoding</skill>
    </mainframe_expertise>
    <carddemo_context>
      Ground truth: 31 COBOL programs, 30 copybooks, 17 BMS copybooks, 17 BMS mapsets, 38 JCL, 22 data files (9 ASCII + 13 EBCDIC).
      Card copybook = CVCRD01Y.cpy (NOT CVCAR00Y.cpy). User security = CSUSR01Y.cpy (NOT COUSR00Y.cpy). Monetary PIC = S9(10)V99 (NOT S9(7)V99).
    </carddemo_context>
    <mindset>Trust nothing. Verify everything against source code.</mindset>
  </role>
  <objective>
    <primary_goal>Validate RE-002 data model documentation for correctness, accuracy, completeness, and absence of hallucinations</primary_goal>
    <validation_targets>
      <target>docs/reverse-engineering/02-data-model/DATA-MODEL.md</target>
      <target>docs/reverse-engineering/02-data-model/DATA-DICTIONARY.md</target>
      <target>docs/reverse-engineering/02-data-model/DATA-LINEAGE.md</target>
      <target>docs/reverse-engineering/02-data-model/diagrams/er-diagram.md</target>
    </validation_targets>
    <success_criteria>
      <criterion>All PIC clauses match actual copybook definitions (especially S9(10)V99 not S9(7)V99)</criterion>
      <criterion>All record lengths verified against copybook comments</criterion>
      <criterion>COMP-3 byte calculations correct ((n+1)/2 rounded up)</criterion>
      <criterion>SQL type mappings correct for actual PICs</criterion>
      <criterion>All 6 VSAM files documented with correct keys</criterion>
      <criterion>CVCAR00Y.cpy and COUSR00Y.cpy hallucinations flagged</criterion>
      <criterion>ER diagram entities match actual copybook record structures</criterion>
    </success_criteria>
  </objective>
  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<ground_truth>
  <cobol_programs directory="app/cbl/" count="31">
    <batch_programs>
      <program>CBACT01C.cbl</program>
      <program>CBACT02C.cbl</program>
      <program>CBACT03C.cbl</program>
      <program>CBACT04C.cbl</program>
      <program>CBCUS01C.cbl</program>
      <program>CBEXPORT.cbl</program>
      <program>CBIMPORT.cbl</program>
      <program>CBSTM03A.CBL</program>
      <program>CBSTM03B.CBL</program>
      <program>CBTRN01C.cbl</program>
      <program>CBTRN02C.cbl</program>
      <program>CBTRN03C.cbl</program>
    </batch_programs>
    <online_programs>
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
  </cobol_programs>

  <copybooks directory="app/cpy/" count="30">
    <copybook>COADM02Y.cpy</copybook>
    <copybook>COCOM01Y.cpy</copybook>
    <copybook>CODATECN.cpy</copybook>
    <copybook>COMEN02Y.cpy</copybook>
    <copybook>COSTM01.CPY</copybook>
    <copybook>COTTL01Y.cpy</copybook>
    <copybook>CSDAT01Y.cpy</copybook>
    <copybook>CSLKPCDY.cpy</copybook>
    <copybook>CSMSG01Y.cpy</copybook>
    <copybook>CSMSG02Y.cpy</copybook>
    <copybook>CSSETATY.cpy</copybook>
    <copybook>CSSTRPFY.cpy</copybook>
    <copybook>CSUSR01Y.cpy</copybook>
    <copybook>CSUTLDPY.cpy</copybook>
    <copybook>CSUTLDWY.cpy</copybook>
    <copybook>CUSTREC.cpy</copybook>
    <copybook>CVACT01Y.cpy</copybook>
    <copybook>CVACT02Y.cpy</copybook>
    <copybook>CVACT03Y.cpy</copybook>
    <copybook>CVCRD01Y.cpy</copybook>
    <copybook>CVCUS01Y.cpy</copybook>
    <copybook>CVEXPORT.cpy</copybook>
    <copybook>CVTRA01Y.cpy</copybook>
    <copybook>CVTRA02Y.cpy</copybook>
    <copybook>CVTRA03Y.cpy</copybook>
    <copybook>CVTRA04Y.cpy</copybook>
    <copybook>CVTRA05Y.cpy</copybook>
    <copybook>CVTRA06Y.cpy</copybook>
    <copybook>CVTRA07Y.cpy</copybook>
    <copybook>UNUSED1Y.cpy</copybook>
  </copybooks>

  <bms_copybooks directory="app/cpy-bms/" count="17">
    <copybook>COACTUP.CPY</copybook>
    <copybook>COACTVW.CPY</copybook>
    <copybook>COADM01.CPY</copybook>
    <copybook>COBIL00.CPY</copybook>
    <copybook>COCRDLI.CPY</copybook>
    <copybook>COCRDSL.CPY</copybook>
    <copybook>COCRDUP.CPY</copybook>
    <copybook>COMEN01.CPY</copybook>
    <copybook>CORPT00.CPY</copybook>
    <copybook>COSGN00.CPY</copybook>
    <copybook>COTRN00.CPY</copybook>
    <copybook>COTRN01.CPY</copybook>
    <copybook>COTRN02.CPY</copybook>
    <copybook>COUSR00.CPY</copybook>
    <copybook>COUSR01.CPY</copybook>
    <copybook>COUSR02.CPY</copybook>
    <copybook>COUSR03.CPY</copybook>
  </bms_copybooks>

  <bms_mapsets directory="app/bms/" count="17">
    <mapset>COACTUP.bms</mapset>
    <mapset>COACTVW.bms</mapset>
    <mapset>COADM01.bms</mapset>
    <mapset>COBIL00.bms</mapset>
    <mapset>COCRDLI.bms</mapset>
    <mapset>COCRDSL.bms</mapset>
    <mapset>COCRDUP.bms</mapset>
    <mapset>COMEN01.bms</mapset>
    <mapset>CORPT00.bms</mapset>
    <mapset>COSGN00.bms</mapset>
    <mapset>COTRN00.bms</mapset>
    <mapset>COTRN01.bms</mapset>
    <mapset>COTRN02.bms</mapset>
    <mapset>COUSR00.bms</mapset>
    <mapset>COUSR01.bms</mapset>
    <mapset>COUSR02.bms</mapset>
    <mapset>COUSR03.bms</mapset>
  </bms_mapsets>

  <jcl_files directory="app/jcl/" count="38">
    <note>33 .jcl (lowercase) + 5 .JCL (uppercase)</note>
  </jcl_files>

  <vsam_files>
    <file name="ACCTDAT" type="KSDS" key="ACCT-ID PIC 9(11)" record_copybook="CVACT01Y.cpy" record_length="300">
      <description>Account master file</description>
    </file>
    <file name="CARDDAT" type="KSDS" key="card number" record_copybook="CVCRD01Y.cpy">
      <description>Card master file</description>
    </file>
    <file name="CUSTDAT" type="KSDS" key="customer ID" record_copybook="CVCUS01Y.cpy">
      <description>Customer master file</description>
    </file>
    <file name="TRANSACT" type="KSDS" key="transaction ID" record_copybook="CVTRA01Y.cpy">
      <description>Daily transaction log</description>
    </file>
    <file name="CCXREF" type="KSDS" key="card number" record_copybook="CVACT03Y.cpy">
      <description>Card-account cross-reference</description>
    </file>
    <file name="USRSEC" type="KSDS" key="user ID" record_copybook="CSUSR01Y.cpy">
      <description>User security file</description>
    </file>
  </vsam_files>

  <key_pic_clauses source="CVACT01Y.cpy">
    <field line="5">ACCT-ID PIC 9(11)</field>
    <field line="6">ACCT-ACTIVE-STATUS PIC X(01)</field>
    <field line="7">ACCT-CURR-BAL PIC S9(10)V99</field>
    <field line="8">ACCT-CREDIT-LIMIT PIC S9(10)V99</field>
    <field line="9">ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99</field>
    <field line="10">ACCT-OPEN-DATE PIC X(10)</field>
    <field line="11">ACCT-EXPIRAION-DATE PIC X(10) — note: typo in original source, not EXPIRATION</field>
    <field line="12">ACCT-REISSUE-DATE PIC X(10)</field>
    <field line="13">ACCT-CURR-CYC-CREDIT PIC S9(10)V99</field>
    <field line="14">ACCT-CURR-CYC-DEBIT PIC S9(10)V99</field>
    <field line="15">ACCT-ADDR-ZIP PIC X(10)</field>
    <field line="16">ACCT-GROUP-ID PIC X(10)</field>
    <field line="17">FILLER PIC X(178)</field>
  </key_pic_clauses>

  <key_pic_clauses source="CSUSR01Y.cpy">
    <field line="18">SEC-USR-ID PIC X(08)</field>
    <field line="19">SEC-USR-FNAME PIC X(20)</field>
    <field line="20">SEC-USR-LNAME PIC X(20)</field>
    <field line="21">SEC-USR-PWD PIC X(08)</field>
    <field line="22">SEC-USR-TYPE PIC X(01)</field>
    <field line="23">SEC-USR-FILLER PIC X(23)</field>
  </key_pic_clauses>

  <known_hallucinations>
    <hallucination id="H1">
      <claim>CVCAR00Y.cpy is the card copybook</claim>
      <reality>CVCAR00Y.cpy does NOT exist — actual card copybook is CVCRD01Y.cpy</reality>
      <affected_prompts>RE-001, RE-002</affected_prompts>
    </hallucination>
    <hallucination id="H2">
      <claim>COUSR00Y.cpy is the user security copybook</claim>
      <reality>COUSR00Y.cpy does NOT exist — actual user security copybook is CSUSR01Y.cpy</reality>
      <affected_prompts>RE-001, RE-002, RE-007</affected_prompts>
    </hallucination>
    <hallucination id="H3">
      <claim>S9(7)V99 COMP-3 for monetary fields</claim>
      <reality>Actual PIC is S9(10)V99 (see CVACT01Y.cpy lines 7-9, 13-14)</reality>
      <affected_prompts>RE-001, RE-002, RE-009</affected_prompts>
    </hallucination>
  </known_hallucinations>

  <data_files>
    <ascii_files directory="app/data/ASCII/" count="9">
      <file>acctdata.txt</file>
      <file>carddata.txt</file>
      <file>cardxref.txt</file>
      <file>custdata.txt</file>
      <file>dailytran.txt</file>
      <file>discgrp.txt</file>
      <file>tcatbal.txt</file>
      <file>trancatg.txt</file>
      <file>trantype.txt</file>
    </ascii_files>
    <ebcdic_files directory="app/data/EBCDIC/" count="13">
      <file>AWS.M2.CARDDEMO.ACCDATA.PS</file>
      <file>AWS.M2.CARDDEMO.ACCTDATA.PS</file>
      <file>AWS.M2.CARDDEMO.CARDDATA.PS</file>
      <file>AWS.M2.CARDDEMO.CARDXREF.PS</file>
      <file>AWS.M2.CARDDEMO.CUSTDATA.PS</file>
      <file>AWS.M2.CARDDEMO.DALYTRAN.PS</file>
      <file>AWS.M2.CARDDEMO.DALYTRAN.PS.INIT</file>
      <file>AWS.M2.CARDDEMO.DISCGRP.PS</file>
      <file>AWS.M2.CARDDEMO.EXPORT.DATA.PS</file>
      <file>AWS.M2.CARDDEMO.TCATBALF.PS</file>
      <file>AWS.M2.CARDDEMO.TRANCATG.PS</file>
      <file>AWS.M2.CARDDEMO.TRANTYPE.PS</file>
      <file>AWS.M2.CARDDEMO.USRSEC.PS</file>
    </ebcdic_files>
  </data_files>
</ground_truth>

<validation_methodology>
  <phase name="1_load_documentation">
    <description>Load and inventory all RE-002 output documents</description>
    <steps>
      <step>Read docs/reverse-engineering/02-data-model/DATA-MODEL.md</step>
      <step>Read docs/reverse-engineering/02-data-model/DATA-DICTIONARY.md</step>
      <step>Read docs/reverse-engineering/02-data-model/DATA-LINEAGE.md</step>
      <step>Read docs/reverse-engineering/02-data-model/diagrams/er-diagram.md</step>
      <step>Inventory all claims made: VSAM files, copybooks, fields, PIC clauses, relationships, SQL types</step>
      <step>Create checklist of every verifiable claim for subsequent phases</step>
    </steps>
    <output_artifact>.work/reverse-engineering/validation/vl-002/claims-inventory.yaml</output_artifact>
  </phase>

  <phase name="2_source_reference_verification">
    <description>Verify every file:line citation against actual source code</description>
    <steps>
      <step>For each file reference in the documentation, verify the file exists at the stated path</step>
      <step>For each line number citation, read the actual source line and compare to the claim</step>
      <step>Flag any reference to CVCAR00Y.cpy (does not exist — should be CVCRD01Y.cpy)</step>
      <step>Flag any reference to COUSR00Y.cpy (does not exist — should be CSUSR01Y.cpy)</step>
      <step>Verify copybook names match actual filenames including case sensitivity</step>
      <step>Record every verified and failed reference with source evidence</step>
    </steps>
    <output_artifact>.work/reverse-engineering/validation/vl-002/source-reference-results.yaml</output_artifact>
  </phase>

  <phase name="3_factual_accuracy_check">
    <description>Verify data-model-specific factual claims against COBOL source</description>
    <steps>
      <step>PIC clause verification: For every PIC clause cited in DATA-DICTIONARY.md, read the actual copybook and compare</step>
      <step>Verify monetary fields use S9(10)V99, NOT S9(7)V99 — read CVACT01Y.cpy lines 7-9, 13-14</step>
      <step>SQL type mapping verification: Check that each PIC-to-SQL mapping follows correct conversion rules</step>
      <step>COMP-3 byte calculation verification: For every COMP-3 field, verify (n+1)/2 rounded up</step>
      <step>Record length verification: Sum all field lengths in each copybook and compare to documented record length</step>
      <step>Verify ACCTDAT record length = 300 bytes as stated in CVACT01Y.cpy comment line 2</step>
      <step>Verify VSAM key definitions match SELECT statements in COBOL programs</step>
      <step>Verify relationship cardinalities match program logic (1:1, 1:N, M:N)</step>
      <step>Verify field name spelling matches source exactly (e.g., ACCT-EXPIRAION-DATE not ACCT-EXPIRATION-DATE)</step>
    </steps>
    <output_artifact>.work/reverse-engineering/validation/vl-002/factual-accuracy-results.yaml</output_artifact>
  </phase>

  <phase name="4_completeness_assessment">
    <description>Check that all data model elements are covered</description>
    <steps>
      <step>Verify all 6 VSAM files are documented: ACCTDAT, CARDDAT, CUSTDAT, TRANSACT, CCXREF, USRSEC</step>
      <step>Verify all fields from each record copybook are documented (not just key fields)</step>
      <step>Verify FILLER fields are accounted for in record length calculations</step>
      <step>Verify all 9 ASCII data files are documented: acctdata.txt, carddata.txt, cardxref.txt, custdata.txt, dailytran.txt, discgrp.txt, tcatbal.txt, trancatg.txt, trantype.txt</step>
      <step>Verify all 13 EBCDIC data files are documented</step>
      <step>Verify data lineage covers key batch programs: CBTRN01C (posting), CBTRN02C (interest), CBTRN03C (statements)</step>
      <step>Verify data lineage covers key online programs: COTRN00C (transaction entry), COACTUPC (account update)</step>
      <step>Verify 88-level conditions are documented as valid value domains</step>
      <step>Check for missing copybooks: CVACT02Y.cpy, CVACT03Y.cpy, CVTRA01Y.cpy through CVTRA07Y.cpy</step>
      <step>Verify cross-reference between physical VSAM files and logical entities</step>
    </steps>
    <output_artifact>.work/reverse-engineering/validation/vl-002/completeness-results.yaml</output_artifact>
  </phase>

  <phase name="5_quality_assessment">
    <description>Evaluate documentation quality, diagrams, and formatting</description>
    <steps>
      <step>Validate ER diagram Mermaid syntax — paste into Mermaid renderer or check syntax rules</step>
      <step>Verify ER diagram entities match actual copybook record structures</step>
      <step>Verify ER diagram relationships match documented cardinalities</step>
      <step>Verify ER diagram field names correspond to actual COBOL field names</step>
      <step>Validate data lineage Mermaid flowchart syntax</step>
      <step>Check markdown formatting: headers, tables, code blocks properly structured</step>
      <step>Verify data dictionary table columns are complete and consistent</step>
      <step>Check for internal consistency across the four documentation files</step>
    </steps>
    <output_artifact>.work/reverse-engineering/validation/vl-002/quality-results.yaml</output_artifact>
  </phase>

  <phase name="6_report_generation">
    <description>Compile all findings into the final validation report</description>
    <steps>
      <step>Calculate scores for each weighted category based on findings</step>
      <step>Classify each finding by severity: Critical, Major, Minor</step>
      <step>Compile hallucination inventory with source evidence</step>
      <step>Compile completeness gap table</step>
      <step>Generate specific, actionable remediation recommendations</step>
      <step>Determine overall verdict: PASS (100), FAIL (less than 100)</step>
      <step>Write final report to docs/reverse-engineering/validation/VL-002-data-model-report.md</step>
    </steps>
    <output_artifact>docs/reverse-engineering/validation/VL-002-data-model-report.md</output_artifact>
  </phase>
</validation_methodology>

<scoring_rubric>
  <category name="Source Reference Accuracy" weight="35" severity="Critical">
    <description>Every file:line citation verified against actual source code</description>
    <scoring_guide>
      <score value="90-100">All file references exist, all line citations match actual source content</score>
      <score value="70-89">Most references correct, 1-2 minor line number discrepancies</score>
      <score value="50-69">Several incorrect file references or hallucinated file names (e.g., CVCAR00Y.cpy)</score>
      <score value="30-49">Many fabricated references, multiple non-existent files cited</score>
      <score value="0-29">Majority of references are fabricated or unverifiable</score>
    </scoring_guide>
  </category>

  <category name="Factual Accuracy" weight="25" severity="Critical">
    <description>PIC clauses, SQL type mappings, field names, COMP-3 calculations match reality</description>
    <scoring_guide>
      <score value="90-100">All PIC clauses correct (S9(10)V99 for monetary), all SQL mappings valid, all field names exact</score>
      <score value="70-89">Minor PIC discrepancies that do not affect migration (e.g., display vs packed not distinguished)</score>
      <score value="50-69">S9(7)V99 used instead of S9(10)V99, or SQL mappings incorrect for actual PICs</score>
      <score value="30-49">Multiple wrong PIC clauses, fabricated field definitions, incorrect COMP-3 calculations</score>
      <score value="0-29">Majority of data type claims are incorrect</score>
    </scoring_guide>
  </category>

  <category name="Completeness" weight="20" severity="Major">
    <description>All VSAM files, all fields, all data files, all lineage paths documented</description>
    <scoring_guide>
      <score value="90-100">All 6 VSAM files with complete field inventories, all data files, full lineage</score>
      <score value="70-89">All VSAM files present but some fields or data files missing</score>
      <score value="50-69">1-2 VSAM files missing, or significant field gaps in documented files</score>
      <score value="30-49">Multiple VSAM files missing, most data files undocumented</score>
      <score value="0-29">Fewer than half of VSAM files documented</score>
    </scoring_guide>
  </category>

  <category name="Quantitative Accuracy" weight="10" severity="Major">
    <description>File counts, record lengths, byte calculations, field counts correct</description>
    <scoring_guide>
      <score value="90-100">Record lengths match copybook comments, COMP-3 calcs correct, field counts accurate</score>
      <score value="70-89">Minor arithmetic errors in non-critical calculations</score>
      <score value="50-69">Record length wrong for 1-2 files, or COMP-3 calculation errors</score>
      <score value="30-49">Multiple record lengths wrong, systematic calculation errors</score>
      <score value="0-29">Most quantitative claims are incorrect</score>
    </scoring_guide>
  </category>

  <category name="Documentation Quality" weight="10" severity="Minor">
    <description>Mermaid syntax valid, markdown well-formed, ER diagram accurate, tables complete</description>
    <scoring_guide>
      <score value="90-100">All Mermaid diagrams render correctly, ER entities match source, clean formatting</score>
      <score value="70-89">Minor Mermaid syntax issues, generally well-structured</score>
      <score value="50-69">Mermaid diagrams broken, or ER diagram entities do not match copybooks</score>
      <score value="30-49">Multiple broken diagrams, inconsistent formatting</score>
      <score value="0-29">Documentation is largely unreadable or structurally broken</score>
    </scoring_guide>
  </category>

  <verdict_thresholds>
    <threshold min="100" max="100" verdict="PASS">Documentation is reliable for downstream use in data migration planning</threshold>
    <threshold min="0" max="99" verdict="FAIL">Documentation has issues and must be remediated or regenerated with corrected RE-002 prompt</threshold>
  </verdict_thresholds>

  <finding_severity_levels>
    <level name="Critical">Hallucinated file references (CVCAR00Y.cpy, COUSR00Y.cpy), wrong PIC clauses (S9(7)V99), fabricated field definitions, incorrect SQL type mappings that would cause data loss during migration</level>
    <level name="Major">Missing VSAM files in analysis, incomplete field documentation, missing data files, absent data lineage for key programs</level>
    <level name="Minor">Broken Mermaid syntax, formatting issues, unclear descriptions, missing FILLER documentation</level>
  </finding_severity_levels>
</scoring_rubric>

<output_specification>
  <report>
    <path>docs/reverse-engineering/validation/VL-002-data-model-report.md</path>
    <format>
# Validation Report: Data Model Documentation (RE-002)

## Verdict: [PASS|FAIL] — Score: [NN]/100

## Score Breakdown

| Category | Weight | Score | Weighted |
|----------|--------|-------|----------|
| Source Reference Accuracy | 35% | NN/100 | NN.N |
| Factual Accuracy | 25% | NN/100 | NN.N |
| Completeness | 20% | NN/100 | NN.N |
| Quantitative Accuracy | 10% | NN/100 | NN.N |
| Documentation Quality | 10% | NN/100 | NN.N |
| **Total** | **100%** | | **NN.N** |

## Critical Findings
[PIC clause errors, hallucinated copybooks, wrong SQL type mappings]

## Major Findings
[Missing VSAM files, incomplete field coverage, missing data lineage]

## Minor Findings
[Mermaid syntax issues, formatting problems]

## Hallucination Inventory

| # | Claim in Documentation | Reality (from source) | Source File:Line | Severity |
|---|----------------------|----------------------|-----------------|----------|
| 1 | ... | ... | ... | Critical |

## PIC Clause Verification

| Field | Documented PIC | Actual PIC | Source | Match |
|-------|---------------|------------|--------|-------|
| ACCT-CURR-BAL | ... | S9(10)V99 | CVACT01Y.cpy:7 | ... |

## SQL Type Mapping Verification

| Field | Documented PIC | Documented SQL Type | Correct SQL Type | Match |
|-------|---------------|--------------------|--------------------|-------|
| ... | ... | ... | ... | ... |

## Record Length Verification

| VSAM File | Documented Length | Calculated Length | Source | Match |
|-----------|-----------------|-------------------|--------|-------|
| ACCTDAT | ... | 300 | CVACT01Y.cpy:2 | ... |

## Completeness Gaps

| # | Expected Item | Status | Notes |
|---|--------------|--------|-------|
| 1 | ACCTDAT VSAM file | Present/Missing/Partial | ... |
| 2 | CARDDAT VSAM file | Present/Missing/Partial | ... |

## ER Diagram Assessment
[Verify entities, relationships, cardinalities, and field accuracy]

## Recommendations
[Specific, actionable recommendations for remediation]

## Remediation Manifest

| ID | Finding | Target File | Location | Current (Wrong) | Required (Correct) | Source Evidence | Remediation Action | RE Prompt |
|----|---------|-------------|----------|-----------------|--------------------|-----------------|--------------------|-----------|
| R-001 | [finding] | [doc path] | [section/line] | [wrong] | [correct] | [source:line] | [fix action] | [RE-NNN] |

### Remediation Instructions
For each row: (1) Read target file at location, (2) Verify wrong value present, (3) Replace with correct value per source evidence, (4) Re-validate

### Affected RE Prompts
[List RE prompts requiring re-execution with reasons]
    </format>
  </report>
  <work_directory>.work/reverse-engineering/validation/vl-002/</work_directory>
  <work_artifacts>
    <artifact>claims-inventory.yaml</artifact>
    <artifact>source-reference-results.yaml</artifact>
    <artifact>factual-accuracy-results.yaml</artifact>
    <artifact>completeness-results.yaml</artifact>
    <artifact>quality-results.yaml</artifact>
  </work_artifacts>
</output_specification>

<foundational_principles>
  <principle id="1">Every PIC clause in the documentation must be verified against the actual copybook source — S9(10)V99 is the correct monetary PIC, not S9(7)V99</principle>
  <principle id="2">Record lengths must be calculated by summing all field PICs including FILLER, and compared against copybook comments</principle>
  <principle id="3">COMP-3 byte calculations follow the formula (n+1)/2 rounded up, where n = total digits including decimals — verify every instance</principle>
  <principle id="4">SQL type mappings must be correct for the actual PIC clauses found in source, not for assumed PICs — S9(10)V99 maps to DECIMAL(12,2), not DECIMAL(9,2)</principle>
  <principle id="5">VSAM file documentation must include primary key, alternate indexes, access mode, and record copybook — all verified against SELECT statements</principle>
  <principle id="6">The ER diagram must reflect actual copybook record structures, not idealized entity designs</principle>
  <principle id="7">Data lineage claims must be verified by reading the actual COBOL program logic (READ, WRITE, REWRITE, COMPUTE, MOVE statements)</principle>
  <principle id="8">Field names must match source exactly, including original typos (e.g., ACCT-EXPIRAION-DATE, not ACCT-EXPIRATION-DATE)</principle>
</foundational_principles>

<critical_reminders>
  <reminder id="1">CVCAR00Y.cpy does NOT exist in the codebase. If the documentation references this file, it is a hallucination. The actual card copybook is CVCRD01Y.cpy. Read app/cpy/ directory listing to confirm.</reminder>
  <reminder id="2">COUSR00Y.cpy does NOT exist in the codebase. If the documentation references this file, it is a hallucination. The actual user security copybook is CSUSR01Y.cpy. Read app/cpy/ directory listing to confirm.</reminder>
  <reminder id="3">Monetary fields in CVACT01Y.cpy use PIC S9(10)V99 (lines 7-9, 13-14), NOT S9(7)V99 COMP-3. If S9(7)V99 appears in documentation, it is WRONG. Read the actual copybook to verify.</reminder>
  <reminder id="4">ACCTDAT record length is 300 bytes as documented in the CVACT01Y.cpy comment on line 2. Verify: 11 + 1 + 12 + 12 + 12 + 10 + 10 + 10 + 12 + 12 + 10 + 10 + 178 = 300.</reminder>
  <reminder id="5">CVCRD01Y.cpy is a communication area copybook (CC-WORK-AREAS), NOT a simple record layout. It contains REDEFINES clauses and 88-level conditions for AID key handling. Verify documentation accurately describes its structure.</reminder>
  <reminder id="6">CSUSR01Y.cpy has record structure: SEC-USR-ID X(08), SEC-USR-FNAME X(20), SEC-USR-LNAME X(20), SEC-USR-PWD X(08), SEC-USR-TYPE X(01), SEC-USR-FILLER X(23) = 80 bytes total. Verify against documentation.</reminder>
  <reminder id="7">The ACCT-EXPIRAION-DATE field has a typo in the original source (missing 'T' in EXPIRATION). If documentation corrects this to ACCT-EXPIRATION-DATE, flag it as a factual inaccuracy — the validator must preserve original source spelling.</reminder>
  <reminder id="8">Do not trust any claim about file organization, access mode, or key definitions without reading the actual SELECT statements in the COBOL programs that use those files.</reminder>
  <reminder id="9">Verify that COMP-3 is actually used where claimed. The CVACT01Y.cpy monetary fields use S9(10)V99 display format, NOT explicitly COMP-3. If the documentation claims COMP-3 without source evidence, flag it.</reminder>
  <reminder id="10">Cross-check data lineage claims by reading actual COBOL READ/WRITE/REWRITE/COMPUTE/MOVE statements in the cited programs. Do not trust lineage documentation at face value.</reminder>
</critical_reminders>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/validation/vl-002/</path>
    <purpose>Persist validation progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
```yaml
validation_phase: "load_documentation|source_reference_verification|factual_accuracy_check|completeness_assessment|quality_assessment|report_generation"
current_document: "document being validated"
documents_completed:
  - path: "docs/reverse-engineering/02-data-model/DATA-MODEL.md"
    status: "not_started|in_progress|complete"
  - path: "docs/reverse-engineering/02-data-model/DATA-DICTIONARY.md"
    status: "not_started|in_progress|complete"
  - path: "docs/reverse-engineering/02-data-model/DATA-LINEAGE.md"
    status: "not_started|in_progress|complete"
  - path: "docs/reverse-engineering/02-data-model/diagrams/er-diagram.md"
    status: "not_started|in_progress|complete"
vsam_files_verified:
  - name: "ACCTDAT"
    pic_clauses_checked: true
    record_length_verified: true
    key_definition_verified: true
  - name: "CARDDAT"
    pic_clauses_checked: false
    record_length_verified: false
    key_definition_verified: false
hallucinations_found:
  - id: "H1"
    claim: "description"
    reality: "what source shows"
    severity: "Critical|Major|Minor"
findings:
  critical: []
  major: []
  minor: []
scores:
  source_reference_accuracy: null
  factual_accuracy: null
  completeness: null
  quantitative_accuracy: null
  documentation_quality: null
artifacts_created:
  - path: "relative path"
    type: "claims-inventory|source-results|accuracy-results|completeness-results|quality-results"
    status: "complete|partial"
next_action: "Detailed description of next step with enough context to resume cold"
last_updated: "ISO timestamp"
```
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/validation/vl-002/progress.yaml</step>
    <step>2. Load completed phase artifacts from .work/reverse-engineering/validation/vl-002/</step>
    <step>3. Review hallucinations_found and findings accumulated so far</step>
    <step>4. Continue from next_action without re-validating completed items</step>
    <step>5. Update progress.yaml after each verification step or phase completion</step>
  </resumption_protocol>
</context_compaction_survival>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/validation/vl-002/progress.yaml if it exists
    2. If progress exists and validation_phase != "complete":
       - Load completed phase artifacts
       - Resume from next_action
    3. If no progress or starting fresh:
       - Verify RE-002 output exists by checking for docs/reverse-engineering/02-data-model/DATA-MODEL.md
       - If RE-002 output does not exist, STOP and report: "RE-002 output not found. Run RE-002 before VL-002."
       - Begin with Phase 1: Load Documentation
       - Create initial progress.yaml
    4. Execute all 6 validation phases in sequence:
       - Phase 1: Load all RE-002 documentation and inventory every verifiable claim
       - Phase 2: Verify every file:line source reference against actual source code
       - Phase 3: Check PIC clauses, SQL mappings, COMP-3 calculations, record lengths, field names
       - Phase 4: Assess completeness of VSAM file coverage, field documentation, data file coverage, lineage
       - Phase 5: Evaluate ER diagram accuracy, Mermaid syntax validity, documentation quality
       - Phase 6: Compile findings, calculate scores, determine verdict, write final report
    5. After completing each phase:
       - Update progress.yaml with detailed next_action
       - Write intermediate artifacts immediately
    6. ALWAYS read the actual source files — never rely on memory or assumptions about file contents
    7. Write final report to docs/reverse-engineering/validation/VL-002-data-model-report.md
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to validate the data model documentation produced by RE-002. The prompt will systematically cross-reference every claim in the data model documentation against the actual COBOL source code, copybooks, and data files. It verifies PIC clause accuracy, SQL type mapping correctness, COMP-3 byte calculations, record length calculations, VSAM file completeness, ER diagram accuracy, and data lineage claims. The validation actively searches for known hallucinations (CVCAR00Y.cpy, COUSR00Y.cpy, S9(7)V99) and flags any documentation that references non-existent files or incorrect field definitions.

## Expected Output

| File | Description |
|------|-------------|
| `docs/reverse-engineering/validation/VL-002-data-model-report.md` | Full validation report with scored verdict (PASS/FAIL), hallucination inventory, PIC clause verification table, SQL type mapping verification, record length verification, completeness gaps, ER diagram assessment, remediation manifest, and actionable remediation recommendations |

## Prerequisites

- RE-002 output must exist in `docs/reverse-engineering/02-data-model/` with at minimum:
  - `DATA-MODEL.md`
  - `DATA-DICTIONARY.md`
  - `DATA-LINEAGE.md`
  - `diagrams/er-diagram.md`

## Depends On

- None (Phase 1 — can run in parallel with VL-001)

## Blocks

- VL-003 (Context Model Validation — benefits from confirmed data model accuracy)
- VL-000 (Cross-Document Consistency — requires all individual validations complete)
