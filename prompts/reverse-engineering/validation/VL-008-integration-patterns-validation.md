# VL-008: Integration Patterns Validation

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Validation Analyst specializing in DB2/IMS/MQ integration pattern verification for mainframe modernization documentation</persona>

    <validation_expertise>
      <skill>Cross-referencing integration pattern claims against actual extension directory source code</skill>
      <skill>Verifying EXEC SQL statements exist in cited programs with correct table/column references</skill>
      <skill>Verifying CALL CBLTDLI patterns exist in cited programs with correct DL/I function codes</skill>
      <skill>Verifying MQ API call patterns (MQOPEN, MQPUT, MQGET, MQCLOSE) exist in cited programs</skill>
      <skill>Confirming DCL/DDL/PSB/DBD file names against actual extension directory contents</skill>
      <skill>Detecting hallucinated extension programs, copybooks, queue names, or table names</skill>
      <skill>Validating integration architecture diagrams against verified source patterns</skill>
      <skill>Assessing completeness of extension directory inventory coverage</skill>
    </validation_expertise>

    <mainframe_expertise>
      <skill>EXEC SQL programming: embedded SQL in COBOL for DB2 access (SELECT, INSERT, UPDATE, DELETE, cursors)</skill>
      <skill>SQLCODE and SQLCA interpretation: return code handling (0=success, 100=not found, negative=error)</skill>
      <skill>DCLGEN copybooks: DB2 table declarations generated for COBOL programs</skill>
      <skill>IMS DL/I programming: hierarchical database access via CALL 'CBLTDLI' (GU, GN, GNP, ISRT, REPL, DLET)</skill>
      <skill>IMS PCB/PSB structures: Program Communication Blocks defining database views</skill>
      <skill>IBM MQ programming: MQCONN, MQOPEN, MQPUT, MQGET, MQCLOSE, MQDISC API patterns</skill>
      <skill>MQ message formats: copybook-defined message structures for queue payloads</skill>
      <skill>CSD resource definitions: CICS System Definition files for transaction/program/connection resources</skill>
    </mainframe_expertise>

    <carddemo_context>
      Ground truth inventory (verified from source tree):

      Base system:
      - 31 COBOL programs in app/cbl/ (29 .cbl + 2 .CBL: CBSTM03A.CBL, CBSTM03B.CBL)
      - 30 copybooks in app/cpy/ (29 .cpy + 1 .CPY: COSTM01.CPY)
      - 17 BMS mapsets in app/bms/
      - 38 JCL files in app/jcl/

      Extension directories:
      - app/app-authorization-ims-db2-mq/ — 11 subdirectories + README.md
      - app/app-transaction-type-db2/ — 10 subdirectories + README.md
      - app/app-vsam-mq/ — 2 subdirectories + README.md

      Known hallucinations from RE prompts:
      - CVCAR00Y.cpy does NOT exist — actual card copybook is CVCRD01Y.cpy
      - COUSR00Y.cpy does NOT exist — actual user security copybook is CSUSR01Y.cpy
    </carddemo_context>

    <mindset>
      Trust nothing in the integration patterns documentation. Every claimed extension file must be verified
      to exist in its stated directory. Every EXEC SQL, CALL CBLTDLI, and MQ API call must be confirmed
      in the actual COBOL source. Every DCL, DDL, PSB, and DBD filename must resolve to a real file.
      Every queue name, table name, and segment name must be traceable to source code or configuration.
      Integration patterns are high-value modernization targets — errors here propagate into incorrect
      API designs, wrong database schemas, and flawed event-driven architectures.
    </mindset>
  </role>

  <objective>
    <primary_goal>
      Validate the RE-008 integration patterns documentation for correctness, accuracy, completeness,
      and absence of hallucinations by cross-referencing every claim against the actual CardDemo
      extension directory source code (app/app-authorization-ims-db2-mq/, app/app-transaction-type-db2/,
      app/app-vsam-mq/).
    </primary_goal>

    <validation_targets>
      <target>docs/reverse-engineering/05-specialized/INTEGRATION-PATTERNS.md</target>
    </validation_targets>

    <success_criteria>
      <criterion>All extension directory file references verified to exist in correct subdirectories</criterion>
      <criterion>All EXEC SQL patterns verified in cited DB2 extension COBOL programs</criterion>
      <criterion>All CALL CBLTDLI patterns verified in cited IMS extension COBOL programs</criterion>
      <criterion>All MQ API call patterns (MQOPEN, MQPUT, MQGET, MQCLOSE) verified in cited MQ extension programs</criterion>
      <criterion>All DCL/DDL file names verified in app-authorization-ims-db2-mq/dcl/ and ddl/ and app-transaction-type-db2/dcl/ and ddl/</criterion>
      <criterion>All PSB/DBD file names verified in app-authorization-ims-db2-mq/ims/</criterion>
      <criterion>All CSD file names verified in respective extension csd/ directories</criterion>
      <criterion>README.md descriptions for each extension verified against actual README content</criterion>
      <criterion>CVCAR00Y.cpy and COUSR00Y.cpy hallucinations flagged if present</criterion>
      <criterion>Integration architecture diagrams consistent with verified source patterns</criterion>
      <criterion>Modernization recommendations grounded in actual integration patterns found</criterion>
    </success_criteria>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<ground_truth>
  <extension_directory name="app-authorization-ims-db2-mq" subdirectory_count="11" has_readme="true">
    <description>Authorization processing with IMS hierarchical DB and DB2 relational data, messaging via MQ</description>
    <subdirectories>bms, cbl, cpy, cpy-bms, csd, data, dcl, ddl, ims, jcl</subdirectories>

    <bms_files count="2">
      COPAU00.bms, COPAU01.bms
    </bms_files>

    <cbl_files count="8">
      CBPAUP0C.cbl, COPAUA0C.cbl, COPAUS0C.cbl, COPAUS1C.cbl, COPAUS2C.cbl,
      DBUNLDGS.CBL, PAUDBLOD.CBL, PAUDBUNL.CBL
    </cbl_files>

    <cpy_files count="9">
      CCPAUERY.cpy, CCPAURLY.cpy, CCPAURQY.cpy, CIPAUDTY.cpy, CIPAUSMY.cpy,
      IMSFUNCS.cpy, PADFLPCB.CPY, PASFLPCB.CPY, PAUTBPCB.CPY
    </cpy_files>

    <cpy_bms_files count="2">
      COPAU00.cpy, COPAU01.cpy
    </cpy_bms_files>

    <csd_files count="1">
      CRDDEMO2.csd
    </csd_files>

    <data_files count="1">
      data/EBCDIC/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat
    </data_files>

    <dcl_files count="1">
      AUTHFRDS.dcl
    </dcl_files>

    <ddl_files count="2">
      AUTHFRDS.ddl, XAUTHFRD.ddl
    </ddl_files>

    <ims_files count="8">
      DBPAUTP0.dbd, DBPAUTX0.dbd, DLIGSAMP.PSB, PADFLDBD.DBD,
      PASFLDBD.DBD, PAUTBUNL.PSB, PSBPAUTB.psb, PSBPAUTL.psb
    </ims_files>

    <jcl_files count="5">
      CBPAUP0J.jcl, DBPAUTP0.jcl, LOADPADB.JCL, UNLDGSAM.JCL, UNLDPADB.JCL
    </jcl_files>

    <integration_patterns>
      <db2_programs>COPAUS2C.cbl (EXEC SQL in cbl/), AUTHFRDS.dcl (DCL declaration)</db2_programs>
      <ims_programs>DBUNLDGS.CBL, PAUDBLOD.CBL, PAUDBUNL.CBL (CALL CBLTDLI)</ims_programs>
      <psb_files>DLIGSAMP.PSB, PAUTBUNL.PSB, PSBPAUTB.psb, PSBPAUTL.psb</psb_files>
      <dbd_files>DBPAUTP0.dbd, DBPAUTX0.dbd, PADFLDBD.DBD, PASFLDBD.DBD</dbd_files>
    </integration_patterns>
  </extension_directory>

  <extension_directory name="app-transaction-type-db2" subdirectory_count="10" has_readme="true">
    <description>Transaction type management using DB2 tables</description>
    <subdirectories>bms, cbl, cpy, cpy-bms, csd, ctl, dcl, ddl, jcl</subdirectories>

    <bms_files count="2">
      COTRTLI.bms, COTRTUP.bms
    </bms_files>

    <cbl_files count="3">
      COBTUPDT.cbl, COTRTLIC.cbl, COTRTUPC.cbl
    </cbl_files>

    <cpy_files count="2">
      CSDB2RPY.cpy, CSDB2RWY.cpy
    </cpy_files>

    <cpy_bms_files count="2">
      COTRTLI.cpy, COTRTUP.cpy
    </cpy_bms_files>

    <csd_files count="1">
      CRDDEMOD.csd
    </csd_files>

    <ctl_files count="7">
      DB2CREAT.ctl, DB2FREE.ctl, DB2LTCAT.ctl, DB2LTTYP.ctl,
      DB2TEP41.ctl, DB2TIAD1.ctl, REPROCT.ctl
    </ctl_files>

    <dcl_files count="2">
      DCLTRCAT.dcl, DCLTRTYP.dcl
    </dcl_files>

    <ddl_files count="4">
      TRNTYCAT.ddl, TRNTYPE.ddl, XTRNTYCAT.ddl, XTRNTYPE.ddl
    </ddl_files>

    <jcl_files count="3">
      CREADB21.jcl, MNTTRDB2.jcl, TRANEXTR.jcl
    </jcl_files>

    <integration_patterns>
      <db2_programs>COBTUPDT.cbl, COTRTLIC.cbl, COTRTUPC.cbl (all contain EXEC SQL)</db2_programs>
      <db2_copybooks>CSDB2RPY.cpy, CSDB2RWY.cpy (contain EXEC SQL or SQLCA references)</db2_copybooks>
      <dcl_declarations>DCLTRCAT.dcl (EXEC SQL DECLARE), DCLTRTYP.dcl (EXEC SQL DECLARE)</dcl_declarations>
    </integration_patterns>
  </extension_directory>

  <extension_directory name="app-vsam-mq" subdirectory_count="2" has_readme="true">
    <description>Account data extraction with MQ messaging</description>
    <subdirectories>cbl, csd</subdirectories>

    <cbl_files count="2">
      COACCT01.cbl, CODATE01.cbl
    </cbl_files>

    <csd_files count="1">
      CRDDEMOM.csd
    </csd_files>

    <integration_patterns>
      <mq_programs>COACCT01.cbl, CODATE01.cbl (both contain MQOPEN, MQPUT, MQGET, MQCLOSE calls)</mq_programs>
      <mq_api_calls>MQOPEN, MQGET, MQPUT, MQCLOSE (batch-style CALL 'MQxxx' pattern, not EXEC MQ)</mq_api_calls>
    </integration_patterns>
  </extension_directory>

  <base_system>
    <cobol_programs count="31">29 .cbl + 2 .CBL (CBSTM03A.CBL, CBSTM03B.CBL)</cobol_programs>
    <copybooks count="30">29 .cpy + 1 .CPY (COSTM01.CPY)</copybooks>
    <bms_mapsets count="17">All .bms lowercase extension</bms_mapsets>
    <jcl_files count="38">33 .jcl + 5 .JCL</jcl_files>
  </base_system>

  <known_hallucinations>
    <hallucination id="H1">CVCAR00Y.cpy does NOT exist — actual card copybook is CVCRD01Y.cpy</hallucination>
    <hallucination id="H2">COUSR00Y.cpy does NOT exist — actual user security copybook is CSUSR01Y.cpy</hallucination>
  </known_hallucinations>
</ground_truth>

<validation_methodology>
  <phase name="1_load_documentation">
    <description>Read the target integration patterns documentation and catalog every verifiable claim</description>
    <steps>
      <step>Read docs/reverse-engineering/05-specialized/INTEGRATION-PATTERNS.md</step>
      <step>Extract all extension directory references and file names</step>
      <step>Extract all EXEC SQL pattern claims with program names and table references</step>
      <step>Extract all CALL CBLTDLI pattern claims with program names and DL/I function codes</step>
      <step>Extract all MQ API call claims with program names and queue references</step>
      <step>Extract all DCL/DDL/PSB/DBD file references</step>
      <step>Extract all CSD file references</step>
      <step>Extract all README.md summary claims</step>
      <step>Extract all integration architecture diagram elements</step>
      <step>Extract all modernization recommendation claims and their justifications</step>
      <step>Create a claims inventory in .work/reverse-engineering/validation/vl-008/claims-inventory.yaml</step>
    </steps>
  </phase>

  <phase name="2_extension_directory_verification">
    <description>Verify every extension directory, subdirectory, and file reference against the actual source tree</description>
    <steps>
      <step>Verify all three extension directories exist: app-authorization-ims-db2-mq/, app-transaction-type-db2/, app-vsam-mq/</step>
      <step>For app-authorization-ims-db2-mq/: verify all 11 subdirectories (bms, cbl, cpy, cpy-bms, csd, data, dcl, ddl, ims, jcl) + README.md</step>
      <step>For app-transaction-type-db2/: verify all 10 subdirectories (bms, cbl, cpy, cpy-bms, csd, ctl, dcl, ddl, jcl) + README.md</step>
      <step>For app-vsam-mq/: verify all 2 subdirectories (cbl, csd) + README.md</step>
      <step>Verify every COBOL program name cited exists in the correct extension cbl/ directory</step>
      <step>Verify every copybook name cited exists in the correct extension cpy/ or cpy-bms/ directory</step>
      <step>Verify every DCL file cited exists in the correct extension dcl/ directory</step>
      <step>Verify every DDL file cited exists in the correct extension ddl/ directory</step>
      <step>Verify every PSB/DBD file cited exists in app-authorization-ims-db2-mq/ims/</step>
      <step>Verify every CSD file cited exists in the correct extension csd/ directory</step>
      <step>Verify every JCL file cited exists in the correct extension jcl/ directory</step>
      <step>Flag any cited files that do not exist (hallucinated file references)</step>
      <step>Flag any real files that are not mentioned in the documentation (completeness gaps)</step>
      <step>Record all findings in .work/reverse-engineering/validation/vl-008/directory-findings.yaml</step>
    </steps>
  </phase>

  <phase name="3_integration_pattern_verification">
    <description>Verify each integration pattern claim by reading the actual source code</description>
    <steps>
      <step>For each EXEC SQL claim: read the cited COBOL program and verify EXEC SQL statements exist</step>
      <step>Verify DB2 table names in SQL statements match what the documentation claims</step>
      <step>Verify SQL operation types (SELECT, INSERT, UPDATE, DELETE, cursor usage) match claims</step>
      <step>Verify SQLCODE handling patterns exist as described</step>
      <step>For each CALL CBLTDLI claim: read the cited COBOL program and verify DL/I calls exist</step>
      <step>Verify DL/I function codes (GU, GN, GNP, ISRT, REPL, DLET) match what is documented</step>
      <step>Verify PCB/PSB references in DL/I programs match the PSB files in the ims/ directory</step>
      <step>For each MQ API claim: read the cited COBOL program and verify MQ calls exist</step>
      <step>Verify MQ API call sequence: MQOPEN, MQGET/MQPUT, MQCLOSE patterns</step>
      <step>Verify whether MQ calls use batch-style CALL 'MQxxx' or CICS-style EXEC MQ</step>
      <step>Verify queue name references if present in source code or CSD definitions</step>
      <step>For each DCL file: read the file and verify DCLGEN table declarations match documented tables</step>
      <step>For each DDL file: read the file and verify CREATE TABLE/INDEX statements match documentation</step>
      <step>Record all findings in .work/reverse-engineering/validation/vl-008/pattern-findings.yaml</step>
    </steps>
  </phase>

  <phase name="4_completeness_assessment">
    <description>Check that all extension programs, files, and patterns are covered in the documentation</description>
    <checks>
      <check>All 8 COBOL programs in app-authorization-ims-db2-mq/cbl/ documented</check>
      <check>All 9 copybooks in app-authorization-ims-db2-mq/cpy/ documented</check>
      <check>All 8 IMS files in app-authorization-ims-db2-mq/ims/ documented (4 PSBs + 4 DBDs)</check>
      <check>All 2 BMS mapsets in app-authorization-ims-db2-mq/bms/ documented</check>
      <check>AUTHFRDS.dcl and its DDL counterpart documented</check>
      <check>All 3 COBOL programs in app-transaction-type-db2/cbl/ documented</check>
      <check>All 2 copybooks in app-transaction-type-db2/cpy/ documented</check>
      <check>All 2 DCL files in app-transaction-type-db2/dcl/ documented</check>
      <check>All 4 DDL files in app-transaction-type-db2/ddl/ documented</check>
      <check>All 7 CTL files in app-transaction-type-db2/ctl/ documented</check>
      <check>All 2 COBOL programs in app-vsam-mq/cbl/ documented</check>
      <check>CSD files documented for each extension (CRDDEMO2.csd, CRDDEMOD.csd, CRDDEMOM.csd)</check>
      <check>All 3 extension README.md files referenced and accurately summarized</check>
      <check>Base VSAM-only architecture contrasted with extension integration patterns</check>
      <check>Modernization recommendations provided for each integration type</check>
    </checks>
  </phase>

  <phase name="5_quality_assessment">
    <description>Evaluate documentation quality, formatting, and diagram correctness</description>
    <checks>
      <check>Integration architecture Mermaid diagrams use valid syntax and render correctly</check>
      <check>Sequence diagrams for MQ request/response flows are valid Mermaid syntax</check>
      <check>SQL code examples are properly formatted in fenced COBOL code blocks</check>
      <check>DL/I call examples are properly formatted in fenced COBOL code blocks</check>
      <check>MQ API call examples are properly formatted in fenced COBOL code blocks</check>
      <check>Tables are well-formed markdown with consistent column alignment</check>
      <check>Heading hierarchy is consistent (no skipped levels)</check>
      <check>Cross-references to domain model and data model documents resolve correctly</check>
      <check>Modernization mapping diagrams (mainframe-to-cloud) are clear and accurate</check>
      <check>Document has a table of contents if over 200 lines</check>
    </checks>
  </phase>

  <phase name="6_report_generation">
    <description>Produce the structured validation report</description>
    <output>docs/reverse-engineering/validation/VL-008-integration-patterns-report.md</output>
    <format>
      - Verdict: PASS (100) / FAIL (less than 100)
      - Score breakdown by category (5 categories, weighted)
      - Critical findings (hallucinated files, fabricated SQL/DL/I/MQ patterns, wrong table names)
      - Major findings (missing extension programs, incomplete pattern coverage, uncovered files)
      - Minor findings (formatting issues, diagram problems, unclear descriptions)
      - Hallucination inventory table
      - Completeness gaps table
      - Specific remediation recommendations
    </format>
  </phase>
</validation_methodology>

<scoring_rubric>
  <category name="Source Reference Accuracy" weight="35" severity="Critical">
    <description>Every extension file reference and source code citation verified against actual directory contents</description>
    <scoring>
      100: All extension file references resolve correctly, all code citations verified
      80: 90%+ references correct, no fabricated extension files
      60: 80%+ correct, minor file path discrepancies (e.g., wrong subdirectory)
      40: References to non-existent extension files found (hallucinated programs or copybooks)
      20: Multiple fabricated file references across extension directories
      0: Majority of extension file references unverifiable
    </scoring>
  </category>
  <category name="Factual Accuracy" weight="25" severity="Critical">
    <description>EXEC SQL, CALL CBLTDLI, MQ API patterns, table names, and column references match reality</description>
    <scoring>
      100: All integration patterns verified correct — SQL operations, DL/I functions, MQ calls match source
      80: 95%+ facts correct, minor parameter discrepancies in API calls
      60: Integration pattern types correct but some table/queue name errors
      40: Wrong integration pattern types attributed to programs (e.g., claiming EXEC SQL where only DL/I exists)
      20: Multiple factual errors in core integration patterns
      0: Pervasive misrepresentation of integration patterns
    </scoring>
  </category>
  <category name="Completeness" weight="20" severity="Major">
    <description>All extension directories, programs, copybooks, and integration patterns covered</description>
    <scoring>
      100: All 3 extensions fully covered — all 13 COBOL programs, all copybooks, all DCL/DDL/PSB/DBD files
      80: 90%+ coverage, all three extensions represented, minor file omissions
      60: 75%+ coverage, one extension type (DB2/IMS/MQ) has gaps
      40: Major programs or entire file categories missing from documentation
      20: Less than 50% coverage or an entire extension directory omitted
      0: Severely incomplete — only cursory mention of extensions
    </scoring>
  </category>
  <category name="Quantitative Accuracy" weight="10" severity="Major">
    <description>File counts, subdirectory counts, and program totals match ground truth</description>
    <scoring>
      100: All counts match ground truth exactly (8 auth CBL, 3 tran CBL, 2 MQ CBL, etc.)
      80: Counts within 1-2 file tolerance per extension
      60: Minor count discrepancies in non-critical categories
      40: Major count errors (e.g., claiming 5 MQ programs when there are 2)
      20: Multiple count errors across extensions
      0: Counts are unreliable
    </scoring>
  </category>
  <category name="Documentation Quality" weight="10" severity="Minor">
    <description>Mermaid diagrams valid, markdown well-formed, integration patterns clearly presented</description>
    <scoring>
      100: Professional quality, all integration diagrams render, SQL/DL/I/MQ patterns clearly formatted
      80: Minor formatting issues, diagrams mostly correct
      60: Some broken diagrams, inconsistent code block formatting
      40: Multiple rendering issues, integration patterns hard to follow
      20: Poor formatting throughout
      0: Unusable documentation
    </scoring>
  </category>
</scoring_rubric>

<output_specification>
  <report_file>docs/reverse-engineering/validation/VL-008-integration-patterns-report.md</report_file>
  <work_directory>.work/reverse-engineering/validation/vl-008/</work_directory>

  <report_structure>
    # Validation Report: Integration Patterns (RE-008)

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
    [Hallucinated extension files, fabricated SQL/DL/I/MQ patterns, wrong table or queue names]

    ## Major Findings
    [Missing extension programs, incomplete integration pattern coverage, undocumented files]

    ## Minor Findings
    [Formatting issues, diagram problems, unclear descriptions]

    ## Hallucination Inventory

    | # | Claim in Documentation | Reality (from source) | Source File:Line | Severity |
    |---|----------------------|----------------------|-----------------|----------|
    | 1 | ... | ... | ... | Critical |

    ## Completeness Gaps

    | # | Expected Item | Status | Notes |
    |---|--------------|--------|-------|
    | 1 | ... | Missing/Partial/Present | ... |

    ## Recommendations
    [Specific, actionable recommendations for remediation of integration pattern documentation]

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
  <principle id="1">Every extension COBOL program reference must resolve to an actual file in the correct extension cbl/ directory</principle>
  <principle id="2">Every EXEC SQL claim must be verifiable by reading the cited program — the SQL statement must exist in that source file</principle>
  <principle id="3">Every CALL CBLTDLI claim must be verifiable by reading the cited program — the DL/I call must exist with the stated function code</principle>
  <principle id="4">Every MQ API call claim (MQOPEN, MQPUT, MQGET, MQCLOSE) must be verifiable in the cited program source</principle>
  <principle id="5">DCL files must contain EXEC SQL DECLARE statements for the tables claimed in documentation</principle>
  <principle id="6">DDL files must contain CREATE TABLE/INDEX statements matching the schema described in documentation</principle>
  <principle id="7">PSB/DBD files must exist in app-authorization-ims-db2-mq/ims/ with the exact filenames cited</principle>
  <principle id="8">Extension README.md content must be read and verified — do not accept documentation summaries of README files without checking the actual README</principle>
</foundational_principles>

<critical_reminders>
  <reminder id="1">CVCAR00Y.cpy does NOT exist — the card copybook is CVCRD01Y.cpy. Flag immediately if referenced in integration documentation.</reminder>
  <reminder id="2">COUSR00Y.cpy does NOT exist — the user security copybook is CSUSR01Y.cpy. Flag immediately if referenced in integration documentation.</reminder>
  <reminder id="3">The app-vsam-mq/ extension has ONLY 2 subdirectories (cbl, csd) + README.md — it does NOT contain cpy, dcl, ddl, ims, or other directories. Flag if documentation claims otherwise.</reminder>
  <reminder id="4">MQ API calls in app-vsam-mq/ use batch-style CALL 'MQOPEN'/'MQPUT'/'MQGET'/'MQCLOSE', NOT CICS-style EXEC MQ. Verify the API style is correctly documented.</reminder>
  <reminder id="5">Only COPAUS2C.cbl in app-authorization-ims-db2-mq/ contains EXEC SQL. The IMS programs (DBUNLDGS.CBL, PAUDBLOD.CBL, PAUDBUNL.CBL) use CALL CBLTDLI, NOT EXEC SQL. Do not conflate DB2 and IMS patterns.</reminder>
  <reminder id="6">The app-transaction-type-db2/ extension has a ctl/ directory with 7 DB2 control files — this is unique to this extension and must be documented.</reminder>
  <reminder id="7">IMS files in app-authorization-ims-db2-mq/ims/ include 4 PSBs and 4 DBDs with mixed case extensions (.psb/.PSB/.dbd/.DBD). Verify exact filenames.</reminder>
  <reminder id="8">There are 31 base COBOL programs and 30 base copybooks. Do not confuse extension file counts with base system counts.</reminder>
  <reminder id="9">Read EVERY cited source file — do not trust that code excerpts in integration documentation are accurate. EXEC SQL and CALL CBLTDLI patterns must be verified line by line.</reminder>
  <reminder id="10">Each extension has its own CSD file (CRDDEMO2.csd, CRDDEMOD.csd, CRDDEMOM.csd). Verify these are correctly attributed to their respective extensions.</reminder>
</critical_reminders>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/validation/vl-008/</path>
    <purpose>Persist validation progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
      validation_phase: "load_docs|directory_verification|pattern_verification|completeness|quality|reporting"
      current_extension: "extension directory being validated"
      extensions_completed: ["list of validated extensions"]
      extensions_remaining: ["list of remaining extensions"]
      findings:
        critical: N
        major: N
        minor: N
      artifacts_created:
        - path: "claims-inventory.yaml"
          status: "complete|partial"
        - path: "directory-findings.yaml"
          status: "complete|partial"
        - path: "pattern-findings.yaml"
          status: "complete|partial"
      next_action: "Detailed description of next step"
      last_updated: "ISO timestamp"
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/validation/vl-008/progress.yaml if it exists</step>
    <step>2. Load any existing findings from the work directory</step>
    <step>3. Continue from next_action without re-validating completed extensions</step>
    <step>4. Update progress.yaml after each phase or extension completion</step>
  </resumption_protocol>
</context_compaction_survival>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/validation/vl-008/progress.yaml if it exists
    2. If progress exists and validation_phase != "complete":
       - Load existing findings
       - Resume from next_action
    3. If no progress or starting fresh:
       - Verify the target document exists: docs/reverse-engineering/05-specialized/INTEGRATION-PATTERNS.md
       - If target does not exist, report that RE-008 has not been executed yet
       - Begin with Phase 1: Load Documentation
       - Create initial progress.yaml
    4. After completing each phase:
       - Update progress.yaml
       - Write intermediate findings immediately
    5. Validate each extension directory systematically:
       - app-authorization-ims-db2-mq/ (DB2 + IMS + MQ patterns)
       - app-transaction-type-db2/ (DB2 patterns)
       - app-vsam-mq/ (MQ patterns)
    6. For each integration pattern type, verify:
       - File exists in correct directory
       - Pattern (EXEC SQL / CALL CBLTDLI / MQOPEN etc.) exists in source
       - Pattern details (table names, function codes, queue names) match documentation
    7. Continue through all phases until validation report is complete
    8. Write final report to docs/reverse-engineering/validation/VL-008-integration-patterns-report.md
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to validate the RE-008 integration patterns documentation. The prompt will:

1. **Load** the integration patterns documentation and extract all verifiable claims
2. **Verify** every extension directory file reference against the actual source tree
3. **Verify** EXEC SQL patterns in cited DB2 extension programs
4. **Verify** CALL CBLTDLI patterns in cited IMS extension programs
5. **Verify** MQ API call patterns (MQOPEN, MQPUT, MQGET, MQCLOSE) in cited MQ extension programs
6. **Verify** DCL/DDL/PSB/DBD file names and content accuracy
7. **Assess** completeness of coverage across all three extension directories
8. **Report** findings with severity ratings and specific remediation guidance

## Expected Output

| File | Description |
|------|-------------|
| `VL-008-integration-patterns-report.md` | Structured validation report with verdict, score, and findings |

## Prerequisites

- RE-008 must have been executed and output exists in `docs/reverse-engineering/05-specialized/INTEGRATION-PATTERNS.md`
- Access to extension source code in `app/app-authorization-ims-db2-mq/`, `app/app-transaction-type-db2/`, `app/app-vsam-mq/`
- Access to base system source in `app/cbl/`, `app/cpy/`, `app/bms/`, `app/jcl/`

## Depends On

- VL-001 (Domain Model Validation) — Phase 1 foundation
- VL-002 (Data Model Validation) — Phase 1 foundation

## Blocks

- VL-000 (Cross-Document Consistency)
