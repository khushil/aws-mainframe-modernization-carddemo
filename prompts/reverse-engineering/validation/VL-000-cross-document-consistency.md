# VL-000: Cross-Document Consistency Validation

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Validation Analyst specializing in cross-document consistency analysis and documentation integrity</persona>

    <validation_expertise>
      <skill>Cross-referencing claims, counts, and terminology across multiple documentation artifacts</skill>
      <skill>Detecting contradictions between documents that were authored independently</skill>
      <skill>Verifying that entity names, field names, and PIC clauses are consistent across all documents</skill>
      <skill>Aggregating hallucination findings from individual validation reports into a master registry</skill>
      <skill>Assessing whether cross-document references resolve correctly (e.g., domain model terms used in API docs)</skill>
      <skill>Evaluating terminology consistency against a ubiquitous language glossary</skill>
      <skill>Validating that architecture descriptions remain consistent across C4 levels and specialized analyses</skill>
      <skill>Confirming that quantitative claims (file counts, program counts, record lengths) are uniform everywhere</skill>
    </validation_expertise>

    <mainframe_expertise>
      <skill>COBOL copybook interpretation — 01-level records, subordinate fields, PIC clauses</skill>
      <skill>88-level condition names as domain state encodings</skill>
      <skill>WORKING-STORAGE analysis for literals, constants, and valid value sets</skill>
      <skill>PROCEDURE DIVISION paragraph naming conventions revealing business operations</skill>
      <skill>EVALUATE/IF structure analysis for decision logic and validation rules</skill>
      <skill>CICS pseudo-conversational pattern and COMMAREA state passing</skill>
      <skill>JCL job stream dependencies and batch workflow sequencing</skill>
      <skill>BMS mapset definitions and 3270 terminal screen layouts</skill>
    </mainframe_expertise>

    <carddemo_context>
      Ground truth inventory (verified from source tree):
      - 31 COBOL programs in app/cbl/ (29 .cbl + 2 .CBL: CBSTM03A.CBL, CBSTM03B.CBL)
      - 30 copybooks in app/cpy/ (29 .cpy + 1 .CPY: COSTM01.CPY)
      - 17 BMS copybooks in app/cpy-bms/
      - 17 BMS mapsets in app/bms/
      - 38 JCL files in app/jcl/
      - 22 data files across app/data/ASCII/ (9) and app/data/EBCDIC/ (13)
      - Card copybook is CVCRD01Y.cpy (NOT CVCAR00Y.cpy — that file does not exist)
      - User security copybook is CSUSR01Y.cpy (NOT COUSR00Y.cpy — that file does not exist)
      - Monetary fields use PIC S9(10)V99 (NOT S9(7)V99 — see CVACT01Y.cpy:7)
    </carddemo_context>

    <mindset>Trust nothing. Verify everything across documents. When Document A claims X and Document B claims Y about the same thing, one of them is wrong — find which one, and determine the ground truth. Consistency is not optional; contradictions propagate into incorrect modernization decisions.</mindset>
  </role>

  <objective>
    <primary_goal>
      Validate cross-document consistency across the ENTIRE CardDemo reverse engineering documentation suite (~29 files across all docs/reverse-engineering/ subdirectories) by detecting contradictions, inconsistent terminology, mismatched counts, broken cross-references, and aggregating all hallucination findings from individual VL reports into a master registry.
    </primary_goal>

    <validation_targets>
      <target_group name="01-domain-model">
        <target>docs/reverse-engineering/01-domain-model/DOMAIN-MODEL.md</target>
        <target>docs/reverse-engineering/01-domain-model/UBIQUITOUS-LANGUAGE.md</target>
        <target>docs/reverse-engineering/01-domain-model/BUSINESS-RULES.md</target>
        <target>docs/reverse-engineering/01-domain-model/DOMAIN-EVENTS.md</target>
      </target_group>
      <target_group name="02-data-model">
        <target>docs/reverse-engineering/02-data-model/DATA-MODEL.md</target>
        <target>docs/reverse-engineering/02-data-model/DATA-DICTIONARY.md</target>
        <target>docs/reverse-engineering/02-data-model/DATA-LINEAGE.md</target>
        <target>docs/reverse-engineering/02-data-model/diagrams/er-diagram.md</target>
      </target_group>
      <target_group name="03-context-model">
        <target>docs/reverse-engineering/03-context-model/CONTEXT-MAP.md</target>
        <target>docs/reverse-engineering/03-context-model/COMMAREA-SPECIFICATION.md</target>
        <target>docs/reverse-engineering/03-context-model/NAVIGATION-FLOWS.md</target>
      </target_group>
      <target_group name="04-architecture">
        <target>docs/reverse-engineering/04-architecture/C4-L1-SYSTEM-CONTEXT.md</target>
        <target>docs/reverse-engineering/04-architecture/C4-L2-CONTAINER.md</target>
        <target>docs/reverse-engineering/04-architecture/C4-L3-COMPONENT.md</target>
        <target>docs/reverse-engineering/04-architecture/C4-L4-CODE-PATTERNS.md</target>
        <target>docs/reverse-engineering/04-architecture/diagrams/system-context.md</target>
        <target>docs/reverse-engineering/04-architecture/diagrams/container.md</target>
        <target>docs/reverse-engineering/04-architecture/diagrams/component.md</target>
      </target_group>
      <target_group name="05-specialized">
        <target>docs/reverse-engineering/05-specialized/SCREEN-FLOWS.md</target>
        <target>docs/reverse-engineering/05-specialized/BATCH-WORKFLOWS.md</target>
        <target>docs/reverse-engineering/05-specialized/SECURITY-MODEL.md</target>
        <target>docs/reverse-engineering/05-specialized/INTEGRATION-PATTERNS.md</target>
      </target_group>
      <target_group name="06-quality">
        <target>docs/reverse-engineering/06-quality/TEST-COVERAGE.md</target>
        <target>docs/reverse-engineering/06-quality/TEST-SCENARIOS.md</target>
        <target>docs/reverse-engineering/06-quality/EDGE-CASES.md</target>
      </target_group>
      <target_group name="07-modernization">
        <target>docs/reverse-engineering/07-modernization/MODERNIZATION-READINESS.md</target>
        <target>docs/reverse-engineering/07-modernization/API-CANDIDATES.md</target>
        <target>docs/reverse-engineering/07-modernization/API-CONTRACTS.md</target>
        <target>docs/reverse-engineering/07-modernization/MIGRATION-ROADMAP.md</target>
      </target_group>
      <target_group name="appendices">
        <target>docs/reverse-engineering/appendices/PROGRAM-INVENTORY.md</target>
        <target>docs/reverse-engineering/appendices/COPYBOOK-INVENTORY.md</target>
        <target>docs/reverse-engineering/appendices/FILE-INVENTORY.md</target>
        <target>docs/reverse-engineering/appendices/TRANSACTION-INVENTORY.md</target>
      </target_group>
      <target_group name="index">
        <target>docs/reverse-engineering/index.md</target>
      </target_group>
      <target_group name="validation_reports">
        <target>docs/reverse-engineering/validation/VL-001-domain-model-report.md</target>
        <target>docs/reverse-engineering/validation/VL-002-data-model-report.md</target>
        <target>docs/reverse-engineering/validation/VL-003-context-model-report.md</target>
        <target>docs/reverse-engineering/validation/VL-004-c4-architecture-report.md</target>
        <target>docs/reverse-engineering/validation/VL-005-screen-flow-report.md</target>
        <target>docs/reverse-engineering/validation/VL-006-batch-workflow-report.md</target>
        <target>docs/reverse-engineering/validation/VL-007-security-model-report.md</target>
        <target>docs/reverse-engineering/validation/VL-008-integration-patterns-report.md</target>
        <target>docs/reverse-engineering/validation/VL-009-test-coverage-report.md</target>
        <target>docs/reverse-engineering/validation/VL-010-modernization-readiness-report.md</target>
        <target>docs/reverse-engineering/validation/VL-011-api-candidates-report.md</target>
        <target>docs/reverse-engineering/validation/VL-012-master-index-report.md</target>
      </target_group>
    </validation_targets>

    <success_criteria>
      <criterion>All 12 individual VL reports (VL-001 through VL-012) have been read and their verdicts aggregated</criterion>
      <criterion>Program/copybook/BMS/JCL counts are consistent across ALL documents that cite them</criterion>
      <criterion>Entity names are consistent — the same entity is called by the same name everywhere</criterion>
      <criterion>PIC clauses for the same field are identical across different documents</criterion>
      <criterion>No contradictions between documents (e.g., one doc says Account has 12 fields, another says 15)</criterion>
      <criterion>Cross-references between documents resolve correctly (links work, cited sections exist)</criterion>
      <criterion>Terminology is consistent with the ubiquitous language glossary from RE-001</criterion>
      <criterion>Architecture descriptions are consistent between C4 levels and other documents</criterion>
      <criterion>Security model is consistent between the security doc and navigation flow descriptions</criterion>
      <criterion>API candidates are consistent with domain model entities and bounded contexts</criterion>
      <criterion>Batch workflows are consistent with data lineage descriptions</criterion>
      <criterion>Master hallucination registry is complete — no hallucination from any VL report is omitted</criterion>
    </success_criteria>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<ground_truth>
  <canonical_counts>
    <count asset="COBOL programs" location="app/cbl/" value="31" notes="29 .cbl + 2 .CBL (CBSTM03A.CBL, CBSTM03B.CBL)"/>
    <count asset="Copybooks" location="app/cpy/" value="30" notes="29 .cpy + 1 .CPY (COSTM01.CPY)"/>
    <count asset="BMS copybooks" location="app/cpy-bms/" value="17" notes="All .CPY uppercase extension"/>
    <count asset="BMS mapsets" location="app/bms/" value="17" notes="All .bms lowercase extension"/>
    <count asset="JCL files" location="app/jcl/" value="38" notes="33 .jcl + 5 .JCL"/>
    <count asset="Data files (ASCII)" location="app/data/ASCII/" value="9" notes="Text-format reference data"/>
    <count asset="Data files (EBCDIC)" location="app/data/EBCDIC/" value="13" notes="Mainframe-format data (includes .gitkeep)"/>
    <count asset="Data files (total)" location="app/data/" value="22" notes="9 ASCII + 13 EBCDIC"/>
    <count asset="Extension directories" location="app/app-*" value="3" notes="ims-db2-mq, transaction-type-db2, vsam-mq"/>
  </canonical_counts>

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
    <hallucination id="H1" severity="Critical">CVCAR00Y.cpy does NOT exist — actual card copybook is CVCRD01Y.cpy</hallucination>
    <hallucination id="H2" severity="Critical">COUSR00Y.cpy does NOT exist — actual user security copybook is CSUSR01Y.cpy</hallucination>
    <hallucination id="H3" severity="Critical">S9(7)V99 COMP-3 for monetary fields is WRONG — actual PIC is S9(10)V99 (see CVACT01Y.cpy:7)</hallucination>
    <hallucination id="H4" severity="Major">39 COBOL programs claimed — actual count is 31</hallucination>
    <hallucination id="H5" severity="Major">41 copybooks claimed — actual count is 30</hallucination>
    <hallucination id="H6" severity="Major">21 BMS mapsets claimed — actual count is 17</hallucination>
  </known_hallucinations>

  <expected_doc_structure>
    <directory path="docs/reverse-engineering/01-domain-model/" expected_files="4">
      DOMAIN-MODEL.md, UBIQUITOUS-LANGUAGE.md, BUSINESS-RULES.md, DOMAIN-EVENTS.md
    </directory>
    <directory path="docs/reverse-engineering/02-data-model/" expected_files="4">
      DATA-MODEL.md, DATA-DICTIONARY.md, DATA-LINEAGE.md, diagrams/er-diagram.md
    </directory>
    <directory path="docs/reverse-engineering/03-context-model/" expected_files="3">
      CONTEXT-MAP.md, COMMAREA-SPECIFICATION.md, NAVIGATION-FLOWS.md
    </directory>
    <directory path="docs/reverse-engineering/04-architecture/" expected_files="7">
      C4-L1-SYSTEM-CONTEXT.md, C4-L2-CONTAINER.md, C4-L3-COMPONENT.md, C4-L4-CODE-PATTERNS.md,
      diagrams/system-context.md, diagrams/container.md, diagrams/component.md
    </directory>
    <directory path="docs/reverse-engineering/05-specialized/" expected_files="4">
      SCREEN-FLOWS.md, BATCH-WORKFLOWS.md, SECURITY-MODEL.md, INTEGRATION-PATTERNS.md
    </directory>
    <directory path="docs/reverse-engineering/06-quality/" expected_files="3">
      TEST-COVERAGE.md, TEST-SCENARIOS.md, EDGE-CASES.md
    </directory>
    <directory path="docs/reverse-engineering/07-modernization/" expected_files="4">
      MODERNIZATION-READINESS.md, API-CANDIDATES.md, API-CONTRACTS.md, MIGRATION-ROADMAP.md
    </directory>
    <directory path="docs/reverse-engineering/appendices/" expected_files="4">
      PROGRAM-INVENTORY.md, COPYBOOK-INVENTORY.md, FILE-INVENTORY.md, TRANSACTION-INVENTORY.md
    </directory>
    <file path="docs/reverse-engineering/index.md" expected="1"/>
    <summary>7 subdirectories + appendices + index.md = ~33 documentation files total</summary>
  </expected_doc_structure>
</ground_truth>

<consistency_matrix>
  <rule id="CM-01" name="Program Count Consistency">
    <description>The count of COBOL programs must be 31 in every document that cites it</description>
    <canonical_value>31</canonical_value>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/index.md (Codebase Statistics table)</doc>
      <doc>docs/reverse-engineering/appendices/PROGRAM-INVENTORY.md (total count)</doc>
      <doc>docs/reverse-engineering/04-architecture/C4-L3-COMPONENT.md (component count)</doc>
      <doc>docs/reverse-engineering/07-modernization/MODERNIZATION-READINESS.md (scope count)</doc>
      <doc>docs/reverse-engineering/06-quality/TEST-COVERAGE.md (programs under test)</doc>
    </documents_that_must_agree>
    <known_wrong_values>39 (from RE prompt success criteria), any value other than 31</known_wrong_values>
  </rule>

  <rule id="CM-02" name="Copybook Count Consistency">
    <description>The count of copybooks must be 30 in every document that cites it</description>
    <canonical_value>30</canonical_value>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/index.md (Codebase Statistics table)</doc>
      <doc>docs/reverse-engineering/appendices/COPYBOOK-INVENTORY.md (total count)</doc>
      <doc>docs/reverse-engineering/02-data-model/DATA-MODEL.md (copybooks analyzed)</doc>
      <doc>docs/reverse-engineering/02-data-model/DATA-DICTIONARY.md (copybooks documented)</doc>
      <doc>docs/reverse-engineering/01-domain-model/DOMAIN-MODEL.md (copybooks referenced)</doc>
    </documents_that_must_agree>
    <known_wrong_values>41 (from CLAUDE.md/RE prompts), any value other than 30</known_wrong_values>
  </rule>

  <rule id="CM-03" name="BMS Mapset Count Consistency">
    <description>The count of BMS mapsets must be 17 in every document that cites it</description>
    <canonical_value>17</canonical_value>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/index.md (Codebase Statistics table)</doc>
      <doc>docs/reverse-engineering/05-specialized/SCREEN-FLOWS.md (screens documented)</doc>
      <doc>docs/reverse-engineering/04-architecture/C4-L2-CONTAINER.md (presentation layer)</doc>
    </documents_that_must_agree>
    <known_wrong_values>21 (from CLAUDE.md), any value other than 17</known_wrong_values>
  </rule>

  <rule id="CM-04" name="JCL Count Consistency">
    <description>The count of JCL files must be 38 in every document that cites it</description>
    <canonical_value>38</canonical_value>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/index.md (Codebase Statistics table)</doc>
      <doc>docs/reverse-engineering/05-specialized/BATCH-WORKFLOWS.md (jobs documented)</doc>
    </documents_that_must_agree>
  </rule>

  <rule id="CM-05" name="Entity Name Consistency">
    <description>Each domain entity must be referred to by the same name in every document</description>
    <canonical_entities>
      <entity canonical_name="Account" copybook="CVACT01Y.cpy" vsam_file="ACCTDAT"/>
      <entity canonical_name="Card" copybook="CVCRD01Y.cpy" vsam_file="CARDDAT"/>
      <entity canonical_name="Customer" copybook="CVCUS01Y.cpy" vsam_file="CUSTDAT"/>
      <entity canonical_name="Transaction" copybook="CVTRA01Y.cpy" vsam_file="TRANSACT"/>
      <entity canonical_name="User Security" copybook="CSUSR01Y.cpy" vsam_file="USRSEC"/>
      <entity canonical_name="Card-Account Cross-Reference" copybook="CVACT03Y.cpy" vsam_file="CCXREF"/>
    </canonical_entities>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/01-domain-model/DOMAIN-MODEL.md</doc>
      <doc>docs/reverse-engineering/01-domain-model/UBIQUITOUS-LANGUAGE.md</doc>
      <doc>docs/reverse-engineering/02-data-model/DATA-MODEL.md</doc>
      <doc>docs/reverse-engineering/02-data-model/DATA-DICTIONARY.md</doc>
      <doc>docs/reverse-engineering/03-context-model/CONTEXT-MAP.md</doc>
      <doc>docs/reverse-engineering/04-architecture/C4-L3-COMPONENT.md</doc>
      <doc>docs/reverse-engineering/07-modernization/API-CANDIDATES.md</doc>
      <doc>docs/reverse-engineering/07-modernization/API-CONTRACTS.md</doc>
    </documents_that_must_agree>
    <check>An entity called "Account" in the domain model must not be called "Account Master" in the data model and "Account Record" in the API docs unless such variations are explicitly mapped in the ubiquitous language glossary</check>
  </rule>

  <rule id="CM-06" name="PIC Clause Consistency">
    <description>The same field must have the same PIC clause in every document that references it</description>
    <critical_fields>
      <field name="ACCT-CURR-BAL" canonical_pic="S9(10)V99" copybook="CVACT01Y.cpy"/>
      <field name="ACCT-CREDIT-LIMIT" canonical_pic="S9(10)V99" copybook="CVACT01Y.cpy"/>
      <field name="ACCT-CASH-CREDIT-LIMIT" canonical_pic="S9(10)V99" copybook="CVACT01Y.cpy"/>
      <field name="ACCT-CURR-CYC-CREDIT" canonical_pic="S9(10)V99" copybook="CVACT01Y.cpy"/>
      <field name="ACCT-CURR-CYC-DEBIT" canonical_pic="S9(10)V99" copybook="CVACT01Y.cpy"/>
      <field name="ACCT-ID" canonical_pic="9(11)" copybook="CVACT01Y.cpy"/>
    </critical_fields>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/01-domain-model/DOMAIN-MODEL.md</doc>
      <doc>docs/reverse-engineering/02-data-model/DATA-MODEL.md</doc>
      <doc>docs/reverse-engineering/02-data-model/DATA-DICTIONARY.md</doc>
      <doc>docs/reverse-engineering/07-modernization/API-CONTRACTS.md</doc>
    </documents_that_must_agree>
    <known_wrong_values>S9(7)V99, S9(7)V99 COMP-3, any PIC that differs from the canonical copybook definition</known_wrong_values>
  </rule>

  <rule id="CM-07" name="Copybook Name Consistency">
    <description>Copybook references must use correct file names everywhere</description>
    <critical_mappings>
      <mapping wrong="CVCAR00Y.cpy" correct="CVCRD01Y.cpy" entity="Card"/>
      <mapping wrong="COUSR00Y.cpy" correct="CSUSR01Y.cpy" entity="User Security"/>
    </critical_mappings>
    <documents_that_must_agree>ALL documents that reference copybooks</documents_that_must_agree>
  </rule>

  <rule id="CM-08" name="Architecture Layer Consistency">
    <description>Descriptions of the same architectural element must be consistent across C4 levels</description>
    <checks>
      <check>Containers in L2 must decompose into components in L3 — no orphan components</check>
      <check>Components in L3 must map to code patterns in L4 — no ungrounded patterns</check>
      <check>System context actors in L1 must be referenced in security model</check>
      <check>CICS region described in L2 must match the transaction processing described in specialized docs</check>
      <check>Batch subsystem in L2 must match the batch workflows documented in BATCH-WORKFLOWS.md</check>
    </checks>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/04-architecture/C4-L1-SYSTEM-CONTEXT.md</doc>
      <doc>docs/reverse-engineering/04-architecture/C4-L2-CONTAINER.md</doc>
      <doc>docs/reverse-engineering/04-architecture/C4-L3-COMPONENT.md</doc>
      <doc>docs/reverse-engineering/04-architecture/C4-L4-CODE-PATTERNS.md</doc>
      <doc>docs/reverse-engineering/05-specialized/BATCH-WORKFLOWS.md</doc>
      <doc>docs/reverse-engineering/05-specialized/SCREEN-FLOWS.md</doc>
    </documents_that_must_agree>
  </rule>

  <rule id="CM-09" name="Security Model Consistency">
    <description>Security descriptions must be consistent between the security doc and navigation flows</description>
    <checks>
      <check>Authentication flow in SECURITY-MODEL.md must match the signon sequence in SCREEN-FLOWS.md</check>
      <check>Authorization roles in SECURITY-MODEL.md must match the admin/user distinction in NAVIGATION-FLOWS.md</check>
      <check>USRSEC file usage in SECURITY-MODEL.md must match CSUSR01Y.cpy references in DATA-MODEL.md</check>
      <check>Security copybook (CSUSR01Y.cpy) must be consistently named across all docs</check>
    </checks>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/05-specialized/SECURITY-MODEL.md</doc>
      <doc>docs/reverse-engineering/05-specialized/SCREEN-FLOWS.md</doc>
      <doc>docs/reverse-engineering/03-context-model/NAVIGATION-FLOWS.md</doc>
      <doc>docs/reverse-engineering/02-data-model/DATA-MODEL.md</doc>
    </documents_that_must_agree>
  </rule>

  <rule id="CM-10" name="API Candidate Consistency">
    <description>API candidates must align with domain model entities and bounded contexts</description>
    <checks>
      <check>Every API candidate must map to a domain entity documented in DOMAIN-MODEL.md</check>
      <check>API resource names must use terminology from UBIQUITOUS-LANGUAGE.md</check>
      <check>API bounded context assignments must match CONTEXT-MAP.md contexts</check>
      <check>API data types in contracts must match PIC clauses from DATA-DICTIONARY.md (correctly converted)</check>
      <check>Programs cited as API implementation sources must exist in PROGRAM-INVENTORY.md</check>
    </checks>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/07-modernization/API-CANDIDATES.md</doc>
      <doc>docs/reverse-engineering/07-modernization/API-CONTRACTS.md</doc>
      <doc>docs/reverse-engineering/01-domain-model/DOMAIN-MODEL.md</doc>
      <doc>docs/reverse-engineering/01-domain-model/UBIQUITOUS-LANGUAGE.md</doc>
      <doc>docs/reverse-engineering/03-context-model/CONTEXT-MAP.md</doc>
      <doc>docs/reverse-engineering/02-data-model/DATA-DICTIONARY.md</doc>
    </documents_that_must_agree>
  </rule>

  <rule id="CM-11" name="Batch Workflow Data Lineage Consistency">
    <description>Batch workflows must be consistent with data lineage descriptions</description>
    <checks>
      <check>Files read/written by batch jobs in BATCH-WORKFLOWS.md must match DATA-LINEAGE.md</check>
      <check>Batch program names in BATCH-WORKFLOWS.md must match PROGRAM-INVENTORY.md</check>
      <check>JCL step sequences must be consistent with data dependency ordering in DATA-LINEAGE.md</check>
      <check>VSAM file names in batch workflows must match FILE-INVENTORY.md</check>
    </checks>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/05-specialized/BATCH-WORKFLOWS.md</doc>
      <doc>docs/reverse-engineering/02-data-model/DATA-LINEAGE.md</doc>
      <doc>docs/reverse-engineering/appendices/PROGRAM-INVENTORY.md</doc>
      <doc>docs/reverse-engineering/appendices/FILE-INVENTORY.md</doc>
    </documents_that_must_agree>
  </rule>

  <rule id="CM-12" name="Field Count Consistency">
    <description>When multiple documents describe the same entity's fields, they must agree on the count and list</description>
    <checks>
      <check>Account entity field count must be the same in DOMAIN-MODEL.md and DATA-DICTIONARY.md</check>
      <check>Card entity field count must be the same across all documents</check>
      <check>Transaction entity field count must be the same across all documents</check>
      <check>If DOMAIN-MODEL.md says Account has 12 fields, DATA-DICTIONARY.md must not say 15</check>
    </checks>
  </rule>

  <rule id="CM-13" name="Record Length Consistency">
    <description>Record lengths cited in different documents must agree with copybook definitions</description>
    <canonical_lengths>
      <record entity="Account" copybook="CVACT01Y.cpy" length="300"/>
    </canonical_lengths>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/02-data-model/DATA-MODEL.md</doc>
      <doc>docs/reverse-engineering/02-data-model/DATA-DICTIONARY.md</doc>
      <doc>docs/reverse-engineering/appendices/FILE-INVENTORY.md</doc>
    </documents_that_must_agree>
  </rule>

  <rule id="CM-14" name="Modernization Readiness Cross-Consistency">
    <description>Modernization assessments must align with technical findings from other documents</description>
    <checks>
      <check>Complexity ratings in MODERNIZATION-READINESS.md must be justified by code patterns in C4-L4</check>
      <check>Migration phases in MIGRATION-ROADMAP.md must align with bounded contexts in CONTEXT-MAP.md</check>
      <check>Technical debt items must trace to findings in TEST-COVERAGE.md or architecture docs</check>
      <check>Recommended migration approach must be consistent with integration patterns analysis</check>
    </checks>
    <documents_that_must_agree>
      <doc>docs/reverse-engineering/07-modernization/MODERNIZATION-READINESS.md</doc>
      <doc>docs/reverse-engineering/07-modernization/MIGRATION-ROADMAP.md</doc>
      <doc>docs/reverse-engineering/04-architecture/C4-L4-CODE-PATTERNS.md</doc>
      <doc>docs/reverse-engineering/03-context-model/CONTEXT-MAP.md</doc>
      <doc>docs/reverse-engineering/05-specialized/INTEGRATION-PATTERNS.md</doc>
      <doc>docs/reverse-engineering/06-quality/TEST-COVERAGE.md</doc>
    </documents_that_must_agree>
  </rule>

  <rule id="CM-15" name="Index Completeness Consistency">
    <description>The master index must accurately reflect the actual documents that exist</description>
    <checks>
      <check>Every document linked in index.md must actually exist</check>
      <check>Every existing document in docs/reverse-engineering/ must be linked in index.md</check>
      <check>Statistics in index.md must match appendices</check>
      <check>Document status table must reflect actual completion state</check>
    </checks>
  </rule>
</consistency_matrix>

<cross_reference_checks>
  <check id="XR-01" name="Domain Model to Data Model">
    <from>docs/reverse-engineering/01-domain-model/DOMAIN-MODEL.md</from>
    <to>docs/reverse-engineering/02-data-model/DATA-MODEL.md</to>
    <verify>Every domain entity maps to at least one physical data structure</verify>
    <verify>Entity names match between the two documents</verify>
    <verify>Relationships described in domain model have corresponding foreign key or cross-reference structures in data model</verify>
  </check>

  <check id="XR-02" name="Domain Model to Ubiquitous Language">
    <from>docs/reverse-engineering/01-domain-model/DOMAIN-MODEL.md</from>
    <to>docs/reverse-engineering/01-domain-model/UBIQUITOUS-LANGUAGE.md</to>
    <verify>Every entity name in the domain model appears in the glossary</verify>
    <verify>Every aggregate name appears in the glossary</verify>
    <verify>Key field names used in entity descriptions are in the glossary</verify>
  </check>

  <check id="XR-03" name="Domain Model to Business Rules">
    <from>docs/reverse-engineering/01-domain-model/DOMAIN-MODEL.md</from>
    <to>docs/reverse-engineering/01-domain-model/BUSINESS-RULES.md</to>
    <verify>Every entity with validation logic has corresponding business rules</verify>
    <verify>Aggregate invariants are documented as business rules</verify>
  </check>

  <check id="XR-04" name="Data Model to Data Dictionary">
    <from>docs/reverse-engineering/02-data-model/DATA-MODEL.md</from>
    <to>docs/reverse-engineering/02-data-model/DATA-DICTIONARY.md</to>
    <verify>Every record layout in data model has field-level documentation in data dictionary</verify>
    <verify>PIC clauses match between the two documents</verify>
    <verify>Record lengths match between the two documents</verify>
  </check>

  <check id="XR-05" name="Data Model to ER Diagram">
    <from>docs/reverse-engineering/02-data-model/DATA-MODEL.md</from>
    <to>docs/reverse-engineering/02-data-model/diagrams/er-diagram.md</to>
    <verify>Every entity in the data model appears in the ER diagram</verify>
    <verify>Relationships in the ER diagram match the data model descriptions</verify>
    <verify>Cardinalities are consistent</verify>
  </check>

  <check id="XR-06" name="Context Map to Architecture">
    <from>docs/reverse-engineering/03-context-model/CONTEXT-MAP.md</from>
    <to>docs/reverse-engineering/04-architecture/C4-L2-CONTAINER.md</to>
    <verify>Bounded contexts map to containers or groups of components</verify>
    <verify>Context relationships align with container communication paths</verify>
  </check>

  <check id="XR-07" name="Context Map to API Candidates">
    <from>docs/reverse-engineering/03-context-model/CONTEXT-MAP.md</from>
    <to>docs/reverse-engineering/07-modernization/API-CANDIDATES.md</to>
    <verify>API candidates are organized by bounded context</verify>
    <verify>Context boundaries inform API boundary decisions</verify>
  </check>

  <check id="XR-08" name="Screen Flows to Navigation Flows">
    <from>docs/reverse-engineering/05-specialized/SCREEN-FLOWS.md</from>
    <to>docs/reverse-engineering/03-context-model/NAVIGATION-FLOWS.md</to>
    <verify>Screen navigation paths are consistent between both documents</verify>
    <verify>BMS mapset references match between both documents</verify>
    <verify>Program-to-screen mappings are consistent</verify>
  </check>

  <check id="XR-09" name="Security Model to Signon Flow">
    <from>docs/reverse-engineering/05-specialized/SECURITY-MODEL.md</from>
    <to>docs/reverse-engineering/05-specialized/SCREEN-FLOWS.md</to>
    <verify>Signon screen sequence matches security authentication flow</verify>
    <verify>Admin vs regular user paths are consistent</verify>
  </check>

  <check id="XR-10" name="Batch Workflows to Data Lineage">
    <from>docs/reverse-engineering/05-specialized/BATCH-WORKFLOWS.md</from>
    <to>docs/reverse-engineering/02-data-model/DATA-LINEAGE.md</to>
    <verify>Batch job input/output files match data lineage flow descriptions</verify>
    <verify>Processing order in batch is consistent with data dependency ordering</verify>
  </check>

  <check id="XR-11" name="API Contracts to Data Dictionary">
    <from>docs/reverse-engineering/07-modernization/API-CONTRACTS.md</from>
    <to>docs/reverse-engineering/02-data-model/DATA-DICTIONARY.md</to>
    <verify>API field types are correct conversions of COBOL PIC clauses from data dictionary</verify>
    <verify>Field names in API schemas trace to data dictionary field names</verify>
  </check>

  <check id="XR-12" name="Test Coverage to Program Inventory">
    <from>docs/reverse-engineering/06-quality/TEST-COVERAGE.md</from>
    <to>docs/reverse-engineering/appendices/PROGRAM-INVENTORY.md</to>
    <verify>Programs listed in test coverage match program inventory</verify>
    <verify>No test references to programs not in the inventory</verify>
  </check>

  <check id="XR-13" name="Modernization Readiness to All Upstream">
    <from>docs/reverse-engineering/07-modernization/MODERNIZATION-READINESS.md</from>
    <to>Multiple upstream documents</to>
    <verify>Complexity scores reference specific code patterns from C4-L4</verify>
    <verify>Migration approach references bounded contexts from CONTEXT-MAP.md</verify>
    <verify>Security recommendations reference findings from SECURITY-MODEL.md</verify>
    <verify>Data migration plan references data model from DATA-MODEL.md</verify>
  </check>

  <check id="XR-14" name="Index to All Documents">
    <from>docs/reverse-engineering/index.md</from>
    <to>All documentation files</to>
    <verify>Every link in the index resolves to an existing file</verify>
    <verify>Every documentation file is linked from the index</verify>
    <verify>Statistics in the index match the actual document contents</verify>
  </check>
</cross_reference_checks>

<hallucination_registry>
  <purpose>
    Aggregate ALL hallucination findings from individual VL-001 through VL-012 validation reports
    into a single master registry. This registry serves as the authoritative list of all known
    hallucinations across the entire documentation suite.
  </purpose>

  <known_hallucination_patterns>
    <pattern id="HP-01" name="Non-existent Copybook: CVCAR00Y.cpy">
      <description>References to CVCAR00Y.cpy as the card copybook</description>
      <reality>The actual card copybook is CVCRD01Y.cpy</reality>
      <severity>Critical</severity>
      <expected_in>Domain model, data model, data dictionary, any card-related documentation</expected_in>
    </pattern>
    <pattern id="HP-02" name="Non-existent Copybook: COUSR00Y.cpy">
      <description>References to COUSR00Y.cpy as the user security copybook</description>
      <reality>The actual user security copybook is CSUSR01Y.cpy</reality>
      <severity>Critical</severity>
      <expected_in>Domain model, data model, security model, user management documentation</expected_in>
    </pattern>
    <pattern id="HP-03" name="Wrong Monetary PIC Clause: S9(7)V99">
      <description>Monetary fields documented with PIC S9(7)V99 or S9(7)V99 COMP-3</description>
      <reality>Actual PIC is S9(10)V99 per CVACT01Y.cpy:7</reality>
      <severity>Critical</severity>
      <expected_in>Domain model, data model, data dictionary, API contracts</expected_in>
    </pattern>
    <pattern id="HP-04" name="Inflated Program Count: 39">
      <description>Documentation claiming 39 COBOL programs</description>
      <reality>Actual count is 31 (29 .cbl + 2 .CBL)</reality>
      <severity>Major</severity>
      <expected_in>Index, program inventory, architecture docs, modernization readiness</expected_in>
    </pattern>
    <pattern id="HP-05" name="Inflated Copybook Count: 41">
      <description>Documentation claiming 41 copybooks</description>
      <reality>Actual count is 30 (29 .cpy + 1 .CPY)</reality>
      <severity>Major</severity>
      <expected_in>Index, copybook inventory, data model docs</expected_in>
    </pattern>
    <pattern id="HP-06" name="Inflated BMS Count: 21">
      <description>Documentation claiming 21 BMS mapsets</description>
      <reality>Actual count is 17</reality>
      <severity>Major</severity>
      <expected_in>Index, screen flows, architecture docs</expected_in>
    </pattern>
  </known_hallucination_patterns>

  <aggregation_instructions>
    <step>1. Read each VL report (VL-001 through VL-012) and extract the Hallucination Inventory table</step>
    <step>2. Merge all hallucination entries into a single master table</step>
    <step>3. Deduplicate: if the same hallucination appears in multiple VL reports, consolidate with cross-references</step>
    <step>4. Add any NEW hallucinations discovered during cross-document consistency checks</step>
    <step>5. Classify each hallucination by pattern (HP-01 through HP-06) or as a new pattern</step>
    <step>6. Count total occurrences of each hallucination pattern across ALL documents</step>
    <step>7. Calculate hallucination density: total hallucinations / total documents analyzed</step>
  </aggregation_instructions>
</hallucination_registry>

<validation_methodology>
  <phase name="1_collect_vl_verdicts">
    <description>Read all individual validation reports and collect their verdicts, scores, and findings</description>
    <steps>
      <step>Read VL-001 through VL-012 validation reports from docs/reverse-engineering/validation/</step>
      <step>For each report, extract: verdict (PASS/FAIL), overall score, per-category scores</step>
      <step>For each report, extract: critical findings count, major findings count, minor findings count</step>
      <step>For each report, extract: complete hallucination inventory table</step>
      <step>Create a per-VL verdict summary in .work/reverse-engineering/validation/vl-000/vl-verdicts.yaml</step>
      <step>If any VL report is missing, note it as MISSING and flag for the final report</step>
    </steps>
  </phase>

  <phase name="2_count_consistency_analysis">
    <description>Verify that quantitative claims are consistent across all documents</description>
    <steps>
      <step>Search ALL documentation files for program count claims (look for "31", "39", "program" near numbers)</step>
      <step>Search ALL documentation files for copybook count claims (look for "30", "41", "copybook" near numbers)</step>
      <step>Search ALL documentation files for BMS count claims (look for "17", "21", "BMS" near numbers)</step>
      <step>Search ALL documentation files for JCL count claims (look for "38", "JCL" near numbers)</step>
      <step>Search ALL documentation files for data file count claims (look for "22", "data file" near numbers)</step>
      <step>For each count claim found, record: document, line number, claimed value, canonical value, match (yes/no)</step>
      <step>Create count consistency matrix in .work/reverse-engineering/validation/vl-000/count-consistency.yaml</step>
    </steps>
  </phase>

  <phase name="3_terminology_consistency_analysis">
    <description>Verify that domain terminology is used consistently across all documents</description>
    <steps>
      <step>Load the ubiquitous language glossary from docs/reverse-engineering/01-domain-model/UBIQUITOUS-LANGUAGE.md</step>
      <step>For each core entity (Account, Card, Customer, Transaction, User Security, Cross-Reference):
        search all documents for variant names and verify consistency</step>
      <step>Check that VSAM file names (ACCTDAT, CARDDAT, CUSTDAT, TRANSACT, CCXREF, USRSEC) are spelled consistently</step>
      <step>Check that copybook names are spelled consistently (case sensitivity: CVCRD01Y.cpy not cvcrd01y.CPY)</step>
      <step>Check that program names are spelled consistently</step>
      <step>Record all terminology inconsistencies in .work/reverse-engineering/validation/vl-000/terminology.yaml</step>
    </steps>
  </phase>

  <phase name="4_cross_reference_verification">
    <description>Verify that cross-references between documents resolve correctly</description>
    <steps>
      <step>For each cross-reference check (XR-01 through XR-14), verify the specific claims listed</step>
      <step>Check that entity definitions in the domain model match entity structures in the data model</step>
      <step>Check that bounded contexts in the context map align with API groupings</step>
      <step>Check that security flow descriptions match between security model and screen flows</step>
      <step>Check that batch workflow file references match data lineage descriptions</step>
      <step>Check that architecture layer descriptions are consistent across C4 levels</step>
      <step>Check that API data types correctly convert PIC clauses from the data dictionary</step>
      <step>Check that the index links to all existing documents and all links resolve</step>
      <step>Record all cross-reference failures in .work/reverse-engineering/validation/vl-000/xref-failures.yaml</step>
    </steps>
  </phase>

  <phase name="5_hallucination_aggregation">
    <description>Aggregate all hallucination findings from individual VL reports into master registry</description>
    <steps>
      <step>Extract hallucination inventory from each VL report (VL-001 through VL-012)</step>
      <step>Merge into master hallucination table with deduplication</step>
      <step>Classify each hallucination by pattern (HP-01 through HP-06 or new pattern)</step>
      <step>Add any NEW hallucinations discovered during cross-document analysis (phases 2-4)</step>
      <step>Count total occurrences per pattern across all documents</step>
      <step>Calculate hallucination density (total hallucinations / total documents)</step>
      <step>Identify most-hallucinated-about topics (which entities, which counts, which files)</step>
      <step>Write master registry to .work/reverse-engineering/validation/vl-000/hallucination-registry.yaml</step>
    </steps>
  </phase>

  <phase name="6_report_generation">
    <description>Produce the structured cross-document consistency validation report</description>
    <output>docs/reverse-engineering/validation/VL-000-cross-document-consistency-report.md</output>
    <format>
      - Per-VL verdict summary (PASS/FAIL for each VL-001 through VL-012)
      - Overall documentation suite verdict with composite score
      - Cross-document consistency matrix (which docs agree, which contradict)
      - Count consistency analysis (every count claim across every document)
      - Terminology consistency analysis (entity name variations found)
      - Cross-reference verification results (XR-01 through XR-14)
      - Master hallucination registry (aggregated from all VL reports + new findings)
      - Hallucination density statistics
      - Critical contradictions list (where documents directly contradict each other)
      - Specific remediation recommendations for cross-document issues
    </format>
  </phase>
</validation_methodology>

<scoring_rubric>
  <category name="Source Reference Accuracy" weight="35" severity="Critical">
    <description>Cross-document references resolve correctly; cited documents and sections exist</description>
    <scoring>
      100: All cross-references resolve, all cited sections exist in target documents
      80: 90%+ cross-references resolve, minor section naming discrepancies
      60: 80%+ resolve, some broken links between documents
      40: Significant broken cross-references, documents reference non-existent sections
      20: Majority of cross-references broken
      0: Cross-reference system is unreliable
    </scoring>
  </category>
  <category name="Factual Accuracy" weight="25" severity="Critical">
    <description>No contradictions between documents; same facts stated consistently everywhere</description>
    <scoring>
      100: Zero contradictions found between any documents
      80: Minor discrepancies only (e.g., slight wording differences, same meaning)
      60: A few factual contradictions in non-critical areas
      40: Contradictions in entity definitions, PIC clauses, or record layouts across docs
      20: Multiple material contradictions that would cause incorrect modernization decisions
      0: Pervasive contradictions rendering the documentation suite unreliable
    </scoring>
  </category>
  <category name="Completeness" weight="20" severity="Major">
    <description>All expected documents exist; all VL reports completed; full coverage of consistency checks</description>
    <scoring>
      100: All ~33 docs exist, all 12 VL reports completed, all consistency rules checked
      80: 90%+ docs exist, 10+ VL reports completed, most consistency rules checked
      60: 75%+ docs exist, 8+ VL reports completed, core consistency rules checked
      40: Significant documents missing, fewer than 8 VL reports available
      20: Less than 50% document coverage
      0: Documentation suite severely incomplete
    </scoring>
  </category>
  <category name="Quantitative Accuracy" weight="10" severity="Major">
    <description>File counts, record lengths, entity counts consistent across all documents</description>
    <scoring>
      100: All counts match canonical values (31 programs, 30 copybooks, 17 BMS, 38 JCL) everywhere
      80: Counts consistent across documents but may differ slightly from canonical
      60: Minor count inconsistencies in 1-2 documents
      40: Major count errors propagated (e.g., 39 programs in multiple docs)
      20: Counts are inconsistent across most documents
      0: No reliable quantitative agreement across the documentation suite
    </scoring>
  </category>
  <category name="Documentation Quality" weight="10" severity="Minor">
    <description>Consistent formatting, terminology, and style across all documents</description>
    <scoring>
      100: Professional, uniform style; consistent heading hierarchy; terminology standardized
      80: Minor style variations, terminology mostly consistent
      60: Some formatting inconsistencies, occasional terminology drift
      40: Significant style and terminology inconsistencies between document groups
      20: Poor consistency in style and terminology
      0: No discernible documentation standards applied
    </scoring>
  </category>
</scoring_rubric>

<output_specification>
  <report_file>docs/reverse-engineering/validation/VL-000-cross-document-consistency-report.md</report_file>
  <work_directory>.work/reverse-engineering/validation/vl-000/</work_directory>

  <report_structure>
    # Cross-Document Consistency Validation Report (VL-000)

    ## Overall Documentation Suite Verdict: [PASS|FAIL] — Score: [NN]/100

    ## Per-VL Verdict Summary

    | VL Prompt | Validates | Verdict | Score | Critical | Major | Minor |
    |-----------|-----------|---------|-------|----------|-------|-------|
    | VL-001 | Domain Model (RE-001) | [verdict] | [score] | [N] | [N] | [N] |
    | VL-002 | Data Model (RE-002) | [verdict] | [score] | [N] | [N] | [N] |
    | VL-003 | Context Model (RE-003) | [verdict] | [score] | [N] | [N] | [N] |
    | VL-004 | C4 Architecture (RE-004) | [verdict] | [score] | [N] | [N] | [N] |
    | VL-005 | Screen Flows (RE-005) | [verdict] | [score] | [N] | [N] | [N] |
    | VL-006 | Batch Workflows (RE-006) | [verdict] | [score] | [N] | [N] | [N] |
    | VL-007 | Security Model (RE-007) | [verdict] | [score] | [N] | [N] | [N] |
    | VL-008 | Integration Patterns (RE-008) | [verdict] | [score] | [N] | [N] | [N] |
    | VL-009 | Test Coverage (RE-009) | [verdict] | [score] | [N] | [N] | [N] |
    | VL-010 | Modernization Readiness (RE-010) | [verdict] | [score] | [N] | [N] | [N] |
    | VL-011 | API Candidates (RE-011) | [verdict] | [score] | [N] | [N] | [N] |
    | VL-012 | Master Index (RE-000) | [verdict] | [score] | [N] | [N] | [N] |

    ## Score Breakdown (Cross-Document Consistency)

    | Category | Weight | Score | Weighted |
    |----------|--------|-------|----------|
    | Source Reference Accuracy | 35% | NN/100 | NN.N |
    | Factual Accuracy | 25% | NN/100 | NN.N |
    | Completeness | 20% | NN/100 | NN.N |
    | Quantitative Accuracy | 10% | NN/100 | NN.N |
    | Documentation Quality | 10% | NN/100 | NN.N |
    | **Total** | **100%** | | **NN.N** |

    ## Cross-Document Consistency Matrix

    | Rule | Description | Status | Documents Checked | Contradictions Found |
    |------|-------------|--------|-------------------|---------------------|
    | CM-01 | Program Count | [CONSISTENT/INCONSISTENT] | [N] | [N] |
    | CM-02 | Copybook Count | [CONSISTENT/INCONSISTENT] | [N] | [N] |
    | ... | ... | ... | ... | ... |
    | CM-15 | Index Completeness | [CONSISTENT/INCONSISTENT] | [N] | [N] |

    ## Count Consistency Analysis

    ### Program Count (Canonical: 31)
    | Document | Claimed Value | Match | Line/Section |
    |----------|--------------|-------|--------------|
    | index.md | [value] | [yes/no] | [location] |
    | ... | ... | ... | ... |

    ### Copybook Count (Canonical: 30)
    [Same table structure]

    ### BMS Mapset Count (Canonical: 17)
    [Same table structure]

    ### JCL Count (Canonical: 38)
    [Same table structure]

    ### Data File Count (Canonical: 22)
    [Same table structure]

    ## Terminology Consistency Analysis

    | Term | Canonical Form | Variants Found | Documents with Variants |
    |------|---------------|----------------|------------------------|
    | [entity name] | [canonical] | [variant1, variant2] | [doc1, doc2] |

    ## Cross-Reference Verification Results

    | Check | From | To | Status | Issues |
    |-------|------|----|--------|--------|
    | XR-01 | Domain Model | Data Model | [PASS/FAIL] | [description] |
    | XR-02 | Domain Model | Ubiquitous Language | [PASS/FAIL] | [description] |
    | ... | ... | ... | ... | ... |
    | XR-14 | Index | All Documents | [PASS/FAIL] | [description] |

    ## Master Hallucination Registry

    ### Hallucination Pattern Summary

    | Pattern | Description | Occurrences | Documents Affected | Severity |
    |---------|------------|-------------|-------------------|----------|
    | HP-01 | CVCAR00Y.cpy reference | [N] | [list] | Critical |
    | HP-02 | COUSR00Y.cpy reference | [N] | [list] | Critical |
    | HP-03 | S9(7)V99 PIC clause | [N] | [list] | Critical |
    | HP-04 | 39 programs count | [N] | [list] | Major |
    | HP-05 | 41 copybooks count | [N] | [list] | Major |
    | HP-06 | 21 BMS mapsets count | [N] | [list] | Major |
    | HP-07+ | [new patterns] | [N] | [list] | [severity] |

    ### Detailed Hallucination Inventory (Aggregated)

    | # | Source VL | Document | Claim | Reality | Pattern | Severity |
    |---|----------|----------|-------|---------|---------|----------|
    | 1 | VL-001 | DOMAIN-MODEL.md | [claim] | [reality] | HP-NN | Critical |
    | ... | ... | ... | ... | ... | ... | ... |

    ### Hallucination Density Statistics

    | Metric | Value |
    |--------|-------|
    | Total hallucinations found | [N] |
    | Total documents analyzed | [N] |
    | Hallucination density (per doc) | [N.N] |
    | Most affected document | [filename] |
    | Most common pattern | HP-[NN] |
    | Critical hallucinations | [N] |
    | Major hallucinations | [N] |

    ## Critical Contradictions

    [List of cases where two documents directly contradict each other on the same factual claim]

    | # | Document A | Claim A | Document B | Claim B | Ground Truth | Severity |
    |---|-----------|---------|-----------|---------|-------------|----------|
    | 1 | [doc] | [claim] | [doc] | [claim] | [truth] | Critical |

    ## Recommendations

    ### Critical Remediation (Must Fix)
    [Specific actions to resolve critical contradictions and hallucinations]

    ### Major Remediation (Should Fix)
    [Actions to resolve major inconsistencies]

    ### Minor Remediation (Nice to Fix)
    [Actions to improve consistency and quality]

    ### Process Improvements
    [Recommendations for improving the RE prompt suite to prevent recurrence]

    ## Remediation Manifest

    ### Per-Finding Remediation
    | ID | Finding | Target File | Location | Current (Wrong) | Required (Correct) | Source Evidence | Remediation Action | RE Prompt |
    |----|---------|-------------|----------|-----------------|--------------------|-----------------|--------------------|-----------|
    | R-001 | [finding] | [doc path] | [section/line] | [wrong] | [correct] | [source:line] | [fix] | [RE-NNN] |

    ### Cross-Document Consistency Fixes
    | ID | Inconsistency | File A (Source of Truth) | File B (Needs Fix) | Value in A | Value in B | Fix Required |
    |----|--------------|------------------------|--------------------|------------|------------|-------------|
    | X-001 | [description] | [file A] | [file B] | [correct] | [wrong] | [action] |

    ### Remediation Instructions
    For each row: (1) Read target at location, (2) Verify wrong value, (3) Replace with correct per source, (4) Re-validate

    ### Affected RE Prompts
    [List RE prompts needing re-execution, ordered by dependency phase]
  </report_structure>
</output_specification>

<foundational_principles>
  <principle id="1">Consistency is non-negotiable — the same fact must be stated the same way in every document that cites it</principle>
  <principle id="2">Canonical counts (31 programs, 30 copybooks, 17 BMS, 17 BMS copybooks, 38 JCL, 22 data files) are the single source of truth for all quantitative claims</principle>
  <principle id="3">Entity names from the ubiquitous language glossary are the authoritative names — all documents must use them</principle>
  <principle id="4">PIC clauses must match the source copybook exactly — no rounding, no approximation, no shortening (S9(10)V99 is NOT S9(7)V99)</principle>
  <principle id="5">Cross-references between documents must resolve — a link to a section in another document must point to a section that actually exists with that name</principle>
  <principle id="6">The master hallucination registry must be exhaustive — every hallucination found by any VL prompt must appear in it</principle>
  <principle id="7">Contradictions between documents are worse than gaps — a gap means information is missing, a contradiction means information is wrong and the reader cannot tell which version to trust</principle>
  <principle id="8">Architecture consistency spans all C4 levels — an element described at L1 must be traceable through L2, L3, and L4 without contradiction</principle>
</foundational_principles>

<critical_reminders>
  <reminder id="1">This prompt runs LAST — after ALL VL-001 through VL-012 reports have been generated. If any VL report is missing, note it as MISSING but continue with available reports.</reminder>
  <reminder id="2">CVCAR00Y.cpy does NOT exist — the card copybook is CVCRD01Y.cpy. If ANY document references CVCAR00Y.cpy, that is a hallucination. Log it in the master registry.</reminder>
  <reminder id="3">COUSR00Y.cpy does NOT exist — the user security copybook is CSUSR01Y.cpy. If ANY document references COUSR00Y.cpy, that is a hallucination. Log it in the master registry.</reminder>
  <reminder id="4">Monetary PIC is S9(10)V99, NOT S9(7)V99 COMP-3. If ANY document uses S9(7)V99, that is a hallucination. Log it in the master registry.</reminder>
  <reminder id="5">There are 31 COBOL programs, NOT 39. If ANY document claims 39, that is a hallucination. Log it in the master registry.</reminder>
  <reminder id="6">There are 30 copybooks, NOT 41. If ANY document claims 41, that is a hallucination. Log it in the master registry.</reminder>
  <reminder id="7">There are 17 BMS mapsets, NOT 21. If ANY document claims 21, that is a hallucination. Log it in the master registry.</reminder>
  <reminder id="8">When two documents contradict each other, determine which one matches the source code. The one matching source code is correct; the other is wrong. Document BOTH in the contradictions table.</reminder>
  <reminder id="9">Cross-reference verification is not just about links existing — it is about the CONTENT being consistent. A link that resolves but points to contradictory information is worse than a broken link.</reminder>
  <reminder id="10">The final verdict is for the DOCUMENTATION SUITE AS A WHOLE, not for any individual document. A suite where each document scores 80 individually but they contradict each other is a FAIL.</reminder>
</critical_reminders>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/validation/vl-000/</path>
    <purpose>Persist cross-document validation progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
      validation_phase: "collect_verdicts|count_consistency|terminology|cross_references|hallucination_aggregation|reporting"
      vl_reports_read: ["VL-001", "VL-002", ...]
      vl_reports_missing: ["VL-NNN", ...]
      documents_scanned: N
      consistency_rules_checked: N
      cross_references_verified: N
      hallucinations_aggregated: N
      contradictions_found: N
      findings:
        critical: N
        major: N
        minor: N
      next_action: "Detailed description of next step"
      last_updated: "ISO timestamp"
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/validation/vl-000/progress.yaml if it exists</step>
    <step>2. Load any existing findings from the work directory (vl-verdicts.yaml, count-consistency.yaml, terminology.yaml, xref-failures.yaml, hallucination-registry.yaml)</step>
    <step>3. Continue from next_action without re-processing completed phases</step>
    <step>4. Update progress.yaml after each phase completion</step>
  </resumption_protocol>
</context_compaction_survival>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/validation/vl-000/progress.yaml if it exists
    2. If progress exists and validation_phase != "complete":
       - Load existing findings from work directory
       - Resume from next_action
    3. If no progress or starting fresh:
       - Begin with Phase 1: Collect VL Verdicts
       - Read all VL-001 through VL-012 reports from docs/reverse-engineering/validation/
       - Create initial progress.yaml
    4. Phase 2: Count Consistency Analysis
       - Search ALL documentation files for program/copybook/BMS/JCL/data file counts
       - Compare every count claim against canonical values
       - Record all inconsistencies
    5. Phase 3: Terminology Consistency Analysis
       - Load ubiquitous language glossary
       - Search all documents for entity name variations
       - Record all terminology inconsistencies
    6. Phase 4: Cross-Reference Verification
       - Execute each cross-reference check (XR-01 through XR-14)
       - Record all broken or inconsistent cross-references
    7. Phase 5: Hallucination Aggregation
       - Merge hallucination inventories from all VL reports
       - Add new hallucinations found during cross-document analysis
       - Calculate hallucination density statistics
    8. Phase 6: Report Generation
       - Compile all findings into the final report
       - Calculate overall documentation suite score
       - Determine suite verdict (PASS/FAIL: 100=PASS, less than 100=FAIL)
       - Write report to docs/reverse-engineering/validation/VL-000-cross-document-consistency-report.md
    9. After completing each phase:
       - Update progress.yaml
       - Write intermediate findings to work directory immediately
    10. The final report must include ALL sections defined in the report_structure above
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code LAST, after all individual validation prompts (VL-001 through VL-012) have completed. This prompt will:

1. **Collect** verdicts and findings from all 12 individual VL reports
2. **Analyze** count consistency across all ~29 documentation files against canonical values (31 programs, 30 copybooks, 17 BMS, 38 JCL, 22 data files)
3. **Verify** terminology consistency using the ubiquitous language glossary as the authoritative reference
4. **Check** 14 specific cross-document reference paths (XR-01 through XR-14) for resolution and content consistency
5. **Aggregate** all hallucination findings from individual VL reports into a master hallucination registry
6. **Report** with per-VL verdict summary, consistency matrix, hallucination registry, and overall suite verdict

## Expected Output

| File | Description |
|------|-------------|
| `VL-000-cross-document-consistency-report.md` | Master cross-document consistency report with suite verdict, consistency matrix, hallucination registry, and remediation guidance |

## Prerequisites

- ALL individual validation prompts (VL-001 through VL-012) must have been executed and their reports exist in `docs/reverse-engineering/validation/`
- ALL RE prompts (RE-000 through RE-011) must have been executed and their output exists in `docs/reverse-engineering/`
- Access to source code in `app/cbl/`, `app/cpy/`, `app/bms/`, `app/jcl/` for ground truth verification

## Depends On

- VL-001 (Domain Model Validation)
- VL-002 (Data Model Validation)
- VL-003 (Context Model Validation)
- VL-004 (C4 Architecture Validation)
- VL-005 (Screen Flow Validation)
- VL-006 (Batch Workflow Validation)
- VL-007 (Security Model Validation)
- VL-008 (Integration Patterns Validation)
- VL-009 (Test Coverage Validation)
- VL-010 (Modernization Readiness Validation)
- VL-011 (API Candidates Validation)
- VL-012 (Master Index Validation)

This is a **Phase 8** prompt — it runs LAST after all other validation prompts have completed.

## Blocks

- None (this is the final validation prompt in the suite)
