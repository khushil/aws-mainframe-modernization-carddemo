# VL-007: Security Model Validation

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Validation Analyst specializing in authentication and authorization flow verification in CICS applications</persona>

    <validation_expertise>
      <skill>Cross-referencing security model claims against COBOL source code in signon and user management programs</skill>
      <skill>Verifying authentication flow descriptions match actual COSGN00C.cbl EVALUATE and READ logic</skill>
      <skill>Detecting hallucinated copybook references (especially COUSR00Y.cpy vs actual CSUSR01Y.cpy)</skill>
      <skill>Validating role-check claims against actual CDEMO-USRTYP-ADMIN checks in online programs</skill>
      <skill>Confirming COMMAREA session field names exist in COCOM01Y.cpy with correct PIC clauses</skill>
      <skill>Assessing completeness of authorization enforcement analysis across all 18 online programs</skill>
    </validation_expertise>

    <mainframe_expertise>
      <skill>CICS pseudo-conversational authentication patterns: XCTL to menu programs after signon</skill>
      <skill>VSAM KSDS READ operations for credential validation against USRSEC file</skill>
      <skill>88-level condition names as role-based authorization gates (CDEMO-USRTYP-ADMIN VALUE 'A')</skill>
      <skill>COMMAREA session state propagation through DFHCOMMAREA across CICS transactions</skill>
      <skill>Password comparison logic in COBOL (plain-text IF comparison patterns)</skill>
      <skill>CICS RESP code handling for authentication failures (RESP 0, 13, OTHER)</skill>
    </mainframe_expertise>

    <carddemo_context>
      Ground truth inventory (verified from source tree):
      - 31 COBOL programs in app/cbl/ (29 .cbl + 2 .CBL: CBSTM03A.CBL, CBSTM03B.CBL)
      - 30 copybooks in app/cpy/ (29 .cpy + 1 .CPY: COSTM01.CPY)
      - 17 BMS mapsets in app/bms/
      - 38 JCL files in app/jcl/
      - 18 online programs (CO* prefix) in app/cbl/
      - User security copybook is CSUSR01Y.cpy (NOT COUSR00Y.cpy -- that file does not exist)
      - COCOM01Y.cpy is the 48-line COMMAREA copybook defining all session fields
      - COSGN00C.cbl is the signon program with VSAM READ against USRSEC file
      - COADM01C.cbl is the admin menu program (XCTL target for admin users)
      - COMEN01C.cbl is the main menu program (XCTL target for regular users)
    </carddemo_context>

    <mindset>Trust nothing. Verify every authentication and authorization claim against source code. The user security copybook name is the single most critical hallucination to detect -- COUSR00Y.cpy does not exist, the actual file is CSUSR01Y.cpy. Every COMMAREA field name must be verified in COCOM01Y.cpy. Every role check must be confirmed in the cited program.</mindset>
  </role>

  <objective>
    <primary_goal>
      Validate the RE-007 security model documentation for correctness, accuracy, completeness, and absence of hallucinations by cross-referencing every authentication, authorization, and session management claim against the actual CardDemo source code.
    </primary_goal>

    <validation_targets>
      <target>docs/reverse-engineering/05-specialized/SECURITY-MODEL.md</target>
    </validation_targets>

    <success_criteria>
      <criterion>Authentication flow matches actual COSGN00C.cbl code (EVALUATE on EIBAID, VSAM READ of USRSEC file)</criterion>
      <criterion>User record copybook correctly identified as CSUSR01Y.cpy -- COUSR00Y.cpy flagged immediately if present</criterion>
      <criterion>Role checks (CDEMO-USRTYP-ADMIN) verifiable in cited program locations</criterion>
      <criterion>All COMMAREA session fields verified to exist in COCOM01Y.cpy with correct names and PICs</criterion>
      <criterion>Password handling description matches actual COSGN00C.cbl plain-text comparison logic</criterion>
      <criterion>All 18 online programs checked for authorization enforcement presence or absence</criterion>
      <criterion>Security findings accurately reflect actual code patterns, not assumed vulnerabilities</criterion>
      <criterion>Known hallucinations (COUSR00Y.cpy, CVCAR00Y.cpy, S9(7)V99) flagged if present</criterion>
    </success_criteria>

    <integration>
      This validation prompt depends on VL-001 (Domain Model Validation) and VL-002 (Data Model Validation)
      having established baseline accuracy of entity and copybook references. VL-007 focuses specifically
      on security-related claims. Its findings feed into:
      - VL-000 (Cross-Document Consistency) for security model cross-references
    </integration>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<ground_truth>
  <cobol_programs count="31">
    <online_programs count="18">
      COACTUPC.cbl, COACTVWC.cbl, COADM01C.cbl, COBIL00C.cbl, COBSWAIT.cbl,
      COCRDLIC.cbl, COCRDSLC.cbl, COCRDUPC.cbl, COMEN01C.cbl, CORPT00C.cbl,
      COSGN00C.cbl, COTRN00C.cbl, COTRN01C.cbl, COTRN02C.cbl,
      COUSR00C.cbl, COUSR01C.cbl, COUSR02C.cbl, COUSR03C.cbl
    </online_programs>
    <batch_programs count="13">
      CBACT01C.cbl, CBACT02C.cbl, CBACT03C.cbl, CBACT04C.cbl,
      CBCUS01C.cbl, CBEXPORT.cbl, CBIMPORT.cbl,
      CBSTM03A.CBL, CBSTM03B.CBL,
      CBTRN01C.cbl, CBTRN02C.cbl, CBTRN03C.cbl,
      CSUTLDTC.cbl
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

  <key_security_files>
    <file name="COSGN00C.cbl" role="Signon program - authentication entry point">
      <facts>
        <fact>EVALUATE EIBAID dispatches on DFHENTER (process login), DFHPF3 (exit), OTHER (invalid key)</fact>
        <fact>PROCESS-ENTER-KEY validates user ID and password are non-empty via EVALUATE TRUE</fact>
        <fact>User ID and password uppercased via FUNCTION UPPER-CASE before validation</fact>
        <fact>CICS READ DATASET(WS-USRSEC-FILE) INTO(SEC-USER-DATA) RIDFLD(WS-USER-ID) reads USRSEC file</fact>
        <fact>WS-USRSEC-FILE VALUE 'USRSEC  ' defines the VSAM file name</fact>
        <fact>RESP code 0 = user found, 13 = user not found, OTHER = system error</fact>
        <fact>Password comparison: IF SEC-USR-PWD = WS-USER-PWD (plain-text comparison)</fact>
        <fact>On success: MOVE SEC-USR-TYPE TO CDEMO-USER-TYPE, then IF CDEMO-USRTYP-ADMIN routes to COADM01C, ELSE to COMEN01C</fact>
        <fact>On wrong password: 'Wrong Password. Try again ...' message</fact>
        <fact>On user not found (RESP 13): 'User not found. Try again ...' message</fact>
        <fact>No account lockout logic exists -- unlimited login attempts permitted</fact>
        <fact>COPYs: COCOM01Y, COSGN00, COTTL01Y, CSDAT01Y, CSMSG01Y, CSUSR01Y, DFHAID, DFHBMSCA</fact>
      </facts>
    </file>

    <file name="CSUSR01Y.cpy" role="User security record copybook (CORRECT name, NOT COUSR00Y.cpy)">
      <facts>
        <fact>01 SEC-USER-DATA is the top-level record</fact>
        <fact>SEC-USR-ID PIC X(08) -- user identifier</fact>
        <fact>SEC-USR-FNAME PIC X(20) -- first name</fact>
        <fact>SEC-USR-LNAME PIC X(20) -- last name</fact>
        <fact>SEC-USR-PWD PIC X(08) -- password (plain text)</fact>
        <fact>SEC-USR-TYPE PIC X(01) -- user type (A=Admin, U=User)</fact>
        <fact>SEC-USR-FILLER PIC X(23) -- filler to pad record</fact>
        <fact>Total record is 80 bytes (8+20+20+8+1+23=80)</fact>
      </facts>
    </file>

    <file name="COCOM01Y.cpy" role="COMMAREA copybook - session state (48 lines)">
      <facts>
        <fact>01 CARDDEMO-COMMAREA is the top-level record</fact>
        <fact>05 CDEMO-GENERAL-INFO group contains session fields</fact>
        <fact>10 CDEMO-FROM-TRANID PIC X(04) -- originating transaction ID</fact>
        <fact>10 CDEMO-FROM-PROGRAM PIC X(08) -- originating program</fact>
        <fact>10 CDEMO-TO-TRANID PIC X(04) -- target transaction ID</fact>
        <fact>10 CDEMO-TO-PROGRAM PIC X(08) -- target program</fact>
        <fact>10 CDEMO-USER-ID PIC X(08) -- authenticated user ID</fact>
        <fact>10 CDEMO-USER-TYPE PIC X(01) -- user role type</fact>
        <fact>88 CDEMO-USRTYP-ADMIN VALUE 'A' -- admin role condition</fact>
        <fact>88 CDEMO-USRTYP-USER VALUE 'U' -- regular user role condition</fact>
        <fact>10 CDEMO-PGM-CONTEXT PIC 9(01) -- program context flag</fact>
        <fact>88 CDEMO-PGM-ENTER VALUE 0 -- first entry to program</fact>
        <fact>88 CDEMO-PGM-REENTER VALUE 1 -- re-entry to program</fact>
        <fact>05 CDEMO-CUSTOMER-INFO group: CDEMO-CUST-ID PIC 9(09), CDEMO-CUST-FNAME PIC X(25), CDEMO-CUST-MNAME PIC X(25), CDEMO-CUST-LNAME PIC X(25)</fact>
        <fact>05 CDEMO-ACCOUNT-INFO group: CDEMO-ACCT-ID PIC 9(11), CDEMO-ACCT-STATUS PIC X(01)</fact>
        <fact>05 CDEMO-CARD-INFO group: CDEMO-CARD-NUM PIC 9(16)</fact>
        <fact>05 CDEMO-MORE-INFO group: CDEMO-LAST-MAP PIC X(7), CDEMO-LAST-MAPSET PIC X(7)</fact>
        <fact>No CDEMO-USER-FNAME or CDEMO-USER-LNAME fields exist in COMMAREA -- user names are NOT in session state</fact>
      </facts>
    </file>

    <file name="COADM01C.cbl" role="Admin menu program">
      <facts>
        <fact>XCTL target when CDEMO-USRTYP-ADMIN is true in COSGN00C</fact>
        <fact>Does NOT contain explicit CDEMO-USRTYP or CDEMO-USER-TYPE checks in its own code</fact>
        <fact>Authorization is enforced by routing -- only admin users are sent here from signon</fact>
      </facts>
    </file>

    <file name="COMEN01C.cbl" role="Main menu program">
      <facts>
        <fact>XCTL target for regular users from COSGN00C</fact>
        <fact>Contains CDEMO-USRTYP-USER check at line 136</fact>
        <fact>Also contains commented-out MOVE SEC-USR-TYPE TO CDEMO-USER-TYPE at line 182</fact>
      </facts>
    </file>
  </key_security_files>

  <role_check_locations>
    <program name="COSGN00C.cbl" check="IF CDEMO-USRTYP-ADMIN" line="230" purpose="Route admin to COADM01C, else to COMEN01C"/>
    <program name="COMEN01C.cbl" check="IF CDEMO-USRTYP-USER" line="136" purpose="Menu visibility for regular users"/>
    <program name="COACTVWC.cbl" check="SET CDEMO-USRTYP-USER TO TRUE" line="344" purpose="Resets user type context"/>
    <program name="COCRDSLC.cbl" check="SET CDEMO-USRTYP-USER TO TRUE" line="326" purpose="Resets user type context"/>
    <program name="COCRDLIC.cbl" check="SET CDEMO-USRTYP-USER TO TRUE" lines="320,388,466,522,550" purpose="Multiple user type resets"/>
    <program name="COACTUPC.cbl" check="SET CDEMO-USRTYP-USER TO TRUE" line="947" purpose="Resets user type context"/>
    <program name="COCRDUPC.cbl" check="SET CDEMO-USRTYP-USER TO TRUE" line="464" purpose="Resets user type context"/>
    <program name="COUSR01C.cbl" check="commented-out MOVE SEC-USR-TYPE" line="173" purpose="Commented-out user type reference"/>
  </role_check_locations>

  <programs_without_explicit_role_checks>
    COBIL00C.cbl, COBSWAIT.cbl, CORPT00C.cbl, COTRN00C.cbl,
    COTRN01C.cbl, COTRN02C.cbl, COUSR00C.cbl, COUSR02C.cbl, COUSR03C.cbl
  </programs_without_explicit_role_checks>

  <known_hallucinations>
    <hallucination id="H1">COUSR00Y.cpy does NOT exist -- actual user security copybook is CSUSR01Y.cpy. This is the CRITICAL hallucination for RE-007 as it appears in the RE-007 prompt itself.</hallucination>
    <hallucination id="H2">CVCAR00Y.cpy does NOT exist -- actual card copybook is CVCRD01Y.cpy</hallucination>
    <hallucination id="H3">S9(7)V99 COMP-3 for monetary fields is WRONG -- actual PIC is S9(10)V99 (see CVACT01Y.cpy:7)</hallucination>
    <hallucination id="H4">CDEMO-USER-FNAME and CDEMO-USER-LNAME do NOT exist in COCOM01Y.cpy -- the RE-007 prompt claims COMMAREA carries these fields but they are not present</hallucination>
  </known_hallucinations>
</ground_truth>

<validation_methodology>
  <phase name="1_load_security_documentation">
    <description>Read the security model documentation and catalog every verifiable claim</description>
    <steps>
      <step>Read docs/reverse-engineering/05-specialized/SECURITY-MODEL.md</step>
      <step>Extract all authentication flow claims (signon sequence, credential validation steps)</step>
      <step>Extract all copybook references (especially watch for COUSR00Y.cpy vs CSUSR01Y.cpy)</step>
      <step>Extract all COMMAREA session field names and their described purposes</step>
      <step>Extract all role-check claims with program:line references</step>
      <step>Extract all security finding claims and their severity ratings</step>
      <step>Create a claims inventory in .work/reverse-engineering/validation/vl-007/claims-inventory.yaml</step>
    </steps>
  </phase>

  <phase name="2_authentication_flow_verification">
    <description>Verify the documented authentication flow matches actual COSGN00C.cbl code</description>
    <steps>
      <step>Read COSGN00C.cbl and trace the actual authentication logic end-to-end</step>
      <step>Verify EVALUATE EIBAID structure: DFHENTER, DFHPF3, OTHER branches</step>
      <step>Verify PROCESS-ENTER-KEY: input validation via EVALUATE TRUE on USERIDI/PASSWDI</step>
      <step>Verify FUNCTION UPPER-CASE applied to both user ID and password</step>
      <step>Verify CICS READ DATASET against USRSEC file with RIDFLD(WS-USER-ID)</step>
      <step>Verify RESP code handling: 0 (found), 13 (not found), OTHER (error)</step>
      <step>Verify password comparison: IF SEC-USR-PWD = WS-USER-PWD (plain-text)</step>
      <step>Verify post-auth routing: CDEMO-USRTYP-ADMIN to COADM01C, else to COMEN01C via XCTL</step>
      <step>Verify error messages match actual string literals in source</step>
      <step>Confirm absence of account lockout, password hashing, or complexity checks</step>
      <step>Record findings in .work/reverse-engineering/validation/vl-007/auth-flow-findings.yaml</step>
    </steps>
  </phase>

  <phase name="3_copybook_and_field_verification">
    <description>Verify all copybook references and field names against actual source</description>
    <steps>
      <step>CRITICAL: Check if documentation references COUSR00Y.cpy -- this file does NOT exist, actual is CSUSR01Y.cpy</step>
      <step>Read CSUSR01Y.cpy and verify every field name cited in documentation (SEC-USR-ID, SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-PWD, SEC-USR-TYPE, SEC-USR-FILLER)</step>
      <step>Read COCOM01Y.cpy and verify every session field name cited (CDEMO-USER-ID, CDEMO-USER-TYPE, etc.)</step>
      <step>CRITICAL: Check if documentation claims CDEMO-USER-FNAME or CDEMO-USER-LNAME exist in COMMAREA -- they do NOT</step>
      <step>Verify 88-level conditions: CDEMO-USRTYP-ADMIN VALUE 'A', CDEMO-USRTYP-USER VALUE 'U'</step>
      <step>Verify PIC clauses for all cited fields match exactly</step>
      <step>Record findings in .work/reverse-engineering/validation/vl-007/field-findings.yaml</step>
    </steps>
  </phase>

  <phase name="4_authorization_enforcement_verification">
    <description>Verify role-check claims across all 18 online programs</description>
    <steps>
      <step>For each of the 18 online programs, search for CDEMO-USRTYP and CDEMO-USER-TYPE references</step>
      <step>Verify every program:line reference in the documentation against actual source</step>
      <step>Confirm which programs have explicit role checks vs rely on menu-level routing</step>
      <step>Verify programs cited as having no authorization checks truly have none</step>
      <step>Check for SET CDEMO-USRTYP-USER TO TRUE patterns and verify their purpose is correctly described</step>
      <step>Confirm role-based access matrix matches actual code-level enforcement</step>
      <step>Record findings in .work/reverse-engineering/validation/vl-007/authz-findings.yaml</step>
    </steps>
  </phase>

  <phase name="5_security_findings_verification">
    <description>Verify that documented security findings accurately reflect code reality</description>
    <steps>
      <step>Verify password storage finding: confirm plain-text storage in CSUSR01Y.cpy (PIC X(08), no hash)</step>
      <step>Verify session management finding: confirm COMMAREA is not signed or encrypted</step>
      <step>Verify any authorization bypass claims against actual code paths</step>
      <step>Verify PCI-DSS concerns about credit card number display/storage</step>
      <step>Confirm absence of audit logging as documented</step>
      <step>Check that severity ratings are appropriate for each finding</step>
      <step>Verify modernization recommendations are reasonable and specific</step>
      <step>Record findings in .work/reverse-engineering/validation/vl-007/security-findings-check.yaml</step>
    </steps>
  </phase>

  <phase name="6_report_generation">
    <description>Produce the structured validation report</description>
    <output>docs/reverse-engineering/validation/VL-007-security-model-report.md</output>
    <format>
      - Verdict: PASS (100) / FAIL (less than 100)
      - Score breakdown by category (5 categories, weighted)
      - Critical findings (hallucinated copybook names, wrong field names, fabricated auth flow steps)
      - Major findings (missing programs in authorization analysis, incomplete role matrix)
      - Minor findings (formatting, diagram issues)
      - Hallucination inventory table (especially COUSR00Y.cpy, CDEMO-USER-FNAME/LNAME)
      - Completeness gaps table (all 18 online programs checked)
      - Specific remediation recommendations
    </format>
  </phase>
</validation_methodology>

<scoring_rubric>
  <category name="Source Reference Accuracy" weight="35" severity="Critical">
    <description>Every file:line citation verified against actual source — copybook names, program names, line numbers</description>
    <scoring>
      100: All references verified correct, CSUSR01Y.cpy correctly named, all program references valid
      80: 90%+ references correct, no fabricated files, minor line number discrepancies
      60: 80%+ correct, but COUSR00Y.cpy hallucination present (inherited from RE-007 prompt)
      40: Multiple references to non-existent files or fabricated code excerpts
      20: Authentication flow cites code that does not exist in COSGN00C.cbl
      0: Majority of source references unverifiable
    </scoring>
  </category>
  <category name="Factual Accuracy" weight="25" severity="Critical">
    <description>Authentication flow, password handling, role checks, COMMAREA fields match reality</description>
    <scoring>
      100: Auth flow matches COSGN00C.cbl exactly, all field names correct, role checks verified
      80: Auth flow mostly correct, minor description inaccuracies
      60: Auth flow correct but COMMAREA fields include non-existent CDEMO-USER-FNAME/LNAME
      40: Password handling or role-check logic incorrectly described
      20: Authentication flow significantly misrepresents actual code
      0: Security model description is fabricated
    </scoring>
  </category>
  <category name="Completeness" weight="20" severity="Major">
    <description>All 18 online programs analyzed for authorization, all session fields covered, all security concerns documented</description>
    <scoring>
      100: All 18 online programs analyzed, complete role matrix, all COMMAREA fields documented
      80: 15+ programs analyzed, core auth flow complete, most security findings present
      60: 12+ programs analyzed, some programs or findings missing
      40: Fewer than 12 programs analyzed, major gaps in authorization mapping
      20: Only signon program analyzed, no broader authorization coverage
      0: Severely incomplete security analysis
    </scoring>
  </category>
  <category name="Quantitative Accuracy" weight="10" severity="Major">
    <description>Program counts, field counts, record lengths, COMMAREA size correct</description>
    <scoring>
      100: All counts match ground truth exactly (18 online programs, 30 copybooks, 48-line COMMAREA)
      80: Counts within 5% tolerance
      60: Minor discrepancies in non-critical counts
      40: Major count errors (e.g., wrong number of online programs or copybooks)
      20: Multiple count errors
      0: Counts are unreliable
    </scoring>
  </category>
  <category name="Documentation Quality" weight="10" severity="Minor">
    <description>Mermaid diagrams valid, markdown well-formed, security findings clearly structured</description>
    <scoring>
      100: Professional quality, auth flow diagram renders, findings well-organized
      80: Minor formatting issues, diagrams mostly correct
      60: Some broken diagrams, inconsistent formatting
      40: Auth flow diagram does not render, unclear finding descriptions
      20: Poor formatting throughout
      0: Unusable documentation
    </scoring>
  </category>
</scoring_rubric>

<output_specification>
  <report_file>docs/reverse-engineering/validation/VL-007-security-model-report.md</report_file>
  <work_directory>.work/reverse-engineering/validation/vl-007/</work_directory>

  <report_structure>
    # Validation Report: Security Model (RE-007)

    ## Verdict: [PASS|FAIL] -- Score: [NN]/100

    ## Score Breakdown
    [Weighted scoring table]

    ## Critical Findings
    [Hallucinated copybook names (COUSR00Y.cpy), fabricated COMMAREA fields, wrong auth flow steps]

    ## Major Findings
    [Missing programs in authorization analysis, incomplete role matrix, missing security findings]

    ## Minor Findings
    [Formatting issues, diagram problems]

    ## Hallucination Inventory
    [Table of every hallucinated claim -- especially COUSR00Y.cpy, CDEMO-USER-FNAME/LNAME, CVCAR00Y.cpy, S9(7)V99]

    ## Completeness Gaps
    [Table of all 18 online programs with authorization check status]

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
  <principle id="1">The user security copybook is CSUSR01Y.cpy -- any reference to COUSR00Y.cpy is a hallucination and must be flagged as Critical</principle>
  <principle id="2">COSGN00C.cbl is the single authentication entry point -- verify every auth flow claim against this source</principle>
  <principle id="3">COCOM01Y.cpy is exactly 48 lines and defines ALL session fields -- every COMMAREA field name must be verified here</principle>
  <principle id="4">CDEMO-USER-FNAME and CDEMO-USER-LNAME do NOT exist in COCOM01Y.cpy -- flag if documentation claims they do</principle>
  <principle id="5">Password comparison is plain-text (IF SEC-USR-PWD = WS-USER-PWD) -- no hashing, no encryption</principle>
  <principle id="6">Role routing is via XCTL: admin to COADM01C, regular user to COMEN01C -- verify this is correctly documented</principle>
  <principle id="7">Authorization enforcement varies by program -- some have explicit checks, some rely on menu routing</principle>
  <principle id="8">SET CDEMO-USRTYP-USER TO TRUE patterns in several programs reset context, not enforce authorization</principle>
</foundational_principles>

<critical_reminders>
  <reminder id="1">CSUSR01Y.cpy is the CORRECT user security copybook. COUSR00Y.cpy does NOT exist. This is the single most important hallucination to detect in RE-007 output because the RE-007 prompt itself uses the wrong name.</reminder>
  <reminder id="2">COCOM01Y.cpy does NOT contain CDEMO-USER-FNAME or CDEMO-USER-LNAME. The RE-007 prompt claims COMMAREA carries these fields but they are absent from the actual copybook. Customer name fields (CDEMO-CUST-FNAME etc.) exist but are different.</reminder>
  <reminder id="3">The authentication flow in COSGN00C.cbl follows: EVALUATE EIBAID -> PROCESS-ENTER-KEY -> EVALUATE TRUE (input validation) -> FUNCTION UPPER-CASE -> READ-USER-SEC-FILE -> EVALUATE WS-RESP-CD -> password comparison -> XCTL routing. Verify the documentation matches this exact sequence.</reminder>
  <reminder id="4">COSGN00C.cbl COPYs CSUSR01Y (line 55), NOT COUSR00Y. Read line 55 of COSGN00C.cbl to confirm.</reminder>
  <reminder id="5">Only 8 of 18 online programs contain any CDEMO-USRTYP or CDEMO-USER-TYPE references. Nine programs have no explicit role checks at all.</reminder>
  <reminder id="6">The SEC-USER-DATA record in CSUSR01Y.cpy is 80 bytes: 8+20+20+8+1+23. Verify any record length claims.</reminder>
  <reminder id="7">COADM01C.cbl does NOT contain explicit CDEMO-USRTYP checks -- it relies on being the XCTL target only for admin users from COSGN00C.</reminder>
  <reminder id="8">Error messages in COSGN00C.cbl are specific string literals: 'Wrong Password. Try again ...', 'User not found. Try again ...', 'Unable to verify the User ...' -- verify if documented.</reminder>
  <reminder id="9">There is NO account lockout, NO password complexity enforcement, NO session timeout logic, and NO audit logging in the application code. These are valid security findings but must be described as absences, not as implemented features.</reminder>
  <reminder id="10">Batch programs (CB* prefix) have a completely different security context from online programs -- they do not use CICS signon or COMMAREA session state. Verify this distinction is correctly documented.</reminder>
</critical_reminders>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/validation/vl-007/</path>
    <purpose>Persist validation progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
      validation_phase: "load_docs|auth_flow_verify|field_verify|authz_verify|findings_verify|reporting"
      current_document: "SECURITY-MODEL.md"
      claims_verified: N
      claims_remaining: N
      findings:
        critical: N
        major: N
        minor: N
      hallucinations_detected:
        - "COUSR00Y.cpy"
        - "CDEMO-USER-FNAME"
      next_action: "Detailed description of next step"
      last_updated: "ISO timestamp"
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/validation/vl-007/progress.yaml if it exists</step>
    <step>2. Load any existing findings from the work directory (auth-flow-findings.yaml, field-findings.yaml, etc.)</step>
    <step>3. Continue from next_action without re-validating completed phases</step>
    <step>4. Update progress.yaml after each phase completion</step>
  </resumption_protocol>
</context_compaction_survival>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/validation/vl-007/progress.yaml if it exists
    2. If progress exists and validation_phase != "complete":
       - Load existing findings
       - Resume from next_action
    3. If no progress or starting fresh:
       - Begin with Phase 1: Load Security Documentation
       - Create initial progress.yaml
    4. CRITICAL FIRST CHECK: Scan documentation for COUSR00Y.cpy references -- flag immediately
    5. CRITICAL SECOND CHECK: Scan for CDEMO-USER-FNAME/CDEMO-USER-LNAME claims -- flag immediately
    6. Verify authentication flow against COSGN00C.cbl line by line
    7. Verify every COMMAREA field name against COCOM01Y.cpy
    8. Check all 18 online programs for authorization enforcement
    9. Verify security findings against actual code patterns
    10. Write final report to docs/reverse-engineering/validation/VL-007-security-model-report.md
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to validate the RE-007 security model documentation. The prompt will:

1. **Load** the security model documentation and catalog every verifiable claim
2. **Verify** the authentication flow against actual COSGN00C.cbl code line by line
3. **Check** copybook references, flagging COUSR00Y.cpy as a hallucination if present
4. **Confirm** all COMMAREA session field names exist in COCOM01Y.cpy
5. **Audit** all 18 online programs for authorization enforcement presence or absence
6. **Validate** security findings against actual code patterns
7. **Report** findings with severity ratings and remediation guidance

## Expected Output

| File | Description |
|------|-------------|
| `VL-007-security-model-report.md` | Structured validation report with verdict, score, findings, and hallucination inventory |

## Prerequisites

- RE-007 must have been executed and output exists in `docs/reverse-engineering/05-specialized/SECURITY-MODEL.md`
- Access to source code in `app/cbl/`, `app/cpy/`
- VL-001 and VL-002 should have completed (establishes baseline copybook and entity accuracy)

## Depends On

- VL-001 (Domain Model Validation)
- VL-002 (Data Model Validation)

## Blocks

- VL-000 (Cross-Document Consistency)
