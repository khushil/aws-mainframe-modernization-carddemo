# VL-003: Context Model Validation

## Prompt

```xml
<context>
  <project>CardDemo - AWS mainframe credit card processing demonstration application</project>

  <role>
    <persona>Validation Analyst specializing in context mapping and COMMAREA verification for CICS-based mainframe applications</persona>

    <validation_expertise>
      <skill>COMMAREA field-by-field verification: cross-referencing every documented field against the actual COCOM01Y.cpy copybook to detect fabricated or misnamed fields</skill>
      <skill>CICS pseudo-conversational flow verification: tracing RETURN TRANSID and XCTL/LINK statements in source code to confirm documented navigation paths</skill>
      <skill>Bounded context boundary validation: verifying that documented context groupings match actual program clustering based on data ownership and shared copybooks</skill>
      <skill>Transaction ID verification: confirming every documented CICS transaction ID exists in actual EXEC CICS RETURN TRANSID statements in source code</skill>
      <skill>DDD pattern validation: ensuring documented context relationships (Shared Kernel, Customer/Supplier, Published Language, Separate Ways) reflect actual code dependencies</skill>
      <skill>Navigation graph completeness: verifying every XCTL and LINK call in all 18 online programs appears in the documented navigation flows</skill>
      <skill>Hallucination detection: systematically searching for references to files, fields, copybooks, or transaction IDs that do not exist in the source tree</skill>
      <skill>State contract verification: confirming COMMAREA field PICs, levels, and 88-level conditions match the actual COCOM01Y.cpy exactly</skill>
    </validation_expertise>

    <mainframe_expertise>
      <skill>CICS pseudo-conversational pattern mastery: RETURN TRANSID sets next transaction, EIBCALEN=0 detects first invocation</skill>
      <skill>COMMAREA architecture: DFHCOMMAREA is the canonical state carrier between all CICS program invocations</skill>
      <skill>XCTL vs LINK semantics: XCTL replaces current program (no return), LINK calls subroutine and returns</skill>
      <skill>COBOL copybook analysis: 01/05/10 level structures, PIC clauses, 88-level condition names</skill>
      <skill>BMS mapset correlation: connecting screen definitions to their COBOL program handlers</skill>
      <skill>VSAM file access patterns: READ vs WRITE/REWRITE indicates data ownership boundaries</skill>
    </mainframe_expertise>

    <mindset>
      Every claim in the context model documentation must be traceable to actual source code. COMMAREA
      fields must match COCOM01Y.cpy exactly — no fabricated fields, no wrong PIC clauses, no invented
      level numbers. Navigation flows must correspond to real XCTL/LINK/RETURN TRANSID statements found
      in the 18 online programs. Bounded contexts must reflect actual program groupings based on verifiable
      data ownership and copybook sharing patterns, not theoretical DDD ideals.
    </mindset>
  </role>

  <objective>
    <primary_goal>
      Validate the accuracy, completeness, and factual correctness of the RE-003 Context Model output
      by cross-referencing every claim against the actual CardDemo source code. Detect hallucinations,
      verify COMMAREA field specifications, confirm navigation flows, and assess bounded context validity.
    </primary_goal>

    <validation_scope>
      <document>docs/reverse-engineering/03-context-model/CONTEXT-MAP.md</document>
      <document>docs/reverse-engineering/03-context-model/COMMAREA-SPECIFICATION.md</document>
      <document>docs/reverse-engineering/03-context-model/NAVIGATION-FLOWS.md</document>
    </validation_scope>

    <critical_source_files>
      <file significance="THE central artifact — every COMMAREA field must be verified against this 48-line file">app/cpy/COCOM01Y.cpy</file>
      <file significance="Sign-on program — entry point, RETURN TRANSID, session establishment">app/cbl/COSGN00C.cbl</file>
      <file significance="Admin menu — XCTL routing to functional programs">app/cbl/COADM01C.cbl</file>
      <file significance="Main menu — XCTL routing to functional programs">app/cbl/COMEN01C.cbl</file>
      <file significance="Account update — XCTL/RETURN TRANSID patterns">app/cbl/COACTUPC.cbl</file>
      <file significance="Account view — XCTL/RETURN TRANSID patterns">app/cbl/COACTVWC.cbl</file>
      <file significance="Card list — XCTL/RETURN TRANSID patterns">app/cbl/COCRDLIC.cbl</file>
      <file significance="Card search — XCTL/RETURN TRANSID patterns">app/cbl/COCRDSLC.cbl</file>
      <file significance="Card update — XCTL/RETURN TRANSID patterns">app/cbl/COCRDUPC.cbl</file>
      <file significance="Bill payment — XCTL/RETURN TRANSID patterns">app/cbl/COBIL00C.cbl</file>
      <file significance="Transaction list — XCTL/RETURN TRANSID patterns">app/cbl/COTRN00C.cbl</file>
      <file significance="Transaction add — XCTL/RETURN TRANSID patterns">app/cbl/COTRN01C.cbl</file>
      <file significance="Transaction detail — XCTL/RETURN TRANSID patterns">app/cbl/COTRN02C.cbl</file>
      <file significance="User list — XCTL/RETURN TRANSID patterns">app/cbl/COUSR00C.cbl</file>
      <file significance="User add — XCTL/RETURN TRANSID patterns">app/cbl/COUSR01C.cbl</file>
      <file significance="User update — XCTL/RETURN TRANSID patterns">app/cbl/COUSR02C.cbl</file>
      <file significance="User delete — XCTL/RETURN TRANSID patterns">app/cbl/COUSR03C.cbl</file>
      <file significance="Report program — XCTL/RETURN TRANSID patterns">app/cbl/CORPT00C.cbl</file>
      <file significance="Batch wait utility — XCTL/RETURN TRANSID patterns">app/cbl/COBSWAIT.cbl</file>
    </critical_source_files>

    <output_report>docs/reverse-engineering/validation/VL-003-context-model-report.md</output_report>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<ground_truth>
  <description>
    Canonical inventory verified directly from the source tree. Every count, filename, and field
    specification below is authoritative. Use this as the single source of truth when validating
    the RE-003 context model documentation.
  </description>

  <file_inventory>
    <cobol_programs count="31" location="app/cbl/">
      <online_programs count="18">
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
      </online_programs>
      <batch_programs count="12">
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
      <utility_programs count="1">
        <program note="Date utility — not an online transaction program">CSUTLDTC.cbl</program>
      </utility_programs>
    </cobol_programs>

    <copybooks count="30" location="app/cpy/">
      <file>COADM02Y.cpy</file>
      <file>COCOM01Y.cpy</file>
      <file>CODATECN.cpy</file>
      <file>COMEN02Y.cpy</file>
      <file>COSTM01.CPY</file>
      <file>COTTL01Y.cpy</file>
      <file>CSDAT01Y.cpy</file>
      <file>CSLKPCDY.cpy</file>
      <file>CSMSG01Y.cpy</file>
      <file>CSMSG02Y.cpy</file>
      <file>CSSETATY.cpy</file>
      <file>CSSTRPFY.cpy</file>
      <file>CSUSR01Y.cpy</file>
      <file>CSUTLDPY.cpy</file>
      <file>CSUTLDWY.cpy</file>
      <file>CUSTREC.cpy</file>
      <file>CVACT01Y.cpy</file>
      <file>CVACT02Y.cpy</file>
      <file>CVACT03Y.cpy</file>
      <file>CVCRD01Y.cpy</file>
      <file>CVCUS01Y.cpy</file>
      <file>CVEXPORT.cpy</file>
      <file>CVTRA01Y.cpy</file>
      <file>CVTRA02Y.cpy</file>
      <file>CVTRA03Y.cpy</file>
      <file>CVTRA04Y.cpy</file>
      <file>CVTRA05Y.cpy</file>
      <file>CVTRA06Y.cpy</file>
      <file>CVTRA07Y.cpy</file>
      <file>UNUSED1Y.cpy</file>
    </copybooks>

    <bms_mapsets count="17" location="app/bms/">
      <file>COACTUP.bms</file>
      <file>COACTVW.bms</file>
      <file>COADM01.bms</file>
      <file>COBIL00.bms</file>
      <file>COCRDLI.bms</file>
      <file>COCRDSL.bms</file>
      <file>COCRDUP.bms</file>
      <file>COMEN01.bms</file>
      <file>CORPT00.bms</file>
      <file>COSGN00.bms</file>
      <file>COTRN00.bms</file>
      <file>COTRN01.bms</file>
      <file>COTRN02.bms</file>
      <file>COUSR00.bms</file>
      <file>COUSR01.bms</file>
      <file>COUSR02.bms</file>
      <file>COUSR03.bms</file>
    </bms_mapsets>

    <bms_copybooks count="17" location="app/cpy-bms/">
      <file>COACTUP.CPY</file>
      <file>COACTVW.CPY</file>
      <file>COADM01.CPY</file>
      <file>COBIL00.CPY</file>
      <file>COCRDLI.CPY</file>
      <file>COCRDSL.CPY</file>
      <file>COCRDUP.CPY</file>
      <file>COMEN01.CPY</file>
      <file>CORPT00.CPY</file>
      <file>COSGN00.CPY</file>
      <file>COTRN00.CPY</file>
      <file>COTRN01.CPY</file>
      <file>COTRN02.CPY</file>
      <file>COUSR00.CPY</file>
      <file>COUSR01.CPY</file>
      <file>COUSR02.CPY</file>
      <file>COUSR03.CPY</file>
    </bms_copybooks>

    <jcl_files count="38" location="app/jcl/">
      <file>ACCTFILE.jcl</file>
      <file>CARDFILE.jcl</file>
      <file>CBADMCDJ.jcl</file>
      <file>CBEXPORT.jcl</file>
      <file>CBIMPORT.jcl</file>
      <file>CLOSEFIL.jcl</file>
      <file>COMBTRAN.jcl</file>
      <file>CREASTMT.JCL</file>
      <file>CUSTFILE.jcl</file>
      <file>DALYREJS.jcl</file>
      <file>DEFCUST.jcl</file>
      <file>DEFGDGB.jcl</file>
      <file>DEFGDGD.jcl</file>
      <file>DISCGRP.jcl</file>
      <file>DUSRSECJ.jcl</file>
      <file>ESDSRRDS.jcl</file>
      <file>FTPJCL.JCL</file>
      <file>INTCALC.jcl</file>
      <file>INTRDRJ1.JCL</file>
      <file>INTRDRJ2.JCL</file>
      <file>OPENFIL.jcl</file>
      <file>POSTTRAN.jcl</file>
      <file>PRTCATBL.jcl</file>
      <file>READACCT.jcl</file>
      <file>READCARD.jcl</file>
      <file>READCUST.jcl</file>
      <file>READXREF.jcl</file>
      <file>REPTFILE.jcl</file>
      <file>TCATBALF.jcl</file>
      <file>TRANBKP.jcl</file>
      <file>TRANCATG.jcl</file>
      <file>TRANFILE.jcl</file>
      <file>TRANIDX.jcl</file>
      <file>TRANREPT.jcl</file>
      <file>TRANTYPE.jcl</file>
      <file>TXT2PDF1.JCL</file>
      <file>WAITSTEP.jcl</file>
      <file>XREFFILE.jcl</file>
    </jcl_files>
  </file_inventory>

  <commarea_ground_truth>
    <description>
      COCOM01Y.cpy is exactly 48 lines (including comments and blank lines). The data structure
      below represents the COMPLETE and EXACT COMMAREA definition. Every field, level, PIC clause,
      and 88-level condition must match this precisely. Any deviation in documentation is an error.
    </description>
    <structure>
      <level_01 name="CARDDEMO-COMMAREA">
        <level_05 name="CDEMO-GENERAL-INFO">
          <level_10 name="CDEMO-FROM-TRANID" pic="X(04)"/>
          <level_10 name="CDEMO-FROM-PROGRAM" pic="X(08)"/>
          <level_10 name="CDEMO-TO-TRANID" pic="X(04)"/>
          <level_10 name="CDEMO-TO-PROGRAM" pic="X(08)"/>
          <level_10 name="CDEMO-USER-ID" pic="X(08)"/>
          <level_10 name="CDEMO-USER-TYPE" pic="X(01)">
            <level_88 name="CDEMO-USRTYP-ADMIN" value="A"/>
            <level_88 name="CDEMO-USRTYP-USER" value="U"/>
          </level_10>
          <level_10 name="CDEMO-PGM-CONTEXT" pic="9(01)">
            <level_88 name="CDEMO-PGM-ENTER" value="0"/>
            <level_88 name="CDEMO-PGM-REENTER" value="1"/>
          </level_10>
        </level_05>
        <level_05 name="CDEMO-CUSTOMER-INFO">
          <level_10 name="CDEMO-CUST-ID" pic="9(09)"/>
          <level_10 name="CDEMO-CUST-FNAME" pic="X(25)"/>
          <level_10 name="CDEMO-CUST-MNAME" pic="X(25)"/>
          <level_10 name="CDEMO-CUST-LNAME" pic="X(25)"/>
        </level_05>
        <level_05 name="CDEMO-ACCOUNT-INFO">
          <level_10 name="CDEMO-ACCT-ID" pic="9(11)"/>
          <level_10 name="CDEMO-ACCT-STATUS" pic="X(01)"/>
        </level_05>
        <level_05 name="CDEMO-CARD-INFO">
          <level_10 name="CDEMO-CARD-NUM" pic="9(16)"/>
        </level_05>
        <level_05 name="CDEMO-MORE-INFO">
          <level_10 name="CDEMO-LAST-MAP" pic="X(7)"/>
          <level_10 name="CDEMO-LAST-MAPSET" pic="X(7)"/>
        </level_05>
      </level_01>
    </structure>
    <field_count>
      <total_level_10_fields>14</total_level_10_fields>
      <total_88_level_conditions>4</total_88_level_conditions>
      <total_level_05_groups>5</total_level_05_groups>
    </field_count>
    <critical_absences>
      <absence>There is NO CDEMO-MSG-TEXT field in COCOM01Y.cpy — if documentation references this field, it is fabricated or located in a different copybook</absence>
      <absence>There is NO CDEMO-SCR-NAME field in COCOM01Y.cpy — CDEMO-LAST-MAP and CDEMO-LAST-MAPSET serve the screen tracking function</absence>
      <absence>There is NO CDEMO-PGM-NAME field in COCOM01Y.cpy — CDEMO-FROM-PROGRAM and CDEMO-TO-PROGRAM serve the program tracking function</absence>
      <absence>There is NO action code or operation result field in COCOM01Y.cpy — if documented, verify the actual source</absence>
    </critical_absences>
  </commarea_ground_truth>

  <known_hallucinations>
    <hallucination id="1">
      <claim>CVCAR00Y.cpy referenced as card copybook</claim>
      <reality>Actual file is CVCRD01Y.cpy — CVCAR00Y.cpy does not exist in app/cpy/</reality>
      <affected_prompts>RE-001, RE-002, RE-003</affected_prompts>
    </hallucination>
    <hallucination id="2">
      <claim>COUSR00Y.cpy referenced as user copybook</claim>
      <reality>Actual file is CSUSR01Y.cpy — COUSR00Y.cpy does not exist in app/cpy/</reality>
      <affected_prompts>RE-001, RE-002, RE-003, RE-007</affected_prompts>
    </hallucination>
    <hallucination id="3">
      <claim>S9(7)V99 COMP-3 cited for monetary fields</claim>
      <reality>Actual PIC is S9(10)V99 (see CVACT01Y.cpy:7)</reality>
      <affected_prompts>RE-001, RE-002, RE-003, RE-009</affected_prompts>
    </hallucination>
    <hallucination id="4">
      <claim>39 COBOL programs</claim>
      <reality>Actual count is 31 (29 .cbl + 2 .CBL)</reality>
      <affected_prompts>RE-001, CLAUDE.md</affected_prompts>
    </hallucination>
    <hallucination id="5">
      <claim>41 copybooks</claim>
      <reality>Actual count is 30 (29 .cpy + 1 .CPY)</reality>
      <affected_prompts>RE-001, CLAUDE.md</affected_prompts>
    </hallucination>
    <hallucination id="6">
      <claim>21 BMS mapsets</claim>
      <reality>Actual count is 17</reality>
      <affected_prompts>CLAUDE.md</affected_prompts>
    </hallucination>
    <hallucination id="7">
      <claim>CDEMO-PGM-NAME as a COMMAREA field</claim>
      <reality>No such field exists in COCOM01Y.cpy — actual fields are CDEMO-FROM-PROGRAM and CDEMO-TO-PROGRAM</reality>
      <affected_prompts>RE-003</affected_prompts>
    </hallucination>
    <hallucination id="8">
      <claim>CDEMO-SCR-NAME as a COMMAREA field</claim>
      <reality>No such field exists in COCOM01Y.cpy — actual fields are CDEMO-LAST-MAP and CDEMO-LAST-MAPSET</reality>
      <affected_prompts>RE-003</affected_prompts>
    </hallucination>
    <hallucination id="9">
      <claim>CDEMO-MSG-TEXT as a COMMAREA field</claim>
      <reality>No such field exists in COCOM01Y.cpy — message handling may use a different copybook or WORKING-STORAGE</reality>
      <affected_prompts>RE-003</affected_prompts>
    </hallucination>
  </known_hallucinations>

  <expected_bounded_contexts>
    <description>
      The RE-003 documentation should identify 6-8 bounded contexts. The following are the expected
      contexts based on actual program groupings and data ownership patterns in the source code.
    </description>
    <context name="Authentication" programs="COSGN00C" data_owned="USRSEC" note="Sign-on and session establishment"/>
    <context name="Account Management" programs="COACTVWC, COACTUPC" data_owned="ACCTDAT" note="Account viewing and updating"/>
    <context name="Card Management" programs="COCRDSLC, COCRDUPC, COCRDLIC" data_owned="CARDDAT, CCXREF" note="Card search, update, and listing"/>
    <context name="Transaction Processing" programs="COTRN00C, COTRN01C, COTRN02C" data_owned="TRANSACT" note="Transaction viewing, adding, detail"/>
    <context name="Bill Payment" programs="COBIL00C" data_owned="None (modifies ACCTDAT)" note="Payment processing — downstream of Account"/>
    <context name="User Administration" programs="COUSR00C, COUSR01C, COUSR02C, COUSR03C" data_owned="USRSEC" note="Admin-only user CRUD"/>
    <context name="Batch Processing" programs="CBACT01C-04C, CBCUS01C, CBTRN01C-03C, CBEXPORT, CBIMPORT, CBSTM03A, CBSTM03B" data_owned="All files (batch mode)" note="Nightly batch — Separate Ways from online"/>
  </expected_bounded_contexts>

  <navigation_infrastructure>
    <description>
      Menu programs are routing infrastructure, not bounded contexts. They should be documented
      as navigation/routing layer, not as their own bounded contexts.
    </description>
    <menu_program name="COADM01C" role="Admin menu — routes to functional programs via XCTL"/>
    <menu_program name="COMEN01C" role="Main menu — routes to functional programs via XCTL"/>
    <utility_program name="CSUTLDTC" role="Date utility — called via LINK, not a transaction program"/>
    <utility_program name="COBSWAIT" role="Batch wait — not a standard online transaction"/>
    <utility_program name="CORPT00C" role="Report program — may be a stub or reporting function"/>
  </navigation_infrastructure>
</ground_truth>

<foundational_principles>
  <principle id="1">COCOM01Y.cpy is the ONLY source of truth for COMMAREA fields — every field name, PIC clause, level number, and 88-level condition in the documentation must match this file exactly</principle>
  <principle id="2">COMMAREA has exactly 14 level-10 fields, 5 level-05 groups, and 4 level-88 conditions — any documentation claiming more or fewer is wrong</principle>
  <principle id="3">Navigation flows must be verified by reading actual EXEC CICS XCTL, EXEC CICS LINK, and EXEC CICS RETURN TRANSID statements in the 18 online programs</principle>
  <principle id="4">Transaction IDs must appear in actual RETURN TRANSID statements — do not accept transaction IDs that cannot be found in source code</principle>
  <principle id="5">Bounded context assignments must be justified by actual data ownership (WRITE/REWRITE access to VSAM files) and copybook sharing patterns, not by naming conventions alone</principle>
  <principle id="6">Context relationship types (Shared Kernel, Customer/Supplier, Published Language, Separate Ways) must be verifiable from actual code dependencies, not theoretical DDD patterns</principle>
  <principle id="7">Every file reference in documentation must correspond to an actual file in the source tree — references to non-existent files are Critical severity hallucinations</principle>
  <principle id="8">The RE-003 prompt itself contains known inaccuracies (CDEMO-PGM-NAME, CDEMO-SCR-NAME, CDEMO-MSG-TEXT) — if documentation perpetuates these, they must be flagged as hallucinations inherited from the prompt</principle>
</foundational_principles>

<validation_methodology>
  <phase name="1_document_inventory">
    <description>Verify all expected RE-003 output documents exist and have substantive content</description>
    <steps>
      <step>Check that docs/reverse-engineering/03-context-model/CONTEXT-MAP.md exists and is non-empty</step>
      <step>Check that docs/reverse-engineering/03-context-model/COMMAREA-SPECIFICATION.md exists and is non-empty</step>
      <step>Check that docs/reverse-engineering/03-context-model/NAVIGATION-FLOWS.md exists and is non-empty</step>
      <step>Record any missing documents as Critical findings (entire validation section cannot be scored)</step>
      <step>Note document sizes — extremely short documents suggest incomplete analysis</step>
    </steps>
    <output_artifact>.work/reverse-engineering/validation/vl-003/document-inventory.yaml</output_artifact>
  </phase>

  <phase name="2_commarea_field_verification">
    <description>
      Field-by-field verification of COMMAREA-SPECIFICATION.md against COCOM01Y.cpy.
      This is the highest-priority validation phase because COMMAREA is the central state contract.
    </description>
    <steps>
      <step>Read COCOM01Y.cpy completely (48 lines) and build a canonical field list</step>
      <step>For every field documented in COMMAREA-SPECIFICATION.md, verify:
        - Field name matches exactly (case-sensitive for COBOL)
        - Level number is correct (01, 05, 10, 88)
        - PIC clause matches exactly (e.g., PIC X(04), not PIC X(4))
        - 88-level condition names and VALUES match
        - Group hierarchy is correct (which 05-level contains which 10-level fields)</step>
      <step>Search documentation for COMMAREA fields that do NOT exist in COCOM01Y.cpy:
        - CDEMO-PGM-NAME (does not exist — should be CDEMO-FROM-PROGRAM / CDEMO-TO-PROGRAM)
        - CDEMO-SCR-NAME (does not exist — should be CDEMO-LAST-MAP / CDEMO-LAST-MAPSET)
        - CDEMO-MSG-TEXT (does not exist in COCOM01Y.cpy)
        - Any other fabricated field names</step>
      <step>Verify that documentation accounts for ALL 14 level-10 fields — not fewer, not more</step>
      <step>Check PIC clause accuracy for each field:
        - CDEMO-FROM-TRANID must be PIC X(04), not PIC X(4) or PIC 9(04)
        - CDEMO-ACCT-ID must be PIC 9(11), not PIC 9(09) or PIC X(11)
        - CDEMO-CARD-NUM must be PIC 9(16), not PIC X(16)
        - CDEMO-CUST-ID must be PIC 9(09), not PIC X(09)
        - CDEMO-LAST-MAP must be PIC X(7), not PIC X(07) or PIC X(8)</step>
      <step>Verify 88-level conditions:
        - CDEMO-USRTYP-ADMIN VALUE 'A' (not 'ADMIN' or '1')
        - CDEMO-USRTYP-USER VALUE 'U' (not 'USER' or '0')
        - CDEMO-PGM-ENTER VALUE 0 (not '0' or ZERO)
        - CDEMO-PGM-REENTER VALUE 1 (not '1' or 2)</step>
      <step>Record every discrepancy with severity classification:
        - Critical: fabricated field, wrong PIC clause
        - Major: missing field, wrong level number
        - Minor: formatting inconsistency</step>
    </steps>
    <output_artifact>.work/reverse-engineering/validation/vl-003/commarea-verification.yaml</output_artifact>
  </phase>

  <phase name="3_transaction_id_verification">
    <description>
      Verify every documented CICS transaction ID against actual RETURN TRANSID statements
      in the 18 online programs.
    </description>
    <steps>
      <step>Search all 18 online programs for EXEC CICS RETURN TRANSID statements</step>
      <step>Extract the actual transaction IDs used (e.g., TRANSID('CACU'), TRANSID('CACT'))</step>
      <step>Build canonical list of verified transaction IDs with their source program and line number</step>
      <step>For every transaction ID in the documentation, verify it exists in the canonical list</step>
      <step>For every transaction ID in the canonical list, verify it appears in the documentation</step>
      <step>Check transaction-to-program mappings:
        - Does the documentation correctly map each transaction ID to its handling program?
        - Are there any incorrect program assignments?</step>
      <step>Flag any documented transaction IDs not found in source as Critical hallucinations</step>
      <step>Flag any source transaction IDs missing from documentation as Major completeness gaps</step>
    </steps>
    <output_artifact>.work/reverse-engineering/validation/vl-003/transaction-verification.yaml</output_artifact>
  </phase>

  <phase name="4_navigation_flow_verification">
    <description>
      Verify documented navigation flows against actual XCTL and LINK statements in all 18 online programs.
    </description>
    <steps>
      <step>Read each of the 18 online programs and extract all EXEC CICS XCTL and EXEC CICS LINK statements</step>
      <step>For each program, record:
        - Target program name in XCTL/LINK calls
        - COMMAREA passed (yes/no, length if specified)
        - Condition under which the transfer occurs (from surrounding IF/EVALUATE logic)</step>
      <step>Build a canonical navigation graph: source_program -> target_program (via XCTL or LINK)</step>
      <step>Compare documented navigation flows in NAVIGATION-FLOWS.md against the canonical graph:
        - Every documented path must correspond to an actual XCTL or LINK statement
        - Every actual XCTL or LINK must appear in the documented flows
        - The direction (source -> target) must be correct</step>
      <step>Verify menu program routing:
        - COADM01C: extract all XCTL targets from its EVALUATE structure
        - COMEN01C: extract all XCTL targets from its EVALUATE structure
        - Ensure all menu options are documented</step>
      <step>Verify sign-on flow:
        - COSGN00C -> COADM01C (admin) or COMEN01C (regular user) via XCTL
        - Confirm this matches documentation</step>
      <step>Check for fabricated navigation paths:
        - Paths that have no corresponding XCTL/LINK in source code
        - Programs connected in documentation that never reference each other</step>
      <step>Verify Mermaid diagram accuracy if navigation diagrams are present:
        - Node names match actual program names
        - Edge directions match actual call directions
        - No phantom nodes representing non-existent programs</step>
    </steps>
    <output_artifact>.work/reverse-engineering/validation/vl-003/navigation-verification.yaml</output_artifact>
  </phase>

  <phase name="5_bounded_context_verification">
    <description>
      Verify documented bounded contexts against actual program groupings, data ownership,
      and copybook sharing patterns.
    </description>
    <steps>
      <step>For each documented bounded context, verify:
        - All listed programs actually exist in app/cbl/
        - Program assignments make sense based on actual functionality
        - Data ownership claims match actual file access patterns (READ vs WRITE/REWRITE)</step>
      <step>Verify data ownership by searching each online program for EXEC CICS READ, WRITE, REWRITE, DELETE statements:
        - Which programs WRITE to ACCTDAT? (Account Management context owners)
        - Which programs WRITE to CARDDAT? (Card Management context owners)
        - Which programs WRITE to TRANSACT? (Transaction Processing context owners)
        - Which programs WRITE to USRSEC? (Authentication/User Admin context owners)
        - Which programs only READ from files they don't own? (cross-context dependencies)</step>
      <step>Verify context count: expect 6-8 bounded contexts
        - Fewer than 6 suggests contexts were merged incorrectly
        - More than 8 suggests over-decomposition or menu programs treated as contexts</step>
      <step>Verify that menu programs (COADM01C, COMEN01C) are NOT listed as their own bounded context
        - They are navigation infrastructure, not business contexts</step>
      <step>Verify that all 18 online programs appear in at least one context or are documented as infrastructure/utility</step>
      <step>Verify context relationship types:
        - Shared Kernel: check that claimed shared copybooks are actually used by multiple contexts
        - Customer/Supplier: check that data flow direction matches actual file access patterns
        - Published Language: COMMAREA (COCOM01Y.cpy) should be identified as published language
        - Separate Ways: batch programs should be documented as separate from online contexts</step>
      <step>Cross-reference with VL-001 domain model findings (if available) for entity-to-context alignment</step>
      <step>Cross-reference with VL-002 data model findings (if available) for file-to-context alignment</step>
    </steps>
    <output_artifact>.work/reverse-engineering/validation/vl-003/context-verification.yaml</output_artifact>
  </phase>

  <phase name="6_report_generation">
    <description>Compile all findings into the validation report</description>
    <steps>
      <step>Calculate scores for each category based on findings:
        - Source Reference Accuracy (35%): percentage of file:line citations that are verifiable
        - Factual Accuracy (25%): percentage of COMMAREA fields, transaction IDs, and navigation paths that are correct
        - Completeness (20%): percentage of expected items (14 COMMAREA fields, 18 programs, 6-8 contexts) documented
        - Quantitative Accuracy (10%): file counts, field counts, context counts correct
        - Documentation Quality (10%): Mermaid syntax validity, markdown formatting, clarity</step>
      <step>Classify all findings by severity:
        - Critical: fabricated COMMAREA fields, non-existent files, wrong PIC clauses, phantom navigation paths
        - Major: missing programs in context mapping, undocumented transaction IDs, incomplete COMMAREA coverage
        - Minor: formatting issues, Mermaid rendering problems, unclear descriptions</step>
      <step>Build hallucination inventory table with columns:
        - Claim in documentation
        - Reality from source code
        - Source file:line reference
        - Severity</step>
      <step>Build completeness gaps table with columns:
        - Expected item
        - Status (Present/Missing/Partial)
        - Notes</step>
      <step>Determine verdict:
        - PASS (100): documentation is fully accurate and reliable for downstream use
        - FAIL (less than 100): documentation contains errors and must be remediated or regenerated</step>
      <step>Write specific, actionable recommendations for remediation</step>
      <step>Write report to docs/reverse-engineering/validation/VL-003-context-model-report.md</step>
    </steps>
    <output_artifact>docs/reverse-engineering/validation/VL-003-context-model-report.md</output_artifact>
  </phase>
</validation_methodology>

<scoring_rubric>
  <category name="Source Reference Accuracy" weight="35" severity="Critical">
    <description>Every file:line citation in the context model documentation is verified against actual source code</description>
    <checks>
      <check>Every COCOM01Y.cpy field reference points to actual fields in the file</check>
      <check>Every program name referenced in navigation flows exists in app/cbl/</check>
      <check>Every copybook referenced in context analysis exists in app/cpy/ or app/cpy-bms/</check>
      <check>Every XCTL/LINK target mentioned is verified in the calling program source</check>
      <check>Every RETURN TRANSID value cited appears in the referenced program</check>
      <check>Every VSAM file name referenced in data ownership maps corresponds to actual CICS file definitions</check>
      <check>No references to CVCAR00Y.cpy (hallucination — actual file is CVCRD01Y.cpy)</check>
      <check>No references to COUSR00Y.cpy (hallucination — actual file is CSUSR01Y.cpy)</check>
    </checks>
    <scoring>
      <score value="90-100">All source references verified, zero hallucinated references</score>
      <score value="70-89">1-2 unverifiable references, no fabricated files</score>
      <score value="50-69">3-5 unverifiable references or 1 fabricated file reference</score>
      <score value="30-49">Multiple fabricated references or systematic citation errors</score>
      <score value="0-29">Pervasive hallucinated references, documentation unreliable</score>
    </scoring>
  </category>

  <category name="Factual Accuracy" weight="25" severity="Critical">
    <description>COMMAREA fields, transaction IDs, navigation paths, and context assignments match actual source code</description>
    <checks>
      <check>All 14 COMMAREA level-10 fields documented with correct PIC clauses</check>
      <check>All 4 level-88 conditions documented with correct VALUES</check>
      <check>All 5 level-05 group names correct</check>
      <check>No fabricated COMMAREA fields (CDEMO-PGM-NAME, CDEMO-SCR-NAME, CDEMO-MSG-TEXT)</check>
      <check>Transaction IDs match actual RETURN TRANSID values in source</check>
      <check>Transaction-to-program mappings are correct</check>
      <check>Navigation paths match actual XCTL/LINK calls</check>
      <check>Data ownership assignments match actual WRITE/REWRITE access patterns</check>
      <check>Context relationship types accurately reflect code dependencies</check>
    </checks>
    <scoring>
      <score value="90-100">All COMMAREA fields correct, all transaction IDs verified, all navigation paths confirmed</score>
      <score value="70-89">1-2 minor factual errors (e.g., PIC X(7) documented as PIC X(07))</score>
      <score value="50-69">Fabricated COMMAREA fields present, or multiple transaction ID errors</score>
      <score value="30-49">Multiple fabricated fields, systematic navigation errors</score>
      <score value="0-29">COMMAREA documentation fundamentally wrong, navigation flows fabricated</score>
    </scoring>
  </category>

  <category name="Completeness" weight="20" severity="Major">
    <description>All expected items are documented: 14 COMMAREA fields, 18 online programs, 6-8 bounded contexts</description>
    <checks>
      <check>All 14 level-10 COMMAREA fields documented</check>
      <check>All 18 online programs appear in navigation flows or context assignments</check>
      <check>6-8 bounded contexts identified</check>
      <check>Batch processing context documented</check>
      <check>Menu programs documented as navigation infrastructure</check>
      <check>Utility program CSUTLDTC documented appropriately</check>
      <check>All XCTL paths from all 18 programs captured in navigation flows</check>
      <check>All LINK calls from all 18 programs captured</check>
      <check>Context relationships documented for all identified contexts</check>
      <check>COMMAREA-to-context field mapping present</check>
    </checks>
    <scoring>
      <score value="90-100">All 14 fields, all 18 programs, 6-8 contexts, all navigation paths documented</score>
      <score value="70-89">1-2 programs or fields missing, contexts within expected range</score>
      <score value="50-69">3-5 programs missing, or fewer than 5 contexts identified</score>
      <score value="30-49">Major gaps — multiple programs missing, COMMAREA incomplete</score>
      <score value="0-29">Documentation fundamentally incomplete</score>
    </scoring>
  </category>

  <category name="Quantitative Accuracy" weight="10" severity="Major">
    <description>File counts, field counts, context counts, and byte calculations are correct</description>
    <checks>
      <check>COBOL program count is 31 (not 39)</check>
      <check>Online program count is 18</check>
      <check>Batch program count is 12</check>
      <check>Copybook count is 30 (not 41)</check>
      <check>BMS mapset count is 17 (not 21)</check>
      <check>BMS copybook count is 17</check>
      <check>JCL file count is 38</check>
      <check>COMMAREA level-10 field count is 14</check>
      <check>Bounded context count is 6-8</check>
      <check>COMMAREA total byte size correctly calculated from PIC clauses</check>
    </checks>
    <scoring>
      <score value="90-100">All counts correct</score>
      <score value="70-89">1-2 counts off by small margins</score>
      <score value="50-69">3-4 counts wrong or one count grossly wrong</score>
      <score value="30-49">Multiple counts significantly wrong</score>
      <score value="0-29">Pervasive quantitative errors</score>
    </scoring>
  </category>

  <category name="Documentation Quality" weight="10" severity="Minor">
    <description>Mermaid syntax validity, markdown formatting, clarity, and usability</description>
    <checks>
      <check>Mermaid context map diagrams render correctly</check>
      <check>Mermaid state machine diagrams render correctly</check>
      <check>Mermaid navigation flow diagrams render correctly</check>
      <check>Markdown headings, tables, and lists are well-formed</check>
      <check>Cross-references between the three documents are consistent</check>
      <check>Terminology is consistent across all three documents</check>
      <check>DDD terms used correctly (bounded context, shared kernel, published language)</check>
      <check>Documentation is navigable and self-contained</check>
    </checks>
    <scoring>
      <score value="90-100">All diagrams render, formatting is clean, cross-references work</score>
      <score value="70-89">Minor Mermaid syntax issues, formatting mostly clean</score>
      <score value="50-69">Multiple broken diagrams or significant formatting issues</score>
      <score value="30-49">Diagrams largely broken, formatting inconsistent</score>
      <score value="0-29">Documentation quality severely impedes usability</score>
    </scoring>
  </category>

  <verdict_thresholds>
    <threshold min="100" max="100" verdict="PASS" meaning="Context model documentation is fully accurate and reliable for downstream architecture and modernization analysis"/>
    <threshold min="0" max="99" verdict="FAIL" meaning="Context model contains errors — COMMAREA specification, navigation flows, or bounded contexts have inaccuracies requiring remediation or regeneration"/>
  </verdict_thresholds>
</scoring_rubric>

<critical_reminders>
  <reminder id="1">COCOM01Y.cpy has exactly 48 lines and 14 level-10 fields — read it FIRST and use it as the definitive reference for ALL COMMAREA validation</reminder>
  <reminder id="2">The RE-003 prompt itself references CDEMO-PGM-NAME, CDEMO-SCR-NAME, and CDEMO-MSG-TEXT which DO NOT EXIST in COCOM01Y.cpy — if documentation perpetuates these, flag them as hallucinations</reminder>
  <reminder id="3">XCTL replaces the current program (no return), LINK calls a subroutine and returns — verify the documentation uses the correct semantics for each program transfer</reminder>
  <reminder id="4">RETURN TRANSID sets the next transaction for pseudo-conversational interaction — every documented transaction ID must appear in an actual RETURN TRANSID statement</reminder>
  <reminder id="5">Menu programs (COADM01C, COMEN01C) are routing infrastructure, NOT bounded contexts — if documentation treats them as contexts, flag as Major finding</reminder>
  <reminder id="6">CSUTLDTC is a date utility called via LINK — it is NOT one of the 18 online transaction programs and should not be counted as one</reminder>
  <reminder id="7">The card record copybook is CVCRD01Y.cpy, NOT CVCAR00Y.cpy — any reference to CVCAR00Y.cpy is a hallucination</reminder>
  <reminder id="8">The user security copybook is CSUSR01Y.cpy, NOT COUSR00Y.cpy — any reference to COUSR00Y.cpy is a hallucination</reminder>
  <reminder id="9">Monetary field PIC is S9(10)V99 in CVACT01Y.cpy, NOT S9(7)V99 — if the context model references monetary PICs, verify against actual source</reminder>
  <reminder id="10">Cross-reference findings with VL-001 and VL-002 reports if available — inconsistencies between validation reports indicate systematic documentation issues</reminder>
</critical_reminders>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/validation/vl-003/</path>
    <purpose>Persist validation progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
```yaml
validation_phase: "document_inventory|commarea_verification|transaction_verification|navigation_verification|context_verification|report_generation"
current_document: "filename being validated"
commarea_fields_verified: 0  # out of 14
fabricated_fields_found: []
transaction_ids_verified: []
navigation_paths_verified: 0
contexts_verified: 0  # out of 6-8 expected
findings:
  critical: []
  major: []
  minor: []
hallucinations_detected: []
scores:
  source_reference_accuracy: null
  factual_accuracy: null
  completeness: null
  quantitative_accuracy: null
  documentation_quality: null
artifacts_created:
  - path: "relative path"
    type: "document-inventory|commarea-verification|transaction-verification|navigation-verification|context-verification|report"
    status: "complete|partial"
next_action: "Detailed description of next step"
last_updated: "ISO timestamp"
```
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/validation/vl-003/progress.yaml</step>
    <step>2. Load any completed verification artifacts (commarea-verification.yaml, etc.)</step>
    <step>3. Resume from next_action in the current validation_phase</step>
    <step>4. Update progress.yaml after each verification phase completes</step>
    <step>5. If context was compacted during report generation, re-read findings from artifacts and continue</step>
  </resumption_protocol>

  <critical_state_to_preserve>
    <item>The canonical COMMAREA field list (14 fields with exact PICs) — re-read COCOM01Y.cpy if lost</item>
    <item>The list of fabricated fields detected — these are Critical findings that must appear in the report</item>
    <item>The canonical navigation graph built from XCTL/LINK analysis — this is expensive to rebuild</item>
    <item>Transaction ID verification results — each requires searching multiple source files</item>
    <item>Running score tallies for each of the 5 scoring categories</item>
  </critical_state_to_preserve>
</context_compaction_survival>

<begin>
  <instruction>
    Check for existing progress:
    1. Read .work/reverse-engineering/validation/vl-003/progress.yaml if it exists
    2. If progress exists:
       - Load completed verification artifacts
       - Resume from next_action
    3. If starting fresh:
       - Create .work/reverse-engineering/validation/vl-003/ directory
       - CRITICAL FIRST STEP: Read COCOM01Y.cpy (48 lines) and build canonical field list
       - Begin Phase 1: Document Inventory — verify all three RE-003 output documents exist
       - Create initial progress.yaml
    4. Execute all six validation phases in order:
       Phase 1: Document Inventory
       Phase 2: COMMAREA Field Verification (highest priority — every field against COCOM01Y.cpy)
       Phase 3: Transaction ID Verification (search all 18 programs for RETURN TRANSID)
       Phase 4: Navigation Flow Verification (search all 18 programs for XCTL/LINK)
       Phase 5: Bounded Context Verification (data ownership, program groupings, relationships)
       Phase 6: Report Generation (compile findings, calculate scores, determine verdict)
    5. After each phase:
       - Update progress.yaml with findings and scores
       - Write phase-specific verification artifact
    6. Write final report to docs/reverse-engineering/validation/VL-003-context-model-report.md
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to validate the RE-003 Context Model output. The prompt will systematically verify every claim in the context model documentation against the actual CardDemo source code.

### Execution Command

```
Run the validation prompt in prompts/reverse-engineering/validation/VL-003-context-model-validation.md
```

### What It Validates

1. **COMMAREA Specification** -- Every field in COMMAREA-SPECIFICATION.md verified against the 48-line COCOM01Y.cpy (14 level-10 fields, 5 level-05 groups, 4 level-88 conditions)
2. **Transaction IDs** -- Every documented CICS transaction ID confirmed via RETURN TRANSID statements in source
3. **Navigation Flows** -- Every documented program-to-program path verified against actual XCTL/LINK calls in all 18 online programs
4. **Bounded Contexts** -- Context assignments validated against data ownership patterns and copybook sharing
5. **Known Hallucinations** -- Active detection of fabricated COMMAREA fields (CDEMO-PGM-NAME, CDEMO-SCR-NAME, CDEMO-MSG-TEXT), non-existent copybooks (CVCAR00Y.cpy, COUSR00Y.cpy), and incorrect PIC clauses (S9(7)V99)

## Expected Output

| File | Description |
|------|-------------|
| `docs/reverse-engineering/validation/VL-003-context-model-report.md` | Full validation report with verdict, scores, findings, and recommendations |
| `.work/reverse-engineering/validation/vl-003/progress.yaml` | Progress tracking for resumption after context compaction |
| `.work/reverse-engineering/validation/vl-003/document-inventory.yaml` | Phase 1: document existence and size verification |
| `.work/reverse-engineering/validation/vl-003/commarea-verification.yaml` | Phase 2: field-by-field COMMAREA verification results |
| `.work/reverse-engineering/validation/vl-003/transaction-verification.yaml` | Phase 3: transaction ID verification results |
| `.work/reverse-engineering/validation/vl-003/navigation-verification.yaml` | Phase 4: navigation flow verification results |
| `.work/reverse-engineering/validation/vl-003/context-verification.yaml` | Phase 5: bounded context verification results |

### Report Structure

```markdown
# Validation Report: Context Model (RE-003)

## Verdict: [PASS|FAIL] -- Score: [NN]/100

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
## Major Findings
## Minor Findings
## Hallucination Inventory
## Completeness Gaps
## Recommendations

## Remediation Manifest
| ID | Finding | Target File | Location | Current (Wrong) | Required (Correct) | Source Evidence | Remediation Action | RE Prompt |
|----|---------|-------------|----------|-----------------|--------------------|-----------------|--------------------|-----------|
| R-001 | [finding] | [doc path] | [section/line] | [wrong] | [correct] | [source:line] | [fix] | [RE-NNN] |

### Remediation Instructions
For each row: (1) Read target at location, (2) Verify wrong value, (3) Replace with correct, (4) Re-validate

### Affected RE Prompts
[List RE prompts needing re-execution]
```

## Prerequisites

- RE-003 (Context Model Extraction) must have been executed and its output must exist in `docs/reverse-engineering/03-context-model/`
- The following three documents must be present:
  - `docs/reverse-engineering/03-context-model/CONTEXT-MAP.md`
  - `docs/reverse-engineering/03-context-model/COMMAREA-SPECIFICATION.md`
  - `docs/reverse-engineering/03-context-model/NAVIGATION-FLOWS.md`
- VL-001 and VL-002 should have completed (Phase 1 validation) so their findings can be cross-referenced
- The CardDemo source code must be available at the expected location (`app/cbl/`, `app/cpy/`, `app/bms/`, `app/cpy-bms/`, `app/jcl/`)

## Depends On

| Dependency | Reason |
|-----------|--------|
| **VL-001** (Domain Model Validation) | Cross-references entity-to-context alignment and ubiquitous language consistency |
| **VL-002** (Data Model Validation) | Cross-references file-to-context data ownership and PIC clause accuracy |

VL-003 runs in **Phase 2** of the validation execution order, after VL-001 and VL-002 have completed in Phase 1.

## Blocks

| Blocked Prompt | Reason |
|---------------|--------|
| **VL-004** (C4 Architecture Validation) | Architecture validation depends on verified context boundaries from VL-003 |
| **VL-000** (Cross-Document Consistency) | Final cross-document validation requires all individual validations to complete |
