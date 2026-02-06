# VL-005: Screen Flow Validation

## Prompt

```xml
<context>
  <role>
    <persona>Validation Analyst specializing in BMS screen definition and CICS screen flow verification</persona>

    <validation_expertise>
      <skill>BMS (Basic Mapping Support) mapset verification: DFHMSD (mapset), DFHMDI (map), DFHMDF (field) macro cross-checking</skill>
      <skill>CICS screen flow tracing: XCTL, RETURN TRANSID, SEND MAP, RECEIVE MAP command verification</skill>
      <skill>3270 terminal field validation: verifying ATTRB parameters (UNPROT, PROT, BRT, DRK, IC, NUM, FSET) against source</skill>
      <skill>PF key handler verification: tracing EVALUATE EIBAID blocks to confirm documented key assignments</skill>
      <skill>Symbolic map copybook cross-referencing: verifying BMS-generated COBOL copybooks in app/cpy-bms/ match BMS definitions</skill>
      <skill>Navigation flow verification: tracing XCTL PROGRAM and RETURN TRANSID calls to confirm screen transitions</skill>
      <skill>Hallucination detection in screen documentation: identifying fabricated field names, non-existent screens, or incorrect positions</skill>
      <skill>BMS field position arithmetic: verifying (row, column) positions and field lengths within 24x80 grid constraints</skill>
      <skill>User journey verification: tracing complete paths through COBOL program logic to confirm documented journeys</skill>
      <skill>Mermaid diagram validation: verifying flowchart nodes match actual screens and edges match actual transitions</skill>
    </validation_expertise>

    <mindset>
      BMS screen documentation is only trustworthy when every field name, position, PF key assignment, and
      navigation flow is verified against the actual BMS source files and their corresponding COBOL programs.
      The most dangerous errors are fabricated screens (claiming mapsets exist that do not), incorrect field
      attributes (marking input fields as output or vice versa), and phantom navigation flows (documenting
      transitions that no XCTL or RETURN TRANSID actually implements). A correct mapset count of 17 is the
      first litmus test -- any document claiming 21 mapsets has propagated the known hallucination from
      CLAUDE.md and must be flagged immediately.
    </mindset>
  </role>

  <objective>
    <primary_goal>
      Validate the BMS Screen Flow documentation (SCREEN-FLOWS.md) produced by RE-005 against
      the actual BMS mapset source files, BMS copybooks, and corresponding COBOL programs in the
      CardDemo codebase. Detect hallucinations, verify field definitions, confirm navigation flows,
      and assess completeness of user journey documentation.
    </primary_goal>

    <validation_targets>
      <target>docs/reverse-engineering/05-specialized/SCREEN-FLOWS.md</target>
    </validation_targets>

    <validation_scope>
      <scope_item>Screen inventory completeness: all 17 BMS mapsets accounted for, no extras fabricated</scope_item>
      <scope_item>Field-level accuracy: field names, positions, lengths, and attributes match DFHMDF definitions</scope_item>
      <scope_item>PF key accuracy: documented PF key assignments match EVALUATE EIBAID blocks in COBOL programs</scope_item>
      <scope_item>Navigation flow accuracy: screen-to-screen transitions match XCTL/RETURN TRANSID calls</scope_item>
      <scope_item>Screen-to-program mapping: each BMS mapset correctly paired with its COBOL program</scope_item>
      <scope_item>BMS copybook coverage: all 17 BMS copybooks in app/cpy-bms/ referenced</scope_item>
      <scope_item>User journey completeness: 6+ end-to-end journeys documented with happy and error paths</scope_item>
      <scope_item>Mermaid diagram correctness: navigation flowchart nodes and edges match verified transitions</scope_item>
    </validation_scope>
  </objective>

  <codebase_location>/home/ubuntu/src/aws-mainframe-modernization-carddemo</codebase_location>
</context>

<ground_truth>
  <description>
    Canonical file inventories verified directly from the source tree. These counts and file
    names are the authoritative reference for all validation checks. Any deviation in the
    documentation being validated must be flagged.
  </description>

  <bms_mapsets count="17" location="app/bms/">
    <file name="COACTUP.bms" screen="Account Update" program="COACTUPC.cbl" copybook="COACTUP.CPY" />
    <file name="COACTVW.bms" screen="Account View" program="COACTVWC.cbl" copybook="COACTVW.CPY" />
    <file name="COADM01.bms" screen="Admin Menu" program="COADM01C.cbl" copybook="COADM01.CPY" />
    <file name="COBIL00.bms" screen="Bill Payment" program="COBIL00C.cbl" copybook="COBIL00.CPY" />
    <file name="COCRDLI.bms" screen="Card List" program="COCRDLIC.cbl" copybook="COCRDLI.CPY" />
    <file name="COCRDSL.bms" screen="Card Search" program="COCRDSLC.cbl" copybook="COCRDSL.CPY" />
    <file name="COCRDUP.bms" screen="Card Update" program="COCRDUPC.cbl" copybook="COCRDUP.CPY" />
    <file name="COMEN01.bms" screen="Main Menu" program="COMEN01C.cbl" copybook="COMEN01.CPY" />
    <file name="CORPT00.bms" screen="Transaction Reports" program="CORPT00C.cbl" copybook="CORPT00.CPY" />
    <file name="COSGN00.bms" screen="Sign On" program="COSGN00C.cbl" copybook="COSGN00.CPY" />
    <file name="COTRN00.bms" screen="Transaction Menu" program="COTRN00C.cbl" copybook="COTRN00.CPY" />
    <file name="COTRN01.bms" screen="Transaction List" program="COTRN01C.cbl" copybook="COTRN01.CPY" />
    <file name="COTRN02.bms" screen="Transaction Detail" program="COTRN02C.cbl" copybook="COTRN02.CPY" />
    <file name="COUSR00.bms" screen="User Menu" program="COUSR00C.cbl" copybook="COUSR00.CPY" />
    <file name="COUSR01.bms" screen="Add User" program="COUSR01C.cbl" copybook="COUSR01.CPY" />
    <file name="COUSR02.bms" screen="Update User" program="COUSR02C.cbl" copybook="COUSR02.CPY" />
    <file name="COUSR03.bms" screen="Delete User" program="COUSR03C.cbl" copybook="COUSR03.CPY" />
  </bms_mapsets>

  <bms_copybooks count="17" location="app/cpy-bms/">
    <file name="COACTUP.CPY" />
    <file name="COACTVW.CPY" />
    <file name="COADM01.CPY" />
    <file name="COBIL00.CPY" />
    <file name="COCRDLI.CPY" />
    <file name="COCRDSL.CPY" />
    <file name="COCRDUP.CPY" />
    <file name="COMEN01.CPY" />
    <file name="CORPT00.CPY" />
    <file name="COSGN00.CPY" />
    <file name="COTRN00.CPY" />
    <file name="COTRN01.CPY" />
    <file name="COTRN02.CPY" />
    <file name="COUSR00.CPY" />
    <file name="COUSR01.CPY" />
    <file name="COUSR02.CPY" />
    <file name="COUSR03.CPY" />
  </bms_copybooks>

  <cobol_programs count="31" location="app/cbl/">
    <file name="CBACT01C.cbl" type="batch" />
    <file name="CBACT02C.cbl" type="batch" />
    <file name="CBACT03C.cbl" type="batch" />
    <file name="CBACT04C.cbl" type="batch" />
    <file name="CBCUS01C.cbl" type="batch" />
    <file name="CBEXPORT.cbl" type="batch" />
    <file name="CBIMPORT.cbl" type="batch" />
    <file name="CBSTM03A.CBL" type="batch" />
    <file name="CBSTM03B.CBL" type="batch" />
    <file name="CBTRN01C.cbl" type="batch" />
    <file name="CBTRN02C.cbl" type="batch" />
    <file name="CBTRN03C.cbl" type="batch" />
    <file name="COACTUPC.cbl" type="online" />
    <file name="COACTVWC.cbl" type="online" />
    <file name="COADM01C.cbl" type="online" />
    <file name="COBIL00C.cbl" type="online" />
    <file name="COBSWAIT.cbl" type="online" />
    <file name="COCRDLIC.cbl" type="online" />
    <file name="COCRDSLC.cbl" type="online" />
    <file name="COCRDUPC.cbl" type="online" />
    <file name="COMEN01C.cbl" type="online" />
    <file name="CORPT00C.cbl" type="online" />
    <file name="COSGN00C.cbl" type="online" />
    <file name="COTRN00C.cbl" type="online" />
    <file name="COTRN01C.cbl" type="online" />
    <file name="COTRN02C.cbl" type="online" />
    <file name="COUSR00C.cbl" type="online" />
    <file name="COUSR01C.cbl" type="online" />
    <file name="COUSR02C.cbl" type="online" />
    <file name="COUSR03C.cbl" type="online" />
    <file name="CSUTLDTC.cbl" type="utility" />
  </cobol_programs>

  <copybooks count="30" location="app/cpy/">
    <file name="COADM02Y.cpy" />
    <file name="COCOM01Y.cpy" />
    <file name="CODATECN.cpy" />
    <file name="COMEN02Y.cpy" />
    <file name="COSTM01.CPY" />
    <file name="COTTL01Y.cpy" />
    <file name="CSDAT01Y.cpy" />
    <file name="CSLKPCDY.cpy" />
    <file name="CSMSG01Y.cpy" />
    <file name="CSMSG02Y.cpy" />
    <file name="CSSETATY.cpy" />
    <file name="CSSTRPFY.cpy" />
    <file name="CSUSR01Y.cpy" />
    <file name="CSUTLDPY.cpy" />
    <file name="CSUTLDWY.cpy" />
    <file name="CUSTREC.cpy" />
    <file name="CVACT01Y.cpy" />
    <file name="CVACT02Y.cpy" />
    <file name="CVACT03Y.cpy" />
    <file name="CVCRD01Y.cpy" />
    <file name="CVCUS01Y.cpy" />
    <file name="CVEXPORT.cpy" />
    <file name="CVTRA01Y.cpy" />
    <file name="CVTRA02Y.cpy" />
    <file name="CVTRA03Y.cpy" />
    <file name="CVTRA04Y.cpy" />
    <file name="CVTRA05Y.cpy" />
    <file name="CVTRA06Y.cpy" />
    <file name="CVTRA07Y.cpy" />
    <file name="UNUSED1Y.cpy" />
  </copybooks>

  <jcl_files count="38" location="app/jcl/" />

  <mapset_names>
    <description>
      The DFHMSD macro name (first token in each BMS file) defines the mapset name.
      The DFHMDI macro name defines the map name within the mapset. These must match
      exactly what is documented.
    </description>
    <mapset file="COACTUP.bms" mapset_name="COACTUP" map_name="COACTU1" />
    <mapset file="COACTVW.bms" mapset_name="COACTVW" map_name="COACTV1" />
    <mapset file="COADM01.bms" mapset_name="COADM01" map_name="COADM0A" />
    <mapset file="COBIL00.bms" mapset_name="COBIL00" map_name="COBIL0A" />
    <mapset file="COCRDLI.bms" mapset_name="COCRDLI" map_name="COCRDL1" />
    <mapset file="COCRDSL.bms" mapset_name="COCRDSL" map_name="COCRDS1" />
    <mapset file="COCRDUP.bms" mapset_name="COCRDUP" map_name="COCRDU1" />
    <mapset file="COMEN01.bms" mapset_name="COMEN01" map_name="COMEN0A" />
    <mapset file="CORPT00.bms" mapset_name="CORPT00" map_name="CORPT0A" />
    <mapset file="COSGN00.bms" mapset_name="COSGN00" map_name="COSGN0A" />
    <mapset file="COTRN00.bms" mapset_name="COTRN00" map_name="COTRN0A" />
    <mapset file="COTRN01.bms" mapset_name="COTRN01" map_name="COTRN0B" />
    <mapset file="COTRN02.bms" mapset_name="COTRN02" map_name="COTRN0C" />
    <mapset file="COUSR00.bms" mapset_name="COUSR00" map_name="COUSR0A" />
    <mapset file="COUSR01.bms" mapset_name="COUSR01" map_name="COUSR0B" />
    <mapset file="COUSR02.bms" mapset_name="COUSR02" map_name="COUSR0C" />
    <mapset file="COUSR03.bms" mapset_name="COUSR03" map_name="COUSR0D" />
  </mapset_names>

  <known_hallucinations>
    <hallucination id="KH-001" description="21 BMS mapsets claimed instead of actual 17">
      <detail>CLAUDE.md and RE-005 success criteria reference 21 BMS mapsets. The actual count is 17.</detail>
      <check>If SCREEN-FLOWS.md claims 21 mapsets, flag as Critical hallucination propagated from RE-005 prompt.</check>
    </hallucination>
    <hallucination id="KH-002" description="CVCAR00Y.cpy referenced as card copybook">
      <detail>The actual card data copybook is CVCRD01Y.cpy. CVCAR00Y.cpy does not exist.</detail>
      <check>Search SCREEN-FLOWS.md for any reference to CVCAR00Y. Flag if found.</check>
    </hallucination>
    <hallucination id="KH-003" description="COUSR00Y.cpy referenced as user copybook">
      <detail>The actual user data copybook is CSUSR01Y.cpy. COUSR00Y.cpy does not exist.</detail>
      <check>Search SCREEN-FLOWS.md for any reference to COUSR00Y. Flag if found.</check>
    </hallucination>
    <hallucination id="KH-004" description="Fabricated BMS mapsets beyond the 17 actual files">
      <detail>Any mapset name not in the ground truth list of 17 is fabricated.</detail>
      <check>Cross-check every mapset mentioned in the document against the ground truth list.</check>
    </hallucination>
    <hallucination id="KH-005" description="CORPT00 (Transaction Reports) omitted from screen inventory">
      <detail>CORPT00.bms is commonly overlooked. It defines the Transaction Reports screen with CORPT00C.cbl.</detail>
      <check>Verify CORPT00 appears in the screen inventory and navigation flows.</check>
    </hallucination>
  </known_hallucinations>

  <key_bms_field_samples>
    <description>
      Spot-check samples from actual BMS source files. Use these to verify field-level accuracy
      in the documentation. If the document claims different field names, positions, lengths, or
      attributes, flag as factual error.
    </description>
    <sample source="COSGN00.bms">
      <field name="USERID" pos="(19,43)" length="8" attrb="FSET,IC,NORM,UNPROT" color="GREEN" />
      <field name="PASSWD" pos="(20,43)" length="8" attrb="DRK,FSET,UNPROT" color="GREEN" />
      <field name="ERRMSG" pos="(23,1)" length="78" attrb="ASKIP,BRT,FSET" color="RED" />
      <field name="TRNNAME" pos="(1,8)" length="4" attrb="ASKIP,FSET,NORM" color="BLUE" />
      <field name="CURDATE" pos="(1,71)" length="8" attrb="ASKIP,FSET,NORM" color="BLUE" />
      <pf_keys>ENTER=Sign-on, F3=Exit</pf_keys>
    </sample>
    <sample source="CORPT00.bms">
      <field name="MONTHLY" pos="(7,10)" length="1" attrb="FSET,IC,NORM,UNPROT" color="GREEN" />
      <field name="YEARLY" pos="(9,10)" length="1" attrb="FSET,NORM,UNPROT" color="GREEN" />
      <field name="CUSTOM" pos="(11,10)" length="1" attrb="FSET,NORM,UNPROT" color="GREEN" />
      <field name="SDTMM" pos="(13,29)" length="2" attrb="FSET,NORM,NUM,UNPROT" color="GREEN" />
      <field name="SDTDD" pos="(13,34)" length="2" attrb="FSET,NORM,NUM,UNPROT" color="GREEN" />
      <field name="SDTYYYY" pos="(13,39)" length="4" attrb="FSET,NORM,NUM,UNPROT" color="GREEN" />
      <field name="EDTMM" pos="(14,29)" length="2" attrb="FSET,NORM,NUM,UNPROT" color="GREEN" />
      <field name="EDTDD" pos="(14,34)" length="2" attrb="FSET,NORM,NUM,UNPROT" color="GREEN" />
      <field name="EDTYYYY" pos="(14,39)" length="4" attrb="FSET,NORM,NUM,UNPROT" color="GREEN" />
      <field name="CONFIRM" pos="(19,66)" length="1" attrb="FSET,NORM,UNPROT" color="GREEN" />
      <field name="ERRMSG" pos="(23,1)" length="78" attrb="ASKIP,BRT,FSET" color="RED" />
      <pf_keys>ENTER=Continue, F3=Back</pf_keys>
    </sample>
  </key_bms_field_samples>

  <screen_to_program_mappings>
    <description>
      Each BMS mapset pairs with exactly one online COBOL program. These mappings must be
      accurately documented. The naming convention is: COXXXX.bms pairs with COXXXXC.cbl.
    </description>
    <mapping bms="COACTUP.bms" program="COACTUPC.cbl" />
    <mapping bms="COACTVW.bms" program="COACTVWC.cbl" />
    <mapping bms="COADM01.bms" program="COADM01C.cbl" />
    <mapping bms="COBIL00.bms" program="COBIL00C.cbl" />
    <mapping bms="COCRDLI.bms" program="COCRDLIC.cbl" />
    <mapping bms="COCRDSL.bms" program="COCRDSLC.cbl" />
    <mapping bms="COCRDUP.bms" program="COCRDUPC.cbl" />
    <mapping bms="COMEN01.bms" program="COMEN01C.cbl" />
    <mapping bms="CORPT00.bms" program="CORPT00C.cbl" />
    <mapping bms="COSGN00.bms" program="COSGN00C.cbl" />
    <mapping bms="COTRN00.bms" program="COTRN00C.cbl" />
    <mapping bms="COTRN01.bms" program="COTRN01C.cbl" />
    <mapping bms="COTRN02.bms" program="COTRN02C.cbl" />
    <mapping bms="COUSR00.bms" program="COUSR00C.cbl" />
    <mapping bms="COUSR01.bms" program="COUSR01C.cbl" />
    <mapping bms="COUSR02.bms" program="COUSR02C.cbl" />
    <mapping bms="COUSR03.bms" program="COUSR03C.cbl" />
  </screen_to_program_mappings>

  <expected_user_journeys minimum="6">
    <journey name="Sign On" path="COSGN00 -> COADM01 (admin) or COMEN01 (regular user)" />
    <journey name="Account Inquiry" path="Menu -> COACTVW (Account View)" />
    <journey name="Account Update" path="Menu -> COACTVW -> COACTUP (Account Update)" />
    <journey name="Card Management" path="Menu -> COCRDSL (Search) -> COCRDLI (List) -> COCRDUP (Update)" />
    <journey name="View Transactions" path="Menu -> COTRN00 (Menu) -> COTRN01 (List) -> COTRN02 (Detail)" />
    <journey name="Bill Payment" path="Menu -> COBIL00 (Bill Payment)" />
    <journey name="Transaction Reports" path="Menu -> CORPT00 (Reports)" />
    <journey name="User Administration" path="Admin Menu -> COUSR00 (Menu) -> COUSR01/COUSR02/COUSR03 (Add/Update/Delete)" />
  </expected_user_journeys>
</ground_truth>

<validation_methodology>
  <phase name="1_mapset_inventory_verification" priority="critical">
    <description>Verify the screen inventory is complete and accurate</description>
    <steps>
      <step>Count the total number of BMS mapsets claimed in the document. It MUST be exactly 17.</step>
      <step>Cross-check every mapset name against the ground truth list of 17 files in app/bms/.</step>
      <step>Flag any mapset names that appear in the document but do NOT exist in app/bms/ (fabricated screens).</step>
      <step>Flag any mapset names from the ground truth list that are MISSING from the document.</step>
      <step>Specifically verify CORPT00.bms is included -- it is the most commonly omitted mapset.</step>
      <step>Check for the "21 BMS mapsets" hallucination. If found, flag as Critical (KH-001).</step>
      <step>Verify the screen-to-program mappings follow the COXXXX.bms -> COXXXXC.cbl convention.</step>
      <step>Confirm all 17 BMS copybooks in app/cpy-bms/ are referenced somewhere in the document.</step>
    </steps>
    <pass_criteria>All 17 mapsets listed, no fabricated mapsets, CORPT00 included, correct count stated.</pass_criteria>
  </phase>

  <phase name="2_field_definition_verification" priority="critical">
    <description>Spot-check field definitions against actual BMS DFHMDF macros</description>
    <steps>
      <step>For each screen documented with field specifications, open the corresponding .bms file.</step>
      <step>Verify field names match the labels on DFHMDF macros in the BMS source.</step>
      <step>Verify POS=(row,column) positions match the documented positions exactly.</step>
      <step>Verify LENGTH values match the documented field lengths.</step>
      <step>Verify ATTRB parameters: check that UNPROT fields are documented as input, PROT/ASKIP as output.</step>
      <step>Verify DRK (dark/hidden) attribute on password fields like PASSWD in COSGN00.bms.</step>
      <step>Spot-check at least 5 screens (COSGN00, CORPT00, COBIL00, COADM01, COUSR01) for field accuracy.</step>
      <step>Flag any field names in the document that do not appear in the corresponding BMS source.</step>
      <step>Verify field positions fit within the 24x80 grid (row 1-24, column 1-80).</step>
    </steps>
    <pass_criteria>Field names, positions, lengths, and types match BMS source for all spot-checked screens.</pass_criteria>
  </phase>

  <phase name="3_pf_key_verification" priority="critical">
    <description>Verify PF key assignments against EVALUATE EIBAID blocks in COBOL programs</description>
    <steps>
      <step>For each screen, locate the PF key footer text in the BMS source (typically row 24).</step>
      <step>Open the corresponding COBOL program and locate EVALUATE EIBAID blocks.</step>
      <step>Verify each WHEN DFHPF* condition matches a documented PF key assignment.</step>
      <step>Check for WHEN DFHENTER handling and confirm ENTER key behavior is documented.</step>
      <step>Check for WHEN DFHCLEAR handling and confirm CLEAR key behavior is documented.</step>
      <step>Verify the target program/action for each PF key by tracing the code in the WHEN block.</step>
      <step>Flag any documented PF key assignments that have no corresponding WHEN clause in the program.</step>
      <step>Flag any WHEN DFHPF* clauses in the program that are not documented.</step>
    </steps>
    <pass_criteria>All documented PF keys have corresponding EVALUATE EIBAID handlers, and vice versa.</pass_criteria>
  </phase>

  <phase name="4_navigation_flow_verification" priority="critical">
    <description>Verify screen-to-screen navigation transitions against XCTL and RETURN TRANSID calls</description>
    <steps>
      <step>For each navigation arrow/edge in the documented flow diagram, identify the source screen program.</step>
      <step>Open the source COBOL program and search for EXEC CICS XCTL PROGRAM statements.</step>
      <step>Verify the XCTL target program name matches the documented destination screen.</step>
      <step>Search for EXEC CICS RETURN TRANSID statements and verify they match documented return flows.</step>
      <step>Check COMMAREA passing between programs to confirm data flow between screens.</step>
      <step>Verify conditional navigation (e.g., admin vs regular user paths from COSGN00C).</step>
      <step>Flag any documented navigation flows that have no supporting XCTL/RETURN TRANSID in source.</step>
      <step>Flag any XCTL/RETURN TRANSID calls in source that are not documented as navigation flows.</step>
      <step>Verify Mermaid flowchart edges match the verified navigation transitions.</step>
    </steps>
    <pass_criteria>Every documented flow has a corresponding XCTL/RETURN TRANSID, and no undocumented flows exist.</pass_criteria>
  </phase>

  <phase name="5_user_journey_verification" priority="major">
    <description>Verify end-to-end user journeys are complete and accurate</description>
    <steps>
      <step>Count the number of documented user journeys. Minimum required is 6.</step>
      <step>Verify each journey step maps to an actual screen (BMS mapset) and program.</step>
      <step>Trace the journey through XCTL/RETURN TRANSID calls to confirm the path is achievable.</step>
      <step>Check that the Sign On journey correctly branches to Admin Menu vs Main Menu based on user type.</step>
      <step>Verify the Card Management journey includes the full Search -> List -> Update flow.</step>
      <step>Verify the Transaction journey includes Menu -> List -> Detail flow.</step>
      <step>Verify the User Administration journey shows all three sub-functions (Add/Update/Delete).</step>
      <step>Check for Transaction Reports journey involving CORPT00 -- commonly omitted.</step>
      <step>Verify error paths are documented (what happens on invalid input at each step).</step>
      <step>Verify role-based access: Admin can reach COUSR* screens, regular users cannot.</step>
    </steps>
    <pass_criteria>At least 6 journeys documented, all steps verified against source, role-based access noted.</pass_criteria>
  </phase>

  <phase name="6_documentation_quality_verification" priority="minor">
    <description>Verify documentation formatting, diagram correctness, and overall quality</description>
    <steps>
      <step>Validate all Mermaid diagram syntax -- flowcharts should parse without errors.</step>
      <step>Verify Mermaid node IDs match actual mapset names (not fabricated identifiers).</step>
      <step>Check markdown formatting: headers, tables, code blocks properly structured.</step>
      <step>Verify ASCII art or screen layout diagrams are readable and reasonably accurate.</step>
      <step>Check that error message catalogs reference actual messages from COBOL program source.</step>
      <step>Verify terminology consistency with RE-001 Domain Model (if available).</step>
      <step>Check for broken internal cross-references or links.</step>
      <step>Verify the document has a logical structure progressing from inventory to flows to journeys.</step>
    </steps>
    <pass_criteria>Mermaid syntax valid, markdown well-formed, diagrams accurate, consistent terminology.</pass_criteria>
  </phase>
</validation_methodology>

<scoring_rubric>
  <category name="Source Reference Accuracy" weight="35" severity="critical">
    <description>
      Every file:line citation, BMS field reference, and program name verified against actual source.
      This is the most heavily weighted category because incorrect source references propagate errors
      into downstream modernization decisions.
    </description>
    <scoring_guide>
      <score range="90-100">All BMS field references verified correct; all program names match; all file paths valid.</score>
      <score range="70-89">1-2 minor field reference errors (e.g., slightly wrong position); all program names correct.</score>
      <score range="50-69">3-5 field reference errors or 1 wrong program mapping; some fabricated field names.</score>
      <score range="30-49">Multiple fabricated field names or wrong program mappings; several invalid file references.</score>
      <score range="0-29">Widespread fabrication of field names, positions, or program mappings.</score>
    </scoring_guide>
  </category>

  <category name="Factual Accuracy" weight="25" severity="critical">
    <description>
      PF key assignments, field attributes (input/output/hidden), navigation flows, and screen
      descriptions match the actual BMS definitions and COBOL program logic. Mapset count must
      be exactly 17.
    </description>
    <scoring_guide>
      <score range="90-100">Mapset count is 17; all PF keys verified; all navigation flows confirmed; field types correct.</score>
      <score range="70-89">Mapset count correct; 1-2 PF key errors or minor navigation inaccuracies.</score>
      <score range="50-69">Mapset count wrong (e.g., claims 21) or 3+ navigation flow errors; some PF keys unverified.</score>
      <score range="30-49">Major factual errors: wrong mapset count, fabricated navigation flows, incorrect field types.</score>
      <score range="0-29">Pervasive factual errors rendering the document unreliable for modernization planning.</score>
    </scoring_guide>
  </category>

  <category name="Completeness" weight="20" severity="major">
    <description>
      All 17 BMS mapsets covered, all 17 BMS copybooks referenced, all screen-to-program mappings
      documented, 6+ user journeys included, CORPT00 (Transaction Reports) not omitted.
    </description>
    <scoring_guide>
      <score range="90-100">All 17 mapsets documented; all 17 copybooks referenced; 6+ user journeys; CORPT00 included.</score>
      <score range="70-89">16 of 17 mapsets documented or 5 user journeys; minor copybook coverage gaps.</score>
      <score range="50-69">14-15 of 17 mapsets documented; 4 user journeys; CORPT00 missing; copybook gaps.</score>
      <score range="30-49">Fewer than 14 mapsets documented; fewer than 4 user journeys; major coverage gaps.</score>
      <score range="0-29">Fewer than 10 mapsets documented; minimal journey documentation; large gaps throughout.</score>
    </scoring_guide>
  </category>

  <category name="Quantitative Accuracy" weight="10" severity="major">
    <description>
      Numeric claims are correct: mapset count (17), field counts per screen, field lengths,
      row/column positions within 24x80 grid, PF key numbers.
    </description>
    <scoring_guide>
      <score range="90-100">All numeric claims verified correct; field lengths match; positions within grid bounds.</score>
      <score range="70-89">1-2 minor numeric errors (e.g., field length off by 1); mapset count correct.</score>
      <score range="50-69">Mapset count incorrect or 3+ numeric errors; some positions outside grid bounds.</score>
      <score range="30-49">Multiple wrong counts and measurements; systematic numeric inaccuracies.</score>
      <score range="0-29">Numeric claims largely fabricated or systematically wrong.</score>
    </scoring_guide>
  </category>

  <category name="Documentation Quality" weight="10" severity="minor">
    <description>
      Mermaid diagrams parse correctly, markdown is well-formed, screen layouts are readable,
      document structure is logical, terminology is consistent.
    </description>
    <scoring_guide>
      <score range="90-100">All Mermaid diagrams valid; clear structure; readable layouts; consistent terminology.</score>
      <score range="70-89">Minor Mermaid syntax issues; generally good structure; minor formatting inconsistencies.</score>
      <score range="50-69">Some broken Mermaid diagrams; inconsistent formatting; unclear section organization.</score>
      <score range="30-49">Multiple broken diagrams; poor structure; hard to follow; terminology confusion.</score>
      <score range="0-29">Diagrams non-functional; formatting severely broken; document unusable.</score>
    </scoring_guide>
  </category>

  <verdict_thresholds>
    <threshold min="100" max="100" verdict="PASS" meaning="Documentation is reliable for UI modernization planning." />
    <threshold min="0" max="99" verdict="FAIL" meaning="Documentation is unreliable and must be regenerated with corrected RE-005 prompt." />
  </verdict_thresholds>
</scoring_rubric>

<output_specification>
  <output_file>docs/reverse-engineering/validation/VL-005-screen-flow-report.md</output_file>
  <working_directory>.work/reverse-engineering/validation/vl-005/</working_directory>

  <report_structure>
    <section name="header">
      <content># Validation Report: BMS Screen Flow Documentation (RE-005)</content>
    </section>
    <section name="verdict">
      <content>## Verdict: [PASS|FAIL] -- Score: [NN]/100</content>
    </section>
    <section name="score_breakdown">
      <content>
## Score Breakdown

| Category | Weight | Score | Weighted |
|----------|--------|-------|----------|
| Source Reference Accuracy | 35% | NN/100 | NN.N |
| Factual Accuracy | 25% | NN/100 | NN.N |
| Completeness | 20% | NN/100 | NN.N |
| Quantitative Accuracy | 10% | NN/100 | NN.N |
| Documentation Quality | 10% | NN/100 | NN.N |
| **Total** | **100%** | | **NN.N** |
      </content>
    </section>
    <section name="critical_findings">
      <content>
## Critical Findings

[Each finding with: ID, description, claim in document, reality from source, source file:line, impact]
      </content>
    </section>
    <section name="major_findings">
      <content>
## Major Findings

[Each finding with: ID, description, what is missing or incorrect, expected content, impact]
      </content>
    </section>
    <section name="minor_findings">
      <content>
## Minor Findings

[Each finding with: ID, description, issue, suggested fix]
      </content>
    </section>
    <section name="hallucination_inventory">
      <content>
## Hallucination Inventory

| # | Claim in Documentation | Reality (from source) | Source File:Line | Severity |
|---|----------------------|----------------------|-----------------|----------|
| 1 | ... | ... | ... | Critical/Major/Minor |
      </content>
    </section>
    <section name="mapset_verification_matrix">
      <content>
## Mapset Verification Matrix

| # | BMS File | Mapset Name | Map Name | Program | Copybook | In Doc? | Fields Verified? | PF Keys Verified? |
|---|----------|-------------|----------|---------|----------|---------|-----------------|-------------------|
| 1 | COACTUP.bms | COACTUP | COACTU1 | COACTUPC.cbl | COACTUP.CPY | Y/N | Y/N/Partial | Y/N/Partial |
| ... | ... | ... | ... | ... | ... | ... | ... | ... |
      </content>
    </section>
    <section name="navigation_flow_verification">
      <content>
## Navigation Flow Verification

| # | From Screen | To Screen | Trigger | XCTL/RETURN in Source | Source File:Line | Verified? |
|---|-------------|-----------|---------|----------------------|-----------------|-----------|
| 1 | ... | ... | ... | ... | ... | Y/N |
      </content>
    </section>
    <section name="user_journey_verification">
      <content>
## User Journey Verification

| # | Journey Name | Steps | All Steps Verified? | Error Paths Documented? | Notes |
|---|-------------|-------|---------------------|------------------------|-------|
| 1 | ... | ... | Y/N/Partial | Y/N | ... |
      </content>
    </section>
    <section name="completeness_gaps">
      <content>
## Completeness Gaps

| # | Expected Item | Status | Notes |
|---|--------------|--------|-------|
| 1 | CORPT00.bms (Transaction Reports) | Present/Missing | ... |
| 2 | ... | ... | ... |
      </content>
    </section>
    <section name="recommendations">
      <content>
## Recommendations

[Specific, actionable recommendations for remediation organized by priority]
      </content>
    </section>
    <section name="remediation_manifest">
      <content>
## Remediation Manifest
| ID | Finding | Target File | Location | Current (Wrong) | Required (Correct) | Source Evidence | Remediation Action | RE Prompt |
|----|---------|-------------|----------|-----------------|--------------------|-----------------|--------------------|-----------|
| R-001 | [finding] | [doc path] | [section/line] | [wrong] | [correct] | [source:line] | [fix] | [RE-NNN] |
### Remediation Instructions
For each row: (1) Read target at location, (2) Verify wrong value, (3) Replace with correct, (4) Re-validate
### Affected RE Prompts
[List RE prompts needing re-execution]
      </content>
    </section>
  </report_structure>
</output_specification>

<foundational_principles>
  <principle id="1">
    The actual BMS mapset count is 17, not 21. Any document claiming 21 has propagated the known
    hallucination from CLAUDE.md. This is the single most important litmus test for RE-005 output
    quality and must be checked first.
  </principle>
  <principle id="2">
    Every field name documented must exist as a label on a DFHMDF macro in the corresponding BMS
    file. Fabricated field names are Critical-severity hallucinations because they would cause
    incorrect field mappings during modernization.
  </principle>
  <principle id="3">
    Field positions (row, column) in documentation must match the POS= parameter on the DFHMDF
    macro exactly. Position errors break screen layout reconstruction and modernization wireframing.
  </principle>
  <principle id="4">
    PF key assignments must be verified in two places: the BMS footer text (typically row 24) and
    the EVALUATE EIBAID block in the corresponding COBOL program. Discrepancies between these
    two sources should also be noted.
  </principle>
  <principle id="5">
    Navigation flows must be traceable to specific EXEC CICS XCTL PROGRAM or EXEC CICS RETURN
    TRANSID statements in COBOL source. Documented transitions without supporting CICS commands
    are fabricated and must be flagged.
  </principle>
  <principle id="6">
    CORPT00.bms (Transaction Reports) is the most commonly omitted screen because RE-005's
    carddemo_context section lists only 16 mapsets, omitting CORPT00. Its absence in the
    generated documentation is a predictable completeness gap that must be specifically checked.
  </principle>
  <principle id="7">
    BMS copybooks in app/cpy-bms/ are generated from BMS mapsets and provide the COBOL data
    structures for field access. Each of the 17 BMS files has a corresponding .CPY file. The
    document should reference these copybooks as the bridge between BMS definitions and COBOL
    program logic.
  </principle>
  <principle id="8">
    Screen-to-program mapping follows the naming convention COXXXX.bms -> COXXXXC.cbl. Any
    documented mapping that deviates from this pattern should be verified by checking the actual
    SEND MAP/RECEIVE MAP commands in the COBOL program to confirm which mapset it uses.
  </principle>
</foundational_principles>

<critical_reminders>
  <reminder id="1">
    CHECK MAPSET COUNT FIRST. If the document claims 21 BMS mapsets, immediately flag KH-001 as
    a Critical hallucination. The correct count is 17.
  </reminder>
  <reminder id="2">
    VERIFY CORPT00 INCLUSION. CORPT00.bms (Transaction Reports) with program CORPT00C.cbl is the
    most commonly omitted screen. Check the screen inventory, navigation flows, AND user journeys
    for its presence.
  </reminder>
  <reminder id="3">
    CROSS-CHECK FIELD NAMES against BMS source. Do not trust documented field names without opening
    the actual .bms file and confirming the DFHMDF label matches. Use the spot-check samples in
    ground_truth as starting points.
  </reminder>
  <reminder id="4">
    VERIFY PF KEYS IN COBOL, not just BMS. The BMS footer shows what is displayed to the user, but
    the EVALUATE EIBAID block in the COBOL program shows what is actually implemented. Document
    discrepancies between displayed and implemented PF keys.
  </reminder>
  <reminder id="5">
    TRACE NAVIGATION with XCTL/RETURN. For every arrow in a navigation flow diagram, find the
    corresponding EXEC CICS XCTL PROGRAM('target') or RETURN TRANSID('tran') in the source program.
    No XCTL/RETURN means the flow is fabricated.
  </reminder>
  <reminder id="6">
    CHECK FOR KNOWN HALLUCINATIONS. Search the document for: CVCAR00Y.cpy (should be CVCRD01Y.cpy),
    COUSR00Y.cpy (should be CSUSR01Y.cpy), and any reference to a count of 21 mapsets, 39 programs,
    or 41 copybooks.
  </reminder>
  <reminder id="7">
    VERIFY ALL 17 BMS COPYBOOKS are referenced. The complete list in app/cpy-bms/ is: COACTUP.CPY,
    COACTVW.CPY, COADM01.CPY, COBIL00.CPY, COCRDLI.CPY, COCRDSL.CPY, COCRDUP.CPY, COMEN01.CPY,
    CORPT00.CPY, COSGN00.CPY, COTRN00.CPY, COTRN01.CPY, COTRN02.CPY, COUSR00.CPY, COUSR01.CPY,
    COUSR02.CPY, COUSR03.CPY.
  </reminder>
  <reminder id="8">
    VERIFY MERMAID SYNTAX. Copy each Mermaid code block and check for syntax validity. Common
    errors include missing node definitions, incorrect arrow syntax, and unescaped special characters
    in node labels.
  </reminder>
  <reminder id="9">
    CHECK FIELD POSITIONS within 24x80 grid. Any documented field with row > 24 or column > 80 is
    impossible on a standard 3270 terminal. Also verify that field length + column does not exceed
    column 80 (fields cannot extend beyond screen edge within a single row).
  </reminder>
  <reminder id="10">
    SAVE PROGRESS INCREMENTALLY. After completing each validation phase, update progress.yaml in
    the working directory. This ensures work is preserved across context window compaction events.
  </reminder>
</critical_reminders>

<context_compaction_survival>
  <work_tracking_directory>
    <path>.work/reverse-engineering/validation/vl-005/</path>
    <purpose>Persist validation progress to survive context window compaction</purpose>
  </work_tracking_directory>

  <progress_tracking_schema>
    <file>progress.yaml</file>
    <structure>
```yaml
validation_phase: "inventory_check|field_verification|pf_key_check|navigation_check|journey_check|quality_check|report_generation"
phases_completed:
  - phase: "inventory_check"
    status: "complete|partial|not_started"
    findings_count: 0
  - phase: "field_verification"
    status: "complete|partial|not_started"
    screens_checked: []
    screens_remaining: []
  - phase: "pf_key_check"
    status: "complete|partial|not_started"
    programs_checked: []
  - phase: "navigation_check"
    status: "complete|partial|not_started"
    flows_verified: 0
    flows_failed: 0
  - phase: "journey_check"
    status: "complete|partial|not_started"
    journeys_verified: 0
  - phase: "quality_check"
    status: "complete|partial|not_started"
  - phase: "report_generation"
    status: "complete|partial|not_started"
findings:
  critical: []
  major: []
  minor: []
hallucinations_found: []
scores:
  source_reference_accuracy: null
  factual_accuracy: null
  completeness: null
  quantitative_accuracy: null
  documentation_quality: null
  total: null
verdict: null
artifacts_created:
  - path: "mapset-verification.yaml"
    status: "complete|partial"
  - path: "field-spot-checks.yaml"
    status: "complete|partial"
  - path: "navigation-flows.yaml"
    status: "complete|partial"
next_action: "Detailed next step description"
last_updated: "ISO timestamp"
```
    </structure>
  </progress_tracking_schema>

  <resumption_protocol>
    <step>1. Read .work/reverse-engineering/validation/vl-005/progress.yaml if it exists</step>
    <step>2. Load any completed artifact files (mapset-verification.yaml, field-spot-checks.yaml, etc.)</step>
    <step>3. Resume from the phase and step indicated by next_action</step>
    <step>4. Update progress.yaml after completing each validation phase</step>
    <step>5. If all phases complete, generate the final report at docs/reverse-engineering/validation/VL-005-screen-flow-report.md</step>
  </resumption_protocol>

  <critical_state>
    <description>
      If context compaction occurs, the following minimum state must be reconstructible from
      the working directory artifacts:
    </description>
    <state_item>Which of the 17 mapsets have been verified in the document</state_item>
    <state_item>Which field spot-checks have been performed and their pass/fail results</state_item>
    <state_item>Which navigation flows have been traced to XCTL/RETURN TRANSID calls</state_item>
    <state_item>All findings discovered so far (critical, major, minor)</state_item>
    <state_item>All hallucinations detected so far</state_item>
    <state_item>Partial scores for completed categories</state_item>
  </critical_state>
</context_compaction_survival>

<begin>
  <instruction>
    Execute the VL-005 Screen Flow Validation by following these steps:

    1. PREREQUISITES CHECK:
       a. Verify docs/reverse-engineering/05-specialized/SCREEN-FLOWS.md exists
       b. If it does not exist, report "TARGET DOCUMENT NOT FOUND" and stop
       c. Check for existing progress in .work/reverse-engineering/validation/vl-005/progress.yaml
       d. If resuming, load state and continue from next_action

    2. PHASE 1 -- MAPSET INVENTORY VERIFICATION (Critical):
       a. Read SCREEN-FLOWS.md and extract the claimed BMS mapset count and list
       b. Compare against ground truth: exactly 17 mapsets
       c. Check for KH-001 (21 mapset hallucination)
       d. Check for KH-005 (CORPT00 omission)
       e. Verify all screen-to-program mappings
       f. Verify all 17 BMS copybooks referenced
       g. Record findings and update progress

    3. PHASE 2 -- FIELD DEFINITION VERIFICATION (Critical):
       a. Select at least 5 screens for spot-checking: COSGN00, CORPT00, COBIL00, COADM01, COUSR01
       b. For each, read the actual BMS file and compare field names, positions, lengths, attributes
       c. Use ground_truth spot-check samples as reference
       d. Flag any fabricated or incorrect field definitions
       e. Record findings and update progress

    4. PHASE 3 -- PF KEY VERIFICATION (Critical):
       a. For each documented screen, extract the claimed PF key assignments
       b. Read the corresponding COBOL program and find EVALUATE EIBAID blocks
       c. Verify WHEN DFHPF* clauses match documented PF keys
       d. Flag undocumented PF keys or documented keys without handlers
       e. Record findings and update progress

    5. PHASE 4 -- NAVIGATION FLOW VERIFICATION (Critical):
       a. Extract all navigation edges from documented flow diagrams
       b. For each edge, find the EXEC CICS XCTL or RETURN TRANSID in the source program
       c. Verify conditional branches (admin vs user from COSGN00C)
       d. Flag fabricated navigation flows
       e. Flag undocumented XCTL/RETURN TRANSID calls
       f. Record findings and update progress

    6. PHASE 5 -- USER JOURNEY VERIFICATION (Major):
       a. Count documented user journeys (minimum 6 required)
       b. Verify each journey step maps to an actual screen and program
       c. Trace paths through XCTL/RETURN TRANSID calls
       d. Check for Transaction Reports journey (CORPT00)
       e. Check error path documentation
       f. Check role-based access documentation
       g. Record findings and update progress

    7. PHASE 6 -- DOCUMENTATION QUALITY (Minor):
       a. Validate Mermaid diagram syntax
       b. Check markdown formatting
       c. Verify screen layout diagrams
       d. Check terminology consistency
       e. Record findings and update progress

    8. SCORING AND REPORT GENERATION:
       a. Calculate scores for each of the 5 categories
       b. Apply weights: Source Reference 35%, Factual 25%, Completeness 20%, Quantitative 10%, Quality 10%
       c. Calculate total weighted score
       d. Determine verdict: PASS (100), FAIL (less than 100)
       e. Generate the final report at docs/reverse-engineering/validation/VL-005-screen-flow-report.md
       f. Include all sections from the output_specification

    9. ALWAYS save progress after each phase to .work/reverse-engineering/validation/vl-005/progress.yaml
  </instruction>
</begin>
```

## Usage

Execute this prompt with Claude Code to validate the BMS Screen Flow documentation produced by RE-005. The prompt will systematically verify every aspect of the screen flow documentation against the actual BMS source files, BMS copybooks, and COBOL programs in the CardDemo codebase.

```bash
# Ensure RE-005 output exists before running validation
ls docs/reverse-engineering/05-specialized/SCREEN-FLOWS.md

# Run the validation
# (paste prompt into Claude Code or reference this file)
```

The validation proceeds through six phases, each targeting a specific aspect of the documentation:

1. **Mapset Inventory Verification** -- Confirms all 17 BMS mapsets are listed with correct count
2. **Field Definition Verification** -- Spot-checks field names, positions, and attributes against BMS source
3. **PF Key Verification** -- Traces PF key assignments through EVALUATE EIBAID blocks in COBOL programs
4. **Navigation Flow Verification** -- Confirms screen transitions via XCTL/RETURN TRANSID calls
5. **User Journey Verification** -- Validates end-to-end paths through the application
6. **Documentation Quality** -- Checks Mermaid syntax, markdown formatting, and terminology

## Expected Output

The validation produces a single report file:

| File | Location | Description |
|------|----------|-------------|
| `VL-005-screen-flow-report.md` | `docs/reverse-engineering/validation/` | Complete validation report with verdict, scores, findings, and recommendations |

The report contains:
- **Verdict** with weighted score (PASS = 100, FAIL < 100)
- **Score Breakdown** across 5 weighted categories (35/25/20/10/10)
- **Mapset Verification Matrix** -- 17-row table showing per-mapset verification status
- **Navigation Flow Verification** -- Each documented flow traced to source
- **User Journey Verification** -- Each journey validated end-to-end
- **Hallucination Inventory** -- All detected fabrications with source evidence
- **Completeness Gaps** -- Missing mapsets, copybooks, journeys, or flows
- **Recommendations** -- Prioritized remediation guidance

Working artifacts are stored in `.work/reverse-engineering/validation/vl-005/` for progress tracking.

## Prerequisites

- RE-005 (Screen Flow Analysis) must have been executed and its output must exist at `docs/reverse-engineering/05-specialized/SCREEN-FLOWS.md`
- The CardDemo source code must be present at the expected codebase location with:
  - 17 BMS mapset files in `app/bms/`
  - 17 BMS copybook files in `app/cpy-bms/`
  - 31 COBOL programs in `app/cbl/` (including 18 online CO* programs)
  - 30 copybooks in `app/cpy/`

## Depends On

| Validation | Reason |
|-----------|--------|
| VL-001 (Domain Model Validation) | Confirmed domain terminology used for consistency checks in Phase 6 |
| VL-002 (Data Model Validation) | Confirmed data structure names for field cross-referencing |

VL-005 runs in **Phase 4** of the validation execution order, in parallel with VL-006, VL-007, and VL-008. It depends on Phase 1 (VL-001 and VL-002) being complete to leverage validated domain terminology and data structure names.

## Blocks

| Validation | Reason |
|-----------|--------|
| VL-000 (Cross-Document Consistency) | VL-000 requires all individual validations to complete before running cross-document checks. Screen flow findings from VL-005 feed into VL-000's consistency analysis across all RE documents. |
