# Prompt Engineering Rubric for Claude Code Prompts

## Overview

This rubric provides reusable patterns and templates for building robust Claude Code prompts that:
- Survive context compaction across long-running tasks
- Handle large files that exceed token limits
- Maintain state and enable reliable resumption
- Follow consistent structural patterns

Use this document as a reference when creating new prompts or enhancing existing ones.

---

## Table of Contents

1. [When to Use These Patterns](#when-to-use-these-patterns)
2. [Core Principles](#core-principles)
3. [Prompt Structure Template](#prompt-structure-template)
4. [Context Compaction Survival Pattern](#context-compaction-survival-pattern)
5. [Large File Handling Pattern](#large-file-handling-pattern)
6. [Avoiding Arbitrary Limits Pattern](#avoiding-arbitrary-limits-pattern)
7. [Progress Tracking Patterns](#progress-tracking-patterns)
8. [Web Research Pattern (Tavily MCP)](#web-research-pattern-tavily-mcp)
9. [Checkpoint Strategies](#checkpoint-strategies)
10. [Begin Section Template](#begin-section-template)
11. [Critical Reminders Template](#critical-reminders-template)
12. [Legacy C++ Codebase Patterns (Golgotha)](#legacy-c-codebase-patterns-golgotha)
    - [Legacy C++ Navigation Pattern](#legacy-c-navigation-pattern)
    - [Cross-Language Tracing Pattern](#cross-language-tracing-pattern)
    - [Platform Abstraction Navigation](#platform-abstraction-navigation)
    - [Build System Understanding Pattern](#build-system-understanding-pattern)
    - [Game Object Hierarchy Pattern](#game-object-hierarchy-pattern)
    - [Subsystem Isolation Pattern](#subsystem-isolation-pattern)
    - [Source Discovery for C++ Pattern](#source-discovery-for-c-pattern)
    - [Enhanced Progress Tracking for Golgotha](#enhanced-progress-tracking-for-golgotha)
    - [Golgotha-Specific Critical Reminders](#golgotha-specific-critical-reminders)
    - [Methodology Templates](#methodology-templates)
13. [Mainframe COBOL Codebase Patterns](#mainframe-cobol-codebase-patterns)
    - [Mainframe COBOL Navigation Pattern](#mainframe-cobol-navigation-pattern)
    - [COBOL-Copybook Dependency Pattern](#cobol-copybook-dependency-pattern)
    - [CICS Transaction Flow Pattern](#cics-transaction-flow-pattern)
    - [BMS Screen Navigation Pattern](#bms-screen-navigation-pattern)
    - [Batch Workflow Pattern](#batch-workflow-pattern)
    - [VSAM File Access Pattern](#vsam-file-access-pattern)
    - [Source Discovery for COBOL Pattern](#source-discovery-for-cobol-pattern)
    - [Enhanced Progress Tracking for Mainframe](#enhanced-progress-tracking-for-mainframe)
    - [Mainframe-Specific Critical Reminders](#mainframe-specific-critical-reminders)
    - [Methodology Templates (COBOL)](#methodology-templates-cobol)
    - [DB2/IMS/MQ Extension Patterns](#db2imsmq-extension-patterns)
    - [CardDemo Quick Reference](#carddemo-quick-reference)
14. [Customisation Guide](#customisation-guide)

---

## When to Use These Patterns

### Always Include Context Compaction Survival When:
- Task involves multiple phases or steps
- Work will take more than 10-15 minutes of Claude time
- Task involves reading/processing multiple source files
- Output involves creating multiple files or documents
- Task has natural checkpoint boundaries (phases, levels, personas, etc.)

### Always Include Large File Handling When:
- Source files may exceed 50KB
- Working with specifications (often large)
- Processing documentation sets
- Reading architecture documents
- Any task where you say "read all the files in..."

### Skip These Patterns When:
- Simple single-file transformations
- Quick Q&A or analysis tasks
- Tasks completable in a single response
- No file I/O required

---

## Core Principles

| Principle | Description |
|-----------|-------------|
| **Disk Over Memory** | Write everything to `.work/` directory; context will be lost but disk persists |
| **Progress After Every Unit** | Update `progress.yaml` after every significant work unit to enable resumption |
| **Summarise Then Discard** | For large files: read chunk → extract key info → write summary → forget chunk |
| **Reference Not Re-read** | Once summarised, reference the summary file; only re-read original for quotes |
| **Clear Next Action** | Always document exactly what to do next for cold resumption |
| **Check Before Starting** | First action is always checking for existing progress; never restart completed work |
| **Complete Then Move** | Finish one unit of work completely before starting another |
| **No Arbitrary Limits** | NEVER use `head -N` or `tail -N` to limit file discovery; process ALL files or show count + warn |
| **Separate Work from Deliverables** | `.work/` is for internal tracking; `docs/` is for final artifacts humans review |

---

## Output Directory Separation

**Critical:** The `.work/` directory is for INTERNAL tracking only. Final deliverables must go to visible directories.

### Directory Purposes

| Directory | Purpose | Visibility | Contents |
|-----------|---------|------------|----------|
| **`.work/`** | Internal tracking, compaction survival | Hidden (dot directory) | `progress.yaml`, inventories, intermediate data |
| **`docs/qa-reports/`** | QA analysis deliverables | Visible, reviewable | `*-REPORT.md` files |
| **`docs/analysis/`** | Code analysis deliverables | Visible, reviewable | Analysis reports, findings |
| **`docs/sysdocs/`** | System documentation | Visible, reviewable | Architecture, API, onboarding docs |

### Standard QA Output Structure

```
repository/
├── .work/                              # INTERNAL - Never deliverables
│   └── qa-analysis/
│       └── {category}/
│           ├── progress.yaml           # Compaction survival
│           ├── *-inventory.yaml        # Intermediate data
│           └── *-findings.yaml         # Raw findings
│
└── docs/                               # DELIVERABLES - Human review
    └── qa-reports/
        └── {category}/
            └── {CATEGORY}-REPORT.md    # Final report
```

### Why This Matters

1. **Dot directories are hidden** - Users won't find reports in file explorers
2. **Source control** - Reports in `docs/` can be committed and tracked
3. **CI/CD integration** - Pipelines can publish `docs/` artifacts easily
4. **Clear separation** - Working state vs deliverables are obviously different

### Research Document Output

For SEC-XX research prompts, final deliverables go to `docs/research/design/`:

| Output | Location | Format |
|--------|----------|--------|
| Research specifications | `docs/research/design/[SYSTEM-NAME]-RESEARCH.md` | RUB-002 format |
| Research index | `docs/research/design/INDEX.md` | Catalog of all research docs |

**Format Specification:** All research documents must follow the format defined in `rubrics/RUB-002-research-document-format.md`, which specifies:
- 10 required sections (Executive Summary through Research Sources)
- Metadata table with version, status, source prompt, GDD section
- Consistent heading numbering (H2=1., H3=1.1)
- Standardized naming: `[SYSTEM-NAME]-RESEARCH.md`

---

## Prompt Structure Template

A well-structured Claude Code prompt follows this pattern:

```xml
# [PROMPT TITLE]

<context>
<project>[Project name and description]</project>
<role>[What role Claude is playing]</role>
<objective>[What this prompt achieves]</objective>
</context>

<foundational_principles>
[Key principles that guide all work - numbered list]
</foundational_principles>

<context_compaction_survival>
[See pattern below]
</context_compaction_survival>

<large_file_handling>
[See pattern below]
</large_file_handling>

<methodology>
[Phases and steps - the actual work to be done]
</methodology>

<output_specifications>
[What files/artifacts to produce and their format]
</output_specifications>

<critical_reminders>
[Key points that must not be forgotten - numbered list]
</critical_reminders>

<begin>
[Instructions for starting/resuming work]
</begin>
```

---

## Context Compaction Survival Pattern

### Template (Copy and Customise)

```xml
<context_compaction_survival>
  <critical_warning>
  THIS WORK WILL SPAN MULTIPLE CONTEXT COMPACTIONS.
  [Description of why this task is extensive].
  You WILL lose context multiple times during this work.
  You MUST implement strategies to survive compaction and resume work correctly.
  </critical_warning>
  
  <work_tracking_directory>
    <path>[OUTPUT_DIR]/.work/</path>
    <purpose>Persistent work state that survives context compaction</purpose>
    <critical>Create this directory FIRST before any other work</critical>
    
    <required_files>
      <file name="progress.yaml">
        <purpose>Track current [phase/level/step] and exactly what to do next</purpose>
        <updated>After EVERY [significant work unit], EVERY [milestone]</updated>
        <critical>MUST be updated frequently - this is your resumption lifeline</critical>
      </file>
      
      <file name="source-discovery.yaml">
        <purpose>Complete catalogue of all source files with sizes</purpose>
        <created>Phase 0 during discovery</created>
        <used_by>All subsequent phases for source lookup</used_by>
      </file>
      
      <!-- Add task-specific tracking files here -->
      <file name="[task-specific].yaml">
        <purpose>[What this tracks]</purpose>
        <created>[When created]</created>
        <format>[Format description]</format>
      </file>
      
      <directory name="source-summaries/">
        <purpose>Summary of each source file</purpose>
        <created>During discovery phase</created>
        <format>One .yaml per source document with key content extracted</format>
      </directory>
      
      <directory name="large-file-summaries/">
        <purpose>Chunked summaries of files too large to read at once</purpose>
        <created>When large files encountered during discovery</created>
        <format>One .yaml per large file with chunk-by-chunk summaries</format>
      </directory>
    </required_files>
  </work_tracking_directory>
  
  <progress_tracking_schema>
```yaml
# progress.yaml - UPDATE AFTER EVERY SIGNIFICANT WORK UNIT
progress:
  last_updated: "[ISO DateTime]"
  current_phase: "[Phase ID]"
  current_step: "[Step ID]"
  status: "In Progress | Blocked | Complete"
  
  # Task-specific phase tracking
  phases:
    phase_0_discovery:
      status: "Not Started | In Progress | Complete"
      # Phase-specific metrics
      
    phase_1_[name]:
      status: "Not Started | In Progress | Complete"
      # Phase-specific metrics
      
  # What's done
  work_completed:
    - item: "[Completed item]"
      completed_at: "[DateTime]"
      
  # What's in progress
  work_in_progress:
    - item: "[Current item]"
      status: "[What's done, what remains]"
      
  # What's remaining
  work_remaining:
    - "[List of pending items]"
    
  # Any blockers
  blockers:
    - "[Any issues preventing progress]"
    
  # CRITICAL: Exactly what to do next
  next_action: "[EXACTLY what to do next when resuming - be specific]"
```
  </progress_tracking_schema>
  
  <resumption_protocol>
  WHEN CONTEXT IS COMPACTED OR SESSION RESUMES:
  
  1. IMMEDIATELY check for existing progress:
     ```bash
     cat [OUTPUT_DIR]/.work/progress.yaml 2>/dev/null || echo "NO_PROGRESS_FILE"
     ```
     
  2. IF progress file exists:
     - Read current_phase, current_step
     - Read next_action (this tells you EXACTLY what to do)
     - Check which [phases/levels/items] are complete
     - Load relevant .work/ files (source-discovery.yaml, summaries as needed)
     - Resume from next_action - do NOT restart from beginning
     - Do NOT re-read source files - use .work/ summaries
     
  3. IF no progress file (fresh start):
     - Initialize .work/ directory structure
     - Begin with Phase 0 (Discovery/Prerequisites)
     
  4. After each significant unit of work:
     - Update progress.yaml immediately
     - Write next_action clearly for potential resumption
     
  5. CHECKPOINT REQUIREMENTS:
     - After EVERY [major deliverable created]
     - After EVERY [phase/level] completed
     - After EVERY [significant milestone]
     - Before ANY [complex operation]
  </resumption_protocol>
  
  <compaction_safe_practices>
    <practice>Write progress.yaml after EVERY [significant work unit]</practice>
    <practice>Write summaries to disk, don't keep in context memory</practice>
    <practice>Reference .work/ files instead of re-reading large sources</practice>
    <practice>Complete one [unit] fully before starting another</practice>
    <practice>Document "next_action" with enough detail to resume cold</practice>
    <practice>Use .work/*.yaml as source of truth, not context memory</practice>
    <practice>Never rely on context to remember what [phases/levels] are done</practice>
  </compaction_safe_practices>
</context_compaction_survival>
```

### Customisation Points

Replace these placeholders when using the template:

| Placeholder | Replace With | Examples |
|-------------|--------------|----------|
| `[OUTPUT_DIR]` | Actual output directory path | `/home/ubuntu/src/project/docs/output` |
| `[significant work unit]` | What constitutes a checkpoint | "spec file", "ADR", "page", "component" |
| `[phase/level/step]` | Your task's hierarchy | "phase", "level", "persona", "stage" |
| `[major deliverable]` | Key outputs | "spec file", "document", "verification" |
| `[task-specific].yaml` | Additional tracking files | "level-status.yaml", "adr-index.yaml" |

---

## Large File Handling Pattern

### Template (Copy and Customise)

```xml
<large_file_handling>
  <critical_warning>
  Some [file type] may exceed token limits and cannot be read in one operation.
  This is especially likely for:
  - [List of file types that tend to be large]
  - [Another type]
  - [Another type]
  You MUST detect and handle large files appropriately.
  </critical_warning>
  
  <detection_strategy>
  During [discovery phase]:
  
  1. Get file sizes for ALL source files:
     ```bash
     find [SOURCE_DIR] -type f \( -name "*.md" -o -name "*.qnt" -o -name "*.yaml" \) -exec ls -la {} \;
     ```
     
  2. Categorise by size:
     - Small: < 50KB (safe to read entirely)
     - Medium: 50-100KB (usually OK, monitor for truncation)
     - Large: > 100KB (requires chunked reading)
     
  3. For large files, calculate estimated chunks:
     - Assume ~300-500 lines per chunk as safe default
     - Use: wc -l [file] to get line count
     - Denser content (specs, code) → smaller chunks (~300 lines)
     - Prose content (docs) → larger chunks (~500 lines)
     
  4. Record in source-discovery.yaml:
```yaml
source_files:
  - file: "[filename]"
    path: "[full path]"
    size_bytes: [size]
    size_category: "small | medium | large"
    line_count: [lines]
    requires_chunked_reading: true | false
    estimated_chunks: [N]  # if large
    content_type: "[description of content]"
    
  large_files_summary:
    count: [N]
    total_size_mb: [size]
    files:
      - "[filename1]"
      - "[filename2]"
```
  </detection_strategy>
  
  <chunked_reading_strategy>
  For files marked as "large":
  
  1. Read file in sections using line ranges:
     ```
     view /path/to/file.md [1, 300]
     view /path/to/file.md [301, 600]
     view /path/to/file.md [601, 900]
     # etc.
     ```
     
  2. After reading EACH chunk, immediately extract:
     - [Key item type 1 relevant to your task]
     - [Key item type 2]
     - [Key item type 3]
     - Cross-references to other files
     
  3. Write chunk summary to large-file-summaries/:
```yaml
# large-file-summaries/[FILE_ID].yaml
file: "[filename]"
path: "[full path]"
total_lines: [N]
total_chunks: [N]
chunks_processed: [N]
fully_summarised: true | false

chunk_summaries:
  - chunk: 1
    lines: "1-300"
    content_type: "[What this chunk contains]"
    key_items:
      - "[Item 1]"
      - "[Item 2]"
    # Task-specific extracted data
    [custom_field]:
      - [extracted data]
      
  - chunk: 2
    lines: "301-600"
    content_type: "[What this chunk contains]"
    key_items:
      - "[Item 3]"
    # ... continue pattern

aggregate_summary:
  total_[items]: [N]
  by_category:
    [category1]: [N]
    [category2]: [N]
  key_topics:
    - "[Topic 1]"
    - "[Topic 2]"
```
  </chunked_reading_strategy>
  
  <using_summaries_for_work>
  When doing work that references large files:
  
  1. FIRST: Read the summary from large-file-summaries/ (small file, fits in context)
  
  2. Use aggregate_summary for high-level information
  
  3. If specific detail needed:
     - Check chunk_summaries to find which chunk has the content
     - Read ONLY that chunk: view [file] [start, end]
     - Extract the specific [item] needed
     - Do NOT keep entire file in context
     
  4. Cite using file + chunk reference:
     "[Source: [filename], Chunk N, Lines X-Y]"
     
  5. For comprehensive outputs:
     - Use aggregate_summary from the summary file
     - Pull specific details chunk by chunk as needed
     - Write output incrementally, saving after each section
     - If compacted, resume from saved progress
  </using_summaries_for_work>
  
  <memory_efficient_patterns>
    <pattern name="Summarise then discard">
      Read chunk → Extract key info → Write to summary file → Move to next chunk
      Don't try to keep entire large file in context.
    </pattern>
    
    <pattern name="Reference not re-read">
      Once summarised, reference the summary file.
      Only re-read original when exact wording/syntax needed.
    </pattern>
    
    <pattern name="Incremental output building">
      For outputs requiring large file content:
      - Write output section by section
      - Save after each section
      - Update progress.yaml with what's done
      - If compacted, resume from saved progress
    </pattern>
    
    <pattern name="Targeted chunk access">
      Need specific item? Don't re-read whole file.
      1. Read summary to find which chunk has it
      2. Read only that chunk
      3. Extract what you need
      4. Discard chunk from context
    </pattern>
  </memory_efficient_patterns>
</large_file_handling>
```

### Size Thresholds Reference

| Category | Size | Line Count (est.) | Handling |
|----------|------|-------------------|----------|
| Small | < 50KB | < 800 lines | Read entirely, still summarise to disk |
| Medium | 50-100KB | 800-1500 lines | Usually OK, summarise anyway, monitor for truncation |
| Large | > 100KB | > 1500 lines | **Chunked reading mandatory** |

### Chunk Size Recommendations

| Content Type | Lines per Chunk | Rationale |
|--------------|-----------------|-----------|
| QUINT specs | 300-400 | Dense, many definitions |
| Code files | 300-400 | Dense, need context |
| Markdown docs | 400-500 | Prose is less dense |
| YAML/JSON | 200-300 | Structured, easy to break |
| Architecture docs | 300-400 | Mixed content |

---

## Avoiding Arbitrary Limits Pattern

### The Problem

Arbitrary limits like `head -20` or `tail -50` in file discovery cause **silent data loss**:
- Files beyond the limit are never processed
- Verification passes on partial data
- Issues in truncated files go undetected
- As the project grows, more data is silently dropped

### The Rule: NEVER Truncate File Discovery

```bash
# ❌ DANGEROUS - Silent data loss
for file in $(find . -name "*.cs" | head -50); do
  process "$file"
done

# ❌ DANGEROUS - Verifies only 20 of potentially 500+ files
for file in $(find "$DIR" -name "*.g.cs" | head -20); do
  verify "$file"
done
```

### Safe Patterns

#### Pattern 1: Process ALL Files (Preferred)
```bash
# ✅ SAFE - Processes everything
for file in $(find . -name "*.cs"); do
  process "$file"
done
```

#### Pattern 2: Show Count + Process All
```bash
# ✅ SAFE - Transparent about volume
TOTAL=$(find . -name "*.cs" | wc -l)
echo "Processing $TOTAL files..."
PROCESSED=0
for file in $(find . -name "*.cs"); do
  ((PROCESSED++))
  echo "[$PROCESSED/$TOTAL] $(basename "$file")"
  process "$file"
done
```

#### Pattern 3: Warn if Multiple When Expecting Single
```bash
# ✅ SAFE - Use when you expect exactly one file
COUNT=$(find "$DIR" -maxdepth 1 -name "*.csproj" | wc -l)
if [ "$COUNT" -eq 0 ]; then
  echo "ERROR: No .csproj found in $DIR"
  exit 1
elif [ "$COUNT" -gt 1 ]; then
  echo "WARNING: Multiple .csproj files in $DIR:"
  find "$DIR" -maxdepth 1 -name "*.csproj"
  echo "Using first one - verify this is correct"
fi
FILE=$(find "$DIR" -maxdepth 1 -name "*.csproj" | head -1)
```

#### Pattern 4: Explicit Sampling (When Justified)
```bash
# ✅ ACCEPTABLE - Only when full processing is impossible
# Must be explicitly justified and transparent
TOTAL=$(find . -name "*.cs" | wc -l)
SAMPLE_SIZE=100

if [ "$TOTAL" -gt "$SAMPLE_SIZE" ]; then
  echo "⚠️ SAMPLING: Checking $SAMPLE_SIZE of $TOTAL files (random sample)"
  echo "   Full verification would take too long"
  FILES=$(find . -name "*.cs" | shuf | head -$SAMPLE_SIZE)
else
  FILES=$(find . -name "*.cs")
fi

for file in $FILES; do
  verify "$file"
done
```

### When head -1 IS Safe

`head -1` is acceptable when extracting a **single value**, not when limiting results:

```bash
# ✅ SAFE - Extracting single value from command output
JAVA_VERSION=$(java -version 2>&1 | head -1)

# ✅ SAFE - Parsing YAML for specific field
STATUS=$(grep "status:" progress.yaml | head -1 | awk '{print $2}')

# ✅ SAFE - Getting first match from grep (intentional)
FIRST_ERROR=$(grep "ERROR" build.log | head -1)
```

### When tail -N IS Safe

`tail -N` is acceptable for **display purposes** (showing recent output):

```bash
# ✅ SAFE - Display only, not processing
echo "Last 20 lines of build output:"
cat build.log | tail -20

# ✅ SAFE - Showing test summary
quint test spec.qnt 2>&1 | tail -20
```

### Checklist for Prompt Authors

Before finalising any prompt, verify:

- [ ] No `head -N` (N>1) in any `find` pipeline
- [ ] No `tail -N` limiting file discovery
- [ ] All file loops process complete results
- [ ] Any single-file selection (`head -1`) warns if multiple found
- [ ] Sampling is explicit, justified, and transparent
- [ ] File counts are displayed before processing

### Real-World Impact

| Pattern | Files Found | Files Processed | Data Loss |
|---------|-------------|-----------------|-----------|
| `find \| head -20` | 509 | 20 | **96%** |
| `find \| head -50` | 509 | 50 | **90%** |
| `find \| head -100` | 509 | 100 | **80%** |
| `find` (no limit) | 509 | 509 | **0%** |

---

## Progress Tracking Patterns

### Basic Progress.yaml Structure

```yaml
progress:
  last_updated: "2025-01-06T10:30:00Z"
  current_phase: "2"
  current_step: "2.3"
  status: "In Progress"
  
  phases:
    phase_0:
      status: "Complete"
      completed_at: "2025-01-06T09:00:00Z"
    phase_1:
      status: "Complete"
      completed_at: "2025-01-06T10:00:00Z"
    phase_2:
      status: "In Progress"
      steps_completed: ["2.1", "2.2"]
      current_step: "2.3"
      
  work_completed:
    - item: "Source discovery"
      completed_at: "2025-01-06T09:00:00Z"
    - item: "File summaries"
      completed_at: "2025-01-06T09:30:00Z"
      
  work_in_progress:
    - item: "Component design - Payment service"
      status: "API defined, implementation pending"
      
  work_remaining:
    - "Component design - Notification service"
    - "Integration testing specs"
    - "Documentation"
    
  blockers: []
  
  next_action: "Complete Payment service implementation in phase 2, step 2.3. Read existing API from .work/payment-api.yaml and generate implementation."
```

### Task-Specific Progress Extensions

#### For Multi-Level Tasks (like 01c cross-context testing)
```yaml
levels:
  level_1:
    status: "Complete"
    specs_created: 5
    counterexamples_found: 2
  level_2:
    status: "In Progress"
    current_context: "Pipeline"
```

#### For Multi-Persona Tasks (like 01f verification)
```yaml
personas:
  security_architect:
    status: "Complete"
    findings_critical: 1
    findings_high: 3
  cost_analyst:
    status: "In Progress"
    sections_reviewed: 3
```

#### For Multi-Phase Architecture (like 01e)
```yaml
phases:
  discovery:
    status: "Complete"
  high_level:
    status: "Complete"
    adrs_created: 5
  detailed:
    status: "In Progress"
    components_designed: 3
    components_remaining: 4
```

---

## Web Research Pattern (Tavily MCP)

### Overview

Tavily MCP provides structured web research capabilities optimized for LLM workflows.
Use these tools instead of generic "search the web" instructions.

### Available Tools

| Tool | Purpose | Use Case |
|------|---------|----------|
| `mcp__tavily__tavily_search` | Web search | Finding current information, docs, tutorials, comparable systems |
| `mcp__tavily__tavily_extract` | Content extraction | Getting full content from known URLs |
| `mcp__tavily__tavily_crawl` | Site crawling | Downloading documentation sites |
| `mcp__tavily__tavily_map` | URL discovery | Finding pages within a site |

### Key Parameters for tavily_search

| Parameter | Description | Default | When to Use |
|-----------|-------------|---------|-------------|
| `query` | Search query (required) | - | Always provide specific, detailed queries |
| `search_depth` | "basic", "advanced", "fast" | "basic" | Use "advanced" for technical topics |
| `topic` | "general", "news", "finance" | "general" | Use "news" for recent developments |
| `max_results` | Number of results | 5 | Increase to 10 for comprehensive research |
| `include_domains` | Restrict to domains | [] | Filter to authoritative sources |
| `exclude_domains` | Block domains | [] | Remove low-quality sources |
| `time_range` | "day", "week", "month", "year" | - | Use for recent information |
| `include_raw_content` | Include full page content | false | Set true when you need full text |

### Template: Research Requirements Section

```xml
<research_requirements>
## Tavily Web Research

### Search Strategy
Use `mcp__tavily__tavily_search` for each topic:

1. **[Topic Area 1]**
   - Query: "[specific search query]"
   - Params: `search_depth: "advanced"`, `max_results: 10`
   - Extract: [what to look for in results]

2. **[Topic Area 2]**
   - Query: "[specific search query]"
   - Params: `topic: "news"`, `time_range: "year"`
   - Extract: [what to look for]

### Documentation Sources
Use `mcp__tavily__tavily_extract` for authoritative docs:
- [Specific URL 1] - [purpose]
- [Specific URL 2] - [purpose]

### Site Crawl (if needed)
Use `mcp__tavily__tavily_crawl` for comprehensive docs:
- Base URL: [documentation site root]
- Purpose: [offline reference, comprehensive coverage]

### Research Workflow
1. Execute tavily_search for each topic area above
2. Review results, identify authoritative sources
3. Use tavily_extract for detailed content from key URLs found
4. Write findings to `.work/[topic]/research-findings.yaml`
5. Reference findings in subsequent design phases
</research_requirements>
```

### Best Practices

1. **Be Specific with Queries**
   - BAD: "medieval games"
   - GOOD: "medieval MMO oath system game mechanics CK3 Manor Lords"

2. **Use Advanced Search for Technical Topics**
   - `search_depth: "advanced"` for comprehensive results
   - `search_depth: "basic"` for quick fact-checking

3. **Domain Filtering for Authoritative Sources**
   - `include_domains: ["docs.unrealengine.com", "dev.epicgames.com"]` for UE5 docs
   - `include_domains: ["gamedeveloper.com", "gdcvault.com"]` for game design

4. **Time-Sensitive Information**
   - Use `time_range: "year"` for recent developments
   - Use `topic: "news"` for current events

5. **Extract After Search**
   - Search finds URLs → Extract gets full content
   - Don't try to get everything from search results alone

6. **Write Findings to Disk**
   - Always save research to `.work/research-findings.yaml`
   - Reference findings in design phases
   - Enables resumption after context compaction

### Example: Complete Research Phase

```markdown
## Phase 1: Research Comparable Systems

Execute Tavily searches:

### 1.1 Search: Game Mechanics
```
mcp__tavily__tavily_search(
  query: "medieval oath system game mechanics fealty CK3 Mount Blade",
  search_depth: "advanced",
  max_results: 10
)
```
Extract key patterns, document in .work/research-findings.yaml

### 1.2 Search: Technical Implementation
```
mcp__tavily__tavily_search(
  query: "MMO server meshing architecture distributed game servers",
  search_depth: "advanced",
  max_results: 10,
  include_domains: ["gamedeveloper.com", "gdcvault.com"]
)
```
Note implementation approaches from industry.

### 1.3 Extract: Key Documentation
```
mcp__tavily__tavily_extract(
  urls: ["https://docs.unrealengine.com/5.0/en-US/networking-overview/"],
  format: "markdown"
)
```
Save UE5-specific implementation details.

### 1.4 Synthesize Findings
Write comprehensive summary to .work/research-findings.yaml:
- Comparable game systems analyzed
- Technical implementation patterns
- Key design decisions from research
- Sources with URLs for reference
```

### Research Findings Schema

```yaml
# .work/[topic]/research-findings.yaml
research:
  last_updated: "[ISO DateTime]"
  queries_executed:
    - query: "[search query]"
      results_count: [N]
      key_findings:
        - "[finding 1]"
        - "[finding 2]"
      sources:
        - title: "[source title]"
          url: "[url]"
          relevance: "[why this matters]"

  extracted_content:
    - url: "[url]"
      extracted_at: "[DateTime]"
      key_points:
        - "[point 1]"
        - "[point 2]"

  synthesis:
    comparable_systems:
      - name: "[system name]"
        relevance: "[why relevant]"
        patterns_to_adopt:
          - "[pattern 1]"
    technical_approaches:
      - approach: "[approach name]"
        pros: ["[pro 1]"]
        cons: ["[con 1]"]
    design_decisions:
      - decision: "[what was decided]"
        rationale: "[why]"
        source: "[where this came from]"
```

---

## Checkpoint Strategies

### When to Checkpoint

| Event | Action |
|-------|--------|
| Phase/Level/Step completed | Update progress.yaml, write summary |
| Major deliverable created | Update progress.yaml, note file path |
| Significant finding discovered | Write to findings file immediately |
| Before complex operation | Save current state |
| After 5-10 minutes of work | Quick progress.yaml update |

### Checkpoint File Naming

```
.work/
├── progress.yaml                    # Always present
├── source-discovery.yaml            # After discovery
├── [phase]-checkpoint.yaml          # After each phase
├── [level]-checkpoint.yaml          # After each level
├── source-summaries/
│   └── [FILE_ID].yaml              # Per source file
├── large-file-summaries/
│   └── [FILE_ID].yaml              # Per large file
└── [task-specific]/
    └── [task-specific-files].yaml  # As needed
```

---

## Begin Section Template

Use this template for the `<begin>` section of any prompt:

```xml
<begin>
=====================================
CRITICAL: CHECK FOR EXISTING PROGRESS FIRST
=====================================
This work may have been started before context compaction.

FIRST ACTION - Check for existing progress:
```bash
cat [OUTPUT_DIR]/.work/progress.yaml 2>/dev/null || echo "NO_PROGRESS_FILE"
```

IF progress file exists:
- Read current_phase, current_step, next_action
- Resume from where you left off
- Do NOT restart from beginning
- Use .work/ summaries, not re-reading sources

IF no progress file (fresh start):
- Proceed with Phase 0 (Discovery/Prerequisites)
- Create .work/ directory structure first

=====================================
CRITICAL: COMPACTION SURVIVAL
=====================================
This work WILL span multiple context compactions.

ALWAYS:
- Write progress to .work/progress.yaml after each significant step
- Write summaries to .work/ directories, not to context memory
- Complete one unit of work fully before starting another
- Document next_action clearly for resumption

=====================================
CRITICAL: LARGE FILE HANDLING
=====================================
Some source files exceed token limits.

For files >100KB:
- Read in chunks of ~300-500 lines
- Summarise each chunk immediately
- Write to .work/large-file-summaries/
- Use summaries for subsequent work, not re-reading original

=====================================
BEGIN NOW
=====================================
FIRST: Check for existing progress (see command above)

IF resuming: Follow next_action from progress.yaml

IF fresh start: 
1. Create .work/ directory structure
2. Run discovery on source files
3. Proceed with Phase 0

[Add any task-specific starting instructions here]
</begin>
```

---

## Critical Reminders Template

Use this template for the `<critical_reminders>` section:

```xml
<critical_reminders>
================================================================================
                    CRITICAL REMINDERS
================================================================================

1. **STATE IN FILES, NOT CONTEXT**
   - progress.yaml is truth
   - Context may compact any time
   - Checkpoint after every [significant unit]

2. **CHECK BEFORE STARTING**
   - Always read progress.yaml first
   - Resume from next_action if exists
   - Never restart completed work

3. **LARGE FILES NEED CHUNKING**
   - Files >100KB require chunked reading
   - Summarise to .work/ as you go
   - Reference summaries, not originals

4. **COMPLETE BEFORE MOVING ON**
   - Finish one [unit] before starting another
   - Write checkpoint before transitions
   - Document what comes next

5. **[TASK-SPECIFIC REMINDER 1]**
   - [Details]

6. **[TASK-SPECIFIC REMINDER 2]**
   - [Details]

[Add more task-specific reminders as needed]

</critical_reminders>
```

---

## Legacy C++ Codebase Patterns (Golgotha)

This section provides specialized patterns for working with the Golgotha legacy C++ game engine codebase (~196,000 lines from late 1990s). Apply these patterns **IN ADDITION TO** the generic patterns above when working on Golgotha.

### When to Apply These Patterns

| Golgotha Challenge | Applicable Pattern |
|--------------------|--------------------|
| 1990s C++ idioms (macros, includes) | Legacy C++ Navigation |
| Scheme/C++ split game logic | Cross-Language Tracing |
| Multiple backends (DX5, Glide, X11) | Platform Abstraction Navigation |
| Custom build system (project.i4) | Build System Understanding |
| Game object inheritance | Game Object Hierarchy |
| 36+ engine subsystems | Subsystem Isolation |
| No modern manifest files | Source Discovery for C++ |

### Large Files Requiring Chunking

These Golgotha files **ALWAYS** require chunked reading:

| File | Lines | Category |
|------|-------|----------|
| `i4/loaders/mp3/layer3.cc` | 2,151 | Codec |
| `golg/objs/stank.cc` | 1,696 | Game object |
| `golg/g1_render.cc` | 1,373 | Rendering |
| `golg/g1_object.cc` | 1,309 | Core system |
| `i4/loaders/jpg/jquant2.cc` | ~1,500 | Codec |

---

### Legacy C++ Navigation Pattern

#### Problem
Golgotha uses 1990s C++ conventions that differ from modern C++:
- Forward declarations instead of includes where possible
- Heavy macro usage (`g1_object_definer`, `S1_SFX`, `li_symbol_ref`)
- Manual memory management (`i4_malloc`, custom allocators)
- Non-standard include paths (quotes, relative to project.i4 paths)
- Versioned serialization (`load_v2`, `load_v3`, ... `load_v9`)

#### Template

```xml
<legacy_cpp_navigation>
  <understanding_conventions>
  **Include Resolution:**
  - Includes use quotes (`"header.hh"`) not angle brackets
  - Search paths defined in `project.i4`: `add_include_directory`
  - Headers may be in peer directories (e.g., `"lisp/lisp.hh"` from golg/)

  **Common Macros to Recognize:**
  | Macro | Purpose | Example |
  |-------|---------|---------|
  | `g1_object_definer<CLASS>` | Register game object type | `g1_object_definer<g1_player_piece_class> def("stank", ...)` |
  | `S1_SFX(name, path, flags, priority)` | Define sound effect | `S1_SFX(gun_fire, "sounds/gun.wav", 0, 10)` |
  | `li_symbol_ref NAME("scheme_name")` | Bind C++ to Lisp symbol | `li_symbol_ref vortex("vortex")` |
  | `i4_profile_class pf_NAME("desc")` | Performance profiling | `i4_profile_class pf_render("rendering")` |

  **Memory Management:**
  - `i4_malloc(size, "reason")` - Custom allocator with tracking
  - `i4_free()` - Matching deallocator
  - Array classes: `i4_array<T>`, `i4_fixed_array<T>`

  **Versioned Serialization:**
  - Load functions: `load_v2()`, `load_v3()`, ... up to current version
  - Each version adds fields progressively
  - Pattern: Check version, call prior loader, then load new fields
  </understanding_conventions>

  <include_tracing_strategy>
  When tracing dependencies:

  1. Extract all `#include` lines from target file
  2. For quoted includes (`"path/header.hh"`):
     - Check same directory first
     - Check `add_include_directory` paths from project.i4
     - Check parent `i4/project.i4` for engine includes
  3. Build dependency map in `.work/include-map/[FILE_ID].yaml`
  </include_tracing_strategy>

  <include_map_schema>
```yaml
# .work/include-map/[FILE_ID].yaml
file: "g1_object.cc"
path: "/golg/g1_object.cc"
direct_includes:
  - header: "g1_object.hh"
    resolved_path: "/golg/g1_object.hh"
    category: "game_core"
  - header: "lisp/lisp.hh"
    resolved_path: "/i4/lisp/lisp.hh"
    category: "engine_lisp"
include_count: 36
categories:
  game_core: 12
  engine_lisp: 5
  engine_math: 4
  engine_file: 3
  game_objects: 12
```
  </include_map_schema>

  <macro_registry_schema>
```yaml
# .work/macro-registry.yaml
macros:
  g1_object_definer:
    type: "object_registration"
    pattern: "g1_object_definer<CLASS>(\"name\", FLAGS, init)"
    files_using: ["stank.cc", "peon_tank.cc", "helicopter.cc"]

  S1_SFX:
    type: "sound_effect"
    pattern: "S1_SFX(name, \"path\", flags, priority)"
    files_using: ["stank.cc", "sound_man.cc"]

  li_symbol_ref:
    type: "lisp_symbol_binding"
    pattern: "li_symbol_ref NAME(\"scheme_name\")"
    files_using: ["g1_object.cc", "tile.cc"]
```
  </macro_registry_schema>
</legacy_cpp_navigation>
```

---

### Cross-Language Tracing Pattern

#### Problem
Golgotha's game logic is split between:
- **Scheme files** (`golg/scheme/*.scm`): Define classes, balance, models
- **C++ implementation**: Actual object behavior in `golg/objs/*.cc`

The linkage is **NAME-BASED**: the string name in C++ matches the symbol in Scheme.

#### Template

```xml
<cross_language_tracing>
  <linkage_overview>
  | Scheme File | Purpose | Linkage Method |
  |-------------|---------|----------------|
  | `classes.scm` | Object variable definitions | `(def_class NAME_vars ...)` |
  | `balance.scm` | Game balance parameters | `(defaults (NAME speed turn ...))` |
  | `models.scm` | Model configurations | Symbolic references |
  | `start.scm` | Startup initialization | Function calls |

  C++ uses the SAME NAMES:
  - `g1_object_definer<CLASS> NAME_def("NAME", ...)` registers object
  - `g1_get_object_type("NAME")` retrieves type at runtime
  </linkage_overview>

  <tracing_workflow>
  To trace a game object (e.g., "stank"):

  **Step 1: Find Scheme Definition**
  ```bash
  grep -n "stank" golg/scheme/classes.scm golg/scheme/balance.scm
  ```
  - `classes.scm`: Variable definitions (`def_class stank_vars`)
  - `balance.scm`: Parameters (`defaults (stank ...)`)

  **Step 2: Find C++ Registration**
  ```bash
  grep -rn "g1_object_definer.*stank\|\"stank\"" golg/objs/
  ```

  **Step 3: Document Linkage**
  Write to `.work/object-linkage/[OBJECT_NAME].yaml`
  </tracing_workflow>

  <object_linkage_schema>
```yaml
# .work/object-linkage/stank.yaml
object_name: "stank"
scheme_definitions:
  classes:
    file: "golg/scheme/classes.scm"
    line: 145
    content: "(def_class stank_vars nil ...)"
    variables:
      - attach_pos
      - smoke_type
      - damage_per_tick
  balance:
    file: "golg/scheme/balance.scm"
    line: 23
    content: "(defaults (stank 0.3 0.2 0.1 10000 ...))"
    params: [speed, turn_speed, ...]
cpp_implementation:
  registration:
    file: "golg/objs/stank.cc"
    line: 83
    pattern: "g1_object_definer<g1_player_piece_class> g1_supertank_def(\"stank\", ...)"
  class: "g1_player_piece_class"
  init_function: "g1_supertank_init"
  key_methods:
    - think
    - draw
    - damage
```
  </object_linkage_schema>

  <lisp_binding_tracing>
  C++ binds to Lisp symbols using `li_symbol_ref`:

  ```cpp
  // In C++:
  static li_symbol_ref vortex("vortex"), explosion1("explosion1");

  // Usage:
  li_call(vortex.get(), params);
  ```

  When you see `li_symbol_ref`:
  1. Search `.scm` files for the symbol name
  2. Document in `.work/lisp-bindings.yaml`

```yaml
# .work/lisp-bindings.yaml
bindings:
  - symbol: "vortex"
    cpp_declaration:
      file: "golg/g1_object.cc"
      line: 59
    scheme_definition:
      file: "golg/scheme/balance.scm"
      line: 89
    purpose: "Vortex visual effect"
```
  </lisp_binding_tracing>

  <message_system_tracing>
  Objects communicate via messages (see `golg/messages.txt`):

  ```cpp
  virtual li_object *message(li_symbol *message_name,
                             li_object *message_params,
                             li_environment *env);
  ```

  Document message flows in `.work/message-flow.yaml`:
```yaml
# .work/message-flow.yaml
messages:
  build:
    senders: ["border_frame", "factory"]
    receivers: ["garage", "airbase", "mainbasepad"]
    params: "list(type, cursor_x, cursor_y)"
    returns: "0 or non-0"

  convoy_deployed:
    senders: ["convoy"]
    receivers: ["factory"]
    params: "li_g1_objref (convoy pointer)"
```
  </message_system_tracing>
</cross_language_tracing>
```

---

### Platform Abstraction Navigation

#### Problem
Golgotha supports multiple platforms and rendering backends. Understanding which code applies when is critical.

#### Architecture Overview

```
┌──────────────────────────────────────────────────┐
│  Game Code (golg/)                               │
│  Uses: r1_render_api_class, i4_display_class     │
└─────────────────────┬────────────────────────────┘
                      │ Abstract Interface
┌─────────────────────▼────────────────────────────┐
│  Render API (render/r1_api.hh)                   │
│  - r1_render_api_class: Abstract rendering       │
│  - r1_texture_manager_class: Texture handling    │
└────────┬──────────────┬──────────────┬───────────┘
         │              │              │
┌────────▼────┐ ┌───────▼──────┐ ┌────▼─────────┐
│ render/dx5/ │ │ render/glide │ │ render/soft  │
│ DirectX 5   │ │ 3Dfx Glide   │ │ Software     │
└─────────────┘ └──────────────┘ └──────────────┘

┌──────────────────────────────────────────────────┐
│  Display API (i4/video/display.hh)               │
│  - i4_display_class: Window/screen abstraction   │
└────────┬──────────────┬──────────────────────────┘
         │              │
┌────────▼────┐ ┌───────▼──────┐
│ i4/video/   │ │ i4/video/    │
│ win32/      │ │ x11/         │
│ DirectX GDI │ │ X Window     │
└─────────────┘ └──────────────┘
```

#### Template

```xml
<platform_abstraction_navigation>
  <platform_detection_strategy>
  When working on platform-specific code:

  1. **Check project.i4 for conditionals:**
     ```
     win32 add_to_executable "file/win32/win_file.cc"
     linux add_to_executable "file/linux/linux_file.cc"
     ```

  2. **Map file to platform in `.work/platform-map.yaml`**

  3. **Identify the active backend for a feature:**
     - Display: `i4_display_list` contains registered displays
     - Rendering: `r1_render_api_class::first` links render APIs
     - Selection at runtime via name matching
  </platform_detection_strategy>

  <platform_map_schema>
```yaml
# .work/platform-map.yaml
files_by_platform:
  win32_only:
    - "i4/file/win32/win_file.cc"
    - "i4/video/win32/directx.cc"
    - "i4/threads/win32_threads.cc"
  linux_only:
    - "i4/file/linux/linux_file.cc"
    - "i4/video/x11/x11devs.cc"
    - "i4/threads/linux/threads.cc"
  cross_platform:
    - "i4/file/file.cc"
    - "i4/video/display.cc"

rendering_backends:
  dx5:
    directory: "render/dx5/"
    platform: "win32"
    base_class: "r1_render_api_class"
  glide:
    directory: "render/glide/"
    platform: "win32"
    base_class: "r1_render_api_class"
  software:
    directory: "render/software/"
    platform: "cross_platform"
    base_class: "r1_render_api_class"

display_backends:
  x11:
    directory: "i4/video/x11/"
    platform: "linux"
  win32:
    directory: "i4/video/win32/"
    platform: "win32"
```
  </platform_map_schema>

  <backend_isolation_strategy>
  When working on ONE backend (e.g., X11):

  1. Focus on the specific directory (e.g., `i4/video/x11/`)
  2. Read the base interface first (`i4/video/display.hh`)
  3. Check which abstract methods are overridden
  4. Ignore other backend directories unless comparing

  Document in `.work/backend-focus.yaml`:
```yaml
current_backend_focus:
  category: "display"
  backend: "x11"
  base_interface: "i4/video/display.hh"
  implementation_dir: "i4/video/x11/"
  key_files:
    - "x11devs.cc"
  related_backends_ignored:
    - "win32/"
    - "glide/"
```
  </backend_isolation_strategy>
</platform_abstraction_navigation>
```

---

### Build System Understanding Pattern

#### Problem
Golgotha uses a custom build system (`i4_make`) with `project.i4` configuration files that have non-standard syntax.

#### Template

```xml
<build_system_understanding>
  <project_i4_syntax>
  Key constructs in project.i4 files:

  | Construct | Purpose | Example |
  |-----------|---------|---------|
  | `[lib name]` | Define a library | `[lib gui]` |
  | `[executable name]` | Define executable | `[executable golg]` |
  | `[list name]` | Define file list | `[list editor]` |
  | `use lib_name` | Include library | `use gui` |
  | `use file=path lib` | Cross-file include | `use file=../i4/project.i4 base` |
  | `"file.cc"` | Add source file | `"sound/sound.cc"` |
  | `add_to_executable` | Force into exe | `add_to_executable "main.cc"` |
  | `win32 / linux` | Platform conditional | `win32 "win_main.cc"` |
  | `ram_file` | Embed resource | `ram_file "resource/font.tga"` |
  | `add_include_directory` | Add include path | `add_include_directory .` |
  </project_i4_syntax>

  <dependency_tracing>
  To understand what goes into an executable:

  1. Read the project.i4 file, find `[executable name]`
  2. Follow `use` directives recursively
  3. Build dependency tree in `.work/build-tree.yaml`
  </dependency_tracing>

  <build_tree_schema>
```yaml
# .work/build-tree.yaml
golg:
  type: executable
  project_file: "golg/project.i4"
  direct_dependencies:
    - name: "libs_used"
      type: list
      expands_to:
        - "file=../i4/project.i4 app"
        - "file=../i4/project.i4 jpg_load"
        - "file=../render/project.i4 render"
  direct_sources:
    - "camera.cc"
    - "map_view.cc"
    - "g1_render.cc"
  platform_specific:
    win32:
      - "render/software/win32_specific.cc"
    linux:
      - "i4/time/unixtime.cc"
  ram_files:
    - "resource/golg_font.tga"
    - "resource/cursor.tga"
```
  </build_tree_schema>

  <finding_file_ownership>
  To find which target includes a specific file:

  ```bash
  grep -rn "filename.cc" --include="project.i4" .
  ```

  Trace back through `use` dependencies to find the executable.
  </finding_file_ownership>
</build_system_understanding>
```

---

### Game Object Hierarchy Pattern

#### Problem
The game object system is central to Golgotha. Understanding the hierarchy and message system is essential.

#### Class Hierarchy

```
g1_object_class (g1_object.hh)
├── Core data: x, y, h, theta, pitch, roll, health
├── Flags: SELECTED, THINKING, BLOCKING, TARGETABLE...
├── Virtual methods: think(), draw(), damage()
│
├── g1_mini_object (attached sub-objects)
│   └── offset, rotation, frame, animation
│
└── Derived classes (in golg/objs/):
    ├── g1_map_piece (map_piece.cc) - Objects on terrain
    │   └── g1_player_piece_class - Player-controlled
    │       ├── stank (supertank)
    │       ├── peon_tank
    │       ├── helicopter
    │       └── ...
    ├── g1_path_object (path_object.cc) - Pathfinding nodes
    ├── Buildings (bases.cc, garage, airbase)
    └── Projectiles (guided_missile.cc, bullet.cc)
```

#### Template

```xml
<game_object_hierarchy>
  <object_lifecycle>
  Understanding object creation and destruction:

  **1. Registration (at startup):**
  ```cpp
  g1_object_definer<g1_player_piece_class>
    g1_supertank_def("stank", FLAGS, g1_supertank_init);
  ```

  **2. Creation (at runtime):**
  ```cpp
  g1_object_class *obj = g1_create_object(g1_supertank_type);
  ```

  **3. Placement:**
  ```cpp
  obj->occupy_location();  // Add to map
  obj->request_think();    // Add to think list
  ```

  **4. Game Loop:**
  ```cpp
  obj->think();            // Called each tick
  obj->draw(context);      // Called each frame
  ```

  **5. Destruction:**
  ```cpp
  obj->request_remove();   // Mark for removal
  obj->unoccupy_location(); // Remove from map
  ```
  </object_lifecycle>

  <message_system>
  Objects communicate via Lisp-style messages:

  ```cpp
  // Sending
  obj->message(li_get_symbol("build"), params, env);

  // Receiving (override in subclass)
  li_object *my_class::message(li_symbol *name,
                               li_object *params,
                               li_environment *env) {
    if (name == build_sym.get()) {
      // Handle build message
    }
    return g1_object_class::message(name, params, env);
  }
  ```

  Reference `golg/messages.txt` for message documentation.
  </message_system>

  <flag_reference>
  Common object flags (from g1_object.hh):

  | Flag | Bit | Purpose |
  |------|-----|---------|
  | SELECTED | 1<<0 | Player has selected this object |
  | THINKING | 1<<1 | Object is in think list |
  | BLOCKING | 1<<14 | Blocks other objects' movement |
  | TARGETABLE | 1<<7 | Can be attacked |
  | GROUND | 1<<8 | Ground unit |
  | AERIAL | 1<<10 | Air unit |
  </flag_reference>
</game_object_hierarchy>
```

---

### Subsystem Isolation Pattern

#### Problem
Golgotha has 36+ subsystems in `i4/`. Working on one (e.g., MP3 loader) shouldn't require understanding all dependencies.

#### Template

```xml
<subsystem_isolation>
  <subsystem_catalog_schema>
```yaml
# .work/subsystem-catalog.yaml
subsystems:
  lisp:
    directory: "i4/lisp/"
    files: 8
    purpose: "Scheme interpreter"
    key_files: ["lisp.cc", "li_class.cc", "li_types.cc"]
    public_headers: ["lisp.hh", "li_class.hh"]

  sound:
    directory: "i4/sound/"
    files: 3
    purpose: "Audio system"
    depends_on: ["loaders"]
    platform_specific:
      win32: ["dsound/"]

  loaders:
    directory: "i4/loaders/"
    files: 20+
    purpose: "Asset loading"
    sub_categories:
      image: ["bmp_load.cc", "tga_load.cc", "jpg/"]
      audio: ["wav_load.cc", "mp3/"]

  video:
    directory: "i4/video/"
    purpose: "Display abstraction"
    platform_specific:
      linux: ["x11/"]
      win32: ["win32/"]
```
  </subsystem_catalog_schema>

  <isolation_strategy>
  When working on a specific subsystem:

  1. **Identify boundaries:**
     - Entry point directory
     - Public interface headers
     - Internal implementation files

  2. **Read only what's needed:**
     - Start with public headers
     - Read `.cc` only when implementation details needed
     - Don't load dependent subsystems unless required

  3. **Track focus in `.work/subsystem-focus.yaml`:**
```yaml
focus:
  subsystem: "mp3"
  directory: "i4/loaders/mp3/"
  public_interface_read: true
  internal_files_read:
    - "layer3.cc"  # 2151 lines - chunked
  dependencies_needed:
    - "file/file.hh"
  dependencies_ignored:
    - "sound/"
```

  4. **Apply large file chunking for internals**
  </isolation_strategy>
</subsystem_isolation>
```

---

### Source Discovery for C++ Pattern

#### Problem
Unlike modern projects with `package.json`, Golgotha requires different discovery approaches.

#### Template

```xml
<source_discovery_cpp>
  <discovery_commands>
  ```bash
  # Total file count
  find . -type f \( -name "*.cc" -o -name "*.hh" \) | wc -l
  # Expected: ~1,006 files

  # Files by directory
  find . -type f -name "*.cc" -printf '%h\n' | sort | uniq -c | sort -rn

  # Large files (>50KB)
  find . -type f \( -name "*.cc" -o -name "*.hh" \) -size +50k -exec ls -lh {} \;
  ```
  </discovery_commands>

  <discovery_schema>
```yaml
# .work/source-discovery.yaml
discovery:
  last_updated: "2026-01-23T..."
  total_files: 1006
  by_extension:
    ".cc": 583
    ".hh": 423
  by_directory:
    golg/:
      total: 180
      subdirs:
        objs/: 85
        editor/: 45
    i4/:
      total: 320
      subdirs:
        lisp/: 12
        loaders/: 45
        video/: 30
    render/:
      total: 60
  large_files:
    - path: "i4/loaders/mp3/layer3.cc"
      size_kb: 58
      lines: 2151
    - path: "golg/objs/stank.cc"
      size_kb: 45
      lines: 1696
```
  </discovery_schema>

  <entry_points>
  Key entry points by task type:

  | Task | Start Here | Then Trace |
  |------|------------|------------|
  | Game objects | `golg/g1_object.hh` | `golg/objs/` implementations |
  | Lisp integration | `i4/lisp/lisp.hh` | `golg/scheme/*.scm` |
  | Rendering | `render/r1_api.hh` | `render/glide/`, `render/software/` |
  | Display | `i4/video/display.hh` | `i4/video/x11/`, `i4/video/win32/` |
  | Build system | `i4/project.i4` | `golg/project.i4` |
  | Game startup | `golg/menu.cc` | `golg/controller.cc` |
  </entry_points>

  <header_source_pairing>
  In Golgotha, headers and sources are co-located:
  ```
  golg/objs/stank.hh  ← Declaration
  golg/objs/stank.cc  ← Implementation
  ```
  When you find a `.cc`, check for matching `.hh` and vice versa.
  </header_source_pairing>
</source_discovery_cpp>
```

---

### Enhanced Progress Tracking for Golgotha

Add a `golgotha_context` section to progress.yaml:

```yaml
# progress.yaml - Golgotha-specific extensions
progress:
  last_updated: "2026-01-23T14:30:00Z"
  current_phase: "2"
  current_step: "2.3"
  status: "In Progress"

  # Standard phase tracking (from generic rubric)
  phases:
    phase_0_discovery:
      status: "Complete"
    # ...

  # GOLGOTHA-SPECIFIC TRACKING
  golgotha_context:
    # Target platform for this session
    target_platform: "linux"
    relevant_backends:
      display: "x11"
      rendering: "software"

    # Object system understanding
    objects_analyzed:
      - name: "stank"
        scheme_file: "golg/scheme/classes.scm"
        cpp_file: "golg/objs/stank.cc"
        linkage_verified: true

    # Lisp/C++ bindings traced
    lisp_bindings_mapped:
      - symbol: "vortex"
        scheme_def: "golg/scheme/balance.scm:52"
        cpp_usage: "golg/g1_object.cc:59"

    # Build dependencies resolved
    build_context:
      target: "golg"
      project_file: "golg/project.i4"
      dependencies_traced: 12

    # Large file chunking status
    large_files:
      - file: "golg/objs/stank.cc"
        total_lines: 1696
        chunks_processed: 4
        fully_summarised: false

    # Message system mapping
    messages_documented:
      - "build"
      - "convoy_deployed"

  next_action: "Continue analyzing lisp bindings in stank.cc, chunk 5 of 6"
```

---

### Golgotha-Specific Critical Reminders

Add these reminders when working on Golgotha:

```xml
<critical_reminders_golgotha>
================================================================================
                    CRITICAL REMINDERS - GOLGOTHA CODEBASE
================================================================================

1. **LISP/C++ LINKAGE IS NAME-BASED**
   - Object name in `g1_object_definer` matches Scheme symbol
   - Always trace both languages for game objects
   - Document linkages in `.work/object-linkage/`

2. **PLATFORM CONDITIONALS MATTER**
   - Check `win32 { }` vs `linux { }` in project.i4
   - Don't mix platform-specific code analysis
   - Track target platform in progress.yaml

3. **36+ I4 SUBSYSTEMS - ISOLATE**
   - Don't try to understand everything at once
   - Focus on one subsystem at a time
   - Read public headers first, internals only when needed

4. **INCLUDE PATHS ARE NON-STANDARD**
   - Check `add_include_directory` in project.i4
   - Quoted includes search relative to source
   - Cross-project: `file=../i4/project.i4`

5. **MESSAGE-BASED OBJECT COMMUNICATION**
   - Objects use `->message()` not direct calls
   - Reference `golg/messages.txt` for documentation
   - Map senders to receivers in `.work/message-flow.yaml`

6. **THESE FILES ALWAYS NEED CHUNKING**
   - layer3.cc (2,151 lines)
   - stank.cc (1,696 lines)
   - g1_render.cc (1,373 lines)
   - g1_object.cc (1,309 lines)

7. **TRACE BOTH LANGUAGES FOR GAME OBJECTS**
   - Scheme: classes.scm, balance.scm
   - C++: golg/objs/*.cc
   - Link via object name strings

8. **CHECK golg/messages.txt FOR MESSAGE DOCS**
   - 30 message types documented
   - Parameters and return values specified
</critical_reminders_golgotha>
```

---

### Methodology Templates

#### Template: Analyzing a Game Object

```xml
<methodology_object_analysis>
## Phase 1: Discover Object Definition
1. Find object name registration:
   ```bash
   grep -rn "g1_object_definer.*\"OBJECT_NAME\"" golg/
   ```
2. Document in `.work/object-linkage/OBJECT_NAME.yaml`

## Phase 2: Trace Scheme Integration
1. Search balance.scm for defaults:
   ```bash
   grep -n "OBJECT_NAME" golg/scheme/balance.scm
   ```
2. Search classes.scm for variable definitions
3. Add to linkage file

## Phase 3: Analyze C++ Implementation
1. Read header file for interface
2. Chunk-read implementation if large (>800 lines)
3. Document key methods: init(), think(), draw(), damage()

## Phase 4: Trace Message Handling
1. Find message() override
2. Map which messages are handled
3. Document in `.work/message-flow.yaml`

## Phase 5: Identify Dependencies
1. Extract #include list
2. Categorize: engine, game, objects
3. Document critical dependencies only
</methodology_object_analysis>
```

#### Template: Working on a Platform Backend

```xml
<methodology_platform_backend>
## Phase 1: Identify Scope
1. Confirm target platform (win32/linux)
2. Identify backend category (display/render/sound)
3. Locate implementation directory

## Phase 2: Understand Interface
1. Read base class header (e.g., display.hh)
2. Document abstract methods to implement
3. Note virtual vs pure virtual

## Phase 3: Analyze Implementation
1. Read implementation in platform directory
2. Check project.i4 for platform conditionals
3. Ignore other platform implementations

## Phase 4: Document Platform Map
1. Update `.work/platform-map.yaml`
2. Track which files are platform-specific
3. Note any shared utilities
</methodology_platform_backend>
```

---

## Mainframe COBOL Codebase Patterns

This section provides specialized patterns for working with mainframe COBOL codebases like AWS CardDemo. Apply these patterns **IN ADDITION TO** the generic patterns above when working on mainframe applications.

### When to Apply These Patterns

| Mainframe Challenge | Applicable Pattern |
|--------------------|-------------------|
| CICS online transactions | CICS Transaction Flow Pattern |
| Copybook dependencies | COBOL-Copybook Dependency Pattern |
| BMS screen handling | BMS Screen Navigation Pattern |
| JCL batch workflows | Batch Workflow Pattern |
| VSAM file access | VSAM File Access Pattern |
| Program naming conventions | Mainframe COBOL Navigation Pattern |
| DB2/IMS/MQ integration | DB2/IMS/MQ Extension Patterns |

### Large Files Requiring Chunking

These CardDemo files **ALWAYS** require chunked reading:

| File | Lines | Size | Category |
|------|-------|------|----------|
| `app/cbl/COACTUPC.cbl` | 4,236 | 179KB | Account update (complex validation) |
| `app/cbl/COCRDLIC.cbl` | 1,459 | 115KB | Card listing |
| `app/cbl/COCRDUPC.cbl` | 1,560 | 124KB | Card update |
| `app/cpy/CSLKPCDY.cpy` | ~1,000 | 51KB | Lookup tables (phone/state codes) |

---

### Mainframe COBOL Navigation Pattern

#### Problem
Mainframe COBOL codebases use conventions unfamiliar to modern developers:
- Fixed-format source with column significance
- Copybooks for shared data structures
- Naming conventions encoding program type and function
- Separation of online (CICS) and batch processing

#### Template

```xml
<mainframe_cobol_navigation>
  <understanding_conventions>
  **Program Naming Conventions:**
  | Prefix | Type | Description | Example |
  |--------|------|-------------|---------|
  | `CO*` | Online | CICS transaction programs | COSGN00C (signon), COACTUPC (account update) |
  | `CB*` | Batch | Batch processing programs | CBTRN02C (transaction posting) |
  | `*C` | Suffix | COBOL program files | All `.cbl` files end in 'C' |
  | `*Y` | Suffix | Copybooks | COCOM01Y, CVACT01Y |

  **Functional Prefixes (after CO/CB):**
  | Code | Function | Programs |
  |------|----------|----------|
  | SGN | Signon/authentication | COSGN00C |
  | ADM | Admin menu | COADM01C |
  | MEN | User menu | COMEN01C |
  | ACT | Account operations | COACTVWC, COACTUPC |
  | CRD | Card operations | COCRDLIC, COCRDSLC, COCRDUPC |
  | TRN | Transaction operations | COTRN00C, COTRN01C, COTRN02C |
  | BIL | Bill payment | COBIL00C |
  | USR | User management | COUSR00C-03C |

  **Directory Structure:**
  ```
  app/
  ├── cbl/          # COBOL programs (39 files)
  ├── cpy/          # Copybooks - shared data structures (41 files)
  ├── bms/          # BMS mapsets - CICS screen definitions (21 files)
  ├── jcl/          # JCL batch job control (38 files)
  ├── asm/          # Assembler utilities
  └── data/         # Data files (ASCII and EBCDIC)
  ```

  **COBOL Column Sensitivity:**
  | Columns | Purpose | Content |
  |---------|---------|---------|
  | 1-6 | Sequence numbers | Ignore (legacy) |
  | 7 | Indicator | `*` = comment, `-` = continuation |
  | 8-11 | Area A | Division, section, paragraph names |
  | 12-72 | Area B | Statements, data definitions |
  | 73-80 | Identification | Ignore (legacy) |
  </understanding_conventions>

  <program_classification_schema>
```yaml
# .work/program-classification.yaml
programs:
  online_cics:
    - name: "COSGN00C"
      function: "Signon/authentication"
      transaction_id: "CC00"
      bms_mapset: "COSGN00"
    - name: "COACTUPC"
      function: "Account update"
      transaction_id: "CA02"
      bms_mapset: "COACTUP"

  batch:
    - name: "CBTRN02C"
      function: "Post daily transactions"
      jcl_job: "POSTTRAN.jcl"
      files_accessed: ["DALYTRAN", "TRANFILE", "ACCTFILE"]

copybooks:
  universal:
    - name: "COCOM01Y"
      purpose: "COMMAREA - inter-program communication"
      used_by: "100% of online programs"
  data_structures:
    - name: "CVACT01Y"
      purpose: "Account record layout"
      vsam_file: "ACCTDAT"
```
  </program_classification_schema>
</mainframe_cobol_navigation>
```

---

### COBOL-Copybook Dependency Pattern

#### Problem
COBOL programs share data structures through copybooks. Understanding which copybooks are essential vs. optional is critical for context management.

#### Template

```xml
<cobol_copybook_dependency>
  <copybook_categories>
  **Category 1: Universal (Always Load First)**
  Essential for understanding any online program:
  - COCOM01Y.cpy - CARDDEMO-COMMAREA (inter-program state)
  - COTTL01Y.cpy - Application titles
  - CSDAT01Y.cpy - Date formatting utilities
  - CSMSG01Y.cpy - Standard messages

  **Category 2: Data Structure (Load Based on File Access)**
  Record layouts for VSAM files:
  - CVACT01Y.cpy - Account record (ACCTDAT)
  - CVACT02Y.cpy - Card record (CARDDAT)
  - CVACT03Y.cpy - Cross-reference record (CCXREF)
  - CVCUS01Y.cpy - Customer record (CUSTDAT)
  - CVTRA05Y.cpy - Transaction record (TRANSACT)

  **Category 3: Screen-Related (Load for UI Analysis)**
  BMS-generated structures:
  - COSGN00.cpy - Signon screen fields
  - COACTUP.cpy - Account update screen fields
  - Pattern: Copybook name matches BMS mapset name

  **Category 4: Utility (Defer Loading - Large)**
  Specialized lookup/validation:
  - CSLKPCDY.cpy (51KB) - Phone area codes, state codes
  - CSUTLDWY.cpy - Validation working storage
  - CSSTRPFY.cpy - String parsing functions

  **CICS Framework (Reference Only):**
  - DFHAID.cpy - Function key constants (DFHENTER, DFHPF3, etc.)
  - DFHBMSCA.cpy - BMS attribute bytes
  </copybook_categories>

  <copybook_injection_strategy>
  **Priority Loading Order:**

  1. **PRIORITY 1 - Always load for online programs:**
     COCOM01Y.cpy (48 lines) - Essential COMMAREA context

  2. **PRIORITY 2 - Load based on data access:**
     IF program reads accounts: Load CVACT01Y.cpy
     IF program reads cards: Load CVACT02Y.cpy, CVCRD01Y.cpy
     IF program reads customers: Load CVCUS01Y.cpy
     IF program reads transactions: Load CVTRA05Y.cpy

  3. **PRIORITY 3 - Load for screen analysis:**
     Load BMS-matching copybook (e.g., COSGN00.cpy for COSGN00C.cbl)

  4. **DEFER LOADING:**
     CSLKPCDY.cpy - Only when analyzing validation logic
     DFHAID.cpy - Reference, don't load (known constants)
  </copybook_injection_strategy>

  <copy_tracing_procedure>
  To trace copybook dependencies:

  1. Extract all COPY statements from target program:
     ```bash
     grep -n "^\s*COPY" app/cbl/COSGN00C.cbl
     ```

  2. Categorize each copybook by type

  3. Build dependency map in .work/:
```yaml
# .work/copybook-dependencies/COSGN00C.yaml
program: "COSGN00C"
copybooks:
  universal:
    - name: "COCOM01Y"
      line: 48
      purpose: "COMMAREA"
  data:
    - name: "CSUSR01Y"
      line: 55
      purpose: "User security record"
  screen:
    - name: "COSGN00"
      line: 50
      purpose: "Signon screen fields"
  framework:
    - name: "DFHAID"
      line: 57
    - name: "DFHBMSCA"
      line: 58
```
  </copy_tracing_procedure>
</cobol_copybook_dependency>
```

---

### CICS Transaction Flow Pattern

#### Problem
CICS programs use a pseudo-conversational model that differs from typical request/response patterns. Understanding this flow is essential for analyzing online programs.

#### Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    CICS Pseudo-Conversational Flow               │
└─────────────────────────────────────────────────────────────────┘

    [User at Terminal]
           │
           ▼
    ┌──────────────┐
    │ CICS Receives │
    │   Input       │
    └──────┬───────┘
           │
           ▼
    ┌──────────────┐     EIBCALEN = 0?
    │   Program    │────────────────────► [First Entry]
    │   Starts     │                           │
    └──────┬───────┘                           ▼
           │                           Initialize COMMAREA
           │ EIBCALEN > 0                     │
           ▼                                   ▼
    ┌──────────────┐                    ┌──────────────┐
    │ RECEIVE MAP  │                    │  SEND MAP    │
    │ (Get Input)  │                    │ (Show Screen)│
    └──────┬───────┘                    └──────┬───────┘
           │                                   │
           ▼                                   ▼
    ┌──────────────┐                    ┌──────────────┐
    │ EVALUATE     │                    │   RETURN     │
    │ EIBAID       │                    │ with TRANSID │
    └──────┬───────┘                    │ & COMMAREA   │
           │                            └──────┬───────┘
           ├── DFHENTER ──► Process Input           │
           ├── DFHPF3 ────► Exit/Return             │
           └── OTHER ─────► Invalid Key             ▼
                                         [Wait for User]
                                              │
                                              └──────► [Loop Back]
```

#### Template

```xml
<cics_transaction_flow>
  <key_cics_commands>
  | Command | Purpose | Example |
  |---------|---------|---------|
  | SEND MAP | Display screen to terminal | `EXEC CICS SEND MAP('COSGN0A') MAPSET('COSGN00')` |
  | RECEIVE MAP | Get user input from screen | `EXEC CICS RECEIVE MAP('COSGN0A') MAPSET('COSGN00')` |
  | RETURN | End transaction, save state | `EXEC CICS RETURN TRANSID(tranid) COMMAREA(data)` |
  | XCTL | Transfer to another program | `EXEC CICS XCTL PROGRAM('COADM01C') COMMAREA(data)` |
  | READ | Read VSAM file record | `EXEC CICS READ DATASET('ACCTDAT') INTO(rec) RIDFLD(key)` |
  | REWRITE | Update VSAM record | `EXEC CICS REWRITE DATASET('ACCTDAT') FROM(rec)` |
  </key_cics_commands>

  <commarea_structure>
  From COCOM01Y.cpy - The universal state carrier:
  ```cobol
  01 CARDDEMO-COMMAREA.
     05 CDEMO-GENERAL-INFO.
        10 CDEMO-FROM-TRANID       PIC X(04).  * Calling transaction
        10 CDEMO-FROM-PROGRAM      PIC X(08).  * Calling program
        10 CDEMO-TO-TRANID         PIC X(04).  * Target transaction
        10 CDEMO-TO-PROGRAM        PIC X(08).  * Target program
        10 CDEMO-USER-ID           PIC X(08).  * Authenticated user
        10 CDEMO-USER-TYPE         PIC X(01).  * 'A'=Admin, 'U'=User
           88 CDEMO-USRTYP-ADMIN   VALUE 'A'.
           88 CDEMO-USRTYP-USER    VALUE 'U'.
        10 CDEMO-PGM-CONTEXT       PIC 9(01).  * 0=Enter, 1=Reenter
           88 CDEMO-PGM-ENTER      VALUE 0.
           88 CDEMO-PGM-REENTER    VALUE 1.
     05 CDEMO-CUSTOMER-INFO.
        10 CDEMO-CUST-ID           PIC 9(09).
        10 CDEMO-CUST-FNAME        PIC X(25).
        10 CDEMO-CUST-MNAME        PIC X(25).
        10 CDEMO-CUST-LNAME        PIC X(25).
     05 CDEMO-ACCOUNT-INFO.
        10 CDEMO-ACCT-ID           PIC 9(11).
        10 CDEMO-ACCT-STATUS       PIC X(01).
     05 CDEMO-CARD-INFO.
        10 CDEMO-CARD-NUM          PIC 9(16).
     05 CDEMO-MORE-INFO.
        10 CDEMO-LAST-MAP          PIC X(7).
        10 CDEMO-LAST-MAPSET       PIC X(7).
  ```
  </commarea_structure>

  <entry_point_analysis>
  **Standard CICS Program Entry Pattern:**
  ```cobol
  PROCEDURE DIVISION.
  MAIN-PARA.
      IF EIBCALEN = 0
          *> First entry - no COMMAREA yet
          MOVE LOW-VALUES TO screen-output-area
          PERFORM SEND-INITIAL-SCREEN
      ELSE
          *> Returning from user input
          EVALUATE EIBAID
              WHEN DFHENTER
                  PERFORM PROCESS-ENTER-KEY
              WHEN DFHPF3
                  PERFORM EXIT-PROGRAM
              WHEN OTHER
                  PERFORM INVALID-KEY-ERROR
          END-EVALUATE
      END-IF.

      EXEC CICS RETURN
                TRANSID(WS-TRANID)
                COMMAREA(CARDDEMO-COMMAREA)
                LENGTH(LENGTH OF CARDDEMO-COMMAREA)
      END-EXEC.
  ```
  </entry_point_analysis>

  <response_code_handling>
  **CICS Response Codes:**
  | RESP | Meaning | Action |
  |------|---------|--------|
  | 0 | Success | Continue processing |
  | 13 | NOTFND | Record not found - handle gracefully |
  | 22 | DUPREC | Duplicate key on write |
  | 27 | DISABLED | File disabled - display error |

  **Pattern:**
  ```cobol
  EXEC CICS READ DATASET(file)
            INTO(record)
            RIDFLD(key)
            RESP(WS-RESP-CD)
            RESP2(WS-REAS-CD)
  END-EXEC.

  EVALUATE WS-RESP-CD
      WHEN 0
          PERFORM PROCESS-RECORD
      WHEN 13
          MOVE 'Record not found' TO WS-MESSAGE
          PERFORM SEND-ERROR-SCREEN
      WHEN OTHER
          PERFORM HANDLE-CICS-ERROR
  END-EVALUATE.
  ```
  </response_code_handling>
</cics_transaction_flow>
```

---

### BMS Screen Navigation Pattern

#### Problem
BMS (Basic Mapping Support) defines CICS screens. Understanding the relationship between BMS mapsets, copybooks, and COBOL programs is essential for UI analysis.

#### Template

```xml
<bms_screen_navigation>
  <bms_structure>
  **BMS Hierarchy:**
  - DFHMSD - Mapset definition (container for maps)
  - DFHMDI - Map definition (individual screen, 24x80)
  - DFHMDF - Field definition (input/output field)

  **Field Naming Convention:**
  BMS field `USERID` generates copybook fields:
  - `USERIDI` - Input value (from user)
  - `USERIDO` - Output value (to display)
  - `USERIDL` - Length (set to -1 to position cursor)
  - `USERIDA` - Attribute byte
  - `USERIDC` - Color (if EXTATT=YES)

  **Common Attributes:**
  | Attribute | Meaning |
  |-----------|---------|
  | ASKIP | Auto-skip (read-only) |
  | UNPROT | Unprotected (user can edit) |
  | PROT | Protected (display only) |
  | BRT | Bright intensity |
  | NORM | Normal intensity |
  | DRK | Dark (hidden, for passwords) |
  | IC | Initial cursor position |
  | FSET | Field set (modified data tag) |
  </bms_structure>

  <screen_program_mapping>
  **CardDemo Screen-Program Mapping:**
  | Transaction | Program | BMS Mapset | Function |
  |-------------|---------|------------|----------|
  | CC00 | COSGN00C | COSGN00 | Login screen |
  | CM00 | COMEN01C | COMEN01 | User main menu |
  | CA00 | COADM01C | COADM01 | Admin main menu |
  | CA01 | COACTVWC | COACTVW | Account view |
  | CA02 | COACTUPC | COACTUP | Account update |
  | CC01 | COCRDLIC | COCRDLI | Card list |
  | CC02 | COCRDSLC | COCRDSL | Card select/view |
  | CC03 | COCRDUPC | COCRDUP | Card update |
  | CT00 | COTRN00C | COTRN00 | Transaction list |
  | CT01 | COTRN01C | COTRN01 | Transaction view |
  | CT02 | COTRN02C | COTRN02 | Transaction add |
  | CB00 | COBIL00C | COBIL00 | Bill payment |
  </screen_program_mapping>

  <screen_analysis_procedure>
  When analyzing a screen:

  1. **Find the BMS mapset:**
     Look for SEND MAP statement in program:
     `EXEC CICS SEND MAP('COSGN0A') MAPSET('COSGN00')`

  2. **Read the BMS source:**
     `app/bms/COSGN00.bms`

  3. **Map fields to copybook:**
     BMS field USERID → USERIDI/USERIDO in COSGN00.cpy

  4. **Trace field population:**
     Find where program moves data to output fields:
     `MOVE value TO USERIDO OF COSGN0AO`

  5. **Trace input validation:**
     Find where program checks input fields:
     `IF USERIDI OF COSGN0AI = SPACES`
  </screen_analysis_procedure>

  <standard_screen_layout>
  **Typical CardDemo Screen (24 rows x 80 columns):**
  ```
  Row 1-3:  Header (Date, Time, Program Name, Title)
  Row 4-5:  Secondary header / Context info
  Row 6-20: Content area (data entry/display)
  Row 21-22: Instructions / Navigation hints
  Row 23:   Error message field (ERRMSG)
  Row 24:   Function key legend
  ```
  </standard_screen_layout>
</bms_screen_navigation>
```

---

### Batch Workflow Pattern

#### Problem
Batch jobs run outside CICS and require different file handling. Understanding JCL job chains and CICS-batch coordination is essential.

#### Template

```xml
<batch_workflow>
  <jcl_structure>
  **Standard JCL Job Structure:**
  ```jcl
  //JOBNAME  JOB 'Description',CLASS=A,MSGCLASS=0,NOTIFY=&SYSUID
  //*****************************************************************
  //* Job description comments
  //*****************************************************************
  //STEP01   EXEC PGM=PROGRAMNAME
  //STEPLIB  DD DISP=SHR,DSN=load.library
  //ddname   DD DISP=...,DSN=dataset.name
  //SYSOUT   DD SYSOUT=*
  ```

  **DD Statement Types:**
  | DISP | Meaning |
  |------|---------|
  | SHR | Shared access (read) |
  | OLD | Exclusive access (read/write) |
  | (NEW,CATLG) | Create new dataset |
  | (MOD,CATLG) | Append to existing |
  </jcl_structure>

  <cics_batch_coordination>
  **Critical Pattern: File Locking**

  CICS and batch cannot access the same VSAM files simultaneously.
  CardDemo uses CLOSEFIL/OPENFIL jobs:

  ```
  1. CLOSEFIL.jcl
     - CEMT SET FIL(ACCTDAT) CLO
     - CEMT SET FIL(CARDDAT) CLO
     - CEMT SET FIL(TRANSACT) CLO

  2. [Batch processing jobs run here]

  3. OPENFIL.jcl
     - CEMT SET FIL(ACCTDAT) OPE
     - CEMT SET FIL(CARDDAT) OPE
     - CEMT SET FIL(TRANSACT) OPE
  ```
  </cics_batch_coordination>

  <carddemo_batch_workflow>
  **Full Batch Cycle (run_full_batch.sh):**

  ```
  Phase 1: CICS Preparation
  ├── CLOSEFIL.jcl          Close CICS files

  Phase 2: Data Refresh
  ├── ACCTFILE.jcl          Refresh account data
  ├── CARDFILE.jcl          Refresh card data
  ├── XREFFILE.jcl          Refresh cross-reference
  ├── CUSTFILE.jcl          Refresh customer data
  ├── TCATBALF.jcl          Refresh category balances

  Phase 3: Core Processing
  ├── POSTTRAN.jcl          Post transactions (CBTRN02C)
  │   Input: DALYTRAN (daily transactions)
  │   Updates: TRANFILE, ACCTFILE, TCATBALF
  │   Output: DALYREJS (rejects)
  │
  ├── INTCALC.jcl           Calculate interest (CBTRN03C)

  Phase 4: Post-Processing
  ├── TRANBKP.jcl           Backup transactions
  ├── COMBTRAN.jcl          Combine backup + new
  ├── TRANIDX.jcl           Build alternate indexes

  Phase 5: CICS Restoration
  └── OPENFIL.jcl           Reopen CICS files
  ```
  </carddemo_batch_workflow>

  <batch_program_pattern>
  **Batch COBOL Pattern (differs from CICS):**

  ```cobol
  *> FILE-CONTROL (batch uses SELECT, not EXEC CICS)
  SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE
         ORGANIZATION IS INDEXED
         ACCESS MODE IS RANDOM
         RECORD KEY IS FD-ACCT-ID
         FILE STATUS IS ACCTFILE-STATUS.

  *> PROCEDURE DIVISION
  MAIN-PARA.
      PERFORM 0000-OPEN-FILES
      PERFORM 1000-PROCESS-RECORDS
          UNTIL END-OF-INPUT
      PERFORM 9000-CLOSE-FILES
      STOP RUN.

  *> File operations use READ/WRITE, not EXEC CICS
  READ ACCOUNT-FILE INTO WS-ACCT-RECORD
       KEY IS WS-ACCT-KEY
       INVALID KEY
           PERFORM HANDLE-NOT-FOUND
       NOT INVALID KEY
           PERFORM PROCESS-RECORD
  END-READ.
  ```
  </batch_program_pattern>
</batch_workflow>
```

---

### VSAM File Access Pattern

#### Problem
VSAM (Virtual Storage Access Method) is the primary file system for mainframe data. Understanding KSDS, alternate indexes, and file status codes is essential.

#### Template

```xml
<vsam_file_access>
  <vsam_concepts>
  **VSAM File Types:**
  - KSDS (Key-Sequenced Data Set) - Primary indexed files
  - AIX (Alternate Index) - Secondary key access
  - PATH - Route to access file via alternate index

  **File Status Codes:**
  | Code | Meaning | Action |
  |------|---------|--------|
  | 00 | Successful | Continue |
  | 02 | Duplicate alternate key | May be OK |
  | 10 | End of file | Normal for sequential read |
  | 22 | Duplicate primary key | Error on write |
  | 23 | Record not found | Handle gracefully |
  | 35 | File not found | Configuration error |
  | 39 | Attribute mismatch | File definition error |
  </vsam_concepts>

  <carddemo_file_inventory>
  **CardDemo VSAM Files:**
  | File | Key Field | Key Length | Record Size | Purpose |
  |------|-----------|------------|-------------|---------|
  | ACCTDAT | ACCT-ID | 11 bytes | 300 bytes | Account master |
  | CARDDAT | CARD-NUM | 16 bytes | 150 bytes | Card master |
  | CUSTDAT | CUST-ID | 9 bytes | 500 bytes | Customer master |
  | TRANSACT | TRAN-ID | 16 bytes | 350 bytes | Transaction log |
  | CCXREF | XREF-CARD-NUM | 16 bytes | 50 bytes | Card→Account xref |
  | TCATBALF | Composite | 17 bytes | 50 bytes | Category balances |
  | USRSEC | USER-ID | 8 bytes | varies | User security |
  </carddemo_file_inventory>

  <online_vs_batch_access>
  **Online (CICS) File Access:**
  ```cobol
  EXEC CICS READ
            DATASET('ACCTDAT')
            INTO(ACCOUNT-RECORD)
            LENGTH(LENGTH OF ACCOUNT-RECORD)
            RIDFLD(WS-ACCT-KEY)
            KEYLENGTH(11)
            RESP(WS-RESP-CD)
            RESP2(WS-REAS-CD)
  END-EXEC.
  ```

  **Batch File Access:**
  ```cobol
  SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE
         ORGANIZATION IS INDEXED
         ACCESS MODE IS RANDOM
         RECORD KEY IS FD-ACCT-ID
         FILE STATUS IS ACCTFILE-STATUS.

  READ ACCOUNT-FILE INTO WS-RECORD
       KEY IS WS-ACCT-KEY
       INVALID KEY
           PERFORM RECORD-NOT-FOUND
       NOT INVALID KEY
           PERFORM PROCESS-RECORD
  END-READ.
  ```

  **Key Differences:**
  | Aspect | Online (CICS) | Batch |
  |--------|---------------|-------|
  | File reference | DATASET('name') | ASSIGN TO ddname |
  | Open/Close | Automatic | Explicit OPEN/CLOSE |
  | Error handling | RESP/RESP2 codes | FILE STATUS |
  | Concurrency | Managed by CICS | Exclusive via JCL |
  </online_vs_batch_access>
</vsam_file_access>
```

---

### Source Discovery for COBOL Pattern

#### Problem
Finding relevant source files in a mainframe codebase requires different search strategies than modern projects.

#### Template

```xml
<source_discovery_cobol>
  <discovery_commands>
  **File Counts:**
  ```bash
  # Total COBOL programs
  find app/cbl -name "*.cbl" | wc -l

  # Total copybooks
  find app/cpy -name "*.cpy" | wc -l

  # BMS mapsets
  find app/bms -name "*.bms" | wc -l

  # JCL jobs
  find app/jcl -name "*.jcl" | wc -l
  ```

  **Glob Patterns:**
  | Pattern | Matches |
  |---------|---------|
  | `app/cbl/CO*.cbl` | Online CICS programs |
  | `app/cbl/CB*.cbl` | Batch programs |
  | `app/cpy/*Y.cpy` | Copybooks |
  | `app/bms/*.bms` | BMS screen definitions |
  | `app/jcl/*.jcl` | Batch job control |
  </discovery_commands>

  <grep_patterns>
  **Common Searches:**
  ```bash
  # Find copybook usage
  grep -rn "COPY.*COCOM01Y" app/cbl/

  # Find VSAM file access
  grep -rn "DATASET.*'ACCTDAT'" app/cbl/

  # Find program transfers
  grep -rn "XCTL.*PROGRAM" app/cbl/

  # Find paragraph definitions
  grep -n "^\s\{7\}[0-9A-Z-]*\.$" app/cbl/COSGN00C.cbl

  # Find EXEC CICS commands
  grep -n "EXEC CICS" app/cbl/COSGN00C.cbl
  ```
  </grep_patterns>

  <common_search_scenarios>
  **Scenario: "Where is account data validated?"**
  1. `grep -rn "ACCT-ID" app/cbl/` → Find programs using account ID
  2. Focus on COACTUPC.cbl (account update)
  3. Look for validation paragraphs: `grep "EDIT.*ACCT" app/cbl/COACTUPC.cbl`

  **Scenario: "What programs update transactions?"**
  1. `grep -rn "TRANSACT" app/cbl/` → Find programs accessing transaction file
  2. Online: COTRN02C.cbl (transaction add)
  3. Batch: CBTRN02C.cbl (transaction posting)

  **Scenario: "How does login authentication work?"**
  1. Start with COSGN00C.cbl (signon program)
  2. Find USRSEC file access: `grep "USRSEC" app/cbl/COSGN00C.cbl`
  3. Trace XCTL to menu programs (COADM01C or COMEN01C)
  </common_search_scenarios>

  <source_discovery_schema>
```yaml
# .work/source-discovery.yaml
discovery:
  last_updated: "2026-01-23T14:30:00Z"

  counts:
    cobol_programs: 39
    copybooks: 41
    bms_mapsets: 21
    jcl_jobs: 38

  by_type:
    online_cics: 16
    batch: 10
    utility: 3

  large_files:
    - path: "app/cbl/COACTUPC.cbl"
      lines: 4236
      requires_chunking: true
    - path: "app/cpy/CSLKPCDY.cpy"
      size_kb: 51
      requires_chunking: true
```
  </source_discovery_schema>
</source_discovery_cobol>
```

---

### Enhanced Progress Tracking for Mainframe

Add a `mainframe_context` section to progress.yaml:

```yaml
# progress.yaml - Mainframe COBOL extensions
progress:
  last_updated: "2026-01-23T14:30:00Z"
  current_phase: "2"
  status: "In Progress"

  # Standard phase tracking
  phases:
    phase_0_discovery:
      status: "Complete"
    # ...

  # MAINFRAME-SPECIFIC TRACKING
  mainframe_context:
    # Current program focus
    current_program: "COACTUPC"
    program_type: "online_cics"
    transaction_id: "CA02"

    # Copybooks loaded
    copybooks_loaded:
      - name: "COCOM01Y"
        purpose: "COMMAREA"
      - name: "CVACT01Y"
        purpose: "Account record"

    # Copybooks identified but not loaded
    copybooks_pending:
      - name: "CSLKPCDY"
        reason: "Large lookup table - defer unless needed"

    # VSAM files accessed
    files_accessed:
      - name: "ACCTDAT"
        operation: "READ/REWRITE"
        record_copybook: "CVACT01Y"

    # Program navigation chain
    navigation:
      from_program: "COADM01C"
      to_program: "COACTUPC"
      via: "XCTL"

    # Screen analysis
    screen_context:
      bms_mapset: "COACTUP"
      fields_analyzed: ["ACCTIDI", "ACTCSNI", "ACTSSNI"]

    # Large file chunking status
    chunking:
      - file: "COACTUPC.cbl"
        total_lines: 4236
        chunks_processed: 4
        current_chunk_lines: "1201-1700"
        sections_summarized:
          - "IDENTIFICATION/ENVIRONMENT (1-200)"
          - "DATA DIVISION (200-650)"
          - "Main logic (650-1200)"

  next_action: "Continue analyzing validation logic in COACTUPC.cbl, chunk 5 (lines 1701-2200) focusing on phone number validation"
```

**Chunking Strategy for Large COBOL Programs:**

| Program Size | Chunks | Strategy |
|--------------|--------|----------|
| < 500 lines | 1 | Read entirely |
| 500-2000 lines | 2-3 | Split at DIVISION boundaries |
| 2000-4000 lines | 8-10 | Split at major section boundaries |
| > 4000 lines | 10+ | Section-by-section analysis |

**COACTUPC.cbl Chunk Breakdown (4,236 lines):**
```
Chunk 1:  Lines 1-200     Headers, IDENTIFICATION, ENVIRONMENT
Chunk 2:  Lines 200-650   DATA DIVISION (working storage)
Chunk 3:  Lines 650-850   LINKAGE SECTION, copybook expansions
Chunk 4:  Lines 850-1200  Main logic, EVALUATE EIBAID
Chunk 5:  Lines 1200-1700 Input processing, initial validations
Chunk 6:  Lines 1700-2200 Field-specific validation edits
Chunk 7:  Lines 2200-2700 More validation (phone, SSN, etc.)
Chunk 8:  Lines 2700-3200 Screen setup, attribute handling
Chunk 9:  Lines 3200-3700 Screen field population
Chunk 10: Lines 3700-4236 Data I/O, utilities, exit logic
```

---

### Mainframe-Specific Critical Reminders

```xml
<critical_reminders_mainframe>
================================================================================
                    CRITICAL REMINDERS - MAINFRAME COBOL
================================================================================

1. **COBOL COLUMN SENSITIVITY**
   - Columns 1-6: Sequence numbers (IGNORE)
   - Column 7: Indicator (`*` = comment, `-` = continuation)
   - Columns 8-11: Area A (division/section/paragraph names)
   - Columns 12-72: Area B (statements)
   - Columns 73-80: Identification (IGNORE)

2. **COPYBOOK EXPANSION**
   - COPY statement includes copybook content inline
   - Variables in copybook available after COPY
   - Watch for COPY...REPLACING for customization
   - Some copybooks are VERY large (CSLKPCDY = 51KB)

3. **CICS PSEUDO-CONVERSATIONAL MODEL**
   - Program ends after EACH user interaction
   - COMMAREA preserves state between invocations
   - EIBCALEN = 0 means first entry (no COMMAREA yet)
   - Always check EIBCALEN before accessing DFHCOMMAREA

4. **PARAGRAPH NAMING CONVENTION**
   - 0xxx: Initialization, file open
   - 1xxx-3xxx: Main processing logic
   - 9xxx: Termination, file close, error handling
   - Named paragraphs: SEND-MAP, RECEIVE-MAP, PROCESS-ENTER-KEY

5. **FILE STATUS ALWAYS CHECK**
   - Every file operation MUST check status
   - '00' is success, anything else needs handling
   - CICS uses RESP/RESP2 codes instead of FILE STATUS
   - Common error: 13 (NOTFND), 22 (DUPREC), 23 (NOT FOUND)

6. **BATCH VS ONLINE FILE ACCESS**
   - Batch: SELECT...ASSIGN, OPEN/CLOSE, READ/WRITE
   - Online: EXEC CICS READ/WRITE/REWRITE
   - Same data files, different access methods
   - Batch requires exclusive access (CICS files must be closed)

7. **LARGE PROGRAMS NEED CHUNKING**
   - COACTUPC.cbl: 4,236 lines (10 chunks)
   - COCRDLIC.cbl: 1,459 lines (3-4 chunks)
   - CSLKPCDY.cpy: 51KB lookup table (reference, don't load)

8. **COMMAREA IS THE STATE MECHANISM**
   - COCOM01Y.cpy defines CARDDEMO-COMMAREA
   - Contains: user ID, program chain, customer/account/card context
   - Always load COCOM01Y first for online program analysis
   - CDEMO-PGM-CONTEXT: 0=first entry, 1=reentry

</critical_reminders_mainframe>
```

---

### Methodology Templates (COBOL)

#### Template 1: Analyzing a CICS Online Program

```xml
<methodology_cics_program>
## Phase 1: Initial Context (5 min)
1. Read program header (lines 1-50)
2. Note: Program ID, purpose, transaction ID
3. Find WORKING-STORAGE SECTION start

## Phase 2: Copybook Survey (3 min)
1. List all COPY statements
2. Load COCOM01Y.cpy (COMMAREA) - ALWAYS
3. Load BMS-matching copybook for screen analysis

## Phase 3: Data Structure Review (5 min)
1. Read WORKING-STORAGE SECTION
2. Identify key variables, flags, work areas
3. Note 88-level condition names (used in EVALUATE)

## Phase 4: Main Flow Analysis (10 min)
1. Find PROCEDURE DIVISION
2. Trace entry point logic (EIBCALEN check)
3. Map EIBAID handling (EVALUATE statement)
4. Identify primary paragraphs called

## Phase 5: Screen Handling (5 min)
1. Find SEND-MAP paragraph
2. Find RECEIVE-MAP paragraph
3. Trace field validations

## Phase 6: Data Access (5 min)
1. Find EXEC CICS READ/WRITE statements
2. Note files accessed, key fields
3. Check error handling (RESP codes)

## Phase 7: Navigation (3 min)
1. Find EXEC CICS XCTL statements
2. Map where this program can go
3. Find EXEC CICS RETURN - how it exits

OUTPUT: Program summary with flow diagram
</methodology_cics_program>
```

#### Template 2: Understanding a Batch Workflow

```xml
<methodology_batch_workflow>
## Phase 1: JCL Overview (5 min)
1. Read job card, step names
2. List programs executed (PGM=)
3. List files accessed (DD statements)

## Phase 2: File Mapping (5 min)
1. Map DD names to datasets
2. Identify input files (DISP=SHR)
3. Identify output files (DISP=(NEW,CATLG))
4. Note VSAM files (KSDS indicators)

## Phase 3: Program Sequence (10 min)
For each EXEC PGM step:
  1. Read program IDENTIFICATION DIVISION
  2. Check FILE-CONTROL for file assignments
  3. Map: DD name → SELECT ASSIGN TO name

## Phase 4: Data Flow Tracing (10 min)
1. Trace: Input file → processing → output file
2. Identify master file updates
3. Note reject/error file outputs

## Phase 5: CICS Coordination (if applicable)
1. Check for CLOSEFIL before batch
2. Check for OPENFIL after batch
3. List files requiring CICS closure

OUTPUT: Workflow diagram showing job dependencies
</methodology_batch_workflow>
```

#### Template 3: Tracing a Data Field

```xml
<methodology_field_trace>
## Phase 1: Find Definition
1. Grep field name in copybooks
2. Note level number, PIC clause, REDEFINES

## Phase 2: Find Usage in Programs
1. Grep field name in COBOL programs
2. Categorize: Read, write, compute, display

## Phase 3: Trace Data Flow
1. Find where field is populated (MOVE TO)
2. Find where field is used (MOVE FROM, IF, COMPUTE)
3. Find screen fields (BMS) that display/capture it

## Phase 4: Validation Rules
1. Find IF/EVALUATE statements testing field
2. Find 88-level conditions on field
3. Note error messages for invalid values

OUTPUT: Field lineage diagram
</methodology_field_trace>
```

---

### DB2/IMS/MQ Extension Patterns

CardDemo includes optional extensions demonstrating enterprise integration:

#### DB2 Integration Pattern

```xml
<db2_integration>
  <location>app/app-transaction-type-db2/</location>

  <key_concepts>
  - EXEC SQL ... END-EXEC blocks for SQL statements
  - DCLGEN copybooks for table definitions
  - SQLCA (SQL Communication Area) for error handling
  - Host variables prefixed with colon (:variable)
  </key_concepts>

  <example_pattern>
  ```cobol
  EXEC SQL
      SELECT TRAN_TYPE_CD, TRAN_TYPE_DESC
      INTO :WS-TRAN-TYPE-CD, :WS-TRAN-TYPE-DESC
      FROM TRAN_TYPE
      WHERE TRAN_TYPE_CD = :WS-INPUT-TYPE
  END-EXEC.

  IF SQLCODE NOT = 0
      PERFORM HANDLE-DB2-ERROR
  END-IF.
  ```
  </example_pattern>

  <sqlcode_reference>
  | SQLCODE | Meaning |
  |---------|---------|
  | 0 | Success |
  | 100 | No data found |
  | -803 | Duplicate key |
  | -904 | Resource unavailable |
  </sqlcode_reference>
</db2_integration>
```

#### IMS DB Pattern

```xml
<ims_db_integration>
  <location>app/app-authorization-ims-db2-mq/</location>

  <key_concepts>
  - DL/I (Data Language/I) calls for hierarchical database
  - PCB (Program Communication Block) masks
  - SSA (Segment Search Arguments) for navigation
  - Status codes in PCB for error handling
  </key_concepts>

  <dli_calls>
  | Call | Purpose |
  |------|---------|
  | GU | Get Unique (direct retrieval) |
  | GN | Get Next (sequential) |
  | GNP | Get Next within Parent |
  | ISRT | Insert segment |
  | DLET | Delete segment |
  | REPL | Replace segment |
  </dli_calls>

  <example_pattern>
  ```cobol
  CALL 'CBLTDLI' USING DLI-GU
                       PCB-MASK
                       IO-AREA
                       SSA-1.

  IF PCB-STATUS-CODE NOT = SPACES
      PERFORM HANDLE-IMS-ERROR
  END-IF.
  ```
  </example_pattern>
</ims_db_integration>
```

#### MQ Integration Pattern

```xml
<mq_integration>
  <location>app/app-vsam-mq/</location>

  <key_concepts>
  - MQOPEN - Connect to queue
  - MQPUT - Send message
  - MQGET - Receive message
  - MQCLOSE - Disconnect from queue
  - Message descriptor (MQMD) for headers
  </key_concepts>

  <example_pattern>
  ```cobol
  CALL 'MQOPEN' USING HCONN
                      MQOD
                      OPEN-OPTIONS
                      HOBJ
                      COMP-CODE
                      REASON.

  IF COMP-CODE NOT = MQCC-OK
      PERFORM HANDLE-MQ-ERROR
  END-IF.

  CALL 'MQPUT' USING HCONN
                     HOBJ
                     MQMD
                     MQPMO
                     BUFFER-LENGTH
                     BUFFER
                     COMP-CODE
                     REASON.
  ```
  </example_pattern>

  <completion_codes>
  | Code | Constant | Meaning |
  |------|----------|---------|
  | 0 | MQCC-OK | Success |
  | 1 | MQCC-WARNING | Partial success |
  | 2 | MQCC-FAILED | Operation failed |
  </completion_codes>
</mq_integration>
```

---

### CardDemo Quick Reference

```yaml
# CardDemo Quick Reference

transaction_ids:
  CC00: "Signon"
  CM00: "User Menu"
  CA00: "Admin Menu"
  CA01: "Account View"
  CA02: "Account Update"
  CC01: "Card List"
  CC02: "Card View"
  CC03: "Card Update"
  CT00: "Transaction List"
  CT01: "Transaction View"
  CT02: "Transaction Add"
  CB00: "Bill Payment"

key_copybooks:
  universal:
    - COCOM01Y: "COMMAREA - always load first"
    - COTTL01Y: "Application titles"
    - CSDAT01Y: "Date utilities"
    - CSMSG01Y: "Message structures"
  data_records:
    - CVACT01Y: "Account record"
    - CVACT02Y: "Card record"
    - CVACT03Y: "Cross-reference record"
    - CVCUS01Y: "Customer record"
    - CVTRA05Y: "Transaction record"
  defer_loading:
    - CSLKPCDY: "51KB lookup tables - load only for validation analysis"

vsam_files:
  ACCTDAT: "Account master (key: ACCT-ID, 11 digits)"
  CARDDAT: "Card master (key: CARD-NUM, 16 chars)"
  CUSTDAT: "Customer master (key: CUST-ID, 9 digits)"
  TRANSACT: "Transaction log (key: TRAN-ID, 16 chars)"
  CCXREF: "Card-to-account cross-reference"
  USRSEC: "User security"

batch_programs:
  CBTRN02C: "Post daily transactions"
  CBTRN03C: "Calculate interest"
  CBACT01C-04C: "Account file maintenance"
  CBCUS01C: "Customer maintenance"
  CBEXPORT: "Data export (EBCDIC conversion)"
  CBIMPORT: "Data import"

batch_workflow_order:
  1: "CLOSEFIL.jcl - Close CICS files"
  2: "Data refresh jobs (ACCTFILE, CARDFILE, etc.)"
  3: "POSTTRAN.jcl - Post transactions"
  4: "INTCALC.jcl - Calculate interest"
  5: "COMBTRAN.jcl - Combine transactions"
  6: "OPENFIL.jcl - Reopen CICS files"
```

---

## Customisation Guide

### Step 1: Determine Task Characteristics

Answer these questions:
1. How many phases/levels/steps? → Determines progress structure
2. What are the major deliverables? → Determines checkpoint triggers
3. What source files are involved? → Determines large file handling
4. What task-specific state needs tracking? → Determines additional .work/ files

### Step 2: Customise Progress Tracking

Based on your task structure:
- **Linear phases**: Use `phase_0`, `phase_1`, etc.
- **Levels**: Use `level_1`, `level_2`, etc.
- **Personas/Actors**: Use named personas
- **Parallel work streams**: Use named work streams

### Step 3: Customise Large File Handling

Based on your source files:
- **QUINT specs**: Extract invariants, state machines, types
- **Architecture docs**: Extract decisions, components, constraints
- **Requirements**: Extract requirements, acceptance criteria
- **Code files**: Extract interfaces, key functions, dependencies

### Step 4: Add Task-Specific Tracking

Common additions:
- `counterexamples.yaml` - For verification tasks
- `adr-index.yaml` - For architecture tasks
- `findings.yaml` - For review/audit tasks
- `[entity]-status.yaml` - For multi-entity tasks

### Step 5: Test Resumption

Before finalising a prompt:
1. Run it until partway through
2. Simulate compaction (start fresh context)
3. Verify it resumes correctly from progress.yaml
4. Verify it uses summaries instead of re-reading files

---

## Quick Reference Card

### First Action (Always)
```bash
cat [OUTPUT_DIR]/.work/progress.yaml 2>/dev/null || echo "NO_PROGRESS_FILE"
```

### Directory Structure
```
.work/
├── progress.yaml           # ALWAYS - update frequently
├── source-discovery.yaml   # ALWAYS - file catalogue
├── source-summaries/       # Per-file summaries
├── large-file-summaries/   # Chunked summaries
└── [task-specific]/        # As needed
```

### Size Thresholds
- Small: < 50KB → Read entirely
- Medium: 50-100KB → Monitor for truncation
- Large: > 100KB → **Chunk required**

### Checkpoint Triggers
- After every major deliverable
- After every phase/level complete
- After every 5-10 minutes of work
- Before any complex operation

### Memory Patterns
1. Summarise then discard
2. Reference not re-read
3. Incremental output building
4. Targeted chunk access

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-01-06 | Initial rubric created from 01c, 01e, 01f patterns |
| 1.1 | 2026-01-23 | Added Legacy C++ Codebase Patterns (Golgotha-specific) section with 7 new patterns, enhanced progress tracking schema, and methodology templates |
| 1.2 | 2026-01-23 | Added Mainframe COBOL Codebase Patterns section with 8 navigation patterns, copybook dependency tracing, CICS flow diagrams, batch workflow templates, DB2/IMS/MQ extension patterns, and CardDemo quick reference |

---

## Related Documents

- `00-development-methodology-overview.md` - Master methodology with examples
- `01c-quint-cross-context-testing-prompt.md` - Example: 5-level testing
- `01e-solution-architecture-prompt.md` - Example: 6-phase architecture
- `01f-solution-architecture-verify.md` - Example: 6-persona verification
