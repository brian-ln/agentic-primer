# Know Tool Graduation Plan

**Epic:** agentic-primer-ivn
**Date:** 2026-02-03
**Current Location:** `/Users/bln/play/agentic-primer/simplify/know` (project-specific)
**Goal:** System-wide accessibility

---

## Executive Summary

The `know` tool is currently a symlink in the `simplify` project directory pointing to `src/session-knowledge/cli.ts`. To make it system-wide, we need to:

1. Install it in a location that's in the system PATH
2. Ensure dependencies are accessible
3. Handle the database location consistently
4. Support multi-project usage

**Recommended Option:** Option B - `~/bin/know` with absolute imports

---

## Current State Analysis

### Existing Infrastructure
- **Tool Location:** `/Users/bln/play/agentic-primer/simplify/know` → `src/session-knowledge/cli.ts`
- **Runtime:** Bun (shebang: `#!/usr/bin/env bun`)
- **Bun Location:** `/Users/bln/.bun/bin/bun` (in PATH)
- **Dependencies:** TypeScript modules with relative imports
- **Database:** `~/.claude/index/sessions-libsql.db` (already centralized)
- **Existing ~/bin:** Yes, with 28 executable tools/scripts
- **~/.claude/ exists:** Yes (Claude Code's data directory)

### Key Constraints
1. Tool uses relative imports (`./classification/`, `./extraction/`, etc.)
2. Requires Bun runtime for TypeScript execution
3. Dependencies: `@libsql/client`, `ai-sdk`, etc. from `node_modules`
4. Database path is already centralized in `~/.claude/`

---

## Option A: ~/.claude/tools/know/ (Least Recommended)

### Structure
```
~/.claude/
├── tools/
│   └── know/
│       ├── bin/know          # Executable symlink/wrapper
│       ├── src/              # Source code
│       └── node_modules/     # Dependencies
└── index/
    └── sessions-libsql.db    # Database (already here)
```

### Installation
```bash
# 1. Create tools directory
mkdir -p ~/.claude/tools/know

# 2. Copy source and dependencies
cp -r /Users/bln/play/agentic-primer/simplify/src/session-knowledge ~/.claude/tools/know/src
cp -r /Users/bln/play/agentic-primer/simplify/node_modules ~/.claude/tools/know/
cp /Users/bln/play/agentic-primer/simplify/package.json ~/.claude/tools/know/

# 3. Create wrapper in ~/bin
cat > ~/bin/know << 'EOF'
#!/usr/bin/env bash
exec bun run ~/.claude/tools/know/src/cli.ts "$@"
EOF
chmod +x ~/bin/know
```

### Pros
- Claude-specific organization
- Keeps knowledge system files together
- Easy to find and maintain alongside database

### Cons
- Creates new directory structure (`tools/` doesn't exist)
- Non-standard location (most CLIs use `/usr/local/bin` or `~/bin`)
- Requires duplication of node_modules (wasteful)
- More complex to update

### Verdict
**Not recommended** - Introduces unnecessary complexity and non-standard structure.

---

## Option B: ~/bin/know (Recommended)

### Structure
```
~/bin/
└── know -> /Users/bln/play/agentic-primer/simplify/src/session-knowledge/cli.ts

~/.claude/
└── index/
    └── sessions-libsql.db

# Source remains in original location
/Users/bln/play/agentic-primer/simplify/src/session-knowledge/
```

### Installation
```bash
# Option B1: Simple symlink (requires running from project dir)
ln -sf /Users/bln/play/agentic-primer/simplify/know ~/bin/know

# Option B2: Wrapper script (preferred - works from anywhere)
cat > ~/bin/know << 'EOF'
#!/usr/bin/env bash
cd /Users/bln/play/agentic-primer/simplify
exec bun run src/session-knowledge/cli.ts "$@"
EOF
chmod +x ~/bin/know
```

### Pros
- Standard location (`~/bin` is conventionally in PATH)
- No duplication - uses existing source and dependencies
- Simple installation (single file)
- Easy to update (just edit wrapper or pull git changes)
- Matches existing tools in `~/bin` (28 other executables)
- Works across all projects

### Cons
- Requires project directory to remain at current location
- Wrapper script adds one indirection
- Changes to source require no special rebuild

### Verdict
**Recommended** - Best balance of simplicity, maintainability, and standard conventions.

---

## Option C: /usr/local/bin/know (System-wide)

### Structure
```
/usr/local/bin/
└── know                      # Standalone executable

~/.claude/
└── index/
    └── sessions-libsql.db
```

### Installation
```bash
# Option C1: Bundle with bun build
cd /Users/bln/play/agentic-primer/simplify
bun build src/session-knowledge/cli.ts --compile --outfile know-standalone
sudo mv know-standalone /usr/local/bin/know

# Option C2: Wrapper script
sudo tee /usr/local/bin/know > /dev/null << 'EOF'
#!/usr/bin/env bash
cd /Users/bln/play/agentic-primer/simplify
exec bun run src/session-knowledge/cli.ts "$@"
EOF
sudo chmod +x /usr/local/bin/know
```

### Pros
- System-wide availability (all users)
- Standard Unix location
- Always in PATH
- Professional deployment

### Cons
- Requires sudo (affects security audit)
- Overkill for single-user system
- Harder to update (need sudo each time)
- Standalone build may have issues with dynamic imports

### Verdict
**Not necessary** - Adds complexity without benefit for single-user development system.

---

## Option D: npm/bun global install

### Structure
```
~/.bun/install/global/node_modules/
└── session-knowledge/
    └── bin/know
```

### Installation
```bash
cd /Users/bln/play/agentic-primer/simplify
# Add to package.json:
{
  "name": "session-knowledge-cli",
  "bin": {
    "know": "./src/session-knowledge/cli.ts"
  }
}

# Install globally
bun link
# Or
bun install -g .
```

### Pros
- Package manager handles PATH
- Version management
- Can publish to npm registry later

### Cons
- Requires package.json modification
- More complex uninstall
- Bun's global install behavior less mature than npm's
- Overkill for development tool

### Verdict
**Future consideration** - Good for distribution, but premature for current use case.

---

## Recommended Solution: Option B2 (Wrapper Script)

### Implementation

#### Step 1: Create the wrapper script
```bash
cat > ~/bin/know << 'EOF'
#!/usr/bin/env bash
# Session Knowledge System - Global wrapper
# Epic: agentic-primer-ivn
#
# This wrapper allows 'know' to run from anywhere while maintaining
# access to project dependencies and source code.

PROJECT_ROOT="/Users/bln/play/agentic-primer/simplify"

# Check if project exists
if [[ ! -d "$PROJECT_ROOT" ]]; then
  echo "Error: Session Knowledge project not found at $PROJECT_ROOT" >&2
  exit 1
fi

# Check if bun is installed
if ! command -v bun &> /dev/null; then
  echo "Error: bun runtime not found. Install from https://bun.sh" >&2
  exit 1
fi

# Run the CLI with all arguments
cd "$PROJECT_ROOT"
exec bun run src/session-knowledge/cli.ts "$@"
EOF

chmod +x ~/bin/know
```

#### Step 2: Verify installation
```bash
# Test from any directory
cd ~
know --help

# Should display the full help menu
```

#### Step 3: Test multi-project usage
```bash
# From different project
cd ~/some-other-project
know stats
know search "vector database"
```

---

## Configuration Strategy

### Database Location
**Current:** `~/.claude/index/sessions-libsql.db` (line 19 in cli.ts)

**Strategy:** Keep as-is. This is correct - centralized, cross-project database.

```typescript
const DB_PATH = join(process.env.HOME!, '.claude/index/sessions-libsql.db');
```

### Environment Variables (Future)
For advanced configuration, consider:

```bash
# ~/.bashrc or ~/.zshrc
export KNOW_DB_PATH="$HOME/.claude/index/sessions-libsql.db"
export KNOW_PROJECT_ROOT="$HOME/play/agentic-primer/simplify"
```

Then modify cli.ts:
```typescript
const DB_PATH = process.env.KNOW_DB_PATH ||
                join(process.env.HOME!, '.claude/index/sessions-libsql.db');
```

### Config File (Optional)
**Not needed now**, but for future:

```yaml
# ~/.knowrc or ~/.claude/know-config.yaml
database:
  path: ~/.claude/index/sessions-libsql.db

embeddings:
  provider: cloudflare
  model: "@cf/baai/bge-base-en-v1.5"

extraction:
  batch_size: 50
  confidence_threshold: 0.65
```

---

## Multi-Project Strategy

### Current Behavior
The tool already supports multi-project usage through centralized database:

1. **Session detection:** Auto-detects current session from most recent `.jsonl` in project directory
2. **Database:** Centralized in `~/.claude/index/sessions-libsql.db`
3. **Project linking:** Sessions are linked to projects via directory path hash

### No Changes Needed
The existing implementation already handles multiple projects:

```typescript
// From cli.ts lines 312-332
const cwd = process.cwd();
const homeDir = process.env.HOME || '';
const projectName = cwd.replace(/\//g, '-');
const projectDir = join(homeDir, '.claude/projects', projectName);
```

This means:
- Running `know add decision "..." from `/project-a/` → Associates with project-a
- Running `know add decision "..." from `/project-b/` → Associates with project-b
- Running `know search "..." from anywhere` → Searches all projects
- Running `know stats` → Shows stats across all projects

### Future Enhancement: Project Filtering
If needed later, add project scope flags:

```bash
know search "vectors" --project agentic-primer
know stats --project-only  # Only current project
know decisions --all-projects  # Explicit cross-project
```

---

## Migration Checklist

- [ ] Create wrapper script in `~/bin/know`
- [ ] Make wrapper executable (`chmod +x`)
- [ ] Test from home directory (`cd ~ && know --help`)
- [ ] Test from different project directory
- [ ] Test all major commands:
  - [ ] `know stats`
  - [ ] `know sessions recent 5`
  - [ ] `know search "test query"`
  - [ ] `know decisions recent 5`
  - [ ] `know add decision "Test graduation" --reasoning "Verification"`
- [ ] Verify database path is correct (`~/.claude/index/sessions-libsql.db`)
- [ ] Test with piped output (`know sessions recent 5 --json | jq`)
- [ ] Document usage in project README
- [ ] Update any project-local scripts that call `./know`
- [ ] Remove old symlink from project root (optional)

---

## Rollback Plan

If something goes wrong:

```bash
# Remove global wrapper
rm ~/bin/know

# Restore project-local usage
cd /Users/bln/play/agentic-primer/simplify
./know --help  # Use with ./
```

No data is affected - database remains untouched.

---

## Future Enhancements

### 1. Standalone Distribution
When ready to share with others:

```bash
# Create standalone executable
bun build src/session-knowledge/cli.ts \
  --compile \
  --minify \
  --target bun-darwin-arm64 \
  --outfile dist/know-macos-arm64

# Or cross-platform
bun build --target bun-linux-x64 --outfile dist/know-linux
bun build --target bun-windows-x64 --outfile dist/know.exe
```

### 2. Package Manager Distribution
```bash
# Publish to npm
npm publish session-knowledge-cli

# Users install with
npm install -g session-knowledge-cli
# or
bun install -g session-knowledge-cli
```

### 3. Homebrew Formula
```ruby
# Formula/know.rb
class Know < Formula
  desc "Session Knowledge System - AI-powered knowledge extraction"
  homepage "https://github.com/user/session-knowledge"
  url "https://github.com/user/session-knowledge/archive/v1.0.0.tar.gz"

  depends_on "bun"

  def install
    bin.install "know"
  end
end
```

### 4. Shell Completions
Add bash/zsh completions:

```bash
# ~/.zsh/completions/_know
#compdef know

_know() {
  local -a commands
  commands=(
    'stats:Show session statistics'
    'search:Semantic search across knowledge'
    'decisions:Query extracted decisions'
    'learnings:Query extracted learnings'
    # ...
  )
  _describe 'command' commands
}
```

---

## Cost Analysis

### Development Time
- **Option A:** 2-3 hours (setup new structure, test)
- **Option B:** 15 minutes (create wrapper, test)
- **Option C:** 1 hour (build, install, test)
- **Option D:** 1-2 hours (package.json changes, testing)

### Maintenance Burden
- **Option A:** High (duplicate dependencies, manual updates)
- **Option B:** Low (git pull updates source automatically)
- **Option C:** Medium (rebuild required for updates)
- **Option D:** Medium (publish workflow needed)

### Storage Impact
- **Option A:** +150MB (duplicate node_modules)
- **Option B:** +0.5KB (wrapper script only)
- **Option C:** +30MB (compiled executable)
- **Option D:** +150MB (global node_modules)

---

## Conclusion

**Install Option B2** - the wrapper script approach:

1. Minimal installation footprint (single 500-byte script)
2. Zero duplication (uses existing source/dependencies)
3. Standard location (`~/bin` matches 28 other user tools)
4. Easy maintenance (git pull updates source)
5. Multi-project support (already built-in)
6. No configuration needed (database already centralized)

**Next Steps:**
1. Execute installation commands from Step 1
2. Run migration checklist
3. Document in project README
4. Use globally: `know stats` from any directory

**Success Criteria:**
- Can run `know` from any directory
- All commands work correctly
- Database remains in `~/.claude/index/`
- No duplication of source or dependencies
