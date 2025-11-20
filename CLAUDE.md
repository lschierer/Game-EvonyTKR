# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an information resource for players of **Evony: The King's Return (EvonyTKR)**. The application helps players analyze generals, buffs/debuffs, pairs, covenants, skill books, and other game mechanics.

**Critical Constraints:**
- Performance and budget are the primary drivers (5th generation attempting to balance these)
- Must support ~500 users (5-10 concurrent) with minimal cost
- Each user makes 1-5 route requests at a time
- 90% of users are US-based

## Tech Stack

**Backend:**
- Perl 5.42+ with Mojolicious web framework
- Minion job queue (SQLite backend) for async processing
- Memcached for IPC and caching (no long-term SQLite usage)

**Frontend:**
- **Keep TypeScript MINIMAL** - performance degrades rapidly with large TS bundles
- Lit web components for necessary client-side interactions
- Most computation happens server-side in Perl

**Deployment:**
- AWS CDK (TypeScript) for infrastructure-as-code
- EC2 instance with pre-production and production stacks

**Data:**
- YAML files for static game data (generals, books, covenants, etc.)
- No long-term database storage (SQLite only for Minion internals)

## Common Commands

### Setup and Dependencies
```bash
just prepare          # Install mise tools and run perl Build.PL
just npmdeps          # Install node dependencies via pnpm
just deps             # Complete dependency setup (Perl + npm)
```

### Development
```bash
just dev              # Full rebuild + watch mode with morbo
just quickdev         # Fast dev server without full rebuild
```

### Building
```bash
just build            # Full production build (Perl + CSS + TypeScript)
just css              # Build CSS only (PostCSS + Spectrum CSS)
just ts               # Build TypeScript only (esbuild compilation)
just images           # Sync images to public directory
```

### Testing
```bash
./Build test          # Run all Perl tests (Test2::V0 framework)
```

### Deployment
```bash
just deploy-dev       # Deploy to AWS dev stack
just deploy-prod      # Deploy to AWS production stack
```

### Code Quality
```bash
just tidy             # Format all Perl code with perltidy
```

## Architecture Overview

### Hybrid Perl/TypeScript Design

This project uses **modular auto-discovery** wherever possible:

1. **Controllers** auto-discovered and loaded as Mojolicious plugins
2. **Minion External tasks** auto-discovered and registered
3. Modules use `register()` pattern for lifecycle hooks
4. Separate namespaces when load order matters

### Data Flow Pipeline

```
YAML Static Files (share/collections/data/)
    ↓
External/Prebuild.pm (orchestrator job)
    ↓
Parallel Minion Jobs:
├── Load Generals, Books, Covenants, Specialties, Ascending Attributes
├── Build Pairs (all valid general combinations)
└── Reduce/Summarize (batch process with conflict detection)
    ↓
Memcached Cache (precomputed data)
    ↓
Controllers → Models → Templates/TypeScript → HTTP Response
```

### Key Namespaces

**lib/Game/EvonyTKR/**
- **Model/** - Domain objects (General, Buff, Covenant, Pair, Book, etc.)
  - **Buff/Summarizer.pm** - Aggregates buffs for display
  - **General/Pair.pm** - Paired general combinations
  - **General/Conflict.pm** - Conflict detection logic
- **Controller/** - Route handlers (Generals, Pairs, Covenants, Books, etc.)
  - Use ControllerBase.pm for shared functionality
  - Roles in Controller/Role/ for mixins
- **External/** - Minion async jobs (all inherit from External/JobBase.pm)
  - **Prebuild.pm** - Main orchestrator
  - **General/Pair/** - Pair building and reduction jobs
- **Role/** - Shared Moose roles and constants
  - **Common.pm** - normalize(), logging utilities
  - **Constants/** - BuffConstants, GeneralConstants, etc.
- **Service/Cache.pm** - Memcached wrapper
- **Converter/** - Tools to convert external data to YAML

**Frontend (lib/):**
- **Generals/Single/** - Single general view (picker, table, state)
- **Generals/Pair/** - Pair matching interface (picker, table, filters)
- **partials/** - Shared web components

## Critical Design Patterns

### Adding a New Buff Type

When adding a buff type to the summarizer, update ALL of these files:

1. **Game/EvonyTKR/Model/Buff/Summarizer.pm**
   - New summarizer function
   - New output field
   - Update `updateBuffs()` with new field

2. **Game/EvonyTKR/Model/General/Pair.pm**
   - New computed field
   - Update `updateBuffs()` to include new field

3. **Game/EvonyTKR/Controller/Generals.pm**
   - Add to buff-summaries stash setting

4. **Game/EvonyTKR/Controller/Generals/Pairs.pm**
   - Add `$*_param` for sorting
   - Add `$*_dir_param` for direction
   - Add comparison in sort function loop

5. **templates/generals/details.html.ep**
   - Add table row for new buff type

6. **templates/generals/pairs/typeIndex.html.ep**
   - Add `my $*_index` declaration
   - Add `my $*_dir` declaration
   - Add `my $*_sort_order` declaration
   - Add column to table header (update all variable references)
   - Add column to table body (update all variable references)

### Plugin Architecture

All controllers follow this pattern:
```perl
sub register ($c, $app, $conf) {
    my $routes = $app->routes;
    # Define routes here
}
```

Minion jobs extend `External/JobBase.pm`:
```perl
sub register ($taskClassf, $minion, $app) {
    $minion->add_task(task_name => sub { ... });
}
```

### Minimal Frontend Rule

**IMPORTANT:** Keep TypeScript minimal. Testing shows performance degrades rapidly with large TS bundles due to computed data types. Only use TypeScript for:
- Interactive UI components (pickers, tables)
- Client-side filtering/sorting when server-side is impractical
- URL state management

Most computation should happen in Perl on the server.

### Caching Strategy

1. **Memcached** - Primary cache for IPC and hot data
2. **Model-level memoization** - Function-level caching
3. **HTTP caching headers** - For static resources

## Testing Guidelines

- Use **Test2::V0** framework for all Perl tests
- Tests live in `t/` directory
- Run single test: `prove -v t/test_name.t`
- Test actual YAML data files (not mocks)

## Known Issues and TODOs

See README.md for current TODO list. Key areas:
- Buff summarizer condition handling is limited
- Conflict group-to-book mappings incomplete
- Monster books not yet implemented
- Passive buff support needs enhancement
- Logging configuration inconsistencies

## Code Style

- Perl: Use `.perltidyrc` and `.perlcriticrc` configs
- TypeScript: Prettier with ESLint
- CSS: Stylelint with PostCSS

## Current Development Branch

**streaming** - Working on pairs, conflicts, and covenants integration
