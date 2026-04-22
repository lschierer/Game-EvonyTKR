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
- Perl 5.42+ with Thunderhorse/PAGI::Server web framework
  - Built on top of my custom extended version in ../PAGI-WebServer 
  - Uses mostly core Thunderhorse (https://metacpan.org/pod/Thunderhorse)
  - Falls back to PAGI::* methods where Thunderhorse does not provide functionality
- Perl Data Language for massive performance gains
- MCE (Many-Core Engine for Perl) for offline processing where parallel work is required.
- very minimal python for XGBoost during the build phase because perl XGBoost support busted on OSX

**Frontend:**
- **Keep TypeScript MINIMAL** - performance degrades rapidly with large TS bundles
- Lit web components for necessary client-side interactions
- Most computation happens server-side in Perl

**Deployment:**
- AWS CDK (TypeScript) for infrastructure-as-code
- EC2 instance with pre-production and production stacks

**Data:**
- YAML files for static game data (generals, books, covenants, etc.)
- Data cached in memory 
 
## Common Commands

### Setup and Dependencies
```bash
mise run prepare          # Install mise tools and run perl Build.PL
mise run npmdeps          # Install node dependencies via pnpm
mise run deps             # Complete dependency setup (Perl + npm)
```

### Development
```bash
mise run dev              # deprecated full rebuild command that previously also started a dev server
mise run quickdev         # Fast dev server without full rebuild
```

### Building
```bash
mise run build            # Full production build (Perl + CSS + TypeScript)
mise run css              # Build CSS only (PostCSS + Spectrum CSS)
mise run ts               # Build TypeScript only (esbuild compilation)
mise run images           # Sync images to public directory
mise run mlModel          # build the mlModel using XGBoost
```

### Testing
```bash
./Build test          # Run all Perl tests (Test2::V0 framework) -- not currently working, run each test script manually. 
```

### Deployment
```bash
mise run deploy-dev       # Deploy to AWS dev stack
mise run deploy-prod      # Deploy to AWS production stack
```

### Code Quality
```bash
mise run tidy             # Format all Perl code with perltidy
```

## Architecture Overview

### Hybrid Perl/TypeScript Design

This project uses **modular auto-discovery** wherever possible:

1. **Controllers** auto-discovered and loaded as Mojolicious plugins
3. Extends Thunderhorse base packages to wire directly into the Thunderhorse lifecycle where possible. 
4. Separate namespaces when load order matters

### Data Flow Pipeline

```
YAML Static Files (share/collections/data/)
    ↓
::Loader::* packages
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
- **Role/** - Shared Moose roles and constants
  - **Common.pm** - normalize(), logging utilities
  - **Constants/** - BuffConstants, GeneralConstants, etc.
- **Service/** - PDL computation service, other subsystems that do not fit in the Thunderhorse model.  
- **Converter/** - Tools to convert external data to YAML

**Frontend (lib/):**
- **Generals/Single/** - Single general view (picker, table, state)
- **Generals/Pair/** - Pair matching interface (picker, table, filters)
- **partials/** - Shared web components

## Critical Design Patterns

- Extend existing base classes PAGI::WebServer where possible. 
- Extend packages from Thunderhorse where PAGI::WebServer does not offer the necessary functionality.
- Build fully custom packages only where neither of the above two patterns fit. 
- update legacy code to this pattern any time it requires substantial changes or stops working with the overall project.
- CSS should be written in small, route/component specific files in ../share/styles/ then compiled into place with the ```mise run css``` command. 

### Minimal Frontend Rule

**IMPORTANT:** Keep TypeScript minimal. Testing shows performance degrades rapidly with large TS bundles due to computed data types. Only use TypeScript for:
- Interactive UI components (pickers, tables)
- Client-side filtering/sorting when server-side is impractical
- URL state management

Most computation should happen in Perl on the server.

## Testing Guidelines

- Use **Test2::V0** framework for all Perl tests
- Tests live in `t/` directory
- Run single test: `prove -v t/test_name.t`
- Test actual YAML data files (not mocks)

## Known Issues and TODOs

See README.md for current TODO list. Key areas:
- Buff summarizer condition handling is limited
- Conflict group-to-book mappings incomplete
- Monster books not partly implemented
- Passive buff support needs enhancement
- Logging configuration inconsistencies

## Code Style

- Perl: Use `.perltidyrc` and `.perlcriticrc` configs
- TypeScript: Prettier with ESLint
- CSS: Stylelint with PostCSS

## Current Development Branch

**PAGI** 
- Working adding a Monster Hunting Simulator
- Trying to get logging to be more consistent
- Removing unused cruft where possible.
