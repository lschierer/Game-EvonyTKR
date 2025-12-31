# Persistence Role Architecture

## Overview

The persistence layer has been refactored into a hybrid architecture with granular roles that can be composed individually or all together via a convenience aggregator role.

## Architecture

### Granular Roles

Each collection type has its own persistence role:

- **Game::EvonyTKR::Role::Persistence::Core** - Base infrastructure
  - `persistence` attribute (Service::Persistence instance)
  - `mark_task_completed()` / `is_task_completed()` - Job tracking

- **Game::EvonyTKR::Role::Persistence::Generals**
  - `general_cache` attribute
  - `add_general()` / `get_general()` / `get_generals()` / `list_generals()`

- **Game::EvonyTKR::Role::Persistence::Books**
  - `builtin_book_cache` / `generic_book_cache` attributes
  - Methods for both builtin and generic books

- **Game::EvonyTKR::Role::Persistence::Covenants**
  - `covenant_cache` attribute
  - `add_covenant()` / `get_covenant()` / `list_covenants()`

- **Game::EvonyTKR::Role::Persistence::Specialties**
  - `specialty_cache` attribute
  - `add_specialty()` / `get_specialty()` / `list_specialties()`

- **Game::EvonyTKR::Role::Persistence::AscendingAttributes**
  - `ascending_attribute_cache` attribute
  - `add_ascending_attribute()` / `get_ascending_attributes()` / `list_ascending_attributes()`

- **Game::EvonyTKR::Role::Persistence::Pairs**
  - `pair_cache` / `conflict_cache` attributes
  - Pair CRUD operations, conflict detection, validation

### Convenience Aggregator

**Game::EvonyTKR::Role::Persistence** - Composes all granular roles

This is the monolithic role that provides everything. Most code should continue using this.

## Usage Patterns

### Most Controllers and Jobs (Recommended)

Use the monolithic role for convenience:

```perl
package Game::EvonyTKR::Controller::Generals;
use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
use Mojo::Base 'Game::EvonyTKR::Role::Persistence', -role;

# Has access to all persistence methods
sub index ($self) {
  my $generals = $self->get_generals();
  ...
}
```

### Specialized Jobs (Optional)

For jobs that only need specific collections, compose granular roles:

```perl
package Game::EvonyTKR::External::Books::Loader;
use Mojo::Base 'Game::EvonyTKR::External::JobBase';
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Books', -role;

# Only has book-related methods, cleaner namespace
sub run ($job) {
  my $books = $self->list_builtin_books();
  ...
}
```

### Tests (Recommended)

Use granular roles for focused mocking:

```perl
package Test::GeneralLoader;
use Mojo::Base -base;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Generals', -role;

# Only need to mock general-related persistence
```

## Benefits

### Monolithic Role
- Simple: one role to compose
- Convenient: everything available
- Good for controllers that need multiple collection types

### Granular Roles
- Explicit dependencies
- Minimal namespace pollution
- Easier testing with focused mocking
- Can evolve collection patterns independently

### Hybrid Approach
- Existing code continues working (uses monolithic)
- New specialized code can use granular roles
- Gradual migration path
- No breaking changes

## Migration Notes

1. All existing code continues to work unchanged
2. ControllerBase and JobBase still use monolithic Persistence role
3. New code can choose granular or monolithic based on needs
4. Consider using granular roles for:
   - Specialized loader jobs
   - Focused unit tests
   - New services that only need specific collections

## File Locations

```
lib/Game/EvonyTKR/Role/
├── Persistence.pm                          # Monolithic aggregator
└── Persistence/
    ├── Core.pm                             # Base infrastructure
    ├── Generals.pm                         # General operations
    ├── Books.pm                            # Book operations
    ├── Covenants.pm                        # Covenant operations
    ├── Specialties.pm                      # Specialty operations
    ├── AscendingAttributes.pm              # Ascending attribute operations
    └── Pairs.pm                            # Pair operations

```
