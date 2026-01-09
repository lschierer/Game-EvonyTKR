# Thunderhorse MVP - Specialties Only

## What We've Built

This is a minimal viable product (MVP) of the Game-EvonyTKR application migrated to the Thunderhorse framework. It includes only Specialties functionality as proof of concept.

### Files Created/Modified

#### Core Application
- **lib/Game/EvonyTKR.pm** - Main Thunderhorse app class
  - Loads modules and controllers
  - Entry point for application build

#### Modules
- **lib/Game/EvonyTKR/Module/DataLoaders.pm** - Data loading module
  - Loads specialty data at startup
  - Registers `specialty_loader()` helper

#### Controllers
- **lib/Game/EvonyTKR/Controller/ControllerBase.pm** - Base controller (converted)
  - Extends `Thunderhorse::Controller`
  - Provides common routes (/health, /robots.txt, /sitemap.xml)
  - Shared functionality for all controllers

- **lib/Game/EvonyTKR/Controller/Specialties.pm** - Specialties controller (converted)
  - Routes: `/Reference/Specialties` (index), `/Reference/Specialties/:name` (details)
  - Async handlers
  - Navigation generation

- **lib/Game/EvonyTKR/Controller/Root.pm** - Root controller (converted)
  - Routes: `/` (homepage), `/Reference`, `/policy/privacy`
  - Markdown page rendering

#### Infrastructure
- **bin/server2.pl** - Server entry point
  - Command line argument parsing (--mode, --port, --host)
  - App initialization and run

- **share/conf/development.yml** - Configuration
  - Server settings
  - Template paths
  - Directory configuration

### Existing Infrastructure Used

- **lib/Game/EvonyTKR/Loader/Specialties.pm** - Already existed
  - Loads YAML files from `share/collections/data/specialties/`
  - Provides `get_specialty()`, `list_specialties()` methods

- **lib/Game/EvonyTKR/Model/Specialty.pm** - Already existed
  - Domain model for specialties

- **share/collections/data/specialties/*.yaml** - Existing data files
  - Specialty definitions

## How to Run

```bash
cd /Volumes/Development/luke/src/PAGI-WebServer/Game-EvonyTKR

# Development mode (default)
perl bin/server2.pl

# Or specify options
perl bin/server2.pl --mode development --port 3000 --host 127.0.0.1
```

Then visit:
- http://localhost:3000/ - Homepage
- http://localhost:3000/Reference/Specialties - Specialties index
- http://localhost:3000/Reference/Specialties/ambush - Individual specialty
- http://localhost:3000/health - Health check endpoint

## What's Still Needed

### Templates

The templates need to be converted from Mojolicious `.html.ep` format to Template Toolkit `.tt` format:

1. **templates/layouts/default.tt** - Main layout
2. **templates/specialties/index.tt** - Specialties list
3. **templates/specialties/details.tt** - Specialty details
4. **templates/root/index.tt** - Homepage template
5. **templates/markdown.tt** - Generic markdown page

### Methods to Implement

Some methods called by controllers but not yet implemented:

- **ControllerBase:**
  - `render_error($code, $message)` - Error page rendering
  - `site_logo()` - Site logo helper

- **Root:**
  - `render_markdown_page()` - From Game::EvonyTKR::Role::MarkdownRenderer
  - `render_navigation()` - From navigation system

### Roles to Check

Make sure these roles work with Thunderhorse:
- `Game::EvonyTKR::Role::StaticPages`
- `Game::EvonyTKR::Role::MarkdownRenderer`
- `Game::EvonyTKR::Role::Logging`
- `Game::EvonyTKR::Role::Common`
- `Game::EvonyTKR::Role::JSON`
- `Game::EvonyTKR::Role::Persistence`

## Testing the MVP

```bash
# 1. Start the server
perl bin/server2.pl

# 2. Test health endpoint
curl http://localhost:3000/health

# 3. Test root
curl http://localhost:3000/

# 4. Test specialties index
curl http://localhost:3000/Reference/Specialties

# 5. Test specific specialty
curl http://localhost:3000/Reference/Specialties/ambush
```

## Next Steps (Incremental Migration)

Once the MVP works, add functionality incrementally:

1. **Books controller** - Similar pattern to Specialties
2. **Generals controller** - More complex with pairs
3. **Covenants controller**
4. **Ascending Attributes**
5. **Other controllers**

Each addition follows the same pattern:
- Convert controller from Mojolicious to Thunderhorse
- Update DataLoaders module to load that data type
- Convert templates from .ep to .tt
- Test routes

## Architecture Notes

### Thunderhorse Pattern

```
WebFramework::App (PAGI-WebServer)
  ↓
Game::EvonyTKR (extends WebFramework::App)
  ↓ loads modules
Game::EvonyTKR::Module::DataLoaders (extends Thunderhorse::Module)
  → Instantiates Loader::Specialties
  → Registers specialty_loader() helper
  ↓ loads controllers
Game::EvonyTKR::Controller::Specialties (extends ControllerBase)
  → Extends Thunderhorse::Controller (via ControllerBase)
  → Routes: /Reference/Specialties, /Reference/Specialties/:name
  → Uses specialty_loader() to get data
```

### Key Differences from Mojolicious

| Mojolicious | Thunderhorse |
|-------------|--------------|
| `sub register($c, $app, $config)` | `sub build($self)` |
| `$app->routes->get('/path')` | `$self->router->add('/path', {...})` |
| `$c->param('id')` | `$args[0]` (from route captures) |
| `$c->render(template => 'foo')` | `return $self->render('foo.tt', {...})` |
| Minion background jobs | Sync loading at startup (for now) |
| `$app->helper(name => sub {...})` | `$self->app->helper(name => sub {...})` |

### Data Flow

```
Startup:
  Game::EvonyTKR->new()
  → build()
  → load_module('DataLoaders')
    → DataLoaders->build()
      → Loader::Specialties->load_all()
      → Register specialty_loader() helper
  → load_controller('Specialties')
    → Specialties->build()
      → Register routes
      → Build navigation (calls specialty_loader())

Request:
  HTTP GET /Reference/Specialties/ambush
  → Router matches route
  → Calls async handler
  → Handler calls specialty_loader()->get_specialty('ambush')
  → Renders template with data
  → Returns HTML
```

## Troubleshooting

### "specialty_loader not available"
- Make sure DataLoaders module loads before Specialties controller
- Check that `load_module('^Game::EvonyTKR::Module::DataLoaders')` runs first

### "Template not found"
- Templates need to be converted from .ep to .tt format
- Check template paths in config: `share/conf/development.yml`

### "Route not found"
- Check that controller `build()` method is being called
- Verify route patterns match request path
- Check load order in Game::EvonyTKR.pm

### "Future=HASH" displayed
- Missing `await` in async handler
- Handler not marked as `async sub`

## Performance Notes

This MVP loads all data synchronously at startup. Future iterations may:
- Add async data loading with Future::AsyncAwait
- Add caching layer
- Add database persistence
- But for ~100 specialties, in-memory is fine

## Success Criteria

The MVP is working when:
- [ ] Server starts without errors
- [ ] /health endpoint returns JSON
- [ ] / returns homepage (even if fallback content)
- [ ] /Reference/Specialties returns specialty list
- [ ] /Reference/Specialties/ambush returns specialty details
- [ ] Navigation appears in sidebar
- [ ] Templates render correctly

Once these work, we can incrementally add more controllers following the same pattern!
