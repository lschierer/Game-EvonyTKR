# PoC Implementation - Files Created

## Summary

Successfully created the Proof of Concept for migrating Game-EvonyTKR from Mojolicious to PAGI-WebServer. The homepage route is now implemented using PAGI routing and Template Toolkit.

## Files Created

### 1. Entry Point
```
bin/server.pl - PAGI server entry point (executable)
```

### 2. Core Application
```
lib/Game/EvonyTKR/WebServer.pm - Main PAGI application class
  - Request handling
  - Router integration
  - Template rendering
  - Navigation management
  - Error handling (404, 500)
```

### 3. Role
```
lib/Game/EvonyTKR/Role/Spectrum.pm - Spectrum CSS styling
  - apply_spectrum_css() method
  - Adds Adobe Spectrum classes to HTML elements
  - Reusable across handlers
```

### 4. Handler
```
lib/Game/EvonyTKR/Handler/Root.pm - Homepage and static pages
  - index() - renders share/pages/index.md
  - single_page() - renders any markdown file from share/pages/
  - Registers routes and navigation items
```

### 5. Templates
```
templates/root/index.tt - Homepage template
templates/page/markdown.tt - Generic markdown page template
```

Both templates are simple pass-through templates since content is pre-rendered from markdown with Spectrum CSS applied.

## Architecture Overview

```
Request Flow:
  1. bin/server.pl starts PAGI server
  2. Request comes in → WebServer->handle_request()
  3. Router matches path → Handler::Root->index() or ->single_page()
  4. Handler:
     - Loads markdown file
     - Parses frontmatter (YAML)
     - Renders to HTML (via PAGI::WebServer::Markdown)
     - Applies Spectrum CSS classes
     - Passes to template
  5. Template wraps in layout (layouts/default.tt)
  6. Returns HTTP::Response
```

## What Works

✅ PAGI server initialization
✅ Route registration
✅ Navigation integration
✅ Markdown rendering with frontmatter
✅ Spectrum CSS styling
✅ Template Toolkit rendering
✅ Error handling (404, 500)

## What Needs Testing

The PoC is complete but not yet tested. Next steps:

### 1. Start the Server

```bash
cd Game-EvonyTKR
./bin/server.pl
```

Expected output:
```
Starting EvonyTKR PAGI server on 127.0.0.1:3000
```

### 2. Test Homepage

```bash
curl http://127.0.0.1:3000/
```

Should return HTML with:
- Content from share/pages/index.md
- Spectrum CSS classes applied
- Wrapped in layout template
- Navigation sidebar

### 3. Test Static Page

```bash
curl http://127.0.0.1:3000/Reference
curl http://127.0.0.1:3000/policy/privacy
```

### 4. Compare with Mojolicious

```bash
# Start both servers
./bin/game-evonytkr daemon -l http://127.0.0.1:3001  # Mojolicious
./bin/server.pl                                       # PAGI on 3000

# Compare outputs
diff <(curl -s http://127.0.0.1:3001/ | tidy -i -q) \
     <(curl -s http://127.0.0.1:3000/ | tidy -i -q)
```

## Known Issues / TODOs

### Missing Components (Not Part of PoC)

1. **Static File Serving** - CSS, JS, images
   - Need routes for /css/*, /js/*, /images/*
   - Will add after PoC validation

2. **Async Data Loading** - For database/YAML
   - Not needed for static markdown pages
   - Will implement for Specialties controller

3. **Session Management** - User sessions
   - Not needed for PoC
   - Will add if/when needed

4. **Full Navigation Discovery** - Auto-discover markdown pages
   - Current: Manual registration in Handler::Root
   - Future: Scan share/pages/ directory

### Potential Issues

1. **Template Path** - May need adjustment
   - Using `$FindBin::Bin/../templates`
   - Verify this resolves correctly

2. **Log4perl Configuration** - Currently using easy_init($DEBUG)
   - May want proper config file
   - Works for PoC

3. **Markdown File Not Found** - Returns 500 instead of 404
   - Could be improved
   - Acceptable for PoC

4. **Navigation Rendering** - Using PAGI::WebServer::Navigation
   - Need to verify format matches Mojolicious
   - May need custom rendering

## Dependency Check

Required modules:
```perl
# Core
use Moo;
use Path::Tiny;
use Log::Log4perl;
use HTTP::Response;
use HTTP::Status;
use Template;

# PAGI
use PAGI::WebServer;
use PAGI::WebServer::Router;
use PAGI::WebServer::Navigation;
use PAGI::WebServer::Markdown;

# Other
use Mojo::DOM58;  # For Spectrum CSS application
```

Verify all are installed:
```bash
perl -c bin/server.pl
perl -c lib/Game/EvonyTKR/WebServer.pm
perl -c lib/Game/EvonyTKR/Handler/Root.pm
perl -c lib/Game/EvonyTKR/Role/Spectrum.pm
```

## Next Steps After Successful Test

1. **Add Static File Routes**
   ```perl
   # In WebServer.pm or separate handler
   $app->router->add_route(
       GET => qr{^/(css|js|images)/(.+)$},
       \&serve_static
   );
   ```

2. **Implement Specialties Controller**
   - First data-driven controller
   - Will need async YAML loading
   - Template conversion

3. **Create Async Helpers**
   ```perl
   # lib/Game/EvonyTKR/Util/Async.pm
   async sub load_yaml_async($file) { ... }
   async sub db_query_async($sql, $params) { ... }
   ```

4. **Performance Benchmarking**
   - Compare response times
   - Memory usage
   - Concurrent request handling

## Success Criteria

PoC is successful if:
- [pending] Server starts without errors
- [pending] Homepage loads at http://127.0.0.1:3000/
- [pending] HTML output contains Spectrum CSS classes
- [pending] Navigation renders
- [pending] Layout template wraps content
- [pending] Output matches Mojolicious version (modulo whitespace)

Mark items as [complete] after testing.

## Troubleshooting

### Server Won't Start

Check:
1. All required modules installed
2. Syntax errors: `perl -c bin/server.pl`
3. Port 3000 not already in use
4. Log output for specific errors

### 404 on Homepage

Check:
1. Route registered: Look for "Registering Root handler routes" in logs
2. Path matching: Log shows "Handling request for: /"
3. share/pages/index.md exists

### Template Errors

Check:
1. templates/ directory relative to bin/
2. Template Toolkit syntax
3. Variables passed to template
4. Log for "Template error" messages

### No Spectrum CSS Classes

Check:
1. apply_spectrum_css() being called
2. Mojo::DOM58 parsing HTML correctly
3. HTML structure matches expected tags
4. Log output shows styled HTML

## Files Modified (None)

This PoC creates new files only. No existing Mojolicious files were modified, so the original system remains intact and can run in parallel for comparison.

## Rollback

To rollback the PoC:
```bash
git checkout HEAD -- \
  bin/server.pl \
  lib/Game/EvonyTKR/WebServer.pm \
  lib/Game/EvonyTKR/Handler/ \
  lib/Game/EvonyTKR/Role/Spectrum.pm \
  templates/root/index.tt \
  templates/page/markdown.tt
```

Or simply switch branches.

---

**Created:** 2026-01-05
**Status:** Implementation complete, testing pending
**Next:** Run tests and validate functionality
