# Template Status for Thunderhorse MVP

## ✅ Templates Ready (with layout wrapper)

### Core Templates
- **templates/layouts/default.tt** - Main layout ✅
- **templates/markdown.tt** - Generic markdown pages ✅
- **templates/root/index.tt** - Homepage ✅

### Specialties (MVP)
- **templates/specialties/index.tt** - List of all specialties ✅
- **templates/specialties/details.tt** - Individual specialty details ✅

### Supporting Templates
- **templates/buffs/single.tt** - Buff display (included by specialties/details.tt) ✅

### Books (Partially Ready)
- **templates/books/index.tt** - Books overview
- **templates/books/skill_books_index.tt** - Skill books list
- **templates/books/generic_books_index.tt** - Generic books list
- **templates/books/details.tt** - Book details

**Status:** Templates exist but need wrapper check

### Page Template
- **templates/page/markdown.tt** - MISSING wrapper, just has `[% content %]`

**Status:** Needs `[% WRAPPER layouts/default.tt %]` added

## ⚠️ Templates Needing Conversion (.html.ep → .tt)

### Generals
- templates/generals/index.html.ep
- templates/generals/details.html.ep
- templates/generals/GeneralTableSingle.html.ep
- templates/generals/pairs/index.html.ep
- templates/generals/pairs/typeIndex.html.ep
- templates/generals/pairs/GeneralTablePair.html.ep
- templates/generals/pairs/loading.html.ep
- templates/generals/uiTarget/index.html.ep
- templates/generals/uiTarget/index_with_file.html.ep
- templates/generals/uiTarget/buffActivation/index.html.ep

### Covenants
- templates/covenants/index.html.ep
- templates/covenants/details.html.ep

### Ascending Attributes
- templates/ascending attributes/index.html.ep
- templates/ascending attributes/details.html.ep

### General Conflict Groups
- templates/general conflict groups/index.html.ep
- templates/general conflict groups/details.html.ep

### Glossary
- templates/glossary/index.html.ep

### Other
- templates/pairs/diagnostic.html.ep
- templates/example/welcome.html.ep
- templates/index.html.ep (if different from root/index.tt)
- templates/markdown.html.ep (if different from markdown.tt)

## Template Variables Expected by Layout

The `layouts/default.tt` expects these variables:

- **title** - Page title (defaults to 'Evony TKR Tips')
- **sidebar** - Boolean, show sidebar navigation (defaults to 1)
- **navigation** - HTML for navigation menu
- **content** - Main page content
- **css_files** - Array of CSS files to include (optional)
- **js_files** - Array of JS files to include (optional)
- **current_year** - For footer copyright

## CSS Files Referenced

Templates reference these CSS files (should be in public/css/):

- /css/global.css
- /css/spectrum-overrides.css
- /css/collectionIndex.css (for list pages)
- /css/collectionDetails.css (for detail pages)

## Template Partials Referenced

- partials/global_header.tt
- partials/global_footer.tt

## Quick Template Wrapper Pattern

When converting .html.ep to .tt or adding wrappers:

```template-toolkit
[% WRAPPER layouts/default.tt %]

<!-- Your content here -->
[% IF items -%]
  [% FOREACH item IN items -%]
    <li>[% item.name %]</li>
  [% END -%]
[% END -%]

[% END %]
```

## Checking Templates for Wrappers

Run this to find .tt templates without wrappers:

```bash
cd /Volumes/Development/luke/src/PAGI-WebServer/Game-EvonyTKR/templates
grep -L "WRAPPER layouts/default.tt" **/*.tt
```

## Next Steps

1. **For MVP (Specialties):** All templates ready! ✅
2. **For Books:** Check book templates have wrappers
3. **For Generals:** Convert .html.ep to .tt with wrappers
4. **For Others:** Convert as needed when migrating those controllers

## Template Conversion Priority

Based on migration plan:

1. ✅ **Specialties** - DONE
2. 🔄 **Books** - Templates exist, check wrappers
3. ⏳ **Generals** - Need conversion
4. ⏳ **Covenants** - Need conversion
5. ⏳ **Others** - Convert as needed
