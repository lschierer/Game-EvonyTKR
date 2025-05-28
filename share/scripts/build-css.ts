import fs from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import postcss from 'postcss';

// Import PostCSS plugins directly
import postcssImport from 'postcss-import';
import postcssExtend from 'postcss-extend';
import postcssNesting from 'postcss-nesting';
import postcssSorting from 'postcss-sorting';
import autoprefixer from 'autoprefixer';
import cssnano from 'cssnano';
import stylelint from 'stylelint';
import { type CssSyntaxError } from 'postcss';
import { exit } from 'node:process';

interface CliArgs {
  outDir: string;
  minify: boolean;
}

function parseArgs(argv: string[]): CliArgs {
  const args = argv.slice(2);
  if (args.length < 1) {
    console.error('Usage: tsx build-css.ts <output-dir> [--minify]');
    process.exit(1);
  }

  return {
    outDir: args[0],
    minify: args.includes('--minify'),
  };
}

async function buildCSS({ outDir, minify }: CliArgs) {
  const stylesDir = path.resolve('styles');
  const outputDir = path.resolve(outDir);

  try {
    const result = await stylelint.lint({
      configFile: 'stylelint.config.js',
      files: 'styles/*.css',
      fix: true,
    });
    // do things with result.report, result.errored, and result.results
    if (result.errored && result.report) {
      console.error('css error:', result.report);
      exit(1);
    }
  } catch (err) {
    // do things with err e.g.
    console.error((err as object)['stack' as keyof typeof err]);
  }

  const plugins = [
    postcssImport({
      path: ['node_modules', '.', './styles'],
    }),
    postcssExtend(),
    postcssNesting(),
    postcssSorting({
      order: ['custom-properties', 'declarations', 'at-rules', 'rules'],
      'properties-order': 'alphabetical',
    }),
    autoprefixer(),
  ];

  if (minify) {
    plugins.push(cssnano({ preset: 'default' }));
  }

  await fs.mkdir(outputDir, { recursive: true });

  const entries = await fs.readdir(stylesDir);
  const cssFiles = entries.filter((file) => file.endsWith('.css'));

  for (const file of cssFiles) {
    const inputPath = path.join(stylesDir, file);
    const outputPath = path.join(outputDir, file);

    const css = await fs.readFile(inputPath, 'utf8');

    try {
      const result = await postcss(plugins).process(css, {
        from: inputPath,
        to: outputPath,
      });

      await fs.writeFile(outputPath, result.css, 'utf8');
      console.log(`✔ Built ${path.relative(process.cwd(), outputPath)}`);
    } catch (err) {
      if (typeof err === 'object' && err) {
        if (err instanceof Error) {
          console.warn(`⚠️  Failed to build ${file}: ${err.message}`);
        }
        if ('name' in err && err.name === 'CssSyntaxError') {
          console.warn((err as CssSyntaxError).showSourceCode());
        }
      }
    }
  }
}

const args = parseArgs(process.argv);
buildCSS(args).catch((err: unknown) => {
  console.error('❌ CSS build failed:', err);
  process.exit(1);
});
