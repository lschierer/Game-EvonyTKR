import { build } from 'esbuild';
import { glob } from 'glob';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import fs from 'fs';
import { execSync } from 'child_process';
import path from 'path';
import process from 'node:process';

const __dirname = dirname(fileURLToPath(import.meta.url));

async function main() {
  // First run TypeScript to generate declaration files
  // First run TypeScript to generate declaration files
  console.log('Running TypeScript compiler...');
  try {
    execSync('pnpm exec tsc -p ./tsconfig.build.json', {
      stdio: 'inherit',
      cwd: process.cwd(),
    });
  } catch (error) {
    console.error('TypeScript compilation failed', error);
    process.exit(1);
  }

  // Find all TypeScript files
  const entryPoints = await glob(path.join(process.cwd(), './ts/**/*.ts'), {
    ignore: ['node_modules/**', 'public/**', '**/*.d.ts', 'build.js'],
    cwd: __dirname,
  });

  // Build with esbuild
  console.log(`Building with esbuild: ${JSON.stringify(entryPoints)}`);
  await build({
    entryPoints,
    outdir: 'public/js',
    outbase: './ts',
    bundle: true,
    splitting: false,
    platform: 'browser',
    format: 'esm',
    target: 'es2020',
    sourcemap: true,
    outExtension: { '.js': '.js' },
  });

  // Fix imports in the generated JS files to include .js extensions
  console.log('Fixing import extensions...');
  await fixImportExtensions();

  console.log('Build completed successfully!');
}

async function fixImportExtensions() {
  const jsFiles = await glob('public/**/*.js', { cwd: __dirname });

  for (const file of jsFiles) {
    const filePath = path.join(__dirname, file);
    let content = fs.readFileSync(filePath, 'utf8');

    // Replace imports without extensions
    content = content.replace(
      /from\s+['"]([^'"]*?)['"];/g,
      (match, importPath) => {
        // Skip external modules
        if (importPath.startsWith('.')) {
          // Add .js extension if it doesn't have one
          if (!importPath.endsWith('.js')) {
            return `from '${importPath}.js';`;
          }
        }
        return match;
      },
    );

    fs.writeFileSync(filePath, content);
  }
}

main().catch((err) => {
  console.error('Build failed:', err);
  process.exit(1);
});
