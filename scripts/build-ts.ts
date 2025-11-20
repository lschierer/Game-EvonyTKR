import { build } from 'esbuild';
import { glob } from 'glob';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import fs from 'fs';
import { execSync } from 'child_process';
import path from 'path';
import process from 'node:process';
import { litCssPlugin } from 'esbuild-plugin-lit-css';

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
  const entryPoints = await glob(path.join(process.cwd(), './lib/**/*.ts'), {
    ignore: ['node_modules/**', 'public/**', '**/*.d.ts', './scripts/*.ts'],
    cwd: __dirname,
  });

  // Build with esbuild
  console.log(`Building with esbuild: ${JSON.stringify(entryPoints)}`);
  await build({
    entryPoints,
    outdir: 'share/public/js',
    outbase: './',
    bundle: true,
    splitting: false,
    platform: 'browser',
    format: 'esm',
    target: 'es2020',
    sourcemap: true,
    outExtension: { '.js': '.js' },
    plugins: [
      litCssPlugin(),
      {
        name: 'per-file-constant',
        setup(build) {
          build.onLoad({ filter: /\.ts$/ }, async (args) => {
            const relativePath = path.relative(process.cwd(), args.path);
            const contents = await fs.promises.readFile(args.path, 'utf8');

            // Find all instances of a special token and replace with the file path
            const modifiedContents = contents.replace(
              /__FILE_PATH__/g,
              JSON.stringify(relativePath),
            );

            return {
              contents: modifiedContents,
              loader: 'ts',
            };
          });
        },
      },
    ],
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
        if ((importPath as string).startsWith('.')) {
          // Add .js extension if it doesn't have one
          if (!(importPath as string).endsWith('.js')) {
            return `from '${importPath}.js';`;
          }
        }
        return match;
      },
    );

    fs.writeFileSync(filePath, content);
  }
}

main().catch((err: unknown) => {
  console.error('Build failed:', err);
  process.exit(1);
});
