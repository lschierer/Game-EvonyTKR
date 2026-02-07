/* tslint:disable-next-line */
import eslint from '@eslint/js';
import { defineConfig } from 'eslint/config';
import tseslint from 'typescript-eslint';
import globals from 'globals';

export default defineConfig(
  {
    ignores: ['**/dist/**', '**/public/**', '**/cdk.out/**'],
  },
  {
    extends: [
      tseslint.configs.recommendedTypeChecked,
      tseslint.configs.strictTypeChecked,
    ],
    rules: {
      'no-unused-vars': 'off',
      '@typescript-eslint/no-unused-vars': 'error',
      '@typescript-eslint/consistent-type-imports': [
        'error',
        {
          prefer: 'type-imports',
          fixStyle: 'inline-type-imports',
        },
      ],
      '@typescript-eslint/restrict-template-expressions': [
        'error',
        {
          allowNumber: true,
        },
      ],
    },
    languageOptions: {
      ecmaVersion: 'latest',
      sourceType: 'module',
      globals: {
        ...globals.browser,
      },
      parserOptions: {
        projectService: {
          allowDefaultProject: ['eslint.config.mts', 'commitlint.config.js', 'stylelint.config.js'],
        },
        tsconfigRootDir: import.meta.dirname,
        projectFolderIgnoreList: ['**/node_modules/**'],
      },
    },
  },
  {
    files: ['**/*/*.js', '**/*/*.mjs'],
    ignores: ['packages/greenwood/public/**'],
    extends: [eslint.configs.recommended, tseslint.configs.disableTypeChecked],
  },

  // Add a new configuration specifically for cdk
  {
    files: [
      'infrastructure/**/*.ts',
    ],
    rules: {
      '@typescript-eslint/no-unused-vars': 'off',
    },
  },
  {
    files: ['packages/infrastructure/sst.config.ts'],
    rules: {
      '@typescript-eslint/triple-slash-reference': 'off',
    },
  },
);
