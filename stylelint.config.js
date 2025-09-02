export default {
  extends: 'stylelint-config-standard',
  ignoreFiles: ['**/node_modules/**'],
  rules: {
    'no-invalid-position-at-import-rule': null,
    'at-rule-no-unknown': [
      true,
      {
        ignoreAtRules: ['import', 'nest', 'apply', 'responsive'],
      },
    ],
    'selector-class-pattern': [
      '^([a-z][a-z0-9]*)(-[a-z0-9]+)*$|^spectrum(-[A-Za-z0-9]+)*(-{1,2}[a-zA-Z0-9]+)*$',
      {
        message:
          'Class selectors should use kebab case or match Spectrum CSS patterns',
      },
    ],
    'selector-pseudo-class-no-unknown': true,
    'selector-pseudo-element-no-unknown': true,
    'no-invalid-position-at-import-rule': true,
  },
};
