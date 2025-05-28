export default {
  extends: "stylelint-config-standard",
  ignoreFiles: ["**/node_modules/**"],
  rules: {
    "no-invalid-position-at-import-rule": null,
    "at-rule-no-unknown": [
      true,
      {
        ignoreAtRules: ["import", "nest", "apply", "responsive"],
      },
    ],
    "selector-class-pattern": null,
  },
};
