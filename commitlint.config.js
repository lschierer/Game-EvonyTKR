export default {
  extends: [
    '@commitlint/config-conventional'
  ],
  /*
   * type-enum is disabled so any type keyword is accepted (not just the
   * conventional-commits list). The structure "type: subject" is still
   * enforced by the other rules (type-empty, subject-empty, etc).
   */
  rules: {
    'type-enum': [0],
  },
};
