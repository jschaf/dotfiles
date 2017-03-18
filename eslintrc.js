/**
 * @fileoverview Personal ESLint config.
 */

/* eslint-env node */

// Named constants for the numbers eslint uses to indicate lint severity.
const OFF = 0;
const WARNING = 1;
const ERROR = 2;

// ESLint configuration object.  Options are described at
// http://eslint.org/docs/user-guide/configuring.
const PERSONAL_ESLINT_CONFIG = {

  extends: [
    'eslint-config-closure-es6',
  ],

  parserOptions: {
    ecmaVersion: 6,
    sourceType: 'script',
  },

  // An environment defines global variables that are predefined.
  env: {
  },

  globals: {
  },

  plugins: [
  ],

  // The list of rules and options are available at
  // http://eslint.org/docs/rules/.
  rules: {
    // The old closure style guide had 3 lines after goog.scope.
    'no-multiple-empty-lines': [WARNING, {max: 3, maxEOF: 1, maxBOF: 0}],
  },

  // ESLint supports adding shared settings into configuration file.  The
  // settings object will be supplied to every rule that will be executed.
  settings: {},

};


module.exports = PERSONAL_ESLINT_CONFIG;
