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
    // 'eslint-config-googlejs-es5'
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
  // rules: {
  //   'comma-dangle': [ERROR, 'always-multiline'],
  // },

  rules: {


    'accessor-pairs': OFF,
    'dot-location': [ERROR, 'property'],
    'dot-notation': ERROR,
    'eol-last': ERROR,

    // The style guide says nothing about the great == vs === debate.
    'eqeqeq': [OFF, 'allow-null'],
    
    // Disable because it doesn't intepret goog.scope as an IIFE.
    'indent': [OFF, 2, {SwitchCase: 1, MemberExpression: 2, outerIIFEBody: 0}],

    'max-len': [WARNING, 80, 4, {
      ignoreComments: true,
      ignoreUrls: true
    }],
    'no-bitwise': OFF,
    'no-extra-bind': ERROR,
    'no-inner-declarations': [ERROR, 'functions'],
    'no-multi-spaces': ERROR,
    'no-shadow': ERROR,
    // Disable undefined warnings until we we add goog.provide and require
    // parsing.
    'no-undef': OFF,
    'no-unused-expressions': OFF,
    'no-unused-vars': [ERROR, {args: 'none'}],
    'quotes': [ERROR, 'single', 'avoid-escape'],
    'require-jsdoc': [WARNING, {require: {FunctionDeclaration: true,
                                          MethodDefinition: true,
                                          ClassDeclaration: true,
                                         }}],
    'space-before-blocks': ERROR,
    'space-before-function-paren': [ERROR, {anonymous: 'never', named: 'never'}],
    // 'strict': [ERROR, 'global'],

  },

  // ESLint supports adding shared settings into configuration file.  The
  // settings object will be supplied to every rule that will be executed.
  settings: {},

};


module.exports = PERSONAL_ESLINT_CONFIG;
