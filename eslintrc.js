const OFF = 0;
const WARNING = 1;
const ERROR = 2;

module.exports = {
  // parser: 'babel-eslint',

  extends: 'eslint:recommended',

  globals: {
    goog: true,
  },

  plugins: [],

  ecmaFeatures: {
    modules: false
  },

  // We're stricter than the default config, mostly. We'll override a few rules
  // and then enable some React specific ones.
  rules: {
    'accessor-pairs': OFF,
    'brace-style': [ERROR, '1tbs'],
    'comma-dangle': [ERROR, 'always-multiline'],
    'consistent-return': ERROR,
    'dot-location': [ERROR, 'property'],
    'dot-notation': ERROR,
    'eol-last': ERROR,
    'eqeqeq': [ERROR, 'allow-null'],
    'indent': [ERROR, 2, {SwitchCase: 1, MemberExpression: 2, outerIIFEBody: 0}],
    'max-len': [WARNING, 80, 4, {
      ignoreComments: true,
      ignoreUrls: true
    }],
    'no-bitwise': OFF,
    'no-extra-bind': ERROR,
    'no-inner-declarations': [ERROR, 'functions'],
    'no-multi-spaces': ERROR,
    'no-restricted-syntax': [ERROR, 'WithStatement'],
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
    'valid-jsdoc': [ERROR, {
      requireReturn: false,
      // This should probably be enabled.
      requireReturnDescription: false,
      prefer: {
        // Use @return instead of @returns
        returns: 'return'
      },
      preferType: {
        'string': 'String'
      }
    }],
  }
};
