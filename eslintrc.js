const OFF = 0;
const WARNING = 1;
const ERROR = 2;

// Private Google tags.
const GOOGLE_CUSTOM_JSDOC_TAGS = [
  'abstract',
  'copyright',
  'disposes',
  'externs',
  // Mark function as returning an ID.  The type can be {consistent}, {unique},
  // {stable}, {xid} or empty.
  'idGenerator',
  'jaggerInject',
  'jaggerModule',
  'jaggerProvide',
  'jaggerProvidePromise',
  'meaning',  // Localization helper.
  'modifies',  // For externs.
  'nocollapse',
  'nocompile',
  'nosideeffects',
  'package', // Indicates package-private.,
  'polymerBehavior',
  'record',
  'struct',
  'template',  // Generics.
  'unrestricted', // Mark class that's not a @struct or @dict.,
  'visibility', // Control blaze build visibility.
  'wizaction',
  'ngInject',
];

// Pubically released closure tags from
// https://developers.google.com/closure/compiler/docs/js-for-compiler
const CLOSURE_JSDOC_TAGS = [
    'abstract',
    'const',
    'constructor',
    'define',
    'deprecated',
    'dict',
    'enum',
    'export',
    'extends',
    'final',
    'implements',
    'implicitCast',
    'inheritDoc',
    'interface',
    'lends',
    'license',
    'preserve',
    'nocollapse',
    'nosideeffects',
    'override',
    'package',
    'param',
    'private',
    'protected',
    'record',
    'return',
    'struct',
    'template',
    'this',
    'throws',
    'type',
    'typedef',
    'unrestricted',
];

module.exports = {
  // parser: 'babel-eslint',

  extends: 'eslint:recommended',

  globals: {
    goog: true,
  },

  plugins: [
    'jsdoc',
    'google',
  ],

  ecmaFeatures: {
    modules: false
  },

  rules: {
    'accessor-pairs': OFF,
    'brace-style': [ERROR, '1tbs'],
    'comma-dangle': [ERROR, 'always-multiline'],
    'consistent-return': ERROR,
    'dot-location': [ERROR, 'property'],
    'dot-notation': ERROR,
    'eol-last': ERROR,
    // The style guide says nothing about the great == vs === debate.
    'eqeqeq': [OFF, 'allow-null'],

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

    // Allow opt_ prefix and var_args in identifiers.  From
    // https://google.github.io/styleguide/javascriptguide.xml?showone=Naming#Naming
    'google/camelcase-optionals': WARNING,
    // The JS style guide 'follows the C++ style guide in spirit'.  The C++
    // style guide mandates two spaces before line-end comments.  See the 'Line
    // Comments' section under
    // https://google.github.io/styleguide/cppguide.html#Implementation_Comments
    'google/line-end-spaced-comment': [ERROR, 2],

    "jsdoc/check-param-names": ERROR,
    "jsdoc/check-tag-names": ERROR,
    "jsdoc/check-types": ERROR,
    "jsdoc/newline-after-description": ERROR,
    "jsdoc/require-description-complete-sentence": ERROR,
    "jsdoc/require-hyphen-before-param-description": ERROR,
    "jsdoc/require-param": ERROR,
    "jsdoc/require-param-description": ERROR,
    "jsdoc/require-param-type": ERROR,
    "jsdoc/require-returns-description": ERROR,
    "jsdoc/require-returns-type": ERROR,

  },

  settings: {
    jsdoc: {
      additionalTagNames: {
        customTags: GOOGLE_CUSTOM_JSDOC_TAGS.concat(CLOSURE_JSDOC_TAGS),
      },
      tagNamePreference: {
        returns: "return",
      }
    }
  }
};
