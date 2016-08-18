#!/bin/zsh

# Run a named lint in an eslint repository.
function my-run-eslint-test() {
    # Fix colors on solarized dark.  See
    # https://github.com/mochajs/mocha/issues/802.
    substitution='s/\x1b\[90m/\x1b[92m/g'
    ./node_modules/.bin/mocha -c "tests/lib/rules/${1}.js" \
                              > >(perl -pe "$substitution") \
                              2> >(perl -pe "$substitution" 1>&2)
}
