#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

function cleanup() {
  local file="$1"
  local numLines="$(wc -l ${file} | awk '{ print $1 }')"
  local dataSrcName="$(rg '<dataSource name="(.*)">' /tmp/db-routines.xml --only-matching --no-line-number --replace '$1')"
  printf "Removing extra routines from datasource '%s' in file: %s\n" "${dataSrcName}" "${file}"

  local backup="/tmp/$(basename ${file}).bak"
  cp "${file}" "${backup}"
  printf "  Backing up file to %s\n" "$backup"

  printf "  Starting lines: %7s\n" "${numLines}"

  # Deletes every classify routine and the following 2 lines.  The XML is structured like:
  #
  # <routine id="294" parent="6" name="classify_event_1003960162_f5ac6bf7eaeb0811890fa0bf">
  #   <RoutineKind>function</RoutineKind>
  # </routine>
  sed -e '/<routine.*name="classify_event_/{N;N;d;}' "${backup}" > "${file}"
  local endLines="$(wc -l ${file} | awk '{ print $1 }')"
  printf "  Ending lines:   %7s\n" "${endLines}"
}

cleanup "$@"
