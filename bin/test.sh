#!/bin/sh
# Transpile the ABAP sources and run the ABAP Unit tests locally (open-abap).
# Same steps as the GitHub Actions workflow (.github/workflows/test.yml).
#
# ci-build/src is a merge of src/ with the SAP built-in DTEL stubs in
# ci/dtel-stubs/ (INTTYPE/ILEN/DECIMALS are not shipped by open-abap-core;
# the stubs must not go into src/, abapGit would try to import them).
set -e

npm install

echo "Preparing transpile input (src + DDIC stubs) ..."
rm -rf ci-build
mkdir ci-build
cp -r src ci-build/src
cp ci/dtel-stubs/*.dtel.xml ci-build/src/

echo "Building ..."
./node_modules/.bin/abap_transpile transpile_for_testing.json

echo "Running unit tests ..."
node transpiled/index.mjs
