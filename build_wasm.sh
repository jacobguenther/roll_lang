!#/bin/bash

wasm-pack build --target no-modules --out-dir temp

cp temp/roll_lang* www/