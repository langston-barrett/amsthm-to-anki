#!/usr/bin/env bash

########################### Stylize

# stylish-haskell kills safe imports, so i won't use it until
# jaspervdj/stylish-haskell#90 is fixed.

for file in $(find . -name "*.hs" -type f -not -path "./.stack-work"); do
  # hindent has no --inplace flag
  cat "$file" | hindent -XArrows -XDeriveGeneric \
  -XDeriveDataTypeable -XExplicitForAll -XFlexibleContexts -XFlexibleInstances \
  -XGADTs -XGeneralizedNewtypeDeriving -XMultiWayIf -XMultiParamTypeClasses \
  -XNoImplicitPrelude -XNondecreasingIndentation \
  -XOverloadedStrings -XPartialTypeSignatures \
  -XScopedTypeVariables -XStandaloneDeriving -XTypeOperators \
                  > "$file".tmp
  mv "$file".tmp "$file"
done

########################### Make sure we always "import safe" safe modules
safe_modules=(
  Control.Eff
  Control.Eff
  Control.Eff.Lift
  Control.Eff.Writer.Strict
  Data.Data
  Data.OpenUnion
  Data.Typeable
  Options.Applicative
)

# for file in $(find . -name "*.hs" -type f -not -path "./.stack-work"); do
#   for mod in "${safe_modules[@]}"; do
#     sed -i "s/import(\\s+)$mod/import safe $mod"
#   done
# done

########################### De-Unicode
for file in $(find . -name "*.hs" -type f -not -path "./.stack-work"); do
  sed -i 's/∘/./g'      "$file"
  sed -i 's/∀/forall/g' "$file"
  sed -i 's/→/->/g'     "$file"
  sed -i 's/⇒/=>/g'     "$file"
  sed -i 's|≢|/=|g'     "$file"
  sed -i 's/∷/::/g'     "$file"
  sed -i 's/≡/==/g'     "$file"
  sed -i 's/∨/||/g'     "$file"
  sed -i 's/∧/&&/g'     "$file"
done

