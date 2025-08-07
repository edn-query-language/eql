# Changelog

## Not yet released
- Add variadic versions of `merge-ast` and `merge-queries`

## [2021.07.18]
- `merge-ast` support arities 0 and 1 to work as a reducing function

## [2021.02.28]
- Add `update-child` helper
- Add `update-recursive-depth` helper

## [1.0.2]
- In the specs for AST, :query is now optional

## [1.0.0]
- Generative features moved to `edn-query-language.core` to avoid requiring test.check for basic operations

## [0.0.10]
- `ast->query` always returns a query, this is a bug fix but if you relied on the bad behavior this may be a breaking change, if that's the case replace your call to `ast->query` with `ast->expr`

## [0.0.9]
- Add helper to mask queries.

## [0.0.8]
- Make out of `query->ast1` nilable

## [0.0.7]
- Fix specs for `query->ast1` and `ast->query`

## [0.0.6]
- Removed `::eql/key` spec, that was a leftover from porting, `::eql/join-key` is the correct one to use.

## [0.0.5]
- focus-subquery* is public
- support removing specs from Clojurescript build

## [0.0.4]
- Add `eql/query->shallow-ast`
