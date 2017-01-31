## Test environments
* local Ubuntu install, R 3.3.2
* local Debian install, R 3.3.2
* Ubuntu on Travis CI, R 3.3.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* Maintainer: 'Adriano Rivolli <rivolli@utfpr.edu.br>'
New submission
Possibly mis-spelled words in DESCRIPTION:
  Brazdil (19:3)
  Giraud (19:16)
  Soares (19:36)
  Vilalta (19:48)
  datasets (10:42, 13:3)

This words are correct. The notes occur only in windows test.

## Resubmission
This is a resubmission. In this version I have:

* Re-write the description removing the package name on it and adding a reference 
  to an article describing what "meta-features" and/or "meta-learning" are.

## Downstream dependencies
There are no downstream dependencies.
