# DND Application

This application is a set of applications for D&D.

It grew out of a point buy calculator.

Currently has a set of ciphers, but will have more soon (I hope).

It is deployed to Github [Pages](https://scottsedgwick.github.io/pointbuy/).

## The original cute stuff

The point is to demonstrate how to:

* Write a web application in Haskell, compiling to WASM and deploying to Pages.
* Compile said Haskell WASM application in GitHub Actions.

The "interesting" code is actually in `.github/workflows/static.yml` and `Rakefile.rb`.  Yes, I wrote the build scripts in Rake.  I should probably convert it to something more Haskell friendly, but it was quick, OK?