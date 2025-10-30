# Point Buy

This application is (close to) a clone of Chicken Dinners 5e Point buy Calculator.

The point is to demonstrate how to:

* Write a web application in Haskell, compiling to WASM and deploying to Pages.
* Compile said Haskell WASM application in GitHub Actions.

The "interesting" code is actually in `.github/workflows/static.yml` and `Rakefile.rb`.  Yes, I wrote the build scripts in Rake.  I should probably convert it to something more Haskell friendly, but it was quick, OK?