require 'rake/clean'

DEPLOY_FOLDER = 'docs'
BUILD_FOLDER = 'dist-newstyle'
STATIC_SRC_FOLDER = 'html-src'

directory DEPLOY_FOLDER

CLEAN.include(DEPLOY_FOLDER)
CLOBBER.include(BUILD_FOLDER)

desc 'Build app'
task :build do
    sh 'wasm32-wasi-cabal build --allow-newer'
end


desc 'Generate the JS files WASM needs to work in the browser'
task :wasm => [:build, DEPLOY_FOLDER] do
    libdir = `wasm32-wasi-ghc --print-libdir`.strip
    postlink = "#{libdir}/post-link.mjs"
    wasmexe = `wasm32-wasi-cabal list-bin pointbuy --allow-newer`.strip
    sh "#{postlink} --input #{wasmexe} --output #{DEPLOY_FOLDER}/ghc_wasm_jsffi.js"
    FileUtils.cp wasmexe, "#{DEPLOY_FOLDER}/app.wasm"
end

task :static => [DEPLOY_FOLDER] do
    Dir.foreach(STATIC_SRC_FOLDER) do |f|
        if !f.start_with?(".") then
            FileUtils.cp "#{STATIC_SRC_FOLDER}/#{f}", "#{DEPLOY_FOLDER}/#{f}"
        end
    end
end

desc "Deploy app"
task :deploy => [:static, :build, :wasm]

desc "Deploy and serve app"
task :serve => :deploy do
    sh "http-server #{DEPLOY_FOLDER}"
end

desc "Generate freeze file for dependencies"
task :serve => :deploy do
    sh "wasm32-wasi-cabal freeze --allow-newer"
end