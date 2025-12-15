default:
    @just --list

check:
    clj-kondo --config-dir resources/clj-kondo.exports/ --lint clj-kondo-test

test:
    clojure -M:test
    just check

build *args:
    clojure -T:build build {{ args }}

release:
    clojure -T:build release
