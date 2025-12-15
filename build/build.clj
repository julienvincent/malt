(ns build
  (:require
   [clojure.string :as str]
   [clojure.tools.build.api :as b]
   [k16.kaven.deploy :as kaven.deploy]))

(def class-dir "target/classes")
(def jar-file "target/lib.jar")
(def lib 'io.julienvincent/malt)

(defn build [{:keys [version]}]
  (b/delete {:path "target"})

  (let [basis (b/create-basis {})
        version (str/replace
                 (or version "0.0.0-SNAPSHOT")
                 #"v" "")]
    (b/copy-dir {:src-dirs (:paths basis)
                 :target-dir class-dir})

    (b/write-pom {:class-dir class-dir
                  :lib lib
                  :version version
                  :basis basis
                  :src-dirs (:paths basis)
                  :pom-data [[:url "https://github.com/julienvincent/malt"]
                             [:description "Malli-Typed interfaces for Clojure"]
                             [:licenses
                              [:license
                               [:name "MIT"]
                               [:url "https://opensource.org/license/mit"]]]]})

    (b/jar {:class-dir class-dir
            :jar-file jar-file})))

(def ^:private clojars-credentials
  {:username (System/getenv "CLOJARS_USERNAME")
   :password (System/getenv "CLOJARS_PASSWORD")})

(defn release [_]
  (kaven.deploy/deploy
   {:jar-path (b/resolve-path jar-file)
    :repository {:id "clojars"
                 :credentials clojars-credentials}}))
