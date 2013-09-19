(defproject Archimedes "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 ;; [org.clojure.contrib/monads "1.3.0-SNAPSHOT"]
                 ;; [org.clojure.contrib/logging "1.3.0-SNAPSHOT"]
                 ;; [log4j "1.2.16"]
                 [org.clojure/core.logic "0.7.5"]]
  ;;  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]]
  ;;  :namespaces [Archimedes.core]
  :java-source-paths ["java"]
  :aliases {"build" ["do" "run" "-m" "Archimedes.bar/code-gen" "java/archimedes/MathOps.java," "javac"]}
  )
