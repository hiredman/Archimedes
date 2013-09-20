(defproject archimedes "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.logic "0.7.5"]]
  :java-source-paths ["java"]
  ;; TODO: lein issue
  :aliases {"build" ["do"
                     "update-in" ":prep-tasks" "empty" "--"
                     "run" "-m" "Archimedes.bar/code-gen" "java/archimedes/MathOps.java" "MathOps,"
                     "javac"]})
