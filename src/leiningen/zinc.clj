(ns leiningen.zinc
  (:require [leiningen.classpath :as classpath]
            [leiningen.core.main :as main]
            [leiningen.core.project :as project]
            [leiningen.deps :as deps]
            [leiningen.help :as help]
            [clojure.set :as cs]
            [clojure.tools.namespace.track :as track]
            [zinc.core :as core]
            [zinc.lein :as lein])
  (:import (sbt.internal.inc ZincUtil Locate LoggedReporter ScalaInstance AnalyzingCompiler)
           (xsbti.compile Inputs Setup IncOptions IncrementalCompiler PerClasspathEntryLookup CompilerCache ZincCompilerUtil ClasspathOptionsUtil CompileOptions CompileOrder AnalysisStore FileAnalysisStore PreviousResult)
           (scala Option Function1)
           (java.util Optional)
           (xsbti T2)
           (java.net URLClassLoader URL)
           (sbt.internal.inc.classpath ClasspathUtilities)
           (java.io File PrintStream)
           (java.util.function Function)
           (sbt.internal.util ConsoleLogger)))
(def ^:dynamic scala-version "2.12.8")
(def ^:dynamic zinc-version "1.2.5")

(def compile-jar
  (core/to-file (lein/maven-local-repo-path
                  "org.scala-lang/scala-compiler" scala-version)))

(defn analysis-store [cache-file]
  (AnalysisStore/getCachedStore (FileAnalysisStore/getDefault cache-file)))

(defn extra-jar [project]
  (let [deps (:dependencies project)]
    (map (fn [[id version]]
           (core/to-file (lein/maven-local-repo-path
                           (name id) version))) deps))
  )
(def library-jar
  (core/to-file (lein/maven-local-repo-path
                  "org.scala-lang/scala-library" scala-version)))
(def compiler-bridge-jar
  (core/to-file (lein/maven-local-repo-path
                  "org.scala-sbt/zinc_2.12" zinc-version)))
(def reflect-jar
  (core/to-file
    (lein/maven-local-repo-path
      "org.scala-lang/scala-reflect" scala-version)))

(def lookup (reify PerClasspathEntryLookup
              (analysis [this classpathEntry]
                (Optional/empty))
              (definesClass [this classpathEntry]
                (Locate/definesClass classpathEntry))))

(def default-sources ["src/scala" "src/java"])
(def default-test-sources ["test/scala" "test/java"])


(defn- option "Returns scala.Some(arg) if arg is not nil else scala.None."
  [arg]
  (if (nil? arg) (Option/apply nil) (new scala.Some arg)))

(defn zinc-logger "Instantiate zinc logger." [project]
  (ConsoleLogger/apply (PrintStream. System/out)))

(defn zincCompiler "Instantiates zinc compiler."
  []
  (ZincUtil/defaultIncrementalCompiler))

(defn- combine-sources
  "Merges leiningen standard source-paths, java-source-paths, and 
  zinc inputs sources. Zinc input sources is deprecated. Leiningen standard 
  paths should be used to specify the source paths as it is recognized by 
  IDEs such as IntelliJ."
  [project]
  (into #{} (cs/union #_(:source-paths project)
                      (:java-source-paths project)
                      [(str (:root project) "/src/java")
                       (str (:root project) "/src/scala")])))

(def tmpdir (System/getProperty "java.io.tmpdir"))

(defn scala-instance [extra-jars]
  (ScalaInstance.
    scala-version
    (URLClassLoader. (into-array URL
                                 (map #(.toURL %) [library-jar
                                                   reflect-jar
                                                   compile-jar])))
    (ClasspathUtilities/rootLoader)
    library-jar
    compile-jar
    (into-array File (concat extra-jars [library-jar reflect-jar compile-jar]))
    (Option/apply scala-version)))

(defn scala-compilers [scala-instance]
  (AnalyzingCompiler.
    scala-instance
    (ZincCompilerUtil/constantBridgeProvider
      scala-instance
      compiler-bridge-jar)
    (ClasspathOptionsUtil/auto)
    (reify Function1
      (apply [this _]))
    (Option/apply nil)))

(defn compilers [scala-instance scala-compilers]
  (ZincUtil/compilers
    scala-instance
    (ClasspathOptionsUtil/boot)
    (Option/apply nil)
    scala-compilers))

(defn options [class-paths sources class-directory scala-options java-options]
  (CompileOptions/of
    (into-array File class-paths)
    (into-array File sources)
    class-directory
    (into-array String scala-options)
    (into-array String java-options)
    100
    (Function/identity)
    CompileOrder/Mixed))

(defn zincInputs "Instantiates zinc inputs." [project test?]
  (let [classpath    (classpath/get-classpath-string project)
        {:keys [classes test-classes scalac-options
                javac-options analysis-cache test-analysis-cache analysis-map
                compile-order mirror-analysis-cache]
         :or   {classes               "target/classes"
                test-classes          "target/test-classes"
                scalac-options        []
                javac-options         []
                analysis-cache        "target/analysis/compile"
                test-analysis-cache   "target/analysis/test-compile"
                analysis-map          {tmpdir tmpdir}
                compile-order         "Mixed"
                mirror-analysis-cache false}}
        (:inputs (:zinc-options project))
        sources      (combine-sources project)
        test-sources (:test-paths project)
        setup        (Setup/of lookup
                               false
                               (if test? (core/to-file test-analysis-cache)
                                         (core/to-file analysis-cache))
                               (CompilerCache/fresh)
                               (IncOptions/of)
                               (LoggedReporter. 100 (zinc-logger project)
                                                (reify Function1
                                                  (apply [this args]
                                                    args)))
                               (Optional/empty)
                               (make-array T2 0))
        scala-instance (scala-instance (extra-jar project))
        scala-compilers (scala-compilers scala-instance)
        compilers (compilers scala-instance scala-compilers)
        class-paths (map #(core/to-file %)
                         (core/to-seq classpath ":"))
        options (options (conj class-paths (core/to-file classes))
                         (map #(core/to-file %) sources)
                         (core/to-file classes)
                         scalac-options
                         javac-options)
        analysis-store (analysis-store (core/to-file analysis-cache))
        previous-result (fn [] (if (.isPresent (.get analysis-store))
                         (PreviousResult/of
                           (-> analysis-store
                               .get
                               .get
                               .getAnalysis
                               Optional/of)
                           (-> analysis-store
                               .get
                               .get
                               .getMiniSetup
                               Optional/of))
                         (PreviousResult/of (Optional/empty) (Optional/empty))))]
    (main/debug "classpath: " class-paths)
    (main/info "sources: " sources)
    (main/info "test-sources: " test-sources)
    (main/debug "analysis-cache: " analysis-cache)
    (main/debug "analysis-map: " analysis-map)

    (try (Inputs/of compilers options setup (previous-result))
         (catch Exception e
           (main/abort "Invalid parameter. " (.getMessage e))))))


(defn- do-compile [project test?]
  (let [logger (zinc-logger project)]
    (try (.compile (zincCompiler)
                   (zincInputs project test?) logger)
         (catch Exception e (main/info (.getMessage e))))))

(defn zinc-compile "Compiles Java and Scala source." [project]
  (do-compile project false))

(defn zinc-test-compile "Compiles Java and Scala test source." [project]
  (do-compile project true))

(defn- continuous-compile [project sources f]
  (let [{:keys [interval-in-ms]
         :or   {interval-in-ms 2000}} (:continuous-compile
                                        (:zinc-options project))]
    (loop [tracker (track/tracker)]
      (let [new-tracker (core/scan tracker sources)]
        (main/debug "new-tracker: " new-tracker)
        (try
          (when (not= new-tracker tracker)
            (f project)
            (main/info "compile completed."))

          (Thread/sleep interval-in-ms)
          (catch Exception ex (.getMessage ex)))
        (recur new-tracker)))))

(defn cc
  "Compiles Java and Scala main sources continuously.
   This doesn't compile test source code so you may want run 
   'lein zinc test-cc' task in a separate terminal."
  [project]
  (let [combined-sources (combine-sources project)]
    (main/info "sources:" combined-sources)
    (continuous-compile project combined-sources zinc-compile)))

(defn test-cc
  "Compiles Java and Scala test sources continuously.
  This doesn't compile main source code so you may want run 
  'lein zinc cc' task in a separate terminal."
  [project]
  (let [test-sources (:test-paths project)]
    (main/info "test-sources:" test-sources)
    (continuous-compile project test-sources zinc-test-compile)))

(defn zinc-profile [project]
  "Generates lein project profile that contains the configurations necessary 
  to run lein zinc plugin."
  (let [{:keys [sbt-version scala-version fork-java?]
         :or   {sbt-version   "0.13.9"
                scala-version (lein/dependency-version
                                (lein/dependency project
                                                 'org.scala-lang/scala-library))
                fork-java?    false
                }} project
        lein-zinc-version (lein/dependency-version
                            (lein/plugin project 'lein-zinc/lein-zinc))]
    (main/info "scala version: " scala-version)
    (main/info "sbt   version: " sbt-version)
    (main/info "fork java?     " fork-java?)
    {:dependencies  [['lein-zinc lein-zinc-version]
                     ['org.scala-lang/scala-compiler scala-version]
                     ['org.scala-lang/scala-library scala-version]
                     ['org.scala-lang/scala-reflect scala-version]
                     ['org.scala-sbt/launcher-interface "1.1.3"]
                     ['org.scala-sbt/compiler-interface sbt-version
                      :classifier "sources"]]
     :sbt-version   sbt-version
     :scala-version scala-version
     :fork-java?    fork-java?}))

(defn zinc
  "Compiles Scala and Java code with Typesafe zinc incremental compiler."
  {:subtasks [#'zinc-compile #'zinc-test-compile #'cc #'test-cc]}
  ([project]
   (let [profile (or (:zinc (:profiles project)) (zinc-profile project))
         project (project/merge-profiles project [profile])]
     (deps/deps project) (zinc-compile project) (zinc-test-compile project)))
  ([project subtask & options]
   (let [profile (or (:zinc (:profiles project)) (zinc-profile project))
         project (project/merge-profiles project [profile])]
     (deps/deps project)
     (case subtask
       "zinc-compile" (zinc-compile project)
       "zinc-test-compile" (zinc-test-compile project)
       "cc" (cc project)
       "test-cc" (test-cc project)
       (help/help project "zinc")))))

;; vim: set ts=2 sw=2 cc=80 et: 
