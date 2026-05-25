(ns clj-async-profiler.jfr-test
  (:require [clj-async-profiler.jfr :as sut]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]])
  (:import (jdk.jfr.consumer RecordingFile)
           java.nio.file.Paths))

(defn- read-fixture []
  (let [path (Paths/get "test/clj_async_profiler/jfr_fixture.jfr" (into-array String []))]
    (RecordingFile/readAllEvents path)))

(defn- alloc-events [events]
  (filter #(= (.getName (.getEventType %)) "jdk.ObjectAllocationSample") events))

(deftest jfr-events->raw-profile-test
  (let [events  (alloc-events (read-fixture))
        raw     (sut/jfr-events->raw-profile events sut/object-allocation-sample-config {})
        top-classes (into #{} (map #(last (str/split (key %) #";")) (take 20 (sort-by val > raw))))]
    (is (pos? (count raw)) "raw profile should be non-empty")
    (is (every? string? (keys raw)) "keys should be stack strings")
    (is (every? number? (vals raw)) "values should be numeric weights")
    ;; The maps+strings workload should produce these allocations.
    (is (contains? top-classes "clojure.lang.PersistentVector"))
    (is (contains? top-classes "clojure.lang.PersistentHashMap$BitmapIndexedNode"))))

(deftest jfr-events->raw-profile-clojure-frames-test
  (let [events (alloc-events (read-fixture))
        raw    (sut/jfr-events->raw-profile events sut/object-allocation-sample-config {})]
    (is (every? #(not (str/includes? % "/")) (map #(first (str/split % #";")) (keys raw)))
        "bottom frames should use dot-separated package names after demunging")
    (is (some #(str/includes? % "clojure.core/") (keys raw))
        "Clojure frames should be demunged to ns/fn form")))

(deftest generate-flamegraph-test
  (let [events   (alloc-events (read-fixture))
        fg-file  (sut/generate-flamegraph events {:title "fixture test"})]
    (is (.exists fg-file))
    (is (> (.length fg-file) 10000))
    (is (str/includes? (slurp fg-file) "fixture test"))))

(deftest profile-integration-test
  (let [fg-file (sut/profile
                 (dotimes [_ 10000]
                   (into {} (map (fn [i] [i (str i)]) (range 20)))))]
    (is (.exists fg-file))
    (is (> (.length fg-file) 10000))))
