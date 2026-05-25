(ns clj-async-profiler.jfr
  "JFR-based profiling pathway: captures JDK flight recorder events and renders
  them as flamegraph HTML using the same pipeline as clj-async-profiler.core.

    (require '[clj-async-profiler.jfr :as jfr])
    (jfr/profile (my-allocating-fn))"
  (:require [clj-async-profiler.post-processing :as post-proc]
            [clj-async-profiler.render :as render]
            [clj-async-profiler.results :as results])
  (:import (jdk.jfr Recording)
           (jdk.jfr.consumer RecordedEvent RecordedFrame RecordingFile)
           java.util.Date
           java.util.HashMap))

;;;; Frame formatting

(defn- recorded-frame->str
  "Format a JFR RecordedFrame as 'pkg/ClassName.method', matching the
  async-profiler collapsed-stacks format so the existing demunger works
  correctly (Clojure frame detection depends on slash-separated packages)."
  [^RecordedFrame frame]
  (let [method (.getMethod frame)
        ;; JFR gives 'pkg.ClassName'; convert to 'pkg/ClassName' for demunger.
        class-slash (.replace ^String (.. method getType getName) "." "/")
        method-name (.getName method)]
    (str class-slash "." method-name)))

;;;; Event → raw profile

(def object-allocation-sample-config
  "Event config for jdk.ObjectAllocationSample."
  {:event-type  "jdk.ObjectAllocationSample"
   :value-fn    (fn [^RecordedEvent e] (.getLong e "weight"))
   :top-frame-fn (fn [^RecordedEvent e]
                   (some-> (.getValue e "objectClass") .getName))})

(defn jfr-events->raw-profile
  "Convert a seq of RecordedEvent objects to a raw profile HashMap<String,Long>
  compatible with post-processing/raw-profile->dense-profile.

  event-config keys:
    :event-type   - JFR event type name to filter on (nil = include all)
    :value-fn     - (RecordedEvent -> long) sample weight
    :top-frame-fn - (RecordedEvent -> String|nil) synthetic top frame, or nil

  Options:
    :transform - fn applied to the demunged stack string before accumulation"
  [events event-config options]
  (let [{:keys [event-type value-fn top-frame-fn]} event-config
        transform (get options :transform identity)
        acc (HashMap.)]
    (doseq [^RecordedEvent event events]
      (when (or (nil? event-type)
                (= (.getName (.getEventType event)) event-type))
        (let [stack-trace (.getStackTrace event)
              frames (when stack-trace (.getFrames stack-trace))]
          (when (and frames (pos? (.size frames)))
            (let [sb (StringBuilder.)
                  n  (.size frames)]
              ;; JFR frames are top-to-bottom; iterate in reverse for bottom-up order.
              (loop [i (dec n)]
                (when (>= i 0)
                  (when (pos? (.length sb)) (.append sb ";"))
                  (.append sb (recorded-frame->str (.get frames i)))
                  (recur (dec i))))
              ;; Synthetic top frame (e.g. the allocated class).
              (when top-frame-fn
                (when-let [top (top-frame-fn event)]
                  (.append sb ";")
                  (.append sb top)))
              (let [raw-stack   (str sb)
                    demunged    (post-proc/demunge-java-clojure-frames raw-stack)
                    stack       (transform demunged)
                    value       (value-fn event)
                    current     (.getOrDefault acc stack 0)]
                (.put acc stack (+ current ^long value))))))))
    acc))

;;;; Recording lifecycle

(defn record-jfr-events
  "Start a JFR recording capturing event-type, call body-fn, stop, and return
  the list of RecordedEvents. The temp JFR file is deleted after reading."
  [event-type body-fn]
  (let [tmp-file (java.io.File/createTempFile "clj-async-profiler-jfr" ".jfr")
        tmp      (.toPath tmp-file)
        rec      (Recording.)]
    (.. rec (enable ^String event-type) withStackTrace)
    (.start rec)
    (try
      (body-fn)
      (finally
        (.stop rec)
        (.dump rec tmp)
        (.close rec)))
    (try
      (RecordingFile/readAllEvents tmp)
      (finally
        (.delete tmp-file)))))

;;;; Public API

(defn generate-flamegraph
  "Generate a flamegraph HTML file from a seq of JFR RecordedEvent objects.
  Returns the java.io.File where the flamegraph was written.

  Options:
    :event-config - event config map (default: object-allocation-sample-config)
    :transform    - fn applied to each demunged stack string before accumulation
    :title        - flamegraph title
    :config       - flamegraph UI config map (transforms, highlight, etc.)"
  ([events] (generate-flamegraph events {}))
  ([events options]
   (let [event-config (get options :event-config object-allocation-sample-config)
         event-type   (:event-type event-config)
         raw-profile  (jfr-events->raw-profile events event-config options)
         dense        (post-proc/raw-profile->dense-profile raw-profile true)
         now          (Date.)
         run-id       (swap! results/next-run-id inc)
         out-file     (results/results-file now run-id (keyword event-type) "flamegraph" "html")
         options      (update options :title #(or % (.getName ^java.io.File out-file)))]
     (spit out-file (render/render-html-flamegraph dense options false))
     (swap! results/file->metadata assoc out-file {:samples (:total-samples dense)})
     out-file)))

(defmacro profile
  "Profile the execution of body using JFR. If the first argument is a map,
  treat it as options. Returns the flamegraph java.io.File.

  Options:
    :event-config - event config map (default: object-allocation-sample-config)
    :transform    - fn applied to each demunged stack string before accumulation
    :title        - flamegraph title
    :config       - flamegraph UI config map (transforms, highlight, etc.)"
  [options? & body]
  (let [[opts body] (if (map? options?)
                      [options? body]
                      [{} (cons options? body)])]
    `(let [event-config# (get ~opts :event-config object-allocation-sample-config)
           events#       (record-jfr-events (:event-type event-config#)
                                            (fn [] ~@body))]
       (generate-flamegraph events# ~opts))))
