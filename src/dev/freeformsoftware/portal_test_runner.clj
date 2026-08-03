(ns dev.freeformsoftware.portal-test-runner
  (:require
   [clojure.string :as str]
   [clojure.test :as t]
   [fulcro-spec.hooks :as hooks]
   [portal.api :as portal])
  (:import (java.sql Timestamp)
           (java.util Date)))

(defonce portal-viewed-atom (atom []))

(defonce test-results-atom (atom []))

(defonce portal-instance (atom nil))

(defonce last-test-ns (atom nil))

(defonce focus-symbol (atom nil))

(def red "#bf616a")
(def green "#a3be8c")
(def neutral "#2d3746")

(defn with-link
  [{:keys [line file name ns column no-name?]}]
  [:span {:style {:float "right"}}
   [:portal.viewer/source-location
    {:label  (if no-name? "" (str name))
     :file   file
     :line   line
     :column column}]])

(defn depth-container
  [depth element]
  [:div {:style {:margin-left (str (+ 7 (* 20 depth)) "px")}} element])

(defn colored-text
  [green? text]
  [:span {:style {:color (if green? green red)}} text])

(defn scan-forward-failures
  "See if any of the remaining elements are a failure that are deeper than the current element. Returns true if a failure is present."
  [es]
  (loop [[element & es] es
         depth          0
         has-been-pos?  false]
    (let [new-depth
          (case (:type element)
            (:begin-behavior :begin-specification :begin-test-var) (inc depth)
            (:end-behavior :end-specification :end-test-var)       (dec depth)
            depth)]
      (cond
        (or (nil? element)
            (and (neg? depth) has-been-pos?))
        false

        (contains? #{:fail :error} (:type element))
        true

        :else
        (recur es
               new-depth
               (or has-been-pos? (> new-depth depth)))))))

(defn completed-reporting?
  [test-results]
  (= (:type (last test-results)) :summary))

(defn any-failed?
  [test-results]
  (some (comp (partial contains? #{:fail :error}) :type) test-results))

(defn failing-report?
  [{:keys [type]}]
  (contains? #{:fail :error} type))

(defn normalize-options
  [options]
  (let [unqualified->qualified {:render-pass-tests?     ::render-pass-tests?
                                :hide-pass-if-any-fail? ::hide-pass-if-any-fail?
                                :include-result?        ::include-result?}]
    (reduce-kv (fn [opts k qualified-k]
                 (if (contains? opts k)
                   (-> opts
                       (assoc qualified-k (get opts k))
                       (dissoc k))
                   opts))
               options
               unqualified->qualified)))

(defn current-ns-object
  [ns]
  (let [ns-sym (if (instance? clojure.lang.Namespace ns)
                 (ns-name ns)
                 ns)]
    (or (find-ns ns-sym)
        (the-ns ns-sym))))

(defn test-ns-for
  [ns]
  (current-ns-object
   (if (and (not (re-find #"test" (str ns))) @last-test-ns)
     @last-test-ns
     ns)))

(defn focus-form-for
  [test-ns]
  (when-let [sym @focus-symbol]
    (when (= (namespace sym) (str (ns-name test-ns)))
      sym)))

(defn make-summary
  [test-results]
  (let [existing-summary (first (filter #(= (:type %) :summary) test-results))
        manually-counted-summary
        (reduce (fn [acc {:keys [type]}]
                  (update acc type (fnil inc 0)))
                {:fail  0
                 :pass  0
                 :error 0
                 :test  0}
                test-results)]
    (merge manually-counted-summary (select-keys existing-summary [:test]))))

(defn ns-short-name
  [ns]
  (->> (str/split (str ns) #"\.")
       (reverse)
       (take 2)
       (reverse)
       (str/join ".")
       (str "...")))

(defn format-summary
  [test-results hide-pass-if-any-fail?]
  (let [meta (first test-results)
        {:keys [test pass fail error] :as summary} (make-summary test-results)]
    [:div
     [:div
      {:style {:border-color
               (cond
                 (or (pos? fail) (pos? error))             red
                 (not (completed-reporting? test-results)) neutral
                 (or (zero? test) (zero? pass))            red
                 :else                                     green)
               :flex-wrap :wrap

               :border-width "8px"
               :border-style :solid

               :padding "15px"
               :display "flex"
               :gap "12px"}}
      (colored-text (pos? test) (if (pos? test) (str test " tests") "No tests found!"))
      (colored-text (zero? error) (str error " error(s)"))
      (colored-text (zero? fail) (str fail " failure(s)"))
      (colored-text (pos? pass) (str pass " assertions(s) passed"))
      (when hide-pass-if-any-fail? (colored-text false "Some tests failed, hiding passed tests"))
      [:span {:style {:color "#2576f1"}} (ns-short-name (:ns meta))]
      [:portal.viewer/relative-time (:run-at meta)]]]))

(defn format-test-results
  [test-results]
  (let [{::keys [render-pass-tests?
                 hide-pass-if-any-fail?]
         :or    {render-pass-tests? true}}
        (first test-results)
        hide-pass-if-any-fail? (and hide-pass-if-any-fail? (any-failed? test-results))]
    (loop [[element & es]      (drop 1 test-results)
           depth               0
           formatted-hiccup    [:<> (format-summary test-results hide-pass-if-any-fail?)]
           context-source-info nil]
      (if (nil? element)
        (with-meta formatted-hiccup {:portal.viewer/default :portal.viewer/hiccup})
        (let [{:keys [type string var ns form-meta]} element
              next-is-begin-spec?                    (= :begin-specification (:type (first es)))

              depth                                  (max 0
                                                          (case type
                                                            (:begin-behavior :begin-specification) (inc depth)
                                                            (:end-behavior :end-specification :end-test-var) (dec depth)
                                                            :begin-test-var (if next-is-begin-spec? depth (inc depth))
                                                            depth))

              context-source-info                    (or (when (= type :begin-test-var) (meta var)) context-source-info)
              any-failures?                          (scan-forward-failures es)
              green?                                 (not any-failures?)

              show-element?                          (or (not hide-pass-if-any-fail?)
                                                         any-failures?
                                                         (failing-report? element))
              new-element                            (when show-element?
                                                       (case type
                                                         :begin-test-var      (if next-is-begin-spec?
                                                                                nil
                                                                                [:<>
                                                                                 (colored-text green?
                                                                                               (:name (meta var)))
                                                                                 (with-link context-source-info)])
                                                         :begin-specification [:<> (colored-text green? string)
                                                                               (with-link (merge context-source-info
                                                                                                 form-meta))]
                                                         :begin-behavior      [:<> (colored-text green? string)
                                                                               (some->> form-meta
                                                                                        (merge context-source-info
                                                                                               {:no-name? true})
                                                                                        (with-link))]
                                                         (:fail :error)       [:portal.viewer/test-report element]
                                                         :pass                (when render-pass-tests?
                                                                                [:portal.viewer/test-report element])
                                                         nil))
              new-element                            (some->> new-element
                                                              (depth-container (dec depth)))]

          (recur es
                 depth
                 (cond-> formatted-hiccup
                   new-element (conj new-element))
                 context-source-info))))))

(defn report-event
  [report event]
  (let [contexts           (seq t/*testing-contexts*)
        vars               (seq t/*testing-vars*)
        event-with-context (cond-> event
                             contexts (assoc :testing-contexts (vec contexts))
                             vars     (assoc :testing-vars vars))]
    (swap! report conj event-with-context)))

(defn clojure-test-event?
  [{:keys [type]}]
  (contains? #{:begin-test-ns
               :end-test-ns
               :begin-test-var
               :end-test-var
               :pass
               :fail
               :error
               :summary}
             type))

(defn run-focused-test-var
  [test-var]
  (binding [t/*report-counters* (ref t/*initial-report-counters*)]
    (t/test-var test-var)
    (let [summary (assoc @t/*report-counters* :type :summary)]
      (t/do-report summary)
      summary)))

(defn run-test
  ([ns]
   (run-test {} ns))
  ([raw-options ns]
   (let [options         (normalize-options raw-options)
         test-ns         (test-ns-for ns)
         focus-form      (focus-form-for test-ns)
         report          (atom [(merge options
                                       {:requested-ns ns
                                        :ns           test-ns
                                        :focus-form   focus-form
                                        :mode         (if focus-form :focused :namespace)
                                        :run-at       (str (Timestamp. (.getTime (Date.))))})])
         original-report t/report]
     (reset! last-test-ns test-ns)

     (add-watch report
                ::view-updater
                (fn [_ _ _ new-value]
                  (reset! test-results-atom new-value)
                  (reset! portal-viewed-atom (format-test-results new-value))))

     (when-not @portal-instance
       (reset! portal-instance (portal/inspect portal-viewed-atom
                                               (merge {:window-title "Portal Test Runner"}
                                                      raw-options))))

     (with-redefs [t/report (fn [event]
                              (when (clojure-test-event? event)
                                (original-report event))
                              (report-event report event))]
       (swap! hooks/hooks assoc
         :on-enter
         (fn [{:fulcro-spec.core/keys [location]}]
           (swap! report (fn [rpts]
                           (conj (vec (butlast rpts))
                                 (assoc (last rpts) :form-meta location))))))
       (let [result (if focus-form
                      (do
                        (println "===== Running focused tests for" focus-form "======")
                        (run-focused-test-var (resolve focus-form)))
                      (do
                        (println "===== Running all ns tests for" (ns-name test-ns) "======")
                        (t/run-tests test-ns)))]
         (if (::include-result? options)
           @test-results-atom
           result))))))

(defn focus-on-form
  [form-sym]
  (let [form-var (or (ns-resolve *ns* form-sym)
                     (throw (ex-info "Could not resolve focused test form"
                                     {:form form-sym
                                      :ns   (ns-name *ns*)})))
        fqn      (symbol (str (ns-name (:ns (meta form-var))))
                         (str (:name (meta form-var))))]
    (swap! focus-symbol (fn [s] (when-not s fqn))))
  [:current-focus-is @focus-symbol])

(comment
  (make-summary @rpt)
  (reset! portal-instance nil)
  (reset! fulcro-spec.hooks/hooks {})
  (run-test 'com.tybaenergy.ui.expandable-columns-spec))
