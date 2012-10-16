
;; ================================================================
;; 
;; ATTENTION: DO NOT EDIT THIS FILE DIRECTLY. 
;;
;;            Edit ../../org/iwtf.org instead. The content of this 
;;            source code file is generated from iwtf.org.
;;
;; ================================================================









;; The only reason to look at this generated code directly is if
;; you're curious about the output of the org literate workflow.



































;; You have been warned.
(ns iwtf.web-element-shopper
  (:use clj-webdriver.core
        iwtf.web-common)
  (:require [seesaw.core :as ui]
            [seesaw.mig :as mig]
            [clojure.string :as s]
            [iwtf.file :as f]))
(def supported-attributes [{:key :id       :mnemonic \i}
                           {:key :name     :mnemonic \n}
                           {:key :text     :mnemonic \t}
                           {:key :href     :mnemonic \h}
                           {:key :class    :mnemonic \c}
                           {:key :xpath    :mnemonic \x :customizable true}
                           {:key :style    :mnemonic \s}
                           {:key :value    :mnemonic \v}
                           {:key :visible  :mnemonic \l}
                           {:key :hidden   :mnemonic \d}
                           {:key :tabindex :mnemonic \b}
                           {:key :tag      :mnemonic \g}
                           {:key :type     :mnemonic \y}
                           {:key :onclick  :mnemonic \k}
                           {:key :alt      :mnemonic \a}])
(defn- set-element [e m] (assoc m :elem e))
(defn- apostrophe? [t] (and (not= nil t) (.contains t "'")))
(defmulti  get-label :key)
(defmethod get-label :default [m] (:key m))
(defmethod get-label :text [m]
  (if (apostrophe? (text (:elem m))) :xpath :text))
(defmulti get-value :key)
(defmethod get-value :default [m] (attribute (:elem m) (:key m)))
(defmethod get-value :value [m] (value (:elem m)))
(defmethod get-value :xpath [m] (xpath (:elem m)))
(defmethod get-value :tag [m] (tag (:elem m)))
(defmethod get-value :href [m]
  (let [url (attribute (:elem m) :href)]
    (if url
      (try (.substring url (.indexOf url "/" 8))
           (catch Exception exc url))
      "")))
(defmethod get-value :text [m]
  (let [t (text (:elem m))]
    (if (apostrophe? t)
      (str "//*[text()=concat(\"" t "\",\"\")]")
      t)))
(defn- choice [m]
  (format "%s: %s"
          (.substring (str (get-label m)) 1)
          (get-value m)))
(defn- sufficient? [k v e browser]
  (and (not (nil? v))
       (= e (find-element browser {k v}))))
(defn- make-custom-field [m]
  (ui/text :text (get-value m)))
(defn- set-custom-field [m]
  (if (:customizable m)
    (into m {:custom-field (make-custom-field m)})
    m))
(defn- mig [m]
  (if (:customizable m)
    [[(:checkbox m) "wrap"] [" "] [(:custom-field m) "gapleft20, growx, wrap"] [" "]]
    [[(:checkbox m) "wrap"] [" "]]))
(defmulti validate-custom-value :key)

(defmethod validate-custom-value :default [m]
  {(get-label m) (ui/text (:custom-field m))})
(defn- prompt [a b]
  (-> (ui/dialog
       :content (mig/mig-panel
                 :items [["Which do you want?" "wrap"]
                         [(str "1 -- " a) "wrap"]
                         [(str "2 -- " b) "wrap"]])
       :resizable? false
       :title "Choice"
       :type :question
       :options [(ui/action :name 1
                            :mnemonic \1
                            :handler #(ui/return-from-dialog % a))
                 (ui/action :name 2
                            :mnemonic \2
                            :handler #(ui/return-from-dialog % b))])
      ui/pack! ui/show!))
(defn- get-selection [m]
  (if (ui/selection (:checkbox m))
    (if (:customizable m)
      (validate-custom-value m)
      {(get-label m) (get-value m)})
    nil))
(defn- get-selected-attributes [ms]
  (reduce (partial merge-with prompt)
          (map get-selection ms)))
(defn- get-selected-element [ms browser]
  (if-let [atts (get-selected-attributes ms)]
    (let [finding (find-element browser atts)
          result  (if (no-match finding) nil finding)]
      [result atts])
    [nil nil]))
(defn- test-selection [ms browser]
  (let [[element atts] (get-selected-element ms browser)]
    (if atts
      (if element
        (flash element)
        (ui/alert (str "Your choices do not match any element on this page.\n"
                       "Tested: " atts)))
      (ui/alert "Nothing selected!"))))

(defn- click-selection [ms browser]
  (let [[element atts] (get-selected-element ms browser)]
    (if atts
      (if element
        (click element)
        (ui/alert (str "Your choices do not match any element on this page.\n"
                       "Tested: " atts)))
      (ui/alert "Nothing selected!"))))

(defn- type-in-selection [ms browser]
  (let [[element atts] (get-selected-element ms browser)]
    (if atts
      (if element
        (input-text element "test-string")
        (ui/alert (str "Your choices do not match any element on this page.\n"
                       "Tested: " atts)))
      (ui/alert "Nothing selected!"))))
(defn- make-checkbox [m browser]
  (ui/checkbox :id (get-label m)
               :text (choice m)
               :mnemonic (:mnemonic m)
               :selected? (sufficient? (get-label m) (get-value m) (:elem m) browser)))
(defn- set-checkbox [browser m]
  (assoc m :checkbox (make-checkbox m browser)))
(defn- make-checkboxes [element browser]
  (->> supported-attributes
       (map (partial set-element element))
       (map (partial set-checkbox browser))
       (map set-custom-field)))
(defn- construct-name-helper [t x]
  (if (and (not (empty? x)) (not (empty? t)))
            (str x "-")
            x))

(defn- hyphenate-spaces [s] (.replace (.trim s) " " "-"))

(defn- construct-name [page kind n]
  (let [t (hyphenate-spaces n)
        k0 (hyphenate-spaces kind)
        p0 (hyphenate-spaces page)
        p (construct-name-helper t p0)
        k (construct-name-helper t k0)]
    (str p k t)))
(defn- get-name [event page kind]
  (->> event ui/to-root ui/group-by-id :code ui/text
       (construct-name page kind)))
(defn redefine-locator [name atts]
  (let [locator-ns (find-ns 'iwtf.locators)
        my-symbol (symbol name)
        existing-var (ns-resolve locator-ns my-symbol)]
    (if existing-var
      (alter-var-root existing-var (fn [_] atts))
      (intern locator-ns my-symbol atts))))
(def locator-file "/home/mint/workshop/iwtf/src/iwtf/locators.clj")

(def bak ".bak")

(defn- copy-small-file
  ([src]      (copy-small-file src (str src bak)))
  ([src dst]  (->> (slurp src)
                   (spit  dst))))

(defn restore-locator-file []
  (copy-small-file (str locator-file bak) locator-file))
(defn- update-locator-file [name expr]
  (copy-small-file locator-file)
  (let [original-content (slurp locator-file)
        pattern-str (str "\\(def\\s" name "\\s[^\\)]*\\)")
        pattern (re-pattern pattern-str)
        result (if (re-find pattern original-content)
                 (s/replace-first original-content pattern expr)
                 (str original-content "\n" expr))]
    (spit locator-file result)))
(defn- return-result [edit event ms page kind element browser]
  (let [[finding atts] (get-selected-element ms browser)]
    (if atts
      (let [name (get-name event page kind)
          good (= element finding)
          msgn (if-not name (format "[%s] is not a valid name.\n" name))
          msgl (if-not good (format "%s doesn't identify the right element."))
          expr (format "(def %s %s)" name atts)]
      (if (and name good)
        (do (println expr)
            (if edit
              (do
                (redefine-locator name atts)
                (update-locator-file name expr)))
            (ui/return-from-dialog event expr))
        (ui/alert (str msgn msgl))))
      (ui/alert "Nothing selected!"))))
(defn- make-widgets [initial-name boxes browser element]
  (let [inam (str initial-name), btn ui/action]
    (concat
     [["Name in test code" "align r"]
      [(ui/text :id :code :text inam :enabled? (.equals "" inam))
       "growx, spanx 3, wrap"]
      ["Element" "gaptop 15, align r"]
      [(btn :name "Flash original" :mnemonic \f
            :handler (fn [_] (flash element))) "split 4"]
      [(btn :name "Flash selection" :mnemonic \e
            :handler (fn [_] (test-selection boxes browser)))]
      [(btn :name "Click selection" :mnemonic \k
            :handler (fn [_] (click-selection boxes browser)))]
      [(btn :name "Type in selection" :mnemonic \y
            :handler (fn [_] (type-in-selection boxes browser))) "wrap"]
      ["Attributes" "gaptop 15, align r"]]
     (reduce concat (map mig boxes)))))
(defn- open-dlg

  ([edit browser element page kind]
     (open-dlg edit browser element page kind ""))

  ([edit browser element page kind initial-name]
     ;;(flash element)
     (let [atts (make-checkboxes element browser)
           wids (make-widgets initial-name atts browser element)
           btn  ui/action]
       (-> (ui/dialog
            :content (mig/mig-panel :items wids)
            :resizable? false
            :title "Element Locator Options"
            :options [(btn :name "Use"
                           :mnemonic \u
                           :handler (fn [ev] (return-result edit ev atts page
                                                            kind element
                                                            browser)))
                      (btn :name "Skip"
                           :mnemonic \p
                           :handler #(ui/return-from-dialog % nil))])
           ui/pack! ui/show!))))
(defn get-name-from-catalog-result
  [r]
  (nth (.split r " ") 1))

(defn get-value-from-catalog-result
  [r] 
  (read-string (first (re-find #"(\{[^\}]+\})" r))))

(declare catalog)

(defn make-replacement-locator  
  [browser name query]
  (if-let [result (catalog browser query name)]
    (let [name (get-name-from-catalog-result result)
          valu (get-value-from-catalog-result result)]
      (do (redefine-locator name valu)
          (update-locator-file name result)))))
  (ui/native!)
  
  (defn last-n-chars [n s]
    (if (nil? s)
      ""
      (.substring s (max 0 (- (.length s) n)))))
  
  (defn last-n-segments [n c s]
    (let [segments (.split s c)
          d (max 0 (- (count segments) n))
          remainder (drop d segments)]
      (.substring (reduce str (interleave (repeat n " ") remainder)) 1)))
  
  (defn get-data [element key limit-fn]
    (limit-fn (get-value {:elem element :key key})))
  
  (defn get-info [element]
    [(get-data element :id    (partial last-n-segments 3 "_"))
     (get-data element :value (partial last-n-chars 12))])
  
  (defn- get-entry-label [element]
    (apply (partial format "%s %s") (get-info element)))
  
  (defn- maybe-set-text [field new-value]
    (if new-value (ui/text! field new-value)))
  
  (defn- make-entry
    ([browser page kind element] (make-entry browser page kind "" element))
    ([browser name element] (make-entry browser "" "" name element))
    ([browser page kind name element]
       (let [tx (get-entry-label element)
             bc (ui/action
                 :name "c"
                 :tip (get-value {:elem element :key :text})
                 :handler (fn [_] (trya (click element))))
             tq (ui/text
                 :text ""
                 :enabled? false
                 :disabled-text-color java.awt.Color/black)
             bd (ui/button
                 :text "d"
                 :tip (get-value {:elem element :key :name})
                 :listen
                 [:action (fn [_] (trya (->> (open-dlg nil browser element page kind name)
                                             (maybe-set-text tq))))
                  :mouse-entered (fn [_] (trya (flash element)))])
             bu (ui/action
                 :name "x"
                 :tip "abandon this locator"
                 :handler (fn [_] (ui/text! tq "")))]
         {:text    tq
          :widgets [[bc] [bd] [bu] [tq "w 20"] [tx "wrap"]]})))
  
  (defn- do-entries [event entries need-exactly-one-result]
    (let [results (->> (map :text entries)
                       (map ui/text)
                       (remove #(.equals "" %)))
          n       (count results)]
      (if need-exactly-one-result
        (if (= 1 n)
          (ui/return-from-dialog event (first results))
          (do (ui/alert (str "We need one result. You have " n)) nil))
        (do
          (println "Here are the locator definitions.")
          (->> results (map println) doall)
          (ui/return-from-dialog event :done)))))
  
  (defn check-for-no-match [elements query]
    (if (no-match (first elements))
      (do
        (ui/alert (format "Didn't find any matching elements for %s\n%s"
                          query
                          "Close all other tabs, check query and current page."))
        false)
      true))
  
(defn- catalog-helper
  [browser query make-entries one-only]
  (let [btn ui/action
        elements (find-elements browser query)
        ok (check-for-no-match elements query)
        elements (if ok (filter visible? elements) elements)]
    (if ok
      (let [entries (map make-entries elements)
            widgets (reduce concat (map :widgets entries))]
        (-> (ui/dialog
             :content (ui/scrollable (mig/mig-panel :items widgets))
             :title (format "Elements matching %s" query)
             :options [(btn :name "Use"
                            :mnemonic \u
                            :handler (fn [ev] (trya (do-entries ev entries one-only))))
                       (btn :name "Skip"
                            :mnemonic \p
                            :handler #(ui/return-from-dialog % nil))])
            ui/pack! ui/show!)))))
  (defn catalog
    "the longer argument list with `page' and `kind' is for shopping for
    multiple elements. the results are printed out. returns :done after
    this.
  
    the shorter argument list with `name' is for finding a single
    replacement locator. returns the string that would be upserted in
    locators.clj for redefinition.
  
    returns nil if user decides to skip or if `query' doesn't match any
    elements."
    ([browser query page kind]
       (catalog-helper browser query (partial make-entry browser page kind) false))
    ([browser query name]
       (catalog-helper browser query (partial make-entry browser name) true)))
