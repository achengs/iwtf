(ns iwtf.file
  (:use iwtf.config)
  (:require [clojure.string :as s]))

(defn spitt [file content]
  (spit file content :append true))

(defn reader [file] (java.io.BufferedReader. (java.io.FileReader. file)))

(defn backup-original-only
  "this creates a backup if one does not already exist. but if one
  does, then this assumes that it is a copy of the original so it does
  nothing. the backup has the same name with .bak appended. returns
  true if backup needed to be made, false if it existed already."
  [file]
  (let [backup-file (str file ".bak")]
    (if (-> backup-file (.replace "/" "\\\\") java.io.File. .exists)
      false
      (do (spit backup-file (slurp file)) true))))

(defn- replace-all [s p f]
    (s/replace s p f))

(defn- update-helper [file pattern replace-fn]
  (->> (replace-all (slurp file) pattern replace-fn)
       (spit file)))

(defn- node-r [p]
  (re-pattern (format "<%s>[^<]*</%s>" p p)))

(defn- attribute-r [p]
  (re-pattern (format "([^a-zA-Z0-9])(%s=\\\"[^\\\"]*\\\")" p)))

(defn- name-value-r [p]
  (re-pattern (format "\\sname=\\\"%s\\\"\\s+value=\\\"[^\\\"]*\\\"" p)))

(defn- has-at-least-one? [text pattern]
  (let [n (count (re-seq pattern text))
        r (> n 0)
        w (> n 1)]
    (if w (println "replacing more than one instance!" pattern))
    r))

(defn- is? [text parameter regex-fn]
  (has-at-least-one? text (apply regex-fn [parameter])))

(defmulti update
  (fn [parameter file & args]
    (let [content (slurp file)]
      (cond (is? content parameter node-r)       :node
            (is? content parameter attribute-r)  :attribute
            (is? content parameter name-value-r) :name-value
            :else (-> (format "file %s: unsupported syntax or doesn't have 1 %s"
                              file parameter)
                      Exception. throw)))))

(defmethod update :node [parameter file new-value]
  (update-helper
   file
   (node-r parameter)
   (fn [_] (format "<%s>%s</%s>" parameter new-value parameter))))

(defmethod update :attribute [parameter file new-value]
  (update-helper
   file
   (attribute-r parameter)
   (fn [[_ a b]] (format "%s%s=\"%s\"" a parameter new-value))))

(defmethod update :name-value [parameter file new-value]
  (update-helper
   file
   (name-value-r parameter)
   (fn [_] (format " name=\"%s\" value=\"%s\"" parameter new-value))))
;; todo handle case where there are multiple timeout="" and you need
;; to identify the right one