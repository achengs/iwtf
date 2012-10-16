
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
(ns iwtf.test-core
  (:use clojure.test
        iwtf.config
        iwtf.locators
        iwtf.web
        iwtf.web-common
        iwtf.web-element-shopper
        iwtf.web-test)
  (:require [clj-webdriver.core :as wd]))
(use-fixtures :once
              fixture-ns ;; handles run mode
              fixture-new-browser) ;; shared browser

(use-fixtures :each
              fixture-test ;; handles run mode
              fixture-delete-all-cookies)
(deftest google-search
  (lt [_ (go :url "http://google.com")
       _ (enter google-home-txt-search-term "search")
       _ (wait-until-visible? google-home-btn-google-search false 2)]))
