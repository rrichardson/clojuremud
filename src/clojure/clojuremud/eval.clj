(ns clojuremud.eval
  (:require [clojail.testers :refer [secure-tester blanket]]
            [clojail.core :refer [sandbox]]
            [clojure.stacktrace :refer [root-cause]])
  (:import java.io.StringWriter
           java.util.concurrent.TimeoutException))

;; Brazenly stolen and adapted from http://github.com/Raynes/tryclojure

(defn eval-form [form sbox]
  (with-open [out (StringWriter.)]
    (let [result (sbox form {#'*out* out})]
      {:expr form
       :result [out result]})))

(defn eval-string [expr sbox]
  (let [form (binding [*read-eval* false] (read-string expr))]
    (eval-form form sbox)))

(def clojuremud-tester
  (conj secure-tester-without-def (blanket "clojuremud")))

(defn make-sandbox [name]
  (sandbox clojuremud-tester
           :timeout 2000
           :namespace name
           :init '(do (require '[clojure.repl :refer [doc source]]))))

(defn run-in [sandbox expr]
  (try
    (eval-string expr sandbox)
    (catch TimeoutException _
      {:error true :message "Execution Timed Out!"})
    (catch Exception e
      {:error true :message (str (root-cause e))})))
