(ns prop-testing-spec-test-check.core-test
  ^{:author "Leeor Engel"}
  (:require [prop-testing-spec-test-check.core :refer :all]
            [prop-testing-spec-test-check.core :as core]
            [clojure.test :refer :all]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as tcg]
            [clojure.test.check.clojure-test :refer :all]
            [com.gfredericks.test.chuck.properties :as tcp]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as stest]))

(defn notes-gen [size] (gen/vector (s/gen ::core/note) size))
(defn rests-gen [size] (gen/vector (s/gen ::core/rest) size))

(defn notes-and-rests-gen [size num-notes]
  (gen/bind (notes-gen num-notes) (fn [v]
                                    (let [remaining (- size num-notes)]
                                      (if (zero? remaining)
                                        (gen/return v)
                                        (gen/fmap (fn [rests] (shuffle (into v rests))) (rests-gen remaining)))))))

(defn melody-gen [size num-notes]
  (s/gen ::core/melody {::core/notes #(notes-and-rests-gen size num-notes)}))

;;
;; test.check version
;;

(defspec with-new-notes-test-check 1000
         (let [test-gens (tcg/let [num-notes tcg/s-pos-int
                                   melody-num-rests tcg/s-pos-int
                                   total-melody-num-notes (tcg/return (+ num-notes melody-num-rests))
                                   melody (melody-gen total-melody-num-notes num-notes)
                                   new-notes (notes-gen num-notes)]
                                  [melody new-notes])]
           (prop/for-all [[melody new-notes] test-gens]
                         (let [new-melody (with-new-notes melody new-notes)]
                           (and (= (count new-notes) (note-count (:notes new-melody)))
                                (= new-notes (remove rest? (:notes new-melody)))
                                (= (count (:notes melody)) (count (:notes new-melody))))))))
;;
;; test.chuck version
;;

(defspec with-new-notes-test-chuck 1000
         (tcp/for-all [num-notes tcg/s-pos-int
                       melody-num-rests tcg/s-pos-int
                       total-melody-num-notes (gen/return (+ num-notes melody-num-rests))
                       melody (melody-gen total-melody-num-notes num-notes)
                       notes (notes-gen num-notes)]
                      (let [new-melody (with-new-notes melody notes)]
                        (and (= (count notes) (note-count (:notes new-melody)))
                             (= notes (remove rest? (:notes new-melody)))
                             (= (count (:notes melody)) (count (:notes new-melody)))))))
;;
;; clojure.spec/test version
;;

(stest/check `with-new-notes {:gen                          {::core/melody     #(melody-gen 5 4)
                                                             ::core/notes-only #(notes-gen 4)}
                              :clojure.spec.test.check/opts {:num-tests 100}})
