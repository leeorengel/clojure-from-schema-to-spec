(ns prop-testing-spec-test-check.core
  ^{:author "Leeor Engel"}
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]))

(def REST-NOTE-NUMBER -1)
(def MIN-NOTE 0)
(def MAX-NOTE 127)

(s/def ::rest (s/spec #(= % REST-NOTE-NUMBER)
                      :gen #(gen/return REST-NOTE-NUMBER)))

(s/def ::note (s/int-in MIN-NOTE (inc MAX-NOTE)))
(s/def ::note-or-rest (s/or :note ::note :rest ::rest))

(s/def ::notes (s/coll-of ::note-or-rest :kind vector? :min-count 1))
(defrecord Melody [notes])

(s/def ::melody (s/keys :req-un [::notes]))

(defn rest? [n] (neg? n))
(s/fdef rest?
        :args (s/cat :n ::note-or-rest)
        :ret boolean?)

(defn note-count [notes] (count (remove rest? notes)))
(s/fdef note-count
        :args (s/cat :notes ::notes)
        :ret integer?
        :fn #(<= (:ret %) (-> % :args :notes count)))

(defn with-new-notes [melody new-notes]
  (let [notes (first (reduce (fn [[updated-notes new-notes] note]
                               (if (rest? note)
                                 [(conj updated-notes note) new-notes]
                                 [(conj updated-notes (first new-notes)) (rest new-notes)]))
                             [[] new-notes] (:notes melody)))]
    (->Melody notes)))

(s/def ::notes-only (s/coll-of ::note :kind vector? :min-count 1))

(defn- de-structure-melody [m]
  (->Melody (reduce (fn [notes [_ n]]
                      (conj notes n))
                    [] (:notes m))))

(defn- note-counts-match? [melody notes]
  (= (count notes) (note-count (:notes melody))))

(defn- notes-match? [melody notes]
  (= notes (remove rest? (:notes melody))))

(s/fdef with-new-notes
        :args (s/cat :melody ::melody
                     :new-notes ::notes-only)
        :ret ::melody
        ;; I think s/unform should work for :ret values but it doesnt? bug?
        :fn (s/and #(note-counts-match? (de-structure-melody (:ret %)) (:new-notes (:args %)))
                   #(notes-match? (de-structure-melody (:ret %)) (:new-notes (:args %)))
                   #(= (count (:notes (:melody (:args %))))
                       (count (:notes (de-structure-melody (:ret %)))))))




