(ns chefkoch-complete.chefkoch
  (:require [clojure.string :as str]
            [markov-chains.core :as m]
            [clojure.java.jdbc :refer :all]
            [net.cgrand.enlive-html :as html]
            [libpython-clj.require :refer [require-python]]
            [libpython-clj.python :as py :refer [py. py.. py.-]]
            [clojure.java.io :as io])
  (:gen-class))

(def test-url "https://www.chefkoch.de/rezepte/2653511416758959/Pikanter-Dattel-Frischkaese-Dip.html")

(require-python '[nltk :as nltk])
#_(nltk/download "book")
(require-python '[nltk.book :as book])
(require-python '[nltk.tokenize :as tokenize])


(def greetings #{"LG" "Liebe Grüße" "Gruß" "Lieben Gruß" "MfG" "MFG" "mfg" "lg" "Lg" "vg" "glg" "GLG" "VG" "Viele Grüße" "Grüße"})

(defn starts-with-greeting? [s]
  (some #{true}
        (map #(str/starts-with? s %)
             greetings)))

(def db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "/home/jon/src/clj-bots/database.db"})

(defn fetch-db-comments [db]
  (query db ["select text from comments"] {:row-fn :text}))

(defn collate-all-comments [db depth]
  (let [comments (fetch-db-comments db)]
    (->> (m/collate (clojure.string/split (clojure.string/join " " comments) #"\s+") depth)
         ;; TODO: passt da shier?
         #_(remove (fn [x] (some #{"LG" "lg" "Lg" "VG"} [x]))))))

(def collate-all-mem
  (memoize collate-all-comments))

(defn comments-with-sub-string [db substring]
  (query db ["select text from generated_comments where instr(lower(text),lower(?)) != 0" substring] ))

(defn original-comments-with-sub-string [db substring]
  (query db ["select text from comments where instr(lower(text),lower(?)) != 0" substring] ))

(defn greeting-in-middle? [comment]
  ;; todo: make sure only one greeting
  (let [strings (str/split comment #" ")
        strings (->> strings
                     (map #(str/replace % #"," ""))
                     (map #(str/replace % #"!" ""))
                     (map #(str/replace % #"\." ""))
                     (map #(str/replace % #"\?" "")))
        length (count strings)
        ;; Komma und so am Ende der Strings weg
        contained-greeting (some greetings strings)
        nr-of-greetings (count (filter #(some greetings [%]) strings))]
    (or (not (<= nr-of-greetings 1))
     (and contained-greeting
          ;; the grreting should be next to last
          (not= contained-greeting
                (nth strings (- length 2)))))))

(defn hallo-in-middle? [comment]
  (let [strings (str/split comment #" ")
        strings (->> strings
                     (map #(str/replace % #"," ""))
                     (map #(str/replace % #"!" ""))
                     (map #(str/replace % #"\." ""))
                     (map #(str/replace % #"\?" "")))
        hallo? (some #(str/starts-with? % "hallo" ) (map str/lower-case strings))
        hi? (or (some #(str/starts-with? % "hi" ) (map str/lower-case strings)))
        length (count strings)]
    (and (or
          hallo?
          hi?)
         (not= (str/lower-case (nth strings 0)) "hallo"))))

(defn invalid-comment? [comment]
  (or (greeting-in-middle? comment)
      (hallo-in-middle? comment)
      (> (count comment) 280)))

;; tokenize splits at these strings, we undo that manually
(def non-splitting-periods
  #{"l.g." "etc." "v.g." "u.a."  "v.a." "z.b." " z." "ca." "ml." "dl." " l." "usw." " v."})

(defn ends-with-ndp? [comment]
  (let [comment (str/lower-case comment)]
    (some true? (map #(str/ends-with? comment %) non-splitting-periods))))

(defn unsplit-comments [comments]
  (loop [comments comments
         result []]
    (if (empty? comments)
      result
      (if (ends-with-ndp? (first comments))
        (recur (drop 2 comments)
               (conj result (str (first comments) " " (second comments))))
        (recur (rest comments)
               (conj result (first comments)))))))

(defn generate-comments [db depth count]
  (remove starts-with-greeting?
          (remove invalid-comment?
                  (unsplit-comments
                   (butlast(rest (tokenize/sent_tokenize
                                  (str/join " " (take count (m/generate (collate-all-mem db depth)))))))))))

(defn store-generated-comments [db depth nr]
  (let [comments (generate-comments db depth nr)
        original-comments-count (count (fetch-db-comments db))]
    (doseq [c (distinct comments)]
      (prn c)
      (let [id (java.util.UUID/randomUUID)]
        (insert! db :generated_comments {:text c :depth depth :uid id
                                         :original_comments_count original-comments-count})))))

(defn fetch-instructions [db]
  (query db ["select instruction from instructions"] {:row-fn :instruction}))

(defn collate-all-instructions [db depth]
  (let [instructions (fetch-instructions db)]
    (m/collate (clojure.string/split (clojure.string/join " " instructions) #"\s+") depth)))

(def all-instructions
  (memoize collate-all-instructions))

(defn generate-instruction []
  (second (remove #(str/ends-with? % "ca.")
                  (tokenize/sent_tokenize
                   (str/join " "
                             (take 150
                                   (m/generate (all-instructions db 3))))))))

(defn generate-instructions [count]
  (str/join " " (repeatedly count generate-instruction)))

(defn fetch-ingredients [db]
  (query db ["select ingredients from ingredients order by random() "] {:row-fn :ingredients}))

(defn collate-all-ingredients [db depth]
  (let [ingredients (fetch-ingredients db)]
    (m/collate (clojure.string/split (clojure.string/join " " ingredients) #"\s+") depth)))

;; TODO: das hier funktioniert ja anders, nicht auf satxebene
#_(defn generate-ingredients [db depth count]
  (take count
        (m/generate (collate-all-ingredients db depth))))

(defn generate-ingredients [db count]
  (str/join " " (remove #(str/ends-with? % "ca")
                        (query db ["select ingredients from ingredients order by random() limit ?" count] {:row-fn :ingredients}))))

(def stops
  #{"und" "in" "mit" "auf" "mal" "an" "für" "oder" "à" "la" "aux" "nach"})

(defn generate-title [db depth l]
  ;; TODO: kein "und", "in", "mit", ... am Anfang oder am Ende

  (let [titles (query db ["select title from pages "] {:row-fn :title})
        titles (map first (map #(str/split % #"von") titles))
        title (take l (remove #(= "-" %)
                              (remove #(= "|" %)
                                      (remove #(= "Chefkoch" %)
                                              (str/split  (str/join " " (take 25 (m/generate (m/collate (clojure.string/split (clojure.string/join " " titles) #"\s+") depth)))) #"\s+")))))
        _ (prn title)
        title (if (some stops [(first title)])
                (rest title)
                title)
        title (if (some stops [(last title)])
                (butlast title)
                title)]
    (str/join " " title)))

(defn generate-recips [db]
  (str (generate-title db 2 4)
       "\n"
       "Zutaten: "
       (generate-ingredients db  4)
       "\n"
       "Vorgehen: "
       (generate-instructions 3)))
