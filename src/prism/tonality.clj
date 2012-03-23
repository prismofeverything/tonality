(ns prism.tonality)

;; scales are sequences of floating point numbers which represent the ratios from unity

(defn otonality
  "Octave scales relative to the given overtone"
  [n]
  (take n (map #(/ % n) (iterate inc n))))

(defn utonality
  "Inverse otonalities"
  [n]
  (let [inverse (* 2 n)]
    (take n (map #(/ inverse %) (iterate dec inverse)))))

(defn equal-temperament
  "Infernal creations of man"
  [n]
  (let [step (Math/pow 2 (/ 1 n))]
    (take n (iterate #(* step %) 1.0))))

(def pure-twelve
  [1/1 16/15 9/8 6/5 5/4 4/3 11/8 3/2 8/5 5/3 16/9 15/8])

(def dark-twelve
  [1/1 17/16 10/9 7/6 11/9 9/7 13/9 3/2 11/7 13/8 7/4 13/7])

(def pure-nineteen
  [1/1 26/25 14/13 10/9 8/7 6/5 5/4 9/7 4/3 7/5 10/7 3/2 14/9 8/5 5/3 7/4 9/5 13/7 25/13])

(defn- find-octave
  [note order]
  (let [level (quot note order)
        extrusion (rem note order)]
    (if (> 0 extrusion)
      (dec level) level)))

(defn tonality
  "Generates a function which translates between discrete notes and specific
  frequencies as specified by the scale.  The fundamental is the frequency at the root tone.
  The root is the discrete note this tonality starts from."
  [scale fundamental root]
  (fn [note]
    (let [order (count scale)
          relative (- note root)
          octave (find-octave relative order)
          tone (mod relative order)
          ratio (nth scale tone)
          power (* fundamental (Math/pow 2 octave))
          frequency (* power ratio)]
      (println frequency)
      frequency)))

;; Nineteen tone equal temperament -------------
;;   =~ (1/1 26/25 14/13 10/9 8/7 6/5 5/4 9/7 4/3 7/5 10/7 3/2 14/9 8/5 5/3 7/4 9/5 13/7 25/13)
(def nineteen
  (tonality (equal-temperament 19) 200.0 48))


