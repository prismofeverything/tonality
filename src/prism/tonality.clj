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
      frequency)))