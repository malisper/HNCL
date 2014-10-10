;;;; HTML Utilities.

(in-package :hncl)

(defstruct (color (:conc-name nil)
                  (:constructor color (r g b)))
  (r 0 :type (integer 0 256))
  (g 0 :type (integer 0 256))
  (b 0 :type (integer 0 256)))

(def dehex (str)
  "Parses a hex value in a string."
  (errsafe (parse-integer str :radix 16)))

(defmemo hex->color (str)
  "Convert a string representing a color to the given color."
  (and (is (len str) 6)
       (with (r (dehex (cut str 0 2))
              g (dehex (cut str 2 4))
              b (dehex (cut str 4 6)))
         (and r g b
              (color r g b)))))

(defmemo gray (n)
  "Equivalent to (color n n n)."
  (color n n n))

(defparameter white*    (gray 255)        "The color white.")
(defparameter black*    (gray 0)          "The color black.")
(defparameter linkblue* (color 0 0 190)   "The color of a link.")
(defparameter orange*   (color 255 102 0) "The color orange.")
(defparameter darkblue* (color 0 0 120)   "The color darkblue.")

(defvar hexreps* (table)
  "A table mapping from the color values to a string of the hex
   value.")

(up i 0 256 (= (gethash i hexreps*)
               (let s (tostring (prf "~16R" i))
                 (if (is (len s) 1) (mkstr "0" s) s))))

(defmemo hexrep (col)
  "Lookup the hex string for this color."
  (mkstr (gethash col!r hexreps*)
         (gethash col!g hexreps*)
         (gethash col!b hexreps*)))
