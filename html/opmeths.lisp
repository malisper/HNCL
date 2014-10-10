(in-package :hncl)

(defvar opmeths* (table)
  "A table mapping from a list containing a tag an attribute to a
   procedure that formats the attribute.")

(def opmeth args
  "Lookup the arguments in opmeths*."
  opmeths*.args)

(def (setf opmeth) (val . args)
  "Sets the operating procedure used for ARGS in opmeths*."
  (= opmeths*.args val))

(mac attribute (tag opt f)
  "Assign the procedure named by F to be mapped from TAG and OPT in
   opmeths*."
  `(= (opmeth ',tag ',opt) ',f))

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

(def opcolor (key val)
  "Generates code to format the attribute KEY with the value VAL as
   a color."
  (w/uniq gv
    `(whenlet ,gv ,val
       (pr ,(mkstr " " key "=#") (hexrep ,gv)))))

(def opstring (key val)
  "Generates code to format the attribute KEY with the value VAL as
   a string."
  `(awhen ,val
     (pr ,(mkstr " " key "=\"") it #\")))

(def opnum (key val)
  "Generates code to format the attribute KEY with the value VAL as
   a number."
  `(awhen ,val
     (pr ,(mkstr " " key "=") it)))

(def opsym (key val)
  "Generates code to set the attribute KEY to value VAL."
  `(pr ,(mkstr " " key "=") ,val))

(def opsel (key val)
  (declare (ignore key))
  "Generates code which sets the attribute KEY to the string 
   'selected' if VAL is non-nil."
  `(when ,val (pr " seclected")))

(def opcheck (key val)
  (declare (ignore key))
  "Generates code which sets the attribute KEY to the string
   'checked' if VAL is non-nil."
  `(when ,val (pr " selected")))

(def opsec (key val)
  "Generates code which sets the attribute KEY to a string
   that contains special symbols which need to be replaced
   (ie '<', '>', '&', etc)"
  `(awhen ,val
     (pr ,(mkstr " " key "=\""))
     (if (isa it 'string) (pr-escaped it) (pr it))
     (pr #\")))

(def pr-escaped (x)
  "Print a string with the codes for characters that can not be
   included, (ie '&#60' instead of '<')."
  (each c x
    (pr (case c #\<  "&#60"
                #\>  "&#62"
                #\"  "&#34"
                #\&  "&#38"
                :else c))))
