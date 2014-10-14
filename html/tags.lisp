(in-package :hncl)

(mac gentag args
  "Generates a single tag."
  (start-tag args))

(mac tag (spec . body)
  "Prints a tag generated from SPEC, surrounding what is printed by
   BODY."
  `(do ,(start-tag spec)
       ,@body
       ,(end-tag spec)))

(mac tag-if (test spec . body)
  "If TEST returns true, this is equivalent to tag. Otherwise this
   just evaluates BODY."
  `(if ,test
       (tag ,spec ,@body)
       (do ,@body)))

(def start-tag (spec)
  "Generates code that prints the starting tag based on SPEC."
  (if (atom spec)
      `(pr ,(downcase (mkstr "<" spec ">")))
      (let opts (tag-options (car spec) (group (cdr spec)))
        (if (all #'stringp opts)
            `(pr ,(mkstr "<" (downcase (car spec)) (apply #'mkstr opts) ">"))
            `(do (pr (downcase ,(mkstr "<" (car spec))))
                 ;; Iterate through the options. If an option is a
                 ;; string, print it, otherwise evaluate it as code.
                 ,@(map (fn (opt)
                          (if (stringp opt)
                              `(pr ,opt)
                              opt))
                        opts)
               (pr ">"))))))

(def end-tag (spec)
  "Generates code that prints the ending tag based on SPEC."
 `(pr ,(downcase (mkstr "</" (carif spec) ">"))))

(def literal (x)
  "Is this a literal object?"
  ;; A literal object is an object whose value is known at
  ;; compile-time.
  (typecase x
    symbol (in x nil t)
    cons   (caris x 'quote)
    t      t))

(def tag-options (spec options)
  "Generate code for printing a tag with spec SPEC and options 
   OPTIONS."
  (if (no options)
      '()
      (let ((opt val) . rest) options
        (iflet meth (if (is opt 'style) #'opstring (opmeth spec opt))
          (if val
              (cons (if (precomputable-tagopt val)
                        (tostring (eval (call meth opt val)))
                        (call meth opt val))
                    (tag-options spec rest))
              (tag-options spec rest))
          (do
            (pr "<!-- ignoring " opt " for " spec "-->")
            (tag-options spec rest))))))

(def precomputable-tagopt (val)
  "Is this tag option computable at compile time?"
  (and (literal val)
       (no (and (typep val 'string) (find #\@ val)))))
