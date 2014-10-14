(in-package :hncl)

(def br (? (n 1))
  "Prints N break line tags."
  (repeat n (pr "<br>"))
  (prn))

(def br2 ()
  "Prints 2 break line tags."
  (prn "<br><br>"))

(mac center    body `(tag center ,@body))
(mac underline body `(tag u ,@body))
(mac tab       body `(tag (table border 0) ,@body))
(mac tr        body `(tag tr ,@body))

(flet1 pratoms (body)
    (if (or (no body)
            (all [and (consp _) (no (is (car _) 'quote))]
                 body))
        body
        `((pr ,@body)))
  (mac td      body         `(tag td ,@(pratoms body)))
  (mac trtd    body         `(tr (td ,@(pratoms body))))
  (mac tdr     body         `(tag (td align 'right) ,@(pratoms body)))
  (mac tdcolor (col . body) `(tag (td bgcolor ,col) ,@(pratoms body))))

(mac row args
  "Creates a row of a table with the given data."
  `(tr ,@(map [list 'td _] args)))

(mac prrow args
  "Prints a table row. If an argument is a number, it is right 
   aligned."
  (w/uniq g
    `(tr ,@(mapeach a args
             `(let ,g ,a
                (if (numberp ,g)
                    (tdr (pr ,g))
                    (td  (pr ,g))))))))

(mac prbold body
  "Prints the body and surrounds it with bold tags."
  `(tag b (pr ,@body)))

(def para args
  "Prints a paragraph tag followed by the rest of the args."
  (gentag p)
  (when args (apply #'pr args)))

(def menu (name items ? sel)
  "Prints a menu with the given name and items."
  (tag (select name name)
    (each i items
      (tag (option selected (is i sel))
        (pr i)))))

(def whitepage body
  "Creates a basic white page."
  `(tag html
     (tag (body bgcolor white alink linkblue)
       ,@body)))
