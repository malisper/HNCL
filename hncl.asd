(in-package :asdf-user)

(defsystem "hncl"
  :description "Hacker News in Common Lisp"
  :version "0.1"
  :author "malisper"
  :depends-on ("clamp" "clamp-experimental")
  :components ((:file "package")
               (:file "html" :depends-on ("package"))))
