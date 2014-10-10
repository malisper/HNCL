(in-package :asdf-user)

(defsystem "hncl"
  :description "Hacker News in Common Lisp"
  :version "0.1"
  :author "malisper"
  :depends-on ("clamp" "clamp-experimental")
  :serial t
  :components ((:file "package")
               (:module "html"
                        :serial t
                        :components ((:file "colors")
                                     (:file "opmeths")
                                     (:file "attributes")))))
