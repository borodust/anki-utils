(asdf:defsystem :anki-utils
  :description "Various Anki helpers"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (:alexandria :babel :sqlite :cl-json :zip :sha1 :local-time :split-sequence :parse-number)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "anki")
               (:file "kanji-from-vocab")))
