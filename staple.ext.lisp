(asdf:load-system :staple-markless)

(defpackage "vorbis-docs"
  (:use #:cl)
  (:local-nicknames
   (#:vorbis #:org.shirakumo.vorbis)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "vorbis-docs")))

(defmethod staple:page-type ((system (eql (asdf:find-system :cl-vorbis))))
  'page*)
