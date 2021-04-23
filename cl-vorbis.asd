#|
 This file is a part of cl-vorbis
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-vorbis
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bindings to stb_vorbis, a simple and free OGG/Vorbis decoding library"
  :homepage "https://Shirakumo.github.io/cl-vorbis/"
  :bug-tracker "https://github.com/Shirakumo/cl-vorbis/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-vorbis.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:cffi
               :trivial-features
               :trivial-garbage
               :documentation-utils))
