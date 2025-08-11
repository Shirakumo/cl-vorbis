(asdf:defsystem cl-vorbis
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Bindings to stb_vorbis, a simple and free OGG/Vorbis decoding library"
  :homepage "https://shirakumo.org/docs/cl-vorbis/"
  :bug-tracker "https://shirakumo.org/project/cl-vorbis/issues"
  :source-control (:git "https://shirakumo.org/project/cl-vorbis.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:cffi
               :trivial-features
               :trivial-garbage
               :static-vectors
               :documentation-utils))
