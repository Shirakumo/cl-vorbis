#|
 This file is a part of cl-vorbis
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.vorbis.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(cffi:define-foreign-library libvorbis
  (:darwin (:or #+X86 "libvorbis-mac-i686.dylib"
                #+X86-64 "libvorbis-mac-amd64.dylib"))
  (:unix (:or #+X86 "libvorbis-lin-i686.so"
              #+X86-64 "libvorbis-lin-amd64.so"))
  (:windows (:or #+X86 "libvorbis-win-i686.dll"
                 #+X86-64 "libvorbis-win-amd64.dll")))

(cffi:defcenum error
  (:no-error 0)
  (:need-more-data 1)
  (:invalid-api-mixing)
  (:out-of-memory)
  (:feature-not-supported)
  (:too-many-channels)
  (:file-open-failure)
  (:seek-without-length)
  (:unexpected-end-of-file 10)
  (:seek-invalid)
  (:invalid-setup 20)
  (:invalid-stream)
  (:missing-capture-pattern 30)
  (:invalid-stream-structure-version)
  (:continued-packet-flag-invalid)
  (:incorrect-stream-serial-number)
  (:invalid-first-page)
  (:bad-packet-type)
  (:cant-find-last-page)
  (:seek-failed)
  (:ogg-skeleton-not-supported))

(cffi:defcstruct (buffer :class buffer :conc-name buffer-)
  (data :pointer)
  (length :int))

(cffi:defcstruct (info :class info :conc-name info-)
  (sample-rate :unsigned-int)
  (channels :int)
  (setup-memory-required :unsigned-int)
  (setup-temp-memory-required :unsigned-int)
  (temp-memory-required :unsigned-int)
  (max-frame-size :int))

(cffi:defcstruct (comment :class comment :conc-name comment-)
  (vendor :string)
  (list-length :int)
  (list :pointer))

(cffi:defcfun (get-info "stb_vorbis_get_info_") :void
  (vorbis :pointer)
  (info :pointer))

(cffi:defcfun (get-comment "stb_vorbis_get_comment_") :void
  (vorbis :pointer)
  (comment :pointer))

(cffi:defcfun (get-error "stb_vorbis_get_error") error
  (vorbis :pointer))

(cffi:defcfun (close "stb_vorbis_close") :void
  (vorbis :pointer))

(cffi:defcfun (get-sample-offset "stb_vorbis_get_sample_offset") :int
  (vorbis :pointer))

(cffi:defcfun (get-file-offset "stb_vorbis_get_file_offset") :unsigned-int
  (vorbis :pointer))

(cffi:defcfun (open-pushdata "stb_vorbis_open_pushdata") :pointer
  (datablock :pointer)
  (length :int)
  (consumed :pointer)
  (error :pointer)
  (buffer :pointer))

(cffi:defcfun (decode-frame-pushdata "stb_vorbis_decode_frame_pushdata") :int
  (vorbis :pointer)
  (datablock :pointer)
  (length :int)
  (channels :pointer)
  (output :pointer)
  (samples :pointer))

(cffi:defcfun (flush-pushdata "stb_vorbis_flush_pushdata") :void
  (vorbis :pointer))

(cffi:defcfun (decode-filename "stb_vorbis_decode_filename") :int
  (filename :string)
  (channels :pointer)
  (sample-rate :pointer)
  (output :pointer))

(cffi:defcfun (decode-memory "stb_vorbis_decode_memory") :int
  (mem :pointer)
  (length :int)
  (channels :pointer)
  (sample-rate :pointer)
  (output :pointer))

(cffi:defcfun (open-memory "stb_vorbis_open_memory") :pointer
  (data :pointer)
  (length :int)
  (error :pointer)
  (buffer :pointer))

(cffi:defcfun (open-filename "stb_vorbis_open_filename") :pointer
  (filename :string)
  (error :pointer)
  (buffer :pointer))

(cffi:defcfun (open-file "stb_vorbis_open_file") :pointer
  (file :pointer)
  (close-handle-on-close :int)
  (error :pointer)
  (buffer :pointer))

(cffi:defcfun (open-file-section "stb_vorbis_open_file_section") :pointer
  (file :pointer)
  (close-handle-on-close :int)
  (error :pointer)
  (buffer :pointer)
  (length :unsigned-int))

(cffi:defcfun (seek-frame "stb_vorbis_seek_frame") :int
  (vorbis :pointer)
  (sample :unsigned-int))

(cffi:defcfun (seek "stb_vorbis_seek") :int
  (vorbis :pointer)
  (sample :unsigned-int))

(cffi:defcfun (seek-start "stb_vorbis_seek_start") :int
  (vorbis :pointer))

(cffi:defcfun (stream-length-in-samples "stb_vorbis_stream_length_in_samples") :unsigned-int
  (vorbis :pointer))

(cffi:defcfun (stream-length-in-seconds "stb_vorbis_stream_length_in_seconds") :float
  (vorbis :pointer))

(cffi:defcfun (get-frame-float "stb_vorbis_get_frame_float") :int
  (vorbis :pointer)
  (channels :pointer)
  (output :float))

(cffi:defcfun (get-frame-short-interleaved "stb_vorbis_get_frame_short_interleaved") :int
  (vorbis :pointer)
  (channel :int)
  (buffer :pointer)
  (length :int))

(cffi:defcfun (get-frame-short "stb_vorbis_get_frame_short") :int
  (vorbis :pointer)
  (channel :int)
  (buffers :pointer)
  (length :int))

(cffi:defcfun (get-samples-float-interleaved "stb_vorbis_get_samples_float_interleaved") :int
  (vorbis :pointer)
  (channels :int)
  (buffer :pointer)
  (length :int))

(cffi:defcfun (get-samples-float "stb_vorbis_get_samples_float") :int
  (vorbis :pointer)
  (channels :int)
  (buffers :pointer)
  (length :int))

(cffi:defcfun (get-samples-short-interleaved "stb_vorbis_get_samples_short_interleaved") :int
  (vorbis :pointer)
  (channels :int)
  (buffer :pointer)
  (length :int))

(cffi:defcfun (get-samples-short "stb_vorbis_get_samples_short") :int
  (vorbis :pointer)
  (channels :int)
  (buffers :pointer)
  (length :int))
