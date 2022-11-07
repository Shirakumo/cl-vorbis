#|
 This file is a part of cl-vorbis
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vorbis.cffi)

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
  (:false -1)
  (:end-of-file -2)
  (:hole -3)
  (:read-error -128)
  (:internal-fault -129)
  (:implementation-error -130)
  (:invalid -131)
  (:not-vorbis -132)
  (:bad-header -133)
  (:version -134)
  (:not-audio -135)
  (:bad-packet -136)
  (:bad-link -137)
  (:not-seekable -138)
  (:no-error 0)
  (:part-open 1)
  (:opened 2)
  (:stream-set 3)
  (:init-set 4))

(cffi:defcstruct (data-source :class data-source :conc-name data-source-)
  (buffer :pointer)
  (size :size)
  (index :size)
  (owner :boolean))

(cffi:defcstruct (callbacks :class callbacks :conc-name callbacks-)
  (read :pointer)
  (seek :pointer)
  (close :pointer)
  (tell :pointer))

(cffi:defcstruct (sync-state :class sync-state :conc-name sync-state-)
  (data :pointer)
  (storage :int)
  (fill :int)
  (returned :int)
  (unsynced :int)
  (header-bytes :int)
  (body-bytes :int))

(cffi:defcstruct (stream-state :class stream-state :conc-name stream-state-)
  (data :pointer)
  (body-storage :long)
  (body-fill :long)
  (body-returned :long)
  (lacing-values (:pointer :int))
  (granule-values (:pointer :int64))
  (lacing-storage :long)
  (lacing-fill :long)
  (lacing-packet :long)
  (lacing-returned :long)
  (header :unsigned-char :count 282)
  (header-fill :int)
  (e-o-s :int)
  (b-o-s :int)
  (serial-number :long)
  (page-number :long)
  (packet-number :int64)
  (granule-position :int64))

(cffi:defcstruct (dsp-state :class dsp-state :conc-name dsp-state-)
  (analysis-p :int)
  (info :pointer)
  (pcm (:pointer (:pointer :float)))
  (pcm-ret (:pointer (:pointer :float)))
  (pcm-storage :int)
  (pcm-current :int)
  (pcm-returned :int)
  (pre-extrapolate :int)
  (eof-flag :int)
  (lw :long)
  (w :long)
  (nw :long)
  (centerw :long)
  (granule-position :int64)
  (sequence :int64)
  (glue-bits :int64)
  (time-bits :int64)
  (floor-bits :int64)
  (res-bits :int64)
  (backend-state :pointer))

(cffi:defcstruct (info :class info :conc-name info-)
  (version :int)
  (channels :int)
  (samplerate :long)
  (bitrate-upper :long)
  (bitrate-nominal :long)
  (bitrate-lower :long)
  (bitrate-window :long)
  (codec-setup :pointer))

(cffi:defcstruct (oggpack-buffer :class oggpack-buffer :conc-name oggpack-buffer)
  (end-byte :long)
  (end-bit :int)
  (buffer :pointer)
  (ptr :pointer)
  (storage :long))

(cffi:defcstruct (vorbis-block :class vorbis-block :conc-name vorbis-block-)
  (pcm (:pointer (:pointer :float)))
  (buffer (:struct oggpack-buffer))
  (lw :long)
  (w :long)
  (nw :long)
  (pcm-end :int)
  (mode :int)
  (eof-flag :int)
  (granule-position :int64)
  (sequence :int64)
  (dsp-state :pointer)
  (local-store :pointer)
  (local-top :long)
  (local-alloc :long)
  (total-use :long)
  (reap :pointer)
  (glue-bits :long)
  (time-bits :long)
  (floor-bits :long)
  (res-bits :long)
  (internal :pointer))

(cffi:defcstruct (comment :class comment :conc-name comment-)
  (user-comments (:pointer :string))
  (comment-lengths (:pointer :int))
  (comments :int)
  (vendor :string))

(cffi:defcstruct (file :class file :conc-name file-)
  (data-source :pointer)
  (seekable :int)
  (offset :int64)
  (end :int64)
  (sync-state (:struct sync-state))
  (links :int)
  (offsets (:pointer :int64))
  (data-offsets (:pointer :int64))
  (serial-numbers (:pointer :long))
  (pcm-lengths (:pointer :int64))
  (info :pointer)
  (vorbis-comment :pointer)
  (pcm-offset :int64)
  (ready-state :int)
  (current-serial-number :long)
  (current-link :int)
  (bit-track :double)
  (samp-track :double)
  (stream-state (:struct stream-state))
  (dsp-state (:struct dsp-state))
  (vorbis-block (:struct vorbis-block))
  (callbacks (:struct callbacks)))

(cffi:defcfun (close "ov_clear") :int
  (file :pointer))

(cffi:defcfun (fopen "ov_fopen") :int
  (path :string)
  (file :pointer))

(cffi:defcfun (open "ov_open") :int
  (handle :pointer)
  (file :pointer)
  (initial :pointer)
  (bytes :long))

(cffi:defcfun (open-callbacks "ov_open_callbacks_") :int
  (datasource :pointer)
  (file :pointer)
  (initial :pointer)
  (bytes :long)
  (callbacks :pointer))

(cffi:defcfun (test "ov_test") :int
  (handle :pointer)
  (file :pointer)
  (initial :pointer)
  (bytes :long))

(cffi:defcfun (test-callbacks "ov_test_callbacks_") :int
  (datasource :pointer)
  (file :pointer)
  (initial :pointer)
  (bytes :long)
  (callbacks :pointer))

(cffi:defcfun (test-open "ov_test_open") :int
  (file :pointer))

(cffi:defcfun (bitrate "ov_bitrate") :long
  (file :pointer)
  (i :int))

(cffi:defcfun (bitrate-instant "ov_bitrate_instant") :long
  (file :pointer))

(cffi:defcfun (streams "ov_streams") :long
  (file :pointer))

(cffi:defcfun (seekable "ov_seekable") :long
  (file :pointer))

(cffi:defcfun (serial-number "ov_serialnumber") :long
  (file :pointer)
  (i :int))

(cffi:defcfun (raw-total "ov_raw_total") :int64
  (file :pointer)
  (i :int))

(cffi:defcfun (pcm-total "ov_pcm_total") :int64
  (file :pointer)
  (i :int))

(cffi:defcfun (time-total "ov_time_total") :double
  (file :pointer)
  (i :int))

(cffi:defcfun (raw-seek "ov_raw_seek") :int
  (file :pointer)
  (pos :int64))

(cffi:defcfun (pcm-seek "ov_pcm_seek") :int
  (file :pointer)
  (pos :int64))

(cffi:defcfun (pcm-seek-page "ov_pcm_seek_page") :int
  (file :pointer)
  (pos :int64))

(cffi:defcfun (time-seek "ov_time_seek") :int
  (file :pointer)
  (pos :double))

(cffi:defcfun (time-seek-page "ov_time_seek_page") :int
  (file :pointer)
  (pos :double))

(cffi:defcfun (raw-seek-lap "ov_raw_seek_lap") :int
  (file :pointer)
  (pos :int64))

(cffi:defcfun (pcm-seek-lap "ov_pcm_seek_lap") :int
  (file :pointer)
  (pos :int64))

(cffi:defcfun (pcm-seek-page-lap "ov_pcm_seek_page_lap") :int
  (file :pointer)
  (pos :int64))

(cffi:defcfun (time-seek-lap "ov_time_seek_lap") :int
  (file :pointer)
  (pos :double))

(cffi:defcfun (time-seek-page-lap "ov_time_seek_page_lap") :int
  (file :pointer)
  (pos :double))

(cffi:defcfun (raw-tell "ov_raw_tell") :int64
  (file :pointer))

(cffi:defcfun (pcm-tell "ov_pcm_tell") :int64
  (file :pointer))

(cffi:defcfun (time-tell "ov_time_tell") :double
  (file :pointer))

(cffi:defcfun (info "ov_info") :pointer
  (file :pointer)
  (link :int))

(cffi:defcfun (comment "ov_comment") :pointer
  (file :pointer)
  (link :int))

(cffi:defcfun (read-float "ov_read_float") :long
  (file :pointer)
  (channels (:pointer (:pointer :float)))
  (samples :int)
  (bitstream (:pointer :int)))

(cffi:defcfun (read-filter "ov_read_filter") :long
  (file :pointer)
  (buffer :pointer)
  (length :int)
  (bigendian-p :boolean)
  (word-size :int)
  (signed-p :boolean)
  (bistream (:pointer :int))
  (filter :pointer))

(cffi:defcfun (read "ov_read") :long
  (file :pointer)
  (buffer :pointer)
  (length :int)
  (bigendian-p :boolean)
  (word-size :int)
  (signed-p :boolean)
  (bitsream :pointer))

(cffi:defcfun (crosslap "ov_crosslap") :int
  (a :pointer)
  (b :pointer))

(cffi:defcfun (half-rate "ov_halfrate") :int
  (file :pointer)
  (flag :int))

(cffi:defcfun (half-rate-p "ov_halfrate_p") :int
  (file :pointer))
