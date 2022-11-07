#|
 This file is a part of cl-vorbis
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.vorbis.cffi
  (:use #:cl)
  (:shadow #:close #:open #:error #:read)
  (:export
   #:libvorbis
   #:error
   #:data-source
   #:data-source-buffer
   #:data-source-size
   #:data-source-index
   #:data-source-owner
   #:callbacks
   #:callbacks-read
   #:callbacks-seek
   #:callbacks-close
   #:callbacks-tell
   #:info
   #:info-version
   #:info-channels
   #:info-samplerate
   #:info-bitrate-upper
   #:info-bitrate-nominal
   #:info-bitrate-lower
   #:info-bitrate-window
   #:comment
   #:comment-user-comments
   #:comment-comment-lengths
   #:comment-comments
   #:comment-vendor
   #:file
   #:file-data-source
   #:file-seekable
   #:file-offset
   #:file-end
   #:file-sync-state
   #:file-links
   #:file-offsets
   #:file-data-offsets
   #:file-serial-numbers
   #:file-pcm-lengths
   #:file-info
   #:file-vorbis-comment
   #:file-pcm-offset
   #:file-ready-state
   #:file-current-serial-number
   #:file-current-link
   #:file-bit-track
   #:file-samp-track
   #:file-stream-state
   #:file-dsp-state
   #:file-vorbis-block
   #:file-callbacks
   #:close
   #:fopen
   #:open
   #:open-callbacks
   #:test
   #:test-callbacks
   #:test-open
   #:bitrate
   #:bitrate-instant
   #:streams
   #:seekable
   #:serial-number
   #:raw-total
   #:pcm-total
   #:time-total
   #:raw-seek
   #:pcm-seek
   #:pcm-seek-page
   #:time-seek
   #:time-seek-page
   #:raw-seek-lap
   #:pcm-seek-lap
   #:pcm-seek-page-lap
   #:time-seek-lap
   #:time-seek-page-lap
   #:raw-tell
   #:pcm-tell
   #:time-tell
   #:info
   #:comment
   #:read-float
   #:read-filter
   #:read
   #:crosslap
   #:half-rate
   #:half-rate-p))

(defpackage #:org.shirakumo.fraf.vorbis
  (:use #:cl)
  (:local-nicknames
   (#:vorbis #:org.shirakumo.fraf.vorbis.cffi))
  (:shadow #:open #:close)
  (:export
   #:need-more-data
   #:vorbis-error
   #:code
   #:file
   #:init
   #:file
   #:handle
   #:channels
   #:samplerate
   #:close
   #:open
   #:open-file
   #:open-pointer
   #:open-vector
   #:with-file
   #:file-offset
   #:sample-index
   #:comments
   #:vendor
   #:seek
   #:seek-frame
   #:sample-count
   #:duration
   #:decode-frame
   #:decode-frame-ptrs
   #:decode
   #:decode-interleaved))
