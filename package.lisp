(defpackage #:org.shirakumo.fraf.vorbis.cffi
  (:use #:cl)
  (:shadow #:close #:error)
  (:export
   #:libvorbis
   #:error
   #:buffer
   #:buffer-data
   #:buffer-length
   #:info
   #:info-samplerate
   #:info-channels
   #:info-setup-memory-required
   #:info-setup-temp-memory-required
   #:info-temp-memory-required
   #:info-max-frame-size
   #:comment
   #:comment-vendor
   #:comment-list-length
   #:comment-list
   #:get-info
   #:get-comment
   #:get-error
   #:close
   #:get-sample-offset
   #:get-file-offset
   #:open-pushdata
   #:decode-frame-pushdata
   #:flush-pushdata
   #:decode-filename
   #:decode-memory
   #:open-memory
   #:open-filename
   #:open-file
   #:open-file-section
   #:seek-frame
   #:seek
   #:seek-start
   #:stream-length-in-samples
   #:stream-length-in-seconds
   #:get-frame-float
   #:get-frame-short-interleaved
   #:get-frame-short
   #:get-samples-float-interleaved
   #:get-samples-float
   #:get-samples-short-interleaved
   #:get-samples-short))

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
   #:shutdown
   #:file
   #:handle
   #:channels
   #:samplerate
   #:max-frame-size
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
