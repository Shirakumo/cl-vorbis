#|
 This file is a part of cl-vorbis
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vorbis)

(docs:define-docs
  (type need-more-data
    "Warning signalled when not enough data is available to complete the operation.")

  (type vorbis-error
    "Error signalled when a vorbis operation fails.

See CODE")

  (function code
    "Returns the internal error code that produced the error.

See VORBIS-ERROR")
  
  (function init
    "Load the foreign library.

Automatically called by OPEN.")
  
  (type file
    "Representation of an OGG/Vorbis file.

See OPEN
See HANDLE
See CHANNELS
See SAMPLERATE
See MAX-FRAME-SIZE
See CLOSE
See FILE-OFFSET
See SAMPLE-INDEX
See COMMENTS
See VENDOR
See SEEK
See SEEK-FRAME
See SAMPLE-COUNT
See DURATION
See DECODE-FRAME
See DECODE-FRAME-PTRS
See DECODE
See DECODE-INTERLEAVED")
  
  (function handle
    "Returns the pointer to the underlying C handle.

See FILE")
  
  (function channels
    "Returns the number of channels in the file.

See FILE")
  
  (function samplerate
    "Returns the sample rate of the audio data in the file.

See FILE")
  
  (function max-frame-size
    "Returns the maximum size of a frame in bytes.

See FILE")
  
  (function close
    "Closes the file and invalidates its handle.

See FILE")
  
  (function open
    "Opens the given input as an OGG/Vorbis file.

Returns the newly generated FILE object or signals an error. The input
can be a STRING/PATHNAME, a CFFI:FOREIGN-POINTER, or an UNSIGNED-BYTE 8
static vector. If you pass a vector that is not pinned in memory, bad
things will happen.

This function automatically calls INIT to ensure the library is
loaded.

See OPEN-FILE
See OPEN-POINTER
See OPEN-VECTOR
See FILE
See VORBIS-ERROR")
  
  (function open-file
    "Open a vorbis file from the file system.

INIT must be called prior to this.

Returns a FILE instance if successful, signals an error otherwise.

See INIT
See FILE
See OPEN")
  
  (function open-pointer
    "Open a vorbis file from a CFFI:FOREIGN-POINTER

INIT must be called prior to this.

Returns a FILE instance if successful, signals an error otherwise.

The memory area MUST NOT move or be freed before CLOSE is called on
the returned file.

See INIT
See FILE
See OPEN")
  
  (function open-vector
    "Open a vorbis file from an octet vector

INIT must be called prior to this.

Returns a FILE instance if successful, signals an error otherwise.

The vector must be a static-vector and MUST NOT move or be deallocated
before CLOSE is called on the returned file.

See INIT
See FILE
See OPEN")
  
  (function with-file
    "Shorthand macro to OPEN a file and CLOSE it on exit.

See FILE
See OPEN
See CLOSE")
  
  (function file-offset
    "Returns the current seek point within the file or memory region.

See FILE")
  
  (function sample-index
    "Returns the sample index the decoder is currently at.

If the index is not known, -1 is returned.

This place may be SETFed, which is equivalent to calling SEEK.

See FILE
See SEEK")
  
  (function comments
    "Returns a list of comments as strings embedded in the vorbis file.

See FILE")
  
  (function vendor
    "Returns the name of the vendor that produced the vorbis file.

See FILE")
  
  (function seek
    "Seek to the given sample in the vorbis file.

See FILE
See SEEK-FRAME")
  
  (function seek-frame
    "Seek in the file such that the next frame being decoded includes the requested sample.

See FILE
See SEEK")
  
  (function sample-count
    "Returns the number of samples total in the vorbis file.

See FILE")
  
  (function duration
    "Returns the length of the vorbis file in seconds.

See FILE")
  
  (function decode
    "Decode a number of samples to per-channel buffers.

BUFFERS must be a list of pinnable vectors that will be filled with
samples, each starting by START and ending before END.

Returns the number of samples that were written to the buffers.

See FILE
See DECODE-INTERLEAVED")
  
  (function decode-interleaved
    "Decode a number of samples to a single buffer, inteleaved.

BUFFER must be a pinnable vector that will be filled with samples,
starting by START and ending before END.

Returns the number of samples that were written to the buffer.

See FILE
See DECODE"))
