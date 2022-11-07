#|
 This file is a part of cl-vorbis
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vorbis)

(cffi:defcallback read-mem-func :size ((buffer :pointer) (element-size :size) (elements :size) (data-source :pointer))
  (let* ((chars (* element-size elements))
         (index (vorbis:data-source-index data-source))
         (size (vorbis:data-source-size data-source))
         (copyable (min chars (- size index))))
    (static-vectors:replace-foreign-memory buffer (cffi:inc-pointer (vorbis:data-source-buffer data-source) index) copyable)
    (setf (vorbis:data-source-index data-source) (+ index copyable))
    copyable))

(cffi:defcallback seek-mem-func :int ((data-source :pointer) (offset :int64) (whence :int))
  (let ((new (ecase whence
               (1 offset)
               (2 (+ offset (vorbis:data-source-index data-source)))
               (3 (+ offset (vorbis:data-source-size data-source))))))
    (cond ((<= new (vorbis:data-source-size data-source))
           (setf (vorbis:data-source-index data-source) new)
           0)
          (T
           1))))

(cffi:defcallback close-mem-func :int ((data-source :pointer))
  (when (vorbis:data-source-owner data-source)
    (cffi:foreign-free (vorbis:data-source-buffer data-source))
    (setf (vorbis:data-source-buffer data-source) (cffi:null-pointer))
    (setf (vorbis:data-source-owner data-source) NIL))
  0)

(cffi:defcallback tell-mem-func :long ((data-source :pointer))
  (vorbis:data-source-index data-source))

(define-condition vorbis-error (error)
  ((file :initarg :file :reader file)
   (code :initarg :code :reader code))
  (:report (lambda (c s) (format s "The vorbis operation failed with the following error:~%  ~a"
                                 (code c)))))

(defun init ()
  (cffi:load-foreign-library 'vorbis:libvorbis))

(defmacro with-pinned-buffer ((ptr data &key (offset 0)) &body body)
  (let ((datag (gensym "DATA")) (thunk (gensym "THUNK")))
    `(let ((,datag ,data))
       (flet ((,thunk (,ptr)
                (declare (type cffi:foreign-pointer ,ptr))
                ,@body))
         (cond #+sbcl
               ((typep ,datag 'sb-kernel:simple-unboxed-array)
                (sb-sys:with-pinned-objects (,datag)
                  (let ((,ptr (sb-sys:vector-sap ,datag)))
                    (,thunk (cffi:inc-pointer ,ptr (* ,offset 4))))))
               (T
                (,thunk (static-vectors:static-vector-pointer ,datag :offset (* ,offset 4)))))))))

(defun check-error (file error)
  (if (< error 0)
      (error 'vorbis-error :file file :code error)
      error))

(defstruct (file
            (:conc-name NIL)
            (:constructor %make-file (handle channels samplerate))
            (:copier NIL)
            (:predicate NIL))
  (handle NIL :type cffi:foreign-pointer)
  (channels 0 :type (unsigned-byte 8) :read-only T)
  (samplerate 0 :type (unsigned-byte 32) :read-only T)
  (bitstream 0 :type (unsigned-byte 32)))

(defun make-file (handle error)
  (check-error NIL error)
  (let ((info (vorbis:info handle -1)))
    (%make-file handle (vorbis:info-channels info) (vorbis:info-samplerate info))))

(defun close (file)
  (vorbis:close (handle file))
  (setf (handle file) (cffi:null-pointer)))

(defun open (thing &rest initargs &key start end)
  (declare (ignore start end))
  (init)
  (etypecase thing
    ((or string pathname)
     (apply #'open-file thing initargs))
    (cffi:foreign-pointer
     (apply #'open-pointer thing initargs))
    ((simple-array (unsigned-byte 8) (*))
     (apply #'open-vector thing initargs))))

(defun open-file (path)
  (let ((file (cffi:foreign-alloc '(:struct vorbis:file))))
    (make-file file (vorbis:fopen (namestring (truename path)) file))))

(defun open-pointer (memory length &key free-on-close)
  (let ((file (cffi:foreign-alloc '(:struct vorbis:file)))
        (data-source (cffi:foreign-alloc '(:struct vorbis:data-source)))
        (callbacks (cffi:foreign-alloc '(:struct vorbis:callbacks))))
    (setf (vorbis:callbacks-read callbacks) (cffi:callback read-mem-func))
    (setf (vorbis:callbacks-seek callbacks) (cffi:callback seek-mem-func))
    (setf (vorbis:callbacks-close callbacks) (cffi:callback close-mem-func))
    (setf (vorbis:callbacks-tell callbacks) (cffi:callback tell-mem-func))
    (setf (vorbis:data-source-buffer data-source) memory)
    (setf (vorbis:data-source-size data-source) length)
    (setf (vorbis:data-source-index data-source) 0)
    (setf (vorbis:data-source-owner data-source) free-on-close)
    (make-file file (vorbis:open-callbacks data-source file (cffi:null-pointer) 0 callbacks))))

(defun open-vector (vector &key (start 0) (end (length vector)))
  (let ((file (cffi:foreign-alloc '(:struct vorbis:file)))
        (data-source (cffi:foreign-alloc '(:struct vorbis:data-source)))
        (callbacks (cffi:foreign-alloc '(:struct vorbis:callbacks))))
    (setf (vorbis:callbacks-read callbacks) (cffi:callback read-mem-func))
    (setf (vorbis:callbacks-seek callbacks) (cffi:callback seek-mem-func))
    (setf (vorbis:callbacks-close callbacks) (cffi:callback close-mem-func))
    (setf (vorbis:callbacks-tell callbacks) (cffi:callback tell-mem-func))
    (setf (vorbis:data-source-buffer data-source) (cffi:inc-pointer (static-vectors:static-vector-pointer vector) start))
    (setf (vorbis:data-source-size data-source) (- end start))
    (setf (vorbis:data-source-index data-source) 0)
    (setf (vorbis:data-source-owner data-source) 0)
    (make-file file (vorbis:open-callbacks data-source file (cffi:null-pointer) 0 callbacks))))

(defmacro with-file ((file input &rest args) &body body)
  (let ((fileg (gensym "FILE")))
    `(let* ((,fileg (open ,input ,@args))
            (,file ,fileg))
       (unwind-protect
            (progn ,@body)
         (close ,fileg)))))

;; TODO: streaming api

(defun file-offset (file)
  (vorbis:raw-tell (handle file)))

(defun sample-index (file)
  (vorbis:pcm-tell (handle file)))

(defun (setf sample-index) (index file)
  (vorbis:pcm-seek (handle file) index)
  index)

(defun comments (file)
  (let ((comment (vorbis:comment (handle file) -1)))
    (loop for i from 0 below (vorbis:comment-comments comment)
          for ptr = (cffi:mem-aref (vorbis:comment-user-comments comment) :pointer i)
          for len = (cffi:mem-aref (vorbis:comment-user-comments comment) :int i)
          collect (cffi:foreign-string-to-lisp ptr :count len :encoding :utf-8))))

(defun vendor (file)
  (vorbis:comment-vendor (vorbis:comment (handle file) -1)))

(defun seek (file sample)
  (vorbis:pcm-seek (handle file) sample))

(defun seek-frame (file sample)
  (vorbis:pcm-seek-page (handle file) sample))

(defun sample-count (file)
  (vorbis:pcm-total (handle file) -1))

(defun duration (file)
  (vorbis:time-total (handle file) -1))

(defun decode (file buffers &key (start 0) end)
  (let* ((count (channels file))
         (pointers (make-array count :element-type 'cffi:foreign-pointer :initial-element (cffi:null-pointer)))
         (end (or end (1- (length (first buffers))))))
    (declare (dynamic-extent pointers))
    (labels ((pin (func i)
               (if buffers
                   (with-pinned-buffer (pointer (pop buffers) :offset start)
                     (setf (aref pointers i) pointer)
                     (pin func (1+ i)))
                   (funcall func)))
             (inner ()
               (cffi:with-foreign-object (arrays :pointer count)
                 (loop for i from 0 below count
                       do (setf (cffi:mem-aref arrays :pointer i) (aref pointers i)))
                 (cffi:with-foreign-object (bitstream :int)
                   (setf (cffi:mem-ref bitstream :int) (bitstream file))
                   (prog1 (check-error file (vorbis:read-float (handle file) arrays (- end start) bitstream))
                     (setf (bitstream file) (cffi:mem-ref bitstream :int)))))))
      (pin #'inner 0))))

(defun decode-interleaved (file buffer &key (start 0) end)
  (with-pinned-buffer (pointer buffer :offset start)
    (cffi:with-foreign-object (bitstream :int)
      (setf (cffi:mem-ref bitstream :int) (bitstream file))
      (prog1 (check-error file (vorbis:read (handle file) pointer (- end start) 0 2 1 bitstream))
        (setf (bitstream file) (cffi:mem-ref bitstream :int))))))
