#|
 This file is a part of cl-vorbis
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.vorbis)

(define-condition need-more-data (warning)
  ())

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
  (case error
    (:no-error
     NIL)
    (:need-more-data
     (warn 'need-more-data))
    (T
     (error 'vorbis-error :file file :code error))))

(defstruct (file
            (:conc-name NIL)
            (:constructor %make-file (handle channels samplerate max-frame-size))
            (:copier NIL)
            (:predicate NIL))
  (handle NIL :type cffi:foreign-pointer)
  (channels 0 :type (unsigned-byte 8) :read-only T)
  (samplerate 0 :type (unsigned-byte 32) :read-only T)
  (max-frame-size 0 :type (unsigned-byte 32) :read-only T))

(defun make-file (handle error)
  (check-error NIL (cffi:mem-ref error 'vorbis:error))
  (cffi:with-foreign-objects ((info '(:struct vorbis:info)))
    (vorbis:get-info handle info)
    (%make-file handle (vorbis:info-channels info) (vorbis:info-samplerate info) (vorbis:info-max-frame-size info))))

(defun check-file-for-error (file)
  (check-error file (vorbis:get-error (handle file))))

(defun close (file)
  (vorbis:close (handle file))
  (setf (handle file) (cffi:null-pointer)))

(defun open (thing &rest initargs &key buffer start end)
  (declare (ignore buffer start end))
  (init)
  (etypecase thing
    ((or string pathname)
     (apply #'open-file thing initargs))
    (cffi:foreign-pointer
     (apply #'open-pointer thing initargs))
    ((simple-array (unsigned-byte 8) (*))
     (apply #'open-vector thing initargs))))

(defun open-file (path &key buffer)
  (cffi:with-foreign-objects ((error 'vorbis:error))
    (setf (cffi:mem-ref error 'vorbis:error) :no-error)
    (make-file (vorbis:open-filename (namestring (truename path)) error (or buffer (cffi:null-pointer))) error)))

(defun open-pointer (memory length &key buffer)
  (cffi:with-foreign-objects ((error 'vorbis:error))
    (setf (cffi:mem-ref error 'vorbis:error) :no-error)
    (make-file (vorbis:open-memory memory length error (or buffer (cffi:null-pointer))) error)))

(defun open-vector (vector &key buffer (start 0) (end (length vector)))
  (cffi:with-foreign-objects ((error 'vorbis:error))
    (setf (cffi:mem-ref error 'vorbis:error) :no-error)
    (make-file (vorbis:open-memory (static-vectors:static-vector-pointer vector :offset start) (- end start) error (or buffer (cffi:null-pointer))) error)))

(defmacro with-file ((file input &rest args) &body body)
  (let ((fileg (gensym "FILE")))
    `(let* ((,fileg (open ,input ,@args))
            (,file ,fileg))
       (unwind-protect
            (progn ,@body)
         (close ,fileg)))))

;; TODO: streaming api

(defun file-offset (file)
  (vorbis:get-file-offset (handle file)))

(defun sample-index (file)
  (vorbis:get-sample-offset (handle file)))

(defun (setf sample-index) (index file)
  (vorbis:seek (handle file) index)
  index)

(defun comments (file)
  (cffi:with-foreign-objects ((comment '(:struct vorbis:comment)))
    (vorbis:get-comment (handle file) comment)
    (loop for i from 0 below (vorbis:comment-list-length comment)
          collect (cffi:mem-aref (vorbis:comment-list comment) :string i))))

(defun vendor (file)
  (cffi:with-foreign-objects ((comment '(:struct vorbis:comment)))
    (vorbis:get-comment (handle file) comment)
    (vorbis:comment-vendor comment)))

(defun seek (file sample)
  (vorbis:seek (handle file) sample))

(defun seek-frame (file sample)
  (vorbis:seek-frame (handle file) sample))

(defun sample-count (file)
  (vorbis:stream-length-in-samples (handle file)))

(defun duration (file)
  (vorbis:stream-length-in-seconds (handle file)))

(defun decode-frame (file &optional buffers)
  (cffi:with-foreign-objects ((channels :int)
                              (output :pointer))
    (let* ((samples (prog1 (vorbis:get-frame-float (handle file) channels output)
                      (check-file-for-error file)))
           (channels (cffi:mem-ref channels :int))
           (output (cffi:mem-ref output :pointer))
           (buffers (or buffers (loop for i from 0 below channels
                                      collect (make-array samples :element-type 'single-float)))))
      (loop for i from 0 below channels
            for buffer in buffers
            for pointer = (cffi:mem-aref output :pointer i)
            do (dotimes (i samples)
                 (setf (aref buffer i) (cffi:mem-aref pointer :float i))))
      (values buffers samples channels))))

(defun decode-frame-ptrs (file)
  (cffi:with-foreign-objects ((channels :int)
                              (output :pointer))
    (let ((samples (prog1 (vorbis:get-frame-float (handle file) channels output)
                     (check-file-for-error file)))
          (channels (cffi:mem-ref channels :int))
          (output (cffi:mem-ref output :pointer)))
      (values output samples channels))))

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
                 (prog1 (vorbis:get-samples-float (handle file) count arrays (- end start))
                   (check-file-for-error file)))))
      (pin #'inner 0))))

(defun decode-interleaved (file buffer &key (start 0) end)
  (with-pinned-buffer (pointer buffer :offset start)
    (prog1 (vorbis:get-samples-float-interleaved (handle file) (channels file) pointer (- end start))
      (check-file-for-error file))))
