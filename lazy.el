;;; lazy.el --- Lazy evaluation library              -*- lexical-binding: t; -*-

;; Copyright (C) 2018  chuntaro@sakura-games.jp

;; Author:  <chuntaro@sakura-games.jp>
;; Keywords: lisp, lazy
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Implementation of `SRFI 45' in Emacs Lisp and Stream functions.
;;
;; SRFI 45
;; https://srfi.schemers.org/srfi-45/srfi-45.html
;; http://www.katch.ne.jp/~leque/translations/srfi-45/srfi-45j.html (Japanese)

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Primitives
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lz-box (x) (list x))
(defalias 'lz-unbox 'car)
(defalias 'lz-setbox 'setcar)

(defmacro lz-lazy (exp)
  `(lz-box (cons 'lz-lazy (lambda () ,exp))))

(defsubst lz-eager (x)
  (lz-box (cons 'lz-eager x)))

(defmacro lz-delay (exp)
  `(lz-lazy (lz-eager ,exp)))

(defun lz-force (promise)
  (while (let ((content (lz-unbox promise)))
           (cl-case (car content)
             (lz-eager (setq promise (cdr content))
                       nil)
             (lz-lazy  (let* ((promise* (funcall (cdr content)))
                              (content  (lz-unbox promise)))    ; *
                         (if (not (eq (car content) 'lz-eager)) ; *
                             (progn (setcar content (car (lz-unbox promise*)))
                                    (setcdr content (cdr (lz-unbox promise*)))
                                    (lz-setbox promise* content))))
                       t))))
  promise)

;; (*) These two lines re-fetch and check the original promise in case
;;     the first line of the let* caused it to be forced.  For an example
;;     where this happens, see reentrancy test 3 below.
;; (*) これらの 2 行は、let* の最初の行が force された場合に元の promise を
;;     再フェッチしてチェックします。これが起こる例については、以下の
;;     reentrancy test 3 を参照してください。
;;
;;=========================================================================
;; Reentrancy test 3: due to John Shutt
;;
;; (define q
;;   (let ((count 5))
;;     (define (get-count) count)
;;     (define p (delay (if (<= count 0)
;;                          count
;;                          (begin (set! count (- count 1))
;;                                 (force p)
;;                                 (set! count (+ count 2))
;;                                 count))))
;;     (list get-count p)))
;; (define get-count (car q))
;; (define p (cadr q))
;;
;; (get-count)  ; =>   5
;; (force p)    ; =>   0
;; (get-count)  ; =>   10
;;
;; Emacs Lisp version:
;; (defvar q
;;   (let ((count 5))
;;     (cl-labels ((get-count () count))
;;       (let ((p (lz-delay (if (<= count 0)
;;                              count
;;                            (progn (setq count (- count 1))
;;                                   (lz-force p)
;;                                   (setq count (+ count 2))
;;                                   count)))))
;;         (list #'get-count p)))))
;; (defvar get-count (car q))
;; (defvar p (cadr q))
;;
;; (funcall get-count)                     ; =>   5
;; (lz-force p)                            ; =>   0
;; (funcall get-count)                     ; =>   10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stream functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro lz-cons (first rest)
  `(lz-delay (cons ,first ,rest)))

(defsubst lz-empty ()
  (lz-delay nil))

(defun lz-empty-p (stream)
  (null (lz-force stream)))

(defsubst lz-first (stream)
  (car (lz-force stream)))

(defsubst lz-rest (stream)
  (cdr (lz-force stream)))

(defun lz-elt (stream n)
  (while (> n 0)
    (setq stream (lz-rest stream))
    (setq n (1- n)))
  (lz-first stream))

(defun lz-length (stream)
  (let ((len 0))
    (while (not (lz-empty-p stream))
      (setq len (1+ len)
            stream (lz-rest stream)))
    len))

(defun lz-stream-p (stream)
  (and (consp stream)
       (consp (car stream))
       (memq (caar stream) '(lz-lazy lz-eager))
       t))

(defun lz-do (function stream)
  (while (not (lz-empty-p stream))
    (funcall function (lz-first stream))
    (setq stream (lz-rest stream))))

(defalias 'lz-each #'lz-do)

(defmacro lz-dostream (spec &rest body)
  (declare (indent 1))
  `(lz-do (lambda (,(car spec))
            ,@body)
          ,(cadr spec)))

(defun lz-into-list (stream)
  (let (list)
    (lz-dostream (elt stream)
      (push elt list))
    (nreverse list)))

(defun lz-range (&optional start end step)
  (unless start (setq start 0))
  (and end (> start end) (setq end start))
  (unless step (setq step 1))
  (lz-lazy
   (if (and end (= start end))
       (lz-empty)
     (lz-cons start (lz-range (+ start step) end step)))))

(defun lz-take (stream n)
  (when (< n 0) (setq n 0))
  (lz-lazy
   (if (or (zerop n)
           (lz-empty-p stream))
       (lz-empty)
     (lz-cons (lz-first stream)
              (lz-take (lz-rest stream) (1- n))))))

(defun lz-drop (stream n)
  (when (< n 0) (setq n 0))
  (lz-lazy
   (progn
     (while (not (or (lz-empty-p stream)
                     (zerop n)))
       (setq n (1- n))
       (setq stream (lz-rest stream)))
     (if (lz-empty-p stream)
         (lz-empty)
       (lz-cons (lz-first stream) (lz-rest stream))))))

(defun lz-take-while (pred stream)
  (lz-lazy
   (if (not (funcall pred (lz-first stream)))
       (lz-empty)
     (lz-cons (lz-first stream)
              (lz-take-while pred (lz-rest stream))))))

(defun lz-drop-while (pred stream)
  (lz-lazy
   (progn
     (while (not (or (lz-empty-p stream)
                     (funcall pred (lz-first stream))))
       (setq stream (lz-rest stream)))
     (unless (lz-empty-p stream)
       (lz-cons (lz-first stream)
                (lz-rest stream))))))

(defun lz-map (function stream)
  (lz-lazy
   (if (lz-empty-p stream)
       (lz-empty)
     (lz-cons (funcall function (lz-first stream))
              (lz-map function (lz-rest stream))))))

(defun lz-filter (pred stream)
  (lz-lazy
   (progn
     (while (not (or (lz-empty-p stream)
                     (funcall pred (lz-first stream))))
       (setq stream (lz-rest stream)))
     (if (lz-empty-p stream)
         (lz-empty)
       (lz-cons (lz-first stream)
                (lz-filter pred (lz-rest stream)))))))

(defun lz-remove (pred stream)
  (lz-filter (lambda (elt) (not (funcall pred elt)))
             stream))

(defun lz-reduce (function stream initial-value)
  (if (lz-empty-p stream)
      initial-value
    (let ((acc initial-value))
      (lz-dostream (elt stream)
        (setq acc (funcall function acc elt)))
      acc)))

(defun lz-reduce-while (pred function stream initial-value)
  (if (lz-empty-p stream)
      initial-value
    (let ((acc initial-value))
      (catch 'lz--break
        (lz-dostream (elt stream)
          (setq acc (funcall function acc elt))
          (unless (funcall pred acc)
            (throw 'lz--break nil))))
      acc)))

(defun lz-find (pred stream &optional default)
  (catch 'lz--break
    (lz-dostream (elt stream)
      (when (funcall pred elt)
        (throw 'lz--break elt)))
    default))

(provide 'lazy)
;;; lazy.el ends here
