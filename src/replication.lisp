;;;; replication.lisp --- leader/follower replication log

(in-package #:scalaxy)

(defstruct (replicator (:constructor %make-replicator (seq log)))
  seq   ; next sequence number to assign
  log)  ; list of (seq . message), most recent first (bounded)

(defun make-replicator ()
  (%make-replicator 0 nil))

(defun replicator-record (replicator seq msg)
  "Record (SEQ . MSG) in the local op log (bounded to the last 1000 ops)."
  (push (cons seq msg) (replicator-log replicator))
  (when (> (length (replicator-log replicator)) 1000)
    (setf (replicator-log replicator)
          (subseq (replicator-log replicator) 0 1000)))
  seq)

(defun replicator-entries (replicator &key (from 0))
  "Return log entries with seq >= FROM in ascending order."
  (nreverse (remove-if (lambda (e) (< (car e) from))
                       (replicator-log replicator))))
