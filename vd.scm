#!/usr/bin/env guile
!#

(use-modules (artanis artanis) (artanis utils) (srfi srfi-1)
             (ice-9 ftw) (ice-9 match) (ice-9 format))
(init-server)

;; TODO: use DB instead
(define *node-table* (make-hash-table))
(define *video-queue* (new-queue))
(define Q_MAX 3)

(define upload-form
  '(form (@ (method "POST") (enctype "multipart/form-data") (action "/scm/upload"))
         "File to upload: " (input (@ (type "file") (name "upfile"))) (br)
         "Notes about the file: " (input (@ (type "text") (name "note")))
         (br) (br)
         (input (@ (type "submit") (value "Press")) "to upload the file!")))

(define (get-uploaded-files)
  (scandir "upload"
           (lambda (s)
             (and (not (or (string=? s ".") (string=? s "..")))
                  (basename s)))))

(get "/scm/register/:id" #:mime 'json
  (lambda (rc)
    (cond
     ((hash-ref *node-table* (params rc "id"))
      (:mime rc (json (object ("operation" "register") ("status" "exists")))))
     (else
      (hash-set! *node-table* (params rc "id") '())
      (:mime rc (json (object ("operation" "register") ("status" "ok"))))))))
 
(get "/scm/check/:id" #:mime 'json
  (lambda (rc)
    (let ((id (params rc "id")))
      (cond
       ((hash-ref *node-table* id)
        => (lambda (files)
             (let* ((ql (get-uploaded-files))
                    (fs (lset-difference string=? ql files)))
               (format #t "upload: ~{~a~^ ~}" ql)
               (:mime rc (json (object ("operation" "check")
                                       ("status" "update")
                                       ("new-video" ,fs)))))))
       (else (:mime rc (json (object ("operation" "check") ("status" "no")))))))))

;; NOTE: client should call update after all files are downloaded!
(get "/scm/update/:id/:file" #:mime 'json
  (lambda (rc)
    (let ((id (params rc "id"))
          (file (params rc "file"))
          (ql (queue->list *video-queue*)))
      (hash-set! *node-table* id (cons id ql)) ; update existed files
      (:mime rc (json (object ("operation" "update") ("status" "ok" )))))))

(define (upload-stat sl fl) (list (car fl) (car sl)))
(post "/scm/upload"
  #:from-post `(store #:path "upload" #:mode #o664 #:success-ret ,upload-stat #:simple-ret? #f)
  #:mime 'json
  (lambda (rc)
    (when (> (queue-length *video-queue*) Q_MAX)
          (queue-out! *node-table*))
    (match (:from-post rc 'store)
      ((file size)
       (queue-in! *video-queue* file)
       (:mime rc (json (object ("operation" "upload") ("status" "ok") ("file" ,file) ("size" ,size)))))
      (else (:mime rc (json (object ("operation" "upload") ("status" "failed"))))))))

(get "/scm/upload" (lambda () (tpl->response upload-form)))


(define (->mac s)
  (cond
   ((not s) #f)
   (else
    (let* ((str (format #f "~12,'0X" s))
           (len (string-length str)))
      (let lp((i 0) (ret '()))
        (cond
         ((>= i len) (string-upcase (string-join (reverse ret) ":")))
         (else (lp (+ i 2) (cons (substring/shared str i (+ i 2)) ret)))))))))
(define *node-status* (make-hash-table))
(define *healthy-span* 30) ; 30 seconds
(get "/scm/heartbeat/:id/:ip/:timestamp" #:mime 'json
  (lambda (rc)
    (let* ((id (->mac (string->number (params rc "id"))))
           (ip (params rc "ip"))
           (new (string->number (params rc "timestamp")))
           (st (hash-ref *node-status* id))
           (old (if st (caar st) new)))
      (cond
       ((not id)
        (:mime rc (json (object ("operation" "heartbeat") ("status" "Invalid id")))))
       ((not new)
        (:mime rc (json (object ("operation" "heartbeat") ("status" "Invalid timestamp")))))
       (else
        (hash-set! *node-status* id (cons (cons old new) ip))
        (:mime rc (json (object ("operation" "heartbeat") ("status" "ok")))))))))

(define (anylize-node id st)
  (let* ((old (caar st))
         (new (cdar st))
         (ip (cdr st))
         (status (if (and (>= new old) (<= (- new old) *healthy-span*)) "live" "dead")))
    (list id ip status)))
(get "/scm/node/status" #:mime 'json
  (lambda (rc)
    (let ((sl (hash-map->list anylize-node *node-status*)))
      (cond
       ((null? sl) (:mime rc (json (object ("operation" "get_status") ("status" "null")))))
       (else
        (:mime rc (json (object
                         ("operation" "get_status")
                         ("status" "ok")
                         ("result" ,sl)))))))))

(run)
