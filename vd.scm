#!/usr/bin/env guile
!#

(setlocale LC_ALL "zh_CN.utf-8")
(use-modules (artanis artanis) (artanis utils) (srfi srfi-1)
             (ice-9 ftw) (ice-9 match) (ice-9 format))
(init-server)

;; TODO: use DB instead
(define *node-table* (make-hash-table))
(define *video-queue* (new-queue))
(define *notice-table* (make-hash-table))
(define notice-time 5)
(define Q_MAX 6)

(define upload-form
  '(form (@ (method "POST") (enctype "multipart/form-data") (action "scm/upload"))
         "File to upload: " (input (@ (type "file") (name "upfile"))) (br)
         "Multicase notification: " (input (@ (type "checkbox") (name "notify"))) (br)
         "Notice: " (input (@ (type "text") (name "note"))) (br)
         "Seconds: " (input (@ (type "number") (name "seconds") (value "5"))) (br)
         (br) (br)
         (input (@ (type "submit") (value "Press")) "to upload the file!")))

(define (get-uploaded-files)
  (scandir "upload"
           (lambda (s)
             (and (not (or (string=? s ".") (string=? s "..")))
                  (basename s)))))

(get "/scm/video/check/:id" #:mime 'json
  (lambda (rc)
    (let ((id (params rc "id")))
      (cond
       ((hash-ref *node-table* id)
        => (lambda (files)
             (let* ((ql (get-uploaded-files))
                    (fs (lset-difference string=? ql files)))
               (format #t "upload: ~{~a~^ ~}~%~%" ql)
               (:mime rc (json (object ("operation" "check")
                                       ("status" "update")
                                       ("new-video" ,fs)))))))
       (else (:mime rc (json (object ("operation" "check") ("status" "no")))))))))

(get "/scm/node/register/:id" #:mime 'json
  (lambda (rc)
    (cond
     ((hash-ref *node-table* (params rc "id"))
      (:mime rc (json (object ("operation" "register") ("status" "exists")))))
     (else
      (hash-set! *node-table* (params rc "id") '())
      (:mime rc (json (object ("operation" "register") ("status" "ok"))))))))

;; NOTE: client should call update after all files are downloaded!
(get "/scm/video/update/:id/:file" #:mime 'json
  (lambda (rc)
    (let ((id (params rc "id"))
          (file (params rc "file"))
          (ql (queue->list *video-queue*)))
      (hash-set! *node-table* id (cons id ql)) ; update existed files
      (:mime rc (json (object ("operation" "update") ("status" "ok" )))))))

(define (generate-note-pic note)
  (system "rm -f notice.jpg")
  (system (format #f "convert -font ./SourceHanSansCN-Regular.otf -pointsize 20 -fill yellow -draw 'text 270,160 ~s ' spacer.jpg notice.jpg" note)))
(define (deal-with-notification rc mfds)
  (let ((has-notify? (find-mfd rc "notify" mfds))
        (note (find-mfd rc "note" mfds))
        (seconds (find-mfd rc "seconds" mfds)))
    (cond
     ((not has-notify?)
      (format #t "There's no any notification!~%"))
     (else
      (generate-note-pic note)
      ;; Regenerate notice table for each node
      (hash-for-each (lambda (k v) (hash-set! *notice-table* k #t)) *node-status*)
      (set! notice-time seconds)))))
(define (upload-stat mfds sl fl)
  (cond
   ((and (not (null? sl)) (not (null? fl)))
    (list mfds (car fl) (car sl)))
   (else (list mfds "null" 0))))
(post "/scm/upload"
  #:from-post `(store #:path "upload" #:mode #o664 #:success-ret ,upload-stat #:simple-ret? #f #:need-mfd? #t)
  #:mime 'json
  (lambda (rc)
    (when (> (queue-length *video-queue*) Q_MAX)
          (queue-out! *video-queue*))
    (match (:from-post rc 'store)
      ((mfds file size)
       (deal-with-notification rc mfds)
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
(define *healthy-span* 35) ; little larger than 30 seconds
(get "/scm/heartbeat/:id/:ip/:timestamp" #:mime 'json
  (lambda (rc)
    (let* ((id (params rc "id"))
           (ip (params rc "ip"))
           (timestamp (string->number (params rc "timestamp"))))
      (cond
       ((not id)
        (:mime rc (json (object ("operation" "heartbeat") ("status" "Invalid id")))))
       ((not timestamp)
        (:mime rc (json (object ("operation" "heartbeat") ("status" "Invalid timestamp")))))
       (else
        (hash-set! *node-status* id (cons timestamp ip))
        (:mime rc (json (object ("operation" "heartbeat") ("status" "ok")))))))))

(define (anylize-node id st)
  (let* ((timestamp (car st))
         (ip (cdr st))
         (status (if (and (>= timestamp 0) (<= (- timestamp (current-time)) *healthy-span*)) "live" "dead")))
    (list (->mac (string->number id)) ip status)))
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

(get "/scm/notice/check/:id" #:mime 'json
  (lambda (rc)
    (let ((id (params rc "id")))
      (cond
       ((hash-ref *notice-table* id)
        (:mime rc (json (object ("operation" "notice_check") ("status" "ok") ("seconds" ,notice-time)))))
       (else (:mime rc (json (object ("operation" "notice_check") ("status" "no")))))))))

(get "/scm/notice/clean/:id" #:mime 'json
  (lambda (rc)
    (hash-remove! *notice-table* (params rc "id"))
    (:mime rc (json (object ("operation" "notice_clean") ("status" "ok"))))))

(run)
