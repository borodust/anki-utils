(cl:in-package :anki-utils)

;;;
;;; https://www.juliensobczak.com/write/2016/12/26/anki-scripting.html
;;; https://decks.fandom.com/wiki/Anki_APKG_format_documentation
;;;

(declaim (special *deck-table*
                  *model-table*
                  *note-table*
                  *config-table*))

(alexandria:define-constant +collection-schema+
    (alexandria:read-file-into-string
     (asdf:system-relative-pathname :anki-utils "src/schema.sql"))
  :test #'equal)


(alexandria:define-constant +field-separator+ (code-char #x1f)
  :test #'equal)


(defun unix-time ()
  (local-time:timestamp-to-unix (local-time:now)))


(defun alist (&rest args)
  (loop for (name value) on args by #'cddr
        collect (cons name value)))


(defun generate-anki-guid ()
  (format nil "~{~A~}"
          (loop repeat 10
                collect (code-char (+ 33 (random 91))))))


(defun anki-checksum (string)
  ;; fixme: probably better to ensure certain encoding
  (parse-number:parse-number
   (subseq (sha1:sha1-hex (babel:string-to-octets string :encoding :utf-8)) 0 8) :radix 16))


(defmethod json:encode-json ((symbol (eql 'true)) &optional stream)
  (format stream "true"))


(defmethod json:encode-json ((symbol (eql 'false)) &optional stream)
  (format stream "false"))


(defun decode-json-boolean (value)
  (if (string= value "true")
      'true
      'false))


(defmacro with-temporary-directory ((tmp-dir) &body body)
  (alexandria:with-gensyms (tmp-file)
    `(uiop:with-temporary-file (:pathname ,tmp-file)
       (let ((,tmp-dir (uiop:ensure-directory-pathname
                        (format nil "~A.dir/" (uiop:native-namestring ,tmp-file)))))
         (unwind-protect
              (progn ,@body)
           (uiop:delete-directory-tree ,tmp-dir :validate (constantly t)))))))


(defmacro with-alist ((&rest keys) alist &body body)
  `(destructuring-bind (&key ,@keys &allow-other-keys) (alexandria:alist-plist ,alist)
     ,@body))


(defgeneric to-alist (object))

(defgeneric from-alist (class alist))

(defgeneric to-sql (object))

(defgeneric from-sql (class executor))

(defclass identified ()
  ((id :initarg :id :initform (error ":id missing") :reader id-of)))


(defclass named ()
  ((name :initarg :name :initform (error ":name missing") :reader name-of)))


(defclass modifiable ()
  ((last-modified :initarg :last-modified :initform (local-time:now) :reader last-modified-time-of)))


;;;
;;; REVLOG
;;;
(defclass anki-revlog () ())

;;;
;;; MODEL
;;;
(defun find-model (id)
  (gethash id *model-table*))


(defun register-model (id model)
  (setf (gethash id *model-table*) model))


(defclass anki-model (identified named)
  ((data :initarg :data :initform nil)))


(defgeneric sort-field-of (anki-model)
  (:method ((this anki-model))
    (with-slots (data) this
      (let ((idx (alexandria:assoc-value data :|sortf|)))
        (cond
          ((numberp idx) idx)
          ((stringp idx) (parse-integer idx))
          (t 0))))))


(defmethod to-alist ((this anki-model))
  (with-slots (data) this
    data))


(defmethod from-alist ((class (eql 'anki-model)) alist)
  (with-alist (|id| |name|) alist
    (make-instance 'anki-model :id |id|
                               :name |name|
                               :data alist)))


;;;
;;; DECK CONFIG
;;;
(defclass anki-deck-config (identified named)
  ((data :initarg :data :initform nil)))


(defun register-config (id config)
  (setf (gethash id *config-table*) config))


(defun find-config (id)
  (gethash id *config-table*))


(defmethod to-alist ((this anki-deck-config))
  (with-slots (data) this
    (if data
        data
        (alist :|id| (id-of this)
               :|name| (name-of this)
               :|replayq| 'true
               :|lapse| (alist :|leechFails| 8
                               :|minInt| 1
                               :|delays| '(10)
                               :|leechAction| 0
                               :|mult|  0)
               :|rev| (alist :|perDay| 100
                             :|fuzz|  0.05
                             :|ivlFct| 1
                             :|maxIvl| 36500
                             :|ease4| 1.3
                             :|bury| 'true
                             :|minSpace| 1)
               :|timer| 0
               :|maxTaken| 60
               :|usn| 0
               :|new| (alist :|perDay| 20
                             :|delays| '(1 10)
                             :|separate| 'true
                             :|ints| '(1 4 7)
                             :|initialFactor| 2500
                             :|bury| 'true
                             :|order| 1)
               :|mod| 0
               :|autoplay| 'true))))


(defmethod from-alist ((class (eql 'anki-deck-config)) alist)
  (with-alist (|id| |name|) alist
    (make-instance 'anki-deck-config
                   :id |id|
                   :name |name|
                   :data alist)))

;;;
;;; DECK
;;;
(defclass anki-deck (identified named modifiable)
  ((description :initarg :description :initform (error ":description missing") :reader description-of)
   (card-map :initform (make-hash-table))
   (config :initarg :config :initform nil :reader config-of)))


(defmethod initialize-instance :after ((this anki-deck) &key)
  (with-slots (config) this
    (unless config
      (setf config (make-instance 'anki-deck-config :id (id-of this)
                                                    :name (name-of this))))))


(defmethod to-alist ((this anki-deck))
  (alist
   :|id| (id-of this)
   :|name| (name-of this)
   :|desc| (description-of this)
   :|usn| 0
   :|mod| (local-time:timestamp-to-unix (last-modified-time-of this))
   :|conf| (id-of this)

   :|extendRev| 50
   :|collapsed| 'false
   :|browserCollapsed| 'true
   :|newToday| '(0 0)
   :|timeToday| '(0 0)
   :|dyn| 0
   :|extendNew| 10
   :|revToday| '(0 0)
   :|lrnToday| '(0 0)))


(defmethod from-alist ((class (eql 'anki-deck)) alist)
  (with-alist (|id| |name| |desc| |mod|) alist
    (make-instance 'anki-deck
                   :id |id|
                   :name |name|
                   :last-modified (local-time:unix-to-timestamp (or |mod| 0))
                   :description |desc|
                   :config (find-config |id|))))


(defun register-deck (id deck)
  (setf (gethash id *deck-table*) deck))


(defun find-deck (id)
  (gethash id *deck-table*))


(defun register-card (deck card-id card)
  (with-slots (card-map) deck
    (setf (gethash card-id card-map) card)))


(defmacro docards ((card deck) &body body)
  `(with-slots (card-map) ,deck
     (loop for ,card being the hash-value of card-map
           do (progn ,@body))))


;;;
;;; NOTE
;;;
(defclass anki-note (identified modifiable)
  ((guid :initarg :guid :initform (generate-anki-guid) :reader guid-of)
   (model :initarg :model :initform (error ":model missing") :reader model-of)
   (tags :initarg :tags :initform nil :reader tags-of)
   (fields :initarg :fields :initform nil :reader fields-of)))


(defun register-note (id note)
  (setf (gethash id *note-table*) note))


(defun find-note (id)
  (gethash id *note-table*))


(defun copy-anki-note (source &key id (tags nil tags-provided-p))
  (make-instance 'anki-note
                 :id (or id (id-of source))
                 :last-modified (last-modified-time-of source)
                 :model (model-of source)
                 :tags (if tags-provided-p tags (tags-of source))
                 :fields (fields-of source)))


(defmethod to-sql ((this anki-note))
  (let ((sort-field-value (nth (sort-field-of (model-of this)) (fields-of this))))
    (list
     (list "INSERT INTO notes (id,guid,mid,mod,usn,tags,flds,sfld,csum,flags,data)
VALUES (?,?,?,?,?,?,?,?,?,?,?)"
           (id-of this)
           (guid-of this)
           (id-of (model-of this))
           (local-time:timestamp-to-unix (last-modified-time-of this))
           0
           (if (tags-of this) (format nil " ~{~A ~}" (tags-of this)) "")
           (format nil (format nil "~~{~~A~~^~A~~}" (code-char #x1f)) (fields-of this))
           sort-field-value
           (anki-checksum sort-field-value)
           0 ""))))


(defmethod from-sql ((class (eql 'anki-note)) executor)
  (let ((values (funcall executor "SELECT
id,guid,mid,mod,tags,flds
FROM notes")))
    (loop for (id guid mid mod tags flds) in values
          do (register-note id
                            (make-instance 'anki-note
                                           :id id
                                           :guid guid
                                           :model (find-model mid)
                                           :tags (remove-if #'alexandria:emptyp
                                                            (split-sequence:split-sequence #\Space tags))
                                           :fields (split-sequence:split-sequence +field-separator+
                                                                                  flds)
                                           :last-modified (local-time:unix-to-timestamp (or mod 0)))))))

;;;
;;; CARD
;;;
(defclass anki-card (identified modifiable)
  ((note :initarg :note :initform (error ":note missing") :reader note-of)
   (deck :initarg :deck :initform (error ":deck missing") :reader deck-of)
   (ordinal :initarg :ordinal :initform 0 :reader ordinal-of)))


(defmethod to-sql ((this anki-card))
  (list
   (list "INSERT INTO cards (id,nid,did,ord,mod,usn,type,queue,due,ivl,factor,reps,lapses,left,odue,odid,flags,data)
VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
         (id-of this)
         (id-of (note-of this))
         (id-of (deck-of this))
         (ordinal-of this)
         (local-time:timestamp-to-unix (last-modified-time-of this))
         0 0 0 0 0 0 0 0 0 0 0 0 "")))



(defmethod from-sql ((class (eql 'anki-card)) executor)
  (let ((values (funcall executor "SELECT
id,nid,did,mod,ord
FROM cards")))
    (loop for (id nid did mod ord) in values
          do (alexandria:if-let ((deck (find-deck did)))
               (register-card deck id
                              (make-instance 'anki-card
                                             :id id
                                             :note (find-note nid)
                                             :deck deck
                                             :ordinal ord
                                             :last-modified (local-time:unix-to-timestamp (or mod 0))))
               (warn "Deck ~A not found for card ~A. Ignoring" did id)))))

;;;
;;; COLLECTION
;;;
(defclass anki-collection (identified modifiable)
  ((decks :initarg :decks :initform nil :reader decks-of)
   (models :initarg :models :initform nil :reader models-of)
   (created-at :initarg :created :initform (local-time:now) :reader created-time-of)))


(defun encode-default-collection-config ()
  (json:encode-json-to-string
   (alist :|nextPos| 1
          :|estTimes| 'true
          :|activeDecks| '(1)
          :|sortType| "noteFld"
          :|timeLim| 0
          :|sortBackwards| 'false
          :|addToCur| 'true
          :|curDeck| 1
          :|newBury| 'true
          :|newSpread| 0
          :|dueCounts| 'true
          :|curModel| "1"
          :|collapseTime| 1200)))


(defmethod to-sql ((this anki-collection))
  (let ((json:*lisp-identifier-name-to-json* (lambda (identifier) (string identifier)))
        (unix-time-now (unix-time))
        (model-table (make-hash-table :test 'equal))
        (config-table (make-hash-table :test 'equal))
        (deck-table (make-hash-table :test 'equal))
        (card-table (make-hash-table :test 'equal))
        (note-table (make-hash-table :test 'equal))
        sql)
    (loop for deck in (decks-of this)
          do (setf (gethash (id-of deck) deck-table) (to-alist deck)
                   (gethash (id-of deck) config-table) (to-alist (config-of deck)))
             (docards (card deck)
               (let* ((note (note-of card))
                      (model (model-of note)))
                 (setf (gethash (id-of card) card-table) card
                       (gethash (id-of note) note-table) note)
                 (unless (gethash (id-of model) model-table)
                   (setf (gethash (id-of model) model-table) (to-alist model))))))
    (loop for note being the hash-value of note-table
          do (setf sql (nconc (to-sql note) sql)))
    (loop for card being the hash-value of card-table
          do (setf sql (nconc (to-sql card) sql)))
    (list*
     (list "INSERT INTO col (id,crt,mod,scm,ver,dty,usn,ls,conf,models,decks,dconf,tags)
VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"
           1
           unix-time-now unix-time-now unix-time-now 11
           0 0 0
           (encode-default-collection-config)
           (json:encode-json-to-string model-table)
           (json:encode-json-to-string deck-table)
           (json:encode-json-to-string config-table)
           "{}")
     sql)))


(defmethod from-sql ((class (eql 'anki-collection)) executor)
  (let ((json:*json-identifier-name-to-lisp* #'identity)
        (json:*boolean-handler* #'decode-json-boolean)
        (*model-table* (make-hash-table))
        (*deck-table* (make-hash-table))
        (*note-table* (make-hash-table))
        (*config-table* (make-hash-table))
        (values (funcall executor "SELECT
id,crt,mod,scm,ver,dty,usn,ls,conf,models,decks,dconf,tags
FROM col")))
    (labels ((parse-id (id)
               (parse-integer (symbol-name id)))
             (parse-encoded-object-map (class map registerer)
               (loop for (encoded-id . object) in (json:decode-json-from-string map)
                     for id = (parse-id encoded-id)
                     for parsed = (from-alist class object)
                     do (funcall registerer id parsed)
                     collect parsed into objects
                     finally (return objects)))
             (parse-models (models)
               (parse-encoded-object-map 'anki-model models #'register-model))
             (parse-configs (models)
               (parse-encoded-object-map 'anki-deck-config models #'register-config))
             (parse-decks (decks)
               (parse-encoded-object-map 'anki-deck decks #'register-deck)))
      (prog1
          (loop for (id crt mod scm ver dty usn ls conf models decks dconf tags) in values
                collect (progn
                          (parse-configs dconf)
                          (make-instance 'anki-collection
                                         :id id
                                         :decks (parse-decks decks)
                                         :models (parse-models models)
                                         :created (local-time:unix-to-timestamp (or crt 0))
                                         :last-modified (local-time:unix-to-timestamp (or mod 0)))))
        (from-sql 'anki-note executor)
        (from-sql 'anki-card executor)))))


(defun save-anki-collections (collections path)
  (with-temporary-directory (tmp-dir)
    (let* ((database-path (merge-pathnames "collection.anki2" tmp-dir))
           (media-path (merge-pathnames "media" tmp-dir)))
      (ensure-directories-exist tmp-dir)
      (sqlite:with-open-database (db database-path)
        (sqlite:execute-multiple-non-query db +collection-schema+)
        (loop for collection in collections
              do (loop for (sql . params) in (to-sql collection)
                       do (apply #'sqlite:execute-non-query db sql params))))
      (alexandria:write-string-into-file "{}" media-path)
      (zip:zip path tmp-dir))))



(defun load-anki-collections (path)
  (with-temporary-directory (tmp-dir)
    (zip:unzip path tmp-dir)
    (sqlite:with-open-database (db (merge-pathnames "collection.anki2" tmp-dir))
      (flet ((%executor (query)
               (sqlite:execute-to-list db query)))
        (from-sql 'anki-collection #'%executor)))))


(defun make-anki-collection-from-deck (deck)
  (let ((model-table (make-hash-table))
        (now (local-time:now)))
    (docards (card deck)
      (let ((model (model-of (note-of card))))
        (unless (gethash (id-of model) model-table)
          (setf (gethash (id-of model) model-table) model))))
    (make-instance 'anki-collection
                   :id 1
                   :decks (list deck)
                   :models (loop for model being the hash-value of model-table
                                 collect model)
                   :created now
                   :last-modified now)))

;;;
;;;
;;;
(defun all-notes (collection)
  (loop with note-map = (make-hash-table)
        for col in collection
        do (loop for deck in (decks-of col)
                 do (docards (card deck)
                      (let ((note (note-of card)))
                        (setf (gethash (id-of note) note-map) note))))
        finally (return (loop for value being the hash-value of note-map
                              collect value))))


(defun find-notes-by-tags (collection &rest tags)
  (loop for note in (all-notes collection)
        when (intersection tags (tags-of note) :test #'string=)
          collect note))


(defun make-note-table-by-primary-field (collection)
  (loop with table = (make-hash-table :test 'equal)
        for note in (all-notes collection)
        do (setf (gethash (first (fields-of note)) table) note)
        finally (return table)))
