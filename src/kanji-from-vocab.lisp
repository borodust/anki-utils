(cl:in-package :anki-utils)

(alexandria:define-constant +kana-table+
    (let* ((kana (alexandria:plist-hash-table
                  '(#\ア #\あ #\イ #\い #\ウ #\う #\エ #\え #\オ #\お
                    #\カ #\か #\キ #\き #\ク #\く #\ケ #\け #\コ #\こ
                    #\サ #\さ #\シ #\し #\ス #\す #\セ #\せ #\ソ #\そ
                    #\タ #\た #\チ #\ち #\ツ #\つ #\テ #\て #\ト #\と
                    #\ナ #\な #\ニ #\に #\ヌ #\ぬ #\ネ #\ね #\ノ #\の
                    #\ハ #\は #\ヒ #\ひ #\フ #\ふ #\ヘ #\へ #\ホ #\ほ
                    #\マ #\ま #\ミ #\み #\ム #\む #\メ #\め #\モ #\も
                    #\ヤ #\や #\ユ #\ゆ #\ヨ #\よ
                    #\ラ #\ら #\リ #\り #\ル #\る #\レ #\れ #\ロ #\ろ
                    #\ワ #\わ #\ヲ #\を
                    #\ン #\ん
                    #\ャ #\ゃ #\ュ #\ゅ #\ョ #\ょ
                    #\ッ #\っ

                    #\ガ #\が #\ギ #\ぎ #\グ #\ぐ #\ゲ #\げ #\ゴ #\ご
                    #\ザ #\ざ #\ジ #\じ #\ズ #\ず #\ゼ #\ぜ #\ゾ #\ぞ
                    #\ダ #\だ #\ヂ #\ぢ #\ヅ #\づ #\デ #\で #\ド #\ど
                    #\バ #\ば #\ビ #\び #\ブ #\ぶ #\ベ #\べ #\ボ #\ぼ
                    #\パ #\ぱ #\ピ #\ぴ #\プ #\ぷ #\ペ #\ぺ #\ポ #\ぽ)
                  :test #'equal)))
      (loop for (k . v) in (loop for value being the hash-value of kana using (hash-key key)
                                 collect (cons value key))
            do (setf (gethash k kana) v))
      kana)
  :test #'equalp)


(defun extract-kanji (deck-path &rest tags)
  (let* ((collection (load-anki-collections deck-path))
         (notes (apply #'find-notes-by-tags collection tags)))
    (flet ((from-kana-p (l) (gethash l +kana-table+)))
      (remove-if #'from-kana-p (remove-duplicates
                                (with-output-to-string (out)
                                  (loop for note in notes
                                        do (format out "~A" (first (fields-of note))))))))))


(defun create-kanji-collection (kanji-string name tags source-path target-path)
  (let* ((source-kanji-collection (load-anki-collections source-path))
         (kanji-note-table (make-note-table-by-primary-field source-kanji-collection))
         (now (local-time:now))
         (starting-id (+ (* (local-time:timestamp-to-unix now) 1000)
                         (local-time:timestamp-millisecond now)))
         (deck (make-instance 'anki-deck
                              :id starting-id
                              :name name
                              :description "")))
    (flet ((%reg-card (card)
             (register-card deck (id-of card) card)))
      (loop with card-id = starting-id
            for kanji across kanji-string
            for note-id from starting-id
            for source-note = (gethash (format nil "~A" kanji) kanji-note-table)
            for note = (when source-note
                         (copy-anki-note source-note
                                         :id note-id
                                         :tags tags))
            if note
              do (%reg-card (make-instance 'anki-card
                                           :id (incf card-id)
                                           :deck deck
                                           :note note
                                           :ordinal 0))
                 (%reg-card (make-instance 'anki-card
                                           :id (incf card-id)
                                           :deck deck
                                           :note note
                                           :ordinal 1))
            else
              do (warn "Note for kanji ~A not found. Skipping" kanji)))
    (save-anki-collections (list (make-anki-collection-from-deck deck)) target-path)))


(defun create-kanji-collection-from-vocab (name tags target-path vocab-path kanji-path)
  (let ((kanji-string (apply #'extract-kanji vocab-path tags)))
    (create-kanji-collection kanji-string name tags kanji-path target-path)))
