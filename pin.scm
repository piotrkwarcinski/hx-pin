(require (prefix-in hlx.static. "helix/static.scm"))
(require (prefix-in hlx.misc. "helix/misc.scm"))
(require (prefix-in hlx.editor. "helix/editor.scm"))
(require (prefix-in hlx.cmd. "helix/commands.scm"))
(require (prefix-in hlx.components. "helix/components.scm"))
(require (prefix-in selector. "selector.scm"))

(define BASE-DIR-NAME "~/.cache/helix")
(define PIN-DIR-NAME "hx-pin")
(define PIN-FILE-NAME "pin.json")

(define PIN-DIR (string-append (canonicalize-path BASE-DIR-NAME) "/" PIN-DIR-NAME))
(define PIN-PATH (string-append PIN-DIR "/" PIN-FILE-NAME))

(struct Pin (path row col) #:transparent)

(define (current-path)
  (let* ([focus (hlx.editor.editor-focus)]
         [focus-doc-id (hlx.editor.editor->doc-id focus)])
    (hlx.editor.editor-document->path focus-doc-id)))

(define (hash->struct struct-descriptor struct-constructor json)
    (define fields (hash-get struct-descriptor '#:fields))
    (apply struct-constructor (map (λ (field) (hash-try-get json field)) fields)))

(define (zip-into-hash ls lso)
    (if (null? ls)
        (hash)
        (hash-insert (zip-into-hash (cdr ls) (cdr lso)) (car ls) (car lso))))

(define (struct->hash struct-descriptor value)
    (define fields (hash-get struct-descriptor '#:fields))
    (define values (struct->list value))
    (zip-into-hash fields values))

(define (json->pins json)
  (transduce
    json
    (mapping (λ (entry) `(,
                     (first entry) ,
                     (map (λ (pin) (hash->struct ___Pin-options___ Pin pin)) (second entry)))))
    (into-hashmap)))

(define (pins->json pins)
  (transduce
    pins
    (mapping (λ (entry) `(,
                     (first entry) ,
                     (map (λ (pin) (struct->hash ___Pin-options___ pin)) (second entry)))))
    (into-hashmap)))

(define (read-pins-json)
  (unless (path-exists? PIN-DIR)
    (create-directory! PIN-DIR))

  (define input-str (cond
    ;; We're just storing these as strings with the quotes still there, so that we
    ;; can call `read` on them accordingly
    [(path-exists? PIN-PATH) (~> (open-input-file PIN-PATH) (read-port-to-string))]
    [else "{}"]))

  (string->jsexpr input-str))

;;@doc
;; A map of projectPath->[]Pin.
(define *pins* (json->pins (read-pins-json)))

(define (helix-picker! pick-list)
  (hlx.misc.push-component! (picker pick-list)))

(define (remove-duplicates lst)
  ;; Iterate over, grabbing each value, check if its in the hash, otherwise skip it
  (define (remove-duplicates-via-hash lst accum set)
    (cond
      [(null? lst) accum]
      [else
       (let ([elem (car lst)])
         (if (hashset-contains? set elem)
             (remove-duplicates-via-hash (cdr lst) accum set)
             (remove-duplicates-via-hash (cdr lst) (cons elem accum) (hashset-insert set elem))))]))

  (reverse (remove-duplicates-via-hash lst '() (hashset))))

(define (merge-hashes h1 h2)
  (define result (hash))
  (map (λ (entry) (set! result (hash-insert result (car entry) (second entry)))) h1)
  (map (λ (entry) (set! result (hash-insert result (car entry) (second entry)))) h2)
  result)

(define (flush-pins pins)
  (define file-pins (read-pins-json))
  (define json (pins->json pins))
  (define merged (merge-hashes file-pins json))
  (define json-str (value->jsexpr-string merged))
  (if (path-exists? PIN-PATH)
    (delete-file! PIN-PATH))
  (let ([output-file (open-output-file PIN-PATH)])
             (write-string json-str output-file)
             (flush-output-port output-file)
             (close-output-port output-file)))

(define (current-row)
  (hlx.static.get-current-line-number))

(define (current-col)
  (define current-position (car (hlx.editor.current-cursor)))
  ;; Probably because of spacing the first column starts at 7.
  (- (hlx.components.position-col current-position) 7))

(define (hash-get-or-else hm key value)
    (define found (hash-try-get hm key))
    (if found found value))

(define cwd hlx.static.get-helix-cwd)

(define (cwd-key)
  (string->symbol (cwd)))

(define (pin-get-list)
  (hash-get-or-else *pins* (cwd-key) (list)))

(define (get-pin-with-path path)
  (define pins (pin-get-list))
  ;; it's rather impossible to not get a result here, thus car
  (car (filter (λ (pin) (equal? (Pin-path pin) path)) pins)))

(define (move-right times)
  (when (> times 0)
    (hlx.static.move_char_right)
    (move-right (- times 1))))

(define (jump-to-position)
  (define pin (get-pin-with-path (current-path)))
  ;; ~> servers as "threading macro"
  ;; it passes the result of consecutive calls to
  ;; subsequent functions. It allows for a cleaner
  ;; way of showing data flow.
  (~> (Pin-row pin)
      (number->string)
      (hlx.cmd.goto))
  (hlx.static.align_view_center)
  (move-right (Pin-col pin)))

(define (helix-exp-picker! pick-list)
  (hlx.misc.push-component! (#%exp-picker pick-list jump-to-position)))

(define (to-subpaths paths)
  (transduce
     paths
     (mapping (λ (path) (string-replace path (cwd) "")))
     (mapping (λ (subpath) (substring subpath 1 (string-length subpath))))
     (into-list)))

 (define (expand-subpath subpath)
   (string-append (cwd) "/" subpath))

 (define (expand-subpaths paths)
  (transduce
     paths
     (mapping expand-subpath)
     (into-list)))

 (define (all-open-files)
  (map hlx.editor.editor-document->path (hlx.editor.editor-all-documents)))

; @doc
; Check if path is already opened..
; Returns documentID if opened, otherwise #false.
(define (path-opened path)
  (define document (transduce
    (hlx.editor.editor-all-documents)
    (filtering (λ (doc) (equal? (hlx.editor.editor-document->path doc) path)))
    ; (mapping (λ (doc) (begin (simple-displayln doc) doc)))
    (into-list)))
  (if (null? document)
      #false
      (car document)))

;;@doc
;; Add current file to pinned files.
(define (pin-add)
  (define pin (Pin (current-path) (current-row) (current-col)))
  (define got (pin-get-list))
  (set! got (filter (λ (p) (not (equal? (Pin-path p) (Pin-path pin)))) got))
  ;; append to the end of the list
  (set! *pins* (hash-insert *pins* (cwd-key) (append got (list pin))))
  (flush-pins *pins*))

;;@doc
;; Remove file from pinned files.
(define (pin-remove path)
  (define got (pin-get-list))
  (if (not (empty? got))
    (begin
      (define current (filter (λ (p) (not (equal? (Pin-path p) path))) got))
      (set! *pins* (hash-insert *pins* (cwd-key) current))
      (flush-pins *pins*)
      (map (λ (p) (Pin-path p)) current))
    '()))

(define (list-swap ls src dst)
  (define (s new-ls i)
    ; (dbg! new-ls)
    ; (dbg! i)
    (cond
      [(< i 0) new-ls]
      [(= i src) (s (cons (list-ref ls dst) new-ls) (- i 1))]
      [(= i dst) (s (cons (list-ref ls src) new-ls) (- i 1))]
      [else (s (cons (list-ref ls i) new-ls) (- i 1))]))
  (s (list) (- (length ls) 1)))

(define (list-find ls el from)
  (cond
    [(null? ls) #false]
    [(equal? (car ls) el) from]
    [else (list-find (cdr ls) el (+ from 1))]))

;;@doc
;; Open pinned files picker.
(define (pin-open)
  (helix-exp-picker!
    (transduce
       (pin-get-list)
       (mapping (λ (pin) (Pin-path pin)))
       (into-list))))

;; @doc
;; Go to a pinned file by its index.
(define (pin-goto num)
  (if (string? num)
    (set! num (string->number num)))
  (define cwd (hlx.static.get-helix-cwd))
  (let* ([paths (map Pin-path (pin-get-list))]
         [subpaths (to-subpaths paths)]
         [subpath (list-ref subpaths num)]
         [path (list-ref paths num)])
  (define doc (path-opened path))
  (if doc
    (hlx.editor.editor-switch-action! doc (hlx.editor.Action/Replace))
    (begin
      (hlx.cmd.open subpath)
      (jump-to-position)))))

(define (selector-open subpath _ _)
  (define path (expand-subpath subpath))
  (define doc (path-opened path))
  (if doc
    (hlx.editor.editor-switch-action! doc (hlx.editor.Action/Replace))
    (begin
      (hlx.cmd.open subpath)
      (jump-to-position))))

(define (selector-open-vertical subpath _ _)
  (define path (expand-subpath subpath))
  (define doc (path-opened path))
  (if doc
    (hlx.editor.editor-switch-action! doc (hlx.editor.Action/VerticalSplit))
    (begin
      (hlx.cmd.open subpath)
      (jump-to-position))))

(define (selector-remove path items _)
  (define updated (pin-remove (expand-subpath path)))
  (set-box! items (to-subpaths updated)))

(define (selector-order-up subpath items cursor times)
  (define path (expand-subpath subpath))
  (define pins (pin-get-list))
  (define pin (car (filter (λ (p) (equal? path (Pin-path p))) pins)))
  (define current-idx (list-find pins pin 0))
  (define new-idx (+ current-idx times))
  (if (and (>= new-idx 0) (< new-idx (length pins)))
      (begin
        (define swapped (list-swap pins current-idx new-idx))
        (set! *pins* (hash-insert *pins* (cwd-key) swapped))
        (set-box! cursor new-idx)
        (set-box! items (to-subpaths (map Pin-path swapped)))
        (flush-pins *pins*))))

;; @doc
;; Open pinned files selector.
(define (pin-open-selector)
  (define cwd (hlx.static.get-helix-cwd))
  (define callbacks (hash
    'enter selector-open
    'v selector-open-vertical
    'j selector-open
    'k selector-open
    'D selector-remove
    (string->symbol "(") (λ (path items cursor) (selector-order-up path items cursor -1))
    (string->symbol ")") (λ (path items cursor) (selector-order-up path items cursor 1))
  ))
  (let* ([paths (map Pin-path (pin-get-list))]
         [subpaths (to-subpaths paths)]
         [index (list-find paths (current-path) 0)]
         [start-index (if index index 0)])
    (dbg! paths)
    (dbg! subpaths)
    (dbg! index)
    (dbg! start-index)
    (hlx.misc.push-component!
      (selector.new-selector
        subpaths
        callbacks
        start-index))))

(define (pin-get-others)
  (define pinned (map Pin-path (pin-get-list)))
  (define all (all-open-files))
  (filter (λ (x) x)
           (hashset->list (hashset-difference (list->hashset pinned)
                                              (list->hashset all)))))

 ;;@doc
;; Close all other files except pinned ones.
(define (pin-close-others)
  (map hlx.cmd.buffer-close (pin-get-others)))

 ;;@doc
;; Close all files and open pinned files.
;; Together with ordering of pinned files this allows to modify
;; the order of helix buffers (as long as they are pinned).
(define (pin-refresh)
  (hlx.cmd.buffer-close-all)
  (pin-open-pinned)
  (pin-goto 0))

(define (pin-open-config)
   (hlx.cmd.open PIN-PATH))

;;@doc
;; Open pinned files.
(define (pin-open-pinned)
  (define pinned (pin-get-list))
  (for-each
    (λ (pin) (begin
      (hlx.cmd.open (Pin-path pin))
      (jump-to-position)
      )) pinned)
  (when (> (length pinned) 0)
    (pin-goto 0)))

(provide pin-add)
(provide pin-open)
(provide pin-open-pinned)
(provide pin-close-others)
(provide pin-open-config)
(provide pin-open-selector)
(provide pin-refresh)
(provide pin-goto)
