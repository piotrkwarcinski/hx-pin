(require "helix/configuration.scm")
(require "helix/misc.scm")

(require-builtin helix/components)
(require (prefix-in helix. "helix/commands.scm"))

(require "helix/editor.scm")
(require (prefix-in helix.static. "helix/static.scm"))

(struct Selector (items
        ;; A map of key => callback.
        callback
        ;; The navigable cursor within items.
        cursor
        max-length
        ;; Where the window will start in the list
        window-start
        ;; Currently unused.
        cursor-position))


(define (for-each-index func lst index)
  (if (null? lst)
      void
      (begin
        (func index (car lst))
        (when (null? lst)
          (return! void))
        (for-each-index func (cdr lst) (+ index 1)))))

(define (selector-render state rect frame)
  ; ;; Snag the cursor position, mapped to an index within the list
  (define cursor-position (unbox (Selector-cursor state)))

  ;; Calculate the block area in terms of the parent
  (define half-parent-width (round (/ (area-width rect) 4)))
  (define half-parent-height (round (/ (area-height rect) 4)))

  (define starting-x-offset (round (/ (area-width rect) 4)))
  (define starting-y-offset (round (/ (area-height rect) 4)))

  ;; Draw a preview area on the right
  (define block-area
    (area starting-x-offset
          (- starting-y-offset 1)
          half-parent-width
          half-parent-height))

  (define x (+ 1 (area-x block-area)))
  (define y (area-y block-area))
  (buffer/clear frame block-area)

  (block/render frame
                (area (- (area-x block-area) 1)
                      (- (area-y block-area) 1)
                      (+ 2 (area-width block-area))
                      (+ 2 (area-height block-area)))
                (make-block (theme->bg *helix.cx*) (theme->bg *helix.cx*) "all" "plain"))


  (define items (unbox (Selector-items state)))

  (for-each-index
    (λ (index row)
       (when (< (+ index y 1) (+ (area-y rect) (area-height rect)))
             (if (equal? index cursor-position)
               (frame-set-string! frame
                                  x
                                  (+ index y 1)
                                  row
                                  (style-bg (style) Color/Gray))
               (frame-set-string! frame
                                  x
                                  (+ index y 1)
                                  row
                                  (theme->bg *helix.cx*))))) items 0))


(define (move-cursor-down selector)
  ;; Current element that we're on
  (define current (Selector-cursor selector))
  (define window-start (Selector-window-start selector))

  (set-box! current (modulo (+ 1 (unbox current)) (length (unbox (Selector-items selector)))))

  (when (> (unbox current) (+ (unbox window-start) (- (unbox (Selector-max-length selector)) 2)))
    (set-box! window-start (+ (unbox window-start) 1)))

  (when (< (unbox current) (unbox window-start))
    (set-box! window-start (unbox current))))

(define (saturating-sub x y)
  (max (- x y) 0))

;; Moves the cursor up
(define (move-cursor-up picker)
  ;; Current element that we're on.
  (define current (Selector-cursor picker))
  (define window-start (Selector-window-start picker))

  (set-box! current (modulo (- (unbox current) 1) (length (unbox (Selector-items picker)))))

  ;; Adjust viewport
  (when (< (unbox current) (unbox window-start))
    (set-box! window-start (- (unbox window-start) 1)))

  ;; Wrap around
  (when (> (unbox current) (+ (unbox window-start) (- (unbox (Selector-max-length picker)) 2)))
    (set-box! window-start
              (saturating-sub (unbox current) (- (unbox (Selector-max-length picker)) 2)))))


(define (selector-event-handler state event)
  (define char (key-event-char event))
  (when (char? char)
        (set! char (string->symbol (string char))))
  (define key-callback (Selector-callback state))
  (define items (Selector-items state))
  (define cursor (Selector-cursor state))

  (cond
    [(key-event-escape? event) event-result/close]
    
    [(equal? char 'j)
     (move-cursor-down state)
     (define item (list-ref (unbox (Selector-items state)) (unbox (Selector-cursor state))))
     (when (hash-contains? key-callback char)
       ((hash-ref key-callback char) item items cursor))
     event-result/consume]

    [(equal? char 'k)
     (move-cursor-up state)
     (define item (list-ref (unbox (Selector-items state)) (unbox (Selector-cursor state))))
     (when (hash-contains? key-callback char)
       ((hash-ref key-callback char) item items cursor))
     event-result/consume]

    [(key-event-enter? event)
     (define item (list-ref (unbox (Selector-items state)) (unbox (Selector-cursor state))))
     (when (hash-contains? key-callback 'enter)
       ((hash-ref key-callback 'enter) item items cursor))
     event-result/close]

    [(hash-contains? key-callback char)
     (define item (list-ref (unbox (Selector-items state)) (unbox (Selector-cursor state))))
     ((hash-ref key-callback char) item items cursor)
     event-result/consume]

    [(mouse-event? event) event-result/ignore]))

(define (selector-cursor-handler state _)
  (Selector-cursor-position state))

(define (new-selector items key-callback start-index)
  (new-component! "steel-selector"
                  (Selector (box items)
                            key-callback
                            (box start-index)
                            (box (- (length items) 1))
                            (box 0)
                            (position 0 0))
                  selector-render
                 (hash "handle_event" selector-event-handler "cursor" selector-cursor-handler)))

(provide new-selector)
