#lang racket/base

(require racket/list racket/match racket/string xml "color.rkt")

(provide xexpr->telnet xexpr->mxp xexpr->string/settings)
(provide register-tags g-tag g-tag? mxp-opt mxp-opt? make-mxp-opt get-mxp-elements settings->CSS)

(provide fontmode default-fontmode fontmode-none fontmode? fontmode-fore fontmode-back fontmode-italic fontmode-bold fontmode-underline fontmode-strike)
(provide terminal-support terminal-support? terminal-support-color terminal-support-italic? terminal-support-bold? terminal-support-underline?
         terminal-support-strike?)
(provide font-color font-color? font-color-ansi font-color-ansi-faint font-color-xterm font-color-rgb)
;(struct font-color (ansi ansi-faint xterm rgb) #:transparent)
#|
New settings mode

each tag has 
- italic?
- strikethrough?
- bold?
- underline?
- send? (if the tag is a send, in which case href is required)
- html-type (one of 'span 'div 'emph 'a etc.)

and a set of 0 or more color schemes
a color scheme is a
 - name (symbol or #f for default)
 - foreground color (#f for no change)
 - background color (#f for no change)





|#
(define tags (make-hasheq))
(define (register-tags . lot)
  (for ([tag (in-list lot)])
    (hash-set! tags (g-tag-name tag) tag)))

(struct g-tag (name html-eqv mxp-options params))
(struct mxp-opt (defn att tag flag open? empty?))



(define (mxp-opt->string name mo)
  (define result (open-output-string))
  (fprintf result "<!EL ~a" name)
  (when mo
    (when (mxp-opt-defn mo)
      (fprintf result " '~a'" (mxp-opt-defn mo)))
    (when (mxp-opt-att mo)
      (fprintf result " ATT='~a'" (mxp-opt-att mo)))
    (when (mxp-opt-tag mo)
      (fprintf result " TAG=~a" (mxp-opt-tag mo)))
    (when (mxp-opt-flag mo)
      (fprintf result " FLAG='~a'" (mxp-opt-flag mo)))
    (when (mxp-opt-open? mo)
      (display " OPEN" result))
    (when (mxp-opt-empty? mo)
      (display " EMPTY" result)))
  (display ">" result)
  (get-output-string result))

(define (get-mxp-elements)
  (hash-map tags
            (lambda (name tag)
              (mxp-opt->string name (g-tag-mxp-options tag)))))

(define (make-mxp-opt #:defn [defn #f] #:att [att #f] #:tag [tag #f] #:flag [flag #f] #:open? [open? #f] #:empty? [empty? #f])
  (if (or defn att tag flag open? empty?)
      (mxp-opt defn att tag flag open? empty?)
      #f))

(struct fontmode (fore back italic strike bold underline) #:transparent)
(define default-fontmode (fontmode #f #f 'off 'off 'off 'off))
(define fontmode-none (fontmode #f #f #f #f #f #f))
(struct font-color (ansi ansi-faint xterm rgb) #:transparent)
(struct terminal-support (color italic? underline? bold? strike?) #:transparent)

;; A tag-settings is a (Hash Symbol -> TagMode)
;; an overrides is a (Hash Symbol -> (Hash Symbol -> color-override))

(define (fontmode-combine fm1 fm2)
  (fontmode
   (or (fontmode-fore fm2) (fontmode-fore fm1))
   (or (fontmode-back fm2) (fontmode-back fm1))
   (or (fontmode-italic fm2) (fontmode-italic fm1))
   (or (fontmode-strike fm2) (fontmode-strike fm1))
   (or (fontmode-bold fm2) (fontmode-bold fm1))
   (or (fontmode-underline fm2) (fontmode-underline fm1))))
   

(define (font-color->ansi col bg? tsc tail)
  (if (font-color? col)
      (case tsc
        [(ansi) (if bg? (cons (number->string (+ 40 (font-color-ansi-faint col))) tail)
                    (if (> (font-color-ansi col) 7)
                        (cons (number->string (+ 22 (font-color-ansi col))) (cons "1" tail))
                        (cons (number->string (+ 30 (font-color-ansi col))) (cons "22" tail))))]
        [(ansi+bold) (cons
                      (number->string
                       (if (> (font-color-ansi col) 7)
                           (+ (if bg? 92 82) (font-color-ansi col))
                           (+ (if bg? 40 30) (font-color-ansi col)))) tail)]
        [(xterm) (cons (if bg? "48" "38") (cons "5" (cons (number->string (font-color-xterm col)) tail)))]
        [(true-color)
         (if (font-color-rgb col)
             (cons (if bg? "48" "38")
                   (cons "2"
                         (cons (number->string (first (font-color-rgb col)))
                               (cons (number->string (second (font-color-rgb col)))
                                     (cons (number->string (third (font-color-rgb col))) tail)))))
             (cons (if bg? "48" "38") (cons "5" (cons (number->string (font-color-xterm col)) tail))))]
        [else tail])
      (appendif (and col tsc) (if bg? '("49") (list "22" "39")) tail)))

(define (consif test value lst)
  (if test (cons value lst) lst))

(define (appendif test lst1 lst2)
  (if test (append lst1 lst2) lst2))

(define (on? v)
  (eq? v 'on))



(define (font-mode-change->ansi fm1 fm2 ts)
  (define requires-reset? #f)
;    (or (and (fontmode-fore fm1)
 ;            (not (fontmode-fore fm2)))
  ;      (and (fontmode-back fm1))
  ;     (not (fontmode-back fm2))))
  (define tsc (terminal-support-color ts))
  (define italic? (on? (fontmode-italic fm2)))
  (define underline? (on? (fontmode-underline fm2)))
  (define strike? (on? (fontmode-strike fm2)))
  
  (consif requires-reset? "0"
          (font-color->ansi (and (or
                                  (not (equal? (fontmode-fore fm1)
                                               (fontmode-fore fm2)))
                                   requires-reset?)
                                 (or (fontmode-fore fm2) 'default))
                            #f tsc
                            (font-color->ansi (and (or
                                                     (not (equal? (fontmode-back fm1)
                                                                  (fontmode-back fm2)))
                                                     requires-reset?)
                                                    (or (fontmode-back fm2) 'default))
                                               #t tsc
                                               (consif (and (terminal-support-italic? ts)
                                                            (or (and requires-reset? italic?)
                                                                (not (eq? (fontmode-italic fm1) (fontmode-italic fm2)))))
                                                        (if italic? "3" "23")
                                                        (consif (and (terminal-support-underline? ts)
                                                                     (or (and requires-reset? underline?)
                                                                         (not (eq? (fontmode-underline fm1) (fontmode-underline fm2)))))
                                                                (if underline? "4" "24")
                                                                (consif (and (terminal-support-strike? ts)
                                                                     (or (and requires-reset? strike?)
                                                                         (not (eq? (fontmode-strike fm1) (fontmode-strike fm2)))))
                                                                (if strike? "9" "29")
                                                                empty)))))))


(define (settings-ref tag params settings tags)
  (define taginfo (hash-ref tags tag #f))
  (let loop ([fm (hash-ref settings tag fontmode-none)]
             [param-list (if taginfo (g-tag-params taginfo) empty)])
    (if (empty? param-list) fm
        (let* (
          [param (first param-list)]
          [param-value (assq param params)]
          [psetting (and param-value (hash-ref settings (string->symbol (format "~a[~a=~a]" tag param (second param-value))) #f))])
          (loop (if psetting (fontmode-combine fm psetting) fm) (rest param-list))))))

(define (write-ansi-code loc out)
  (when (cons? loc)
    (display "\e[" out)
    (display (string-join loc ";") out)
    (display "m" out)))

(define div-like-tags '(div p)) ;; todo

(define (xexpr->telnet xpr terminal settings [tags tags])
  (define out (open-output-string))
  (write-ansi-code '("0") out)
  (let loop ([format default-fontmode]
             [content (cddr xpr)]
             [last-was-nl? #f])
    (cond [(cons? content)
           (define nl?
             (match (first content)
               [(? string?) (display (first content) out) (char=? (string-ref (first content) (sub1 (string-length (first content)))) #\newline)]
               [(or 'br (list 'br '())) (display "\e[K\n" out) #t]
               [(list tag (list params ...) body ...)
                (define new-format (fontmode-combine format (settings-ref tag params settings tags)))
                (define tag-info (hash-ref tags tag #f))
                (define is-div? (and tag-info (memq (g-tag-html-eqv tag-info) div-like-tags)))
                (when (and is-div? (not last-was-nl?))
                  (display "\e[K\n" out))
                (write-ansi-code (font-mode-change->ansi format new-format terminal)  out)
                (define nested-nl? (loop new-format body (or last-was-nl? is-div?)))
                (when (and is-div? (not nested-nl?))
                  (display "\e[K\n" out))
                (write-ansi-code (font-mode-change->ansi new-format format terminal) out)
                (or nested-nl? is-div?)]
               [else last-was-nl?]))
           (loop format (rest content) nl?)]
          [else last-was-nl?]))
  (get-output-string out))

(define (html-params params)
  (if (empty? params) ""
      (string-join (map (λ (pair)
                          (format "~a=\"~a\"" (first pair) (second pair))) params) " " #:before-first " ")))

(define (xexpr->mxp xpr terminal settings [tags tags])
  (define out (open-output-string))
  (write-ansi-code '("0") out)
  (let loop ([font-format default-fontmode]
             [content (cddr xpr)]
             [last-was-nl? #f])
    (cond [(cons? content)
           (define nl?
             (match (first content)
               [(? string?) (display (first content) out) (char=? (string-ref (first content) (sub1 (string-length (first content)))) #\newline)]
               [(or 'br (list 'br '())) (display "\e[K\n" out) #t]
               [(list tag (list params ...) body ...)
                (define new-format (fontmode-combine font-format (settings-ref tag params settings tags)))
                (define tag-info (hash-ref tags tag #f))
                (define is-div? (and tag-info (memq (g-tag-html-eqv tag-info) div-like-tags)))
                (when (and is-div? (not last-was-nl?))
                  (display "\e[K\n" out))
                (write-ansi-code (font-mode-change->ansi font-format new-format terminal)  out)
                (display (format "\e[4z<~a~a>" tag (html-params params)) out)
                (define nested-nl? (loop new-format body (or last-was-nl? is-div?)))
                (display (format "\e[4z</~a>" tag) out)
                (when (and is-div? (not nested-nl?))
                  (display "\e[K\n" out))
                (write-ansi-code (font-mode-change->ansi new-format font-format terminal) out)
                (or is-div? nested-nl?)]
               [else last-was-nl?]))
           (loop font-format (rest content) nl?)]
          [else last-was-nl?]))
  (get-output-string out))

(define (tag->html tag tags)
  (g-tag-html-eqv (hash-ref tags tag (g-tag tag 'span #f '()))))

(define (add-options-to-tags xpr settings tags)
  (let loop ([content xpr])
    (match content
      [(? string?) content]
      [(list 'br '()) content]
      [(? symbol?) `(,content ())]
      [(list tag (list params ...) body ...)
       `(,(tag->html tag tags) (,@(cons (list 'class (string-append "ws__" (symbol->string tag))) (map (lambda (pair)
                        (cons (string->symbol (string-append "data-mxp-" (symbol->string (car pair))))
                              (cdr pair))) params)))
              ,@(map loop body))])))

(define (xexpr->string/settings xpr settings [tags tags])
  (xexpr->string (add-options-to-tags xpr settings tags)))




(define (byte->hex b)
  (if (< b 16)
      (string-append "0" (number->string b 16))
      (number->string b 16)))

(define (color->CSS fmc)
  (define rgb (or (font-color-rgb fmc)
                  (xterm->rgb (font-color-xterm fmc))
                  (xterm->rgb (font-color-ansi fmc))
                  ))
  (string-append "#"
                 (string-join (map byte->hex rgb) "")))


(define (fontmode->CSS fm)
  (define result (open-output-string))
  (define fg (fontmode-fore fm))
  (define bg (fontmode-back fm))
  (when fg
    (display "color: " result)
    (display (color->CSS fg) result)
    (displayln ";" result))
  (when bg
    (display "background-color: " result)
    (display (color->CSS bg) result)
    (displayln ";" result))
  (when (fontmode-italic fm)
    (display "font-mode: " result)
    (display (if (eq? (fontmode-italic fm) 'on) "italic" "normal") result)
    (displayln ";" result))
  (when (fontmode-bold fm)
    (display "font-weight: " result)
    (display (if (eq? (fontmode-bold fm) 'on) "bold" "normal") result)
    (displayln ";" result))
  (when (or (on? (fontmode-underline fm))
            (on?  (fontmode-strike fm)))
    (display "text-decoration:" result)
    (when (on? (fontmode-underline fm))
      (display " underline" result))
    (when (on? (fontmode-strike fm))
      (display " line-through" result))
    (displayln ";" result))
  (get-output-string result))

(define (settings->CSS settings [tags tags])
  (define result (open-output-string))
  (for ([(tag setting) (in-hash settings)])
    (define tag/string (symbol->string tag))
    (define tag/split (regexp-match #rx"^([^#]*)\\[([^#=]*)=(.*)\\]$" tag/string))
    (define just-tag (if tag/split (second tag/split) tag/string))
    (define html-tag (hash-ref tags (string->symbol just-tag) #f))
    (define tag-selector (if html-tag (g-tag-html-eqv html-tag) ""))
    (define maybe-attr (if tag/split (third tag/split) #f))
    (define maybe-val (if tag/split (fourth tag/split) #f))
    (displayln (format "~a.~a~a~a {\n~a}\n"
                     tag-selector "ws__" just-tag
                     (if maybe-attr (format "[data-mxp-~a=\"~a\"]" maybe-attr maybe-val) "")
                     (fontmode->CSS setting)
                     ) result))
  (get-output-string result))

(module+ test
  (define test-tags
    (list
     (g-tag 'keyword 'emph #f '())
     (g-tag 'room-name 'div (make-mxp-opt #:flag "RoomName") '())
     (g-tag 'exits 'div (make-mxp-opt #:flag "RoomExit") '())
     (g-tag 'exit 'a (make-mxp-opt #:defn "<send href=\"&text;\" EXPIRE=\"exits\">") '())
     (g-tag 'gossip 'div (make-mxp-opt #:open? #t #:att "channel sender") '(channel))
     (g-tag 'shout 'div (make-mxp-opt #:open? #t #:att "sender") '())
     (g-tag 'text 'div #f '())
     ))
  (define test-tags/hash (make-hasheq (map (lambda (gt) (cons (g-tag-name gt) gt)) test-tags)))
  (define test-text
    `(text () "Welcome to " (keyword () "Test Server") "! <><><>"
           (gossip ((channel "wizards") (sender "Zared")) "wizards only, fool!")
           (shout ((sender "Nobody")) "LOL!")
           (exits ()
                  "Obvious Exits: "
                  (exit () "north") ", "
                  (exit () "south") ", "
                  (exit () "east") ", "
                  (exit () "dennis"))))
  (define test-settings
    (hasheq 'keyword (fontmode (font-color 10 2 10 (list 128 255 128)) #f #f #f #f #f)
            'shout (fontmode (font-color 11 3 11 #f) #f #f #f #f #f)
            'exits (fontmode (font-color 2 2 2 #f) #f #f #f #f #f)
            'gossip (fontmode (font-color 6 6 6 #f) #f #f #f #f #f)
            '|gossip[channel=wizards]| (fontmode #f (font-color 5 5 53 #f)  #f #f 'on 'on)))
  ;  _(define (settings-ref tag params settings tags)
  (settings-ref 'keyword empty test-settings test-tags/hash)
  (settings-ref 'room-name empty test-settings test-tags/hash)
  (settings-ref 'chat '((type "shout")) test-settings test-tags/hash)
  (settings-ref 'chat '((type "club") (channel "LFP")) test-settings test-tags/hash)
  (settings-ref 'chat '((type "club") (channel "wizards")) test-settings test-tags/hash)

  (displayln "MXP CONFIGURATION")
  (for ([gt (in-list test-tags)])
    (displayln (mxp-opt->string (g-tag-name gt) (g-tag-mxp-options gt))))
  (displayln "CSS CONFIGURATION")
  (displayln (settings->CSS test-settings test-tags/hash))
  ;  (mxp-opt->string 'keyword #f)
  ;  (mxp-opt->string 'exitss (mxp-opt #f #f #f "RoomExit" #f #f)))
  (displayln "TEST XML\n--------")
  test-text
  (displayln "XTERM\n--------")
  (displayln (xexpr->telnet test-text (terminal-support 'xterm #t #t #f #t) test-settings test-tags/hash))
  (xexpr->telnet test-text (terminal-support 'xterm #t #t #f #t) test-settings test-tags/hash)
  (displayln "XTERM+MXP\n--------")
  (displayln (xexpr->mxp test-text (terminal-support 'xterm #t #t #f #t) test-settings test-tags/hash))
  (xexpr->mxp test-text (terminal-support 'xterm #t #t #f #t) test-settings test-tags/hash)
  (displayln "HTML\n--------")
  (displayln (xexpr->string/settings test-text test-settings test-tags/hash))
  )
