#lang racket
(require xml)


(provide xexpr->telnet xexpr->mxp xexpr->string/settings)

(provide terminal-support terminal-support? terminal-support-color terminal-support-italic? terminal-support-underline?)
(provide font-mode font-mode? font-mode-color font-mode-italic font-mode-bold font-mode-underline font-mode-visible?)
(provide font-color font-color? font-color-ansi font-color-ansi-faint font-color-xterm font-color-rgb)

;; a Settings is a
;; (hash/c symbol? (or/c false? (cons int int))

(struct font-mode (color italic bold underline visible?) #:transparent) 
(define default-font-mode (font-mode '(#f . #f) #f #f #f #t))
(struct terminal-support (color italic? underline?) #:transparent)
(struct font-color (ansi ansi-faint xterm rgb) #:transparent)



;; foreground background : (list num num (list num num num)) or f (no change) or 'default
;; italic bold underline: boolean
;; visible?: boolean

(define (color-combine c1 c2)
  (cond [(false? c1) c2]
        [(false? c2) c1]
        [(and (false? (car c2))
              (false? (cdr c2)))
         c1]
        [(false? (car c2))
         (cons (car c1) (cdr c2))]
        [(false? (cdr c2))
         (cons (car c2) (cdr c1))]
        [else c2]))
                 
(define (font-mode-combine fm1 fm2)
  (font-mode
   (color-combine (font-mode-color fm1)
                  (font-mode-color fm2))
   (or (font-mode-italic fm1)
       (font-mode-italic fm2))
   (or (font-mode-bold fm1)
       (font-mode-bold fm2))
   (or (font-mode-underline fm1)
       (font-mode-underline fm2))
   (and (font-mode-visible? fm1)
        (font-mode-visible? fm2))))


;; todo - optimize this, the big cond is a bit rough maybe?
(define (font-color->ansi col bg? ts)
  (define tsc (terminal-support-color ts))
  (cond [(not (and tsc col)) empty]
        [(and bg? (symbol=? tsc 'ansi))
         (list (number->string (+ 40 (font-color-ansi-faint col))))]
        [(and (symbol=? tsc 'ansi)
              (> (font-color-ansi col) 7))
         (list (number->string (+ 22 (font-color-ansi col))) "1")]
        [(and (symbol=? tsc 'ansi+bold)
              (> (font-color-ansi col) 7))
         (list (number->string (+ (if bg? 92 82)
                                  (font-color-ansi col))))]
        [(memq tsc '(ansi ansi+bold))
         (list (number->string (+ 30 (font-color-ansi col))))]
        [(symbol=? tsc 'xterm)
         (list (if bg? "48" "38") "5" (number->string (font-color-xterm col)))]
        [else
         (list (if bg? "48" "38") "2"
               (number->string (first (font-color-rgb col)))
               (number->string (second (font-color-rgb col)))
               (number->string (third (font-color-rgb col))))]))

(define (font-color->ansi2 col bg? ts tail)
  (define tsc (terminal-support-color ts))
  (if col
      (case tsc
        [(ansi) (if bg? (cons (number->string (+ 40 (font-color-ansi-faint col))) tail)
                    (if (> (font-color-ansi col) 7)
                        (cons (number->string (+ 22 (font-color-ansi col))) (cons "1" tail))
                        (cons (number->string (+ 30 (font-color-ansi col))) tail)))]
        [(ansi+bold) (cons
                      (number->string
                       (if (> (font-color-ansi col) 7)
                           (+ (if bg? 92 82) (font-color-ansi col))
                           (+ (if bg? 40 30) (font-color-ansi col)))) tail)]
        [(xterm) (cons (if bg? "48" "38") (cons "5" (cons (number->string (font-color-xterm col)) tail)))]
        [(true-color) (cons (if bg? "48" "38")
                            (cons "2"
                                  (cons (number->string (first (font-color-rgb col)))
                                        (cons (number->string (second (font-color-rgb col)))
                                              (cons (number->string (third (font-color-rgb col))) tail)))))]
        [else tail])
      tail))
                               


(define (consif test value lst)
  (if test (cons value lst) lst))

(define (appendif test lst1 lst2)
  (if test (append lst1 lst2) lst2))

(define (font-mode-change->ansi fm1 fm2 settings)
  (define ts (hash-ref settings 'terminal (terminal-support 'ansi #f #f)))
  (define requires-reset?
    (or (and (car (font-mode-color fm1))
             (not (car (font-mode-color fm2))))
        (and (cdr (font-mode-color fm1))
             (not (cdr (font-mode-color fm2))))))
  (consif requires-reset? "0"
          (font-color->ansi2 (and (or
                                   (not (equal? (car (font-mode-color fm1))
                                                (car (font-mode-color fm2))))
                                   requires-reset?)
                                  (car (font-mode-color fm2)))
                             #f ts
                             (font-color->ansi2 (and (or
                                                      (not (equal? (cdr (font-mode-color fm1))
                                                                   (cdr (font-mode-color fm2))))
                                                      requires-reset?)
                                                     (cdr (font-mode-color fm2)))
                                                #t ts
                                                (consif (and (terminal-support-italic? ts)
                                                             (or (and requires-reset? (font-mode-italic fm2))
                                                                 (not (boolean=? (font-mode-italic fm1) (font-mode-italic fm2)))))
                                                        (if (font-mode-italic fm2) "3" "23")
                                                        (consif (and (terminal-support-underline? ts)
                                                                     (or (and requires-reset? (font-mode-underline fm2))
                                                                         (not (boolean=? (font-mode-underline fm1) (font-mode-underline fm2)))))
                                                                (if (font-mode-underline fm2) "4" "24")
                                                                empty))))))
#|(flatten
   (list
    (if requires-reset? "0" empty)
    (if (and (car (font-mode-color fm2))
             (or
              (not (equal? (car (font-mode-color fm1))
                           (car (font-mode-color fm2))))
              requires-reset?))
        (font-color->ansi (car (font-mode-color fm2)) #f ts)
        empty)
    (if (and (cdr (font-mode-color fm2))
             (or
              (not (equal? (cdr (font-mode-color fm1))
                           (cdr (font-mode-color fm2))))
              requires-reset?))
        (font-color->ansi (cdr (font-mode-color fm2)) #t ts)
        empty)
    (if (and (terminal-support-italic? ts)
             (not (boolean=? (font-mode-italic fm1) (font-mode-italic fm2))))
        (if (font-mode-italic fm2) "3" "23")
        empty)
    (if (and (terminal-support-underline? ts)
             (not (boolean=? (font-mode-underline fm1) (font-mode-underline fm2))))
        (if (font-mode-italic fm2) "4" "24")
        empty))))
  |#
                        
  
(define (settings-ref tag params settings)
  (if (assq 'type params)
      (hash-ref settings (string->symbol (string-append (symbol->string tag) "#" (second (assq 'type params))))
                (λ () (hash-ref settings tag (λ () (font-mode #f #f #f #f #t)))))
      (hash-ref settings tag (λ () (font-mode #f #f #f #f #t)))))
      

(define (settings-show? tag params settings)
  (font-mode-visible? (settings-ref tag params settings)))

(define (settings-format tag params settings)
  (define colors (hash-ref settings tag #f))
  (if colors (format "\e[~am" (string-join (map number->string colors) ";")) "\e[0m"))


(define (write-ansi-code loc out)
  (when (cons? loc)
    (display "\e[" out)
    (display (string-join loc ";") out)
    (display "m" out)))

#|(define (xexpr->telnet xpr settings)
  (define out (open-output-string))
  (display (settings-format (first xpr) (second xpr) settings) out)
  (let loop ([format-stack (list (settings-format (first xpr) (second xpr) settings))]
             [content (cddr xpr)])
    (when (cons? content)
      (match (first content)
        [(? string?) (display (first content)  out)]
        [(list tag (list params ...) body ...)
         #:when (settings-show? tag params settings)
         (display (settings-format tag params settings) out)
         (loop (cons (settings-format tag params settings) format-stack) body)
         (if (cons? format-stack)
             (display (first format-stack) out)
             (display "\e0m" out))]
        [else (void)])
      (loop format-stack (rest content))))
  (get-output-string out))
|#


(define (xexpr->telnet xpr settings)
  (define out (open-output-string))
  (write-ansi-code '("0") out)
  (let loop ([format default-font-mode]
             [content (cddr xpr)])
    (when (cons? content)
      (match (first content)
        [(? string?) (display (first content) out)]
        [(list tag (list params ...) body ...)
         #:when (settings-show? tag params settings)
         (define new-format (font-mode-combine format (settings-ref tag params settings)))
         (write-ansi-code (font-mode-change->ansi format new-format settings)  out)
         (loop new-format body)
         (write-ansi-code (font-mode-change->ansi new-format format settings) out)]
        [else (void)])
      (loop format (rest content))))
  (get-output-string out))

(define (html-params params)
  (if (empty? params) ""
      (string-join (map (λ (pair)
                          (format "~a=\"~a\"" (first pair) (second pair))) params) " " #:before-first " ")))
   
(define (add-options-to-tags xpr settings)
  (let loop ([content xpr])
    (match content
      [(? string?) content]
      [(list tag (list params ...) body ...)
        #:when (settings-show? tag params settings)
       `(,tag (,@(map (lambda (pair)
                        (cons (string->symbol (string-append "data-mxp-" (symbol->string (car pair))))
                              (cdr pair))) params))
              ,@(map loop body))])))

(define (byte->hex b)
  (if (< b 16)
      (string-append "0" (number->string b 16))
      (number->string b 16)))

(define (color->CSS fmc)
  (define rgb (or (font-color-rgb fmc)
;                  (xterm->rgb (font-color-xterm fmc))
                  ;                  (ansi->rgb (font-color-ansi fmc))
                  ))
  (string-append "#"
                 (string-join (map byte->hex rgb) "")))

(define (font-mode->CSS fm)
  (define result (open-output-string))
  (define fg (and (font-mode-color fm) (car (font-mode-color fm))))
  (define bg (and (font-mode-color fm) (cdr (font-mode-color fm))))
  (when fg
    (display "color: " result)
    (displayln (color->CSS fg) result))
  (when bg
    (display "background-color: " result)
    (displayln (color->CSS bg) result))
  (when (font-mode-italic fm)
    (displayln "font-mode: italic" result))
  (when (font-mode-bold fm)
    (displayln "font-weight: bold" result))
  (when (font-mode-underline fm)
    (displayln "text-decoration: underline" result))
  (get-output-string result))
  
(define (settings->CSS settings)
  (define result (open-output-string))
  (for ([(tag setting) (in-hash settings)]
        #:unless (eq? tag 'terminal))
    (define tag/string (symbol->string tag))
    (define tag/split (regexp-match #rx"^([^#]*)#([^#]*)$" tag/string))
    (define just-tag (if tag/split (second tag/split) tag/string))
    (define maybe-attr (if tag/split (third tag/split) #f))
    (displayln (format "~a~a {\n~a}\n"
                     just-tag
                     (if maybe-attr (format "[data-mxp-type=\"~a\"]" maybe-attr) "")
                     (font-mode->CSS setting)
                     ) result))
  (get-output-string result))
     
(define (xexpr->string/settings xpr settings)
  (xexpr->string (add-options-to-tags xpr settings)))

(define (xexpr->mxp xpr settings)
  (define out (open-output-string))
  (let loop ([content (cddr xpr)])
    (when (cons? content)
      (match (first content)
        [(? string?) (display (first content) out)]
        [(list tag (list params ...) body ...)
         #:when (settings-show? tag params settings)
         (display (format "\e[4z<~a~a>" tag (html-params params)) out)
         (loop body)
         (display (format "\e[4z</~a>" tag) out)]
        [else (void)])
      (loop (rest content))))
  (get-output-string out))

(module+ test
  (define test-text
    `(text () "Welcome to " (emph () "GriftOS Test Server") "!" (chat ((type "club")) "CLUB CHAT")))
  (define test-settings
    (hasheq 'emph (font-mode (cons (font-color 15 7 200 (list 128 128 128)) #f) #f #f #f #t)
            'chat#club (font-mode (cons (font-color 15 4 299 (list 9 8 16)) #f) #t #t #t #t)
            'terminal (terminal-support 'true-color #t #t)))
  (displayln "css")
  (displayln (settings->CSS test-settings))
  (displayln "xexpr->telnet")
  (xexpr->telnet test-text test-settings)
  (displayln "xexpr->mxp")
  (xexpr->mxp test-text test-settings)
  (displayln "xexpr->string/settings")
  (xexpr->string/settings test-text test-settings))


;(require profile profile/render-graphviz)
;(profile (xexpr->telnet test-text test-settings) #:repeat 200000 #:delay 0.005 #:use-errortrace? #t #:render render)
;" VOOP "
;(profile (xexpr->telnet test-text test-settings) #:repeat 200000 #:delay 0.005 #:use-errortrace? #f #:render render)
;(displayln (xexpr->telnet test-text test-settings))
