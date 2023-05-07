#lang racket/gui


;; listbox helper function
(define (listbox-add-row listbox row-lst)
    (send listbox append (car row-lst))
    (define new-row-idx (sub1 (send listbox get-number)))
    (send listbox set-data new-row-idx row-lst)
    (for ((col-idx (in-naturals))
          (col-label row-lst))
         (send listbox set-string new-row-idx col-label col-idx)))

;; listbox helper function
(define (listbox-get-all-data listbox)
  (for/list ((row-idx (range (send listbox get-number))))
       (send listbox get-data row-idx)))

(define frame (new frame% [label "GIMP Plugin Boilerplate Generator"]))

(define plugin-name-text (new text-field% [parent frame] [label "Plugin Name:"]))
(define plugin-description-text (new text-field% [parent frame] [label "Plugin Description:"]))
(define author-name-text (new text-field% [parent frame] [label "Author Name:"]))

(define generate-code-in-radio (new radio-box% [parent frame]
                                          [label "Generate Code in:"]
                                          [choices '("TinyScheme")]))

(define (generate-code-button-callback button event)
    (let* ((language (send generate-code-in-radio get-item-label
                           (send generate-code-in-radio get-selection)))
         (plugin-name (send plugin-name-text get-value))
         (plugin-description (send plugin-description-text get-value))
         (author-name (send author-name-text get-value)))
    (generate-code language plugin-name plugin-description author-name)))

;; Create a list to hold the items
(define choices (list
                  "SF-IMAGE"
                  "SF-DRAWABLE"
                  "SF-VALUE"
                  "SF-STRING"
                  "SF-COLOR"
                  "SF-TOGGLE"
                  "SF-BRUSH"
                  "SF-PATTERN"
                  "SF-GRADIENT"
                  "SF-PALETTE"
                  "SF-FILENAME"
                  "SF-DIRNAME"
                  "SF-OPTION"
                  "SF-ENUM"
                  )) 

(define hbox (new horizontal-panel% [parent frame]))

(define sf-item-choice (new choice% [label "Add Plugin GUI Item: "]
                            [parent hbox]
                            [choices choices]))
(define sf-label-text (new text-field% [parent hbox] [label ""]))
(define sf-value-text (new text-field% [parent hbox] [label ""]))

;; Create a button to add a new item
(define add-button (new button% [label "+"]
                                [parent hbox]
                                [callback
                                  (lambda (button event)
                                   (let ([the-choice (send sf-item-choice get-string
                                                           (send sf-item-choice get-selection))])
                                     (listbox-add-row
                                       listbox
                                       (list the-choice
                                             (send sf-label-text get-value) 
                                             (send sf-value-text get-value)))))]))

;; Create a dropdown menu to select the type of item to add
(define dialog (new dialog% [parent frame] [label "Select Item Type"]))

;; Create a listbox to display the items
(define listbox (new list-box% [label ""]
                              [parent frame]
                              [choices '()]
                              [columns '("Type" "Label" "Values")]
                              [style '(single column-headers)]))

(listbox-add-row listbox '("SF-IMAGE" "Image" "0"))
(listbox-add-row listbox '("SF-DRAWABLE" "Layer" "0"))

(define (name->proc-name str)
  (string-downcase
   (string-append "script-fu-" (string-replace str " " "-"))))

(define (generate-code language plugin-name plugin-description author-name)
  (define proc-name (name->proc-name plugin-name))
  (define year
    (number->string (date-year (seconds->date (current-seconds)))))
  (displayln (cond
    ((string=? language "TinyScheme")
     (string-append
       "(define (" proc-name " image drawable)\n"
       "  ; Your plugin code here\n"
       "  (gimp-message \"Hello, World!\"))\n\n"
       "(register\n"
       "  \"script-fu-" proc-name "\"\n"
       "  \"" plugin-name "\"\n"
       "  \"" plugin-description "\"\n"
       "  \"" author-name "\"\n"
       "  \"\"\n"
       "  \"" year "\"\n"
       "  \"<Image>/Plugins/" plugin-name "\"\n"
       "  \"\"\n"
       "  \"\"\n\n"

       "  ; define the GUI\n"
       "  ;type  ;label  ;values\n"
       (string-join (map (lambda (x)
                           (apply format "  ~a \"~a\" ~a" x))
                         (listbox-get-all-data listbox))
                    "\n")
       "\n)\n")))))

(define generate-code-button (new button%
                                  [parent frame]
                                  [label "Generate Code"]
                                  [callback 
                                    (lambda (button event)
                                      (generate-code-button-callback button event))] 
                                  ))

;; Add the components to the frame and show it
(send frame show #t)
