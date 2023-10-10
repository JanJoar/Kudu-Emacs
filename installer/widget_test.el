(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar widget-example-repeat)

(defun widget-example ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer "*Widget Example*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Here is some documentation.\n\n")
  (widget-create 'editable-field
                 :size 13
                 :format "Name: %v " ; Text after the field!
                 "My Name")
  (widget-create 'menu-choice
                 :tag "Choose"
                 :value "This"
                 :help-echo "Choose me, please!"
                 :notify (lambda (widget &rest ignore)
                           (message "%s is a good choice!"
                                    (widget-value widget)))
                 '(item :tag "This option" :value "This")
                 '(choice-item "That option")
                 '(editable-field :menu-tag "No option" "Thus option"))
  (widget-create 'editable-field
                 :format "Address: %v"
                 "Some Place\nIn some City\nSome country.")
  (widget-insert "\nSee also ")
  (widget-create 'link
                 :notify (lambda (&rest ignore)
                           (widget-value-set widget-example-repeat
                                             '("En" "To" "Tre"))
                           (widget-setup))
                 "other work")
  (widget-insert
    " for more information.\n\nNumbers: count to three below\n")
  (setq widget-example-repeat
        (widget-create 'editable-list
                       :entry-format "%i %d %v"
                       :notify
                       (lambda (widget &rest ignore)
                         (let ((old (widget-get widget
                                                ':example-length))
                               (new (length (widget-value widget))))
                           (unless (eq old new)
                             (widget-put widget ':example-length new)
                             (message "You can count to %d." new))))
                       :value '("One" "Eh, two?" "Five!")
                       '(editable-field :value "three")))
  (widget-insert "\n\nSelect multiple:\n\n")
  (widget-create 'checkbox t)
  (widget-insert " This\n")
  (widget-create 'checkbox nil)
  (widget-insert " That\n")
  (widget-create 'checkbox
                 :notify (lambda (&rest ignore) (message "Tickle"))
                 t)
  (widget-insert " Thus\n\nSelect one:\n\n")
  (widget-create 'radio-button-choice
                 :value "One"
                 :notify (lambda (widget &rest ignore)
                           (message "You selected %s"
                                    (widget-value widget)))
                 '(item "One") '(item "Another One.")
                 '(item "A Final One."))
  (widget-insert "\n")
    (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (let ((name (widget-value (widget-children (widget-at (point))))))
                             (if (= (length (widget-value widget-example-repeat)) 3)
                                 (message "Congratulations, %s!" name)
                               (error "Three was the count!"))))
                 "Apply Form")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (if (= (length
                                   (widget-value widget-example-repeat))
                                  3)
                               (message "Congratulation" )
                             (error "Three was the count!")))
                 "Apply Form")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (widget-example))
                 "Reset Form")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)) 

(require 'widget)

(defun my-widget-program ()
  "Create a simple widget program."
  (interactive)
  (switch-to-buffer "*Widget Program*")
  (erase-buffer)
  (widget-create 'editable-field
                 :size 20
                 :format "Name: %v "
                 :notify (lambda (&rest ignore)
                           (message "Hello, %s!" (widget-value (widget-children (widget-at (point)))))))
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (message "Button pressed! Name: %s" (widget-value (widget-children (widget-at (point)))))) "Press me")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))

;; Run the widget program
(my-widget-program)
