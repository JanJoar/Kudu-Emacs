(require 'widget)

(defun Kudu-installer ()
  "Create and read a text field widget."
  (interactive)
  (switch-to-buffer "*Kudu Installer*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (let ((hostname "") (username "") (disk ""))
    (widget-insert "\n")
    (setq disks (get-disks))
    (message (car disks))
    (widget-create 'editable-field
                   :size 30
                   :format "Toastname: %v "
                   :notify (lambda (widget &rest ignore)
                             (setq hostname (widget-value widget))))

    (widget-insert "\n")
    (widget-create 'radio-button-choice                                                                 
                   :value "One"                                                                         
                   :tag "radio-tag"                                                                     
                   :notify (lambda (widget &rest ignore)                                                
                             (setq disk                                                          
                                   (widget-value widget)))
                   `(item ,(nth 0 disks))
                   `(item ,(nth 1 disks))
                   `(item ,(nth 2 disks))
                   `(item ,(nth 3 disks))
                   `(item ,(nth 4 disks))
                   `(item ,(nth 5 disks))
                   `(item ,(nth 6 disks))
                   `(item ,(nth 7 disks))
                   `(item ,(nth 8 disks))
                   `(item ,(nth 9 disks))
                   `(item ,(nth 11 disks))
                   `(item ,(nth 12 disks))
                   `(item ,(nth 13 disks))
                   `(item ,(nth 69 disks))                   
                   )
    
    (widget-insert "\n")
    (widget-create 'editable-field
                   :size 30
                   :format "Username: %v "
                   :notify (lambda (widget &rest ignore)
                             (setq username (widget-value widget))))
    (widget-insert "\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (upload hostname username disk)
                             )
                   "Apply Form")

    
    (use-local-map widget-keymap)
    (widget-setup)
    ))

(defun upload (hostname username disk)
  (message "format")
  (setq cmd (format
             "bash ./install_test.sh --hostname %s --username %s --disk %s &"
             hostname
             username
             disk
             ))
  (message cmd)
  (shell-command cmd)
  )

;; Example usage:
;; M-x eval-buffer RET
;; M-x Kudu-installer
(defun get-disks ()
  "Get a list of disks on the system."
  (interactive)
  (when (eq system-type 'gnu/linux)
    (split-string
     (shell-command-to-string "./get_disks.sh")
     "\n" t))
  )

;; Example usage
(defun disks ()
(interactive)
(setq disks (get-disks))
(message "Disks: %s" disks)
)

(defmacro add-radio (hej)
  `("item" ,hej))
