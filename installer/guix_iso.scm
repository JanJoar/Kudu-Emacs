
(use-modules 
  (gnu)
  (gnu packages emacs)
  )
(use-service-modules networking ssh)
(use-package-modules screen ssh)

(operating-system
  (host-name "kudu-inst")
  (timezone "Europe/Stockholm")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets '("$DISK"))))

  (kernel-arguments (list "console=ttyS0,115200"))
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "$ROOT_UUID"
				  'ext4))
                         (type "ext4")) %base-file-systems)))

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
  (users %base-user-accounts)

  ;; Globally-installed packages.
  (packages (append (list 
		      screen 
		      emacs
		      )
		    %base-packages))

  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server.
  (services (append (list (service dhcp-client-service-type)
                          (service openssh-service-type
                                   (openssh-configuration
                                    (openssh openssh-sans-x)
                                    (port-number 2222))))
                    %base-services)))
