
(use-modules (gnu))
(use-service-modules networking ssh)
(use-package-modules screen ssh)

(operating-system
  (host-name "{{hostname}}")
  (timezone "{{timezone}}")
  (locale "{{locale}}")

  ;; Boot in "legacy" BIOS mode, assuming /dev/sdX is the
  ;; target hard disk, and "my-root" is the label of the target
  ;; root file system.
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets '("{{disk_bootloader}}"))))
  ;; It's fitting to support the equally bare bones ‘-nographic’
  ;; QEMU option, which also nicely sidesteps forcing QWERTY.
  (kernel-arguments (list "console=ttyS0,115200"))
  (file-systems (cons (file-system
                        (device (file-system-label "kudu-root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
	(users  
		(list
			{% for user in users %}
				(user-account
					(name "{user.name}")
					(comment "{user.comment}")
					(group "{user.group}")
					(supplementary-groups '("wheel" "audio" "video"))
				)
			{% endfor %}
		%base-user-accounts
	))

  ;; Globally-installed packages.
  (packages (list 
	      screen
	      emacs
	      emacs-exwm
	      wmctl
	      brightnessctl
	      git
	      icecat
	      openttd
	      %base-packages
	))

  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server.
  (services (append (list (service dhcp-client-service-type)
                          (service openssh-service-type
                                   (openssh-configuration
                                    (openssh openssh-sans-x)
                                    (port-number 2222))))
                    %base-services)))
