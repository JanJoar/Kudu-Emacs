;    Kudu Emacs --- A fully functioning Gnu Emacs system
;    Copyright (C) 2023  Joar von Arndt
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU Affero General Public License as published
;    by the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU Affero General Public License for more details.
;
;    You should have received a copy of the GNU Affero General Public License
;    along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary: This file simply serves to load other Emacs lisp files in order to neatly separate different concepts


(org-babel-load-file (expand-file-name "~/.emacs.d/packages.org")) ;; Loads any needed packages.
(org-babel-load-file (expand-file-name "~/.emacs.d/secret.org")) ;; User-unique information (like E-mail address and full name) that you might not want to share openly. Empty by default.
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org")) ;; The main configuration file, running commands, setting keybinds, and configuring packages.

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; init.el ends here
