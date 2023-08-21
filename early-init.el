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

;;; Commentary: Early optimizations mostly for improved startup times. A not-insignificant parts of are taken from https://github.com/Stefanomarton/DotFiles/ and his wonderful improvements.

(defvar me/gc-cons-threshold 100000000)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold me/gc-cons-threshold
                  gc-cons-percentage 0.1)))

(defun me/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun me/restore-garbage-collection-h ()
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold me/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'me/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'me/restore-garbage-collection-h)

;; Disabling these things here prevents them from ever loading
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tab-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)

;;; early-init.el ends here
