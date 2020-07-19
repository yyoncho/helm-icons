;;; helm-icons.el --- Helm icons  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: convenience

;; Version: 0.1
;; URL: https://github.com/yyoncho/helm-icons
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (f "0.20.0") (treemacs "2.7"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received candidate copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package plugs icons into `helm' standart functions.

;;; Code:

(require 'treemacs-themes)
(require 'treemacs-icons)
(require 'dash)
(require 'seq)

(defgroup helm-icons nil
  "Helm treemacs icons."
  :group 'helm)

(defcustom helm-icons-mode->icon
  '((dired-mode . dir-closed)
    (emacs-lisp-mode . "el")
    (spacemacs-buffer-mode . "el"))
  "Lookup Emacs mode -> `treemacs' icon key."
  :type '(alist :key-type symbol :value-type sexp))

(defun helm-icons--get-icon (ext)
  "Get icon for EXT."
  (treemacs-get-icon-value ext nil (treemacs-theme->name (treemacs-current-theme))))

(defun helm-icons-buffers-add-icon (candidates _source)
  "Add icon to buffers source.
CANDIDATES is the list of candidates."
  (-map (-lambda ((display . buffer))
          (cons (concat
                 (with-current-buffer buffer
                   (or (->> (assoc major-mode helm-icons-mode->icon)
                            (cl-rest)
                            helm-icons--get-icon)
                       (-some->> (buffer-file-name)
                         f-ext
                         helm-icons--get-icon)
                       (helm-icons--get-icon 'fallback)))
                 display)
                buffer))
        candidates))

(defun helm-icons-files-add-icons (candidates _source)
  "Add icon to files source.
CANDIDATES is the list of candidates."
  (-map (-lambda (candidate)
          (-let [(display . file-name) (if (listp candidate)
                                           candidate
                                         (cons candidate candidate))]
            (cons (concat (cond
                           ((f-dir? file-name) (helm-icons--get-icon 'dir-closed))
                           ((helm-icons--get-icon (f-ext file-name)))
                           ((helm-icons--get-icon 'fallback)))
                          display)
                  file-name)))
        candidates))

(defun helm-icons-add-transformer (fn source)
  "Add FN to `filtered-candidate-transformer' slot of SOURCE."
  (setf (alist-get 'filtered-candidate-transformer source)
        (-uniq (append
                (-let [value (alist-get 'filtered-candidate-transformer source)]
                  (if (seqp value) value (list value)))
                (list fn)))))

(defun helm-icons--make (orig name class &rest args)
  "The advice over `helm-make-source'.
ORIG is the original function.
NAME, CLASS and ARGS are the original params."
  (let ((result (apply orig name class args)))
    (cl-case class
      ((helm-recentf-source helm-source-ffiles helm-locate-source helm-fasd-source)
       (helm-icons-add-transformer
        #'helm-icons-files-add-icons
        result))
      ((helm-source-buffers helm-source-projectile-buffer)
       (helm-icons-add-transformer
        #'helm-icons-buffers-add-icon
        result)))
    (cond
     ((or (-any? (lambda (source-name) (s-match source-name name))
                 '("Projectile files"
                   "Projectile projects"
                   "Projectile directories"
                   "Projectile recent files"
                   "Projectile files in current Dired buffer"
                   "dired-do-rename.*"
                   "Elisp libraries (Scan)")))
      (helm-icons-add-transformer
       #'helm-icons-files-add-icons
       result)))
    result))

;;;###autoload
(defun helm-icons-enable ()
  "Enable `helm-icons'."
  (interactive)
  (advice-add 'helm-make-source :around #'helm-icons--make)
  (treemacs--setup-icon-background-colors))

(provide 'helm-icons)
;;; helm-icons.el ends here
