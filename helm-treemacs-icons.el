;;; helm-treemacs-icons.el --- Helm integration with treemacs icons  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'treemacs-themes)
(require 'treemacs-icons)

(defcustom helm-treemacs-icons-mode->icon
  '((dired-mode . dir-open)
    (emacs-lisp-mode . "el")
    (spacemacs-buffer-mode . "el"))
  "Lookup emacs mode -> `treemacs' icon key.")

(defun helm-treemacs-icons--get-icon (ext)
  "Get icon for EXT."
  (treemacs-get-icon-value ext nil (treemacs-theme->name (treemacs-current-theme))))

(defun helm-treemacs-icons-buffers-add-icon (candidates _source)
  "Add icon to buffer source.
CANDIDATES is the list of candidates."
  (-map (-lambda ((display . buffer))
          (cons (concat
                 (with-current-buffer buffer
                   (or (->> (assoc major-mode helm-treemacs-icons-mode->icon)
                            (cl-rest)
                            helm-treemacs-icons--get-icon)
                       (-some->> (buffer-file-name)
                         f-ext
                         helm-treemacs-icons--get-icon)
                       (helm-treemacs-icons--get-icon 'fallback)))
                 display)
                buffer))
        candidates))

(defun helm-treemacs-icons-files-add-icons (candidates _source)
  (-map (-lambda ((display . file-name))
          (cons (concat (cond
                         ((f-dir? file-name) (helm-treemacs-icons--get-icon 'dir-open))
                         ((helm-treemacs-icons--get-icon (f-ext file-name)))
                         ((helm-treemacs-icons--get-icon 'fallback)))
                        display)
                file-name))
        candidates))

(defun helm-treemacs-icons-add-transformer (fn source)
  "Add FN to `filtered-candidate-transformer' slot of SOURCE."
  (setf (slot-value source 'filtered-candidate-transformer)
        (-uniq (append
                (-let [value (slot-value source 'filtered-candidate-transformer)]
                  (if (seqp value) value (list value)))
                (list fn)))))

(defun helm-treemacs-icons-add-transformer-created-source (fn source)
  "Add FN to `filtered-candidate-transformer' slot of SOURCE.
SOURCE is already created `helm' source."
  (setf (alist-get 'filtered-candidate-transformer source)
        (-uniq (append
                (-let [value (alist-get 'filtered-candidate-transformer source)]
                  (if (seqp value) value (list value)))
                (list fn)))))

;;;###autoload
(defun helm-treemacs-icons-enable ()
  "Enable `helm-treemacs-icons'"
  (interactive)

  (with-eval-after-load 'helm-buffers
    (cl-defmethod helm-setup-user-source :after ((source helm-source-buffers))
      (helm-treemacs-icons-add-transformer #'helm-treemacs-icons-buffers-add-icon source)))


  (with-eval-after-load 'helm-files
    (cl-defmethod helm-setup-user-source :after ((source helm-source-ffiles))
      (helm-treemacs-icons-add-transformer #'helm-treemacs-icons-files-add-icons source)))

  (with-eval-after-load 'helm-for-files
    (cl-defmethod helm-setup-user-source :after ((source helm-locate-source))
      (helm-treemacs-icons-add-transformer #'helm-treemacs-icons-files-add-icons source))

    (helm-treemacs-icons-add-transformer-created-source
     #'helm-treemacs-icons-files-add-icons
     helm-source-recentf))

  (with-eval-after-load 'helm-locate-library
    (helm-treemacs-icons-add-transformer-created-source #'helm-treemacs-icons-files-add-icons helm-source-locate)))

(provide 'helm-treemacs-icons)
;;; helm-treemacs-icons.el ends here
