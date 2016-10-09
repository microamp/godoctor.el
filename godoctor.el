;;; godoctor.el --- Emacs frontend for godoctor

;; Copyright (C) 2016 james sangho nah <microamp@protonmail.com>
;;
;; Author: james sangho nah <microamp@protonmail.com>
;; Version: 0.0.1
;; Keywords: go golang refactoring
;; Homepage: https://github.com/microamp/godoctor.el

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs frontend for godoctor

;;; Code:

(defcustom godoctor-executable "godoctor"
  "Default executable for godoctor."
  :type 'string
  :group 'godoctor)

(defvar godoctor-refactoring-rename "rename")
(defvar godoctor-refactoring-extract "extract")
(defvar godoctor-refactoring-toggle "toggle")
(defvar godoctor-refactoring-godoc "godoc")

(defun godoctor-cmd (args dry-run)
  (let* ((cmd (list godoctor-executable nil t nil))
         (with-dry-run (if dry-run args (cons "-w" args))))
    (append cmd with-dry-run)))

(defun godoctor-rename-cmd (pos new-name &optional dry-run)
  (godoctor-cmd (list "-file" (buffer-file-name) "-pos" pos
                      godoctor-refactoring-rename new-name)
                dry-run))

(defun godoctor-extract-cmd (pos new-name &optional dry-run)
  (godoctor-cmd (list "-file" (buffer-file-name) "-pos" pos
                      godoctor-refactoring-extract new-name)
                dry-run))

(defun godoctor-toggle-cmd (pos &optional dry-run)
  (godoctor-cmd (list "-file" (buffer-file-name) "-pos" pos
                      godoctor-refactoring-toggle)
                dry-run))

(defun godoctor-godoc-cmd (&optional dry-run)
  (godoctor-cmd (list "-file" (buffer-file-name)
                      godoctor-refactoring-godoc)
                dry-run))

(defun get-pos-region ()
  (let* ((start (region-beginning))
         (end (region-end))
         (len (- end start)))
    (format "%d,%d" start len)))

(defun execute-godoctor-command (compilation-buffer cmd &optional dry-run)
  (with-current-buffer (get-buffer-create compilation-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (message "Running godoctor...")
    (let* ((win (display-buffer (current-buffer)))
           (proc (apply #'call-process cmd))
           (successful (= proc 0)))
      (compilation-mode)
      (if (and successful (not dry-run))
          ;; If successful *and* not dry run, kill the buffer
          (kill-buffer compilation-buffer)
        ;; Otherwise, keep it displayed with errors or diffs
        (shrink-window-if-larger-than-buffer win)
        (set-window-point win (point-min)))
      (message (if successful "godoctor completed"
                 (format "godoctor exited with %d" proc))))))

;;;###autoload
(defun godoctor-rename (&optional dry-run)
  (interactive)
  (let ((symbol (symbol-at-point)))
    (unless symbol
      (error "No symbol at point"))
    (let* ((compilation-buffer "*godoctor rename*")
           (offset (point))
           (new-name (symbol-name symbol))
           (len (length new-name))
           (pos (format "%d,%d" offset len))
           (new-name (read-string "New name: " new-name))
           (cmd (godoctor-rename-cmd pos new-name dry-run)))
      (execute-godoctor-command compilation-buffer cmd dry-run))))

;;;###autoload
(defun godoctor-rename-dry-run ()
  (interactive)
  (godoctor-rename t))

;;;###autoload
(defun godoctor-extract (&optional dry-run)
  (interactive)
  (let* ((compilation-buffer "*godoctor extract*")
         (pos (get-pos-region))
         (new-name (read-string "Function name: "))
         (cmd (godoctor-extract-cmd pos new-name dry-run)))
    (execute-godoctor-command compilation-buffer cmd dry-run)))

;;;###autoload
(defun godoctor-extract-dry-run ()
  (interactive)
  (godoctor-extract t))

;;;###autoload
(defun godoctor-toggle (&optional dry-run)
  (interactive)
  (let* ((compilation-buffer "*godoctor toggle*")
         (pos (get-pos-region))
         (cmd (godoctor-toggle-cmd pos dry-run)))
    (execute-godoctor-command compilation-buffer cmd dry-run)))

;;;###autoload
(defun godoctor-toggle-dry-run ()
  (interactive)
  (godoctor-toggle t))

;;;###autoload
(defun godoctor-godoc (&optional dry-run)
  (interactive)
  (let* ((compilation-buffer "*godoctor godoc*")
         (cmd (godoctor-godoc-cmd dry-run)))
    (execute-godoctor-command compilation-buffer cmd dry-run)))

;;;###autoload
(defun godoctor-godoc-dry-run ()
  (interactive)
  (godoctor-godoc t))

(provide 'godoctor)
