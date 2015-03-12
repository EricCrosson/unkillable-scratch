;;; unkillable-scratch.el --- Disallow the *scratch* buffer from being killed
;; Version: 0.0.20140311

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: scratch
;; Package-Version: 0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode that will disallow the *scratch*
;; buffer from being killed.

;; Usage:

;; (unkillable-scratch 1)

;; Thanks to
;; [[http://emacswiki.org/emacs/RecreateScratchBuffer][EmacsWiki:
;; Recreate Scratch Buffer]]

;;; Code
(defgroup scratch nil
  "*Scratch* buffer."
  :group 'scratch)

;;;###autoload
(defun unkillable-scratch-buffer ()
  "A hook designed to be added to hook
`kill-buffer-query-functions' to prevent the *scratch* buffer
from ever being killed. Instead of a successful kill, the
*scratch* buffer will be regenerated."
  (if (not (equal (buffer-name (current-buffer)) "*scratch*"))
      t
    (delete-region (point-min) (point-max))
    (insert (or initial-scratch-message ""))
    nil))

(defun unkillable-scratch-turn-on ()
  "Turn on function `unkillable-scratch'."
  (add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer))

(defun unkillable-scratch-turn-off ()
  "Turn off function `unkillable-scratch'."
  (remove-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer))

;;;###autoload
(define-minor-mode unkillable-scratch
  "A minor mode to disallow the *scratch* buffer from being killed."
  :init-value nil
  :global t
  :group 'scratch
  (if unkillable-scratch
      (unkillable-scratch-turn-on)
    (unkillable-scratch-turn-off)))

(provide 'unkillable-scratch)

;;; unkillable-scratch.el ends here
