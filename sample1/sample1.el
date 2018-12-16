;;; sample1.el --- An example of gamegrid
;; Author: grugrut <grugruglut+github@gmail.com>
;; URL:
;; Version: 1.00

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'gamegrid)

;;; customize
(defgroup sample1 nil
  "."
  :prefix "sample1-"
  :group 'games)

(defcustom sample1-buffer-name "*Sample1*"
  "Name used for Sample1 buffer."
  :group 'sample1
  :type 'string)

(defconst sample1-player 0)
(defconst sample1-block 1)
(defconst sample1-blank 7)
(defconst sample1-border 8)
(defconst sample1-space 9)

(defconst sample1-width 40)
(defconst sample1-height 20)

;;; variable
(defvar sample1-player-pos nil)
(make-variable-buffer-local 'sample1-player-pos)
(defvar sample1-positions nil)
(make-variable-buffer-local 'sample1-positions)
(defvar sample1-tick 0.5)
(make-variable-buffer-local 'sample1-tick)
(defvar sample1-total-cell 0)
(make-variable-buffer-local 'sample1-total-cell)
(defvar sample1-player-velocity 0)
(make-variable-buffer-local 'sample1-player-velocity)

;;; keymaps
(defvar sample1-null-map
  (let ((map (make-sparse-keymap 'sample1-null-map)))
    (define-key map "n" 'sample1-start-game)
    (define-key map " " 'sample1-gen-cell)
    (define-key map [up] 'sample1-speed-down)
    (define-key map [down] 'sample1-speed-up)
    (define-key map [right] 'sample1-player-right)
    (define-key map [left] 'sample1-player-left)
    (define-key map "f" 'sample1-player-left)
    (define-key map "j" 'sample1-player-right)
    map))

;;; display options
(defvar sample1-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar sample1-player-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [1 0 0])
     (color-tty "red"))))

(defvar sample1-block-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 1])
     (color-tty "blue"))))

(defvar sample1-cell-options
  '(((glyph colorize)
     (emacs-tty ?0)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty)))
  )

(defvar sample1-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "white"))
    ))

(defvar sample1-space-options
  '(((t ?\040))
    nil
    nil))

;;; functions
(defun sample1-display-options ()
  "."
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
            (cond ((= c sample1-blank)
                   sample1-blank-options)
                  ((= c sample1-player)
                   sample1-player-options)
                  ((= c sample1-block)
                   sample1-block-options)
                  ((= c sample1-border)
                   sample1-border-options)
                  ((= c sample1-space)
                   sample1-space-options)
                  (t '(nil nil nil)))))
    options))

(defun sample1-draw-info ()
  "."
  (let* ((string (format "Speed: %f, Total cell: %d" sample1-tick sample1-total-cell))
         (len (length string)))
    (dotimes (x len)
      (gamegrid-set-cell (+ 1 x)
                         (+ 1 sample1-height)
                         (aref string x)))))

(defun sample1-start-game ()
  "."
  (interactive)
  (sample1-reset-game)
  ;;(use-local-map sample1-mode-map)
  (gamegrid-start-timer sample1-tick 'sample1-update-game))

(defun sample1-reset-game ()
  "."
  (gamegrid-kill-timer)
  (sample1-init-buffer)
  (setq sample1-positions nil
        sample1-total-cell 0
        sample1-tick 0.5
        sample1-player-pos (vector (/ sample1-width 2) (- sample1-height 1))
        ))

(defun sample1-init-buffer ()
  "."
  (gamegrid-init-buffer sample1-width (+ 2 sample1-height) sample1-space)
  (let ((buffer-read-only nil))
    (dotimes (y sample1-height)
      (dotimes (x sample1-width)
        (gamegrid-set-cell x y sample1-space)))))

(defun sample1-update-game (sample1-buffer)
  "."
  (if (eq (current-buffer) sample1-buffer)
      (let ((next-positions))
        (sample1-draw-info)
        (gamegrid-set-cell (aref sample1-player-pos 0) (aref sample1-player-pos 1) sample1-space)
        (unless (= sample1-player-velocity 0)
          (let* ((x (aref sample1-player-pos 0))
                 (y (aref sample1-player-pos 1))
                 (new-x (+ x sample1-player-velocity)))
            (if (< new-x 0)
                (setq new-x 0))
            (if (>= new-x sample1-width)
                (setq new-x (- sample1-width 1))
              (setq sample1-player-pos (vector new-x y))
              (setq sample1-player-velocity 0))))
        (gamegrid-set-cell (aref sample1-player-pos 0) (aref sample1-player-pos 1) sample1-player)
        (dolist (pos sample1-positions)
          (let ((x (aref pos 0))
                (y (aref pos 1))
                (c (aref pos 2)))
            (gamegrid-set-cell x y sample1-space)
            (setq y (+ 1 y))
            (unless (>= y sample1-height)
              (if (= (gamegrid-get-cell x y) sample1-player)
                  (sample1-stop-game))
              (gamegrid-set-cell x y c)
              (setq next-positions (cons (vector x y c) next-positions)))))
        (setq sample1-positions next-positions)
        (sample1-gen-cell))))

(defun sample1-stop-game ()
  "."
  (interactive)
  (gamegrid-kill-timer)
  (gamegrid-add-score "sample1-scores" sample1-total-cell))

(defun sample1-gen-cell ()
  "."
  (interactive)
  (let ((x (random sample1-width))
        (y 0)
        (c sample1-block))
    (gamegrid-set-cell x y c)
    (setq sample1-positions (cons (vector x y c) sample1-positions))
    (setq sample1-total-cell (1+ sample1-total-cell)))
  )

(defun sample1-speed-up ()
  "."
  (interactive)
  (setq sample1-tick (- sample1-tick 0.05))
  (if (< sample1-tick 0.05)
      (setq sample1-tick 0.05))
  (gamegrid-set-timer sample1-tick))

(defun sample1-speed-down ()
  "."
  (interactive)
  (setq sample1-tick (+ sample1-tick 0.05))
  (gamegrid-set-timer sample1-tick))

(defun sample1-player-left ()
  "."
  (interactive)
  (setq sample1-player-velocity -1))

(defun sample1-player-right ()
  "."
  (interactive)
  (setq sample1-player-velocity 1))

(put 'sample1-mode 'mode-class 'special)
(define-derived-mode sample1-mode nil "Sample1"
  "A mode for sample1."
  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)
  (use-local-map sample1-null-map)
  (setq show-trailing-whitespace nil)
  (setq gamegrid-use-glyphs t)
  (setq gamegrid-use-color t)
  (gamegrid-init (sample1-display-options)))

;;;###autoload
(defun sample1 ()
  "."
  (interactive)

  (select-window (or (get-buffer-window sample1-buffer-name)
                     (selected-window)))
  (switch-to-buffer sample1-buffer-name)
  (gamegrid-kill-timer)
  (sample1-mode)
  (sample1-start-game))

(provide 'sample1)

;;; sample1.el ends here
