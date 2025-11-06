;;; mir.el --- A holistic Incremental Reading Experience -*- lexical-binding: t; -*-

;; Author: Giorgi Gvalia <gvalia@pm.me>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: incremental-reading srs anki supermemo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; This package allows the user to import articles that will be automatically scheduled to be read. This is called incremental reading.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + denote
;; + denote-sequence

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'package-name)

;;;; Usage

;; mir saves all the reading material you import into `mir-archive-directory', so make sure that it is configured. Afterwards, use any of these commands to import the reading material:

;; `mir-import-buffer': Import the contents of the current buffer.
;; `mir-extract-or-import-from-region': Import the selected text from the current buffer.
;; `mir-import-from-minibuffer': Type out or yank what you want to import in the minibuffer.

;; Afterwards, use `mir-read' to start reading and `mir-read-next' to move onto the next topic. If you'd like to extract a part of a topic into a new topic, use `mir-extract-or-import-from-region'.

;;;; Tips

;; + You can customize how the scheduling works.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; Code:

;;;; Requirements

(require 'denote)
(require 'denote-sequence)

;;;; Customization

(defgroup mir nil
  "Settings for `mir': an emacs-integrated incremental reading package.")

;; If this directory doesn't exist, file write commands error out.
(defcustom mir-archive-directory (expand-file-name "~/Documents/mir/")
  "The directory where all the imported material will be stored.")
(defcustom mir-db-location (concat mir-archive-directory "mir.db")
  "The location for mir's database")

(defcustom mir-default-a-factor 2
  "The default A-factor for new cards.")
(defcustom mir-default-interval 1
  "How soon the newly added cards will be shown in days.")
(defcustom mir-scale-a-factor-by-priority t
  "Whether the A-factor will be scaled based on the topic's priority. The
formula for calculating the new factor is priority/17.543859 + 1.2. This
ensures that the A-factor is between 1.2 and 6.9 for priorities ranging
from 0 to 100 (lower meaning more important topics).")

(defcustom mir-query-function #'mir--get-topics-for-today-by-priority
  "The function that is used to fetch topics for populating `mir-queue'. The default option
is to fetch all the topics that are due today sorted by priority.")
(defcustom mir-bury-buffer-after-finishing t
  "Whether to bury the current buffer when calling `mir-read-next'. When set to a non-nil value, the user will see the currently open buffer be dismissed at the end of a reading session (when there's no more topics in the queue).")

;;;; Variables

(defvar mir-queue '()
  "The queue for topics to be read.")

(defvar mir--current-topic '()
  "The topic that's under review as of right now. This is NOT the next
topic.")

;;;; Commands

;;;###autoload
(defun mir-extract-or-import-from-region ()
  "Make a new topic note from the selected region"
  (interactive)
  (if-let* (((region-active-p))
            (text (buffer-substring-no-properties (region-beginning) (region-end))))
      ;; if we're somehow in an existing mir topic, do an extract.
      (if (string= default-directory mir-archive-directory)
          (mir-extract text)
        (mir-import text (completing-read "Priority: " nil)))
    (user-error "%s" "Region not active, skipping")))

;; Problem: importing from epubs and naming the file epub works with
;; emacs but it's not a TRUE epub.

;;;###autoload
(defun mir-import-buffer (priority)
  ""
  (interactive "nPriority: ")
  (mir-import (buffer-substring (point-min) (point-max)) priority))

;;;###autoload
(defun mir-import-from-minibuffer (text name priority)
  (interactive "MInscribe your material: \nMName: \nnPriority: ")
  (mir-import text priority name))

;;;###autoload
(defun mir-read ()
  (interactive)
  (if-let* ((next-topic (mir-queue-next)))
      (mir-show-topic next-topic)
    (user-error "%s" "Queue is empty for today: nothing to read")))

;;;###autoload
(defun mir-read-next ()
  (interactive)
  ;; update the db
  (when mir--current-topic
    (mir-update-topic mir--current-topic))
  ;; set it to nil to prevent repeated invocations of the command from
  ;; updating the topic many many times
  (setq mir--current-topic nil)
  ;; get the next item in the queue
  (if-let* ((next-topic (mir-queue-next)))
      (mir-show-topic next-topic)
    (progn
      (when mir-bury-buffer-after-finishing
        (bury-buffer))
      (user-error "%s" "Queue is now empty: nothing to read"))))

;;;; Functions

;;;;; Public

;; Currently all the imports create new files. Is this what we want?
;; Probably yes.
;;
;; Name: ask the user, prefill the name of the buffer

(defun mir-import (text priority &optional name)
  (let* ((extension (mir--get-extension-to-current-buffer))
         (name (or name (mir--make-file-name)))
         (file-name (mir--format-file-name name nil extension))
         (file-id (denote-extract-id-from-string file-name)))
    (mir--add-topic-to-db file-id priority)
    (write-region text nil file-name)))

(defun mir-extract (text)
  (let* ((extension (mir--get-extension-to-current-buffer))
         (name (completing-read "Extract title: " nil nil nil (mir--make-file-name)))
         ;; TODO: folgezettel stuff
         (file-name (mir--format-file-name name '("extract") extension))
         (file-id (denote-extract-id-from-string file-name)))
    ;; maybe randomly subtract priority vals?
    (mir--add-topic-to-db file-id (nth 1 mir--current-topic))
    (write-region text nil file-name)))

(defun mir-get-topics-for-today-by-priority ()
  "Returns a list of topics due to review today, sorted by priority."
  (sqlite-select (mir--get-db)
                 "SELECT * FROM topics WHERE last_review IS NULL OR julianday('now') - julianday(last_review) >= interval ORDER BY priority DESC;"))

(defun mir-show-topic (topic)
  (setq mir--current-topic topic)
  (let ((denote-directory mir-archive-directory)
        (id (car topic)))
    (find-file (denote-get-path-by-id id))))

(defun mir-update-topic (topic)
  (let* ((id (car topic))
         (priority (nth 1 topic))
         (old-af (nth 2 topic))
         (old-interval (nth 3 topic))
         (new-af (if mir-scale-a-factor-by-priority
                     (+ 1.2 (/ priority 17.543859))
                   old-af))
         (new-interval (* old-interval new-af)))
    (sqlite-execute (mir--get-db)
                    "UPDATE topics SET last_review = date('now', 'localtime'), a_factor=?, interval= ? WHERE id = ?;" `(,new-af ,new-interval ,id))))

(defun mir-queue-next ()
  (setq mir-queue (funcall mir-query-function))
  (when mir-queue
      (pop mir-queue)))

(defun mir-set-a-factor ()
    (interactive))

(defun mir-reschedule ()
    (interactive))

(defun mir-set-priority ()
    (interactive))

;; TODO: extract clozes to anki
;; TODO: use autogenerated folgezettel to denote trees
;; Dismiss the current buffer if there's no more to read. Bury buffer?

;; After we're done with a piece, we can:
;; - use a reserved "archive" keyword
;; - use an "archive" extension
;; - or prepend the original extension with like ".archive.html"

;;;;; Private

(defun mir--get-db ()
  (sqlite-open mir-db-location))

(defun mir--init-db ()
  (sqlite-execute (mir--get-db)
                  "CREATE TABLE IF NOT EXISTS topics (id TEXT PRIMARY KEY, priority NOT NULL, a_factor NOT NULL, interval NOT NULL, added NOT NULL, last_review)"))

(defun mir--add-topic-to-db (id priority)
  (mir--init-db)
  (sqlite-execute (mir--get-db)
                  "INSERT INTO topics (id, priority, a_factor, interval, added) VALUES(?, ?, ?, ?, ?)"
                  `(,id ,priority ,mir-default-a-factor ,mir-default-interval ,(format-time-string "%Y-%m-%d"))))

(defun mir--format-file-name (name tags extension)
  "Wrapper around `denote-format-file-name' for now."
  (denote-format-file-name
   mir-archive-directory
   (denote-get-identifier (current-time))
   tags name extension nil))

(defun mir--get-extension-to-current-buffer ()
  "We assume that everything is .txt for now"
  ".txt")

;; It'd be cool to have a list of functions that process the buffer
;; name, this should probably be customizable.
(defun mir--make-file-name ()
  (completing-read "Topic title: " nil nil nil (buffer-name)))

;;;; Footer

(provide 'mir)

;;; package-name.el ends here
