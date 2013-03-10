;;;; ido-ql-quickload.el
;;;;
;;;; ido-ql-quickload is available under the MIT license;
;;;; see LICENSE for details.
;;;;
;;;; For a detailed introduction see: README.md
;;;;
;;;; Copyright (C) 2013 Sviridov Alexander <sviridov.vmi@gmail.com>

(require 'ido)

;;;=================================================================================================

(defvar ido-ql-quickload-save-file "~/.ido-ql-quickload"
  "File in which the ido-ql-quickload state is saved between Emacs sessions.")

(defvar ido-ql-quickload-statistics (make-hash-table :test 'equal)
  "Variable in which the ido-ql-quickload statistics is stored")

(defvar ido-ql-quickload-history nil
  "Variable in which the ido-ql-quickload history is stored")

(defvar ido-ql-quickload-max-history-size 5
  "Variable that defines the ido-ql-quickload history maximum size")

(defvar ido-ql-quickload-ignore-local-projects-priority nil
  "If IDO-QL-QUICKLOAD-IGNORE-LOCAL-PROJECTS-PRIORITY is T then (QL:QUICKLOAD) doesn't
   take into account the location of the projects")

;;;=================================================================================================

(defun ido-ql-quickload-drop-extra-history-items ()
  "Drops extra items from tail of IDO-QL-QUICKLOAD-HISTORY"
  (when (< ido-ql-quickload-max-history-size (length ido-ql-quickload-history))
    (setf ido-ql-quickload-history 
          (butlast ido-ql-quickload-history 
                   (- (length ido-ql-quickload-history)
                      (max 0 ido-ql-quickload-max-history-size))))))

;;;=================================================================================================

(defun ido-ql-quickload-initialize ()
  "Reading the contents of the IDO-QL-QUICKLOAD-SAVE-FILE
   into IDO-QL-QUICKLOAD-HISTORY and IDO-QL-QUICKLOAD-STATISTICS"
  (when (file-readable-p ido-ql-quickload-save-file)
    (with-temp-buffer
      (insert-file-contents ido-ql-quickload-save-file)
      (setf ido-ql-quickload-history (read (current-buffer))
            ido-ql-quickload-statistics (read (current-buffer))))

    ;; In case the user has reduced the value of the IDO-QL-QUICKLOAD-MAX-HISTORY-SIZE
    ;; between sessions
    (ido-ql-quickload-drop-extra-history-items)))

;;;=================================================================================================

(defun ido-ql-quickload-update-system-score (system)
  "Increments SYSTEM score.
   For new SYSTEM sets score to 1"
  (setf (gethash system ido-ql-quickload-statistics)
        (+ 1 (gethash system ido-ql-quickload-statistics 0))))

;;;=================================================================================================

(defun ido-ql-quickload-update-history (system)
  "Moves SYSTEM to first position at IDO-QL-QUICKLOAD-HISTORY.
   If (LENGTH IDO-QL-QUICKLOAD-HISTORY) = IDO-QL-QUICKLOAD-MAX-HISTORY-SIZE
   and (NOT (MEMBER SYSTEM IDO-QL-QUICKLOAD-HISTORY)) drops the last item of
   IDO-QL-QUICKLOAD-HISTORY"
  (when (plusp ido-ql-quickload-max-history-size)
    (setf ido-ql-quickload-history (cons system (remove system ido-ql-quickload-history)))
    (ido-ql-quickload-drop-extra-history-items)))

;;;=================================================================================================

(defun ido-ql-quickload-save-to-file ()
  "Saves IDO-QL-QUICKLOAD-HISTORY and IDO-QL-QUICKLOAD-STATISTICS 
   into IDO-QL-QUICKLOAD-SAVE-FILE"
  (interactive)
  (with-temp-file (expand-file-name ido-ql-quickload-save-file)
    (print ido-ql-quickload-history (current-buffer))
    (print ido-ql-quickload-statistics (current-buffer))))
    
(add-hook 'kill-emacs-hook 'ido-ql-quickload-save-to-file)

;;;=================================================================================================

(defun ido-ql-quickload-sort-systems-names (systems-names)
  "Sorts SYSTEMS-NAMES list by:
   1. Score
   2. Aplhabet"
  (let ((grouped-by-score-names-table (make-hash-table))
        (result nil))
    (dolist (system-name systems-names)
      (push system-name
            (gethash (gethash system-name ido-ql-quickload-statistics 0) 
                     grouped-by-score-names-table)))
    (maphash (lambda (score names)
               (push (cons score (sort names #'string-lessp)) result))
      grouped-by-score-names-table)
    (mapcan #'rest 
      (sort result (lambda (prev next) (> (car prev) (car next)))))))

;;;=================================================================================================

(defun ido-ql-quickload-select-system ()
  "Asks the user to select SYSTEM to QL-QUICKLOAD with IDO.
   Systems are sorted in order:
   1. <= IDO-QL-QUICKLOAD-MAX-HISTORY-SIZE number of last selected systems
   2. Quicklisp local systems sorted by score and name
   3. Other Quicklisp systems sorted by score and name"
  (let* ((ido-enable-flex-matching t)

         (local-systems (nset-difference (slime-eval '(ql:list-local-systems))
                                         ido-ql-quickload-history
                                         :test #'string-equal))

         (quicklisp-systems (nset-difference
                             (slime-eval '(cl:mapcar (cl:function ql-dist:name) 
                                                     (ql:system-list)))
                             (append ido-ql-quickload-history
                                     local-systems)
                             :test #'string-equal))

         (systems-list (if ido-ql-quickload-ignore-local-projects-priority
                           (append ido-ql-quickload-history
                                   (ido-ql-quickload-sort-systems-names
                                     (append local-systems quicklisp-systems)))
                           (append ido-ql-quickload-history
                                   (ido-ql-quickload-sort-systems-names local-systems)
                                   (ido-ql-quickload-sort-systems-names quicklisp-systems))))

         (system (ido-completing-read "" systems-list)))

    (ido-ql-quickload-update-system-score system)
    (ido-ql-quickload-update-history system)

    system))

;;;=================================================================================================

(defun ql:quickload ()
  (interactive)
  (let ((slime-buffer (find-if (lambda (buffer) (string-match-p "slime-repl" (buffer-name buffer))) 
                               (buffer-list)))
        (buffer (current-buffer)))
    (switch-to-buffer slime-buffer)
    (end-of-buffer)
    (insert "(ql:quickload :")
    (let ((system (ido-ql-quickload-select-system)))
      (end-of-line)
      (insert system))
    (insert ")")
    (execute-kbd-macro (read-kbd-macro "RET"))
    (switch-to-buffer buffer)))

;;;=================================================================================================

(provide 'ido-ql-quickload)

;;;=================================================================================================
