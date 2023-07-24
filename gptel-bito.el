;;; gptel-bito.el --- Bito support for GPTel         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Karthik Chikmagalur

;; Author: Karthik Chikmagalur;; <karthikchikmagalur@gmail.com>
;; Keywords: convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Bito support for GPTel.  Utility functions.

;;; Code:

(require 'gptel)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'ansi-color)

(defvar gptel-bito--process-alist nil
  "Alist of active GPTel Bito requests.")

(defvar gptel-bito--char-map '((?\n . "\\n") (?\" . "\\\"") (?\[ . "\\\[") (?\] . "\\\]")))

(defvar gptel-bito--char-filter '((?\r . "")))

(defvar gptel-bito--prompt-path (concat (file-name-directory load-file-name) "bito-templates/generate.txt")
  "Prompt file path for Bito.")

(defun gptel-bito--map-char (str char-map)
  (cl-loop for c across str
         if (alist-get c char-map)
         concat (alist-get c char-map)
         else
         concat (string c)))

(defun gptel-bito--make-string (question answer)
  (format "{\"question\":%s,\"answer\":%s}\n" (json-encode-string question) (json-encode-string answer)))

(defun gptel-bito--parse-prompts (prompts)
  "Produce list of arguments for calling bito.

PROMPTS is the data to parse."
  (let (bito-questions bito-answers bito-pending)
    (dolist (prompt prompts)
      (let ((role (plist-get prompt :role))
            (content (plist-get prompt :content)))
        (when (string= role "system")
          (push content bito-questions)
          (push "OK" bito-answers))
        (when (string= role "user")
          (push content bito-questions))
        (when (string= role "assistant")
          (push content bito-answers))))
    (setq bito-pending (pop bito-questions))
    (setq bito-questions (nreverse bito-questions))
    (setq bito-answers (nreverse bito-answers))
    (list :context (mapconcat 'identity (cl-mapcar #'gptel-bito--make-string bito-questions bito-answers) nil) :pending bito-pending)
    ))

(defun gptel-bito--get-args (prompts)
  "Produce list of arguments for calling bito.

PROMPTS is the data to send, TOKEN is a unique identifier."
  (let* ((bito-prompts (gptel-bito--parse-prompts prompts))
         (bito-context (plist-get bito-prompts :context))
         (bito-pending (plist-get bito-prompts :pending))
         args)
    ;; context file
    (when bito-context
      (let ((temp-file (make-temp-file "emacs-bito-context-" nil ".txt")))
        (with-temp-file temp-file
          (insert "{\"bitoclicontext_v\":\"1.0\",\"session_id\":\"20d42132-0630-47b0-8245-69782856380b\"}\n")
          (insert bito-context))
        (push "-c" args)
        (push temp-file args)))

    ;; input file
    (let ((temp-file (make-temp-file "emacs-bito-pending-" nil ".txt")))
        (with-temp-file temp-file
          (insert bito-pending))
        (push "-f" args)
        (push temp-file args))

    ;; prompt file
    (push "-p" args)
    (push gptel-bito--prompt-path args)
    (nreverse args)))

;;TODO: The :transformer argument here is an alternate implementation of
;;`gptel-response-filter-functions'. The two need to be unified.
;;;###autoload
(defun gptel-bito-get-response (info &optional callback)
  "Retrieve response to prompt in INFO.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :buffer (the gptel buffer)
- :position (marker at which to insert the response).

  Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (let* ((token (md5 (format "%s%s%s%s"
                             (random) (emacs-pid) (user-full-name)
                             (recent-keys))))
         (args (gptel-bito--get-args (plist-get info :prompt)))
         (process (apply #'start-process "gptel-bito"
                         (generate-new-buffer "*gptel-bito*") "bito" args)))
    (with-current-buffer (process-buffer process)
      (set-process-query-on-exit-flag process nil)
      (setf (alist-get process gptel-bito--process-alist)
            (nconc (list :token token
                         :callback (or callback
                                       #'gptel-bito--stream-insert-response)
                         :transformer (when (or (eq gptel-default-mode 'org-mode)
                                                (eq (buffer-local-value
                                                     'major-mode
                                                     (plist-get info :buffer))
                                                    'org-mode))
                                        (gptel--stream-convert-markdown->org)))
                   info))
      
      (progn (set-process-sentinel process #'gptel-bito--stream-cleanup)
             (set-process-filter process #'gptel-bito--stream-filter))
      )))


(defun gptel-bito--stream-insert-response (response info)
  "Insert streaming RESPONSE from ChatGPT into the gptel buffer.

INFO is a mutable plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  (let ((start-marker (plist-get info :position))
        (tracking-marker (plist-get info :tracking-marker))
        (transformer (plist-get info :transformer)))
    (when response
        (with-current-buffer (marker-buffer start-marker)
          (save-excursion
            (unless tracking-marker
              (gptel--update-header-line " Typing..." 'success)
              (goto-char start-marker)
              (unless (or (bobp) (plist-get info :in-place))
                (insert "\n\n"))
              (insert "[ai]")
              (setq tracking-marker (set-marker (make-marker) (point)))
              (set-marker-insertion-type tracking-marker t)
              (plist-put info :tracking-marker tracking-marker))
            
            (when transformer
              (setq response (funcall transformer response)))
            
            (goto-char tracking-marker)
            (insert response))))))

(defun gptel-bito--stream-filter (process output)
  (let* ((proc-info (alist-get process gptel-bito--process-alist)))
    (with-current-buffer (process-buffer process)
      ;; Insert output
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point)))
      
      ;; Handle read-only gptel buffer
      (when (with-current-buffer (plist-get proc-info :buffer)
              (or buffer-read-only
                  (get-char-property (plist-get proc-info :position) 'read-only)))
        (message "Buffer is read only, displaying reply in buffer \"*ChatGPT response*\"")
        (display-buffer
         (with-current-buffer (get-buffer-create "*ChatGPT response*")
           (goto-char (point-max))
           (move-marker (plist-get proc-info :position) (point) (current-buffer))
           (current-buffer))
         '((display-buffer-reuse-window
            display-buffer-pop-up-window)
           (reusable-frames . visible))))
      
      ;; run callback with output
      (funcall (or (plist-get proc-info :callback)
                   #'gptel-bito--stream-insert-response)
               (gptel-bito--map-char (ansi-color-filter-apply output) gptel-bito--char-filter)
               proc-info))))

(defun gptel-bito--stream-cleanup (process status)
  "Process sentinel for GPTel bito requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when gptel--debug
      (with-current-buffer proc-buf
        (clone-buffer "*gptel-error*" 'show)))
    
    (let* ((info (alist-get process gptel-bito--process-alist))
           (gptel-buffer (plist-get info :buffer))
           (tracking-marker (plist-get info :tracking-marker))
           (start-marker (plist-get info :position))
           (ok? (string= status "finished\n")))
      (if ok?
          (progn
            ;; Finish handling response
            (with-current-buffer (marker-buffer start-marker)
              (pulse-momentary-highlight-region (+ start-marker 2) tracking-marker)
              (save-excursion (goto-char tracking-marker)
                              (insert "[/ai]")
                              (if gptel-mode (insert "\n\n" (gptel-prompt-string))
                                (insert "\n")))
              )
            (with-current-buffer gptel-buffer
              (when gptel-mode (gptel--update-header-line  " Ready" 'success))))
        ;; Or message error
        (message "Bito error (%s)." status)
        (with-current-buffer gptel-buffer
          (when gptel-mode
            (gptel--update-header-line
             (format " Bito Error: %s" status) 'error))))
      (with-current-buffer gptel-buffer
        (run-hooks 'gptel-post-response-hook)))
    
    (setf (alist-get process gptel-bito--process-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(provide 'gptel-bito)
;;; gptel-bito.el ends here