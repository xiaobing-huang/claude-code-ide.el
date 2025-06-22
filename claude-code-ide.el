;;; claude-code-ide.el --- Claude Code integration for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (vterm "0.0.1"))
;; Keywords: ai, claude, code, assistant
;; URL: https://github.com/manzaltu/claude-code-ide.el

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Claude Code IDE integration for Emacs provides seamless integration
;; with Claude Code CLI.
;;
;; This package manages Claude Code sessions within Emacs, providing
;; project-aware session management with automatic working directory detection.
;;
;; Features:
;; - Project-aware sessions with automatic working directory detection
;; - Terminal integration via vterm
;; - Multiple concurrent sessions per project
;; - Clean session management with automatic cleanup on exit
;;
;; Usage:
;; M-x claude-code-ide - Start Claude Code for current project
;; M-x claude-code-ide-stop - Stop Claude Code for current project
;; M-x claude-code-ide-switch-to-buffer - Switch to project's Claude buffer
;; M-x claude-code-ide-list-sessions - List and switch between all sessions
;; M-x claude-code-ide-check-status - Check CLI availability and version

;;; Code:

(require 'vterm)
(require 'cl-lib)
(require 'project)

;;; Customization

(defgroup claude-code-ide nil
  "Claude Code integration for Emacs."
  :group 'tools
  :prefix "claude-code-ide-")

(defcustom claude-code-ide-cli-path "claude"
  "Path to the Claude Code CLI executable."
  :type 'string
  :group 'claude-code-ide)

(defcustom claude-code-ide-buffer-name-function #'claude-code-ide--default-buffer-name
  "Function to generate buffer names for Claude Code sessions.
The function is called with one argument, the working directory,
and should return a string to use as the buffer name."
  :type 'function
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-side 'right
  "Side of the frame where the Claude Code window should appear.
Can be `'left', `'right', `'top', or `'bottom'."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-width 90
  "Width of the Claude Code side window when opened on left or right."
  :type 'integer
  :group 'claude-code-ide)

(defcustom claude-code-ide-window-height 20
  "Height of the Claude Code side window when opened on top or bottom."
  :type 'integer
  :group 'claude-code-ide)

(defcustom claude-code-ide-focus-on-open t
  "Whether to focus the Claude Code window when it opens."
  :type 'boolean
  :group 'claude-code-ide)

;;; Variables

(defvar claude-code-ide--cli-available nil
  "Whether Claude Code CLI is available and detected.")

(defvar claude-code-ide--processes (make-hash-table :test 'equal)
  "Hash table mapping project/directory roots to their Claude Code processes.")

;;; Helper Functions

(defun claude-code-ide--default-buffer-name (directory)
  "Generate default buffer name for DIRECTORY."
  (format "*claude-code[%s]*"
          (file-name-nondirectory (directory-file-name directory))))

(defun claude-code-ide--get-working-directory ()
  "Get the current working directory (project root or current directory)."
  (if-let ((project (project-current)))
      (expand-file-name (project-root project))
    (expand-file-name default-directory)))

(defun claude-code-ide--get-buffer-name (&optional directory)
  "Get the buffer name for the Claude Code session in DIRECTORY.
If DIRECTORY is not provided, use the current working directory."
  (funcall claude-code-ide-buffer-name-function
           (or directory (claude-code-ide--get-working-directory))))

(defun claude-code-ide--get-process (&optional directory)
  "Get the Claude Code process for DIRECTORY or current working directory."
  (gethash (or directory (claude-code-ide--get-working-directory))
           claude-code-ide--processes))

(defun claude-code-ide--set-process (process &optional directory)
  "Set the Claude Code PROCESS for DIRECTORY or current working directory."
  (puthash (or directory (claude-code-ide--get-working-directory))
           process
           claude-code-ide--processes))

(defun claude-code-ide--cleanup-dead-processes ()
  "Remove entries for dead processes from the process table."
  (maphash (lambda (directory process)
             (unless (process-live-p process)
               (remhash directory claude-code-ide--processes)))
           claude-code-ide--processes))

(defun claude-code-ide--display-buffer-in-side-window (buffer)
  "Display BUFFER in a side window according to customization.
The window is displayed on the side specified by
`claude-code-ide-window-side' with dimensions from
`claude-code-ide-window-width' or `claude-code-ide-window-height'.
If `claude-code-ide-focus-on-open' is non-nil, the window is selected."
  (let* ((side claude-code-ide-window-side)
         (slot 0)
         (window-parameters '((no-delete-other-windows . t)))
         (display-buffer-alist
          `((,(regexp-quote (buffer-name buffer))
             (display-buffer-in-side-window)
             (side . ,side)
             (slot . ,slot)
             (window-width . ,(if (memq side '(left right))
                                  claude-code-ide-window-width
                                'fit-window-to-buffer))
             (window-height . ,(if (memq side '(top bottom))
                                   claude-code-ide-window-height
                                 'fit-window-to-buffer))
             (window-parameters . ,window-parameters)))))
    (let ((window (display-buffer buffer)))
      ;; Select the window to give it focus if configured to do so
      (when (and window claude-code-ide-focus-on-open)
        (select-window window))
      window)))

(defvar claude-code-ide--cleanup-in-progress nil
  "Flag to prevent recursive cleanup calls.")

(defun claude-code-ide--cleanup-on-exit (directory)
  "Clean up process tracking when Claude exits for DIRECTORY."
  (unless claude-code-ide--cleanup-in-progress
    (setq claude-code-ide--cleanup-in-progress t)
    (unwind-protect
        (progn
          ;; Remove from process table
          (remhash directory claude-code-ide--processes)
          ;; Kill the vterm buffer if it exists
          (let ((buffer-name (claude-code-ide--get-buffer-name directory)))
            (when-let ((buffer (get-buffer buffer-name)))
              (when (buffer-live-p buffer)
                (let ((kill-buffer-hook nil) ; Disable hooks to prevent recursion
                      (kill-buffer-query-functions nil)) ; Don't ask for confirmation
                  (kill-buffer buffer)))))
          (message "Cleaned up Claude Code session for %s"
                   (file-name-nondirectory (directory-file-name directory))))
      (setq claude-code-ide--cleanup-in-progress nil))))

;;; CLI Detection

(defun claude-code-ide--detect-cli ()
  "Detect if Claude Code CLI is available."
  (let ((available (condition-case nil
                       (eq (call-process claude-code-ide-cli-path nil nil nil "--version") 0)
                     (error nil))))
    (setq claude-code-ide--cli-available available)))

(defun claude-code-ide--ensure-cli ()
  "Ensure Claude Code CLI is available, detect if needed."
  (unless claude-code-ide--cli-available
    (claude-code-ide--detect-cli))
  claude-code-ide--cli-available)

;;; Commands

(defun claude-code-ide--toggle-existing-window (existing-buffer)
  "Toggle visibility of EXISTING-BUFFER window.
If the window is visible, it will be hidden.
If the window is not visible, it will be shown in a side window."
  (let ((window (get-buffer-window existing-buffer)))
    (if window
        ;; Window is visible, hide it
        (progn
          (delete-window window)
          (message "Claude Code window hidden"))
      ;; Window is not visible, show it
      (progn
        (claude-code-ide--display-buffer-in-side-window existing-buffer)
        (message "Claude Code window shown")))))

(defun claude-code-ide--create-vterm-session (buffer-name working-dir)
  "Create a new vterm session for Claude Code.
BUFFER-NAME is the name for the vterm buffer.
WORKING-DIR is the working directory.

Returns a cons cell of (buffer . process) on success.
Signals an error if vterm fails to initialize."
  (let* ((vterm-buffer-name buffer-name)
         (default-directory working-dir)
         ;; vterm uses vterm-environment for passing env vars
         (vterm-environment vterm-environment))
    ;; Create vterm buffer without switching to it
    (let ((buffer (save-window-excursion
                    (vterm buffer-name))))
      ;; Check if vterm successfully created a buffer
      (unless buffer
        (error "Failed to create vterm buffer.  Please ensure vterm is properly installed"))
      ;; Send command to run Claude
      (with-current-buffer buffer
        (vterm-send-string (concat claude-code-ide-cli-path "\n")))
      ;; Get the process that vterm created
      (let ((process (get-buffer-process buffer)))
        (unless process
          (error "Failed to get vterm process.  The vterm buffer may not have initialized properly"))
        ;; Check if buffer is still alive
        (unless (buffer-live-p buffer)
          (error "Vterm buffer was killed during initialization"))
        (cons buffer process)))))

(defun claude-code-ide--start-session ()
  "Start a Claude Code session for the current project.

This function handles:
- CLI availability checking
- Dead process cleanup
- Existing session detection and window toggling
- New session creation
- Process and buffer lifecycle management"
  (unless (claude-code-ide--ensure-cli)
    (user-error "Claude Code CLI not available.  Please install it and ensure it's in PATH"))

  (unless (fboundp 'vterm)
    (user-error "The package vterm is not installed.  Please install the vterm package"))

  ;; Try to require vterm to ensure it's loaded
  (require 'vterm nil t)

  ;; Clean up any dead processes first
  (claude-code-ide--cleanup-dead-processes)

  (let* ((working-dir (claude-code-ide--get-working-directory))
         (buffer-name (claude-code-ide--get-buffer-name))
         (existing-buffer (get-buffer buffer-name))
         (existing-process (claude-code-ide--get-process working-dir)))

    ;; If buffer exists and process is alive, toggle the window
    (if (and existing-buffer
             (buffer-live-p existing-buffer)
             existing-process)
        (claude-code-ide--toggle-existing-window existing-buffer)
      ;; Create new vterm session
      (let* ((buffer-and-process (claude-code-ide--create-vterm-session
                                  buffer-name working-dir))
             (buffer (car buffer-and-process))
             (process (cdr buffer-and-process)))
        (claude-code-ide--set-process process working-dir)
        ;; Set up process sentinel to clean up when Claude exits
        (set-process-sentinel process
                              (lambda (_proc event)
                                (when (or (string-match "finished" event)
                                          (string-match "exited" event)
                                          (string-match "killed" event)
                                          (string-match "terminated" event))
                                  (claude-code-ide--cleanup-on-exit working-dir))))
        ;; Also add buffer kill hook as a backup
        (with-current-buffer buffer
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      (claude-code-ide--cleanup-on-exit working-dir))
                    nil t))
        ;; Display the buffer in a side window
        (claude-code-ide--display-buffer-in-side-window buffer)
        (message "Claude Code started in %s"
                 (file-name-nondirectory (directory-file-name working-dir)))))))

;;;###autoload
(defun claude-code-ide ()
  "Run Claude Code in a terminal for the current project or directory."
  (interactive)
  (claude-code-ide--start-session))

;;;###autoload
(defun claude-code-ide-check-status ()
  "Check Claude Code CLI status."
  (interactive)
  (claude-code-ide--detect-cli)
  (if claude-code-ide--cli-available
      (let ((version-output
             (with-temp-buffer
               (call-process claude-code-ide-cli-path nil t nil "--version")
               (buffer-string))))
        (message "Claude Code CLI version: %s" (string-trim version-output)))
    (message "Claude Code is not installed.")))

;;;###autoload
(defun claude-code-ide-stop ()
  "Stop the Claude Code session for the current project or directory."
  (interactive)
  (let* ((working-dir (claude-code-ide--get-working-directory))
         (buffer-name (claude-code-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (progn
          ;; Kill the buffer (cleanup will be handled by hooks)
          ;; The process sentinel will handle cleanup when the process dies
          (kill-buffer buffer)
          (message "Stopping Claude Code in %s..."
                   (file-name-nondirectory (directory-file-name working-dir))))
      (message "No Claude Code session is running in this directory"))))


;;;###autoload
(defun claude-code-ide-switch-to-buffer ()
  "Switch to the Claude Code buffer for the current project.
If the buffer is not visible, display it in the configured side window.
If the buffer is already visible, switch focus to it."
  (interactive)
  (let ((buffer-name (claude-code-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (if-let ((window (get-buffer-window buffer)))
            ;; Buffer is visible, just focus it
            (select-window window)
          ;; Buffer exists but not visible, display it
          (claude-code-ide--display-buffer-in-side-window buffer))
      (user-error "No Claude Code session for this project.  Use M-x claude-code-ide to start one"))))

;;;###autoload
(defun claude-code-ide-list-sessions ()
  "List all active Claude Code sessions and switch to selected one."
  (interactive)
  (claude-code-ide--cleanup-dead-processes)
  (let ((sessions '()))
    (maphash (lambda (directory _)
               (push (cons (abbreviate-file-name directory)
                           directory)
                     sessions))
             claude-code-ide--processes)
    (if sessions
        (let ((choice (completing-read "Switch to Claude Code session: "
                                       sessions nil t)))
          (when choice
            (let* ((directory (alist-get choice sessions nil nil #'string=))
                   (buffer-name (funcall claude-code-ide-buffer-name-function directory)))
              (if-let ((buffer (get-buffer buffer-name)))
                  (claude-code-ide--display-buffer-in-side-window buffer)
                (user-error "Buffer for session %s no longer exists" choice)))))
      (message "No active Claude Code sessions"))))

(provide 'claude-code-ide)

;;; claude-code-ide.el ends here
