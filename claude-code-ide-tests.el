;;; claude-code-ide-tests.el --- Tests for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Test suite for claude-code-ide.el using ERT
;;
;; Run tests with:
;;   `emacs -batch -L . -l ert -l claude-code-ide-tests.el -f ert-run-tests-batch-and-exit'

;;; Code:

(require 'ert)

;;; Mock Implementations

;; === Mock vterm module ===
(defvar vterm--process nil)
(defvar vterm-buffer-name nil)
(defvar vterm-shell nil)
(defvar vterm-environment nil)

(defun vterm (&optional buffer-name)
  "Mock vterm function for testing with optional BUFFER-NAME."
  (let ((buffer (generate-new-buffer (or buffer-name vterm-buffer-name "*vterm*"))))
    (with-current-buffer buffer
      ;; Create a mock process that exits immediately
      (setq vterm--process (make-process :name "mock-vterm"
                                         :buffer buffer
                                         :command '("true")
                                         :connection-type 'pty
                                         :sentinel (lambda (_ event)
                                                     (when (string-match "finished" event)
                                                       (setq vterm--process nil))))))
    buffer))

;; Mock vterm functions
(defun vterm-send-string (_string)
  "Mock vterm-send-string function for testing."
  nil)

(defun vterm-send-return ()
  "Mock vterm-send-return function for testing."
  nil)

(defun vterm-send-key (_key &optional _shift _meta _ctrl)
  "Mock vterm-send-key function for testing."
  nil)

(provide 'vterm)

;; === Mock Emacs display functions ===
(unless (fboundp 'display-buffer-in-side-window)
  (defun display-buffer-in-side-window (buffer _alist)
    "Mock display-buffer-in-side-window for testing."
    (set-window-buffer (selected-window) buffer)
    (selected-window)))

;; === Load required modules ===
(require 'claude-code-ide)

;;; Test Helper Functions

(defmacro claude-code-ide-tests--with-mocked-cli (cli-path &rest body)
  "Execute BODY with claude CLI path set to CLI-PATH."
  `(let ((claude-code-ide-cli-path ,cli-path)
         (claude-code-ide--cli-available nil))
     ,@body))

(defun claude-code-ide-tests--with-temp-directory (test-body)
  "Execute TEST-BODY in a temporary directory context.
Creates a temporary directory, sets it as `default-directory',
executes TEST-BODY, and ensures cleanup even if TEST-BODY fails."
  (let ((temp-dir (make-temp-file "claude-code-ide-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          (funcall test-body))
      (delete-directory temp-dir t))))

(defun claude-code-ide-tests--clear-processes ()
  "Clear the process hash table for testing.
Ensures a clean state before each test that involves process management."
  (clrhash claude-code-ide--processes))

(defun claude-code-ide-tests--wait-for-process (buffer)
  "Wait for the process in BUFFER to finish.
This prevents race conditions in tests by ensuring mock processes
have completed before cleanup.  Waits up to 5 seconds."
  (with-current-buffer buffer
    (let ((max-wait 50)) ; 5 seconds max (50 * 0.1s)
      (while (and vterm--process
                  (process-live-p vterm--process)
                  (> max-wait 0))
        (sleep-for 0.1)
        (setq max-wait (1- max-wait))))))

;;; Tests for Helper Functions

(ert-deftest claude-code-ide-test-default-buffer-name ()
  "Test default buffer name generation for various path formats."
  ;; Normal path
  (should (equal (claude-code-ide--default-buffer-name "/home/user/project")
                 "*claude-code[project]*"))
  ;; Path with trailing slash
  (should (equal (claude-code-ide--default-buffer-name "/home/user/my-app/")
                 "*claude-code[my-app]*"))
  ;; Root directory
  (should (equal (claude-code-ide--default-buffer-name "/")
                 "*claude-code[]*"))
  ;; Path with spaces
  (should (equal (claude-code-ide--default-buffer-name "/home/user/my project/")
                 "*claude-code[my project]*"))
  ;; Path with special characters
  (should (equal (claude-code-ide--default-buffer-name "/home/user/my-project@v1.0/")
                 "*claude-code[my-project@v1.0]*")))

(ert-deftest claude-code-ide-test-get-working-directory ()
  "Test working directory detection."
  (claude-code-ide-tests--with-temp-directory
   (lambda ()
     ;; Without project, should return current directory
     (let ((expected (expand-file-name default-directory)))
       (should (equal (claude-code-ide--get-working-directory) expected))))))

(ert-deftest claude-code-ide-test-get-buffer-name ()
  "Test buffer name generation using custom function."
  ;; Test with custom function
  (let ((claude-code-ide-buffer-name-function
         (lambda (dir) (format "test-%s" (file-name-nondirectory dir)))))
    (claude-code-ide-tests--with-temp-directory
     (lambda ()
       (should (string-match "^test-claude-code-ide-test-"
                             (claude-code-ide--get-buffer-name))))))

  ;; Test that nil directory is handled correctly
  (let ((claude-code-ide-buffer-name-function
         (lambda (dir) (if dir
                           (format "*custom[%s]*" (file-name-nondirectory dir))
                         "*custom[none]*"))))
    (should (equal (funcall claude-code-ide-buffer-name-function nil)
                   "*custom[none]*"))))

(ert-deftest claude-code-ide-test-process-management ()
  "Test process storage and retrieval."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         (let ((dir (claude-code-ide--get-working-directory))
               (mock-process 'mock-process))
           ;; Initially no process
           (should (null (claude-code-ide--get-process dir)))

           ;; Set a process
           (claude-code-ide--set-process mock-process dir)
           (should (eq (claude-code-ide--get-process dir) mock-process))

           ;; Get process without specifying directory
           (should (eq (claude-code-ide--get-process) mock-process)))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-cleanup-dead-processes ()
  "Test cleanup of dead processes."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let* ((live-process (make-process :name "test-live"
                                         :command '("sleep" "10")
                                         :buffer nil))
             (dead-process-name "test-dead"))
        ;; Create a mock dead process
        (puthash "/dir1" live-process claude-code-ide--processes)
        (puthash "/dir2" dead-process-name claude-code-ide--processes)

        ;; Before cleanup
        (should (= (hash-table-count claude-code-ide--processes) 2))

        ;; Run cleanup
        (claude-code-ide--cleanup-dead-processes)

        ;; After cleanup - only live process remains
        (should (= (hash-table-count claude-code-ide--processes) 1))
        (should (gethash "/dir1" claude-code-ide--processes))
        (should (null (gethash "/dir2" claude-code-ide--processes)))

        ;; Clean up the live process
        (delete-process live-process))
    (claude-code-ide-tests--clear-processes)))

;;; Tests for CLI Detection

(ert-deftest claude-code-ide-test-detect-cli ()
  "Test CLI detection mechanism."
  (let ((claude-code-ide--cli-available nil))
    ;; Test with invalid CLI path
    (let ((claude-code-ide-cli-path "nonexistent-claude-cli"))
      (claude-code-ide--detect-cli)
      (should (null claude-code-ide--cli-available)))

    ;; Test with valid command (echo exists on most systems)
    (let ((claude-code-ide-cli-path "echo"))
      (claude-code-ide--detect-cli)
      (should claude-code-ide--cli-available))))

(ert-deftest claude-code-ide-test-ensure-cli ()
  "Test CLI availability checking."
  (let ((claude-code-ide--cli-available nil)
        (claude-code-ide-cli-path "echo"))
    ;; Initially not available
    (should (null claude-code-ide--cli-available))

    ;; After ensure, should be detected
    (should (claude-code-ide--ensure-cli))
    (should claude-code-ide--cli-available)))

;;; Command Tests

(ert-deftest claude-code-ide-test-run-without-cli ()
  "Test run command when CLI is not available."
  (let ((claude-code-ide--cli-available nil)
        (claude-code-ide-cli-path "nonexistent-claude-cli"))
    (should-error (claude-code-ide)
                  :type 'user-error)))

(ert-deftest claude-code-ide-test-run-without-vterm ()
  "Test run command when vterm is not available."
  (let ((claude-code-ide--cli-available t)
        (claude-code-ide-cli-path "echo")
        (orig-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (if (eq sym 'vterm) nil (funcall orig-fboundp sym)))))
      (should-error (claude-code-ide)
                    :type 'user-error))))

(ert-deftest claude-code-ide-test-run-with-cli ()
  "Test successful run command execution."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         (let ((claude-code-ide--cli-available t)
               (claude-code-ide-cli-path "echo"))
           ;; Run claude-code-ide
           (claude-code-ide)

           ;; Check that buffer was created
           (let ((buffer-name (claude-code-ide--get-buffer-name)))
             (should (get-buffer buffer-name))

             ;; Check that process was registered
             (should (claude-code-ide--get-process))

             ;; Wait for process to finish and clean up
             (claude-code-ide-tests--wait-for-process (get-buffer buffer-name))
             ;; Kill the buffer explicitly since we're in batch mode
             (when (get-buffer buffer-name)
               (kill-buffer buffer-name))))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-run-existing-session ()
  "Test run command when session already exists."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         (let ((claude-code-ide--cli-available t)
               (claude-code-ide-cli-path "echo"))
           ;; Start first session
           (claude-code-ide)
           (let* ((buffer-name (claude-code-ide--get-buffer-name))
                  (first-buffer (get-buffer buffer-name)))

             ;; Verify we have the buffer
             (should first-buffer)

             ;; Try to run again - should not create new buffer
             (claude-code-ide)

             ;; Should still have same buffer
             (should (eq (get-buffer buffer-name) first-buffer))

             ;; Wait for process and clean up
             (claude-code-ide-tests--wait-for-process first-buffer)
             (kill-buffer first-buffer)))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-check-status ()
  "Test status check command."
  (let ((claude-code-ide-cli-path "echo")
        (claude-code-ide--cli-available nil))
    ;; Should not error and should detect CLI
    (claude-code-ide-check-status)
    (should claude-code-ide--cli-available)))

(ert-deftest claude-code-ide-test-stop-no-session ()
  "Test stop command when no session is running."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         ;; Should not error when no session exists
         (claude-code-ide-stop)))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-stop-with-session ()
  "Test stop command with active session."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         (let ((claude-code-ide--cli-available t)
               (claude-code-ide-cli-path "echo"))
           ;; Start a session
           (claude-code-ide)
           (let ((buffer-name (claude-code-ide--get-buffer-name)))
             ;; Verify session exists
             (should (get-buffer buffer-name))
             (should (claude-code-ide--get-process))

             ;; Wait for process to finish before stopping
             (claude-code-ide-tests--wait-for-process (get-buffer buffer-name))

             ;; Stop the session
             (claude-code-ide-stop)

             ;; Verify session is stopped
             (should (null (get-buffer buffer-name)))
             (should (null (claude-code-ide--get-process)))))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-switch-to-buffer-no-session ()
  "Test `switch-to-buffer' command when no session exists."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (should-error (claude-code-ide-switch-to-buffer)
                    :type 'user-error)
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-toggle-window-functionality ()
  "Test that running claude-code-ide on an existing session toggles the window."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (claude-code-ide-tests--with-temp-directory
       (lambda ()
         (let ((claude-code-ide--cli-available t)
               (claude-code-ide-cli-path "echo")
               (test-dir default-directory))
           ;; Start a session
           (claude-code-ide)
           (let* ((buffer-name (claude-code-ide--get-buffer-name))
                  (session-buffer (get-buffer buffer-name)))

             ;; Verify we have the buffer
             (should session-buffer)

             ;; Simulate window being visible (in batch mode we can't test actual windows)
             ;; Just verify the command runs without error when session exists
             (let ((default-directory test-dir))
               ;; Running claude-code-ide again should toggle (not error)
               (claude-code-ide))

             ;; Wait for process and clean up
             (claude-code-ide-tests--wait-for-process session-buffer)
             (kill-buffer session-buffer)))))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-list-sessions-empty ()
  "Test listing sessions when none exist."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      ;; Should not error when no sessions exist
      (claude-code-ide-list-sessions)
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-list-sessions-with-sessions ()
  "Test listing sessions functionality."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (progn
        ;; Test that list-sessions works with no sessions
        (claude-code-ide-list-sessions)

        ;; Manually add mock entries to the process table
        (puthash "/tmp/project1" (current-buffer) claude-code-ide--processes)
        (puthash "/tmp/project2" (current-buffer) claude-code-ide--processes)

        ;; Verify we have 2 entries
        (should (= (hash-table-count claude-code-ide--processes) 2))

        ;; List sessions should work without error
        (claude-code-ide-list-sessions))
    (claude-code-ide-tests--clear-processes)))

;;; Edge Case Tests

(ert-deftest claude-code-ide-test-concurrent-sessions ()
  "Test managing multiple concurrent sessions."
  (claude-code-ide-tests--clear-processes)
  (unwind-protect
      (let ((claude-code-ide--cli-available t)
            (claude-code-ide-cli-path "echo")
            (dir1 (make-temp-file "claude-test-1" t))
            (dir2 (make-temp-file "claude-test-2" t)))
        ;; Start sessions in different directories
        (let ((default-directory dir1))
          (claude-code-ide)
          (should (claude-code-ide--get-process dir1)))
        (let ((default-directory dir2))
          (claude-code-ide)
          (should (claude-code-ide--get-process dir2)))
        ;; Verify both sessions exist
        (should (= (hash-table-count claude-code-ide--processes) 2))
        ;; Clean up
        (let ((buffers (mapcar (lambda (dir)
                                 (funcall claude-code-ide-buffer-name-function dir))
                               (list dir1 dir2))))
          (dolist (buffer-name buffers)
            (when-let ((buffer (get-buffer buffer-name)))
              (claude-code-ide-tests--wait-for-process buffer)
              (kill-buffer buffer))))
        (delete-directory dir1 t)
        (delete-directory dir2 t))
    (claude-code-ide-tests--clear-processes)))

(ert-deftest claude-code-ide-test-custom-buffer-naming ()
  "Test custom buffer naming function."
  (let ((claude-code-ide-buffer-name-function
         (lambda (dir)
           (format "TEST-%s"
                   (upcase (file-name-nondirectory (directory-file-name dir)))))))
    (claude-code-ide-tests--with-temp-directory
     (lambda ()
       (let ((expected (format "TEST-%s"
                               (upcase (file-name-nondirectory
                                        (directory-file-name default-directory))))))
         (should (equal (claude-code-ide--get-buffer-name) expected)))))))

(ert-deftest claude-code-ide-test-window-placement-options ()
  "Test different window placement configurations."
  (dolist (side '(left right top bottom))
    (let ((claude-code-ide-window-side side))
      ;; Just verify the setting is accepted
      (should (eq claude-code-ide-window-side side)))))

(ert-deftest claude-code-ide-test-error-handling ()
  "Test error handling in various scenarios."
  ;; Test with nil CLI path
  (let ((claude-code-ide-cli-path nil)
        (claude-code-ide--cli-available nil))
    (should-error (claude-code-ide) :type 'user-error))

  ;; Test with empty CLI path
  (let ((claude-code-ide-cli-path "")
        (claude-code-ide--cli-available nil))
    (should-error (claude-code-ide) :type 'user-error)))

;;; Run all tests

(defun claude-code-ide-run-tests ()
  "Run all claude-code-ide test cases."
  (interactive)
  (ert-run-tests-batch-and-exit "^claude-code-ide-test-"))

(provide 'claude-code-ide-tests)

;;; claude-code-ide-tests.el ends here
