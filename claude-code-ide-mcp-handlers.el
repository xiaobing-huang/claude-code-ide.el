;;; claude-code-ide-mcp-handlers.el --- MCP protocol handlers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, claude, mcp

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

;; This file implements handlers for MCP (Model Context Protocol) tools.
;; It provides implementations for file operations, editor state queries,
;; and workspace information that Claude Code CLI can use through the
;; MCP protocol.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)

;;; File Operations

(defun claude-code-ide-mcp-handle-list-files (params)
  "Handle listFiles tool request with PARAMS."
  (let* ((root (expand-file-name
                (or (alist-get 'root params)
                    default-directory)))
         (pattern (alist-get 'pattern params))
         (files '()))
    (when (file-directory-p root)
      (let ((default-directory root))
        (setq files
              (if pattern
                  ;; Use glob pattern matching
                  (file-expand-wildcards pattern t)
                ;; List all files recursively
                (directory-files-recursively root ".*" nil)))))
    ;; Convert to relative paths
    (mapcar (lambda (file)
              (file-relative-name file root))
            files)))

(defun claude-code-ide-mcp-handle-read-file (params)
  "Handle readFile tool request with PARAMS."
  (let ((path (expand-file-name (alist-get 'path params))))
    (if (file-readable-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
      (error "File not found or not readable: %s" path))))

(defun claude-code-ide-mcp-handle-write-file (params)
  "Handle writeFile tool request with PARAMS."
  (let ((path (expand-file-name (alist-get 'path params)))
        (content (alist-get 'content params)))
    (with-temp-file path
      (insert content))
    t))

(defun claude-code-ide-mcp-handle-edit-file (params)
  "Handle editFile tool request with PARAMS."
  (let ((path (expand-file-name (alist-get 'path params)))
        (old-text (alist-get 'oldText params))
        (new-text (alist-get 'newText params)))
    (if (file-readable-p path)
        (let ((content (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string))))
          (let ((new-content (replace-regexp-in-string
                              (regexp-quote old-text)
                              new-text
                              content
                              t t)))
            (if (string= content new-content)
                (error "Old text not found in file")
              (with-temp-file path
                (insert new-content))
              t)))
      (error "File not found: %s" path))))

(defun claude-code-ide-mcp-handle-open-file (params)
  "Handle openFile tool request with PARAMS."
  (let* ((path (expand-file-name (alist-get 'path params)))
         (line (alist-get 'line params))
         (buffer (find-file-noselect path)))
    (when buffer
      (with-current-buffer buffer
        (when (and line (numberp line))
          (goto-char (point-min))
          (forward-line (1- line))))
      (display-buffer buffer)
      t)))

(defun claude-code-ide-mcp-handle-save-document (params)
  "Handle saveDocument tool request with PARAMS."
  (let ((path (expand-file-name (alist-get 'path params))))
    (when-let ((buffer (get-file-buffer path)))
      (with-current-buffer buffer
        (save-buffer))
      t)))

(defun claude-code-ide-mcp-handle-close-tab (params)
  "Handle close_tab tool request with PARAMS."
  (let ((path (expand-file-name (alist-get 'path params))))
    (when-let ((buffer (get-file-buffer path)))
      (kill-buffer buffer)
      t)))

;;; Editor State

(defun claude-code-ide-mcp-handle-get-current-selection (_params)
  "Handle getCurrentSelection tool request."
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties
                 (region-beginning)
                 (region-end))))
      `((text . ,text)
        (filePath . ,(buffer-file-name))
        (startLine . ,(line-number-at-pos (region-beginning)))
        (endLine . ,(line-number-at-pos (region-end)))))))

(defun claude-code-ide-mcp-handle-get-open-editors (_params)
  "Handle getOpenEditors tool request."
  (let ((editors '()))
    (dolist (buffer (buffer-list))
      (when (buffer-file-name buffer)
        (push `((filePath . ,(buffer-file-name buffer))
                (fileName . ,(file-name-nondirectory (buffer-file-name buffer)))
                (language . ,(with-current-buffer buffer
                               (if (derived-mode-p 'prog-mode)
                                   (replace-regexp-in-string
                                    "-mode$" ""
                                    (symbol-name major-mode))
                                 "text"))))
              editors)))
    (vconcat (nreverse editors))))

(defun claude-code-ide-mcp-handle-get-active-editor (_params)
  "Handle getActiveEditor tool request."
  (when-let ((file (buffer-file-name)))
    `((filePath . ,file)
      (fileName . ,(file-name-nondirectory file))
      (language . ,(if (derived-mode-p 'prog-mode)
                       (replace-regexp-in-string
                        "-mode$" ""
                        (symbol-name major-mode))
                     "text")))))

;;; Workspace Information

(defun claude-code-ide-mcp-handle-get-workspace-folders (_params)
  "Handle getWorkspaceFolders tool request."
  (let ((folders '()))
    ;; Add current project if any
    (when-let ((project (project-current)))
      (push `((uri . ,(concat "file://" (project-root project)))
              (name . ,(file-name-nondirectory
                        (directory-file-name (project-root project)))))
            folders))
    ;; Add unique project roots from all buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let ((project (project-current))
                   (root (project-root project)))
          (unless (cl-find root folders
                           :key (lambda (f) (substring (alist-get 'uri f) 7))
                           :test #'string=)
            (push `((uri . ,(concat "file://" root))
                    (name . ,(file-name-nondirectory
                              (directory-file-name root))))
                  folders)))))
    (vconcat folders)))

;;; Tool Registration

(defvar claude-code-ide-mcp-tools
  '(("listFiles" . claude-code-ide-mcp-handle-list-files)
    ("readFile" . claude-code-ide-mcp-handle-read-file)
    ("writeFile" . claude-code-ide-mcp-handle-write-file)
    ("editFile" . claude-code-ide-mcp-handle-edit-file)
    ("openFile" . claude-code-ide-mcp-handle-open-file)
    ("saveDocument" . claude-code-ide-mcp-handle-save-document)
    ("close_tab" . claude-code-ide-mcp-handle-close-tab)
    ("getCurrentSelection" . claude-code-ide-mcp-handle-get-current-selection)
    ("getOpenEditors" . claude-code-ide-mcp-handle-get-open-editors)
    ("getActiveEditor" . claude-code-ide-mcp-handle-get-active-editor)
    ("getWorkspaceFolders" . claude-code-ide-mcp-handle-get-workspace-folders))
  "Alist mapping tool names to handler functions.")

(defun claude-code-ide-mcp-handle-tool (tool-name params)
  "Handle MCP tool request for TOOL-NAME with PARAMS."
  (let ((handler (alist-get tool-name claude-code-ide-mcp-tools nil nil #'string=)))
    (if handler
        (funcall handler params)
      (error "Unknown tool: %s" tool-name))))

(provide 'claude-code-ide-mcp-handlers)

;;; claude-code-ide-mcp-handlers.el ends here
