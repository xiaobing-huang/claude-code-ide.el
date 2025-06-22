;;; claude-code-ide-mcp.el --- MCP server for Claude Code IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yoav Orot
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (websocket "1.12"))
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

;; This file implements an MCP (Model Context Protocol) server for Claude Code IDE.
;; It provides a WebSocket server that Claude CLI can connect to, handling JSON-RPC
;; messages and exposing Emacs functionality through MCP tools.

;;; Code:

(require 'websocket)
(require 'json)
(require 'cl-lib)
(require 'project)
(require 'url-parse)

;;; Constants

(defconst claude-code-ide-mcp-version "2024-11-05"
  "MCP protocol version.")

(defconst claude-code-ide-mcp-port-range '(10000 . 65535)
  "Port range for WebSocket server.")

(defconst claude-code-ide-mcp-max-port-attempts 100
  "Maximum number of attempts to find a free port.")

;;; Variables

;; Only keep the global sessions table
(defvar claude-code-ide-mcp--sessions (make-hash-table :test 'equal)
  "Hash table mapping project directories to MCP sessions.")

;;; Error Definition

(define-error 'mcp-error "MCP Error" 'error)

;;; Session Management

(cl-defstruct claude-code-ide-mcp-session
  "Structure to hold all state for a single MCP session."
  server           ; WebSocket server instance
  client           ; Connected WebSocket client
  port             ; Server port
  project-dir      ; Project directory
  deferred         ; Hash table of deferred responses
  last-buffer)     ; Last active buffer

(defun claude-code-ide-mcp--get-buffer-project ()
  "Get the project directory for the current buffer.
Returns the expanded project root path if a project is found,
otherwise returns nil."
  (when-let ((project (project-current)))
    (expand-file-name (project-root project))))

(defun claude-code-ide-mcp--get-session-for-project (project-dir)
  "Get the MCP session for PROJECT-DIR.
Returns the session structure if found, nil otherwise."
  (when project-dir
    (gethash project-dir claude-code-ide-mcp--sessions)))

(defun claude-code-ide-mcp--get-current-session ()
  "Get the MCP session for the current buffer's project.
This is a convenience function that combines
`claude-code-ide-mcp--get-buffer-project' and
`claude-code-ide-mcp--get-session-for-project'."
  (when-let ((project-dir (claude-code-ide-mcp--get-buffer-project)))
    (claude-code-ide-mcp--get-session-for-project project-dir)))

(defun claude-code-ide-mcp--find-session-by-websocket (ws)
  "Find the MCP session that owns the WebSocket WS.
Searches through all active sessions to find the one with matching client.
Returns the session if found, nil otherwise."
  (let ((found-session nil))
    (maphash (lambda (_project-dir session)
               (when (eq (claude-code-ide-mcp-session-client session) ws)
                 (setq found-session session)))
             claude-code-ide-mcp--sessions)
    found-session))

;;; Lockfile Management

(defun claude-code-ide-mcp--lockfile-directory ()
  "Return the directory for MCP lockfiles."
  (expand-file-name "~/.claude/ide/"))

(defun claude-code-ide-mcp--lockfile-path (port)
  "Return the lockfile path for PORT."
  (format "%s%d.lock" (claude-code-ide-mcp--lockfile-directory) port))

(defun claude-code-ide-mcp--create-lockfile (port project-dir)
  "Create a lockfile for PORT with server information for PROJECT-DIR."
  (let* ((lockfile-dir (claude-code-ide-mcp--lockfile-directory))
         (lockfile-path (claude-code-ide-mcp--lockfile-path port))
         (workspace-folders (vector project-dir))
         (lockfile-content `((pid . ,(emacs-pid))
                             (workspaceFolders . ,workspace-folders)
                             (ideName . "Emacs")
                             (transport . "ws"))))
    ;; Ensure directory exists
    (make-directory lockfile-dir t)
    ;; Write lockfile directly without temp file
    (condition-case err
        (with-temp-file lockfile-path
          (insert (json-encode lockfile-content)))
      (error
       (message "Failed to create lockfile: %s" err)
       (signal 'mcp-error (list (format "Failed to create lockfile: %s" (error-message-string err))))))))

(defun claude-code-ide-mcp--remove-lockfile (port)
  "Remove the lockfile for PORT."
  (when port
    (let ((lockfile-path (claude-code-ide-mcp--lockfile-path port)))
      (when (file-exists-p lockfile-path)
        (delete-file lockfile-path)))))


;;; JSON-RPC Message Handling

(defun claude-code-ide-mcp--make-response (id result)
  "Create a JSON-RPC response with ID and RESULT."
  `((jsonrpc . "2.0")
    (id . ,id)
    (result . ,result)))

(defun claude-code-ide-mcp--make-error-response (id code message &optional data)
  "Create a JSON-RPC error response with ID, CODE, MESSAGE and optional DATA."
  `((jsonrpc . "2.0")
    (id . ,id)
    (error . ((code . ,code)
              (message . ,message)
              ,@(when data `((data . ,data)))))))

(defun claude-code-ide-mcp--send-notification (method params)
  "Send a JSON-RPC notification with METHOD and PARAMS to the current session."
  ;; Try to find the session for the current buffer
  (when-let* ((project-dir (claude-code-ide-mcp--get-buffer-project))
              (session (claude-code-ide-mcp--get-session-for-project project-dir))
              (client (claude-code-ide-mcp-session-client session)))
    (let ((message `((jsonrpc . "2.0")
                     (method . ,method)
                     (params . ,params))))
      (condition-case err
          (websocket-send-text client (json-encode message))
        (error
         (message "Failed to send notification %s: %s" method err))))))

(defun claude-code-ide-mcp--handle-initialize (id _params)
  "Handle the initialize request with ID."
  (claude-code-ide-mcp--make-response
   id
   `((protocolVersion . ,claude-code-ide-mcp-version)
     (serverInfo . ((name . "claude-code-ide-mcp")
                    (version . "0.1.0")))
     (capabilities . ((tools . ((listFiles . t)
                                (readFile . t)
                                (writeFile . t)
                                (editFile . t)
                                (openFile . t)
                                (saveDocument . t)
                                (close_tab . t)
                                (openDiff . t)
                                (closeAllDiffTabs . t)
                                (getDiagnostics . t)
                                (getCurrentSelection . t)
                                (getOpenEditors . t)
                                (getWorkspaceFolders . t)
                                (getActiveEditor . t)
                                (getTabIds . t))))))))

(defun claude-code-ide-mcp--handle-ping (id _params)
  "Handle ping request with ID."
  (claude-code-ide-mcp--make-response id :json-empty))

(defun claude-code-ide-mcp--handle-unknown-method (id method)
  "Handle unknown METHOD with ID."
  (claude-code-ide-mcp--make-error-response
   id
   -32601
   (format "Method not found: %s" method)))

(defun claude-code-ide-mcp--handle-request (session id method params)
  "Handle JSON-RPC request with ID, METHOD and PARAMS for SESSION."
  (let ((response
         (pcase method
           ("initialize" (claude-code-ide-mcp--handle-initialize id params))
           ("ping" (claude-code-ide-mcp--handle-ping id params))
           (_ (claude-code-ide-mcp--handle-unknown-method id method)))))
    (when response
      (claude-code-ide-mcp--send-response session response))))

(defun claude-code-ide-mcp--send-response (session response)
  "Send RESPONSE through the WebSocket client of SESSION."
  (when-let ((client (claude-code-ide-mcp-session-client session)))
    (condition-case err
        (websocket-send-text client (json-encode response))
      (error
       (message "Failed to send response: %s" err)))))

(defun claude-code-ide-mcp--process-message (session message)
  "Process incoming MESSAGE string for SESSION."
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-array-type 'vector)
             (json-false :json-false)
             (json-null :json-null)
             (parsed (json-read-from-string message))
             (id (alist-get 'id parsed))
             (method (alist-get 'method parsed))
             (params (alist-get 'params parsed)))
        (if method
            ;; It's a request
            (claude-code-ide-mcp--handle-request session id method params)
          ;; It's a response - we don't handle these in this simple version
          (message "Received response with id %s" id)))
    (json-error
     (message "JSON parse error: %s" (error-message-string err)))
    (error
     (message "Error processing message: %s" (error-message-string err)))))

;;; WebSocket Server Functions

(defun claude-code-ide-mcp--find-free-port ()
  "Find a free port in the configured range."
  (let ((min-port (car claude-code-ide-mcp-port-range))
        (max-port (cdr claude-code-ide-mcp-port-range))
        (attempts 0))
    (while (< attempts claude-code-ide-mcp-max-port-attempts)
      (let ((port (+ min-port (random (- max-port min-port)))))
        (condition-case nil
            (let ((test-server (websocket-server
                                port
                                :host 'local
                                :on-open #'ignore
                                :on-message #'ignore
                                :on-close #'ignore
                                :on-error #'ignore)))
              ;; If we got here, the port is free
              (websocket-server-close test-server)
              (cl-return port))
          (error
           ;; Port is in use, try another
           (setq attempts (1+ attempts))))))
    (error "Could not find a free port after %d attempts" attempts)))

(defun claude-code-ide-mcp--on-open (ws)
  "Handle new WebSocket connection WS."
  ;; Find which session this websocket belongs to
  (let ((session nil))
    (maphash (lambda (_project-dir sess)
               (when (eq (claude-code-ide-mcp-session-server sess) (websocket-server ws))
                 (setq session sess)))
             claude-code-ide-mcp--sessions)
    (when session
      (setf (claude-code-ide-mcp-session-client session) ws)
      (message "Claude Code connected to MCP server"))))

(defun claude-code-ide-mcp--on-message (ws frame)
  "Handle incoming message FRAME from WebSocket WS."
  (when-let* ((session (claude-code-ide-mcp--find-session-by-websocket ws))
              (text (websocket-frame-text frame)))
    (claude-code-ide-mcp--process-message session text)))

(defun claude-code-ide-mcp--on-close (ws)
  "Handle WebSocket WS closing."
  (when-let ((session (claude-code-ide-mcp--find-session-by-websocket ws)))
    (setf (claude-code-ide-mcp-session-client session) nil)
    (message "Claude Code disconnected from MCP server")))

(defun claude-code-ide-mcp--on-error (ws type err)
  "Handle WebSocket WS error of TYPE with ERR."
  (message "WebSocket error (%s): %s" type (error-message-string err)))

;;; Public Functions

(defun claude-code-ide-mcp-start (project-dir)
  "Start MCP server for PROJECT-DIR and return the port number."
  ;; Check if session already exists
  (when-let ((existing-session (gethash project-dir claude-code-ide-mcp--sessions)))
    (error "MCP server already running for project %s on port %d"
           project-dir
           (claude-code-ide-mcp-session-port existing-session)))

  (let* ((port (claude-code-ide-mcp--find-free-port))
         (server (websocket-server
                  port
                  :host 'local
                  :on-open #'claude-code-ide-mcp--on-open
                  :on-message #'claude-code-ide-mcp--on-message
                  :on-close #'claude-code-ide-mcp--on-close
                  :on-error #'claude-code-ide-mcp--on-error))
         (session (make-claude-code-ide-mcp-session
                   :server server
                   :client nil
                   :port port
                   :project-dir project-dir
                   :deferred (make-hash-table :test 'equal))))
    ;; Store session
    (puthash project-dir session claude-code-ide-mcp--sessions)
    ;; Create lockfile
    (claude-code-ide-mcp--create-lockfile port project-dir)
    (message "MCP server started on port %d for %s" port project-dir)
    port))

(defun claude-code-ide-mcp-stop-session (project-dir)
  "Stop the MCP server for PROJECT-DIR."
  (when-let ((session (gethash project-dir claude-code-ide-mcp--sessions)))
    ;; Close client connection if any
    (when-let ((client (claude-code-ide-mcp-session-client session)))
      (websocket-close client))
    ;; Close server
    (when-let ((server (claude-code-ide-mcp-session-server session)))
      (websocket-server-close server))
    ;; Remove lockfile
    (claude-code-ide-mcp--remove-lockfile (claude-code-ide-mcp-session-port session))
    ;; Remove from sessions table
    (remhash project-dir claude-code-ide-mcp--sessions)
    (message "MCP server stopped for %s" project-dir)))

(provide 'claude-code-ide-mcp)

;;; claude-code-ide-mcp.el ends here
