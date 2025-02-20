;;; devcontainer-mode --- Support for devcontainer in emacs -*- lexical-binding: t; -*-

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/devcontainer-mode
;; Version: 0.1.0
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; To be documented

;;; Code:


(require 'project)
(require 'ansi-color)

(defvar devcontainer-execute-outside-container '("grep" "rg" "ag"))

(defvar devcontainer--project-info nil)

(defun devcontainer-find-executable ()
  (executable-find "devcontainer"))

(defun devcontainer-container-needed ()
  (if (or (devcontainer--starting-or-failed)
          (file-exists-p (concat (project-root (project-current)) ".devcontainer/devcontainer.json")))
      t
    (devcontainer--set-current-project-state 'no-devcontainer)))

(defun devcontainer-container-id ()
  (and (devcontainer-container-needed)
       (let ((output (shell-command-to-string (devcontainer--determine-container-id-cmd "--all"))))
         (when (> (length output) 0)
           (substring output 0 -1)))))

(defun devcontainer-container-up ()
  (and (not (devcontainer--starting-or-failed))
       (devcontainer-container-needed)
       (let ((output (shell-command-to-string (devcontainer--determine-container-id-cmd))))
         (devcontainer--set-current-project-state 'devcontainer-is-down)
         (when (> (length output) 0)
           (devcontainer--set-current-project-state 'devcontainer-is-up)
           (substring output 0 -1)))))

(defun devcontainer--set-current-project-state (state)
  (setf (alist-get (project-current) devcontainer--project-info nil nil 'equal) state)
  nil)

(defun devcontainer--current-project-state ()
  (alist-get (project-current) devcontainer--project-info nil nil 'equal))

(defun devcontainer--starting-or-failed ()
  (or (equal (devcontainer--current-project-state) 'devcontainer-is-starting)
      (equal (devcontainer--current-project-state) 'devcontainer-startup-failed)))

(defun devcontainer-up ()
  (interactive)
  (if (and (if (devcontainer-container-needed) t
             (message "Project does not use a devcontainer.")
             (devcontainer--set-current-project-state 'no-devcontainer))
           (or (devcontainer-find-executable)
               (user-error "Don't have devcontainer executable.")))
      (let* ((stdout-buf (let ((buf (get-buffer-create "*devcontainer stdout*")))
                           (with-current-buffer buf
                             (setq-local buffer-read-only nil)
                             (erase-buffer)
                             (compilation-mode))
                           (temp-buffer-window-show buf)
                           buf))
             (cmd `(,(devcontainer-find-executable) "up" "--workspace-folder" ,(project-root (project-current))))
             (proc (make-process
                    :name "devcontainer up"
                    :command cmd
                    :buffer stdout-buf
                    :filter #'devcontainer--build-process-stdout-filter
                    :sentinel #'devcontainer--build-sentinel)))
        (process-put proc 'project-root (project-root (project-current)))
        (devcontainer--set-current-project-state 'devcontainer-is-starting))))

(defun devcontainer-restart ()
  (interactive)
  (when (or (devcontainer-container-needed)
            (user-error "No devcontainer for current project"))
    (when (devcontainer-container-up)
      (devcontainer-kill-container))
    (devcontainer-up)))

(defun devcontainer-rebuild-and-restart ()
  (interactive)
  (when (or (devcontainer-container-needed)
            (user-error "No devcontainer for current project"))
    (when (devcontainer-container-up)
      (devcontainer-remove-container))
    (devcontainer-remove-image)
    (devcontainer-up)))

(defun devcontainer--image-repo-name ()
  (when (devcontainer-container-needed)
    (let ((directory-hash (secure-hash 'sha256 (directory-file-name (project-root (project-current))))))
      (format "vsc-%s-%s-uid" (project-name (project-current)) directory-hash))))

(defun devcontainer-image-id ()
  (when-let* (((devcontainer-container-needed))
              (cmd (format "docker images --quiet %s" (devcontainer--image-repo-name)))
              (output (shell-command-to-string cmd)))
    (when (> (length output) 0)
      (substring output 0 -1))))

(defun devcontainer--build-process-stdout-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((start (point))
            (buffer-read-only nil))
        (insert (string-replace "\r\n" "\n" string))
        (ansi-color-apply-on-region start (point-max))))))

(defun devcontainer--build-sentinel (process event)
  (let* ((project-root-dir (process-get process :project-root))
         (buf (process-buffer process))
         (cmd-result (with-current-buffer buf
                       (let ((result (progn
                                       (goto-char (point-max))
                                       (backward-sexp)
                                       (substring (buffer-string) (1- (point)))))
                             (buffer-read-only nil))
                         (goto-char (point-max))
                         (insert (format "Process %s %s" (process-name process) event))
                         result)))
         (container-launch-result (condition-case nil
                                      (json-parse-string cmd-result)
                                    (json-parse-error nil))))
    (if container-launch-result
        (let ((outcome (gethash "outcome" container-launch-result))
              (container-id (gethash "containerId" container-launch-result)))
          (setf (alist-get (project-current) devcontainer--project-info nil nil 'equal) 'devcontainer-is-up)
          (if (string= outcome "success")
              (message "Sucessfully brought up container id %s" (substring container-id 0 12))
            (let ((message (gethash "message" container-launch-result))
                  (description (gethash "description" container-launch-result)))
              (user-error "%s: %s – %s" outcome message description)
              (setf (alist-get (project-current) devcontainer--project-info nil nil 'equal) 'devcontainer-startup-failed))))
      (user-error "Garbeled output from `devcontainer up'. See *devcontainer stdout* buffer.")
      (setf (alist-get (project-current) devcontainer--project-info nil nil 'equal) 'devcontainer-startup-failed))))

(defun devcontainer--determine-container-id-cmd (&optional args)
  (concat
   "docker container ls --filter label=devcontainer.local_folder="
   (substring (project-root (project-current)) 0 -1)
   " --format {{.ID}}"
   (when args (concat " " args))))

(defun devcontainer-kill-container ()
  (interactive)
  (when-let ((container-id (or (devcontainer-container-up)
                               (user-error "No container running"))))
    (shell-command-to-string (concat "docker container kill " container-id))
    (devcontainer--update-project-info)
    (message "Killed container %s" container-id)))

(defun devcontainer-remove-container ()
  (interactive)
  (when-let ((container-id (or (devcontainer-container-id)
                               (user-error "No container to be removed"))))
    (shell-command-to-string (concat "docker container kill " container-id))
    (shell-command-to-string (concat "docker container rm " container-id))
    (message "Removed container %s" container-id)))

(defun devcontainer-remove-image ()
  (interactive)
  (when-let* (((or (devcontainer-container-needed)
                   (user-error "No devcontainer for current project")))
              (image-id (devcontainer-image-id)))
    (when-let* ((container-id (devcontainer-container-id)))
      (devcontainer-remove-container))
    (shell-command-to-string (concat "docker image rm " image-id))
    (message "Removed image %s" image-id)))

(defun devcontainer-vterm ()
  (interactive)
  (if (devcontainer-container-up)
      (let ((vterm-shell (concat "devcontainer exec" (devcontainer--workspace-folder) "bash")))
        (vterm))
    (user-error "devcontainer not running")))

(defun devcontainer-ansi-term ()
  (interactive)
  (if (devcontainer-container-up)
      (ansi-term (concat "devcontainer exec" (devcontainer--workspace-folder) "bash"))))

(defun devcontainer--workspace-folder ()
  (concat " --workspace-folder " (project-root (project-current)) " "))


(define-minor-mode devcontainer-mode
  "Toggle `devcontainer-mode'"
  :init-value nil
  :global t
  :lighter (:eval (devcontainer--lighter))
  (if devcontainer-mode
      (advice-add 'compilation-start :around #'devcontainer--compile-start-advice)
    (advice-remove 'compilation-start #'devcontainer--compile-start-advice)))

(defun devcontainer--lighter ()
  (concat "DevC" (devcontainer--lighter-tag)))

(defun devcontainer--lighter-tag ()
  (if (devcontainer-find-executable)
      (let* ((current-project (project-current))
             (devc-state (or (alist-get current-project devcontainer--project-info  nil nil 'equal)
                             (and current-project (devcontainer--update-project-info)))))
        (pcase devc-state
          ('no-devcontainer "-")
          ('devcontainer-is-down ">")
          ('devcontainer-is-starting "*")
          ('devcontainer-startup-failed "#")
          ('devcontainer-is-up "!")
          (_ "?")))
    "¿"))

(defun devcontainer--update-project-info ()
  (and
   (devcontainer-container-needed)
   (devcontainer-container-up))
  (alist-get (project-current) devcontainer--project-info nil nil 'equal))

(defun devcontainer--compile-start-advice (compile-fun command &rest rest)
  (if (and devcontainer-mode
           (devcontainer--devcontainerize-command command)
           (devcontainer-container-needed))
      (if (devcontainer-container-up)
          (apply compile-fun (concat "devcontainer exec --workspace-folder . " command) rest)
        (message "Devcontainer not running. Please start it first."))
    (apply compile-fun command rest)))

(defun devcontainer--devcontainerize-command (command)
  (not (member (car (split-string (file-name-base command) " "))
               devcontainer-execute-outside-container)))

(provide 'devcontainer-mode)

;;; devcontainer-mode.el ends here
