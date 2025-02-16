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

(defun devcontainer-find-executable ()
  (executable-find "devcontainer"))

(defun devcontainer-container-needed ()
  (file-exists-p (concat (project-root (project-current)) ".devcontainer/devcontainer.json")))


(defun devcontainer-container-id ()
  (and (devcontainer-container-needed)
       (let ((output (shell-command-to-string (devcontainer--determine-container-id-cmd "--all"))))
         (when (> (length output) 0)
           (substring output 0 -1)))))


(defun devcontainer-container-up ()
  (and (devcontainer-container-needed)
       (let ((output (shell-command-to-string (devcontainer--determine-container-id-cmd))))
         (when (> (length output) 0)
           (substring output 0 -1)))))

(defun devcontainer-up ()
  (interactive)
  (if (and (or (devcontainer-container-needed)
               (message "Project does not use a devcontainer.") nil)
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
                    :sentinel #'devcontainer--build-sentinel))))))

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

(defun devcontainer-image-id ()
  (when (devcontainer-container-needed)
    (let ((directory-hash (secure-hash 'sha256 (directory-file-name (project-root (project-current))))))
      (format "vcs-%s-%s" (project-name (project-current)) directory-hash))))


(defun devcontainer--build-process-stdout-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((start (point))
            (buffer-read-only nil))
        (insert (string-replace "\r\n" "\n" string))
        (ansi-color-apply-on-region start (point-max))))))

(defun devcontainer--build-sentinel (process event)
  (let* ((buf (process-buffer process))
         (cmd-result (with-current-buffer buf
                       (let ((result (progn
                                       (goto-char (point-max))
                                       (backward-sexp)
                                       (substring (buffer-string) (1- (point)))))
                             (buffer-read-only nil))
                         (goto-char (point-max))
                         (insert (format "Process %s %s" process event))
                         result)))
         (container-launch-result (condition-case nil
                                      (json-parse-string cmd-result)
                                    (json-parse-error nil))))
    (if container-launch-result
        (let ((outcome (gethash "outcome" container-launch-result))
              (container-id (gethash "containerId" container-launch-result)))
          (if (string= outcome "success")
              (message "Sucessfully brought up container id %s" (substring container-id 0 12))
            (let ((message (gethash "message" container-launch-result))
                  (description (gethash "description" container-launch-result)))
              (user-error "%s: %s – %s" outcome message description))))
      (user-error "Garbeled output from `devcontainer up'. See *devcontainer stdout* buffer."))))



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
  (when-let* ((container-id (or (devcontainer-container-id)
                                (user-error "No devcontainer for current project")))
              (image-id (devcontainer-image-id)))
    (shell-command-to-string (concat "docker container kill " container-id))
    (shell-command-to-string (concat "docker container rm " container-id))
    (shell-command-to-string (concat "docker image rm " image-id))
    (message "Removed image %s" image-id)))

(defun devcontainer-vterm ()
  (interactive)
  (if (devcontainer-container-up)
      (let ((vterm-shell (concat "devcontainer exec" (devcontainer--workspace-folder) "bash")))
        (vterm))
    (user-error "devcontainer not running")))

(defun devcontainer--workspace-folder ()
  (concat " --workspace-folder " (project-root (project-current)) " "))


(define-minor-mode devcontainer-mode
  "Toggle `devcontainer-mode'"
  :init-value nil
  :global t
  :lighter "dc"
  (if devcontainer-mode
      (advice-add 'compilation-start :around #'devcontainer--compile-start-advice)
    (advice-remove 'compilation-start #'devcontainer--compile-start-advice)))

(defun devcontainer--compile-start-advice (compile-fun command &rest rest)
  (if (and devcontainer-mode
           (devcontainer-container-needed))
      (if (devcontainer-container-up)
          (apply compile-fun (concat "devcontainer exec --workspace-folder . " command) rest)
        (message "Devcontainer not running. Please start it first."))
    (apply compile-fun command rest)))

(provide 'devcontainer-mode)

;;; devcontainer-mode.el ends here
