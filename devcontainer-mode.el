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
(require 'comint)

(defcustom devcontainer-execute-outside-container '("grep" "rg" "ag")
  "A list of programs that should not be executed inside the devcontainer."
  :group 'devcontainer-mode
  :type '(repeat string))

(defvar devcontainer--project-info nil
  "The data structure for state of the devcontainers of all active projects.")

(defun devcontainer--find-executable ()
  "Find the executable of `devcontainer'."
  (executable-find "devcontainer"))


(defun devcontainer--root ()
  "Deduce the root directory of the current project."
  (if-let ((proj (project-current)))
      (expand-file-name (project-root proj))
    (user-error "Not in a project")))

(defun devcontainer-container-needed ()
  "Dertermine if the current project needs (i.e. defines) a devcontainer."
  (cond ((eq (devcontainer--current-project-state) 'no-devcontainer) nil)
        ((devcontainer--current-project-state) t)
        ((file-exists-p (expand-file-name ".devcontainer/devcontainer.json" (devcontainer--root)))
         (devcontainer--set-current-project-state 'devcontainer-is-needed)
         t)
        (t (devcontainer--set-current-project-state 'no-devcontainer))))

(defun devcontainer-container-id ()
  "Determine the id of the primary docker container of the current project."
  (and (devcontainer-container-needed)
       (let ((output (shell-command-to-string (devcontainer--determine-container-id-cmd "--all"))))
         (when (> (length output) 0)
           (substring output 0 -1)))))

(defun devcontainer-image-id ()
  "Determine the image id of the primary docker container of the current project."
  (when-let* (((devcontainer-container-needed))
              (cmd (format "docker images --quiet %s" (devcontainer--image-repo-name)))
              (output (shell-command-to-string cmd)))
    (when (> (length output) 0)
      (substring output 0 -1))))

(defun devcontainer-is-up ()
  "Check if the devcontainer of the current project is running."
  (and (not (devcontainer--starting-or-failed))
       (devcontainer-container-needed)
       (let ((output (shell-command-to-string (devcontainer--determine-container-id-cmd))))
         (devcontainer--set-current-project-state 'devcontainer-is-down)
         (when (> (length output) 0)
           (devcontainer--set-current-project-state 'devcontainer-is-up)
           (substring output 0 -1)))))

;;;###autoload
(defun devcontainer-up (&optional show-buffer)
  "Start the devcontainer of the current project.

If SHOW-BUFFER is non nil, the buffer of the startup process is shown."
  (interactive
   (list (called-interactively-p 'interactive)))
  (if (and (if (devcontainer-container-needed) t
             (message "Project does not use a devcontainer.")
             (devcontainer--set-current-project-state 'no-devcontainer))
           (or (devcontainer--find-executable)
               (user-error "Don't have devcontainer executable.")))
      (let* ((cmdargs `("up" "--workspace-folder" ,(devcontainer--root)))
             (buffer (get-buffer-create "*devcontainer stdout*"))
             (proc (with-current-buffer buffer
                     (let ((inhibit-read-only t)) (erase-buffer))
                     (apply #'make-comint-in-buffer "devcontainer" buffer (devcontainer--find-executable) nil cmdargs)
                     (devcontainer-up-buffer-mode)
                     (when show-buffer
                       (temp-buffer-window-show buffer))
                     (get-buffer-process buffer))))
        (message "Starting devcontainer...")
        (set-process-sentinel proc #'devcontainer--build-sentinel)
        (devcontainer--set-current-project-state 'devcontainer-is-starting))))

;;;###autoload
(defun devcontainer-restart (&optional show-buffer)
  "Restart the devcontainer of the current project.

If SHOW-BUFFER is non nil, the buffer of the startup process is shown.

The primary docker is killed before restart.  Ohter containers of the
devcontainer stack simply remain alive."
  (interactive
   (list (called-interactively-p 'interactive)))
  (when (or (devcontainer-container-needed)
            (user-error "No devcontainer for current project"))
    (when (devcontainer-is-up)
      (devcontainer-kill-container))
    (devcontainer-up show-buffer)))

;;;###autoload
(defun devcontainer-rebuild-and-restart (&optional show-buffer)
  "Restart the devcontainer of the current project.

If SHOW-BUFFER is non nil, the buffer of the startup process is shown.

The primary docker container is killed and removed before restart.
Moreover the image of the primary docker container is removed to make
sure that the image is rebuilt before the restart.

The primary docker container is killed before restart.  Ohter containers
of the devcontainer stack simply remain alive."
  (interactive
   (list (called-interactively-p 'interactive)))
  (when (or (devcontainer-container-needed)
            (user-error "No devcontainer for current project"))
    (when (devcontainer-is-up)
      (devcontainer-remove-container))
    (devcontainer-remove-image)
    (devcontainer-up show-buffer)))

(defun devcontainer-kill-container ()
  "Kill the primary docker container of the current project."
  (interactive)
  (when-let ((container-id (or (devcontainer-is-up)
                               (user-error "No container running"))))
    (shell-command-to-string (concat "docker container kill " container-id))
    (devcontainer--update-project-info)
    (message "Killed container %s" container-id)))

(defun devcontainer-remove-container ()
  "Remove the primnary docker container of the current project."
  (interactive)
  (when-let ((container-id (or (devcontainer-container-id)
                               (user-error "No container to be removed"))))
    (shell-command-to-string (concat "docker container kill " container-id))
    (shell-command-to-string (concat "docker container rm " container-id))
    (devcontainer--set-current-project-state 'devcontainer-is-needed)
    (message "Removed container %s" container-id)))

(defun devcontainer-remove-image ()
  "Remove the image of the primary docker container of the current project."
  (interactive)
  (when-let* (((or (devcontainer-container-needed)
                   (user-error "No devcontainer for current project")))
              (image-id (devcontainer-image-id)))
    (when-let* ((container-id (devcontainer-container-id)))
      (devcontainer-remove-container))
    (shell-command-to-string (concat "docker image rm " image-id))
    (message "Removed image %s" image-id)))

(defvar devcontainer-mode-map (make-sparse-keymap)
  "The keymap for `devcontainer-mode'.")

(define-minor-mode devcontainer-mode
  "Toggle `devcontainer-mode'.

When `devcontainer-mode' is active and the current projects is defining
a devcontainer, all compilation launches are prepended with
`devcontainer exec' so that the compilation is performed inside the
devcontainer.  Use `devcontainer-execute-outside-container' to exclude
programs from being executed inside the devcontainer."
  :init-value nil
  :global t
  :lighter (:eval (devcontainer--lighter))
  :keymap devcontainer-mode-map
  :group 'devcontainer-mode
  (if devcontainer-mode
      (advice-add 'compilation-start :around #'devcontainer--compile-start-advice)
    (advice-remove 'compilation-start #'devcontainer--compile-start-advice)))


(defun devcontainer--set-current-project-state (state)
  (setf (alist-get (project-current) devcontainer--project-info nil nil 'equal) state)
  nil)

(defun devcontainer--current-project-state ()
  (alist-get (project-current) devcontainer--project-info nil nil 'equal))

(defun devcontainer--starting-or-failed ()
  (or (equal (devcontainer--current-project-state) 'devcontainer-is-starting)
      (equal (devcontainer--current-project-state) 'devcontainer-startup-failed)))

(defun devcontainer--image-repo-name ()
  (when (devcontainer-container-needed)
    (let ((directory-hash (secure-hash 'sha256 (directory-file-name (devcontainer--root)))))
      (format "vsc-%s-%s-uid" (project-name (project-current)) directory-hash))))

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
   (directory-file-name (devcontainer--root))
   " --format {{.ID}}"
   (when args (concat " " args))))

(defun devcontainer-vterm ()
  (interactive)
  (if (devcontainer-is-up)
      (let ((vterm-shell (format "devcontainer exec %s bash" (devcontainer--workspace-folder))))
        (vterm))
    (user-error "devcontainer not running")))

(defun devcontainer-command-prefix ()
  "Provide the command prefix to execute a command inside the local devcontainer.

If `devcontainer-mode' is on and your current project has a devcontainer
up and running, the string `devcontainer exec --workspace-folder
$PROJECT_ROOT ' is returned, otherwise `nil'"
  (when (and devcontainer-mode (devcontainer-is-up))
    (format "devcontainer exec %s " (devcontainer--workspace-folder))))

(defun devcontainer-ansi-term ()
  (interactive)
  (if (devcontainer-is-up)
      (ansi-term (concat (devcontainer-command-prefix) "--remote-env=\"TERM=eterm-256color\" bash"))))

(defun devcontainer--workspace-folder ()
  (concat "--workspace-folder " (devcontainer--root)))

(defun devcontainer--lighter ()
  (concat "DevC" (devcontainer--lighter-tag)))

(defun devcontainer--lighter-tag ()
  (if (devcontainer--find-executable)
      (let* ((current-project (project-current))
             (devc-state (or (alist-get current-project devcontainer--project-info  nil nil 'equal)
                             (and current-project (devcontainer--update-project-info)))))
        (pcase devc-state
          ('no-devcontainer "-")
          ('devcontainer-is-needed "+")
          ('devcontainer-is-down ">")
          ('devcontainer-is-starting "*")
          ('devcontainer-startup-failed "#")
          ('devcontainer-is-up "!")
          (_ "?")))
    "¿"))

(defun devcontainer--update-project-info ()
  (and
   (devcontainer-container-needed)
   (devcontainer-is-up))
  (alist-get (project-current) devcontainer--project-info nil nil 'equal))

(defun devcontainer--compile-start-advice (compile-fun command &rest rest)
  (if (and devcontainer-mode
           (project-current)
           (devcontainer--devcontainerize-command command)
           (devcontainer-container-needed))
      (if (devcontainer-is-up)
          (apply compile-fun (format "devcontainer exec %s %s" (devcontainer--workspace-folder) command) rest)
        (message "Devcontainer not running. Please start it first."))
    (apply compile-fun command rest)))

(defun devcontainer--devcontainerize-command (command)
  (not (member (car (split-string (file-name-base command) " "))
               devcontainer-execute-outside-container)))

(easy-menu-define devcontainer-menu devcontainer-mode-map
  "Menu to manage devcontainers"
  '("Devcontainer"
    :visible (not (equal (devcontainer--current-project-state) 'no-devcontainer))
    :active (not (equal (devcontainer--current-project-state) 'no-devcontainer))
    ["Start/Restart" devcontainer-restart
     :active (and (not (equal (devcontainer--current-project-state) 'no-devcontainer))
                  (not (equal (devcontainer--current-project-state) 'devcontainer-is-starting)))]
    ["Restart and rebuild" devcontainer-rebuild-restart
     :active (and (not (equal (devcontainer--current-project-state) 'no-devcontainer))
                  (not (equal (devcontainer--current-project-state) 'devcontainer-is-starting)))]
    ["Kill" devcontainer-kill-container :active (equal (devcontainer--current-project-state) 'devcontainer-is-up)]
    ["Remove container" devcontainer-remove-container
     :active (and (not (equal (devcontainer--current-project-state) 'no-devcontainer))
                  (not (equal (devcontainer--current-project-state) 'devcontainer-is-needed))
                  (not (equal (devcontainer--current-project-state) 'devcontainer-is-starting)))]))


(defvar devcontainer-up-buffer-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map ["q"] #'quit-window)
    map))


(define-derived-mode devcontainer-up-buffer-mode comint-mode
  "Devcontainer Start"
  "Major mode for devcontainer start buffers"
  (setq-local buffer-read-only t)
  (setq-local comint-terminfo-terminal "eterm-color"))


(defvar devcontainer--command-history nil)

(defun devcontainer-execute-command (command)
  (interactive
   (list (read-from-minibuffer "Command: " (car devcontainer--command-history) nil nil '(devcontainer--command-history . 1))))
  (when (not (devcontainer-is-up))
    (user-error "devcontainer not running"))
  (let* ((container-id (devcontainer-container-id))
         (cmd-args (append `("exec" "--workspace-folder" ,(devcontainer--root))
                           (split-string-shell-command command)))
         (buffer (get-buffer-create "*DevC command*"))
         (name (concat "DevC-" (devcontainer--root) "-" command))
         (proc (with-current-buffer buffer
                 (let ((inhibit-read-only t)) (erase-buffer))
                 (apply #'make-comint-in-buffer name buffer "devcontainer" nil cmd-args)
                 (devcontainer-container-execute-buffer-mode)
                 (temp-buffer-window-show buffer)
                 (get-buffer-process buffer)))
         (pts (string-trim (shell-command-to-string (format "docker exec %s ls -1t /dev/pts | head -1" container-id)))))
    (process-put proc 'pts pts)))


(defun devcontainer--exec-buffer ()
  (get-buffer "*DevC command*"))

(defun devcontainer-kill-command ()
  (interactive)
  (let* ((container-id (devcontainer-container-id))
         (proc (get-buffer-process (devcontainer--exec-buffer)))
         (pts (process-get proc 'pts)))
    (shell-command-to-string (format "docker exec %s pkill -t pts/%s" container-id pts))))

(defvar devcontainer-container-execute-buffer-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map ["q"] (lambda ()
                              (if (process-live-p (get-buffer-process (current-buffer)))
                                  (insert "q")
                                (quit-window))))
    map))

(define-derived-mode devcontainer-container-execute-buffer-mode comint-mode
  "Devcontainer Start"
  "Major mode for devcontainer start buffers"
  (setq-local buffer-read-only nil)
  (setq-local kill-buffer-hook nil)
  (setq-local comint-terminfo-terminal "eterm-color"))


(provide 'devcontainer-mode)

;;; devcontainer-mode.el ends here
