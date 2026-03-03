;;; devcontainer.el --- Support for devcontainer -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Johannes Mueller

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/devcontainer.el
;; Version: 0.1.0
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package lets you handle i.e. start, restart devcontainers of your projects
;; and forwards all your `compile' commands into the devcontainer by advising
;; `compilation-start'.  So as long you use `compile' to build, test and run
;; (parts of your) software, all will be executed inside the devcontainer.


;;; Code:

(require 'project)
(require 'ansi-color)
(require 'comint)
(require 'tramp-container)
(require 'tramp)

(defcustom devcontainer-execute-outside-container '("grep" "rg" "ag")
  "A list of programs that should not be executed inside the devcontainer."
  :group 'devcontainer
  :type '(repeat string))

(defcustom devcontainer-engine 'docker
  "The container engine to use, one of `docker' or `podman'.

To specify the path of the podman/docker executable, customise
`tramp-podman-program' or `tramp-docker-program'."
  :group 'devcontainer
  :type '(choice (const podman)
                 (const docker)))

(defcustom devcontainer-term-shell "bash"
  "The shell to be invoked when using `devcontainer-term'."
  :group 'devcontainer
  :type 'string)

(defcustom devcontainer-term-environment nil
  "An `alist' to define environment variables for `devcontainer-term'.

These variables are to be passed to the container's shell when a
terminal session is started in the container using `devcontainer-term'.

Hint: in order to prevent disturbing control sequences in the shell prompt
you can add the element `(\"TERM\" . \"xterm-256color\")'"
  :group 'devcontainer
  :type '(alist :key-type string :value-type string))

(defcustom devcontainer-term-function #'ansi-term
  "The function to be used to start a terminal inside the container."
  :group 'devcontainer
  :type 'function)

(defcustom devcontainer-post-startup-hook nil
  "Hook to be called after a devcontainer comes up.

The hook functions should have four parameters:
* container-id – a string representing the container id
* container-name – a string representing the container-name
* remote-user – a string of the remote user name
* remote-workdir – the workdir path in the container."
  :group 'devcontainer
  :type 'hook)

(defcustom devcontainer-startup-secrets-file nil
  "The file passed to the `--secrets-file' option of the `devcontainer up' command.

The file can be defined either with an absolute path or relative to the
devcontainer's workspace folder AKA the project's root directory.  It
must be a JSON file containing key/value pairs that will appear in the
startup process as environment variables."
  :group 'devcontainer
  :type 'file)

(defcustom devcontainer-execution-buffer-naming #'devcontainer-find-execute-buffer-name
  "Define the way buffers for `devcontainer-execute-command' are to be named.

If a string, the string is just use as is as candidate for the buffer
name.  If a function the function is called with the command string as
argument.  The function is then supposed to return candidate of the
buffer name."
  :group 'devcontainer
  :type '(choice (string :tag "Static name")
                 (function :tag "Function taking the command string.")))

(defcustom devcontainer-apply-customization 'ask
  "Define weather to apply customizations defined in `devcontainer.json' file.

The `devcontainer.json' file can provide customizations, i.e. variable
settings and commands to execute, in order to align Emacs' configuration
with the project.  As that is a potential security risk this is not done
unconditionally.  By this option the user can choose between:
* `always' – always apply the customizations for every repository.
* `never' – never apply the customizations for any repository.
* `ask' – ask for a new repositort and then remember the answer."
  :group 'devcontainer
  :type '(choice (const always)
                 (const never)
                 (const ask)))

(defvar devcontainer--project-info nil
  "The data structure for state of the devcontainer's of all active projects.

This is basically a cache that we need not to call the docker
executable that often.")

(defvar devcontainer--command-history nil
  "The history of commands for `devcontainer-execute-command'.")

(defvar devcontainer--customization-request-cache-alist nil
  "Cached answers whether to apply devconatiner's customization.")

(defun devcontainer--docker-path ()
  "Return the path of the Docker-compatible command to call.
If `devcontainer-engine' equals \"docker\", use `tramp-docker-program',
else `tramp-podman-program'."
  (if (eq 'docker devcontainer-engine)
      tramp-docker-program
    tramp-podman-program))

(defun devcontainer--call-engine-string-sync (&rest args)
  "Call `devcontainer-engine' with ARGS.
If the command exit code is 0, return output or nil for empty output.
Otherwise, raise an `error'."
  (with-temp-buffer
    ;; LET* for ordering guarantee
    (let* ((ret (apply
                 #'call-process
                 (devcontainer--docker-path)
                 nil                            ; INFILE
                 (current-buffer)
                 nil                            ; DISPLAY
                 args))
           (out (string-trim-right
                 (buffer-substring-no-properties (point-min)
                                                 (point-max)))))
      (if (eql 0 ret)
          (and (not (string-empty-p out)) out)
        (error
         "%s returned %d: %s"
         devcontainer-engine
         ret
         out)))))

(defun devcontainer--make-cli-args (action &rest args)
  "Setup cli arguments for the devcontainer command performing ACTION with ARGS."
  (append
   (list
    (devcontainer--find-executable)
    action
    "--docker-path" (devcontainer--docker-path)
    "--workspace-folder" (devcontainer--root))
   ;; TODO dotfiles argument
   args))

(defun devcontainer--find-executable ()
  "Find the executable of `devcontainer'."
  (executable-find "devcontainer"))

(defun devcontainer--root ()
  "Deduce the root directory of the current project."
  (when-let ((proj (project-current)))
      (expand-file-name (project-root proj))))

(defun devcontainer-config-files ()
  "Get the JSON config files for the current project.

https://containers.dev/implementors/spec/#devcontainerjson"
  (let ((default-directory (devcontainer--root)))
    (append (seq-filter #'file-exists-p '(".devcontainer/devcontainer.json" ".devcontainer.json"))
            (seq-sort #'string< (file-expand-wildcards ".devcontainer/*/devcontainer.json")))))

(defun devcontainer-container-needed-p ()
  "Determine if the current project needs (i.e. specifies) a devcontainer."
  (cond ((eq (devcontainer--current-project-state) 'no-devcontainer) nil)
        ((devcontainer--current-project-state) t)
        ((devcontainer-config-files)
         (devcontainer--set-current-project-state 'devcontainer-is-needed)
         t)
        (t (devcontainer--set-current-project-state 'no-devcontainer))))

(defun devcontainer-container-id ()
  "Determine the id of the primary docker container of the current project."
  (let ((get-ctr-id
         (lambda (all)
           (devcontainer--call-engine-string-sync
            "container"
            "ls"
            (format
             "--filter=label=devcontainer.local_folder=%s"
             (directory-file-name (devcontainer--root)))
            "--format={{.ID}}"
            (format "--all=%s" (if all "true" "false"))))))
    (and (devcontainer-container-needed-p)
         (or
          (funcall get-ctr-id nil)
          (funcall get-ctr-id t)))))

(defun devcontainer-container-name ()
  "Determine the name of the primary docker container of the current project."
  (when-let* ((container-id (devcontainer-container-id)))
    (thread-first
      (devcontainer--call-engine-string-sync "container" "inspect" container-id "--format={{.Name}}")
      (string-trim-right)
      (string-trim-left "/"))))

(defun devcontainer-image-id ()
  "Determine the image id of the primary docker container of the current project."
  (and (devcontainer-container-needed-p)
       (devcontainer--call-engine-string-sync "images" "--quiet" (devcontainer--image-repo-name))))

(defun devcontainer--image-repo-name ()
  "Calculate the current project's devcontainer's image name."
  (when (devcontainer-container-needed-p)
    (let ((directory-hash (secure-hash 'sha256 (directory-file-name (devcontainer--root)))))
      (format "vsc-%s-%s-uid" (project-name (project-current)) directory-hash))))

(defun devcontainer-up-container-id ()
  "Return the devcontainer's container id if the container is up otherwise nil."
  (and (not (devcontainer--starting-or-failed))
       (devcontainer-container-needed-p)
       (let ((output (devcontainer--call-engine-string-sync
                      "container"
                      "ls"
                      (format "--filter=label=devcontainer.local_folder=%s"
                              (directory-file-name (devcontainer--root)))
                      "--format={{.ID}}")))
         (devcontainer--set-current-project-state 'devcontainer-is-down)
         (when output
           (devcontainer--set-current-project-state 'devcontainer-is-up))
         output)))

(defun devcontainer--secrets-file-arg ()
  "Return secrets file CLI arg for `devcontainer up' if set and existant."
  (let ((secrets-file (pcase devcontainer-startup-secrets-file
                        ('nil nil)
                        ((pred file-name-absolute-p) devcontainer-startup-secrets-file)
                        (_  (concat (file-name-as-directory (devcontainer--root))
                                    devcontainer-startup-secrets-file)))))
    (when (and secrets-file (file-exists-p secrets-file))
      `("--secrets-file" ,secrets-file))))

;;;###autoload
(defun devcontainer-up (&optional show-buffer)
  "Start the devcontainer of the current project.

If SHOW-BUFFER is non nil, the buffer of the startup process is shown."
  (interactive
   (list (called-interactively-p 'interactive)))
  (when (eq (devcontainer--current-project-state) 'devcontainer-is-starting)
    (user-error "Another devcontainer is starting up.  Please wait until that is finished"))
  (if (and (if (devcontainer-container-needed-p) t
             (message "Project does not use a devcontainer.")
             (devcontainer--set-current-project-state 'no-devcontainer))
           (or (devcontainer--find-executable)
               (user-error "Don't have devcontainer executable")))
      (let* ((cmd (apply #'devcontainer--make-cli-args "up" (devcontainer--secrets-file-arg)))
             (buffer (devcontainer--comint-process-buffer "devcontainer" "devcontainer startup" cmd 'insert-cli))
             (proc (with-current-buffer buffer
                (devcontainer-up-buffer-mode)
                (when show-buffer
                  (temp-buffer-window-show buffer))
                (get-buffer-process buffer))))
        (message "Starting devcontainer...")
        (set-process-sentinel proc #'devcontainer--build-sentinel)
        (devcontainer--set-current-project-state 'devcontainer-is-starting))))

(defun devcontainer-find-execute-buffer-name (command)
  "Create unique name for buffer including COMMAND.

This function is the default function to name the buffer used for
`devcontainer-execute-command'."
  (let ((project (file-name-nondirectory (directory-file-name (devcontainer--root)))))
    (concat project ": " command)))

(defun devcontainer--make-execution-buffer-name (command)
  "Make execution buffer name for COMMAND according to config.

Evaluates `devcontainer-execution-buffer-naming'"
  (if (functionp devcontainer-execution-buffer-naming)
      (funcall devcontainer-execution-buffer-naming command)
    devcontainer-execution-buffer-naming))

;;;###autoload
(defun devcontainer-execute-command (command)
  "Execute COMMAND in the container – batch mode."
  (interactive (devcontainer--execute-interactive-args))
  (let ((command (string-trim command)))
    (devcontainer--do-execute-command-buffer (split-string-shell-command command)
    (format "DevC %s" (devcontainer--make-execution-buffer-name command))
    'insert-cli)))

(defun devcontainer--do-execute-command-buffer (command buffer-name &optional insert-cli)
  "Execute COMMAND in a new buffer named BUFFER-NAME.
Insert the command line if INSERT-CLI is non-nil."
  (unless (devcontainer-container-needed-p)
    (user-error "No devcontainer for current project"))
  (unless (devcontainer-up-container-id)
    (user-error "The devcontainer not running.  Please start it first"))
  (let* ((cmd (apply #'devcontainer--make-cli-args "exec" command))
         (buffer (devcontainer--comint-process-buffer
                  "devcontainer"
                  buffer-name
                  cmd
                  insert-cli)))
    (temp-buffer-window-show buffer)
    buffer))

;;;###autoload
(defun devcontainer-execute-command-interactive (command)
  "Execute COMMAND in the container – interactive mode."
  (interactive (devcontainer--execute-interactive-args))
  (let ((buffer (devcontainer--do-execute-command-buffer
                 (append (devcontainer--execute-term-environment) (split-string-shell-command command))
                 (format "DevC %s" (devcontainer--make-execution-buffer-name command)))))
    (with-current-buffer buffer
      (setq-local buffer-read-only nil))
    buffer))

(defun devcontainer--execute-interactive-args ()
  "Prompt user for a command for `devcontainer-execute-command-interactive'."
  (let ((proposal (car devcontainer--command-history))
        (history '(devcontainer--command-history . 1)))
     (list (read-from-minibuffer "Command: " proposal nil nil history))))

(defun devcontainer--execute-term-environment ()
  "Add cli option to inject `devcontainer-term-environment'."
  (if devcontainer-term-environment
      (append '("--remote-env")
              (mapcar (lambda (elt)
                        (format "%s=%s" (car elt)
                                            (if (string-match-p "[ \t\r\n]" (cdr elt))
                                                (format "\"%s\"" (cdr elt))
                                              (cdr elt))))
                      devcontainer-term-environment))
    '()))

(defun devcontainer--existing-buffer-available (buffer-name)
  "Find the first available buffer name prefixed BUFFER-NAME[<N>]."
  (thread-last
    (buffer-list)
    (seq-filter (lambda (buffer) (string-prefix-p buffer-name (buffer-name buffer))))
    (seq-find (lambda (buffer) (not (process-live-p (get-buffer-process buffer)))))))

(defun devcontainer--get-execution-buffer (buffer-name)
  "Provide an unused buffer for an execution process with BUFFER-NAME.

Takes the first buffer whose name prefixed BUFFER-NAME[<N>] that has
not a running process associated with it or creates a new one."
  (or (devcontainer--existing-buffer-available buffer-name)
      (generate-new-buffer buffer-name)))

(defun devcontainer--comint-process-buffer (proc-name buffer-name command &optional insert-cli)
  "Make a comint buffer.

PROC-NAME is the name given to the process object.  BUFFER-NAME is the
name given to the buffer.  COMMAND is a list of strings representing the
command line.  If INSERT-CLI is non-nil, the command line is inserted at
the first line of the buffer."
  (let ((buffer (devcontainer--get-execution-buffer (format "*%s*" buffer-name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when insert-cli
          (insert (string-join command " "))
          (insert "\n")))
      (apply #'make-comint-in-buffer
             proc-name
             buffer
             (car command)
             nil         ; STARTFILE
             (cdr command)))
    buffer))

;;;###autoload
(defun devcontainer-restart (&optional show-buffer)
  "Restart the devcontainer of the current project.

If SHOW-BUFFER is non nil, the buffer of the startup process is shown.

The primary docker is killed before restart.  Ohter containers of the
devcontainer stack simply remain alive."
  (interactive
   (list (called-interactively-p 'interactive)))
  (when (or (devcontainer-container-needed-p)
            (user-error "No devcontainer for current project"))
    (when (devcontainer-up-container-id)
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
  (when (or (devcontainer-container-needed-p)
            (user-error "No devcontainer for current project"))
    (when (devcontainer-up-container-id)
      (devcontainer-remove-container))
    (devcontainer-remove-image)
    (devcontainer-up show-buffer)))

;;;###autoload
(defun devcontainer-kill-container ()
  "Kill the primary docker container of the current project."
  (interactive)
  (when-let ((container-id (or (devcontainer-up-container-id)
                               (user-error "No container running"))))
    (devcontainer--call-engine-string-sync "container"
                                           "kill"
                                           container-id)
    (devcontainer--update-project-info)
    (message "Killed container %s" container-id)))

;;;###autoload
(defun devcontainer-remove-container ()
  "Remove the primnary docker container of the current project."
  (interactive)
  (when-let ((container-id (or (devcontainer-container-id)
                               (user-error "No container to be removed"))))
    (devcontainer--call-engine-string-sync "container"
                                           "kill"
                                           container-id)
    (devcontainer--call-engine-string-sync "container"
                                           "rm"
                                           container-id)
    (devcontainer--set-current-project-state 'devcontainer-is-needed)
    (message "Removed container %s" container-id)))

;;;###autoload
(defun devcontainer-remove-image ()
  "Remove the image of the primary docker container of the current project."
  (interactive)
  (when-let* (((or (devcontainer-container-needed-p)
                   (user-error "No devcontainer for current project")))
              (image-id (devcontainer-image-id)))
    (when-let* ((container-id (devcontainer-container-id)))
      (devcontainer-remove-container))
    (devcontainer--call-engine-string-sync "image"
                                           "rm"
                                           image-id)
    (message "Removed image %s" image-id)))

(defvar devcontainer-mode-map (make-sparse-keymap)
  "The keymap for `devcontainer-mode'.")

;;;###autoload
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
  :group 'devcontainer
  (if devcontainer-mode
      (progn
        (advice-add 'compilation-start :around #'devcontainer--compile-start-advice)
        (advice-add 'find-file-noselect :around #'devcontainer--find-file-apply-customization-advice))
    (advice-remove 'compilation-start #'devcontainer--compile-start-advice)
    (advice-remove 'find-file-noselect #'devcontainer--find-file-apply-customization-advice)))

(defun devcontainer--set-current-project-state (state)
  "Set the current project's devcontainer state cache to STATE."
  (setf (alist-get (project-current) devcontainer--project-info nil nil 'equal) state)
  nil)

(defun devcontainer--current-project-state ()
  "Retrieve the devcontainer state of the current project."
  (alist-get (project-current) devcontainer--project-info nil nil 'equal))

(defun devcontainer-invalidate-cache ()
  "Invalidate the devcontainer state cache for the current project."
  (setq devcontainer--project-info (assoc-delete-all (project-current) devcontainer--project-info 'equal)))

(defun devcontainer--starting-or-failed ()
  "Return t if the current project's devcontainer is starting or start has failed."
  (or (equal (devcontainer--current-project-state) 'devcontainer-is-starting)
      (equal (devcontainer--current-project-state) 'devcontainer-startup-failed)))

(defun devcontainer--build-sentinel (process event)
  "The sentinel for PROCESS handling EVENT."
  (let ((container-launch-result
         (condition-case nil
             (json-parse-string (devcontainer--extract-process-result-json (process-buffer process)))
           (json-parse-error nil))))
    (devcontainer--append-success-failure-info process event)
    (unless container-launch-result
      (setf (alist-get (project-current) devcontainer--project-info nil nil 'equal) 'devcontainer-startup-failed)
      (error "Garbled output from `devcontainer up'.  See *devcontainer startup* buffer"))

    (setf (alist-get (project-current) devcontainer--project-info nil nil 'equal) 'devcontainer-is-up)

    (let ((outcome (gethash "outcome" container-launch-result))
          (container-id (gethash "containerId" container-launch-result)))
      (if (string= outcome "success")
          (let ((container-name (gethash "composeProjectName" container-launch-result))
                (remote-user (gethash "remoteUser" container-launch-result))
                (remote-workdir (gethash "remoteWorkspaceFolder" container-launch-result)))
            (run-hook-with-args 'devcontainer-post-startup-hook container-id container-name remote-user remote-workdir)
            (message "Successfully brought up container id %s" (substring container-id 0 12)))
        (let ((message (gethash "message" container-launch-result))
              (description (gethash "description" container-launch-result)))
          (setf (alist-get (project-current) devcontainer--project-info nil nil 'equal) 'devcontainer-startup-failed)
          (user-error "%s: %s – %s" outcome message description))))))

(defun devcontainer--extract-process-result-json (buffer)
  "Extract the devcontainer process result json out out BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max))
    (backward-sexp)
    (substring (buffer-string) (1- (point)))))

(defun devcontainer--append-success-failure-info (process event)
  "Append a line to PROCESS buffer about sucess or failing EVENT."
  (with-current-buffer (process-buffer process)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (insert (format "Process %s %s" (process-name process) event)))))

(defun devcontainer-term ()
  "Start a shell inside the container.

There are the following customization options:

* `devcontainer-term-function' to determine the terminal emulator
  (defaults to `ansi-term').

* `devcontainer-term-shell' – the shell command to be used inside the
  container (defaults to `bash').

* `devcontainer-term-environment' to add custom modifications to the
  environment."
  (interactive)
  (when (devcontainer-up-container-id)
    (funcall devcontainer-term-function
             (string-join (append (devcontainer-advice 'in-terminal)
                                  `(,devcontainer-term-shell))
                          " "))))

(defun devcontainer--workspace-folder ()
  "Retrieve the `--workspace-folder' switch for the current project root."
  (concat "--workspace-folder " (devcontainer--root)))

(defun devcontainer--lighter ()
  "Setup the lighter for `devcontainer-mode'."
  (concat "DevC" (devcontainer--lighter-tag)))

(defun devcontainer--lighter-tag ()
  "Make the state tag for the lighter of `devcontainer-mode'."
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
  "Update the current project's devcontainer state info cache.

Note that it happens implicitly by calling the relevant functions to
update the cache."
  (and (devcontainer-container-needed-p)
       (devcontainer-up-container-id))
  (alist-get (project-current) devcontainer--project-info nil nil 'equal))

(defun devcontainer-advisable-p ()
  "Return non-nil if it is advisable to run a command inside the container."
  (and devcontainer-mode
       (project-current)
       (not (tramp-tramp-file-p (project-root (project-current))))
       (devcontainer-container-needed-p)))

(defun devcontainer--make-env-cli-args (in-terminal)
  "Make cli-args of the environment variables in `devcontainer-remote-environment'.

If IN-TERMINAL is non-nil, also the ones of
`devcontainer-term-environment' are added."
  (apply #'append
         (remq nil
               (mapcar (lambda (var)
                         (when var (list "--env" (format "%s=%s" (car var) (cdr var)))))
                       (cons nil (append (devcontainer-remote-environment)
                                         (when in-terminal devcontainer-term-environment)))))))

(defun devcontainer-advice (&optional in-terminal)
  "Determine the prefix that is to be used to run a command inside the container.

If IN-TERMINAL is non nil, the \"-it\" flag is set."
  (when-let* ((container-id (devcontainer-up-container-id)))
    (let ((remote-user (devcontainer-remote-user)))
      (remq nil
            (append
             (list
              (symbol-name devcontainer-engine)
              "exec"
              (when in-terminal "-it")
              "--workdir" (devcontainer-remote-workdir)
              (when remote-user "--user") remote-user)
             (devcontainer--make-env-cli-args in-terminal)
             (list container-id))))))

(defun devcontainer--fix-quoted-env-elements (command-string)
  "Fix overquoted environment elements in COMMAND-STRING.

This is a kind of ugly repair of the environment cli args as in `--env
FOO=bar'.  `shell-quote-argument' quotes the `=' sign to `--env FOO\\=bar'.
This reverts that quote."
  (replace-regexp-in-string "--env \\([a-zA-Z0-9_]+\\)\\\\=" "--env \\1=" command-string))

(defun devcontainer-advise-command (command)
  "Prepend COMMAND with to run inside the container if possible.

If COMMAND is a string, the advice is prefixed as a string.  If it is a
list of CLI arguments, the advice is prefixed as list.  So it should
work no matter if it is used in `compile' or in other functions issuing
commands to a shell."
  (if (and (devcontainer-advisable-p)
           (devcontainer--devcontainerize-command-p (if (stringp command) command (string-join command " "))))
      (if-let ((advice (devcontainer-advice)))
          (if (stringp command)
              (devcontainer--fix-quoted-env-elements
               (string-join (append (mapcar (lambda (el) (shell-quote-argument el nil)) advice) (list command)) " "))
            (append advice command))
        (user-error "The devcontainer not running.  Please start it first"))
    command))

(defun devcontainer--compile-start-advice (compile-fun command &rest rest)
  "Advise the function COMPILE-FUN by modifying COMMAND passing REST."
  (let ((command (devcontainer-advise-command command)))
    (apply compile-fun command rest)))

(defun devcontainer--devcontainerize-command-p (command)
  "Return t if COMMAND is to be run inside the container.

That means not excluded by config."
  (not (member (car (split-string (file-name-base command) " "))
               devcontainer-execute-outside-container)))

(defun devcontainer--find-file-apply-customization-advice (find-file-fun filename &rest args)
  "Advise FIND-FILE-FUN to apply the customizations.

FILENAME and ARGS are just passed."
  (let ((already-existing (get-file-buffer filename)))
    (with-current-buffer (apply find-file-fun filename args)
      (unless already-existing
        (devcontainer--read-and-apply-customizations))
      (current-buffer))))

(defun devcontainer--read-and-apply-customizations ()
  "Read the customizations from `devcontainer.json' and apply them."
  (when-let* ((config (devcontainer--read-devcontainer-json))
              (customizations (gethash "customizations" config))
              (emacs-customizations (gethash "emacs" customizations))
              (_ (devcontainer--apply-configuration-requested-p)))
    (devcontainer--apply-customizations emacs-customizations)
    (when-let* ((mode-specific-customizations (gethash "modes" emacs-customizations)))
      (dolist (key (devcontainer--sorted-mode-keys mode-specific-customizations))
        (devcontainer--apply-mode-specific-customizations key (gethash key mode-specific-customizations))))))

(defun devcontainer--apply-configuration-requested-p ()
  "Non-nil if the devcontainer's customization is to be applied."
  (or (eq devcontainer-apply-customization 'always)
      (and (eq devcontainer-apply-customization 'ask)
           (devcontainer--ask-configuration-or-cached))))

(defun devcontainer--ask-configuration-or-cached ()
  "Ask whether to apply devcontainer's customization if answer not cached."
  (if-let ((cached (assoc (project-current) devcontainer--customization-request-cache-alist)))
      (cdr cached)
    (let ((answer (y-or-n-p "Apply container customizations for this project? ")))
      (push (cons (project-current) answer) devcontainer--customization-request-cache-alist)
      answer)))

(defun devcontainer-forget-current-project-customization-policy ()
  "Forget the decision about whether to apply `devcontainer.json' customizations."
  (interactive)
  (setq devcontainer--customization-request-cache-alist
        (assoc-delete-all (project-current) devcontainer--customization-request-cache-alist)))

(defun devcontainer--sorted-mode-keys (mode-specific-customizations)
  "Return the sorted keys of MODE-SPECIFIC-CUSTOMIZATIONS."
  (sort (hash-table-keys mode-specific-customizations)
        (lambda (left right) (provided-mode-derived-p (intern right) (intern left)))))

(defun devcontainer--apply-customizations (customizations)
  "Apply CUSTOMIZATIONS."
  (maphash (lambda (key value)
             (if (string-prefix-p "(" key)
                 (eval (read key))
               (set (intern key) value)))
           customizations))

(defun devcontainer--apply-mode-specific-customizations (mode customizations)
  "Apply CUSTOMIZATIONS for MODE."
  (when (provided-mode-derived-p major-mode (intern mode))
    (devcontainer--apply-customizations customizations)))

(easy-menu-define devcontainer-menu devcontainer-mode-map
  "Menu to manage devcontainers."
  '("Devcontainer"
    :visible (not (equal (devcontainer--current-project-state) 'no-devcontainer))
    :active (not (equal (devcontainer--current-project-state) 'no-devcontainer))
    ["Start/Restart" devcontainer-restart
     :active (and (not (equal (devcontainer--current-project-state) 'no-devcontainer))
                  (not (equal (devcontainer--current-project-state) 'devcontainer-is-starting)))]
    ["Restart and rebuild" devcontainer-rebuild-restart
     :active (and (not (equal (devcontainer--current-project-state) 'no-devcontainer))
                  (not (equal (devcontainer--current-project-state) 'devcontainer-is-starting)))]
    ["Kill" devcontainer-kill-container :active (equal (devcontainer--current-project-state) 'devcontainer-up-container-id)]
    ["Remove container" devcontainer-remove-container
     :active (and (not (equal (devcontainer--current-project-state) 'no-devcontainer))
                  (not (equal (devcontainer--current-project-state) 'devcontainer-is-needed))
                  (not (equal (devcontainer--current-project-state) 'devcontainer-is-starting)))]))


(define-derived-mode devcontainer-up-buffer-mode comint-mode
  "Devcontainer Start"
  "Major mode for devcontainer start buffers."
  (setq-local buffer-read-only t)
  (setq-local comint-terminfo-terminal "eterm-color"))


(defun devcontainer-container-environment ()
  "Retrieve the container environment of current devcontainer if it's up."
  (when-let* ((container-id (devcontainer-container-id)))
    (mapcar (lambda (varstring) (apply #'cons (split-string varstring "=")))
            (json-parse-string
             (car (process-lines (devcontainer--docker-path)
                                 "container"
                                 "inspect"
                                 container-id
                                 "--format={{json .Config.Env}}"))
             :object-type 'alist))))

(defun devcontainer--container-metadata ()
  "Retrieve the devcontainer's metadata if it's up."
  (and-let* ((metadata-json (devcontainer--inspect-container "{{index .Config.Labels \"devcontainer.metadata\"}}"))
             (_ (length> metadata-json 0)))
    (seq-reduce #'append (json-parse-string metadata-json :object-type 'alist) nil)))

(defun devcontainer--inspect-container (format-query)
  "Query FORMAT-QUERY from `docker container inspect --format='."
  (when-let* ((container-id (devcontainer-container-id)))
    (string-trim
     (car (process-lines
           (symbol-name devcontainer-engine)
           "container" "inspect" container-id
           (format "--format=%s" format-query))))))

(defun devcontainer-remote-user ()
  "Retrieve the remote user name of the current project's devcontainer if it's up."
  (or (alist-get 'remoteUser (devcontainer--container-metadata))
      (devcontainer--inspect-container "{{.Config.User}}")))

(defun devcontainer-remote-environment ()
  "Retrieve the defined remote environment of current devcontainer if it's up."
  (when-let* ((metadata (devcontainer--container-metadata)))
    (mapcar (lambda (elt)
              (cons (car elt) (devcontainer--interpolate-variable (cdr elt))))
            (alist-get 'remoteEnv metadata))))

(defun devcontainer--interpolate-variable (string)
  "Interpolate devcontainer variable into STRING."
  (replace-regexp-in-string
   "\\${\\([[:alpha:]]+\\)\\(:[[:alpha:]]+\\)?}"
   #'devcontainer--lookup-variable
   string))

(defun devcontainer--lookup-variable (match)
  "Lookup a devcontainer variable according to MATCH.

Devcontainer defines some variable in
https://containers.dev/implementors/json_reference/#variables-in-devcontainerjson"
  (save-match-data
    (pcase (cons (match-string 1 match) (string-trim-left (or (match-string 2 match) "") ":"))
      (`("localWorkspaceFolderBasename" . ,_)
       (file-name-nondirectory (directory-file-name (devcontainer--root))))
      (`("containerEnv" . ,var-name)
       (alist-get var-name (devcontainer-container-environment) nil nil 'equal)))))

(defun devcontainer-remote-workdir ()
  "Determine the remote workspace folder."
  (when-let* ((config (devcontainer--read-devcontainer-json)))
    (devcontainer--interpolate-variable (file-name-as-directory (or (gethash "workspaceFolder" config)
                                                                    (devcontainer--determine-workspace-folder-from-container))))))

(defun devcontainer--read-devcontainer-json ()
  "Read the devcontainer.json file if it exists."
  (when-let* ((devcontainer-json-file (car (devcontainer-config-files))))
    (with-temp-buffer
      (insert-file-contents (concat (file-name-as-directory (devcontainer--root)) devcontainer-json-file))
      (devcontainer--bust-json-comments-in-buffer)
      (condition-case nil
          (json-parse-string (buffer-string))
        (json-parse-error (progn
                            (message "Broken json in devcontainer.json")
                            nil))))))

(defun devcontainer--determine-workspace-folder-from-container ()
  "Determine the remote workdir in the devcontainer."
  (devcontainer--inspect-container "{{(index .Mounts 0).Destination}}"))

(defun devcontainer--bust-json-comments-in-buffer ()
  "Bust comments in the `devcontainer.json' buffer."
  (while (re-search-forward "^\\([^\"]*?\\)\\(\\(\"[^\"]*\"[^\"]*?\\)*\\)//.*" nil t)
    (replace-match "\\1\\2")))

;;;###autoload
(defun devcontainer-tramp-dired (container-id container-name remote-user remote-workdir)
  "Open a Dired window inside devcontainer's working folder.

When called interactively, all the arguments are determined
automatically.  The arguments for the non-interactive call are set up in
a compatible way to `devcontainer-post-startup-hook'.

* CONTAINER-ID – a string representing the container id
* CONTAINER-NAME – a string representing the container-name
* REMOTE-USER – a string of the remote user name
* REMOTE-WORKDIR – the workdir path in the container."
  (interactive
   (if-let ((container-id (devcontainer-up-container-id)))
       (list container-id (devcontainer-container-name) (devcontainer-remote-user) (devcontainer-remote-workdir))
     (user-error "No running devcontainer for current project")))
  (let ((vec (format "/%s:%s@%s:%s" devcontainer-engine remote-user (or container-id container-name) remote-workdir)))
    (dired vec)))


(when (boundp 'savehist-additional-variables)
  (if (bound-and-true-p savehist-loaded)
      (add-to-list 'savehist-additional-variables 'devcontainer--customization-request-cache-alist)
    (add-hook 'savehist-mode-hook
              (lambda()
                (add-to-list 'savehist-additional-variables 'devcontainer--customization-request-cache-alist)))))


(provide 'devcontainer)

;;; devcontainer.el ends here
