(require 'mocker)
(require 'devcontainer)

(defmacro fixture-tmp-dir (test-repo &rest body)
  (declare (indent 1))
  `(let* ((tmp-dir (make-temp-file "home" 'directory))
          (real-project-root-dir (concat (file-name-as-directory tmp-dir) "project"))
          (project-root-dir "~/project/")
          (devcontainer--project-info nil)
          (home-dir (getenv "HOME")))
     (make-directory real-project-root-dir)
     (shell-command-to-string (format "tar -xf test/%s.tar --directory %s" ,test-repo real-project-root-dir))
     (setenv "HOME" tmp-dir)
     (mocker-let ((project-current () ((:output (cons 'foo project-root-dir) :min-occur 0)))
                  (project-root (project) ((:input `((foo . ,project-root-dir)) :output project-root-dir :min-occur 0))))
       (unwind-protect
           ,@body
         (delete-directory tmp-dir 'recursively)
         (setenv "HOME" home-dir)))))

(ert-deftest devcontainer-command-unavailable ()
  (mocker-let ((executable-find (cmd) ((:input '("devcontainer") :output nil))))
    (should-not (devcontainer--find-executable))))

(ert-deftest devcontainer-command-available ()
  (mocker-let ((executable-find (cmd) ((:input '("devcontainer") :output "/path/to/devcontainer"))))
    (should (equal (devcontainer--find-executable) "/path/to/devcontainer"))))

(ert-deftest devcontainer-config-files-present ()
  (fixture-tmp-dir "test-repo-configs"
    (should (equal (devcontainer-config-files)
                   '(".devcontainer/devcontainer.json" ".devcontainer.json"
                     ".devcontainer/java/devcontainer.json" ".devcontainer/python/devcontainer.json")))))

(ert-deftest container-is-needed-not-known ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (should (devcontainer-container-needed))
    (should (equal devcontainer--project-info `(((foo . ,project-root-dir) . devcontainer-is-needed))))))

(ert-deftest container-is-not-needed ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (should-not (devcontainer-container-needed))
    (should (equal devcontainer--project-info `(((foo . ,project-root-dir) . no-devcontainer))))))

(ert-deftest container-is-not-needed-already-known ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (let ((devcontainer--project-info `(((foo . ,project-root-dir) . no-devcontainer))))
      (should-not (devcontainer-container-needed))
      (should (equal devcontainer--project-info `(((foo . ,project-root-dir) . no-devcontainer)))))))

(ert-deftest container-is-needed-already-known ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (let ((devcontainer--project-info `(((foo . ,project-root-dir) . devcontainer-is-needed))))
      (should (devcontainer-container-needed))
      (should (equal devcontainer--project-info `(((foo . ,project-root-dir) . devcontainer-is-needed)))))))

(ert-deftest container-id-no-container-defined ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (should-not (devcontainer-container-id))))

(ert-deftest container-id-no-container-set-up ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (let ((cmd-first `("container" "ls"
                       ,(format "--filter=label=devcontainer.local_folder=%s" real-project-root-dir)
                       "--format={{.ID}}"
                       "--all=false"))
          (cmd-second `("container" "ls"
                        ,(format "--filter=label=devcontainer.local_folder=%s" real-project-root-dir)
                        "--format={{.ID}}"
                        "--all=true")))
      (mocker-let ((devcontainer--call-engine-string-sync (&rest args) ((:input cmd-first :output nil)
                                                                        (:input cmd-second :output nil))))
      (should-not (devcontainer-container-id))))))

(ert-deftest container-id-container-set-up-and-not-running ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (let ((cmd-first `("container" "ls"
                       ,(format "--filter=label=devcontainer.local_folder=%s" real-project-root-dir)
                       "--format={{.ID}}"
                       "--all=false"))
          (cmd-second `("container" "ls"
                        ,(format "--filter=label=devcontainer.local_folder=%s" real-project-root-dir)
                        "--format={{.ID}}"
                        "--all=true")))
      (mocker-let ((devcontainer--call-engine-string-sync (&rest args) ((:input cmd-first :output nil)
                                                                        (:input cmd-second :output "abc"))))
        (should (equal (devcontainer-container-id) "abc"))))))

(ert-deftest container-id-container-set-up-and-running ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (let ((cmd `("container" "ls"
                 ,(format "--filter=label=devcontainer.local_folder=%s" real-project-root-dir)
                 "--format={{.ID}}"
                 "--all=false")))
      (mocker-let ((devcontainer--call-engine-string-sync (&rest args) ((:input cmd :output "abc"))))
        (should (equal (devcontainer-container-id) "abc"))))))

(ert-deftest container-id-no-container-running ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (let ((cmd `("container" "ls"
                 ,(format "--filter=label=devcontainer.local_folder=%s" real-project-root-dir)
                 "--format={{.ID}}")))
      (mocker-let ((devcontainer--call-engine-string-sync (&rest args) ((:input cmd :output nil))))
        (should-not (devcontainer-is-up))
        (should (equal devcontainer--project-info `(((foo . ,project-root-dir) . devcontainer-is-down))))))))

(ert-deftest container-id-container-running ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (let ((cmd `("container" "ls"
                 ,(format "--filter=label=devcontainer.local_folder=%s" real-project-root-dir)
                 "--format={{.ID}}")))
      (mocker-let ((devcontainer--call-engine-string-sync (&rest args) ((:input cmd :output "abc" :occur 1))))
        (should (equal (devcontainer-is-up) "abc"))
        (should (equal devcontainer--project-info `(((foo . ,project-root-dir) . devcontainer-is-up))))))))

(ert-deftest container-id-container-starting ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-starting))))
    (mocker-let ((project-current () ((:output '(foo . "~/foo/bar/")))))
      (should-not (devcontainer-is-up))
      (should (equal devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-starting)))))))

(ert-deftest container-id-container-failed ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-startup-failed))))
    (mocker-let ((project-current () ((:output '(foo . "~/foo/bar/")))))
      (should-not (devcontainer-is-up))
      (should (equal devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-startup-failed)))))))

(ert-deftest container-needed-container-failed ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-startup-failed))))
    (mocker-let ((project-current () ((:output '(foo . "~/foo/bar/")))))
      (should (devcontainer-container-needed))
      (should (equal devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-startup-failed)))))))

(ert-deftest container-invalidate-cache-in-project ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-startup-failed)
                                      ((bar . "~/bar/bar/") . devcontainer-is-up))))
    (mocker-let ((project-current () ((:output '(foo . "~/foo/bar/")))))
      (devcontainer-invalidate-cache)
      (should (equal devcontainer--project-info '(((bar . "~/bar/bar/") . devcontainer-is-up)))))))

(ert-deftest container-invalidate-cache-outside-project ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-startup-failed)
                                      ((bar . "~/bar/bar/") . devcontainer-is-up))))
    (mocker-let ((project-current () ((:output nil))))
      (devcontainer-invalidate-cache)
      (should (equal devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-startup-failed)
                                                  ((bar . "~/bar/bar/") . devcontainer-is-up)))))))

(ert-deftest container-up-no-devcontainer-needed ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (mocker-let ((get-buffer-create (name) ((:occur 0)))
                 (message (msg) ((:input '("Project does not use a devcontainer.") :output t))))
      (devcontainer-up))))

(ert-deftest container-up-devcontainer-needed-no-excecutable ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (mocker-let ((get-buffer-create (name) ((:input '("*devcontainer startup*") :occur 0)))
                 (devcontainer--find-executable () ((:output nil)))
                 (user-error (msg) ((:input '("Don't have devcontainer executable")))))
      (devcontainer-up))))

(ert-deftest devcontainer-image-id-non-existent ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (should-not (devcontainer-image-id))))

(ert-deftest devcontainer-image-id-existent ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (let* ((project-name (file-name-nondirectory (directory-file-name (file-name-directory project-root-dir))))
           (root-dir-name (directory-file-name real-project-root-dir))
           (cmd `("images" "--quiet" ,(format "vsc-%s-abcdef-uid" project-name))))
      (mocker-let ((secure-hash (algorithm string) ((:input `(sha256 ,root-dir-name) :output "abcdef")))
                   (project-name (pr) ((:input `((foo . ,project-root-dir)) :output project-name)))
                   (devcontainer--call-engine-string-sync (&rest cmd) ((:input cmd :output "d8f16cb43d9b"))))
        (should (equal (devcontainer-image-id) "d8f16cb43d9b"))))))

(ert-deftest container-up-devcontainer-needed-executable-available ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (let ((stdout-buf (get-buffer-create "*devcontainer startup*"))
          (cmdargs `("--docker-path" "/path/to/docker"
                     "--workspace-folder" ,(file-name-as-directory real-project-root-dir)
                     "up")))
      (mocker-let ((get-buffer-create (name) ((:input '("*devcontainer startup*") :output stdout-buf)))
                   (devcontainer--find-executable () ((:output "/some/path/devcontainer")))
                   (message (msg) ((:input '("Starting devcontainer..."))))
                   (user-error (msg) ((:input '("Don't have devcontainer executable.") :occur 0)))
                   (devcontainer--docker-path () ((:output "/path/to/docker")))
                   (make-comint-in-buffer (proc-name buf cmd startfile &rest args)
                                          ((:input (append `("devcontainer" ,stdout-buf "/some/path/devcontainer" nil) cmdargs))))
                   (get-buffer-process (buf) ((:input `(,stdout-buf) :output 'proc)))
                   (set-process-sentinel (proc sentinel) ((:input '(proc devcontainer--build-sentinel)))))
        (devcontainer-up)
        (should (equal devcontainer--project-info `(((foo . ,project-root-dir) . devcontainer-is-starting))))))))

(ert-deftest container-up-sentinel-success-no-hooks ()
  (with-temp-buffer
    (insert "{\"outcome\":\"success\",\"containerId\":\"8af87509ac808da58ff21688019836b1d73ffea8b421b56b5c54b8f18525f382\",\"remoteUser\":\"vscode\",\"remoteWorkspaceFolder\":\"/workspaces/devcontainer.el\"}")
    (mocker-let ((process-buffer (proc) ((:input '(myproc) :output (current-buffer))))
                 (process-name (proc) ((:input '(myproc) :output "devcontainer up")))
                 (project-current () ((:output '(foo . "~/foo/bar/"))))
                 (message (msg id) ((:input '("Successfully brought up container id %s" "8af87509ac80")))))
      (devcontainer--build-sentinel 'myproc "finished")
      (should (string-suffix-p "Process devcontainer up finished" (buffer-string)))
      (should (equal devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-up)))))))

(ert-deftest container-up-sentinel-success-with-hook ()
  (defvar container-id-was nil)
  (defvar container-name-was nil)
  (defvar remote-user-was nil)
  (defvar remote-workdir nil)
  (add-hook 'devcontainer-post-startup-hook
            (lambda (container-id container-name remote-user remote-workdir)
              (setq container-id-was container-id)
              (setq container-name-was container-name)
              (setq remote-user-was remote-user)
              (setq remote-workdir-was remote-workdir)))
  (with-temp-buffer
    (insert "{\"outcome\":\"success\",\"containerId\":\"8af87509ac80\",\"remoteUser\":\"vscode\",\"composeProjectName\":\"devcontainer-name\",\"remoteWorkspaceFolder\":\"/workspaces/name\"}")
    (mocker-let ((process-buffer (proc) ((:input '(myproc) :output (current-buffer))))
                 (process-name (proc) ((:input '(myproc) :output "devcontainer up")))
                 (project-current () ((:output '(foo . "~/foo/bar/"))))
                 (message (msg id) ((:input '("Successfully brought up container id %s" "8af87509ac80")))))
      (devcontainer--build-sentinel 'myproc "finished")
      (should (equal container-id-was "8af87509ac80"))
      (should (equal container-name-was "devcontainer-name"))
      (should (equal remote-user-was "vscode"))
      (should (equal remote-workdir-was "/workspaces/name"))
      (should (string-suffix-p "Process devcontainer up finished" (buffer-string)))
      (should (equal devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-up)))))))

(ert-deftest container-up-sentinel-defined-failure ()
  (with-temp-buffer
    (insert "{\"outcome\":\"error\",\"message\":\"Some error message\",\"description\":\"some description\"}")
    (mocker-let ((process-buffer (proc) ((:input '(myproc) :output (current-buffer))))
                 (process-name (proc) ((:input '(myproc) :output "devcontainer up")))
                 (project-current () ((:output '(foo . "~/foo/bar/"))))
                 (user-error (tmpl outcome msg desc) ((:input '("%s: %s – %s" "error" "Some error message" "some description")))))
      (devcontainer--build-sentinel 'myproc "exited abnormally with code 1")
      (should (string-suffix-p "Process devcontainer up exited abnormally with code 1" (buffer-string)))
      (should (equal devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-startup-failed)))))))

(ert-deftest container-up-sentinel-defined-garbled ()
  (with-temp-buffer
    (insert "Some non json stuff")
    (mocker-let ((process-buffer (proc) ((:input '(myproc) :output (current-buffer))))
                 (process-name (proc) ((:input '(myproc) :output "devcontainer up")))
                 (project-current () ((:output '(foo . "~/foo/bar/"))))
                 (user-error (msg) ((:input '("Garbled output from `devcontainer up'.  See *devcontainer startup* buffer")))))
      (devcontainer--build-sentinel 'myproc "exited abnormally with code 1")
      (should (string-suffix-p "Process devcontainer up exited abnormally with code 1" (buffer-string)))
      (should (equal devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-startup-failed)))))))

(ert-deftest kill-container-existent ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-up)))
        (cmd '("container" "ls"
               "--filter=label=devcontainer.local_folder=/home/me/foo/bar"
               "--format={{.ID}}")))
    (mocker-let ((devcontainer--call-engine-string-sync (&rest _cmd)
                                                        ((:input cmd :output "8af87509ac80" :occur 1)
                                                         (:input '("container" "kill" "8af87509ac80"))
                                                         (:input cmd :output nil)))
                 (devcontainer-container-needed () ((:output t)))
                 (project-current () ((:output '(foo . "~/foo/bar/"))))
                 (project-root (prg) ((:input '((foo . "~/foo/bar/")) :output "~/foo/bar/")))
                 (message (tmpl container-id) ((:input '("Killed container %s" "8af87509ac80")))))
      (let ((home-dir (getenv "HOME")))
        (unwind-protect
            (progn
              (setenv "HOME" "/home/me")
              (devcontainer-kill-container)
              (should (equal devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-down))))
              (setenv "HOME" home-dir)))))))

(ert-deftest kill-container-non-existent ()
  (mocker-let ((devcontainer-is-up () ((:output nil)))
               (user-error (msg) ((:input '("No container running")))))
    (devcontainer-kill-container)))

(ert-deftest remove-container-existent ()
  (setq devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-down)))
  (mocker-let ((devcontainer-container-id () ((:output "8af87509ac80")))
               (project-current () ((:output '(foo . "~/foo/bar/"))))
               (devcontainer--call-engine-string-sync (&rest cmd) ((:input '("container" "kill" "8af87509ac80"))
                                                                   (:input '("container" "rm" "8af87509ac80"))))
               (message (tmpl container-id) ((:input '("Removed container %s" "8af87509ac80")))))
    (devcontainer-remove-container)
    (should (equal devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-needed))))))

(ert-deftest remove-container-non-existent ()
  (mocker-let ((devcontainer-container-id () ((:output nil)))
               (user-error (msg) ((:input '("No container to be removed")))))
    (devcontainer-remove-container)))

(ert-deftest remove-image-non-existent-not-needed ()
  (mocker-let ((devcontainer-container-needed () ((:output nil)))
               (user-error (msg) ((:input '("No devcontainer for current project")))))
    (devcontainer-remove-image)))


(ert-deftest remove-image-non-existent-needed ()
  (mocker-let ((devcontainer-container-needed () ((:output t)))
               (devcontainer-image-id () ((:output nil)))
               (devcontainer--call-engine-string-sync (cmd) ((:occur 0))))
    (devcontainer-remove-image)))


(ert-deftest remove-image-existent-no-container ()
  (mocker-let ((devcontainer-container-needed () ((:output t)))
               (devcontainer-container-id () ((:output nil)))
               (devcontainer-image-id () ((:output "d8f16cb43d9b")))
               (devcontainer-remove-container () ((:occur 0)))
               (devcontainer--call-engine-string-sync (&rest args) ((:input '("image" "rm" "d8f16cb43d9b"))))
               (message (msg id) ((:input '("Removed image %s" "d8f16cb43d9b")))))
    (devcontainer-remove-image)))

(ert-deftest remove-image-existent-with-container ()
  (mocker-let ((devcontainer-container-needed () ((:output t)))
               (devcontainer-container-id () ((:output "abcdef")))
               (devcontainer-image-id () ((:output "d8f16cb43d9a")))
               (devcontainer-remove-container () ((:occur 1)))
               (devcontainer--call-engine-string-sync (&rest args) ((:input '("image" "rm" "d8f16cb43d9a"))))
               (message (msg id) ((:input '("Removed image %s" "d8f16cb43d9a")))))
    (devcontainer-remove-image)))

(ert-deftest restart-container-non-existent ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (mocker-let ((user-error (msg) ((:input '("No devcontainer for current project")))))
      (devcontainer-restart))))

(ert-deftest restart-container-not-up ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (mocker-let ((devcontainer-is-up () ((:output nil)))
                 (devcontainer-up (show-buffer) ((:input '(nil) :output t))))
      (devcontainer-restart))))

(ert-deftest restart-container-is-up ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (mocker-let ((devcontainer-is-up () ((:output t)))
                 (devcontainer-kill-container () ((:output t)))
                 (devcontainer-up (show-buffer) ((:input '(nil) :output t))))
      (devcontainer-restart))))

(ert-deftest rebuild-and-restart-container-non-existent ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (mocker-let ((user-error (msg) ((:input '("No devcontainer for current project")))))
      (devcontainer-rebuild-and-restart))))

(ert-deftest rebuild-and-restart-container-not-up ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (mocker-let ((devcontainer-is-up () ((:output nil)))
                 (devcontainer-remove-image () ((:output t)))
                 (devcontainer-up (show-buffer) ((:input '(nil) :output t))))
      (devcontainer-rebuild-and-restart))))

(ert-deftest rebuild-and-restart-container-is-up ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (mocker-let ((devcontainer-is-up () ((:output t)))
                 (devcontainer-remove-container () ((:output t)))
                 (devcontainer-remove-image () ((:output t)))
                 (devcontainer-up (show-buffer) ((:input '(nil) :output t))))
      (devcontainer-rebuild-and-restart))))

(ert-deftest devcontainer-prefix-mode-off-no-running-devcontainer ()
  (devcontainer-mode -1)
  (should-not (devcontainer-command-prefix)))

(ert-deftest devcontainer-prefix-mode-on-no-running-devcontainer ()
  (devcontainer-mode 1)
  (mocker-let ((devcontainer-is-up () ((:output nil))))
    (should-not (devcontainer-command-prefix))))

(ert-deftest devcontainer-prefix-mode-off-with-running-devcontainer ()
  (devcontainer-mode -1)
  (mocker-let ((devcontainer-is-up () ((:occur 0))))
    (should-not (devcontainer-command-prefix))))

(ert-deftest devcontainer-prefix-mode-on-with-running-devcontainer ()
  (devcontainer-mode 1)
  (mocker-let ((devcontainer-is-up () ((:output t)))
               (project-current () ((:output '(foo . "~/foo/bar/") :min-occur 1)))
               (project-root (project) ((:input '((foo . "~/foo/bar/")) :output "~/foo/bar/"))))
    (let ((home-dir (getenv "HOME")))
      (unwind-protect
         (progn
           (setenv "HOME" "/home/me")
           (should (equal (devcontainer-command-prefix) "devcontainer exec --workspace-folder /home/me/foo/bar/ ")))
        (setenv "HOME" home-dir)))))

(ert-deftest compile-start-advice-devcontainer-down ()
  (devcontainer-mode 1)
  (fixture-tmp-dir "test-repo-devcontainer"
    (mocker-let ((devcontainer-is-up () ((:output nil))))
      (should-error (devcontainer--compile-start-advice (lambda (&rest _) t) "my-command foo")))))

(ert-deftest compilation-start-advised ()
  (devcontainer-mode 1)
  (should (advice-member-p 'devcontainer--compile-start-advice 'compilation-start))
  (devcontainer-mode -1)
  (should-not (advice-member-p 'devcontainer--compile-start-advice 'compilation-start)))

(ert-deftest compile-start-advice-no-devcontainer-mode ()
  (devcontainer-mode -1)
  (mocker-let ((my-compile-fun (command &rest rest) ((:input '("my-command foo" mode name-function hight-light-regexp continue)))))
    (should-not (devcontainer-advisable))
    (should (equal (devcontainer-advise-command "foo-command") "foo-command"))
    (devcontainer--compile-start-advice #'my-compile-fun "my-command foo" 'mode 'name-function 'hight-light-regexp 'continue)))

(ert-deftest compile-start-advice-devcontainer-mode-no-project ()
  (devcontainer-mode 1)
  (mocker-let ((project-current () ((:output nil)))
               (my-compile-fun (command &rest rest) ((:input '("my-command foo")))))
    (should-not (devcontainer-advisable))
    (should (equal (devcontainer-advise-command "foo-command") "foo-command"))
    (devcontainer--compile-start-advice #'my-compile-fun "my-command foo")))

(ert-deftest compile-start-advice-devcontainer-mode-no-devcontainer ()
  (devcontainer-mode 1)
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (mocker-let ((my-compile-fun (command &rest rest) ((:input '("my-command foo")))))
      (should-not (devcontainer-advisable))
      (should (equal (devcontainer-advise-command "foo-command") "foo-command"))
      (devcontainer--compile-start-advice #'my-compile-fun "my-command foo"))))

(ert-deftest compile-start-advice-devcontainer-up ()
  (devcontainer-mode 1)
  (fixture-tmp-dir "test-repo-devcontainer"
    (let ((cmd "docker exec --workdir /workspaces/the-project/  --env PATH=/home/vscode/bin cdefab my-command foo"))
      (mocker-let ((my-compile-fun (command &rest rest) ((:input `(,cmd))))
                   (devcontainer-remote-workdir () ((:output "/workspaces/the-project/")))
                   (devcontainer-remote-environment () ((:output '(("PATH" . "/home/vscode/bin")))))
                   (devcontainer-is-up () ((:output "cdefab"))))
        (should (devcontainer-advisable))
        (should (equal (devcontainer-advise-command "my-command foo") cmd))
        (devcontainer--compile-start-advice #'my-compile-fun "my-command foo")))))

(ert-deftest compilation-start-no-exclude-simple ()
  (devcontainer-mode 1)
  (fixture-tmp-dir "test-repo-devcontainer"
    (let ((cmd "docker exec --workdir /workspaces/project/  --env PATH=/usr/bin abcdef grep foo")
          (devcontainer-execute-outside-container nil))
      (mocker-let ((my-compile-fun (command &rest rest) ((:input `(,cmd))))
                   (devcontainer-remote-workdir () ((:output "/workspaces/project/")))
                   (devcontainer-remote-environment () ((:output '(("PATH" . "/usr/bin")))))
                   (devcontainer-is-up () ((:output "abcdef"))))
        (should (equal (devcontainer-advise-command "grep foo") cmd))
        (devcontainer--compile-start-advice #'my-compile-fun "grep foo")))))

(ert-deftest compilation-start-exclude-simple ()
  (devcontainer-mode 1)
  (fixture-tmp-dir "test-repo-devcontainer"
    (let ((cmd "grep foo")
          (devcontainer-execute-outside-container '("grep" "rg")))
    (mocker-let ((my-compile-fun (command &rest rest) ((:input `(,cmd)))))
      (should (equal (devcontainer-advise-command "grep foo") "grep foo"))
      (devcontainer--compile-start-advice #'my-compile-fun "grep foo")))))

(ert-deftest compilation-start-exclude-absolute-path ()
  (devcontainer-mode 1)
  (fixture-tmp-dir "test-repo-devcontainer"
    (let ((cmd "/usr/bin/rg foo")
          (devcontainer-execute-outside-container '("grep" "rg")))
    (mocker-let ((my-compile-fun (command &rest rest) ((:input `(,cmd)))))
      (devcontainer--compile-start-advice #'my-compile-fun "/usr/bin/rg foo")))))

(ert-deftest lighter-not-on-project-no-project-info ()
  (let ((devcontainer--project-info nil))
    (mocker-let ((project-current () ((:output nil)))
                 (devcontainer--find-executable () ((:output "/some/path/devcontainer"))))
      (should (string= (devcontainer--lighter) "DevC?")))))

(ert-deftest lighter-on-projects-call-update-when-no-project-info ()
  (let ((devcontainer--project-info nil))
    (mocker-let ((project-current () ((:output '(foo . "~/foo/bar/") :min-occur 0)))
                 (devcontainer--find-executable () ((:output "/some/path/devcontainer")))
                 (devcontainer--update-project-info () ((:input '() :output 'no-devcontainer))))
      (should (string= (devcontainer--lighter) "DevC-")))))

(ert-deftest lighter-on-project-with-no-container ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . no-devcontainer))))
    (mocker-let ((project-current () ((:output '(foo . "~/foo/bar/") :min-occur 0)))
                 (devcontainer--find-executable () ((:output "/some/path/devcontainer"))))
      (should (string= (devcontainer--lighter) "DevC-")))))

(ert-deftest lighter-on-project-with-container-needed ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-needed))))
    (mocker-let ((project-current () ((:output '(foo . "~/foo/bar/") :min-occur 0)))
                 (devcontainer--find-executable () ((:output "/some/path/devcontainer"))))
      (should (string= (devcontainer--lighter) "DevC+")))))

(ert-deftest lighter-on-project-with-container-down ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-down))))
    (mocker-let ((project-current () ((:output '(foo . "~/foo/bar/") :min-occur 0)))
                 (devcontainer--find-executable () ((:output "/some/path/devcontainer"))))
      (should (string= (devcontainer--lighter) "DevC>")))))

(ert-deftest lighter-on-project-with-container-starting ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-starting))))
    (mocker-let ((project-current () ((:output '(foo . "~/foo/bar/") :min-occur 0)))
                 (devcontainer--find-executable () ((:output "/some/path/devcontainer"))))
      (should (string= (devcontainer--lighter) "DevC*")))))

(ert-deftest lighter-on-project-with-container-startup-failed ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-startup-failed))))
    (mocker-let ((project-current () ((:output '(foo . "~/foo/bar/") :min-occur 0)))
                 (devcontainer--find-executable () ((:output "/some/path/devcontainer"))))
      (should (string= (devcontainer--lighter) "DevC#")))))

(ert-deftest lighter-projects-on-project-with-container-is-up ()
  (let ((devcontainer--project-info '(((foo . "~/foo/bar/") . devcontainer-is-up))))
    (mocker-let ((project-current () ((:output '(foo . "~/foo/bar/") :min-occur 0)))
                 (devcontainer--find-executable () ((:output "/some/path/devcontainer"))))
      (should (string= (devcontainer--lighter) "DevC!")))))

(ert-deftest lighter-projects-on-project-with-no-executable ()
  (mocker-let ((devcontainer--find-executable () ((:output nil))))
    (should (string= (devcontainer--lighter) "DevC¿"))))

(ert-deftest devcontainer-state-no-container ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (should (equal (devcontainer--update-project-info) 'no-devcontainer))
    (should (equal devcontainer--project-info `(((foo . ,project-root-dir) . no-devcontainer))))))

(ert-deftest devcontainer-state-container-down ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (let ((cmd `("container" "ls"
                 ,(format "--filter=label=devcontainer.local_folder=%s" real-project-root-dir)
                 "--format={{.ID}}")))
      (mocker-let ((devcontainer--call-engine-string-sync (&rest args) ((:input cmd :output nil))))
        (should (equal (devcontainer--update-project-info) 'devcontainer-is-down))
        (should (equal devcontainer--project-info `(((foo . ,project-root-dir) . devcontainer-is-down))))))))

(ert-deftest devcontainer--container-env-no-container ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (mocker-let ((devcontainer-container-id () ((:output nil))))
      (should-not (devcontainer-container-environment)))))

(ert-deftest devcontainer--container-env-container-up ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (mocker-let ((devcontainer-container-id () ((:output "abcdef")))
                 (process-lines (cmd &rest args) ((:input '("docker" "container" "inspect"
                                                            "abcdef"
                                                            "--format={{json .Config.Env}}")
                                                   :output '("[\"MY_VAR=uuu\",\"SOME_VAR=foo\",\"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\"]")))))
      (should (equal (devcontainer-container-environment) '(("MY_VAR" . "uuu")
                                                            ("SOME_VAR" . "foo")
                                                            ("PATH" . "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")))))))
(ert-deftest devcontainer--remote-user-no-container ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (mocker-let ((devcontainer-container-id () ((:output nil))))
      (should-not (devcontainer-remote-user)))))

(ert-deftest devcontainer--container-user-container-up ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (mocker-let ((devcontainer-container-id () ((:output "abcdef")))
                 (process-lines (cmd &rest args) ((:input '("docker" "container" "inspect"
                                                            "abcdef"
                                                            "--format={{index .Config.Labels \"devcontainer.metadata\"}}")
                                                   :output '("[{\"remoteUser\":\"vscode\"}]")))))
      (should (equal (devcontainer-remote-user) "vscode")))))

(ert-deftest devcontainer--remote-env-no-container ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (mocker-let ((devcontainer-container-id () ((:output nil))))
      (should-not (devcontainer-remote-environment)))))

(ert-deftest devcontainer--remote-env-container-up ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (mocker-let ((devcontainer-container-id () ((:output "abcdef")))
                 (devcontainer--root () ((:output "/foo/bar/this-project/")))
                 (devcontainer-container-environment () ((:output '(("PATH" . "/usr/local/bin:/usr/bin:/bin") ("SOME_VAR" . "foo")))))
                 (process-lines (cmd &rest args) ((:input '("docker" "container" "inspect"
                                                            "abcdef"
                                                            "--format={{index .Config.Labels \"devcontainer.metadata\"}}")
                                                   :output '("[{\"id\":\"./local-features/git\"},{\"id\":\"ghcr.io/devcontainers/features/common-utils:2\"},{\"remoteUser\":\"vscode\"},{\"postCreateCommand\":\"uvsync\",\"containerEnv\":{\"SOME_VAR\":\"foo\"},\"remoteEnv\":{\"PATH\":\"/workspaces/${localWorkspaceFolderBasename}/.venv/bin:${containerEnv:PATH}\",\"VIRTUAL_ENV\":\"/workspaces/${localWorkspaceFolderBasename}/.venv/\"}}]")))))
      (should (equal (devcontainer-remote-environment)
                     '((PATH . "/workspaces/this-project/.venv/bin:/usr/local/bin:/usr/bin:/bin")
                       (VIRTUAL_ENV . "/workspaces/this-project/.venv/")
                       ))))))

(ert-deftest devcontainer--workdir-no-devcontainer ()
  (fixture-tmp-dir "test-repo-no-devcontainer"
    (should-not (devcontainer-remote-workdir))))

(ert-deftest devcontainer--workdir-devcontainer-no-workspace-parameter ()
  (fixture-tmp-dir "test-repo-devcontainer"
    (mocker-let ((devcontainer--root () ((:output (file-name-as-directory real-project-root-dir)))))
      (should (equal (devcontainer-remote-workdir) "/")))))

(ert-deftest devcontainer--workdir-devcontainer-workspace-parameter-clean-json ()
  (fixture-tmp-dir "test-repo-devcontainer-workspace-folder"
    (mocker-let ((devcontainer--root () ((:output (file-name-as-directory real-project-root-dir)))))
      (should (equal (devcontainer-remote-workdir) "/workspaces/project/")))))

(ert-deftest devcontainer--workdir-devcontainer-workspace-parameter-json-with-comments ()
  (fixture-tmp-dir "test-repo-devcontainer-comments-json"
    (mocker-let ((devcontainer--root () ((:output (file-name-as-directory real-project-root-dir)))))
      (should (equal (devcontainer-remote-workdir) "/workspaces/project/")))))


(ert-deftest devcontainer--call-engine-string-sync-null-result ()
  (mocker-let ((devcontainer--docker-path () ((:output (concat default-directory "test/docker-fake.sh")))))
    (should-not (devcontainer--call-engine-string-sync "null-result"))))

(ert-deftest devcontainer--call-engine-string-sync-error ()
  (mocker-let ((devcontainer--docker-path () ((:output (concat default-directory "test/docker-fake.sh")))))
    (should-error (devcontainer--call-engine-string-sync "error"))))

(ert-deftest devcontainer--call-engine-string-sync-one-line-result ()
  (mocker-let ((devcontainer--docker-path () ((:output (concat default-directory "test/docker-fake.sh")))))
    (should (equal (devcontainer--call-engine-string-sync "one-line" "foobar") "foobar"))))


;;; devcontainer.el-test.el ends here
