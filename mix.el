;;; mix.el --- Mix Major Mode

;;; Commentary:

;;; Code:

(defgroup mix nil
  "Mix process group."
  :prefix "mix-"
  :group 'mix)

(defcustom mix--path-to-bin
  (or (executable-find "mix")
      "/usr/bin/mix")
  "Path to the mix executable."
  :type 'file
  :group 'mix)

(defcustom mix--start-in-umbrella
  t
  "Start mix command in the umbrella app root or use a subproject."
  :type 'boolean
  :group 'mix)

(defcustom mix--command-compile "compile"
  "Subcommand used by `mix-compile'."
  :type 'string
  :group 'mix)

(defcustom mix--command-test "test"
  "Subcommand used by `mix-test'."
  :type 'string
  :group 'mix)

(defcustom mix--envs '("dev" "prod" "test")
  "The list of mix envs to use as defaults."
  :type 'list
  :group 'mix)

(defcustom mix--default-env nil
  "The default mix env to run mix commands with.
It's used in prompt"
  :type '(string boolean)
  :group 'mix)

(define-derived-mode mix-mode compilation-mode "Mix Mode."
  "Major mode for the Mix buffer."
  (setq major-mode 'mix-mode)
  (setq mode-name "Mix")
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (add-hook 'compilation-filter-hook 'mix--output-filter))

(defun mix--project-root ()
  "Find the root of the current mix project."
  (let ((closest-path (or buffer-file-name default-directory)))
    (if (and mix--start-in-umbrella (string-match-p (regexp-quote "apps") closest-path))
        (let* ((potential-umbrella-root-parts (butlast (split-string closest-path "/apps/")))
               (potential-umbrella-root (string-join potential-umbrella-root-parts ""))
               (umbrella-app-root (mix--find-closest-mix-file-dir potential-umbrella-root)))
          (or umbrella-app-root (mix--find-closest-mix-file-dir closest-path)))
      (mix--find-closest-mix-file-dir closest-path))))

(defun mix--find-closest-mix-file-dir (path)
  "Find the closest mix file to the current buffer PATH."
    (let ((root (locate-dominating-file path "mix.exs")))
    (when root
      (file-truename root))))

(defun mix--remove-mix-prefix-from-task (task)
  "Remove the first `mix` word from TASK string."
  (let* ((parts (split-string task "mix "))
        (parts-without-first-mix (cdr parts)))
    (concat (mapconcat 'identity parts-without-first-mix " "))))

(defun mix--all-available-tasks ()
  "List all available mix tasks."
  (let* ((project-root (mix--project-root))
         (default-directory (or project-root default-directory))
         (cmd (concat (shell-quote-argument mix--path-to-bin) " help"))
         (tasks-string (shell-command-to-string cmd))
         (raw-tasks (split-string tasks-string "\n")))
    (mapcar 'mix--remove-mix-prefix-from-task raw-tasks)))

(defun mix--output-filter ()
  "Remove control characters from output."
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun mix--start (name command &optional prompt)
  "Start the mix process NAME with the mix command COMMAND.
Returns the created process.
If PROMPT is non-nil, modifies command.  See `mix--prompt`"
  (let* ((buffer (concat "*mix " name "*"))
         (project-root (mix--project-root))
         (base-cmd (concat (shell-quote-argument mix--path-to-bin) " " command))
         (cmd (mix--prompt base-cmd prompt))
         (default-directory (or project-root default-directory)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (and project-root
                              buffer-file-name
                              (string-prefix-p project-root (file-truename buffer-file-name)))))
    (compilation-start cmd 'mix-mode (lambda(_) buffer))
    (get-buffer-process buffer)))

(defun mix--env-prompt ()
  "Prompt for a mix environment variable."
  (completing-read "mix-environment: " mix--envs nil nil mix--default-env))

(defun mix--additional-params ()
  "Prompt for additional mix task params."
  (read-string "additional mix task params: "))

(defun mix--prompt (command prefix)
  "Promp for additional params for mix task.
If PREFIX is equal to (4), prompt for mix MIX_ENV
and prepend it to COMMAND.  If PREFIX is equal to (16).
prompt for additional params for mix task and append them to COMMAND.
IF PREFIX is equal to (64), prompt both for MIX_ENV and additional params."
  (cond ((equal prefix '(4)) (concat "MIX_ENV=" (mix--env-prompt) " " command))
        ((equal prefix '(16)) (concat command " " (mix--additional-params)))
        ((equal prefix '(64)) (concat "MIX_ENV=" (mix--env-prompt) " " command " " (mix--additional-params)))
        (t command)))

;;;###autoload
(defun mix-compile (&optional prefix)
  "Run the mix compile command.
If PREFIX is non-nil, prompt for additional params.  See `mix--prompt`"
  (interactive "P")
  (mix--start "compile" mix--command-compile prefix))

;;;###autoload
(defun mix-test (&optional prefix)
  "Run the mix test command.
If PREFIX is non-nil, prompt for additional params.  See `mix--prompt`"
  (interactive "P")
  (mix--start "test" mix--command-test prefix))

;;;###autoload
(defun mix-test-current-buffer (&optional prefix)
  "Run the mix test for the current buffer.
If PREFIX is non-nil, prompt for additional params.  See `mix--prompt`"
  (interactive "P")
  (let* ((current-file-path (expand-file-name buffer-file-name))
         (test-command (concat mix--command-test " " current-file-path)))
    (mix--start "test" test-command prefix)))

;;;###autoload
(defun mix-test-current-test (&optional prefix)
  "Run the mix test for the curret test.
If PREFIX is non-nil, prompt for additional params.  See `mix--prompt`"
  (interactive "P")
  (let* ((current-buffer-line-number (number-to-string (line-number-at-pos)))
         (current-file-path (expand-file-name buffer-file-name))
         (test-command (concat mix--command-test " " current-file-path ":" current-buffer-line-number)))
    (mix--start "test" test-command prefix)))

(defun mix-execute-task (&optional prefix)
  "Select and run mix task.
If PREFIX is non-nil, prompt for additional params.  See `mix--prompt`"
  (interactive "P")
  (let* ((task-with-doc (completing-read "select mix task: " (mix--all-available-tasks)))
         (task (string-join (butlast (split-string task-with-doc "#" t split-string-default-separators)) "")))
    (mix--start "test" task prefix)))

(defvar mix-minor-mode-map (make-keymap) "Mix-mode keymap.")
(defvar mix-minor-mode nil)

;;;###autoload
(define-minor-mode mix-minor-mode
  "Mix minor mode. Used to hold keybindings for mix-mode.
\\{mix-minor-mode-map}"
  nil " mix" mix-minor-mode-map)

(define-key mix-minor-mode-map (kbd "C-c C-c C-e") 'mix-execute-task)
(define-key mix-minor-mode-map (kbd "C-c C-c C-c") 'mix-compile)
(define-key mix-minor-mode-map (kbd "C-c C-c C-t") 'mix-test)
(define-key mix-minor-mode-map (kbd "C-c C-c C-o") 'mix-test-current-buffer)
(define-key mix-minor-mode-map (kbd "C-c C-c C-f") 'mix-test-current-test)

(provide 'mix)

;;; mix.el ends here
