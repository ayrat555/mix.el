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
  :type 'string)

(defcustom mix--command-test "test"
  "Subcommand used by `mix-test'."
  :type 'string)

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

(defun mix--output-filter ()
  "Remove control characters from output."
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun mix--start (name command)
  "Start the mix process NAME with the mix command COMMAND.
Returns the created process."
  (let* ((buffer (concat "*mix " name "*"))
         (project-root (mix--project-root))
         (cmd (concat (shell-quote-argument mix--path-to-bin) " " command))
         (default-directory (or project-root default-directory)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (and project-root
                              buffer-file-name
                              (string-prefix-p project-root (file-truename buffer-file-name)))))
    (compilation-start cmd 'mix-mode (lambda(_) buffer))
    (let ((process (get-buffer-process buffer)))
      process)))

;;;###autoload
(defun mix-compile ()
  "Run the mix compile command."
  (interactive)
  (mix--start "compile" mix--command-compile))

;;;###autoload
(defun mix-test ()
  "Run the mix test command."
  (interactive)
  (mix--start "test" mix--command-test))

;;;###autoload
(defun mix-test-current-buffer ()
  "Run the mix test for the current buffer."
  (interactive)
  (let* ((current-file-path (expand-file-name buffer-file-name))
         (test-command (concat mix--command-test " " current-file-path)))
    (mix--start "test" test-command)))

;;;###autoload
(defun mix-test-current-test ()
  "Run the mix test for the curret test."
  (interactive)
  (let* ((current-buffer-line-number (number-to-string (line-number-at-pos)))
         (current-file-path (expand-file-name buffer-file-name))
         (test-command (concat mix--command-test " " current-file-path ":" current-buffer-line-number)))
    (mix--start "test" test-command)))


(defvar mix-minor-mode-map (make-keymap) "Mix-mode keymap.")
(defvar mix-minor-mode nil)

;;;###autoload
(define-minor-mode mix-minor-mode
  "Mix minor mode. Used to hold keybindings for mix-mode.
\\{mix-minor-mode-map}"
  nil " mix" mix-minor-mode-map)

(define-key mix-minor-mode-map (kbd "C-c C-c C-c") 'mix-compile)
(define-key mix-minor-mode-map (kbd "C-c C-c C-t") 'mix-test)
(define-key mix-minor-mode-map (kbd "C-c C-c C-o") 'mix-test-current-buffer)
(define-key mix-minor-mode-map (kbd "C-c C-c C-f") 'mix-test-current-test)

(provide 'mix)
;;; mix-process.el ends here
