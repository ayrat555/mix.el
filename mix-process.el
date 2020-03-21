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

(define-derived-mode mix-mode compilation-mode "Mix Mode."
  "Major mode for the Mix buffer."
  (setq major-mode 'mix-mode)
  (setq mode-name "Mix")
  (setq-local truncate-lines t))

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

(defun mix--start (name command)
  "Start the mix process NAME with the mix command COMMAND.
Returns the created process."
  (let* ((buffer (concat "*mix " name "*"))
         (project-root (mix--project-root))
         (cmd
          (mapconcat #'identity (list (shell-quote-argument mix--path-to-bin)
                                      command)
                     " "))
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

(provide 'mix)
;;; mix-process.el ends here
