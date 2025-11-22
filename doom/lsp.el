;;; lsp.el -*- lexical-binding: t; -*-

;; LSP config

(setq lsp-use-plists t)

;; Format with prettier rather than ts-ls, for example
(setq +format-with-lsp nil)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(after! lsp-mode
        (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.node_modules\\'")
        (setq lsp-clients-typescript-prefer-use-project-ts-server t)
        (setq lsp-clients-typescript-tls-path "vtsls")
        (advice-add (if (progn (require 'json)
                        (fboundp 'json-parse-buffer))
                        'json-parse-buffer
                'json-read)
                :around
                #'lsp-booster--advice-json-parse)
        (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; Tailwind CSS support
(use-package! lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-server-path (executable-find "tailwindcss-language-server")
        ))
