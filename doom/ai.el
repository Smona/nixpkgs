;;; ai.el -*- lexical-binding: t; -*-
;;; configuration for coding assistants


(map! :leader
    (:prefix ("r" . "LLM Agents")
     :desc "Agent shell" "r" #'agent-shell-toggle
     :desc "Send region or error" "s" #'agent-shell-send-dwim))
(map! :map agent-shell-mode-map
    :n "g j" #'agent-shell-next-item
    :n "g k" #'agent-shell-previous-item)

(use-package! acp)
;; (use-package! agent-shell-sidebar
;;   :after agent-shell)
(use-package! agent-shell
  :after acp
  :config
  ;; Set claude as default agent
  (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
  (setq agent-shell-prefer-viewport-interaction t)
  (setq agent-shell-session-strategy 'prompt)
  ;; Prefer compact usage since we use a narrower window than the detailed one allows
  (setq agent-shell-show-context-usage-indicator t)
  ;; Evil state-specific RET behavior: insert mode = newline, normal mode = send
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'comint-send-input)
  (evil-define-key 'normal agent-shell-mode-map
    (kbd "RET") #'comint-send-input
    [tab]       #'agent-shell-ui-toggle-fragment-at-point
    (kbd "q")   #'agent-shell-toggle)

  ;; Evil bindings for viewport compose (edit) mode
  (evil-define-key 'normal agent-shell-viewport-edit-mode-map
    (kbd "RET") #'agent-shell-viewport-compose-send
    (kbd "q")   #'agent-shell-toggle
    (kbd "M-p") #'agent-shell-viewport-compose-peek-last
    (kbd "g m") #'agent-shell-viewport-set-session-mode
    (kbd "g v") #'agent-shell-viewport-set-session-model
    (kbd "g s") #'agent-shell-viewport-search-history
    (kbd "?")   #'agent-shell-viewport-help-menu)
  (evil-define-key 'insert agent-shell-viewport-edit-mode-map
    (kbd "C-c C-c")    #'agent-shell-viewport-compose-send
    (kbd "M-<return>") #'agent-shell-viewport-compose-send
    (kbd "<return>") #'agent-shell-viewport-compose-send
    (kbd "S-<return>") #'newline
    (kbd "M-p")        #'agent-shell-viewport-compose-peek-last
    (kbd "M-n")        #'agent-shell-viewport-next-history)

  ;; Evil bindings for viewport view mode
  (evil-define-key 'normal agent-shell-viewport-view-mode-map
    (kbd "r")   #'agent-shell-viewport-reply
    (kbd "RET") #'agent-shell-viewport-reply
    (kbd "y")   #'agent-shell-viewport-reply-yes
    (kbd "m")   #'agent-shell-viewport-reply-more
    (kbd "a")   #'agent-shell-viewport-reply-again
    (kbd "c")   #'agent-shell-viewport-reply-continue
    (kbd "C-c") #'agent-shell-viewport-interrupt
    (kbd "q")   #'agent-shell-toggle
    (kbd "j")   #'agent-shell-viewport-next-item
    (kbd "k")   #'agent-shell-viewport-previous-item
    (kbd "M-n")  #'agent-shell-viewport-next-page
    (kbd "M-p")  #'agent-shell-viewport-previous-page
    [tab]       #'agent-shell-ui-toggle-fragment-at-point
    (kbd "g m") #'agent-shell-viewport-set-session-mode
    (kbd "g v") #'agent-shell-viewport-set-session-model
    (kbd "g s") #'agent-shell-viewport-search-history
    (kbd "g t") #'agent-shell-viewport-view-traffic
    (kbd "g l") #'agent-shell-viewport-view-acp-logs
    (kbd "g o") #'agent-shell-viewport-open-transcript
    (kbd "g y") #'agent-shell-viewport-copy-session-id
    (kbd "?")   #'agent-shell-viewport-help-menu)

  ;; Evil bindings for diff mode
  (evil-define-key 'normal agent-shell-diff-mode-map
    (kbd "a")   #'agent-shell-diff-accept-all
    (kbd "x")   #'agent-shell-diff-reject-all
    (kbd "f")   #'agent-shell-diff-open-file
    (kbd "j")   #'diff-hunk-next
    (kbd "k")   #'diff-hunk-prev
    (kbd "q")   #'kill-current-buffer)
  (evil-set-initial-state 'agent-shell-diff-mode 'normal)

  ;; Start compose in insert mode, view in normal mode
  (evil-set-initial-state 'agent-shell-viewport-edit-mode 'insert)
  (evil-set-initial-state 'agent-shell-viewport-view-mode 'normal)
  ;; Disable line numbers in viewport buffers
  (add-hook 'agent-shell-viewport-edit-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'agent-shell-viewport-view-mode-hook (lambda () (display-line-numbers-mode -1)))

  ;; Display agent-shell (and viewport) buffers in a right side window.
  ;; Both shell and viewport buffers go through `agent-shell--display-buffer',
  ;; which honours `agent-shell-display-action'.
  ;; Not using agent-shell-sidebar because it doesn't support viewport/compose mode:
  ;; https://github.com/cmacrae/agent-shell-sidebar/issues/8
  (setq agent-shell-display-action
        '(display-buffer-in-side-window
          (side . right)
          (slot . 0)
          (window-width . 0.4)
          (dedicated . t)
          (window-parameters . ((no-delete-other-windows . t)))))
  )

