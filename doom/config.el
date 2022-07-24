;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Mel Bourgeois"
      user-mail-address "mason.bourgeois@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq! doom-font (font-spec :family "FuraCode NF" :size 13.0 :weight 'normal :width 'normal))
(setq! doom-unicode-font (font-spec :family "FuraCode NF" :size 13.0 :weight 'normal :width 'normal))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(setq doom-theme 'doom-monokai-octagon)
;; Other themes I like: doom-rouge, doom-challenger-deep, doom-monokai-spectrum

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Set file paths
(setq org-directory "~/org/")
(setq org-roam-directory (file-truename "~/org"))
; Make doom/goto-private-config-file et al point to the source rather than the
; built nix store object
(setq doom-private-dir "~/.config/nixpkgs/doom/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Hotkeys
(map! :leader
      :desc "Clear search highlight"
      "s c" #'evil-ex-nohighlight)
(map! :leader
      :desc "Resize window interactively"
      "w c" #'+hydra/window-nav/body)
(map! :leader
      :desc "Repeat a recent search"
      "s R" #'vertico-repeat-select)

;; MX Master 3 forward/back buttons
(map! "<mouse-8>" #'previous-buffer)
(map! "<mouse-9>" #'next-buffer)


(after! auth-source
        ;; Allow non-encrypted authinfo
        (add-to-list 'auth-sources "~/.authinfo"))

(after! evil-snipe
        (setq evil-snipe-scope 'buffer))

(after! vterm
        (setq vterm-timer-delay 0.01))

;; track habits
(add-to-list 'org-modules 'org-habit)

(after! org
        ;; Use non-monospace font by default in org mode
        ;; (add-hook 'org-mode-hook 'variable-pitch-mode)

        ;; hard wrap org mode
        (add-hook 'org-mode-hook 'auto-fill-mode)
        ;; disable line numbers in org mode
        (add-hook 'org-mode-hook (lambda () (setq-local display-line-numbers nil)))

        ;; Add CLOSED: timestamps when marking done
        (setq org-log-done 'time)

        ;; Use a special TODO state for repeating tasks
        (setq org-todo-repeat-to-state "LOOP")

        ;; Only show upcoming deadlines when they're getting close
        (setq org-deadline-warning-days 3)

        ;; Automatically display inline images
        (add-hook 'org-mode-hook 'org-display-inline-images)

        ;; Log state changes into a collapsible drawer
        (setq org-log-into-drawer t)

        ;; Hide text formatting characters in org mode
        (setq org-hide-emphasis-markers t))


(after! org-agenda
        (setq org-agenda-files '("~/org" "~/org/daily")))

(after! org-roam
        (org-roam-db-autosync-mode)
        ;; Strip timestamp out of roam capture template
        (setq org-roam-capture-templates
        '(("d" "default" plain
                "%?"
                :if-new (file+head "${slug}.org" "#+title: ${title}\n")
                :unnarrowed t)))

        ; Workaround an upstream issue with evil, as described in https://github.com/syl20bnr/spacemacs/issues/14137
        (defadvice org-roam-node-insert (around append-if-in-evil-normal-mode activate compile)
        "If in evil normal mode and cursor is on a whitespace character, then go into
        append mode first before inserting the link. This is to put the link after the
        space rather than before."
        (let ((is-in-evil-normal-mode (and (bound-and-true-p evil-mode)
                                        (not (bound-and-true-p evil-insert-state-minor-mode))
                                        (looking-at "[[:blank:]]"))))
        (if (not is-in-evil-normal-mode)
        ad-do-it
        (evil-append 0)
        ad-do-it
        (evil-normal-state)))))

(after! org-roam-dailies
        ;; Customizations to road dailies capture templates
        (setq org-roam-dailies-capture-templates '(
                ("d" "default" entry "* %<%H:%M> %?"
                :if-new  (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>")
                )
        ))
        )

;; org-roam-ui config

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package! ox-slack
        :after org)

;; Various editor enhancements

;; Auto-update matching HTML tag
(use-package! auto-rename-tag
     :hook (typescript-tsx-mode . auto-rename-tag-mode)
     :hook (web-mode . auto-rename-tag-mode)
     :config
     (auto-rename-tag-mode t))

;; LSP config

;; I haven't been able to get graphql-lsp to start without crashing,
;; and it takes ts-ls down with it.
(setq lsp-disabled-clients '(graphql-lsp))

;; Format with prettier rather than ts-ls, for example
(setq +format-with-lsp nil)

; The following functions make it simpler to run syncthing in the background on systems that don't
; have an init system, such as WSL.

(defun syncthing-start ()
        "Start syncthing background process"
        (interactive)
        (start-process "syncthing" "*syncthing*" "syncthing" "-no-browser"))

(defun syncthing-stop ()
        "Stop syncthing background process"
        (interactive)
        (kill-process "syncthing"))

(use-package! wakatime-mode
  :config
  (setq wakatime-api-key "8339e7e5-7fba-4939-a812-d3108ee1f0f5")
  (global-wakatime-mode))

(setq +treemacs-git-mode 'deferred)

(after! treemacs
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq treemacs-width-is-initially-locked nil))

;; Render youtube links as clickable inline images in emacs, and youtube embeds
;; in HTML exports. Source: http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html

(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))
