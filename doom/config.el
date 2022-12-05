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
(setq! doom-font (font-spec :family "Cascadia Code" :size 13.0 :weight 'normal :width 'normal))
;; Nerd fonts break italics rendering within emacs for many fonts, so we use
;; the NF variant only for unicode glyphs. It appears that glyphs from other
;; languages will still use the default doom-font.
(setq! doom-unicode-font (font-spec :family "FiraCode Nerd Font"))

;; Give lines more space, while keeping text centered.
;; Source: https://github.com/syl20bnr/spacemacs/issues/10502#issuecomment-404453194
(setq-default default-text-properties '(line-spacing 0.15 line-height 1.15))
;; No extra line spacing in vterm, where it can make text taller than powerline symbols & break continuity.
(add-hook! 'vterm-mode-hook (setq-local default-text-properties '(line-spacing 0 line-height 1)))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(setq doom-theme 'doom-tokyo-night)
;; Other themes I like: doom-rouge, doom-challenger-deep, doom-monokai-spectrum, doom-monokai-octagon, doom-city-lights, doom-material, doom-palenight, doom-tokyo-night

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

;; Theme customization
(custom-set-faces!
  ;; Italic comments
  '(font-lock-comment-face :slant italic))

;; The below was borrowed from https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;; TODO: inherit font color from default face
 (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (headline           `(:inherit default :weight bold)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

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
;; forward/back touchpad swipes with the gesture-improvements gnome extension
;; https://extensions.gnome.org/extension/4245/gesture-improvements/
(map! "<269025062>" #'previous-buffer)
(map! "<269025063>" #'next-buffer)

;; Scrolling customization
(setq scroll-margin 6)

;; Enable horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction nil)
(setq mouse-wheel-progressive-speed nil)

;; Focus follows mouse, just like in the desktop
(setq mouse-autoselect-window t)

(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-height 35)

(after! evil-snipe
        (setq evil-snipe-scope 'buffer))

(after! vterm
        (setq vterm-timer-delay 0.03))

;; (after! magit
        ;; magit-delta is disabled until they fix the performance on large diffs:
        ;; https://github.com/dandavison/magit-delta/issues/9
        ;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

        ;; Performance optimizations
        ;; https://magit.vc/manual/magit/Performance.html
        ;; Disabled since magit-delta was causing most of the performance issues
        ;; (setq magit-refresh-status-buffer nil))

; Don't require saving a file to update the git gutters.
; NOTE: keep an eye on the performance of this convenience feature.
(after! git-gutter
        (setq git-gutter:update-interval 2))

;; track habits
(add-to-list 'org-modules 'org-habit)

(after! org
        ;; Use non-monospace font by default in org mode
        ;; (add-hook 'org-mode-hook 'variable-pitch-mode)
        ;;
        ;; TODO: add author & date in preamble: https://emacs.stackexchange.com/questions/45191/how-to-place-author-and-date-under-title
        (setq org-html-postamble "
        <footer>
                <a href=\"https://github.com/Smona\">
                        <svg class=\"logo\" viewBox=\"0 0 1024 1024\" fill=\"none\" xmlns=\"http://www.w3.org/2000/svg\">
                                <path fill-rule=\"evenodd\" clip-rule=\"evenodd\" d=\"M8 0C3.58 0 0 3.58 0 8C0 11.54 2.29 14.53 5.47 15.59C5.87 15.66 6.02 15.42 6.02 15.21C6.02 15.02 6.01 14.39 6.01 13.72C4 14.09 3.48 13.23 3.32 12.78C3.23 12.55 2.84 11.84 2.5 11.65C2.22 11.5 1.82 11.13 2.49 11.12C3.12 11.11 3.57 11.7 3.72 11.94C4.44 13.15 5.59 12.81 6.05 12.6C6.12 12.08 6.33 11.73 6.56 11.53C4.78 11.33 2.92 10.64 2.92 7.58C2.92 6.71 3.23 5.99 3.74 5.43C3.66 5.23 3.38 4.41 3.82 3.31C3.82 3.31 4.49 3.1 6.02 4.13C6.66 3.95 7.34 3.86 8.02 3.86C8.7 3.86 9.38 3.95 10.02 4.13C11.55 3.09 12.22 3.31 12.22 3.31C12.66 4.41 12.38 5.23 12.3 5.43C12.81 5.99 13.12 6.7 13.12 7.58C13.12 10.65 11.25 11.33 9.47 11.53C9.76 11.78 10.01 12.26 10.01 13.01C10.01 14.08 10 14.94 10 15.21C10 15.42 10.15 15.67 10.55 15.59C13.71 14.53 16 11.53 16 8C16 3.58 12.42 0 8 0Z\" transform=\"scale(64)\" fill=\"#1B1F23\"/>
                        </svg>
                </a>
                <a href=\"https://www.linkedin.com/in/m3ourgeois/\">
                        <svg class=\"logo\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\">
                                <path xmlns=\"http://www.w3.org/2000/svg\" d=\"M12 0c-6.627 0-12 5.373-12 12s5.373 12 12 12 12-5.373 12-12-5.373-12-12-12zm-2 16h-2v-6h2v6zm-1-6.891c-.607 0-1.1-.496-1.1-1.109 0-.612.492-1.109 1.1-1.109s1.1.497 1.1 1.109c0 .613-.493 1.109-1.1 1.109zm8 6.891h-1.998v-2.861c0-1.881-2.002-1.722-2.002 0v2.861h-2v-6h2v1.093c.872-1.616 4-1.736 4 1.548v3.359z\"/>
                        </svg>
                </a>
        </footer>
        ")
        ;; TODO: extract to separate file:
        ;; https://stackoverflow.com/questions/19614104/how-to-tell-org-mode-to-embed-my-css-file-on-html-export
        ;; https://emacs.stackexchange.com/questions/3374/set-the-background-of-org-exported-code-blocks-according-to-theme
        (setq org-html-style "
        <link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/meyer-reset/2.0/reset.min.css\" integrity=\"sha512-NmLkDIU1C/C88wi324HBc+S2kLhi08PN5GDeUVVVC/BVt/9Izdsc9SVeVfA1UZbY3sHUlDSyRXhCzHfr6hmPPw==\" crossorigin=\"anonymous\" referrerpolicy=\"no-referrer\" />
        <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
        <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
        <link href=\"https://fonts.googleapis.com/css2?family=Fira+Code&family=Noto+Sans:ital,wght@0,400;0,700;1,400;1,700&display=swap\" rel=\"stylesheet\">
        <style type=\"text/css\">

                body {
                        --background: #1d1a2a;
                        --background-dark: #0e0c17;
                        --background-light: #38344b;
                        --rounding: 12px;
                        --font-family-monospace: 'Fira Code', monospace;
                        font-family: 'Noto Sans', sans-serif;
                        line-height: 1.5;
                        background-color: var(--background);
                        color: #e0e0e0;
                        padding: 0;
                        margin: 0;
                }

                img {
                        max-width: 100%;
                }

                strong, b {
                        font-weight: bold;
                }

                em, i {
                        font-style: italic;
                }

                p {
                        margin: 0.7em 0;
                        line-height: inherit;
                }

                pre, code {
                        font-family: var(--font-family-monospace);
                        font-size: 0.9em;
                }
                .org-src-container {
                        position: relative;
                }
                pre.src {
                        background-color: var(--background-dark);
                        padding: 0.7em;
                        margin: 1.4em 0;
                        border: none;
                        border-radius: var(--rounding);
                        /* Pin language tag to container rather than scroll area */
                        position: unset;
                }
                pre.src::before {
                        top: -1em;
                        right: 0;
                        background-color: var(--background-dark);
                        padding: 3px 6px;
                        border-top-right-radius: var(--rounding);
                }
                code {
                        background: var(--background-dark);
                        padding: 0.1em 0.3em;
                        border-radius: 3px;
                        white-space: nowrap;
                }

                a {
                        color: inherit;
                        text-decoration: underline #198d93;
                        text-decoration-thickness: auto;
                        transition: all 100ms ease-out;
                        text-underline-offset: 3px;
                        text-decoration-thickness: 3px;
                        text-decoration-skip-ink: none;
                }
                a:hover {
                        text-decoration-thickness: 1.4em;
                        text-underline-offset: -1em;
                }

                ul {
                        list-style-type: disc;
                }
                ol {
                        list-style-type: decimal;
                }
                ol li::marker {
                        font-weight: bold;
                }

                li {
                        margin-left: 1.8em;
                        margin-top: 0.3em;
                        margin-bottom: 0.5em;
                }

                table {
                        width : 100%;
                        border-radius: var(--rounding);
                        background-color: var(--background-light);
                        border: none;
                }

                /* Primarily intended for youtube embeds */
                iframe {
                        width: 100%;
                        margin: 1.5em auto;
                        display: block;
                        border-radius: var(--rounding);
                }

                #content {
                        max-width: 40em;
                        padding: 0 1em;
                }

                .footref {
                        vertical-align: super;
                        font-size: 0.8em;
                }

                h1, h2, h3, h4, h5, h6 {
                        font-weight: bold;
                        margin: 0.9em 0 0.3em;
                }
                h1 { font-size: 2.0rem; }
                h2 { font-size: 1.7rem; }
                h3 { font-size: 1.4rem; }
                h4 { font-size: 1.2rem; }

                .tag {
                        /* Hide tags */
                        display: none;
                }

                footer {
                        padding: 2.8em;
                        margin-top: 4rem;
                        background: var(--background-dark);
                        display: flex;
                        gap: 1.4em;
                        justify-content: center;
                        align-items: center;
                }

                .logo {
                        width: 44px;
                        height: 44px;
                        opacity: 0.7;
                        transition: all 100ms ease-out;
                }
                .logo:hover {
                        opacity: 1;
                }

                .logo path {
                        fill: currentColor;
                }
        </style>
        ")

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

        ;; Render youtube links as clickable inline images in emacs, and youtube embeds
        ;; in HTML exports. Source: http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
        ;; TODO: extract to function

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

(use-package! org-super-agenda
  :after org
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-header-map (make-sparse-keymap)) ;; Prevent conflicts with doom keybinds
  (setq org-super-agenda-groups '(
        (:name "Habits" :habit t)
        (:name "Important" :priority>= "B")
        (:name "Today" :time-grid t :scheduled today)
        (:name "Cobalt" :tag "cobalt")
)))

;; Various editor enhancements

;; LSP config

;; I haven't been able to get graphql-lsp to start without crashing,
;; and it takes ts-ls down with it.
(setq lsp-disabled-clients '(graphql-lsp))

;; Format with prettier rather than ts-ls, for example
(setq +format-with-lsp nil)

; The following functions make it simpler to run syncthing in the background on systems that don't
; have an init system, such as WSL.

(defun syncthing-start ()
        "Start a syncthing process in the background."
        (interactive)
        (start-process "syncthing" "*syncthing*" "syncthing" "-no-browser"))

(defun syncthing-stop ()
        "Stop the syncthing background process if it exists."
        (interactive)
        (kill-process "syncthing"))

(use-package! auth-source
  :config
  (let ((credential (auth-source-user-and-password "wakatime")))
        (setq wakatime-api-key (cadr credential)))
  (let ((credential (auth-source-user-and-password "api.github.com" "Smona^grip")))
        (setq grip-github-user (car credential)
              grip-github-password (cadr credential))))

(use-package! wakatime-mode
  :after auth-source
  :config
  (global-wakatime-mode))

(setq +treemacs-git-mode 'deferred)

(after! treemacs
  (setq doom-themes-treemacs-theme "doom-colors")
  (treemacs-set-scope-type 'Perspectives)
  (treemacs-follow-mode t)
  (setq treemacs-width-is-initially-locked nil))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

;; Disable web-mode's default prettify symbols since they conflict with native ligatures
(after! web-mode
        (setq web-mode-prettify-symbols-alist '()))

(use-package! dirvish
  :config
  (dirvish-override-dired-mode))

;; Use language major mode, syntax highlihting, and configuration in markdown code blocks.
(use-package! polymode)
(use-package! poly-markdown)


(use-package! protobuf-mode)

;; Emacs 29 configuration

;; Hide unimportant warnings from doom incompatibility with emacs 29
(setq warning-minimum-level :error)

(add-to-list 'default-frame-alist '(alpha-background . 95))
(pixel-scroll-precision-mode)

(use-package! yuck-mode)
