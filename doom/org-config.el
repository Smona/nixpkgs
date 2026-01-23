;;; org.el -*- lexical-binding: t; -*-
;;
;; Configuration for org-mode

;; Set file paths
(setq org-directory "~/org/")
(setq org-roam-directory (file-truename "~/org"))
(after! org-agenda
        (setq org-agenda-files '("~/org" "~/org/daily")))

;; Hotkeys
(map! :map org-mode-map
      :localleader
      :desc "Open Org Roam UI"
      "m u" #'org-roam-ui-open)
(map! :map org-mode-map
      :localleader
      :desc "Show property drawers"
      "m p" #'org-tidy-untidy-buffer)

;; The below was borrowed from https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;; Disabled for now b/c catppuccin theme handles header sizes nicely
;; (defun my/org-mode-hook ()
;;   (set-face-attribute 'org-level-4 nil :height 1.1)
;;   (set-face-attribute 'org-level-3 nil :height 1.2)
;;   (set-face-attribute 'org-level-2 nil :height 1.3)
;;   (set-face-attribute 'org-level-1 nil :height 1.4)
;;   (set-face-attribute 'org-document-title nil :height 2.0 :underline nil))
;; (add-hook 'org-load-hook #'my/org-mode-hook)

(custom-theme-set-faces
        'user
        '(org-block ((t (:inherit fixed-pitch))))
        '(org-code ((t (:inherit (shadow fixed-pitch)))))
        '(org-document-info ((t (:foreground "dark orange"))))
        '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
        '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
        '(org-link ((t (:underline t))))
        '(org-visual-indent-blank-pipe-face ((t (:background "#1e1e2e"))))
        '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
        '(org-property-value ((t (:inherit fixed-pitch))) t)
        '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
        '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
        '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; track habits
(add-to-list 'org-modules 'org-habit)

;; (defun my/org-local-hook ()
  ;; (setq-local default-text-properties '(line-spacing 0.2 line-height 1.2))
   ;; Ensure saved code block results are always up to date, and speed up
   ;; iteration, without tempting going a while without saving 😉
   ;; The last argument makes it buffer-local.
   ;; DISABLED because this does _not_ work well with jupyter-style notebooks
   ;; (add-hook 'before-save-hook 'org-babel-execute-buffer nil t))

 (use-package! valign
     :after org
     :config
     (setq valign-fancy-bar t)
     (add-hook 'org-mode-hook #'valign-mode)
 )

(map! :after evil-org
      :map evil-org-mode-map
      :ni "C-<return>" #'org-insert-heading-respect-content)

(after! org
        ;; properly define stuck projects
        (setq org-stuck-projects
                '("+TODO=\"PROJ\"" ("TODO" "STRT") nil "") )
        ;; Use non-monospace font by default in org mode
        (add-hook 'org-mode-hook 'variable-pitch-mode)
        ;; (add-hook 'org-mode-hook 'my/org-local-hook)
        ;; TODO: make this a bit bigger
        (setq org-ellipsis "⊕")
        (setq org-hidden-keywords '(title))
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
        (setq org-hide-emphasis-markers t)

       (setq org-agenda-custom-commands
        '(
        ;; ("i" "Inbox" tags-todo "+TODO=\"TODO\""
        ;;         ((org-agenda-files (file-expand-wildcards "~/pim/gtd/inbox.org"))))
        ("n" "Next actions" tags-todo "+TODO=\"TODO\"|TODO=\"STRT\"")
        ("p" "Projects" tags-todo "+TODO=\"PROJ\"")
        ("w" "Waiting" tags-todo "+TODO=\"WAIT\"")
        ("i" "Ideas for Someday" tags-todo "+TODO=\"IDEA\"")
        ;; ("o" "Actions and Projects" tags-todo "+TODO=\"TODO\"|TODO=\"PROJ\"")
        ))

        (setq +org-capture-todo-file "inbox.org")
        (setq org-capture-templates '(
                ("t" "Personal todo" entry (file +org-capture-todo-file)
                "* TODO %?\n%i\n%a")
                ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox")
                "* %u %?\n%i\n%a" :prepend t)
                ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file)
                "* %U %?\n%i\n%a" :prepend t)
                ("p" "Templates for projects")
                ;; NOTE: using only centralized project TODOs currently, but here's doom's default:
                ;; ("pt" "Project-local todo" entry
                ;; (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?\n%i\n%a"
                ;; :prepend t)
                ;; ("pn" "Project-local notes" entry
                ;; (file+headline +org-capture-project-notes-file "Inbox") "* %U %?\n%i\n%a"
                ;; :prepend t)
                ;; ("pc" "Project-local changelog" entry
                ;; (file+headline +org-capture-project-changelog-file "Unreleased")
                ;; "* %U %?\n%i\n%a" :prepend t)
                ;; ("o" "Centralized templates for projects")
                ("pt" "Project todo" entry #'+org-capture-central-project-todo-file
                "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
                ("pn" "Project notes" entry #'+org-capture-central-project-notes-file
                "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
                ("pc" "Project changelog" entry #'+org-capture-central-project-changelog-file
                "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))))


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
        ;; Customizations to roam dailies capture templates
        (setq org-roam-dailies-capture-templates '(
                ("d" "default" entry "* %<%H:%M> %?"
                :if-new  (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>")
                )
        ))
        )

; org-roam-ui config

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

(setq-default prettify-symbols-alist '(("#+begin_src" . "")
                                       ("#+end_src" . " ")
                                       ("#+begin_src js" . "")
                                       ("#+begin_src bash" . "")
                                       ("#+begin_src python" . "")
                                       ("#+begin_src jupyter-python" . "󰺂")
                                       ("#+begin_src typescript" . "")
                                       (":exports" . "󰶭")
                                       ("#+RESULTS:" . "")
                                       (":LOGBOOK:" . "󱚈")
                                       ("#+begin_quote" . "")
                                       ("#+end_quote" . "")
                                       ("#+filetags:" . "󱈤")
                                       (" TODO" . "")
                                       (" IDEA" . "")
                                       (" STRT" . "")
                                       (" LOOP" . "")
                                       (" DONE" . "")
                                       (" KILL" . "")
                                       (" PROJ" . "")
                                       ("[ ]" . "")
                                       ("[x]" . "")
                                       ("[X]" . "")
                                       ("lambda" . 955)))
(add-hook 'org-mode-hook 'prettify-symbols-mode)
(use-package! org-superstar
  :config
        ;; Hide bullets for headings with TODO states in favor of the prettified icon
        (setq! org-superstar-special-todo-items 'hide)
        (setq! org-superstar-remove-leading-stars t)
        (add-hook 'org-mode-hook 'org-superstar-mode)
  )
(setq! org-startup-indented nil)
(setq! org-indent-mode-turns-on-hiding-stars nil)
(use-package! org-visual-indent
  :config
  (setq! org-visual-indent-color-indent '(
                                         ;; TODO: tie these to variables, either the same way catppuccin-theme does or referencing the relevant heading faces.
                                         (1 (:background "#f38ba8" :foreground "#f38ba8" :height .1))
                                         (2 (:background "#fab387" :foreground "#fab387" :height .1))
                                         (3 (:background "#f9e2af" :foreground "#f9e2af" :height .1))
                                         (4 (:background "#a6e3a1" :foreground "#a6e3a1" :height .1))
                                         (5 (:background "#74c7ec" :foreground "#74c7ec" :height .1))
                                         (6 (:background "#b4befe" :foreground "#b4befe" :height .1))
                                         (7 (:background "#cba6f7" :foreground "#cba6f7" :height .1))
                                         (8 (:background "#eba0ac" :foreground "#eba0ac" :height .1))
                                         ))
(add-hook 'org-mode-hook #'org-visual-indent-mode))

(use-package! org-tidy
  :config
  (setq! org-tidy-properties-style 'fringe)
  :hook
  (org-mode . org-tidy-mode))

;; Disable VC gutter in org files
(add-hook 'org-mode-hook (lambda () (diff-hl-mode 0)))
