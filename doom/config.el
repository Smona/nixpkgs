;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "doom-dashboard.el" doom-user-dir)
(load! "org-config.el" doom-user-dir)
(load! "../doom_vars.el")

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
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setq! doom-font (font-spec :family "MonoLisa Nerd Font" :size 12.0 :weight 'normal :width 'normal))
(setq! doom-variable-pitch-font (font-spec :family "Source Serif 4" :size 20))
(setq! doom-unicode-font doom-font)
;; Use native unicode emoji rendering
(setq! emojify-display-style 'unicode)
;; Override all emoji unicode block to Noto Color Emoji
(after! unicode-fonts
  (dolist (unicode-block '("Emoticons"
                           "Dingbats"
                           "Enclosed Alphanumeric Supplement"
                           "Miscellaneous Technical"
                           "Enclosed Ideographic Supplement"
                           "Geometric Shapes Extended"
                           "Arrows"
                           "Geometric Shapes"
                           "Miscellaneous Symbols"
                           "Miscellaneous Symbols and Arrows"
                           "Miscellaneous Symbols and Pictographs"
                           "Supplemental Symbols and Pictographs"
                           "Transport and Map Symbols"))
    (push "Noto Color Emoji" (cadr (assoc unicode-block unicode-fonts-block-font-mapping))))
  (push '("Symbols and Pictographs Extended-A" ("Noto Color Emoji")) unicode-fonts-block-font-mapping)
  )

;; Give lines more space, while keeping text centered.
;; FIXME: disabled because it now makes the git gutter dashed.
;; More compact text doesn't look too bad tho!
;; Source: https://github.com/syl20bnr/spacemacs/issues/10502#issuecomment-404453194
;; (setq-default default-text-properties '(line-spacing 0.05 line-height 1.05))
;; No extra line spacing in vterm, where it can make text taller than powerline symbols & break continuity.
;; (add-hook! 'vterm-mode-hook (setq-local default-text-properties '(line-spacing 0 line-height 1)))
;; (add-hook! 'org-mode-hook (setq-local default-text-properties '(line-spacing 0 line-height 1)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
;; Other themes I like: doom-rouge, doom-challenger-deep, doom-monokai-spectrum, doom-monokai-octagon, doom-city-lights, doom-material, doom-palenight, doom-tokyo-night, doom-dracula
(load-theme 'catppuccin :no-confirm)
(setq doom-theme 'catppuccin)
;; Use fixed-pitch font in treemacs
(setq doom-themes-treemacs-enable-variable-pitch nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Make doom/goto-private-config-file et al point to the source rather than the
;; built nix store object
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

(map! :localleader
      :desc "Run NPM script"
      "n r" #'npm-mode-npm-run)

;; MX Master 3 forward/back buttons
(map! "<mouse-8>" #'previous-buffer)
(map! "<mouse-9>" #'next-buffer)
;; forward/back touchpad swipes with the gesture-improvements gnome extension
;; https://extensions.gnome.org/extension/4245/gesture-improvements/
(map! "<269025062>" #'previous-buffer)
(map! "<Back>" #'previous-buffer)
(map! "<269025063>" #'next-buffer)
(map! "<Forward>" #'next-buffer)

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

;; Always use indentation of 2 for JSON
(add-hook! 'json-mode-hook (setq-local js-indent-level 2))

;; (after! magit
;; magit-delta is disabled until they fix the performance on large diffs:
;; https://github.com/dandavison/magit-delta/issues/9
;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;; Performance optimizations
;; https://magit.vc/manual/magit/Performance.html
;; Disabled since magit-delta was causing most of the performance issues
;; (setq magit-refresh-status-buffer nil))

;; Don't require saving a file to update the git gutters.
;; NOTE: keep an eye on the performance of this convenience feature.
(after! git-gutter
  (setq git-gutter:update-interval 0.3))

;; Various editor enhancements

;; LSP config

(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.node_modules\\'")
  (setq lsp-clients-typescript-prefer-use-project-ts-server t))

;; Format with prettier rather than ts-ls, for example
(setq +format-with-lsp nil)

;; The following functions make it simpler to run syncthing in the background on systems that don't
;; have an init system, such as WSL.

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
  (setq auth-source-do-cache nil))

(defun load-password (host user)
  "Loads a password from auth-source"
  (let ((found (car (auth-source-search :host host :user user :require '(:secret)))))
    (funcall (plist-get found :secret))))

(after! grip-mode
  (setq grip-github-user "Smona^grip")
  (setq grip-github-password (load-password "api.github.com" "Smona^grip")))


(defun my/wakatime-hook () (setq wakatime-api-key (load-password "wakatime" "smona")))
(use-package! wakatime-mode
  :after (auth-source epa)
  :config
  (global-wakatime-mode)
  ;; This seems to reliably decrypt and load the secret without crashing startup 😵
  ;; If you see an error with startup hooks saying "void-function nil", it's probably this
  ;; hook running before necessary stuff has initialized to decrypt the authinfo file.
  (add-hook! 'doom-first-buffer-hook 'my/wakatime-hook))

(use-package! prettier
  :config
  (global-prettier-mode)
  (setq prettier-enabled-parsers '(angular babel babel-flow babel-ts css elm graphql html json json5 json-stringify less markdown mdx postgresql scss svelte typescript vue xml yaml)))

(use-package! blacken
  :config
  (add-hook! 'python-mode-hook 'blacken-mode))

(setq +treemacs-git-mode 'deferred)

(after! treemacs
  (treemacs-set-scope-type 'Perspectives)
  (treemacs-follow-mode t)
  (setq treemacs-width-is-initially-locked nil))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

;; Disable modes' default prettify symbols since they conflict with native ligatures
(after! web-mode
  (setq web-mode-prettify-symbols-alist '()))
(after! rjsx-mode
  (setq js--prettify-symbols-alist '()))

(use-package! dirvish
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-hide-details t)
  (setq dirvish-open-with-programs
        ;; Audio
'((("ape" "stm" "s3m" "ra" "rm" "ram" "wma" "wax" "m3u" "med" "669" "mtm" "m15" "uni" "ult" "mka" "flac" "axa" "kar" "midi" "mid" "s1m" "smp" "smp3" "rip" "multitrack" "ecelp9600" "ecelp7470" "ecelp4800" "vbk" "pya" "lvp" "plj" "dtshd" "dts" "mlp" "eol" "uvva" "uva" "koz" "xhe" "loas" "sofa" "smv" "qcp" "psid" "sid" "spx" "opus" "ogg" "oga" "mp1" "mpga" "m4a" "mxmf" "mhas" "l16" "lbc" "evw" "enw" "evb" "evc" "dls" "omg" "aa3" "at3" "atx" "aal" "acn" "awb" "amr" "ac3" "ass" "aac" "adts" "726" "abs" "aif" "aifc" "aiff" "au" "mp2" "mp3" "mp2a" "mpa" "mpa2" "mpega" "snd" "vox" "wav")
  "vlc" "%f")
        ;; Video
 (("f4v" "rmvb" "wvx" "wmx" "wmv" "wm" "asx" "mk3d" "mkv" "fxm" "flv" "axv" "webm" "viv" "yt" "s1q" "smo" "smov" "ssw" "sswf" "s14" "s11" "smpg" "smk" "bk2" "bik" "nim" "pyv" "m4u" "mxu" "fvt" "dvb" "uvvv" "uvv" "uvvs" "uvs" "uvvp" "uvp" "uvvu" "uvu" "uvvm" "uvm" "uvvh" "uvh" "ogv" "m2v" "m1v" "m4v" "mpg4" "mp4" "mjp2" "mj2" "m4s" "3gpp2" "3g2" "3gpp" "3gp" "avi" "mov" "movie" "mpe" "mpeg" "mpegv" "mpg" "mpv" "qt" "vbs")
  "vlc" "%f"))))

;; Use language major mode, syntax highlihting, and configuration in markdown code blocks.
(use-package! polymode)
(use-package! poly-markdown)


;; Language support

(use-package! protobuf-mode)
(use-package! systemd)
(use-package! jinja2-mode)

;; Emacs 29 configuration

;; Hide unimportant warnings from doom incompatibility with emacs 29
(setq warning-minimum-level :error)

(add-to-list 'default-frame-alist '(alpha-background . 95))
(pixel-scroll-precision-mode)

(use-package! yuck-mode)

;; Custom default file templates
;; (set-file-template! "\\.tsx$" :trigger "__.tsx" :mode 'tsx-mode)

;; ROS support
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))

;; Typescript esnext support
(add-to-list 'auto-mode-alist '("\\.mts$" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.cts$" . typescript-mode))

(use-package! mermaid-mode)

;; Add support for viewing webp and other "exotic" image formats
(setq image-use-external-converter t)

;; Prevent vterm from messing with evil's mode-based cursor styling
(advice-add #'vterm--redraw :around (lambda (fun &rest args) (let ((cursor-type cursor-type)) (apply fun args))))
;; Fix https://github.com/emacs-evil/evil-collection/issues/651
(defadvice! +vterm-update-cursor (orig-fn &rest args) :before #'vterm-send-key (vterm-goto-char (point)))
(defadvice! +vterm-update-cursor-boon (orig-fn &rest args) :before #'boon-insert (vterm-goto-char (point)))

;; Get clippy suggestions from rust LSP
(setq lsp-rust-analyzer-cargo-watch-command "clippy")

;; Nix support
(after! nix-mode
        (add-hook! 'before-save-hook 'nix-format-before-save))

;; Tailwind CSS support
(use-package! lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package! jtsx
   :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  :config
    (map! :map jtsx-jsx-mode-map
        :localleader
        :desc "Jump to matching JSX tag"
        "%" #'jtsx-jump-jsx-element-tag-dwim)
    (map! :map jtsx-tsx-mode-map
        :localleader
        :desc "Jump to matching JSX tag"
        "%" #'jtsx-jump-jsx-element-tag-dwim)

  (add-hook 'jtsx-jsx-mode-hook 'lsp)
  (add-hook 'jtsx-tsx-mode-hook 'lsp)
  (add-hook 'jtsx-typescript-mode-hook 'lsp)
  (add-hook 'jtsx-jsx-mode-hook 'prettier-mode)
  (add-hook 'jtsx-tsx-mode-hook 'prettier-mode)
  (add-hook 'jtsx-typescript-mode-hook 'prettier-mode)

  ;; Integrate JTSX smart comment function with evil-nerd-commenter
  (defun use-jtsx-comment (beg end)
    (set-mark beg)(goto-char end)(activate-mark)(jtsx-comment-dwim nil))
  (defun my-comment-or-uncomment-region (beg end)
    (if (equal major-mode #'jtsx-tsx-mode)
        (use-jtsx-comment beg end)
      (evilnc-comment-or-uncomment-region-internal beg end)))
  (setq evilnc-comment-or-uncomment-region-function
        'my-comment-or-uncomment-region)

  ;; TODO: fix syntax highlighting in "show documentation" LSP help window for tsx? files.
  )

(after! rustic
  (setq rustic-format-on-save t))

(use-package! s3ed
        :config
        (map! :leader
                :desc "Find file in S3"
                "f a" #'s3ed-find-file)
        (s3ed-mode))

;; Safe local variables
(put 'python-pytest-executable 'safe-local-variable #'stringp)

;; This is needed to make bins installed from nixpkgs accessible to emacs when launched from the dock/spotlight on macos,
;; even with emacs-homebrew-plus's path hacks. It should also make doom/env less necessary on Linux, although that isn't
;; tested at time of writing.
(use-package! exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Blink the cursor while it's resting
(blink-cursor-mode)
(setq blink-cursor-delay 1.5)
(setq blink-cursor-interval 1)
(setq blink-cursor-blinks 0)

;; Enable ghost text for editing text inputs in chrome with emacs!
(use-package! atomic-chrome
  :init (atomic-chrome-start-server))

;; Undo doom hardcoding nerd fonts icons to "Symbols Nerd Font Mono"
;; in doom-ui.el for powerline symbols so they line up.
(add-hook! 'after-setting-font-hook (dolist (character '(#xe0b6 #xe0b4))
        (set-fontset-font t character "MonoLisa Nerd Font")))
