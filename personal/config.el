;;; config --- custom config file

(setq default-frame-alist '((font . "Operator Mono-14")))

(setq standard-indent 2
      css-indent-offset 2
      js-indent-level 2
      nginx-indent-level 2
      typescript-indent-level 2)

(prelude-require-package 'js2-mode)
(require 'js2-mode)
(setq js2-basic-offset 2
      js2-strict-missing-semi-warning nil
      js2-strict-trailing-comma-warning nil
      js2-strict-inconsistent-return-warning nil
      js2-highlight-undeclared-vars t
      js2-bounce-indent-p t)

;; better scrolling
(prelude-require-package 'smooth-scroll)
(require 'smooth-scroll)
(smooth-scroll-mode t)
(setq smooth-scroll/hscroll-step-size 12)
(setq smooth-scroll/vscroll-step-size 12)

(prelude-require-package 'smooth-scrolling)
(require 'smooth-scrolling)
(smooth-scrolling-mode t)
(setq recenter-positions '(middle top))
(setq next-screen-context-lines 10)

(prelude-require-package 'flycheck)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

;; http://emacs.stackexchange.com/a/21207
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(prelude-require-package 'web-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.eex\\'" . web-mode))
(setq web-mode-attr-indent-offset 2
      web-mode-attr-value-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-attr-indent-offset 2)

(setq line-spacing 1)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(prelude-require-package 'company-tern)
(require 'company-tern)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-tern))
(setq company-dabbrev-downcase nil)

(prelude-require-package 'prettier-js)
(require 'prettier-js)

(exec-path-from-shell-initialize)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))


(defun web-mode-hooks ()
  (cond
   ((string-match "[jt]sx" (file-name-extension buffer-file-name))
    (progn
      (web-mode-set-content-type "jsx")
      (tern-mode)
      (setup-tide-mode)
      (company-mode)
      (smartparens-mode)
      (prettier-js-mode)))

   ((string= (file-name-extension buffer-file-name) "eex")
    (web-mode-set-engine "elixir"))))

(add-hook 'web-mode-hook 'web-mode-hooks)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq company-tooltip-align-annotations t)

(prelude-require-package 'wakatime-mode)
(global-wakatime-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(setq exec-path (append exec-path '("~/.nvm/versions/node/v12.16.1/bin/")))

(setq reb-re-syntax 'string)
