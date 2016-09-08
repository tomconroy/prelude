(setq default-frame-alist '((font . "Operator Mono-13")))

(setq standard-indent 2
      css-indent-offset 2
      js-indent-level 2
      nginx-indent-level 2)

(require 'js2-mode)
(setq js2-basic-offset 2
      js2-strict-missing-semi-warning nil
      js2-strict-trailing-comma-warning nil
      js2-strict-inconsistent-return-warning nil
      js2-highlight-undeclared-vars t
      js2-bounce-indent-p t)

;; better scrolling
(require 'smooth-scroll)
(smooth-scroll-mode t)
(setq smooth-scroll/hscroll-step-size 12)
(setq smooth-scroll/vscroll-step-size 12)

(require 'smooth-scrolling)
(smooth-scrolling-mode t)
(setq recenter-positions '(middle top))
(setq next-screen-context-lines 10)

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


(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(setq web-mode-attr-indent-offset 2
      web-mode-attr-value-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-attr-indent-offset 2)

(add-hook 'web-mode-hook
          (lambda ()
            (web-mode-set-content-type "jsx")))

(setq line-spacing 1)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(require 'company-tern)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-tern))
(setq company-dabbrev-downcase nil)
