;; `abn/listify'
(def-test! abn/listify
  (should (equal (abn/listify 'a) '(a)))
  (should (equal (abn/listify '(a)) '(a))))


;; `abn//resolve-hook-forms'
(def-test! abn//resolve-hook-forms
  (should (equal (abn//resolve-hook-forms '(js2-mode haskell-mode))
                 '(js2-mode-hook haskell-mode-hook)))
  (should (equal (abn//resolve-hook-forms '(quote (js2-mode-hook haskell-mode-hook)))
                 '(js2-mode-hook haskell-mode-hook))))

;; `add-hook!'
(def-test! add-one-to-one-hook
  (let (hooks)
    (add-hook! 'hooks 'a-hook)
    (should (equal hooks '(a-hook)))))

(def-test! add-many-to-one-hook
  (let (hooks)
    (add-hook! 'hooks '(hook-a hook-b hook-c))
    (should (equal hooks '(hook-c hook-b hook-a)))))

(def-test! add-one-to-many-hooks
  (let (hooks-a hooks-b hooks-c)
    (add-hook! '(hooks-a hooks-b hooks-c) 'a-hook)
    (should (equal hooks-a '(a-hook)))
    (should (equal hooks-b '(a-hook)))
    (should (equal hooks-c '(a-hook)))))

(def-test! add-many-to-many-hooks
  (let (hooks-a hooks-b hooks-c)
    (add-hook! '(hooks-a hooks-b hooks-c) '(hook-a hook-b hook-c))
    (should (equal hooks-a '(hook-c hook-b hook-a)))
    (should (equal hooks-b '(hook-c hook-b hook-a)))
    (should (equal hooks-c '(hook-c hook-b hook-a)))))

(def-test! add-non-literal-hooks
  (let (some-mode-hook)
    (add-hook! some-mode 'a-hook)
    (should (equal some-mode-hook '(a-hook)))))
