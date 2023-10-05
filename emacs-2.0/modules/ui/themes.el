(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package ef-themes
  :config
  (setq ef-themes-headings ; read the manual's entry or the doc string
      '((0 . (variable-pitch light 1.9))
        (1 . (variable-pitch light 1.8))
        (2 . (variable-pitch regular 1.7))
        (3 . (variable-pitch regular 1.6))
        (4 . (variable-pitch regular 1.5))
        (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
        (6 . (variable-pitch 1.3))
        (7 . (variable-pitch 1.2))
        (t . (variable-pitch 1.1))))
  (setq ef-themes-to-toggle '(ef-summer ef-cherie)
        ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t))

(use-package modus-themes)
