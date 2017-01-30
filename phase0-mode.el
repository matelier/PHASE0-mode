;;PHASE0 MODE

;; Copyright (C) 2015, 2016, ASMS Co., Ltd.
;; This file cannot be further distributed either in the original or in a modified form.
;; Users are free to modify the code solely for their personal use
;; and are encouraged to share their improvements with the authors at (info@asms.co.jp).

;;(add-to-list `interpreter-mode-alist `("phase0" . phase0-mode))
;;(add-to-list 'auto-mode-alist '("phase0", phase0-mode))

(defvar phase0-mode-hook nil)

(defvar phase0-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for phase0 major mode")

(defvar phase0-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?!  "<" st)  ; comment start
    (modify-syntax-entry ?\n ">" st)  ; comment end
;    (modify-syntax-entry ?\{ "(}" st)  ;
;    (modify-syntax-entry ?\} ")}" st)  ;
   st)
  "Syntax table for phase0 mode")

(defconst phase0-mode-keywords-regexp
  (regexp-opt `("automatic" "initial" "continuation" "fixed_charge"
		"file" "monk" "mesh" "tetrahedral" "parabolic"
		"atomic_charge_density" "atomic_orbitals"
		"dft-d2" "pawpot"
		"facecentered" "bodycentered" "basecentered" "hexagonal" "rhombohedral"
		"internal" "cartesian"
		"bravais" "primitive" "ferro" "antiferro" "para" "noncollinear" "initially"
		"rmm3" "davidson" "mddavidson" "pdavidson" "mdkosugi" "pkosugi"
		"pulay" "broyden2" "simple"
		"quench" "cg" "gdiis" "bfgs" "velocity_verlet"
		"hse06" "cube" "individual" "effective_charge" "piezoelectric_const" )))

(defvar phase0-mode-font-lock-keywords
  `(
    ("sw_[a-z_]+" . font-lock-variable-name-face)
    ("^[ \t]*#\\(tag\\|units\\|default\\).*"      . font-lock-string-face)
    ;; Number (literal)
    ("[ \t=][-+]?[0-9\.]+\\([deDE][-+]?[0-9]+\\)?" . font-lock-constant-face)
    ;; 1st Level
    ("control\\|accuracy\\|structure\\(_evolution\\)?\\|wavefunction_solver\\|charge_mixing\\|postprocessing\\|phonon\\|print\\(out\\)?level" . font-lock-function-name-face)
    ("berry_phase\\|epsilon" . font-lock-function-name-face)
    ;; 2nd Level
    ("ksampling\\|smearing\\|\\(scf\\|ek\\|force\\)_convergence"         . font-lock-type-face)
    ("esm\\|\\bhybrid_functional"                                        . font-lock-type-face)
    ("hubbard\\|projector_list"                                          . font-lock-type-face)
    ("unit_cell[ \t]*{\\|symmetry\\|atom_list\\|element_list\\|strain"   . font-lock-type-face)
    ("ferromagnetic_state"                                               . font-lock-type-face)
    ("solvers\\|line_minimization\\|stress[ \t]*{\\|[pl]?dos[ \t]*{"     . font-lock-type-face)
    ("[^_]charge[ \t]*{"                                                 . font-lock-type-face)
    ("rmm[ \t]*{\\|mixing_methods"                                       . font-lock-type-face)
    ("lattice[ \t]*{\\|polarization\\|workfunc\\|wannier"                . font-lock-type-face)
    ;; 3rd Level
    ("mesh[ \t]*{\\|kshift\\|projectors\\|tspace\\|atoms"      . font-lock-reference-face)
    ;; Variables
    ("initial_\\(wavefunctions\\|charge_density\\|occmat\\)"     . font-lock-variable-name-face)
    ("PAW[ \t]*=\\|cutoff_\\(cd\\|wf\\)\\|num_bands\\|num_extra_bands"  . font-lock-variable-name-face)
    ("unit_cell_type\\|lattice_system"                           . font-lock-variable-name-face)
    ("coordinate_system"                                         . font-lock-variable-name-face)
    ("functional_type\\|vdw_method"                              . font-lock-variable-name-face)
    ("magnetic_state\\|spin_fix_period\\|total_spin\\|spinorbit" . font-lock-variable-name-face)
    ("condition\\|max_iteration\\|cpumax\\|\\bmethod"            . font-lock-variable-name-face)
    ("delta_\\(total_energy\\|eigenvalue\\)"                     . font-lock-variable-name-face)
    ("max_force"                                                 . font-lock-variable-name-face)
    ("edelta_change_to_rmm"                                      . font-lock-variable-name-face)
    ("displacement"                                              . font-lock-variable-name-face)
    ("dt_\\(upp\\|low\\)er_critical"                             . font-lock-variable-name-face)
    ("\\bn[xyz]\\b\\|\\bk[123]\\b"                               . font-lock-variable-name-face)
    ("\\b[abc]_vector\\b"                                        . font-lock-variable-name-face)
    ("\\b[abc]\\b\\|alpha\\|beta\\|gamma\\|omega"                . font-lock-variable-name-face)
    ("filetype\\|title\\|partial_charge"                         . font-lock-variable-name-face)
    ("erange_\\(min\\|max\\|delta\\)"                            . font-lock-variable-name-face)
    ("displaced_atom\\|\\bu[xyz]\\b\\|g_index\\|property"        . font-lock-variable-name-face)
    ("BZ_integration\\|transition_moment\\|photon\\|crystal_type". font-lock-variable-name-face)
    ("electric_field\\|nb_wan90\\|seedname"                      . font-lock-variable-name-face)
    ;; Keywords
    ("\\bo\\(n\\|ff\\)\\b"                          . font-lock-keyword-face) ;; on / off
    ;; Units
    ("rydberg\\|hartree\\|angstrom\\|bohr\\|GPa\\|eV\\|hour\\|day\\|[ \t]sec\\|[ \t]fs" . font-lock-builtin-face) ;; . font-lock-warning-face)
    , phase0-mode-keywords-regexp
      ))

; Emacs <= 23 (https://github.com/flycheck/flycheck/issues/150)
(unless (fboundp `setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var), val)))

(if (<= emacs-major-version 23)
    (defun phase0-mode ()
      (interactive)
      (setq major-mode 'phase0-mode
	    mode-name "PHASE0")
      (set-syntax-table phase0-mode-syntax-table)
      (setq-local font-lock-defaults
		  `(phase0-mode-font-lock-keywords nil t))
      (run-hooks 'phase0-mode-hook)
      )
  (define-derived-mode phase0-mode prog-mode "PHASE0"
    "Major mode for editing PHASE0 input file."
    (set-syntax-table phase0-mode-syntax-table)
    (setq-local font-lock-defaults
		`(phase0-mode-font-lock-keywords nil t))
    (run-hooks 'phase0-mode-hook)
    ;(show-paren-mode t)
    (setq tab-always-indent t) ; experimental
    ))

;;OUTLINE MODE
;;\C-c\C-o\C-t  hide-body
;;\C-c\C-o\C-a  show-all
;;\C-c\C-o\C-d  hide-subtree
;;\C-c\C-o\C-s  show-subtree
(add-hook 'phase0-mode-hook
	  'outline-minor-mode)
(add-hook 'phase0-mode-hook
	  '(lambda ()
	     (make-local-variable 'outline-regexp)
	     (setq outline-regexp "[a-zA-Z_/]")))
(setq outline-minor-mode-prefix "\C-c\C-o")

;;;ALIGN (experimental)
(require 'align)
(add-to-list 'align-rules-list
	     '(period-assignment
;	       (regexp . ".\\( *\\)")
	       (regexp  . "\\(^\\|\\S-\\)\\([ \t]+\\)")
	       (group  . 2)
	       (repeat . t)
	       (modes  . '(phase0-mode)))
	     )
;
(provide `phase0-mode)
