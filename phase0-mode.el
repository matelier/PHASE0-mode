;;PHASE0 MODE

;; Copyright (C) 2015, 2016, 2017, ASMS Co., Ltd.
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
  (regexp-opt `("on" "off" "automatic" "initial" "continuation" "fixed_charge"
		"file" "monk" "mesh" "tetrahedral" "parabolic"
		"atomic_charge_density" "atomic_orbitals"
		"dft-d2" "pbesol" "vdwdf2" "pawpot"
		"facecentered" "bodycentered" "basecentered" "hexagonal" "rhombohedral"
		"internal" "cartesian"
		"bravais" "primitive"
		"ferro" "antiferro" "para" "collinear" "noncollinear" "initially" "whole"
		"restricted" "unrestricted" "disregard"
		"rmm3" "davidson" "mddavidson" "pdavidson" "mdkosugi" "pkosugi"
		"pulay" "broyden2" "simple"
		"quench" "cg" "gdiis" "bfgs" "velocity_verlet"
		"hse06" "cube" "individual" "effective_charge" "piezoelectric_const"
		"by_atomic_positions" "regular_intervals"
		"ks" "single" "powder" "proportional" "directin"
		)))

(defvar phase0-mode-font-lock-keywords
  `(
    ("sw_[a-z_]+" . font-lock-variable-name-face)
    ("^[ \t]*#\\(tag *\\(no\\)?\\|units\\|default\\)"      . font-lock-string-face)
    ;; Number (literal)
    ("[ \t=][-+]?[0-9\.]+\\([deDE][-+]?[0-9]+\\)?" . font-lock-constant-face)
    ;; 1st Level
    ("control\\|accuracy\\|structure\\(_evolution\\)?\\|wavefunction_solver\\|charge_mixing\\|postprocessing\\|phonon\\|print\\(out\\)?level" . font-lock-function-name-face)
    ("berry_phase\\|epsilon\\|multiple_replica[ \t]*{" . font-lock-function-name-face)
    ;; 2nd Level
    ("ksampling\\|smearing\\|\\(scf\\|ek\\|force\\)_convergence"         . font-lock-type-face)
    ("esm\\|\\bhybrid_functional\\|spinorbit"                            . font-lock-type-face)
    ("hubbard\\|projector_list"                                          . font-lock-type-face)
    ("\\(unit_cell\\|symmetry\\|atom_list\\|element_list\\|strain\\)[ \t]*{" . font-lock-type-face)
    ("ferromagnetic_state"                                               . font-lock-type-face)
    ("\\(solvers\\|line_minimization\\|[^_]stress\\|[pl \t]dos\\)[ \t]*{"  . font-lock-type-face)
    ("[^_]charge[ \t]*{"                                                 . font-lock-type-face)
    ("rmm[ \t]*{\\|mixing_methods"                                       . font-lock-type-face)
    ("lattice[ \t]*{\\|polarization\\|workfunc\\|wannier\\|raman"        . font-lock-type-face)
    ("photon\\|transition_moment\\|BZ_integration\\|band_gap_correction" . font-lock-type-face)
    ("replicas[ \t]*{"                                         . font-lock-type-face)
    ("atom_list_end[01]"                                       . font-lock-type-face)
    ("corelevels"                                       . font-lock-type-face)
    ;; 3rd Level
    ("mesh[ \t]*{\\|kshift\\|projectors\\|tspace\\|atoms"      . font-lock-reference-face)
    ("partial_charge[ \t]*{\\|layerdos"                        . font-lock-reference-face)
    ("polar\\|poynting\\|energy[ \t]*{"                        . font-lock-reference-face)
    ("spectrum\\|propagation\\|laser"                          . font-lock-reference-face)
    ("external_stress"                                         . font-lock-reference-face)
    ;; 4th Level
    ("incident\\|scattered"     . font-lock-variable-name-face)
    ;; Variables
    ("initial_\\(wavefunctions\\|charge_density\\|occmat\\)"     . font-lock-variable-name-face)
    ("PAW[ \t]*=\\|cutoff_\\(cd\\|wf\\)\\|num_bands\\|num_extra_bands"  . font-lock-variable-name-face)
    ("unit_cell_type\\|lattice_system"                               . font-lock-variable-name-face)
    ("coordinate_system"                                             . font-lock-variable-name-face)
    ("functional_type\\|vdw_method\\|xctype"                         . font-lock-variable-name-face)
    ("base_\\(reduction\\|symmetrization\\)_for_GAMMA"               . font-lock-variable-name-face)
    ("magnetic_state\\|spin_fix_period\\|total_spin\\|[ \t]mode"     . font-lock-variable-name-face)
    ("condition\\|max_\\(scf_\\)?iteration\\|cpumax\\|\\bmethod"     . font-lock-variable-name-face)
    ("delta_\\(total_energy\\|eigenvalue\\)"                         . font-lock-variable-name-face)
    ("max_force\\|max_stress"                                        . font-lock-variable-name-face)
    ("edelta_change_to_rmm"                                          . font-lock-variable-name-face)
    ("displacement"                                                  . font-lock-variable-name-face)
    ("dt_\\(upp\\|low\\)er_critical"                                 . font-lock-variable-name-face)
    ("\\bn[xyz]\\b\\|\\bk[123]\\b\\|\\bk[xyz]\\b\\|\\bp[xyz]\\b"     . font-lock-variable-name-face)
    ("temperature\\|freq_pitch\\|hwhm\\|wavelength"                  . font-lock-variable-name-face)
    ("low\\|high\\|step"                                             . font-lock-variable-name-face)
    ("\\b[a-c]_vector\\b"                                            . font-lock-variable-name-face)
    ("\\b[a-c][ \t]*=\\|alpha\\|beta\\|gamma\\|omega"                . font-lock-variable-name-face)
    ("partial_charge_filetype\\|filetype\\|title"                    . font-lock-variable-name-face)
    ("erange_\\(min\\|max\\|delta\\)"                                . font-lock-variable-name-face)
    ("photon\\|crystal_type\\|scissor_operator\\|symmetry"           . font-lock-variable-name-face)
    ("displaced_atom\\|\\bu[xyz]\\b\\|g_index\\|property"            . font-lock-variable-name-face)
    ("electronic_dielectric_constant"                                . font-lock-variable-name-face)
    ("electric_field"                                                . font-lock-variable-name-face)
    ("nb_wan90\\|seedname\\|spin_component_wan90"                    . font-lock-variable-name-face)
    ("normal_axis\\|slicing_way\\|crtdst"                            . font-lock-variable-name-face)
    ("mobile\\|\\br[x-z]\\b\\|\\bvdw\\b"                             . font-lock-variable-name-face)
    ("element\\|atomicnumber\\|[ \t]mass\\|zeta\\|\\bm[x-z]\\b"      . font-lock-variable-name-face)
    ("\\bsol\\|till_n\\|rmxs"                                        . font-lock-variable-name-face)
    ("s\\(11\\|22\\|33\\)"                                           . font-lock-variable-name-face)
    ("multiple_replica_mode\\|howtogive_coordinates\\|number_of_replicas" . font-lock-variable-name-face)
    ("replica_number\\|from_endpoint_images" . font-lock-variable-name-face)
    ("ueff\\|group\\|radius\\|proj_group\\|\\bl\\b" . font-lock-variable-name-face)
    ;; Keywords
;;    ("\\bo\\(n\\|ff\\)\\b"                          . font-lock-keyword-face) ;; on / off
    ;; Units
    ("rydberg\\|hartree\\|angstrom\\|bohr\\|GPa\\|eV\\|atomic_mass\\|min\\|hour\\|day\\|[ \t]sec\\|[ \t]fs" . font-lock-builtin-face) ;; . font-lock-warning-face)
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
