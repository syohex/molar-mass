;;; molar-mass.el --- Calculates molar mass of a molecule
;; -*- coding: utf-8 -*-
;;
;; Copyright 2021 by Sergi Ruiz Trepat
;;
;; Author: Sergi Ruiz Trepat
;; Created: 2021
;; Version: 0.1
;; Keywords: chemistry
;; Homepage: https://github.com/sergiruiztrepat/molar-mass.el
;;
;; Molar-mass is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;;; Commentary:
;;
;; Use it with M-x molar-mass
;; 
;; It works interactively and also with region.  You can mark a region with
;; a formula and it will give you its molar mass
;; Example:

;;Mark region : H2O
;;Call M-x molar-mass
;;
;;=> Molar mass of H2O : 18.015 g/mol (uma)
;;
;;; Code:

(defconst molar-mass-elements-mass
      '(
	("H" 1.0079)
	("He" 4.0026)
	("Li" 6.941)
	("Be" 9.0122)
	("B" 10.811)
	("C" 12.0107)
	("N" 14.0067)
	("O" 15.9994)
	("F" 18.9984)
	("Ne" 20.1797)
	("Na" 22.9897)
	("Mg" 24.305)
	("Al" 26.9815)
	("Si" 28.0855)
	("P" 30.9738)
	("S" 32.065)
	("Cl" 35.453)
	("K" 39.0983)
	("Ar" 39.948)
	("Ca" 40.078)
	("Sc" 44.9559)
	("Ti" 47.867)
	("V" 50.9415)
	("Cr" 51.9961)
	("Mn" 54.938)
	("Fe" 55.845)
	("Ni" 58.6934)
	("Co" 58.9332)
	("Cu" 63.546)
	("Zn" 65.39)
	("Ga" 69.723)
	("Ge" 72.64)
	("As" 74.9216)
	("Se" 78.96)
	("Br" 79.904)
	("Kr" 83.8)
	("Rb" 85.4678)
	("Sr" 87.62)
	("Y" 88.9059)
	("Zr" 91.224)
	("Nb" 92.9064)
	("Mo" 95.94)
	("Tc" 98.00)
	("Ru" 101.07)
	("Rh" 102.9055)
	("Pd" 106.42)
	("Ag" 107.8682)
	("Cd" 112.411)
	("In" 114.818)
	("Sn" 118.71)
	("Sb" 121.76)
	("I" 126.9045)
	("Te" 127.6)
	("Xe" 131.293)
	("Cs" 132.9055)
	("Ba" 137.327)
	("La" 138.9055)
	("Ce" 140.116)
	("Pr" 140.9077)
	("Nd" 144.24)
	("Pm" 145.00)
	("Sm" 150.36)
	("Eu" 151.964)
	("Gd" 157.25)
	("Tb" 158.9253)
	("Dy" 162.5)
	("Ho" 164.9303)
	("Er" 167.259)
	("Tm" 168.9342)
	("Yb" 173.04)
	("Lu" 174.967)
	("Hf" 178.49)
	("Ta" 180.9479)
	("W" 183.84)
	("Re" 186.207)
	("Os" 190.23)
	("Ir" 192.217)
	("Pt" 195.078)
	("Au" 196.9665)
	("Hg" 200.59)
	("Tl" 204.3833)
	("Pb" 207.2)
	("Bi" 208.9804)
	("Po" 209.00)
	("At" 210.00)
	("Rn" 222.00)
	("Fr" 223.00)
	("Ra" 226.00)
	("Ac" 227.00)
	("Pa" 231.0359)
	("Th" 232.0381)
	("Np" 237.00)
	("U" 238.0289)
	("Am" 243.00)
	("Pu" 244.00)
	("Cm" 247.00)
	("Bk" 247.00)
	("Cf" 251.00)
	("Es" 252.00)
	("Fm" 257.00)
	("Md" 258.00)
	("No" 259.00)
	("Rf" 261.00)
	("Lr" 262.00)
	("Db" 262.00)
	("Bh" 264.00)
	("Sg" 266.00)
	("Mt" 268.00)
	("Hs" 277.00)))

(defun molar-mass ()
  "Calculates molar mass of a molecule."
  (interactive)
  (let* ((data (if (region-active-p)
		   (buffer-substring-no-properties
		    (region-beginning)
		    (region-end))
		 (read-string "Formula: ")))
	 (elements (mapcar 'char-to-string data))
	 (elements-aux '()))   ;; auxiliar list to clean blanks and
    ;; dashes in the next while
    
    (while elements
      (if (not (equal (car elements) (or " " "-")))
	  (push (car elements) elements-aux))
      (setq elements (cdr elements)))
    (setq elements (reverse elements-aux))
	 
    (print
     (format "Molar mass of %s : %.3f g/mol (uma)"   ;; 3 decimal digits
	     data
	     (molar-mass-total-mass (molar-mass-pairs-list elements))))))

(defun molar-mass-total-mass (elem)
  "ELEM is a processed list of pairs (atoms - atomic mass).
Returns the total mass of the molecule."
  (let (($total-mass 0))
    (while elem
      (setq $total-mass
	    (+ $total-mass
	       (* (car elem) (cadr elem))))
      (setq elem (cddr elem)))
    $total-mass))

(defun molar-mass-pairs-list (elem)
  "ELEM is a list of chars from the original string representing a molecule.
The function returns pairs of (atoms - elements)"
  (let (p1 ;;first element of a pair (symbol)
	p2 ;; second element of a pair (number)
	(pairs '()))
    (while (>= (length elem) 1)
      (cond
       ;; If paren in list, molar-mass-cut-list-in and call recursively.
       ((member "(" elem)
	(setq p2 (cadr (member ")" elem)))
	(setq p1 (molar-mass-total-mass
		  (molar-mass-pairs-list (molar-mass-cut-list-in elem "(" ")"))))
	(setq elem (molar-mass-cut-list-out elem "(" ")")))
	       
       ;; If first is upcase (element) and second is number
       ((and (molar-mass-upcase-p (car elem))
	     (molar-mass-number-p (cadr elem)))
	(setq p1 (cadr (assoc (car elem) molar-mass-elements-mass)))
	(if (not (molar-mass-number-p (caddr elem)))
	    (progn
	      (setq p2 (cadr elem))
	      (setq elem (cddr elem)))
	  ;; If there's two numbers
	  (progn
	    (setq p2 (concat (cadr elem) (caddr elem)))
	    (setq elem (cdddr elem)))))

       ;; If first is upcase and second downcase (one element)
       ((and (molar-mass-upcase-p (car elem))
	     (and (not (molar-mass-upcase-p (cadr elem)))
		  (not (molar-mass-number-p (cadr elem)))))
	(setq p1 (cadr (assoc (concat (car elem) (cadr elem)) molar-mass-elements-mass)))
	(if (molar-mass-number-p (caddr elem))
	    (progn
	      (setq p2 (caddr elem))
	      (setq elem (cdddr elem))
	      ;; If there're two numbers
	      (if (molar-mass-number-p (car elem))
		  (progn
		    (setq p2 (concat p2 (car elem)))
		    (setq elem (cdr elem)))))
	  (progn
	    (setq p2 "1")
	    (setq elem (cddr elem)))))

       ;; If there're two upcase letters (two elements)
       ((and (molar-mass-upcase-p (car elem))
	     (molar-mass-upcase-p (cadr elem)))
	(setq p1 (cadr (assoc (car elem) molar-mass-elements-mass)))
	(setq p2 "1")
	(setq elem (cdr elem))))
      ;; Update list pairs
      (setq pairs (cons p1 pairs))
      (setq pairs (cons (string-to-number p2) pairs)))
    ;; return pairs
    pairs))

(defun molar-mass-upcase-p (char)
  "Return t if CHAR is upcase, nil if not."
  (setq case-fold-search nil)
  (ignore-errors
    (if (string-match-p "[A-Z]" char) t)))

(defun molar-mass-number-p (char)
  "Return t if CHAR is a number, nil if not."
  (ignore-errors
    (if (string-match-p "[0-9]" char) t)))

(defun molar-mass-cut-list-in (list first last)
  "Cut LIST and return another list with elements between FIRST and LAST."
  (let (($cut-list '())) ;; list to return
    (setq list (cdr (member first list)))
    (while (and list (not (equal (car list) last)))
      (push (car list) $cut-list)
      (setq list (cdr list)))
    (reverse $cut-list)))
    
(defun molar-mass-cut-list-out (list first last)
  "Cut LIST and return another list with elements not between FIRST and LAST."
  (let (($cut-list '()))
    (while (not (equal (car list) first))
      (push (car list) $cut-list)
      (setq list (cdr list)))

    (while (not (equal (car list) last))
      (pop list))
    
    (setq list (cddr list))
    (while list
      (push (car list) $cut-list)
      (setq list (cdr list)))
    (reverse $cut-list)))

(provide 'molar-mass)

;;; molar-mass.el ends here
