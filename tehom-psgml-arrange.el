;;; tehom-psgml-arrange.el --- Rearrange XML/SGML easily

;; Copyright (C) 2000 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: hypermedia, extensions, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Purpose

;; Rearranging SGML and XML elements without doing a lot of navigating
;; by hand.  Useful on rambledocs (rmb) documents, when you've
;; accumulated a lot of nodes in random order.

;; Usage

;; Basically you use tehom-psgml-arr-set-name-property on each type of
;; node you want to move.  Then you alternate using
;; tehom-psgml-arr-cut and tehom-psgml-arr-paste.

;; ***WARNING***: This code completely expects you to know what you're
;; doing.  It supports my work habits, but a lot more work could be
;; done wrt safety and foolproofness.  If you don't know SGML or XML,
;; this package will merely help you ruin your document very fast.

;; The cut nodes better come from somewhere that can afford to lose
;; them, otherwise you'll invalidate you document.

;; Similarly, you have to paste them somewhere that can accept them.

;; If some of your nodes have the same name, some will be duplicated,
;; some left out.

;; Non-features:  (Plenty)

;; Ideally, this would be surrounded by one or more wrapper functions
;; that collect all the user's interactions before cutting, including
;; where to paste back in.

;; It would intelligently help the user indicate which attribute is
;; the "name" attribute of a node, and would understand a
;; configuration list.

;; It would hold the user's hand wrt choosing a paste spot.

;; It would deal with paste-back-in on a hier, programmable level.
;; One desirable functionality is to create a new node at the same
;; time in the same place, in effect demoting some nodes to lower
;; level in an outline-like dtd.

;; It would only offer to cut nodes that can safely be removed and can
;; safely be pasted to the target element.

;; It would allow sets of nodes, rather than basically supporting just
;; one type at a time.

;; It would test that all the nodes offered to be cut had unique
;; names, error if not.

;;; Code:

;;;;;;;;;;;;;;
;;Requirements

(provide 'tehom-psgml-arrange)

(require 'psgml)
(require 'tehom-psgml)
(require 'arrange)
(eval-when-compile
  (require 'cl))

;;;;;;;
;;Types

(defstruct tehom-psgml-node-data
  ""
  name
  start
  end
  text)

;;;;;;;;;;;;;;;
;;Configuration

(defgroup tehom-psgml nil
  "Organize on-the-fly commentary."
  :group 'hypermedia)


(defcustom tehom-psgml-arr-att-name-alist nil 

  "A mapping from dtd-name * element-gi to name-attribute 
The elements are of the form \(dtd-name . alist\)
The nested alists' elements are of the form \(element-gi
. name-attribute\)" 

  :type 
  '(repeat
    (cons
      (string :tag "DTD name")
      (repeat
	(cons
	  (string :tag "Element name")
	  (string :tag "Element's name attribute")))))

  ;;It would be nice to have a "set" method similar to
  ;;tehom-psgml-arr-set-name-property but that would require being in
  ;;the correct buffer.
  :group 'tehom-psgml
  :group 'rambledocs)


;;;;;;;;;;;
;;Utility functions
(defun tehom-build-nested-alist (lis value)
  ""

  (if
    (null lis)
    value
    (list
      (cons
	(car lis)
	(tehom-build-nested-alist (cdr lis) value)))))

(defun tehom-set-in-nested-alist
  (alist lis value)
  "Recursively sets LIS in ALIST.
Returns the altered version of ALIST.
LIS should be a dotted list."

  (if
    (null lis)
    value
    
    (let
      (
	(cell
	  (assoc (car lis) alist)))
      (if
	(not cell)
	(append
	  (tehom-build-nested-alist lis value)
	  alist)
	(progn
	  (callf 
	    tehom-set-in-nested-alist (cdr cell) (cdr lis) value)
	  alist)))))


(eval-when-compile
  (setf
    (get 'tehom-set-in-nested-alist 'rtest-suite)
    '("tehom-set-in-nested-alist"
       (
	 (tehom-set-in-nested-alist '(("a" . 1)) '("a") 2)
	 '(("a" . 2)))
       
       (
	 (tehom-set-in-nested-alist '(("a" . 1) ("b" . 3)) '("a") 2)
	 '(("a" . 2) ("b" . 3)))

       (
	 (tehom-set-in-nested-alist '() '("a") 2)
	 '(("a" . 2)))

       (
	 (tehom-set-in-nested-alist '() '("a" "b" "c") 2)
	 '(("a" ("b" ("c" . 2)))))

       (
	 (tehom-set-in-nested-alist
	   '(("a" . (("b" . 1))))  
	   '("a" "b") 2)
	 '(("a" ("b" . 2))))
       
       (
	 (tehom-set-in-nested-alist
	   '() '("a" "b") 2)
	 '(("a" ("b" . 2))))

       (
	 (tehom-set-in-nested-alist
	   '() '("a" "b" "c") 2)
	 '(("a" ("b" ("c" . 2)))))

       (
	 (tehom-set-in-nested-alist
	   '(("a" ("b" ("c" . 1)))) 
	   '("a" "b" "d") 2)
	 '(("a" ("b" ("d" . 2) ("c" . 1)))))
       

       )))

;;;;;;;;;;;;;;;;;;;;;
;;Low-level functions

(defun tehom-psgml-arr-nodelist->nodedata-list (lis alist)
  "Return a list of tehom-psgml-node-data respctive to LIS
LIS must be a list of psgml `element'.
ALIST must be an alist mapping from element's gi to its name-attribute
name." 
  
  (loop
    for el in lis

    append
    (let*
      ( 
	(gi
	  (sgml-element-gi el))
	(name-att-name
	  (cdr (assoc gi alist))))

      ;;Only proceed if the atribute is accessible, otherwise give nil.
      (when 
	(and
	  name-att-name
	  (not (string= name-att-name "")))
	(let
	  (
	    (name
	      (sgml-element-attval el name-att-name)))

	  (list
	    (make-tehom-psgml-node-data
	      :name  name
	      :start (set-marker (make-marker) (sgml-element-start el))
	      :end   (set-marker (make-marker) (sgml-element-end   el)))))))))

(defun tehom-psgml-arrange-nodedata-list (lis)
  "Arrange the nodes that will be cut."
  
  (let*
    ((strings 
       (mapcar #'tehom-psgml-node-data-name lis))
      (use-strings
	(arrange-strings-other-window strings)))
    
    (loop
      for el in lis

      if
      (member* (tehom-psgml-node-data-name el) use-strings :test #'string=)
      collect el into use-els
      else
      collect el into skip-els

      finally return (list use-els skip-els))))


(defun tehom-psgml-arr-cut-1 (lis)
  ""
  
  (loop
    for el in lis

    do
    (let
      ( (s (tehom-psgml-node-data-start el))
	(e (tehom-psgml-node-data-end   el)))
      
      (setf (tehom-psgml-node-data-text el)
	(buffer-substring s e))
      
      (delete-region s e))
    
    collect el))


(defun tehom-psgml-arr-paste-1 (lis)
  "Insert the elements of LIS at point.
The elements of LIS must be type tehom-psgml-node-data.

It is the caller's responsibility to make sure point is a good place
to paste the elements into." 
  
  ;;Could also try to indent the line with (sgml-indent-line nil nil)
  (dolist (el lis)
    (insert
      (tehom-psgml-node-data-text el)
      "\n")))




(defun tehom-psgml-arr-unmark-list (lis)
  "Clear all the markers in LIS.
The elements of LIS must be type tehom-psgml-node-data."
  
  (dolist (el lis)
    (set-marker (tehom-psgml-node-data-start el) nil)
    (set-marker (tehom-psgml-node-data-end  el) nil)))


(defun tehom-psgml-arr-get-alist ()
  ""
  
  (let*
    (      
      (dtd-name 
	(tehom-psgml-get-dtd-name))
      (dtd-alist
	(cdr (assoc dtd-name tehom-psgml-arr-att-name-alist))))
    dtd-alist))

;;;;;;;;;;;;;;;
;; Entry points


(defun tehom-psgml-arr-set-name-property ()
  "Set the name property of the element at point."
  
  (interactive)
  ;;Make sure we are in a psgml buffer.
  (sgml-need-dtd)

  (let*
    ( (el 
	(sgml-find-element-of (point)))
      (el-gi
	(sgml-element-gi el))
      (new-att-name
	(tehom-psgml-pick-attribute el))
      (dtd-name 
	(tehom-psgml-get-dtd-name)))

    (callf 
      tehom-set-in-nested-alist
      tehom-psgml-arr-att-name-alist
      (list dtd-name el-gi)
      new-att-name)))


;;Ideally, these would be replaced by safer, more intelligent
;;handholding functions. 

(defvar tehom-psgml-arr-last-cut nil "" )

(defun tehom-psgml-arr-cut ()
  "Cut some of the children at point in an xml/sgml buffer.

This uses psgml, so other markup packages mustn't be in control."

  (interactive)

  ;;Make sure we are in a psgml buffer.
  (sgml-need-dtd)

  (let*
    ( (position (point))
      (parent 
	(sgml-find-element-of position))
      (nodelist 
	(tehom-psgml-all-children parent))
      (nodedata-list 
	(tehom-psgml-arr-nodelist->nodedata-list 
	  nodelist
	  (tehom-psgml-arr-get-alist)))
      (nodedata-cell 
	(tehom-psgml-arrange-nodedata-list nodedata-list)))

    (when
      (car nodedata-cell)
      (push
	(tehom-psgml-arr-cut-1 (car nodedata-cell))
	tehom-psgml-arr-last-cut))

    (tehom-psgml-arr-unmark-list (car  nodedata-cell))
    (tehom-psgml-arr-unmark-list (cadr nodedata-cell))))



(defun tehom-psgml-arr-paste ()
  "Past back the text cut by tehom-psgml-arr-cut-0."
  
  (interactive)
  (if (null tehom-psgml-arr-last-cut)
    (error "tehom-psgml-arr-last-cut kill list is empty"))
  
  (tehom-psgml-arr-paste-1 (pop tehom-psgml-arr-last-cut)))


;;; tehom-psgml-arrange.el ends here