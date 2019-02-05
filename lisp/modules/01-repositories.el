;; Load package.el - Modified 2019-001-12 (changed URL to official Melpa.org)
(require 'package)

;;Add melpa to list of repositories
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") 
             t)

;;Initialize package.el
(package-initialize)
