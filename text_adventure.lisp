(defparameter *nodes* '((living-room (you are in the living-room.
				       a wizard is snoring loudly on the couch.))
			 (garden (you are in a beautiful garden.
				  there is a well in front of you.))
			 (attic (you are in the attic.
				 there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))


(defparameter *edges* '((living-room 
			 (garden west door)
			 (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

(defun objects-at (location objects object-locations)
  (labels ((at-loc-p (objects)
	     (eq (cadr (assoc objects object-locations)) location)))
    (remove-if (lambda (o) (not (at-loc-p o))) objects)))

(defun describe-objects (location objects object-locations)
  (labels ((describe-obj (object)
	     `(you see a ,object on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at location objects object-locations)))))


(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))


