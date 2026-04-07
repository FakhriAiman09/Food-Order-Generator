#|
Author:      Muhammad Fakhri Aiman Amrenorman
E-mail:      mba6052@psu.edu
Course:      CMPSC 460
Assignment:  Lisp Programming Project
Due date:    4/6/2026
File:        FoodOrderGenerator.LISP
Purpose:     This program uses random-number generation to create food orders
             from lists of different categories of food items. It also allows
             the user to add food items to or remove food items from the lists
             initially provided. The program loops until the user decides that
             s/he is done creating food orders.
IDE/Compiler:
             LispWorks 8.1.2 Personal Edition
Operating system:
             MS Windows 11
References:  Web sites listed on course Programming Language Resources page
             Lisp interactive sessions and demo functions posted on course
             Demo Programs page
|#

; Function to display the main menu of options
(defun display-main-menu ()
  (format t "~%MENU OPTIONS:~%")
  (format t "1) Generate a food order~%")
  (format t "2) Add a food item~%")
  (format t "3) Remove a food item~%")
  (format t "4) Quit~%"))

; Function to display the food item category submenu
(defun display-sub-menu ()
  (format t "~%FOOD ITEM TYPES:~%")
  (format t "1) Sandwich~%")
  (format t "2) Side~%")
  (format t "3) Beverage~%")
  (format t "4) Dessert~%"))

; Function to display all four food item lists
(defun display-all-lists (sandwiches sides beverages desserts)
  (format t "~%Current food items lists:~%")
  (format t "Sandwiches: ~a~%" sandwiches)
  (format t "Sides     : ~a~%" sides)
  (format t "Beverages : ~a~%" beverages)
  (format t "Desserts  : ~a~%" desserts))

; Function to obtain and validate the main menu choice (1-4)
(defun get-menu-choice ()
  (format t "Please enter your choice: ")
  (finish-output)
  (let ((choice (read)))
    (if (and (integerp choice) (>= choice 1) (<= choice 4))
        choice
        (progn
          (format t "Invalid choice! Please re-enter.~%")
          (get-menu-choice)))))

; Function to obtain and validate the submenu choice (1-4)
(defun get-sub-menu-choice ()
  (format t "Please enter your choice: ")
  (finish-output)
  (let ((choice (read)))
    (if (and (integerp choice) (>= choice 1) (<= choice 4))
        choice
        (progn
          (format t "Invalid choice! Please re-enter.~%")
          (get-sub-menu-choice)))))

; Function to obtain a food item name from the user
(defun get-food-item ()
  (format t "Please enter your food item: ")
  (finish-output)
  (read))

; Function to generate a random food order from each list
(defun generate-food-order (sandwiches sides beverages desserts)
  (let ((sandwich (nth (random (length sandwiches)) sandwiches))
        (side     (nth (random (length sides))      sides))
        (beverage (nth (random (length beverages))  beverages))
        (dessert  (nth (random (length desserts))   desserts)))
    (format t "~%Random food order:~%")
    (format t " ~a, ~a, ~a, ~a~%" sandwich side beverage dessert)))

; Function to add a food item to a list only if not already present
(defun add-food-item (food-list item)
  (if (member item food-list)
      food-list
      (append food-list (list item))))

; Function to remove a food item from a list safely
(defun remove-food-item (food-list item)
  (if (null food-list)
      food-list
      (remove item food-list)))

; Function to handle adding a food item to the chosen category
(defun handle-add (sandwiches sides beverages desserts)
  (display-all-lists sandwiches sides beverages desserts)
  (display-sub-menu)
  (let ((cat  (get-sub-menu-choice))
        (item (get-food-item)))
    (cond
      ((= cat 1)
       (let ((new-list (add-food-item sandwiches item)))
         (format t "Sandwiches: ~a~%" new-list)
         (list new-list sides beverages desserts)))
      ((= cat 2)
       (let ((new-list (add-food-item sides item)))
         (format t "Sides     : ~a~%" new-list)
         (list sandwiches new-list beverages desserts)))
      ((= cat 3)
       (let ((new-list (add-food-item beverages item)))
         (format t "Beverages : ~a~%" new-list)
         (list sandwiches sides new-list desserts)))
      ((= cat 4)
       (let ((new-list (add-food-item desserts item)))
         (format t "Desserts  : ~a~%" new-list)
         (list sandwiches sides beverages new-list))))))

; Function to handle removing a food item from the chosen category
(defun handle-remove (sandwiches sides beverages desserts)
  (display-all-lists sandwiches sides beverages desserts)
  (display-sub-menu)
  (let ((cat  (get-sub-menu-choice))
        (item (get-food-item)))
    (cond
      ((= cat 1)
       (let ((new-list (remove-food-item sandwiches item)))
         (format t "Sandwiches: ~a~%" new-list)
         (list new-list sides beverages desserts)))
      ((= cat 2)
       (let ((new-list (remove-food-item sides item)))
         (format t "Sides     : ~a~%" new-list)
         (list sandwiches new-list beverages desserts)))
      ((= cat 3)
       (let ((new-list (remove-food-item beverages item)))
         (format t "Beverages : ~a~%" new-list)
         (list sandwiches sides new-list desserts)))
      ((= cat 4)
       (let ((new-list (remove-food-item desserts item)))
         (format t "Desserts  : ~a~%" new-list)
         (list sandwiches sides beverages new-list))))))

; Main driver function that initializes lists and loops until user quits
(defun loop-program ()
  (let ((sandwiches (list 'blt 'tuna-salad 'grilled-cheese 'hamburger 'cheesesteak))
        (sides      (list 'tossed-salad 'pasta-salad 'fries 'onion-rings 'chips))
        (beverages  (list 'lemonade 'iced-tea 'water 'soda 'coffee))
        (desserts   (list 'ice-cream 'cake 'pie 'brownie 'cookies)))
    (loop
      (display-main-menu)
      (let ((choice (get-menu-choice)))
        (cond
          ((= choice 1)
           (generate-food-order sandwiches sides beverages desserts))
          ((= choice 2)
           (let ((updated (handle-add sandwiches sides beverages desserts)))
             (setf sandwiches (first  updated))
             (setf sides      (second updated))
             (setf beverages  (third  updated))
             (setf desserts   (fourth updated))))
          ((= choice 3)
           (let ((updated (handle-remove sandwiches sides beverages desserts)))
             (setf sandwiches (first  updated))
             (setf sides      (second updated))
             (setf beverages  (third  updated))
             (setf desserts   (fourth updated))))
          ((= choice 4)
           (format t "~%\"Thank you for using my Food Order Generator!\"~%")
           (return (values))))))))