(defun MainDB_Menu(dataBase isUp created)
(terpri)

(format t "~C~C" #\return #\linefeed)
(princ dataBase)
(princ isUp)
(princ created)
(format t "Welcome to DataBase Managment System.~C~C" #\return #\linefeed)
(format t "Create Database.~C~C" #\return #\linefeed)
(format t "Create Table.~C~C" #\return #\linefeed)
(format t "Drop Table.~C~C" #\return #\linefeed)
(format t "Import data Table.~C~C" #\return #\linefeed)
(format t "Select.~C~C" #\return #\linefeed)
(format t "Drop database.~C~C" #\return #\linefeed)
(format t "Startup database.~C~C" #\return #\linefeed)
(format t "Shutdown database.~C~C" #\return #\linefeed)
(format t "Union.~C~C" #\return #\linefeed)
(princ "Introduce Instruction: ")
(protocol (read-line) dataBase isUp created)

)

(defun protocol(instr database isUp created)
	(format t instr  #\return #\linefeed)
	    (cond ((equal isUp 0)
  		(cond ((string-equal instr "Startup database") (Startup_database database isUp created))
  			  ((string-equal instr "Shutdown database") 
  			  	(myElse database isUp "~C~CDatabase already Shutdown" created)))))
  		(cond ((equal isUp 1)(cond((string-equal instr "Create Database") 
  									(format t "~C~C--Introduce database name: " #\return #\linefeed)
  									(Create_Database (read-line) database isUp created))
  							 ((equal created 1)
  							 	(cond((string-equal instr "Create Table") 
  							 		(format t "~C~C--Introduce table name: " #\return #\linefeed)
  							 		(create_Table database isUp created (constructTable (read-line))))
  							 		 ((string-equal instr "Shutdown database") (shutdown_Database database created))
  							 		 ((string-equal instr "Import data Table")(importDataTable database))
  							 		 ((string-equal instr "Drop database") (drop_Database))
  							 		 ((string-equal instr "Drop Table") (dropTable database))
  							 		 ((string-equal instr "Select") (select database))
  							 		 )
  							 	(myElse database isUp "~C~CInstruction not recognized" created)
  							  
  							 (myElse database isUp "~C~CDatabase not created yet" created))
  			  			
  			)))
  			
  		(myElse database isUp "~C~CDatabase is not running up" created)
)

(defun Create_Database(name database pIsUp created )
	(cond((null database)
		(format t "~C~C--Creating Database: "  #\return #\linefeed)
		(format t name  #\return #\linefeed)
		(setup_Database 0 (cons name database) pIsUp created)))
		;(MainDB_Menu (cons name  database) pIsUp 1)))
	(myElse database pIsUp "~C~CDatabase already created" created))
	
(defun setup_Database(cont database pIsUp created)
	;(princ (cdr (reverse database)))
	(cond((equal cont 2)(MainDB_Menu (reverse database) pIsUp 1)))
	
	(setup_Database (+ cont 1) (cons '(("catalog")) database) pIsUp created)
)

(defun drop_Database()
	(MainDB_Menu () 1 0))

(defun Startup_database(database isUp created)

	(cond((equal isUp 0)
		(format t "Write the file name: " #\return #\linefeed)
		(create_Txt (read-line))
		(format t "~C~C--Initializing DataBase" #\return #\linefeed)(MainDB_Menu database 1 created))))


(defun create_Txt(name) 
	(with-open-file (stream name :direction :output)
    (close stream)
))

(defun shutdown_Database(database created)
	(MainDB_Menu database 0 created)
	)

(defun myElse(database isUp message created)
	(format t message #\return #\linefeed)
	(MainDB_Menu database isUp created))

(defun create_Table(database isUp created table)
	(MainDB_Menu (cons (car database) (cons (fillMap (car(cdr database)) table) 
		(cons (fillCatalog ( fillEmptyMap(car(cdr(cdr database))) table) table) ()))) isUp created))
	

(defun constructTable(name)
	(format t "~C~CSet number of columns: " #\return #\linefeed) 
	(cons name (cons (constructTableAux () (parse-integer (read-line))) ())))

(defun constructTableAux(list con)
	(cond ((equal con 0) list)
	(T (format t "~C~CSet colunm name: " #\return #\linefeed) (cons (read-line) (constructTableAux list (- con 1))
	))))


(defun addElement(name list)
	(reverse(cons name list)))

(defun longlist (lista)
	(cond ((null lista) 0)
		(T (+ 1 (longlist (CDR lista))))
	))

(defun fillMap(pBase list)
(reverse (cons list  (reverse pBase))))


(defun fillEmptyMap(pBase list)
	(reverse (cons (cons (car list) ())  (reverse pBase))))

(defun fillCatalog(pBase list)
	(cond ((null (cdr (car pBase))) (cons(reverse(cons  (cons (car list) ()) (car pBase))) (cdr pBase)))
		 (T (cons (cons (car(car pBase)) (cons (reverse(cons (car list) (reverse(car(cdr (car pBase)))) )) () )) (cdr pBase) ))) 
	)

(defun delete (x lista)
	(cond ((endp lista) lista)((equal x (CAR lista)) (CDR lista))
		(T (cons (CAR lista) (delete x (CDR lista))))))


(defun getElement( n list)
	 (nth n list))


(defun importDataTable(database)
	(format t "~C~C--Introduce table name: " #\return #\linefeed)
	(MainDB_Menu (writeData database (dataPos database (read-line) 
		(getElement 1(car(getElement 2 database))))) 1 1))

	
(defun writeData(database pos)
	 (format t "~C~C--Introduce list with data: " #\return #\linefeed)
	 (princ (reverse(cons (searchTableMap pos database (read-from-string (read-line))) (cons (getElement 1 database) 
	 	(cons (car database) ()))))))

(defun lengthCompar(len list pos)
	(cond ((equal len (longlist(getElement 1 (getElement pos (getElement 1 list))))) 1)))

(defun dataPos(database name list)
		(cond ((equal (+ 1(longlist list)) (indexInList name list)) (myElse database 1 "Table doesn't exist" 1))
			(T(indexInList name list)))
	)
(defun indexInList(name list)
		(cond((null list) 1 ) 
		((equal name (car list))(indexInList name ()))
		(T (+ 1 (indexInList name (CDR list))))))


(defun searchTableMap(pos list info)
	(princ (lengthCompar (longlist info) list pos))
	(cond((equal 1 (lengthCompar (longlist info) list pos))
		(finalDataList pos (getElement 2 list) (reverse(cons info (reverse(getElement pos 
			(getElement 2 list))))) 0))
	(T(myElse list 1 "Cantidad de datos no corresponde" 1))))
	

(defun finalDataList (pos list data cont)
	(cond((null list) ())
		((equal cont pos)(cons data (finalDataList pos (cdr list) data (+ 1 cont))))
		(T(cons (car list) (finalDataList pos (cdr list) data (+ 1 cont)) )))
	)

(defun dropTable(database)
		(format t "~C~C--Introduce table name: " #\return #\linefeed)
		(MainDB_Menu (passBy (read-line) database) 1 1
		))

(defun passBy(name  database)
	(settingUpDrop (dataPos database name 
			(getElement 1(car(getElement 2 database)))) database name))
	

(defun settingUpDrop(pos database name)
	(cons (car database) (cons (finalDataDelete pos (getElement 1 database) 0) 
		(cons (finalDataDelete pos (deleteFromCatalog (getElement 2 database) name) 0) ())
	)))
		
(defun deleteFromCatalog(list  name)
	 (cons (cons  (getElement 0 (getElement 0 list)) 
	 	(cons (delete name (getElement 1 (getElement 0 list)))  ())) (cdr list))
	)
(defun finalDataDelete (pos list cont)
	(cond((null list) ())
		((equal cont pos)(finalDataDelete pos (cdr list) (+ 1 cont)))
		(T(cons (car list) (finalDataDelete pos (cdr list) (+ 1 cont)) )))
	)
(defun select(database)
	(format t "~C~C--Introduce table name: " #\return #\linefeed)
	(princ(askInstruction database (read-line))
	)
	(MainDB_Menu database 1 1)
	)


(defun askInstruction(database table)
	(format t "~C~C--Instruction Set: " #\return #\linefeed)
	(format t "~C~C--(*)--All table: " #\return #\linefeed)
	(format t "~C~C--(Column1 Column2)--Specific table data: " #\return #\linefeed)
	(format t "~C~C--Introduce Instruction: " #\return #\linefeed)
	(selectProtocol database table (read-from-string (read-line)))
	)


(defun selectProtocol(database table insList)
	(cond((string-equal "*" (car insList))(selectAll table database))
		(T(ansConstructor database table insList (searchColumnMap table database)))
		)
	)


(defun ansConstructor(database table insList col)
	(cond((null insList) ())
		(T(cons (selectSpecData database table (dataPosSpec database (car insList) col)) 
			(ansConstructor database table (cdr insList) col))

		)))


(defun dataPosSpec(database name list)
		(cond ((equal (longlist list) (indexInListSpec name list)) (myElse database 1 "Table doesn't exist" 1))
			 (T(indexInListSpec name list)))
	)

(defun indexInListSpec(name list)
		(cond((null list) 0 ) 
		((string-equal name (car list))(indexInListSpec name ()))
		(T (+ 1 (indexInListSpec name (CDR list))))))



(defun selectSpecData(database table  pos)
	(compareInfo pos (cdr(getElement (dataPos database table 
			(getElement 1(car(getElement 2 database)))) (getElement 2 database))))	
	)
(defun searchColumnMap(table database)
	(getElement 1(getElement (dataPos database table 
			(getElement 1(car(getElement 2 database)))) (getElement 1 database))))

(defun compareInfo(cPos findList)
	(cond((null findList) ())
		 (T(cons (getElement cPos (car findList)) (compareInfo cPos (cdr findList))))))
	
(defun selectAll(name database)
	 (getElement (dataPos database name 
			(getElement 1(car(getElement 2 database)))) (getElement 2 database)))

(MainDB_Menu nil 0 0)