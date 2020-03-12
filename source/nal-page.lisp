(in-package :nal)

(define-easy-handler (index :uri "/NAL-Reasoner/index.html"
                                :default-request-type :post)

	    (conocimiento valorV relacion expresion path)

  (with-html
    (:html
     (:head 
     	(:title "NAL-Reasoner")
     	(:link :rel "stylesheet" :type "text/css" :href "css/pushbar.css")
     	(:link :rel "stylesheet" :type "text/css" :href "css/estilo.css")
    	(:meta :charset "UTF-8")
	    (:link :rel "stylesheet" 
     		:href "https://fonts.googleapis.com/css?family=Libre+Franklin&display=swap")
     	(:script :src "https://kit.fontawesome.com/d0a92786ea.js"
     		:crossorigin "anonymous"))
     	
     (:body
      (:div :id "contenedor"
      	(:header
      		(:table :style "width:100%"
      		(:tr
      			(:th :id "logoIPN" :style "width:100px;vertical-align:top"
      				(:img :src "images/logo_ipn3.png" :height "90"))
      			(:th :id "titulo" :style "text-align:center;vertical-align:top; padding: 3px"
      				(:h1 "NAL-Reasoner"))
      			(:th :id "institucion" :style "text-align:right;width:365px;vertical-align:top;padding:10px"
      				(:h3 "Centro de Investigación en Computación")
      				(:h3 "Laboratorio de Inteligencia Artificial"))
      			(:th :id "logoCIC" :style "text-align:right;width:100px;vertical-align:top;padding: 8px; padding-top: 0px"
      				(:img :src "images/logo_cic3.png" :height "75")))))
      	(:aside
      		(:ul :class "tabs1"
      			(:li :class "tabs__item active" :onclick "openTab(event,'TABLA')"
      				(:h4 "BC"))
      			(:li :class "tabs__item" :onclick "openTab(event,'BC');"
      				(:h4 "TABLA")))
          ;------------------variables para subir archivos--------------------------
          (let (post-parameter-p)
			    (when (post-parameter "file1")
			      (handle-file (post-parameter "file1"))
			      (setq post-parameter-p t))
			    (when (post-parameter "clean")
			      (clean-tmp-dir)
			      (setq post-parameter-p t)))

          (defun parser (aux)
            (cond 
              ((not (equal (parseq 'judgement aux) 'NIL)) 
                            (insert aux) 
                            (insert2 (parseq 'judgement aux)) )
              ((and (equal (parseq 'judgement aux) 'NIL) (not (null aux)) )
                    (insert2 (cons "Mensaje error" aux)) )  ))

      		(:div :id "aside"
      			(:span :class "c1" "Base de Conocimiento")
      			(:table :id "TABLA" :class "tabcontent active"
      				(:tr 
      					(:th :id "num" "No")
      					(:th :id "exp" "Expresión")
      					(:th :id "vv" "Valor de verdad")) 
                (parser conocimiento)
                (loop for i from 1 to (- *cont* 1)
                 do 
                  (setf expresion (parseq 'judgement (first (obtiene-expresion (list i)))))
                  (setf valorV (second expresion))
                  (setf relacion (first expresion))
                    (htm
                     (:tr 
                      (:td (print i))
                      (:td (print relacion))
                      (:td (print valorV))))) )
      			(:div :id "BC" :class "tabcontent")))
      	(:section :id "contenido"
      		(:ul :class "tabs2"
      			(:li :class "tabs__item2 active" :onclick "openTab2(event,'informacion')"
      				(:h4 "SALIDA"))
      			(:li :class "tabs__item2" :onclick "openTab2(event,'DEBUG')"
      				(:h4 "DEBUG")))
      		(:div :class "tabcontent2 active" :id "informacion"
            (loop for i from 1 to (- *cont2* 1) 
                   do 
                   (setf expresion (obtiene-mensaje (list i)))
                   (htm
                      (:p :class "parrafo-salida" "  "(print i) 
                        "La expresión: " (print (first expresion)) " ha sido añadida"))))

      		(:div :class "tabcontent2" :id "DEBUG") 
          ;(print(parseq 'judgement conocimiento))
      		(:button :class "simbolo" :onclick "simbolo('->')" :style "margin-left: 10px" "->")
      		(:button :onclick "simbolo('<->')"  :class "simbolo" "<->")
      		(:button :onclick "simbolo('o->')"  :class "simbolo" "o->")
      		(:button :onclick "simbolo('->o')"  :class "simbolo" "->o")
      		(:button :onclick "simbolo('o->o')"  :class "simbolo" "o->o")
      		(:button :onclick "simbolo('&cap;')"  :class "simbolo" "&cap;")
      		(:button :onclick "simbolo('&cup;')"  :class "simbolo" "&cup;")
      		(:button :onclick "simbolo('&ominus;')"  :class "simbolo" "&ominus;")
      		(:button :onclick "simbolo('&#8212;')"  :class "simbolo" "&#8212;")
      		(:button :onclick "simbolo('+')"  :class "simbolo" "+")
      		;;---CAMBIAR---
      		;(:input :type :text :class "conocimiento" :id "conocimiento" :value (or expresion "perro -> animal") )
      		;(:button :onclick "almacena()" "Subir") )
  			
      		(:p (:form :method :post 
           		(:input :type :text :class "conocimiento" :id "conocimiento" 	:name "conocimiento"	
           			:placeholder "Estructura: perro -> animal" )
          		(:input :class "botonSubir" :type "submit" ))) )
      		

      	(:div :class "pc" :data-pushbar-target "mypushbar1"
      		(:i :class "fas fa-angle-double-left"))
      	(:section :data-pushbar-id "mypushbar1" :class "pushbar from_right"
      		(:button :class "cerrar" :data-pushbar-close :value"x")
      		(:button :class "regresar" :onclick "back()" (:p "<"))
      		(:section :class "menu"
      			(:div :class "submenu" :onclick "display_pushbar('.politica')"  (:h5 "Política de Control"))
	      		(:div :class "submenu" :onclick "display_pushbar('.selecciona-BC')"  (:h5 "Seleccionar Base de Conocimiento"))
	      		(:div :class "submenu" :onclick "display_pushbar('.sube-BC')"  (:h5 "Subir Base de Conocimiento")))

      		;;---------------------------------------FORMULARIO POLIÍTICA DE CONTROL------------------------------
      		(:section :class "politica sub-menu"
      			(:span :class "c2" "Política de Control")
				;;---Cambiar---
				(:form :action "#" :method "get" :style "margin-left: 10px"
				(:legend "Parámetros tipo 1") :br
				(:p "Algo 1: "
					(:label 
						(:select :name "algo1"
							(:option "algo1.1")
							(:option "algo1.2")
							(:option "algo1.3")))) :br
				(:p "Algo 2: "
					(:label 
						(:input :type "radio" :name "algo2" :value "tipo1") "Tipo 1")
					(:label 
						(:input :type "radio" :name "algo2" :value "tipo2") "Tipo 2")) :br
				(:p "Rango de algo: "
					(:input :type "range" :name "volumen" :min "0" :max "10" :step "1"))
				(:legend "Parámetros tipo 2") :br
				(:p "Algo 3: " :br :t 
					(:label "&nbsp;&nbsp;&nbsp;&nbsp;"
						(:input :type "checkbox" :name "op1") "Opción 1 ") :br
					(:label "&nbsp;&nbsp;&nbsp;&nbsp;"
						(:input :type "checkbox" :name "op2") "Opción 2 ") :br
					(:label "&nbsp;&nbsp;&nbsp;&nbsp;"
						(:input :type "checkbox" :name "op3") "Opción 3 ")) :br
				(:p 
					(:label "Color: "
						(:input :type "color" :name "colorfavorito"))) :br
				(:p 
					(:input :type "submit" :class "submit" :value "Enviar datos")
					(:input :type "reset" :value "Restaurar"))))


      		;---------------------------------------SELECCIONAR BASE DE CONOCIMIENTO-----------------------------------------
      		(:section :class "selecciona-BC sub-menu"
      			(:span :class "c4" "Seleccionar Base de Conocimiento")
      			
      			(when *tmp-test-files*
			        (htm
			         (:p
			          (:table :style "padding: 5px;" :border 1 :cellpadding 2 :cellspacing 0
			           (loop for (path file-name nil) in *tmp-test-files*
			                 for counter from 1
			                 do (htm
			                     (:tr (:td :align "right" (str counter))
			                      (:td (:a :href (format nil "files/~A?path=~A"
			                                             (url-encode file-name)
			                                             (url-encode (namestring path)))
			                            (esc file-name)))
                            (print file-name)
                            (print  path)
			                      (:td :align "right"
			                       (str (ignore-errors
			                              (with-open-file (in path)
			                                (file-length in)) ))
			                       "&nbsp;Bytes")))))))) )
      		 

      		;---------------------------------------SUBIR BASE DE CONOCIMIENTO-----------------------------------------
      		(:section :class "sube-BC sub-menu"
      			(:span :class "c3" "Subir Base de Conocimiento")
			  	(no-cache)
			      (:form :method :post :enctype "multipart/form-data"
			       (:p :class "parrafo-subir-bc" "Archivo: "
			        (:input :type :file :accept ".txt"
			         :name "file1"))
			       (:p :class "parrafo-subir-bc" (:input :type :submit))) )

             

            (when *tmp-test-files*
             ;Crea un nuevo archivo para guardar las expresiones ingresadas en la caché
              ;"/home/jenifer/Escritorio/NAL/BC/prueba.txt"
               (with-open-file  (stream "/home/jenifer/Escritorio/NAL/BC/prueba.txt"
                                :direction :output :if-exists :supersede)
                   (loop for i from 1 to (- *cont* 1)
                   do 
                    (write-line (first (obtiene-expresion (list i))) stream)) )

              (initialize-cache) ;----- reiniciar la bc de expresiones y mensajes de error antes de evaluar el nuevo archivo 
              (setq path (first (first *tmp-test-files*) ))  ;--- selecciona el último archivo subido
              (with-open-file (in path)
                (loop for line = (read-line in nil) ;------LEE ARCHIVO
                     while line do (parser line)) ) )  ;--- parsea cada línea

        );/section data-pushbar-id 
        	
      	(:footer
      		(:p "Instituto Politécnico Nacional. Centro de Investigación en Computación.")))
      (:script :type "text/javascript" :src "js/funciones.js") ))))
