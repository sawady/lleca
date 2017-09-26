Generador de parsers Lleca
==================

Lleca es un generador de parsers implementado en Haskell, para la materia de 
Parseo y Generación de Código de la Universidad Nacional de Quilmes.

Compilando el código
--------------------

Puede utilizar GHC y la plataforma haskell para compilar, pero sugerimos utilizar
Haskell Stack compilar la aplicación. Siga los siguientes pasos

1. [Instalar Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clonar este repositorio en una carpeta en su equipo
3. Abrir una terminal en dicha carpeta y ejecutar `stack setup`
5. En la misma terminal ejecutar `stack build happy`

Su sistema habrá descargado todas las dependencias necesarias para compilar
el código fuente. En la misma terminal puede ejecutar ahora
```
stack build
```
Esto compilará el código fuente y generará un archivo ejecutable para su
plataforma. Recuerde que si realiza alguna modificación al código, deberá
volver a ejecutar este comando para el correcto funcionamiento del mismo.

Corriendo la aplicación
-----------------------

Una vez construida la aplicación puede utilizar el binario generado directamente,
o puede continuar utilizando stack para mayor comodidad.

La aplicación espera una serie de argumentos para poder ejecutarse correctamente.
Puede ver los argumentos esperados ejecutando el programa con el flag `-?` o `--help`

Utilice stack de la siguiente forma para ver la ayuda de la aplicación:

```
stack exec lleca-exe -- -?
```

El primer doble guion indica que los argumentos deben ser pasados a la aplicación
ejecutada en lugar de a stack, luego de este simbolo puede colocar tantos parametros
de la aplicación como le parezca.

Los posibles flags que puede dar a la aplicación son:

* -? --help          (Muestra el mensaje de ayuda)
* -V --version       (Muestra la versión de la aplicación)
* --numeric-version  (Muestra la versión numérica de la aplicación)
* -t --tokens        (Muestra el resultado de la tokenización en la consola)
* -a --ast           (Muestra el AST en formato texto en la consola)

Puede combinar estos flags como desee.

Finalmente hay dos parametros que son de suma relevancia y que esperan un valor
como argumento, el nombre del archivo de entrada y del de salida.

* -g --file\[=\[input\]\]  (Gramática de entrada)
* -f --file\[=\[input\]\]  (Archivo a parsear)
* -o --out\[=\[output\]\]  (El archivo de salida, defaultea a ast)


Corriendo los tests de la aplicación
------------------------------------

Si desea correr los tests puede ejecutar los mismos mediante el comando:

```
stack test
```

Esto evaluará los tests utilizando HSpec y verificará el correcto funcionamiento
de las distintas partes del sistema.
