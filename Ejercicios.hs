main = do
    putStrLn("Menu\n1.-Fibonacci\n2.-Números 1 al 10\n3.-Factorial\n4.-Desaparece Números\n5.-Palindromo\n6.-Calculadora\n7.-Salir")
    opc <- getLine
    menu (read opc)

menu opc = do
    case opc of
        1 -> fib
        2 -> numeros 0
        3 -> fac 
        4 -> desaparece [0,1,2,3,4,5,6,7,8,9,10]
        5 -> palindromo
        6 -> calculadora
        7 -> print("Finalizo")
        _ -> print("Opcion No valida")

------------------------------------------------------------------------------------------------------------------------------------
fib = do
    putStrLn("Ingresa cuantos deseas")
    pos <- getLine
    print(fibonacci (read pos))
    main

fibonacci n = take n [ fst t | t <- iterate (\(a,b) -> (b,a+b)) (0,1)]
------------------------------------------------------------------------------------------------------------------------------------
numeros a= do
        if (a <= 10) 
                then do
                        print a
                        numeros (a+1)
        else    do
                putStrLn("Terminó")
                main
------------------------------------------------------------------------------------------------------------------------------------
fac = do
    putStrLn("Ingresa cuantos deseas")
    pos <- getLine
    print(factorial (read pos))
    main

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
------------------------------------------------------------------------------------------------------------------------------------
palindromo = do
        putStrLn("Ingresa la cadena sin [ ]")
        p<-getLine
        isPalindromo p
        main
isPalindromo p = do
        if(p == reverse p)
                then
                        putStrLn("Es palindromo")
                else
                        putStrLn("No es palindromo")
------------------------------------------------------------------------------------------------------------------------------------
calculadora = do
        putStrLn("-----------Menú-----------")
        putStrLn("Opcion 1 = Suma")
        putStrLn("Opcion 2 = Resta")
        putStrLn("Opcion 3 = Multiplicación")
        putStrLn("Opcion 4 = División")
        putStrLn("Opcion 5 = Salir")
        a <- getLine
        casos a

casos a = do
    case a of
        "1" -> do
            putStrLn("Ingresa el número :")
            num <- getLine
            putStrLn("Ingresa el número 2:")
            num2 <- getLine
            putStrLn("La suma es = "++ show(suma (read num) (read num2)))
            main
        "2" -> do
            putStrLn("Ingresa el número :")
            num <- getLine
            putStrLn("Ingresa el número 2:")
            num2 <- getLine
            putStrLn("La resta es = "++ show(resta (read num) (read num2)))
            main
        "3" -> do
            putStrLn("Ingresa el número :")
            num <- getLine
            putStrLn("Ingresa el número 2:")
            num2 <- getLine
            putStrLn("La resta es = "++ show(multiplicacion (read num) (read num2)))
            main
        "4" -> do
            putStrLn("Ingresa el número :")
            num <- getLine
            putStrLn("Ingresa el número 2:")
            num2 <- getLine
            putStrLn("La resta es = "++ show(division (read num) (read num2)))
            main
        "5" -> putStrLn("Salir")

        "6" -> do
            main
        _   -> do
            putStrLn("Opcion no disponible")
            main
suma num1 num2 = num1 + num2

resta num1 num2 = num1 - num2

multiplicacion num1 num2 = num1 * num2

division num1 num2 = num1 / num2
------------------------------------------------------------------------------------------------------------------------------------
desaparece n= do
        if (length n == 0)
                then do
                        putStrLn("Finish him")
                        main
                else do
                        print n
                        let lista = (init n)
                        desaparece lista