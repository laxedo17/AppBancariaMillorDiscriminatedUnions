module AppBancariaDU.Auditar
//Na app bancaria agora imos incluir:
//  Empregar options en situacións prácticas
//  Usar discriminated unions para modelar con precisión un conxunto pechado de casos
//  Traballar con coleccións con datos máis complexos
//  Aplicar as regras de negocio a través do type system

open AppBancariaDU.Domain  

//Loguease na consola
let imprimirTransaccion _ idConta transaccion =    
    printfn "Conta %O: %s of %M" idConta transaccion.Operacion transaccion.Cantidade

//Loguease a ambas a consola e o sistema de arquivos
let loggerComposto =
    let loggers =
        [ RepositorioDeArquivos.escribirTransaccion 
          imprimirTransaccion]
    fun idConta propietario transaccion ->
        loggers
        |> List.iter(fun logger -> logger idConta propietario transaccion)
