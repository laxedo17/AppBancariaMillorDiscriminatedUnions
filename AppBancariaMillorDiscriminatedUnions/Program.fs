module AppBancariaDU.Program

open System
open AppBancariaDU.Domain
open AppBancariaDU.Operacions

let retirarConAuditoria cantidade (ContaDeCredito conta as contaDeCredito) =
    auditarComo "retirar" Auditar.loggerComposto retirar cantidade contaDeCredito conta.IdConta conta.Propietario

let ingresarConAuditoria cantidade (contaClasificada:ContaClasificada) =
    let idConta = contaClasificada.GetField ( fun a -> a.IdConta )
    let propietario = contaClasificada.GetField ( fun p -> p.Propietario)
    auditarComo "ingresar" Auditar.loggerComposto ingresar cantidade contaClasificada idConta propietario
let tryCargarContaDendeDisco = RepositorioDeArquivos.tryAtoparTransaccionsEnDisco >> Option.map Operacions.cargarConta

type Comando = ContaCmd of OperacionBancaria | Sair

[<AutoOpen>]
module CommandParsing =
    let tryParse cmd =
        match cmd with 
        | 'i' -> Some (ContaCmd Ingresar)
        | 'r' -> Some (ContaCmd Retirar)
        | 'x' -> Some Sair 
        | _ -> None

[<AutoOpen>]
module EntradaDeUsuario =
    let comandos =
        Seq.initInfinite( fun _ ->
            Console.Write "(i)ngresar, (r)etirar ou (s)air; "
            let saida = Console.ReadKey().KeyChar
            Console.WriteLine()
            saida)

    let getCantidade comando =
        let capturarCantidade _ =
            Console.Write "Entra Cantidade: "
            Console.ReadLine() |> Decimal.TryParse
        Seq.initInfinite capturarCantidade
        |> Seq.choose(fun cantidade -> 
            match cantidade with
            | true, cantidade when cantidade <= 0M -> None
            | false, _ -> None 
            | true, cantidade -> Some (comando, cantidade))
        |> Seq.head

[<EntryPoint>]
let main _ =
    let abrirConta =
        Console.Write "Por favor entra o teu nome: "
        let propietario = Console.ReadLine()

        match tryCargarContaDendeDisco propietario with
        | Some conta -> conta
        | None ->
            DentroDeCredito(ContaDeCredito { IdConta = Guid.NewGuid()
                                             Saldo = 0M 
                                             Propietario = { Nome = propietario }})
    printfn "O balance actual e %M€" (abrirConta.GetField(fun a -> a.Saldo))

    let procesarComando conta (comando, cantidade) =
         printfn ""
         let conta =
            match comando with
            | Ingresar -> conta |> ingresarConAuditoria cantidade
            | Retirar ->
                match conta with
                | DentroDeCredito conta -> conta |> retirarConAuditoria cantidade
                | EnNumerosVermellos _ ->
                    printfn "Non podes retirar fondos xa que che tes a conta en numeros vermellos"
                    conta
         printfn "O saldo actual e %M€" (conta.GetField(fun c -> c.Saldo))
         match conta with 
         | DentroDeCredito _ -> ()
         | EnNumerosVermellos _ -> printfn "A tua conta esta en numeros vermellos"
         conta
         
    let pechandoConta =
        comandos 
        |> Seq.choose tryParse
        |> Seq.takeWhile ((<>) Sair)
        |> Seq.choose(fun cmd ->
            match cmd with 
            | Sair -> None 
            | ContaCmd cmd -> Some cmd)
        |> Seq.map getCantidade
        |> Seq.fold procesarComando abrirConta
    printfn ""
    printfn "Pechando o saldo:\r\n %A" pechandoConta
    Console.ReadKey() |> ignore

    0 // return an integer exit code