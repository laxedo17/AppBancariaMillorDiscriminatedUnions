module AppBancariaDU.Operacions

open System 
open AppBancariaDU.Domain

let private clasificarConta conta = 
    if conta.Saldo >= 0M then (DentroDeCredito(ContaDeCredito conta))
    else EnNumerosVermellos conta

//Retira unha cantidade de cartos da conta se hai fondos suficientes
let retirar cantidade (ContaDeCredito conta) =
    { conta with Saldo = conta.Saldo - cantidade }
    |> clasificarConta

//Ingresa unha cantidade nunha conta
let ingresar cantidade conta =
    let conta =
        match conta with 
        | EnNumerosVermellos conta -> conta 
        | DentroDeCredito (ContaDeCredito conta) -> conta  
    { conta with Saldo = conta.Saldo + cantidade }
    |> clasificarConta

// Fai alguna operacion na conta como retirar ou ingresar con auditoria
let auditarComo nomeOperacion auditar operacion cantidade conta idConta propietario =
    let contaActualizada = operacion cantidade conta
    let transaccion = { Operacion = nomeOperacion; Cantidade = cantidade; Datar = DateTime.UtcNow }
    auditar idConta propietario.Nome transaccion
    contaActualizada

let tryParseOperacionSerializar  operacion =
    match operacion with 
    | "retirar" -> Some Retirar
    | "ingresar" -> Some Ingresar
    | _ -> None

//Crea unha cantidade dun conxunto historico de transaccions
let cargarConta (propietario, idConta, transaccions) =
    let abrindoConta = clasificarConta { IdConta = idConta; Saldo = 0M; Propietario = { Nome = propietario} }

    transaccions
    |> Seq.sortBy(fun tsx -> tsx.Datar)
    |> Seq.fold(fun conta tsx -> 
        let operacion = tryParseOperacionSerializar tsx.Operacion 
        match operacion, conta with 
        | Some Ingresar, _ -> conta |> ingresar tsx.Cantidade
        | Some Retirar, DentroDeCredito conta -> conta |> retirar tsx.Cantidade
        | Some Retirar, EnNumerosVermellos _ -> conta 
        | None, _ -> conta) abrindoConta