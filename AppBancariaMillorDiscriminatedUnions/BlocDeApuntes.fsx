#load "Domain.fs"
#load "Operacions.fs"

open AppBancariaDU.Operacions
open AppBancariaDU.Domain
open System

type ContaDeCredito = ContaDeCredito of Conta  

type ContaClasificada = 
    | Credito of ContaDeCredito
    | EnNumerosVermellos of Conta   

let clasificarConta conta =   
    if conta.Saldo < 0M then (EnNumerosVermellos conta)
    else Credito (ContaDeCredito conta)  

let retirar cantidade (ContaDeCredito conta) =  
    (conta with Saldo = conta.Saldo - cantidade) 
    |> clasificarConta

let ingresar cantidade conta = 
    let conta = 
        match conta with
        | Credito (ContaDeCredito cantidade) -> conta 
        | EnNumerosVermellos conta -> conta
    { conta with Saldo = conta.Saldo + cantidade }   
    |> clasificarConta

let minhaConta = { Saldo = 0M; Propietario = { Nome = "Laurie" }; IdConta = Guid.NewGuid } |> clasificarConta

minhaConta
|> ingresar 50M
|> ingresar 100M
//|> retirar 500M <- non compila

let retiradaDeCartosSegura cantidade contaClasificada =
    match contaClasificada with
    | Credito conta -> conta |> retirar cantidade
    | EnNumerosVermellos _ ->
        printfn "A tua conta esta en numeros vermellos - retirada de cartos rexeitada"
        contaClasificada

minhaConta
|> depositar 50M
|> depositar 100M
|> retiradaDeCartosSegura 500M
|> retiradaDeCartosSegura 500M