namespace AppBancariaDU.Domain

open System

type OperacionBancaria = Ingresar | Retirar 
type Cliente = { Nome : string }
type Conta = { IdConta : Guid; Propietario : Cliente; Saldo : decimal } 
type Transaccion = { Datar : DateTime; Operacion : string; Cantidade : decimal }

//Representa unha conta bancaria que se sabe e de Credito
type ContaDeCredito = ContaDeCredito of Conta 
//Unha conta bancaria a cal pode estar en estado de credito ou en numeros vermellos
type ContaClasificada =
    | DentroDeCredito of ContaDeCredito
    | EnNumerosVermellos of Conta  
    member this.GetField getter =
        match this with
        | DentroDeCredito (ContaDeCredito conta) -> getter conta
        | EnNumerosVermellos conta -> getter conta

module Transaccions =
    //Serializa unha transaccion
    let serializar transaccion =
        sprintf "%O***%s***%M" transaccion.Datar transaccion.Operacion transaccion.Cantidade
    let deserializar (contidosDoArquivo:string) =
        let partes = contidosDoArquivo.Split([|"***"|], StringSplitOptions.None)
        { Datar = DateTime.Parse partes.[0] 
          Operacion = partes.[1]
          Cantidade = Decimal.Parse partes.[2] }