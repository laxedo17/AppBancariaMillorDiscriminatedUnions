module AppBancariaDU.RepositorioDeArquivos

open AppBancariaDU.Domain
open System.IO 
open System  

let private rutaContas =
    let ruta = @"contas"
    Directory.CreateDirectory ruta |> ignore 
    ruta
let private tryAtoparCarpetaConta propietario =
    let carpetas = Directory.EnumerateDirectories(rutaContas, sprintf "%s_*" propietario) |> Seq.toList
    match carpetas with
    | [] -> None 
    | carpeta :: _ -> Some(DirectoryInfo(carpeta).Name)

let private construirRuta(propietario, idConta:Guid) = sprintf @"%s\%s_%O" rutaContas propietario idConta 

let cargarTransaccions (carpeta:string) =
    let propietario, idConta =
        let partes = carpeta.Split '_'
        partes.[0], Guid.Parse partes.[1]
    propietario, idConta, construirRuta(propietario, idConta)
                            |>Directory.EnumerateFiles
                            |> Seq.map (File.ReadAllText >> Transaccions.deserializar)

//Atopa todas as transaccions no disco dun propietario especifico
let tryAtoparTransaccionsEnDisco = tryAtoparCarpetaConta >> Option.map cargarTransaccions 

//loguease no sistema de arquivos
let escribirTransaccion idConta propietario transaccion = 
    let ruta = construirRuta(propietario, idConta)
    ruta |> Directory.CreateDirectory |> ignore 
    let rutaArquivo = sprintf "%s/%d.txt" ruta (transaccion.Datar.ToFileTimeUtc())
    let linha = sprintf "%O***%s***%M" transaccion.Datar transaccion.Operacion transaccion.Cantidade
    File.WriteAllText(rutaArquivo, linha)


