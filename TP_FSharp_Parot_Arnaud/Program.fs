﻿open System
open System.Text.RegularExpressions

// Exo 1 : 
(*
let five = 5
let seven = 7
let cube x = x * x * x

cube five |> printfn "%i"
cube seven |> printfn "%i"
*)

//---------------------------------------------------------------------------------------------------------------

(*
let interestRate (balance: decimal): single = //implémenter la fonction
    match balance with
    | y when (y < 0m) -> 3.213f
    | y when (y < 1000m) -> 0.5f
    | y when (1000m <= y && y < 5000m) -> 1.621f
    | y when (y > 5000m) -> 2.475f

let interest (balance: decimal): decimal = // implementer ici
    balance * decimal (interestRate(balance) / 100f)

let annualBalanceUpdate(balance: decimal): decimal = // Implémenter
   balance + interest(balance)

let amountToDonate(balance: decimal) (taxFreePercentage: float): int =
    

// => 27

interestRate 200.75m |> printfn "%f"
interest 200.75m |> printfn "%f"
annualBalanceUpdate 200.75m |> printfn "%f"
*)

//-----------------------------------------------------------------------------------------------------------------

// Exo 2 :

(*
let message(chaine: string) : string =
    let section = chaine.Split[|':'|]
    section[1].Trim(' ')

let logLevel(chaine: string) : string =
    let section = chaine.Split[|':'|]
    section[0].Trim(' ', '[', ']').ToLower()

let reformat(chaine: string) : string =
    message(chaine) + " (" + logLevel(chaine) + ")"



message "[ERREUR] : Opération invalide" |> printfn "%s"
message "[WARNING] : Disque presque plein" |> printfn "%s"
logLevel "[ERREUR] : Opération invalide" |> printfn "%s"
reformat "[INFO] : Opération terminée" |> printfn "%s"

let talkToBOB(paroles: string) : string =
    match paroles with
    | y when (paroles.IndexOf('?') > 0 && Regex.Match(paroles, "[a-z]").Success) -> "Bien sûr"
    | y when (paroles.IndexOf('?') < 0 && Regex.Match(paroles, "^[A-Z]+$").Success) -> "Whoa, calme-toi !"
    | y when (paroles.IndexOf('?') > 0 && Regex.Match(paroles, "[A-Z]").Success) -> "Calme-toi, je sais ce que je fais"
    | y when (paroles = "") -> "Très bien. Sois comme ça"
    | _ -> "Peu importe"

talkToBOB "Comment ça va ?" |> printfn "%s"
talkToBOB "RGTHEJYJY" |> printfn "%s"
talkToBOB "COMMENT CA VA ?" |> printfn "%s"
talkToBOB "" |> printfn "%s"
talkToBOB "Test" |> printfn "%s"
*)

//-------------------------------------------------------------------------------------------------------------------

// Exo : 3
(*
type Coach = {Name: string; FormerPlayer: bool}
type Stats = {Wins: int; Pertes: int}
type Team = {Name: string; Coach: Coach; Stats: Stats}

let createCoach(nom: string) (ancienJoueur: bool): Coach =
    {Name = nom; FormerPlayer = ancienJoueur}

let createStats(victoire: int) (defaite: int): Stats =
    {Wins = victoire; Pertes = defaite}

let createTeam(nom: string) (coach: Coach) (stats: Stats): Team =
    {Name = nom; Coach = coach; Stats = stats}

let replaceCoach(equipe: Team) (newCoach: Coach): Team =
    {equipe with Coach = newCoach} 

let isSameTeam(equipe1: Team) (equipe2: Team): bool =
    equipe1 = equipe2

let rootForTeam(equipe: Team): bool =
    match equipe with
    | y when (y.Coach.Name = "Gregg Popovich") -> true
    | y when (y.Coach.FormerPlayer) -> true
    | y when (y.Name = "Chicago Bulls") -> true
    | y when (y.Stats.Wins > 60) -> true
    | y when (y.Stats.Pertes > y.Stats.Wins) -> true
    | _ -> false


let pacersCoach = createCoach "Larry Bird" true
let pacersStats = createStats 58 24
let pacersTeam = createTeam "Indiana Pacers" pacersCoach pacersStats
let lakersCoach = createCoach "Del Harris" false
let lakersStats = createStats 61 21
let lakersTeam = createTeam "LA Lakers" lakersCoach lakersStats
isSameTeam pacersTeam lakersTeam |> printfn "%b"

let spursCoach = createCoach "Gregg Popovich" false
let spursStats = createStats 56 26
let spursTeam = createTeam "San Antonio Spurs" spursCoach spursStats
rootForTeam spursTeam |> printfn "%b"

(*
let coach = createCoach "Larry Bird" true
let record = createStats 58 24
let newCoach = createCoach "Isiah Thomas" true
let team = createTeam "Indiana Pacers" coach record
replaceCoach team newCoach |> printfn "%A"
*)

// Partie : B

type Approbation =
    | Oui
    | Non
    | Bof

type Cuisine =
    | Coréenne
    | Turc

type Genre =
    | Crime 
    | Horreur 
    | Romance
    | Thriller

type Activity =
    | BoardGame
    | Chill
    | Film of Genre
    | Restaurant of Cuisine
    | Walk of int

let rateActivity(activite: Activity) : Approbation =
    match activite with
    | BoardGame -> Non
    | Chill -> Non
    | Film Romance -> Oui
    | Restaurant Coréenne -> Oui
    | Restaurant Turc -> Bof
    | Walk i when i < 3 -> Oui
    | Walk i when i < 5 -> Bof
    | _ -> Non

let printApprobation(approbation: Approbation): string =
    match approbation with
    | Oui -> "Oui"
    | Non -> "Non"
    | Bof -> "Peut-être"

printApprobation(rateActivity (Film Romance)) |> printfn "%s"
*)

//----------------------------------------------------------------------------------------------------

// Exo 4 :

