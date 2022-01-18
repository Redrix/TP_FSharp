open System
open System.Text.RegularExpressions

// Exo 1 : 
let five = 5
let seven = 7
let cube x = x * x * x

cube five |> printfn "%i"
cube seven |> printfn "%i"

//---------------------------------------------------------------------------------------------------------------

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
    Convert.ToInt32(Math.Round((balance * decimal ((taxFreePercentage / 100.0) * 2.0)), MidpointRounding.ToNegativeInfinity))

interestRate 200.75m |> printfn "%f"
interest 200.75m |> printfn "%f"
annualBalanceUpdate 200.75m |> printfn "%f"
let solde = 550.5m
let taxFreePercentage = 2.5
amountToDonate solde taxFreePercentage |> printfn "%i"

//-----------------------------------------------------------------------------------------------------------------

// Exo 2 :

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

// Partie B :

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

//-------------------------------------------------------------------------------------------------------------------

// Exo : 3

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

let coach = createCoach "Larry Bird" true
let record = createStats 58 24
let newCoach = createCoach "Isiah Thomas" true
let team = createTeam "Indiana Pacers" coach record
replaceCoach team newCoach |> printfn "%A"

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

//----------------------------------------------------------------------------------------------------

// Exo 4 :

let lastWeek = [|0; 2; 5; 3; 7; 8; 4|]

let visitesHier(lastWeek: int[]): int =
    lastWeek[lastWeek.Length - 2]

let total(customers: int[]): int =
    Array.sum(customers)

let joursSansVisite(jours: int[]): bool =
    jours |> Array.exists(fun single -> single = 0)

let incrementTodaysCount(jours: int[]): int[] =
    jours[jours.Length - 1] <- jours[jours.Length - 1] + 1
    jours

let oddWeek(week: int[]): bool =
    match week with
    | y when (y[1] = 0 && y[3] = 0 && y[5] = 0) -> true
    | y when (y[1] = 10 && y[3] = 10 && y[5] = 10) -> true
    | y when (y[0] = 5 && y[2] = 5 && y[4] = 5 && y[6] = 5) -> true
    | _ -> false
        
visitesHier [|3; 5; 0; 7; 4; 1|] |> printfn "%i"
total [|3; 5; 0; 7; 4; 1|] |> printfn "%i"
joursSansVisite [|3; 5; 0; 7; 4; 1|] |> printfn "%b"
let birdCount = [|3; 5; 0; 7; 4; 1|]
incrementTodaysCount birdCount |> printfn "%A"
oddWeek [|1; 0; 5; 0; 12; 0; 2|] |> printfn "%A"
oddWeek [|5; 0; 5; 12; 5; 3; 5|] |> printfn "%A"


// Partie B :

let newList(): List<_> =
    []

let existingList(): List<String> =
    ["F#" ; "Clojure" ; "Haskell"]

let addLanguage(newLanguage: String)(list: List<String>): List<String> =
    newLanguage::list

let countLanguages(list: List<String>): int =
    list.Length

let reverseList(theList: List<String>): List<String> =
    theList |> List.rev

let excitingList(theList: List<String>): bool =
    match theList with
    | y when (y[0] = "F#") -> true
    | y when (y[1] = "F#" && y.Length <= 3) -> true
    | _ -> false

addLanguage "TypeScript" ["JavaScript" ; "CoffeeScript"] |> printfn "%A"
countLanguages ["C#" ; "Racket" ; "Rust" ; "Ruby"] |> printfn "%A"
reverseList ["Prolog" ; "C" ; "Idris" ; "Assembly"] |> printfn "%A"
excitingList ["Nim" ; "F#"] |> printfn "%A"