open Infixes;

let fromPersonToIPP = (person: personParam): initialPersonPrefix => {
    switch person {
        | FirstSing => FirstSing
        | SecondSing => SecondSing
        | ThirdSingHuman => ThirdSingHuman
        | ThirdSingNonHuman => ThirdSingNonHuman
        | FirstPlur => FirstPlur
        | SecondPlur => SecondPlur
        | ThirdPlurHuman => ThirdPlurHuman
        | ThirdPlurNonHuman => ThirdPlurNonHuman
    }
}

let fromPersonToFPP = (person: personParam): finalPersonPrefix => {
    switch person {
        | FirstSing => FirstSing
        | SecondSing => SecondSing
        | ThirdSingHuman => ThirdSingHuman
        | ThirdSingNonHuman => ThirdSingNonHuman
        | _ => %todo("TODO: handle plural subjects in perfective verbs")
    }
}