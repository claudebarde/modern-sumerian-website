type personParam =
    | FirstSing
    | SecondSing
    | ThirdSingHuman
    | ThirdSingNonHuman
    | FirstPlur
    | SecondPlur
    | ThirdPlurHuman
    | ThirdPlurNonHuman
    
module FirstPrefix = {
    type t =
    | Modal
    | Negative
    | NegativeNan
}

type preformative =
    | A
    | I
    | U

type initialPersonPrefix =
    | FirstSing
    | SecondSing
    | ThirdSingHuman
    | ThirdSingNonHuman
    | FirstPlur
    | SecondPlur
    | ThirdPlurHuman
    | ThirdPlurNonHuman

type indirectObjectPrefix =
    | FirstSing
    | SecondSing
    | ThirdSingHuman
    | ThirdSingNonHuman
    | FirstPlur
    | SecondPlur
    | ThirdPlurHuman
    | ThirdPlurNonHuman

type finalPersonPrefix =
    | FirstSing
    | SecondSing
    | ThirdSingHuman
    | ThirdSingNonHuman

type finalPersonSuffix =
    | FirstSing
    | SecondSing
    | ThirdSingHuman
    | ThirdSingNonHuman
    | FirstPlur
    | SecondPlur
    | ThirdPlurHuman
    | ThirdPlurNonHuman

type adverbialPrefix =
    | Ablative
    | Terminative

type locativePrefix =
    | InWithInitialPerson
    | InWithoutInitialPerson
    | OnWithInitialPerson
    | OnWithoutInitialPerson