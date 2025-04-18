let dict = Dict.fromArray([("eme", 0x12174), ("ĝir15", 0x120A0), ("ul", 0x12109), ("la", 0x121B7), ("im", 0x1214E), ("ĝen", 0x1207A)]) 

let displayCuneiforms = (words: array<string>): array<string> => {
    Js.Array2.map(
        words,
        (word) => {
            switch dict->Dict.get(word) {
            | Some(code) => code->Js.String.fromCodePoint
            | None => word
            }
        },
    )
}

let pronounToPersonParam = (pronoun: string): option<Infixes.personParam> => {
    switch pronoun {
    | "first-sing" => Some(Infixes.FirstSing)
    | "second-sing" => Some(Infixes.SecondSing)
    | "third-sing-human" => Some(Infixes.ThirdSingHuman)
    | "third-sing-nonhuman" => Some(Infixes.ThirdSingNonHuman)
    | "first-plur" => Some(Infixes.FirstPlur)
    | "second-plur" => Some(Infixes.SecondPlur)
    | "third-plur-human" => Some(Infixes.ThirdPlurHuman)
    | "third-plur-nonhuman" => Some(Infixes.ThirdPlurNonHuman)
    | _ => None
    }
}