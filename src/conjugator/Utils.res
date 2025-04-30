open Infixes;

let preformativePos = 1
let coordinatorPos = 2
let firstPrefixPos = 0
let ventivePos = 3
let middlePrefixPos = 4
let initialPersonPrefixPos = 5
let indirectObjectPrefixPos = 6
let comitativePos = 7
let adverbialPos = 8
let locativePos = 9
let finalPersonPrefixPos = 10
let stemPos = 11;
let edMarkerPos = 12
let finalPersonSuffixPos = 13
let subordinatorPos = 14

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

type markerName = 
    | FirstPrefix
    | Preformative
    | Coordinator
    | Ventive
    | MiddlePrefix
    | InitialPersonPrefix
    | IndirectObjectPrefix
    | Comitative
    | Adverbial
    | Locative
    | FinalPersonPrefix
    | Stem
    | EdMarker
    | FinalPersonSuffix
    | Subordinator

let markerByPos = (pos: int): option<markerName> => {
    switch pos {
        | 0 => Some(FirstPrefix)
        | 1 => Some(Preformative)
        | 2 => Some(Coordinator)
        | 3 => Some(Ventive)
        | 4 => Some(MiddlePrefix)
        | 5 => Some(InitialPersonPrefix)
        | 6 => Some(IndirectObjectPrefix)
        | 7 => Some(Comitative)
        | 8 => Some(Adverbial)
        | 9 => Some(Locative)
        | 10 => Some(FinalPersonPrefix)
        | 11 => Some(Stem)
        | 12 => Some(EdMarker)
        | 13 => Some(FinalPersonSuffix)
        | 14 => Some(Subordinator)
        | _ => None
    }
}

let markerToPos = (marker: markerName): int => {
    switch marker {
        | FirstPrefix => firstPrefixPos
        | Preformative => preformativePos
        | Coordinator => coordinatorPos
        | Ventive => ventivePos
        | MiddlePrefix => middlePrefixPos
        | InitialPersonPrefix => initialPersonPrefixPos
        | IndirectObjectPrefix => indirectObjectPrefixPos
        | Comitative => comitativePos
        | Adverbial => adverbialPos
        | Locative => locativePos
        | FinalPersonPrefix => finalPersonPrefixPos
        | Stem => stemPos
        | EdMarker => edMarkerPos
        | FinalPersonSuffix => finalPersonSuffixPos
        | Subordinator => subordinatorPos
    }
}

let rec findPreviousMorpheme = (arr: VerbShared.verbOutput, pos: int): option<(string, markerName)> => {
    if (pos < 1) {
        None
    } else {
        switch arr[pos - 1] {
            | Some(morph) if String.length(morph) > 0 => {
                switch markerByPos(pos - 1) {
                    | Some(marker) => Some((morph, marker))
                    | None => None
                }
            }
            | _ => findPreviousMorpheme(arr, pos - 1)
        }
    }
}

let rec findNextMorpheme = (arr: VerbShared.verbOutput, pos: int): option<(string, markerName)> => {
    if (pos > 14) {
        None
    } else {
        switch arr[pos + 1] {
            | Some(morph) if String.length(morph) > 0 => {
                switch markerByPos(pos + 1) {
                    | Some(marker) => Some((morph, marker))
                    | None => None
                }
            }
            | _ => findNextMorpheme(arr, pos + 1)
        }
    }
}

let startsWithVowel = (str: string): bool => {
    let firstChar = str -> Js.String2.get(0);
    switch firstChar {
        | "a" | "á" | "e" | "è" | "i" | "o" | "u" => true
        | _ => false
    }
}

let endsWithVowel = (str: string): bool => {
    let lastChar = str -> Js.String2.get(Js.String2.length(str) - 1);
    switch lastChar {
        | "a" | "á" | "e" | "è" | "i" | "o" | "u" => true
        | _ => false
    }
}

let startsWithConsonant = (str: string): bool => {
    let firstChar = str -> Js.String2.get(0);
    switch firstChar {
        | "b" | "d" | "h" | "ḫ" | "g" | "k" | "l" | "m" | "n" | "p" | "r" | "s" | "š" 
        | "t" | "w" | "z" => true
        | _ => false
    }
}

let removeFirstChar = (str: string): string => {
    if (String.length(str) > 0) {
        str -> Js.String2.slice(~from=1, ~to_=Js.String2.length(str))
    } else {
        str
    }
}

let consonantVowelSequence = (str: string): string => {
    str 
    -> Js.String2.split("")
    -> Js.Array2.map((ch) => {
        switch ch {
            | "a" | "á" | "e" | "è" | "i" | "o" | "u" => "V"
            | _ => "C"
        }
    })
    ->Js.Array2.joinWith("")
}