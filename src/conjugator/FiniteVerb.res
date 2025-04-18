open Infixes
/*
    Slot 1 Modal prefix (ḫa), negative particle, prefix of anteriority, stem (in imperative forms)
    Slot 2 Finite-marker prefix, modal prefixes (all the other)
    Slot 3 Coordinator prefix
    Slot 4 Ventive (cislocative) prefix
    Slot 5 Middle prefix or the 3.SG.NH pronominal prefix /b/ (specifying the person,
    gender and number of the first in the sequence of adverbial prefixes)
    Slot 6 Initial Pronominal Prefix (= IPP) (specifying the person, gender and number
    of the first in the sequence of adverbial prefixes)
    Slot 7 Adverbial I: dative prefix
    Slot 8 Adverbial II: comitative prefix
    Slot 9 Adverbial III: ablative or terminative prefix
    Slot 10 Adverbial IV: locative1, locative2, or locative3 prefix
    Slot 11 Final Pronominal Prefix (= FPP) (referring to A or P, depending on the tense,
    or locative3)
    Slot 12 stem
    Slot 13 present-future marker (in intransitive verbs)
    Slot 14 pronominal suffix (referring A, S, or P depending on the tense)
    Slot 15 subordinator
*/

type t = VerbShared.verbForm;

type ipfvStem =
    | Reduplicate
    | EdMarker
    | Other(string)

let new = (stem: string): t => {
    {
        stem,
        is_perfective: false,
        is_transitive: false,
        obliqueObject: None,
        firstPrefix: None,
        preformative: None,
        coordinator: false,
        ventive: false,
        middlePrefix: false,
        initialPersonPrefix: None,
        indirectObjectPrefix: None,
        comitative: false,
        adverbial: None,
        locative: None,
        finalPersonPrefix: None,
        edMarker: false,
        finalPersonSuffix: None,
        subordinator: false
    }
}

let isPerfective = (verb: t): t => {
    {...verb, is_perfective: true}
}

let isImperfective = (verb: t, ipfvStem: option<ipfvStem>): t => {
    switch (ipfvStem) {
        | Some(Reduplicate) => {...verb, stem: verb.stem ++ "-" ++ verb.stem, is_perfective: false, edMarker: false}
        | Some(EdMarker) => {...verb, is_perfective: false, edMarker: true}
        | Some(Other(stem)) => {...verb, is_perfective: false, stem}
        | None => {...verb, is_perfective: false}
    }
}

let isTransitive = (verb: t): t => {
    {...verb, is_transitive: true}
}

let isIntransitive = (verb: t): t => {
    {...verb, is_transitive: false}
}

let setNegative = (verb: t): t => {
    {...verb, firstPrefix: Some(FirstPrefix.Negative)}
}
let resetNegative = (verb: t): t => {
    {...verb, firstPrefix: None}
}

let setModal = (verb: t): t => {
    {...verb, firstPrefix: Some(FirstPrefix.Modal)}
}
let resetModal = (verb: t): t => {
    {...verb, firstPrefix: None}
}

let setPreformative = (verb: t, preformative: preformative): t => {
    {...verb, preformative: Some(preformative)}
}
let resetPreformative = (verb: t): t => {
    {...verb, preformative: None}
}

let setCoordinator = (verb: t): t => {
    {...verb, coordinator: true}
}
let resetCoordinator = (verb: t): t => {
    {...verb, coordinator: false}
}

let setVentive = (verb: t): t => {
    {...verb, ventive: true}
}
let resetVentive = (verb: t): t => {
    {...verb, ventive: false}
}

let setTerminative = (verb: t, person: option<personParam>): t => {
    let ipp: option<initialPersonPrefix> = switch person {
        | Some(FirstSing) => Some(FirstSing)
        | Some(SecondSing) => Some(SecondSing)
        | Some(ThirdSingHuman) => Some(ThirdSingHuman)
        | Some(ThirdSingNonHuman) => Some(ThirdSingNonHuman)
        | Some(FirstPlur) => Some(FirstPlur)
        | Some(SecondPlur) => Some(SecondPlur)
        | Some(ThirdPlurHuman) => Some(ThirdPlurHuman)
        | Some(ThirdPlurNonHuman) => Some(ThirdPlurNonHuman)
        | None => None
    }
    {...verb, adverbial: Some(Terminative), initialPersonPrefix: ipp}
}
let resetTerminative = (verb: t): t => {
    {...verb, adverbial: None, initialPersonPrefix: None}
}

let setAblative = (verb: t, person: option<personParam>): t => {
    let ipp: option<initialPersonPrefix> = switch person {
        | Some(FirstSing) => Some(FirstSing)
        | Some(SecondSing) => Some(SecondSing)
        | Some(ThirdSingHuman) => Some(ThirdSingHuman)
        | Some(ThirdSingNonHuman) => Some(ThirdSingNonHuman)
        | Some(FirstPlur) => Some(FirstPlur)
        | Some(SecondPlur) => Some(SecondPlur)
        | Some(ThirdPlurHuman) => Some(ThirdPlurHuman)
        | Some(ThirdPlurNonHuman) => Some(ThirdPlurNonHuman)
        | None => None
    }
    {...verb, adverbial: Some(Ablative), initialPersonPrefix: ipp}
}
let resetAblative = (verb: t): t => {
    {...verb, adverbial: None, initialPersonPrefix: None}
}

let setMiddlePrefix = (verb: t): t => {
    {...verb, middlePrefix: true}
}
let resetMiddlePrefix = (verb: t): t => {
    {...verb, middlePrefix: false}
}

let setInitialPersonPrefix = (verb: t, ipp: personParam): t => {
    let prefix: initialPersonPrefix = switch ipp {
        | FirstSing => FirstSing
        | SecondSing => SecondSing
        | ThirdSingHuman => ThirdSingHuman
        | ThirdSingNonHuman => ThirdSingNonHuman
        | FirstPlur => FirstPlur
        | SecondPlur => SecondPlur
        | ThirdPlurHuman => ThirdPlurHuman
        | ThirdPlurNonHuman => ThirdPlurNonHuman
    }
    {...verb, initialPersonPrefix: Some(prefix)}
}

let setFinalPersonPrefix = (verb: t, pp: personParam): t => {
    let prefix: finalPersonPrefix = switch pp {
        | FirstSing => FirstSing
        | SecondSing => SecondSing
        | ThirdSingHuman => ThirdSingHuman
        | ThirdSingNonHuman => ThirdSingNonHuman
        | _ => %todo("TODO: add final person prefixes for the plural")
    }
    {...verb, finalPersonPrefix: Some(prefix)}
}

let setIndirectObject = (verb: t, ipp: personParam): t => {
    let prefix: indirectObjectPrefix = switch ipp {
        | FirstSing => FirstSing
        | SecondSing => SecondSing
        | ThirdSingHuman => ThirdSingHuman
        | ThirdSingNonHuman => ThirdSingNonHuman
        | FirstPlur => FirstPlur
        | SecondPlur => SecondPlur
        | ThirdPlurHuman => ThirdPlurHuman
        | ThirdPlurNonHuman => ThirdPlurNonHuman
    }
    {...verb, indirectObjectPrefix: Some(prefix)}
}

let setComitative = (verb: t, person: option<personParam>): t => {
    let ipp: option<initialPersonPrefix> = switch person {
        | Some(FirstSing) => Some(FirstSing)
        | Some(SecondSing) => Some(SecondSing)
        | Some(ThirdSingHuman) => Some(ThirdSingHuman)
        | Some(ThirdSingNonHuman) => Some(ThirdSingNonHuman)
        | Some(FirstPlur) => Some(FirstPlur)
        | Some(SecondPlur) => Some(SecondPlur)
        | Some(ThirdPlurHuman) => Some(ThirdPlurHuman)
        | Some(ThirdPlurNonHuman) => Some(ThirdPlurNonHuman)
        | None => None
    }
    {...verb, comitative: true, initialPersonPrefix: ipp}
}
let resetComitative = (verb: t): t => {
    {...verb, comitative: false, initialPersonPrefix: None}
}

let setLocativeIn = (verb: t, person: option<personParam>): t => {
    switch (person) {
        | Some(_) => {...verb, locative: Some(InWithInitialPerson)}
        | None => {...verb, locative: Some(InWithoutInitialPerson)}
    }
}

let setLocativeOn = (verb: t, person: option<personParam>): t => {
    switch (person) {
        | Some(_) => {...verb, locative: Some(OnWithInitialPerson)}
        | None => {...verb, locative: Some(OnWithoutInitialPerson)}
    }
}
let resetLocative = (verb: t): t => {
    {...verb, locative: None}
}

let setSubject = (verb: t, person: personParam): t => {
    if !verb.is_transitive || (verb.is_transitive && !verb.is_perfective) {
        let suffix: option<finalPersonSuffix> = switch person {
            | FirstSing => Some(FirstSing)
            | SecondSing => Some(SecondSing)
            | ThirdSingHuman => Some(ThirdSingHuman)
            | ThirdSingNonHuman => Some(ThirdSingNonHuman)
            | FirstPlur => Some(FirstPlur)
            | SecondPlur => Some(SecondPlur)
            | ThirdPlurHuman => Some(ThirdPlurHuman)
            | ThirdPlurNonHuman => Some(ThirdPlurNonHuman)
        }
        {...verb, finalPersonSuffix: suffix}
    } else {
        let prefix: option<finalPersonPrefix> = switch person {
            | FirstSing => Some(FirstSing)
            | SecondSing => Some(SecondSing)
            | ThirdSingHuman => Some(ThirdSingHuman)
            | ThirdSingNonHuman => Some(ThirdSingNonHuman)
            | _ => %todo("TODO: handle plural subjects in perfective verbs")
        }
        {...verb, finalPersonPrefix: prefix}
    }
}

let setObject = (verb: t, person: personParam): t => {
    if verb.is_transitive && verb.is_perfective {
        let suffix: option<finalPersonSuffix> = switch person {
            | FirstSing => Some(FirstSing)
            | SecondSing => Some(SecondSing)
            | ThirdSingHuman => Some(ThirdSingHuman)
            | ThirdSingNonHuman => Some(ThirdSingNonHuman)
            | FirstPlur => Some(FirstPlur)
            | SecondPlur => Some(SecondPlur)
            | ThirdPlurHuman => Some(ThirdPlurHuman)
            | ThirdPlurNonHuman => Some(ThirdPlurNonHuman)
        }
        {...verb, finalPersonSuffix: suffix}
    } else if verb.is_transitive && !verb.is_perfective {
        let prefix: option<finalPersonPrefix> = switch person {
            | FirstSing => Some(FirstSing)
            | SecondSing => Some(SecondSing)
            | ThirdSingHuman => Some(ThirdSingHuman)
            | ThirdSingNonHuman => Some(ThirdSingNonHuman)
            | _ => %todo("TODO: handle plural objects in imperfective verbs")
        }
        {...verb, finalPersonPrefix: prefix}
    } else {
        // cannot set the object of an intransitive verb
        // TODO: may be worth throwing an error here
        // but it will mess with the verb building with pipes
        verb
    }
}

let setObliqueObject = (verb: t, person: personParam): t => {
    // 18.2.1 Since a finite verbal form can contain at most one final person-prefix, the oblique
    // object cannot be expressed with a final person-prefix if a verbal form already contains such a
    // prefix for expressing the transitive subject or the direct object. If that is the case, that is to say,
    // if a finite verbal form contains a final person-prefix expressing the transitive subject or the
    // direct object, an oblique object is expressed with an initial person-prefix.
    switch verb.finalPersonPrefix {
        | Some(_) => 
            { ...verb, obliqueObject: person -> Utils.fromPersonToIPP -> VerbShared.InitialPersonPrefix }
        | None => 
            { ...verb, obliqueObject:  person -> Utils.fromPersonToFPP -> VerbShared.FinalPersonPrefix }
    }
}

let setEdMarker = (verb: t): t => {
    {...verb, edMarker: true}
}
let resetEdMarker = (verb: t): t => {
    {...verb, edMarker: false}
}

let print = (verb: t): result<string, string> => VerbOutput.print(verb)