open Infixes

type t = {
    // Slot 1 Modal prefix (ḫa), negative particle, prefix of anteriority, stem (in imperative forms)
    slot1: option<(FirstPrefix.t, string)>,
    // Slot 2 Finite-marker prefix, modal prefixes (all the other)
    slot2: option<string>,
    // Slot 3 Coordinator prefix
    slot3: option<string>,
    // Slot 4 Ventive (cislocative) prefix
    slot4: option<string>,
    // Slot 5 Middle prefix or the 3.SG.NH pronominal prefix /b/ (specifying the person,
    // gender and number of the first in the sequence of adverbial prefixes)
    slot5: option<string>,
    // Slot 6 Initial Pronominal Prefix (= IPP) (specifying the person, gender and number
    // of the first in the sequence of adverbial prefixes)
    slot6: option<(initialPersonPrefix, string)>,
    // Slot 7 Adverbial I: dative prefix
    slot7: option<string>,
    // Slot 8 Adverbial II: comitative prefix
    slot8: option<string>,
    // Slot 9 Adverbial III: ablative or terminative prefix
    slot9: option<(adverbialPrefix, string)>,
    // Slot 10 Adverbial IV: locative1, locative2, or locative3 prefix
    slot10: option<string>,
    // Slot 11 Final Pronominal Prefix (= FPP) (referring to A or P, depending on the tense,
    // or locative3)
    slot11: option<(finalPersonPrefix, string)>,
    // Slot 12 stem
    slot12: option<string>,
    // Slot 13 present-future marker (in intransitive verbs)
    slot13: option<string>,
    // Slot 14 pronominal suffix (referring A, S, or P depending on the tense)
    slot14: option<(finalPersonSuffix, string)>,
    // Slot 15 subordinator
    slot15: option<string>,
}

let new = (): t => {
    slot1: None,
    slot2: None,
    slot3: None,
    slot4: None,
    slot5: None,
    slot6: None,
    slot7: None,
    slot8: None,
    slot9: None,
    slot10: None,
    slot11: None,
    slot12: None,
    slot13: None,
    slot14: None,
    slot15: None
}

let rec analyse = (verbArr: array<string>, verbRec:VerbShared.verbForm, verb: t, start: int): t => {
    if start >= 15 {
        verb
    } else {
        switch (start, verbArr[start]) {
        | (0, Some(prefix)) => {
            if String.length(prefix) > 0 {
                let firstPrefix = Option.getExn(verbRec.firstPrefix, ~message=`First prefix is expected to be set in verb: ${verbRec.stem}`)
                let verb = { ...verb, slot1: Some((firstPrefix, prefix)) }
                analyse(verbArr, verbRec, verb, start + 1)
            } else {
                analyse(verbArr, verbRec, verb, start + 1)
            } 
        }
        | (1, Some(prefix)) => {
            if String.length(prefix) > 0 {
                let verb = { ...verb, slot2: Some(prefix) }
                analyse(verbArr, verbRec, verb, start + 1)
            } else {
                analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (2, Some(prefix)) => {
            if String.length(prefix) > 0 {
                let verb = { ...verb, slot3: Some(prefix) }
                analyse(verbArr, verbRec, verb, start + 1)
            } else {
                analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (3, Some(prefix)) => {
            if String.length(prefix) > 0 {
                let verb = { ...verb, slot4: Some(prefix) }
                analyse(verbArr, verbRec, verb, start + 1)
            } else {
                analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (4, Some(prefix)) => {
            if String.length(prefix) > 0 {
                let verb = { ...verb, slot5: Some(prefix) }
                analyse(verbArr, verbRec, verb, start + 1)
            } else {
                analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (5, Some(prefix)) => {
            switch verbRec.initialPersonPrefix {
            | Some(ipp) => {
                if String.length(prefix) > 0 {
                    let verb = { ...verb, slot6: Some((ipp, prefix)) }
                    analyse(verbArr, verbRec, verb, start + 1)
                } else {
                    analyse(verbArr, verbRec, verb, start + 1)
                }
            }
            | _ => analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (6, Some(prefix)) => {
            if String.length(prefix) > 0 {
                let verb = { ...verb, slot7: Some(prefix) }
                analyse(verbArr, verbRec, verb, start + 1)
            } else {
                analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (7, Some(prefix)) => {
            if String.length(prefix) > 0 {
                let verb = { ...verb, slot8: Some(prefix) }
                analyse(verbArr, verbRec, verb, start + 1)
            } else {
                analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (8, Some(prefix)) => {
            if String.length(prefix) > 0 {
                let adverbial = Option.getExn(verbRec.adverbial, ~message=`Adverbial is expected to be set in verb: ${verbRec.stem}`)
                let verb = { ...verb, slot9: Some((adverbial, prefix)) }
                analyse(verbArr, verbRec, verb, start + 1)
            } else {
                analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (9, Some(prefix)) => {
            if String.length(prefix) > 0 {
                let verb = { ...verb, slot10: Some(prefix) }
                analyse(verbArr, verbRec, verb, start + 1)
            } else {
                analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (10, Some(prefix)) => {
            switch verbRec.finalPersonPrefix {
            | Some(fpp) => {
                if String.length(prefix) > 0 {
                    let verb = { ...verb, slot11: Some((fpp, prefix)) }
                    analyse(verbArr, verbRec, verb, start + 1)
                } else {
                    analyse(verbArr, verbRec, verb, start + 1)
                }
            }
            | _ => analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (11, Some(prefix)) => {
            if String.length(prefix) > 0 {
                let verb = { ...verb, slot12: Some(prefix) }
                analyse(verbArr, verbRec, verb, start + 1)
            } else {
                analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (12, Some(prefix)) => {
            if String.length(prefix) > 0 {
                let verb = { ...verb, slot13: Some(prefix) }
                analyse(verbArr, verbRec, verb, start + 1)
            } else {
                analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (13, Some(prefix)) => {
            switch verbRec.finalPersonSuffix {
            | Some(fpp) => {
                if String.length(prefix) > 0 {
                    let verb = { ...verb, slot14: Some((fpp, prefix)) }
                    analyse(verbArr, verbRec, verb, start + 1)
                } else {
                    analyse(verbArr, verbRec, verb, start + 1)
                }
            }
            | _ => analyse(verbArr, verbRec, verb, start + 1)
            }
        }
        | (14, Some(prefix)) => {
            if String.length(prefix) > 0 {
                { ...verb, slot15: Some(prefix) }
            } else {
                verb
            }
        }
        | _ => verb
        }
    }
}

let output = (verb: t): array<(string, string)> => {
    let slot1 = switch verb.slot1 {
        | None => ("", "")
        | Some((prefix_type, prefix)) => switch prefix_type {
            | FirstPrefix.Modal => ("modal", prefix)
            | FirstPrefix.Negative => ("negative", prefix)
            | FirstPrefix.NegativeNan => ("negativeNan", prefix)
        }
    };
    let slot2 = switch verb.slot2 {
        | None => ("", "")
        | Some(prefix) => ("preformative", prefix)
    };
    let slot3 = switch verb.slot3 {
        | None => ("", "")
        | Some(prefix) => ("coordinator", prefix)
    };
    let slot4 = switch verb.slot4 {
        | None => ("", "")
        | Some(prefix) => ("ventive", prefix)
    };
    let slot5 = switch verb.slot5 {
        | None => ("", "")
        | Some(prefix) => ("middlePrefix", prefix)
    };
    let slot6 = switch verb.slot6 {
        | None => ("", "")
        | Some((_, prefix)) => ("initialPersonPrefix", prefix)
    };
    let slot7 = switch verb.slot7 {
        | None => ("", "")
        | Some(prefix) => ("dative", prefix)
    };
    let slot8 = switch verb.slot8 {
        | None => ("", "")
        | Some(prefix) => ("comitative", prefix)
    };
    let slot9 = switch verb.slot9 {
        | None => ("", "")
        | Some((prefix_type, prefix)) => switch prefix_type {
            | Infixes.Ablative => ("ablative", prefix)
            | Infixes.Terminative => ("terminative", prefix)
        }
    };
    let slot10 = switch verb.slot10 {
        | None => ("", "")
        | Some(prefix) => ("locative", prefix)
    };
    let slot11 = switch verb.slot11 {
        | None => ("", "")
        | Some((_, prefix)) => ("finalPersonPrefix", prefix)
    };
    let slot12 = switch verb.slot12 {
        | None => ("", "")
        | Some(prefix) => ("stem", prefix)
    };
    let slot13 = switch verb.slot13 {
        | None => ("", "")
        | Some(prefix) => ("edMarker", prefix)
    };
    let slot14 = switch verb.slot14 {
        | None => ("", "")
        | Some((_, prefix)) => ("finalPersonSuffix", prefix)
    };
    let slot15 = switch verb.slot15 {
        | None => ("", "")
        | Some(prefix) => ("subordinator", prefix)
    };

    [slot1, slot2, slot3, slot4, slot5, slot6, slot7, slot8, slot9, slot10, slot11, slot12, slot13, slot14, slot15]
    ->Array.filter(((x, y)) => String.length(x) > 0 || String.length(y) > 0)
}

let print = (verb: t): string => {
    let slot1 = switch verb.slot1 {
        | None => ""
        | Some((_, prefix)) => prefix
    };
    let slot2 = Option.getOr(verb.slot2, "");
    let slot3 = Option.getOr(verb.slot3, "");
    let slot4 = Option.getOr(verb.slot4, "");
    let slot5 = Option.getOr(verb.slot5, "");
    let slot6 = switch verb.slot6 {
        | None => ""
        | Some((_, prefix)) => prefix
    };
    let slot7 = Option.getOr(verb.slot7, "");
    let slot8 = Option.getOr(verb.slot8, "");
    let slot9 = switch verb.slot9 {
        | None => ""
        | Some((_, prefix)) => prefix
    };
    let slot10 = Option.getOr(verb.slot10, "");
    let slot11 = switch verb.slot11 {
        | None => ""
        | Some((_, prefix)) => prefix
    };
    let slot12 = Option.getOr(verb.slot12, "");
    let slot13 = Option.getOr(verb.slot13, "");
    let slot14 = switch verb.slot14 {
        | None => ""
        | Some((_, prefix)) => prefix
    };
    let slot15 = Option.getOr(verb.slot15, "");

    [slot1, slot2, slot3, slot4, slot5, slot6, slot7, slot8, slot9, slot10, slot11, slot12, slot13, slot14, slot15]
    ->Array.filter((x) => String.length(x) > 0)
    ->Array.join(" / ")
}