open Infixes

type obliqueObject =
    | FinalPersonPrefix(finalPersonPrefix)
    | InitialPersonPrefix(initialPersonPrefix)
    | None

type verbForm = {
    stem: string,
    is_perfective: bool,
    is_transitive: bool,
    obliqueObject: obliqueObject,
    firstPrefix: option<FirstPrefix.t>,
    preformative: option<preformative>,
    coordinator: bool,  // coordinator
    ventive: bool,      // ventive prefix
    middlePrefix: bool, // Middle prefix or the 3.SG.NH pronominal prefix
    initialPersonPrefix: option<initialPersonPrefix>,
    indirectObjectPrefix: option<indirectObjectPrefix>,
    comitative: bool,   // comitative prefix
    adverbial: option<adverbialPrefix>,
    locative: option<locativePrefix>,
    finalPersonPrefix: option<finalPersonPrefix>,
    edMarker: bool, // ed_marker
    finalPersonSuffix: option<finalPersonSuffix>,
    subordinator: bool, // subordinator
}