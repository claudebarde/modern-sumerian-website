@val external toNumber: (string) => int = "Number"
@module external cuneiformCodePoints: JSON.t = "./cuneiform_code_points.json"

type codePointData = {
    codepoint: string,
    name: string,
}
type codePoints = array<codePointData>
type jsonCuneiformData = {
    codepoints: array<codePointData>,
}
@scope("JSON") @val
external parseCuneiformData: string => jsonCuneiformData = "parse"

type cuneiformData = (string, string) // (Unicode code point, sound)

let fallbackDict = Dict.fromArray([("eme", "0x12174"), ("ĝir15", "0x120A0"), ("ul", "0x12109"), ("la", "0x121B7"), ("im", "0x1214E"), ("ĝen", "0x1207A"), ("ʔak", "0x1201D"), ("tuku", "0x12307"), ("niĝ", "0x120FB"), ("šum", "0x122E7"), ("naĝ", "0x12158")]) 

let searchCuneiforms = (words: array<string>): array<(string, option<string>)> => {
    let cuneiformData = cuneiformCodePoints->JSON.stringify->parseCuneiformData
    Array.map(words, (word) => {
        switch Array.find(cuneiformData.codepoints, (item) => item.name == word->String.toUpperCase) {
        | Some(codePointData) => (word, Some(codePointData.codepoint))
        | None => {
            // checks if the word is in the fallback dictionary
            switch fallbackDict->Dict.get(word) {
            | Some(code) => (word, Some(code))
            | None => (word, None)
            }
        }
        }
    })
}

let searchCuneiformsStartWith = (word: string): array<(string, string)> => {
    let cuneiformData = cuneiformCodePoints->JSON.stringify->parseCuneiformData
    // looks in the official dictionary
    let resFromDict = cuneiformData.codepoints
    ->Array.filter((item) => item.name->String.startsWith(word->String.toUpperCase))
    ->Array.map((item) => (item.name, item.codepoint))
    // looks in the fallback dictionary
    let resFromFallback = fallbackDict
    ->Dict.toArray
    ->Array.filter(((dictWord, _)) => dictWord->String.startsWith(word->String.toLowerCase))
    // merges the two results
    [...resFromDict, ...resFromFallback]
}

let displayCuneiforms = (words: array<string>): array<cuneiformData> => {
    words->searchCuneiforms->Array.map(((word, codePoint)) => {
        switch codePoint {
        | Some(code) => (code->toNumber->String.fromCodePoint, word)
        | None => (word, word)
        }
    })  
}

let parseVerbSyllables = (word: string, stem: string): array<string> => {
    let regex = %re("/[^aeiu]*[aeiu]+(?:[^aeiu]*$|[^aeiu](?=[^aeiu]))?/gi")
    let vowelsRegex = %re("/(?<=[aeiu])(?=[aeiu])/gi")
    let cvcRegex = %re("/([^aeiu])([aeiu])([^aeiu])/gi")
    let syllables = word->String.split(stem)
    let (beforeStem, afterStem): (array<string>, array<string>) = switch (syllables[0], syllables[1]) {
    | (Some(before), None) => {
        let resBefore = switch String.match(before, regex) {
        | Some(matches) => matches->Array.map(match => Option.getOr(match, ""))
        | None => []
        }

        (resBefore, [])
    }
    | (None, Some(after)) => {
        let resAfter = switch String.match(after, regex) {
        | Some(matches) => matches->Array.map(match => Option.getOr(match, ""))
        | None => []
        }

        ([], resAfter)
    }
    | (Some(before), Some(after)) => {
        let resBefore = switch String.match(before, regex) {
        | Some(matches) => {
            matches->Array.map(match => Option.getOr(match, ""))
        }
        | None => []
        }

        let resAfter = switch String.match(after, regex) {
        | Some(matches) => matches->Array.map(match => Option.getOr(match, ""))
        | None => []
        }

        (resBefore, resAfter)
    }
    | (None, None) => ([], [])
    }

    let formatting = (syllables: array<string>): array<string> => {
        let res = syllables
        // splits clusters of vowels
        ->Array.map(syll => syll->String.splitByRegExp(vowelsRegex))
        ->Array.flat
        ->Array.map(syll => Option.getOr(syll, ""))
        // splits CVC clusters
        ->Array.map(syll => switch syll->String.match(cvcRegex) {
        | Some(matches) => matches->Array.map(match => {
            let match = match->Option.getOr("")->String.split("")
            switch (match[0], match[1], match[2]) {
                | (Some(first), Some(middle), Some(last)) => [ first ++ middle, middle ++ last ]
                | _ => []
            }
        })->Array.flat
        | None => [syll]
        })
        ->Array.flat

        res
    }
    
    [...beforeStem->formatting, stem, ...afterStem]
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

@module external resultStyles: {..} = "./components/Conjugator.module.scss"
let buildResults = (verb: FiniteVerb.t): Jsx.element => {
    switch verb->FiniteVerb.print {
        | Ok({verb: conjugatedVerb, analysis}) => [
            <div className={resultStyles["verbResult"]} key="verbResults">
                <span style={fontSize: "1.2rem"} key="verbForm">
                    {conjugatedVerb->React.string}
                </span>
                <span key="cuneiforms">
                    {
                        conjugatedVerb
                        ->parseVerbSyllables(verb.stem)
                        ->displayCuneiforms
                        ->Array.mapWithIndex(((codePoint, word), i) => {
                            <CuneiformChar
                                key={codePoint ++ word ++ Int.toString(i)}
                                codePoint={codePoint}
                                pronunciation={word}
                            />
                        })
                        ->React.array
                    }
                </span>
            </div>,
            <table key="verbAnalysis">
                <tbody>
                    <tr>
                        {analysis->VerbAnalysis.output->Array.map(
                            ((output_type, _)) => {
                                <th key={output_type}>
                                    {
                                        switch output_type {
                                            | "middlePrefix" => "Middle Prefix"
                                            | "initialPersonPrefix" => "Initial Person Prefix"
                                            | "finalPersonPrefix" => "Final Person Prefix"
                                            | "edMarker" => "ED Marker"
                                            | "finalPersonSuffix" => "Final Person Suffix"
                                            | _ => `${output_type->String.charAt(0)->String.toUpperCase}${output_type->String.sliceToEnd(~start=1)->String.toLowerCase}`
                                        }->React.string
                                    }
                                </th>
                            },
                        )->React.array}
                    </tr>
                    <tr>
                        {analysis->VerbAnalysis.output->Array.mapWithIndex(
                            ((_, value), i) => {
                                <td key={value ++ Int.toString(i)}>
                                    {value->React.string}
                                </td>
                            },
                        )->React.array}
                    </tr>
                </tbody>
            </table>,
            <span key="cuneiformWarning" style={{fontSize: "0.6rem", fontStyle: "italic"}}>
                {"The cuneiforms are auto-generated and may not be historically accurate"->React.string}
            </span>
        ]->React.array
        | Error(err) => {err->React.string}
    }
}

module EpsdDict = {
    @module external epsdDict: JSON.t = "./epsd_links.json"

    type epsdData = {
        word: string,
        ref: string,
    }
    type t = array<epsdData>
    type defaultJsonImport = {
        default: t,
    }

    @scope("JSON") @val
    external parseEpsdDict: string => defaultJsonImport = "parse"

    let getEpsdLink = (word: string): option<string> => {
        let {default: dict} = epsdDict->JSON.stringify->parseEpsdDict
        let epsdDict = 
            dict->Array.map((item) => (item.word, item.ref))
            ->Dict.fromArray
        switch epsdDict->Dict.get(word) {
        | Some(ref) => Some(`https://oracc.museum.upenn.edu/epsd2/sux/${ref}`)
        | None => None
        }
    }
}