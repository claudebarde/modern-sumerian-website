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

let fallbackDict = Dict.fromArray([("eme", "0x12174"), ("ĝir15", "0x120A0"), ("ul", "0x12109"), ("la", "0x121B7"), ("im", "0x1214E"), ("ĝen", "0x1207A")]) 

/* let rec buildDictFromJson = (json: JSON.t): result<Dict.t<string>, string> => {
    switch json {
        | Array(arr) => 
            // Console.log2("array:", arr)
            let res = arr->Array.map((item) => buildDictFromJson(item))
            Console.log2("res:", res)
            // let reduce = res->Array.reduce(Ok(()), 
            //     (item, acc) => 
            //         switch item {
            //             | Ok(_) => acc
            //             | Error(err) => Error(err)
            //         }
            //     )
            // switch reduce {
            //     | Ok(_) => res->Array.map(item => Option.getUnsafe)->Dict.fromArray->Ok
            //     | Error(err) => Error(err)
            // }
            Error("Invalid JSON format for dictionary")
        | Object(obj) => {
            Console.log2("object:", obj)
            switch obj->Dict.get("codepoints") {
                | Some(arr) => buildDictFromJson(arr)
                | None => {
                    switch (obj->Dict.get("codepoint")) {
                        | Some(codepoint) => {
                            switch (codepoint->Dict.get("codepoint"), codepoint->Dict.get("name")) {
                                | (Some(codepoint), Some(name)) => switch (codepoint, name) {
                                    | (String(codepoint), String(name)) => {
                                        let code = codepoint->toNumber->String.fromCodePoint
                                        let dict = Dict.fromArray([((code, name))])
                                        Ok(dict)
                                    }
                                    | _ => Error("Invalid JSON format for dictionary")
                                }
                                | _ => Error("Invalid JSON format for dictionary")
                            }
                        }
                        | None => Error("Invalid JSON format for dictionary")
                    }
                }
            }
        }
        | _ => Error("Invalid JSON format for dictionary")
    }
} */

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

let displayCuneiforms = (words: array<string>): array<string> => {
    words->searchCuneiforms->Array.map(((word, codePoint)) => {
        switch codePoint {
        | Some(code) => code->toNumber->String.fromCodePoint
        | None => word
        }
    })  
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

let buildResults = (verb: FiniteVerb.t): Jsx.element => {
    switch verb->FiniteVerb.print {
        | Ok({verb, analysis}) => [
            <span key="verbForm">
                {verb->React.string}
            </span>,
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