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