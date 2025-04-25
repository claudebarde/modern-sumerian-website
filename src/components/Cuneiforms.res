@module external styles: {..} = "./Cuneiforms.module.scss"

@react.component
let make = () => {
    let (input, setInput) = React.useState(() => "");
    let (sumerianOutput, setSumerianOutput) = React.useState(() => None);
    let (cuneiformOutput, setCuneiformOutput) = React.useState(() => None);
    let (cuneiformPreview, setCuneiformPreview) = React.useState(() => None);
    let (cuneiformSuggestions, setCuneiformSuggestions) = React.useState(() => []);

    let processInput = (input: string, enterPressed: bool) => {
        // makes replacements in the input for foreign characters
        let replacedInput = input
            ->String.replaceAll("gj", "ĝ")
            ->String.replaceAll("sj", "š")
            ->String.replaceAll("hj", "ḫ")

        setInput(_ => replacedInput);
        // search for cuneiforms that start with the input
        if String.length(replacedInput) > 0 {
            let cuneiforms = WebUtils.searchCuneiformsStartWith(replacedInput);
            setCuneiformSuggestions(_ => cuneiforms->Array.slice(~start=0, ~end=5));
        } else {
            setCuneiformSuggestions(_ => []);
        }
        // generates the cuneiforms preview
        let cuneiformsPreview = WebUtils.searchCuneiforms([replacedInput]);
        switch cuneiformsPreview {
            | [res] => setCuneiformPreview(_ => Some(res))
            | _ => setCuneiformPreview(_ => None)
        }

        if (enterPressed) {
            // adds the input to the output
            setSumerianOutput(prev => {
                switch prev {
                    | Some(prevInput) => Some(prevInput ++ " " ++ replacedInput)
                    | None => Some(replacedInput)
                }
            });
            // resets the input field
            setInput(_ => "");
            // resets the cuneiforms suggestions
            setCuneiformSuggestions(_ => []);
            // TODO: generates the cuneiforms
            setCuneiformOutput(_ => Some("niĝ-na-me"))
        }
    }

    <div className={styles["cuneiforms"]}>
        <input
            type_="text"
            placeholder="Enter Sumerian words here"
            value={input}
            onChange={(ev: JsxEvent.Form.t) => {
                let target = JsxEvent.Form.target(ev)
                let value: string = target["value"]
                processInput(value, false)
            }}
            onKeyDown={(ev: JsxEventU.Keyboard.t) => {
                if (JsxEventU.Keyboard.key(ev) == "Enter") {
                    processInput(input, true)
                }
            }}
        />
        <div className={styles["cuneiformsSuggestions"]}>
            {
                cuneiformSuggestions
                ->Array.mapWithIndex(((cuneiform, codePoint), i) => {
                    <div key={cuneiform ++ Int.toString(i)}>
                        <span className="cuneiforms" style={fontSize: "1.3rem"}>
                            {codePoint->WebUtils.toNumber->String.fromCodePoint->React.string}
                        </span>
                        <span>
                            {cuneiform->React.string}
                        </span>
                    </div>
                })
                ->React.array
            }
        </div>
        <div>
            {"Options"->React.string}
        </div>
        {
            switch input {
                | "" => <div>{"No preview"->React.string}</div>
                | _ => <div className={styles["preview"]}>
                    <p className={styles["title"]}>{"Preview"->React.string}</p>
                    <div className={styles["output"]}>
                        {input->React.string}
                    </div>
                    <div className={styles["output"]}>
                        {
                            switch cuneiformPreview {
                                | Some((word, cuneiform)) => switch cuneiform {
                                    | Some(codePoint) => <span className="cuneiforms" style={fontSize: "1.5rem"}>
                                        {codePoint->WebUtils.toNumber->String.fromCodePoint->React.string}
                                    </span>
                                    | None => <span>{word->React.string}</span>
                                }
                                | None => <span>{"No cuneiform preview"->React.string}</span>
                            }
                        }
                    </div>
                </div>
            }
        }
        {
            switch sumerianOutput {
                | Some(output) => 
                    <div className={styles["sumerianOutput"]}>
                        <p className={styles["title"]}>{"Sumerian output"->React.string}</p>
                        <div className={styles["output"]}>
                            {output->React.string}
                        </div>
                    </div>
                | None => 
                    <div>
                        {" "->React.string}
                    </div>
            }
        }
        {
            switch cuneiformOutput {
                | Some(output) => <div>
                    <div className={styles["cuneiformOutput"]}>
                        <p className={styles["title"]}>{"Cuneiform output"->React.string}</p>
                        <div className={styles["output"]}>
                            {
                                output
                                ->String.split("-")
                                ->WebUtils.displayCuneiforms
                                ->Array.mapWithIndex(((codePoint, word), i) => {
                                    let element = <span className="cuneiforms" style={fontSize: "1.5rem"} key={codePoint ++ word ++ Int.toString(i)}>
                                        {codePoint->React.string}
                                    </span>
                                    React.cloneElement(element, {"data-tooltip": word})
                                })
                                ->React.array
                            }
                        </div>
                    </div>
                </div>
                | None => <div>{" "->React.string}</div>
            }
        }
    </div>
}