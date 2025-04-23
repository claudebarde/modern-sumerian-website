@module external styles: {..} = "./Conjugator.module.scss"

module ReactSelect = {
    type selectOption = {
        label: string,
        value: string,
    }

    let personParamToOption = (pp: Infixes.personParam): selectOption => {
        switch pp {
        | Infixes.FirstSing => {label: "I", value: "first-sing"}
        | Infixes.SecondSing => {label: "You (sing)", value: "second-sing"}
        | Infixes.ThirdSingHuman => {label: "He/She", value: "third-sing-human"}
        | Infixes.ThirdSingNonHuman => {label: "It", value: "third-sing-nonhuman"}
        | Infixes.FirstPlur => {label: "We", value: "first-plur"}
        | Infixes.SecondPlur => {label: "You (plur)", value: "second-plur"}
        | Infixes.ThirdPlurHuman => {label: "They (human)", value: "third-plur-human"}
        | Infixes.ThirdPlurNonHuman => {label: "They (non-human)", value: "third-plur-nonhuman"}
        }
    }

    @module("react-select") @react.component
    external make: (
        ~options: array<selectOption>,
        ~value: Nullable.t<selectOption>,
        ~onChange: (selectOption) => (),
    ) => React.element = "default"
}

@react.component
let make = () => {
    let (error, setError) = React.useState(_ => None)
    let (verbStem, setVerbStem) = React.useState(_ => Nullable.null)
    let (verbForm, setVerbForm) = React.useState(_ => None)
    let (isPerfective, setIsPerfective) = React.useState(_ => None)
    let (isTransitive, setIsTransitive) = React.useState(_ => None)
    let (preformative, setPreformative) = React.useState(_ => None)
    let (modal, setModal) = React.useState(_ => false)
    let (negative, setNegative) = React.useState(_ => false)
    let (ventive, setVentive) = React.useState(_ => false)
    let (comitative, setComitative) = React.useState(_ => false)
    let (ablative, setAblative) = React.useState(_ => false)
    let (terminative, setTerminative) = React.useState(_ => false)
    let (middlePrefix, setMiddlePrefix) = React.useState(_ => false)
    let (initialPersonPrefix, setInitialPersonPrefix) = React.useState(_ => Nullable.null)
    let (subject, setSubject) = React.useState(_ => Nullable.null)
    let (object, setObject) = React.useState(_ => Nullable.null)
    let (indirectObject, setIndirectObject) = React.useState(_ => Nullable.null)

    let verbOptions: array<ReactSelect.selectOption> = [
        {label: "ak", value: "ʔak"},
        {label: "ĝen", value: "ĝen"},
        {label: "tuku", value: "tuku"},
    ]

    let pronounOptions: array<ReactSelect.selectOption> = [
        {label: "I", value: "first-sing"},
        {label: "You (sing)", value: "second-sing"},
        {label: "He/She", value: "third-sing-human"},
        {label: "It", value: "third-sing-nonhuman"},
        {label: "We", value: "first-plur"},
        {label: "You (plur)", value: "second-plur"},
        {label: "They (human)", value: "third-plur-human"},
        {label: "They (non-human)", value: "third-plur-nonhuman"},
    ]

    let setNewVerbStem = (val: ReactSelect.selectOption) => {
        setVerbStem(_ => val->Nullable.make)
        setVerbForm(_ => Some(FiniteVerb.new(val.value)))
        setError(_ => None)
    }

    let changePronoun = (val: ReactSelect.selectOption, pronoun: string) => {
        if (Option.isNone(isPerfective) && Option.isNone(isTransitive)) {
            setError(_ => Some("Aspect and transitivity must be selected"))
        } else {
            switch (pronoun, val.value->WebUtils.pronounToPersonParam) {
                | ("initial-person-prefix", Some(personParam)) => {
                    setVerbForm(prevVerbForm => {
                        switch prevVerbForm {
                            | Some(verb) => {
                                setError(_ => None)
                                setInitialPersonPrefix(_ => personParam->Nullable.make)
                                Some(FiniteVerb.setInitialPersonPrefix(verb, personParam))
                            }
                            | None => None
                        }
                    })
                }
                | ("subject", Some(personParam)) => {
                    setVerbForm(prevVerbForm => {
                        switch prevVerbForm {
                            | Some(verb) => {
                                setError(_ => None)
                                setSubject(_ => personParam->Nullable.make)
                                Some(FiniteVerb.setSubject(verb, personParam))
                            }
                            | None => None
                        }
                    })
                }
                | ("object", Some(personParam)) => {
                    setVerbForm(prevVerbForm => {
                        switch prevVerbForm {
                            | Some(verb) => {
                                setError(_ => None)
                                setObject(_ => personParam->Nullable.make)
                                Some(FiniteVerb.setObject(verb, personParam))
                            }
                            | None => None
                        }
                    })
                }
                | ("indirect-object", Some(personParam)) => {
                    setVerbForm(prevVerbForm => {
                        switch prevVerbForm {
                            | Some(verb) => {
                                setError(_ => None)
                                setIndirectObject(_ => personParam->Nullable.make)
                                Some(FiniteVerb.setIndirectObject(verb, personParam))
                            }
                            | None => None
                        }
                    })
                }
                | _ => ()
            }
        }
    }

    let changePreformative = (ev: JsxEvent.Form.t) => {
        if (Option.isNone(isPerfective) && Option.isNone(isTransitive)) {
            setError(_ => Some("Aspect and transitivity must be selected"))
        } else {
            let target = JsxEvent.Form.target(ev)
            let value: string = target["value"]
            let preformative = switch value {
                | "preformative-a" => Some(Infixes.A)
                | "preformative-i" => Some(Infixes.I)
                | "preformative-u" => Some(Infixes.U)
                | _ => None
            }
            setVerbForm(prevVerbForm => {
                switch (prevVerbForm, preformative) {
                    | (Some(verb), Some(preformative)) => {
                        setError(_ => None)     
                        setPreformative(_ => Some(preformative))
                        Some(FiniteVerb.setPreformative(verb, preformative))
                    }
                    | (None, _) => {
                        setError(_ => Some("No verb stem selected"))
                        None
                    }
                    | _ => None
                }
            })
        }
    }

    let changePrefix = (value: string, checked: bool) => {
        if Nullable.isNullable(verbStem) {
            setError(_ => Some("No verb stem selected"))
        } else if (Option.isNone(isPerfective) && Option.isNone(isTransitive)) {
            setError(_ => Some("Aspect and transitivity must be selected"))
        } else {
            switch value {
                | "modal" => {
                    setModal(_ => checked)
                    setVerbForm(prevVerbForm => {
                        switch prevVerbForm {
                            | Some(verb) => {
                                setError(_ => None)
                                if checked {
                                    setModal(_ => checked)
                                    Some(FiniteVerb.setModal(verb))
                                } else {
                                    setModal(_ => checked)
                                    Some(FiniteVerb.resetModal(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | "negative" => {
                    setNegative(_ => checked)
                    setVerbForm(prevVerbForm => {
                        switch prevVerbForm {
                            | Some(verb) => {
                                setError(_ => None)
                                if checked {
                                    setNegative(_ => checked)
                                    Some(FiniteVerb.setNegative(verb))
                                } else {
                                    setNegative(_ => checked)
                                    Some(FiniteVerb.resetNegative(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | "ventive" => {
                    setVentive(_ => checked)
                    setVerbForm(prevVerbForm => {
                        switch prevVerbForm {
                            | Some(verb) => {
                                setError(_ => None)
                                if checked {
                                    setVentive(_ => checked)
                                    Some(FiniteVerb.setVentive(verb))
                                } else {
                                    setVentive(_ => checked)
                                    Some(FiniteVerb.resetVentive(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | "comitative" => {
                    setComitative(_ => checked)
                    setVerbForm(prevVerbForm => {
                        switch prevVerbForm {
                            | Some(verb) => {
                                setError(_ => None)
                                if checked {
                                    setComitative(_ => checked)
                                    Some(FiniteVerb.setComitative(verb, initialPersonPrefix->Nullable.toOption))
                                } else {
                                    setComitative(_ => checked)
                                    Some(FiniteVerb.resetComitative(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | "ablative" => {
                    setAblative(_ => checked)
                    setVerbForm(prevVerbForm => {
                        switch prevVerbForm {
                            | Some(verb) => {
                                setError(_ => None)
                                if checked {
                                    setAblative(_ => checked)
                                    Some(FiniteVerb.setAblative(verb, initialPersonPrefix->Nullable.toOption))
                                } else {
                                    setAblative(_ => checked)
                                    Some(FiniteVerb.resetAblative(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | "terminative" => {
                    setTerminative(_ => checked)
                    setVerbForm(prevVerbForm => {
                        switch prevVerbForm {
                            | Some(verb) => {
                                setError(_ => None)
                                if checked {
                                    setTerminative(_ => checked)
                                    Some(FiniteVerb.setTerminative(verb, initialPersonPrefix->Nullable.toOption))
                                } else {
                                    setTerminative(_ => checked)
                                    Some(FiniteVerb.resetTerminative(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | "middle-prefix" => {
                    setMiddlePrefix(_ => checked)
                    setVerbForm(prevVerbForm => {
                        switch prevVerbForm {
                            | Some(verb) => {
                                setError(_ => None)
                                if checked {
                                    setMiddlePrefix(_ => checked)
                                    Some(FiniteVerb.setMiddlePrefix(verb))
                                } else {
                                    setMiddlePrefix(_ => checked)
                                    Some(FiniteVerb.resetMiddlePrefix(verb))
                                }
                            }
                            | None => None
                        }
                    })
                }
                | _ => ()
            }
        }
    }

    <div className={styles["conjugator"]}>
        <div className={styles["selectors"]}>
            <div className={styles["firstColumn"]}>
                <div>
                    <span>
                        {"Verb Stem"->React.string}
                    </span>
                    <ReactSelect 
                        options={verbOptions} 
                        value={verbStem} 
                        onChange={setNewVerbStem} 
                    />
                </div>
                <div>
                    <p>
                        {"Aspect"->React.string}
                    </p>
                    <div className={styles["withLabels"]}>
                        <label>
                            <input 
                                type_="radio" 
                                name="perfective" 
                                value="isPerfective" 
                                checked={switch isPerfective {
                                    | Some(true) => true
                                    | Some(false) => false
                                    | None => false
                                }}
                                onChange={_ => {
                                    setVerbForm(prevVerbForm => {
                                        switch prevVerbForm {
                                            | Some(verb) => {
                                                setError(_ => None)
                                                setIsPerfective(_ => Some(true))
                                                Some(FiniteVerb.isPerfective(verb))
                                            }
                                            | None => None
                                        }
                                    })
                                }} 
                            />
                            {"Perfective"->React.string}
                        </label>
                        <label>
                            <input 
                                type_="radio" 
                                name="perfective" 
                                value="isImperfective"
                                checked={switch isPerfective {
                                    | Some(true) => false
                                    | Some(false) => true
                                    | None => false
                                }}
                                onChange={_ => {
                                    setVerbForm(prevVerbForm => {
                                        switch prevVerbForm {
                                            | Some(verb) => {
                                                setError(_ => None)
                                                setIsPerfective(_ => Some(false))
                                                Some(FiniteVerb.isImperfective(verb, None))
                                            }
                                            | None => None
                                        }
                                    })
                                }} 
                            />
                            {"Imperfective"->React.string}
                        </label>
                    </div>
                </div>
                <div>
                    <p>
                        {"Transitivity"->React.string}
                    </p>
                    <div className={styles["withLabels"]}>
                        <label>
                            <input 
                                type_="radio" 
                                name="transitivity" 
                                value="isTransitive" 
                                checked={switch isTransitive {
                                    | Some(true) => true
                                    | Some(false) => false
                                    | None => false
                                }}
                                onChange={_ => {
                                    setVerbForm(prevVerbForm => {
                                        switch prevVerbForm {
                                            | Some(verb) => {
                                                setError(_ => None)
                                                setIsTransitive(_ => Some(true))
                                                Some(FiniteVerb.isTransitive(verb))
                                            }
                                            | None => None
                                        }
                                    })
                                }} 
                            />
                            {"Transitive"->React.string}
                        </label>
                        <label>
                            <input 
                                type_="radio" 
                                name="transitivity" 
                                value="isIntransitive" 
                                checked={switch isTransitive {
                                    | Some(true) => false
                                    | Some(false) => true
                                    | None => false
                                }}
                                onChange={_ => {
                                    setVerbForm(prevVerbForm => {
                                        switch prevVerbForm {
                                            | Some(verb) => {
                                                setError(_ => None)
                                                setIsTransitive(_ => Some(false))
                                                Some(FiniteVerb.isIntransitive(verb))
                                            }
                                            | None => None
                                        }
                                    })
                                }} 
                            />
                            {"Intransitive"->React.string}
                        </label>
                    </div>
                </div>
                <div>
                    <span>
                        {"Subject"->React.string}
                    </span>
                    <ReactSelect 
                        options={pronounOptions} 
                        value={
                            switch subject {
                                | Value(pp) => ReactSelect.personParamToOption(pp)->Nullable.make
                                | _ => Nullable.null
                            }
                        }
                        onChange={changePronoun(_, "subject")}
                    />
                </div>
                <div>
                    <span>
                        {"Object"->React.string}
                    </span>
                    <ReactSelect 
                        options={pronounOptions} 
                        value={
                            switch object {
                                | Value(pp) => ReactSelect.personParamToOption(pp)->Nullable.make
                                | _ => Nullable.null
                            }
                        }
                        onChange={changePronoun(_, "object")}
                    />
                </div>
                <div>
                    <span>
                        {"Indirect Object"->React.string}
                    </span>
                    <ReactSelect 
                        options={pronounOptions}
                        value={
                            switch indirectObject {
                                | Value(pp) => ReactSelect.personParamToOption(pp)->Nullable.make
                                | _ => Nullable.null
                            }
                        }
                        onChange={changePronoun(_, "indirect-object")}
                    />
                </div>
            </div>
            <div className={styles["secondColumn"]}>
                <div>
                    <p>
                        {"Preformative"->React.string}
                    </p>
                    <div className={styles["withLabels"]}>
                        <label>
                            <input 
                                type_="radio" 
                                name="preformative" 
                                value="preformative-a" 
                                checked={switch preformative {
                                    | Some(Infixes.A) => true
                                    | _ => false
                                }}
                                onChange={changePreformative}
                            />
                            {"A"->React.string}
                        </label>
                        <label>
                            <input 
                                type_="radio" 
                                name="preformative" 
                                value="preformative-i" 
                                checked={switch preformative {
                                    | Some(Infixes.I) => true
                                    | _ => false
                                }}
                                onChange={changePreformative}
                            />
                            {"I"->React.string}
                        </label>
                        <label>
                            <input 
                                type_="radio" 
                                name="preformative" 
                                value="preformative-u" 
                                checked={switch preformative {
                                    | Some(Infixes.U) => true
                                    | _ => false
                                }}
                                onChange={changePreformative}
                            />
                            {"U"->React.string}
                        </label>
                    </div>
                </div>
                <div>
                    <span>
                        {"Initial Person Prefix"->React.string}
                    </span>
                    <ReactSelect 
                        options={pronounOptions} 
                        value={
                            switch initialPersonPrefix {
                                | Value(pp) => ReactSelect.personParamToOption(pp)->Nullable.make
                                | _ => Nullable.null
                            }
                        }
                        onChange={changePronoun(_, "initial-person-prefix")}
                    />
                </div>
                <div>
                    <p>
                        {"Other Prefixes"->React.string}
                    </p>
                    <div className={styles["withLabels"]}>
                        <label>
                            <input 
                                type_="checkbox" 
                                name="modal" 
                                value="modal"
                                checked={modal}
                                onChange={ev => {
                                    let target = JsxEvent.Form.target(ev)
                                    let value: string = target["value"]
                                    let checked: bool = target["checked"]
                                    changePrefix(value, checked)
                                }}
                             />
                            {"ḪA"->React.string}
                        </label>
                        <label>
                            <input 
                                type_="checkbox" 
                                name="negative" 
                                value="negative" 
                                checked={negative}
                                onChange={ev => {
                                    let target = JsxEvent.Form.target(ev)
                                    let value: string = target["value"]
                                    let checked: bool = target["checked"]
                                    changePrefix(value, checked)
                                }}
                            />
                            {"NU"->React.string}
                        </label>
                        <label>
                            <input 
                                type_="checkbox" 
                                name="ventive" 
                                value="ventive" 
                                checked={ventive}
                                onChange={ev => {
                                    let target = JsxEvent.Form.target(ev)
                                    let value: string = target["value"]
                                    let checked: bool = target["checked"]
                                    changePrefix(value, checked)
                                }}
                            />
                            {"MU"->React.string}
                        </label>
                    </div>
                    <div className={styles["withLabels"]}>
                        <label>
                            <input 
                                type_="checkbox" 
                                name="comitative" 
                                value="comitative" 
                                checked={comitative}
                                onChange={ev => {
                                    let target = JsxEvent.Form.target(ev)
                                    let value: string = target["value"]
                                    let checked: bool = target["checked"]
                                    changePrefix(value, checked)
                                }}
                            />
                            {"DA"->React.string}
                        </label>
                        <label>
                            <input 
                                type_="checkbox" 
                                name="ablative" 
                                value="ablative" 
                                checked={ablative}
                                onChange={ev => {
                                    let target = JsxEvent.Form.target(ev)
                                    let value: string = target["value"]
                                    let checked: bool = target["checked"]
                                    changePrefix(value, checked)
                                }}
                            />
                            {"TA"->React.string}
                        </label>
                        <label>
                            <input 
                                type_="checkbox" 
                                name="terminative" 
                                value="terminative" 
                                checked={terminative}
                                onChange={ev => {
                                    let target = JsxEvent.Form.target(ev)
                                    let value: string = target["value"]
                                    let checked: bool = target["checked"]
                                    changePrefix(value, checked)
                                }}
                            />
                            {"ŠI"->React.string}
                        </label>
                    </div>
                    <div className={styles["withLabels"]}>                        
                        <label>
                            <input 
                                type_="checkbox" 
                                name="middle-prefix" 
                                value="middle-prefix" 
                                checked={middlePrefix}
                                onChange={ev => {
                                    let target = JsxEvent.Form.target(ev)
                                    let value: string = target["value"]
                                    let checked: bool = target["checked"]
                                    changePrefix(value, checked)
                                }}
                            />
                            {"BA"->React.string}
                        </label>
                    </div>
                </div>
            </div>
        </div>
        <div className={styles["result"]}>
            {
                switch ((verbForm), error) {
                | (_, Some(err)) => {
                    <span className={styles["error"]}>
                        {err->React.string}
                    </span>
                }
                | (Some(verb), None) => WebUtils.buildResults(verb)
                | (None, None) => <span>{"No Selected Verb"->React.string}</span>
                }
            }
        </div>
        <div className={styles["buttons"]}>
            <button onClick={_ => {
                setVerbStem(_ => Nullable.null)
                setVerbForm(_ => None)
                setIsPerfective(_ => None)
                setIsTransitive(_ => None)
                setPreformative(_ => None)
                setModal(_ => false)
                setNegative(_ => false)
                setVentive(_ => false)
                setComitative(_ => false)
                setAblative(_ => false)
                setTerminative(_ => false)
                setMiddlePrefix(_ => false)
                setInitialPersonPrefix(_ => Nullable.null)
                setSubject(_ => Nullable.null)
                setObject(_ => Nullable.null)
                setIndirectObject(_ => Nullable.null)
                setError(_ => None)
            }}>
                {"Clear"->React.string}
            </button>
            <button>
                {"Copy"->React.string}
            </button>
        </div>
    </div>
}