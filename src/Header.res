@module external styles: {..} = "./Header.module.scss"

@react.component
let make = () => {
    let url = RescriptReactRouter.useUrl()
    let env = %raw("process.env.NODE_ENV")

    <header>
        <div>
            <h1>
            <span>
                {"Modern Sumerian"->React.string}
            </span>
            <span className="cuneiforms">
                {["eme", "ĝir15"]
                ->WebUtils.displayCuneiforms
                ->Array.mapWithIndex(((codePoint, word), i) => {
                    <span key={codePoint ++ word ++ Int.toString(i)} title={word}>
                        {codePoint->React.string}
                    </span>
                })
                ->React.array}
            </span>
        </h1>
        </div>
        <div>
            {env === "development" ? (               
                <nav className={styles["navColumn"]} role="navigation">
                <ul>
                    <li>
                        <a
                            className={
                                switch url.path->List.head {
                                | Some(_) => styles[""]
                                | None => styles["active"]
                                }
                            }
                            onClick={_ => {
                                RescriptReactRouter.push("/")
                            }}>
                            {"Home"->React.string}
                        </a>
                    </li>
                    <li>
                        <a
                            className={
                                switch url.path->List.head {
                                | Some(path) if path === "conjugator" => styles["active"]
                                | Some(path) if path === "cuneiforms" => styles["active"]
                                | Some(path) if path === "dictionary" => styles["active"]
                                | _ => styles[""]
                                }
                            }>
                            {"Tools"->React.string}
                        </a>
                        <ul className={styles["dropdown"]}>
                            <li>
                                <a 
                                    onClick={_ => { RescriptReactRouter.push("conjugator") }}
                                >
                                    {"Conjugator"->React.string}
                                </a>
                            </li>
                            <li>
                                <a 
                                    onClick={_ => { RescriptReactRouter.push("cuneiforms") }}
                                >
                                    {"Cuneiforms"->React.string}
                                </a>
                            </li>
                            <li>
                                <a 
                                    onClick={_ => { RescriptReactRouter.push("dictionary") }}
                                >
                                    {"Dictionary"->React.string}
                                </a>
                            </li>
                        </ul>
                    </li>
                    <li>
                        <a
                            className={
                                switch url.path->List.head {
                                | Some(path) if path === "lessons" => styles["active"]
                                | _ => styles[""]
                                }
                            }
                            onClick={_ => {
                                RescriptReactRouter.push("lessons")
                            }}>
                            {"Lessons"->React.string}
                        </a>
                    </li>
                    <li>
                        <a
                            className={
                                switch url.path->List.head {
                                | Some(path) if path === "links" => styles["active"]
                                | _ => styles[""]
                                }
                            }
                            onClick={_ => {
                                RescriptReactRouter.push("links")
                            }}>
                            {"Links"->React.string}
                        </a>
                    </li>
                </ul>
            </nav>
        ) : React.null}
        </div>
    </header>
}