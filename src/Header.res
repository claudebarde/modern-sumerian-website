@module external styles: {..} = "./Header.module.scss"

@react.component
let make = () => {
    let url = RescriptReactRouter.useUrl()

    <header>
        <div>
            <h1>
            <span>
                {"Modern Sumerian"->React.string}
            </span>
            <span className="cuneiform">
                {["eme", "ĝir15"]
                ->WebUtils.displayCuneiforms
                ->Js.Array2.joinWith("")
                ->React.string}
            </span>
        </h1>
        </div>
        <div>
            <nav className={styles["navColumn"]}>
            <ul>
                <li>
                    <button
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
                    </button>
                </li>
                <li>
                    <button
                        className={
                            switch url.path->List.head {
                            | Some(path) if path === "conjugator" => styles["active"]
                            | _ => styles[""]
                            }
                        }
                        onClick={_ => {
                            RescriptReactRouter.push("conjugator")
                        }}>
                        {"Conjugator"->React.string}
                    </button>
                </li>
                <li>
                    <button
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
                    </button>
                </li>
            </ul>
        </nav>
        </div>
    </header>
}